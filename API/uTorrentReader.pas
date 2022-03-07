///
///  Author: Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///  Version: 1.0.1
///
///  https://github.com/bnzbnz/qBit4Delphi
///  https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)
///

unit uTorrentReader;

interface
uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults,
  uBEncode, DateUtils, SysUtils;

const
  TORRENTPATHSEPARATOR = '\';
type

  TFileData = class(TObject)
    Length: Int64;
    PathList: TStringList;
    FullPath: AnsiString;
    PiecesRoot: AnsiString; // V2 Only
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TTorrentDataInfo = class(TObject)
    MetaVersion: Int64;
    MultiFiles: Boolean;
    FileList: TObjectList<TFileData>;
    Length: Int64;
    Name: AnsiString;
    PieceLength: Int64;
    Pieces: AnsiString; // V1 Only
    IsPrivate: Boolean;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TTorrentData = class(TObject)
  public
    Announce: AnsiString;
    AnnounceList: TStringList;
    Comment: AnsiString;
    CreatedBy: AnsiString;
    CreationDate: TDateTime;
    Hash: String;
    Info: TTorrentDataInfo;
    UrlList: TStringList;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TTorrentReader = class(TObject)
  private
    FData: TTorrentData;
    FBe: TBEncoded;
    procedure ParseFileListV2(Dic: TBencoded; Path: AnsiString; FileData: TFileData);
    procedure Parse(Be: TBencoded);
  public
    class function LoadFromStream(Stream: TStream): TTorrentReader;
    class function LoadFromFile(Filename: String): TTorrentReader;
    class function LoadFromString(Str: String): TTorrentReader;
    constructor Create; overload;
    destructor Destroy; override;

    property Data: TTorrentData read FData;
    property BEncoded: TBEncoded read FBe;
  end;

implementation
uses System.NetEncoding, System.Hash;

{ TTorrentData }

constructor TTorrentData.Create;
begin
  inherited;
  Info := TTorrentDataInfo.Create;
  AnnounceList := TStringList.Create;
  UrlList := TStringList.Create;
end;

destructor TTorrentData.Destroy;
begin
  UrlList.Free;
  AnnounceList.Free;
  Info.Free;
  inherited;
end;

{ TTorrentReader }

constructor TTorrentReader.Create;
begin
  inherited;
  FBe := nil;
  FData := TTorrentData.Create;
end;

destructor TTorrentReader.Destroy;
begin
  FBe.Free;
  FData.Free;
  inherited;
end;

class function TTorrentReader.LoadFromStream(Stream: TStream): TTorrentReader;
begin
  Result := TTorrentReader.Create;
  try
    Result.FBe := TBEncoded.Create(Stream);
    Result.Parse(Result.FBe);
  except
    FreeAndNil(Result);
  end;
end;

class function TTorrentReader.LoadFromFile(Filename: String): TTorrentReader;
var
  FileStream: TFileStream;
begin
  FileStream := nil;
  try
    FileStream:=TFileStream.Create(Filename, fmShareDenyNone);
    Result := LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

class function TTorrentReader.LoadFromString(Str: String): TTorrentReader;
var
  StringStream: TStringStream;
begin
  StringStream := nil;
  try
    StringStream:=TStringStream.Create(Str);
    Result := LoadFromStream(StringStream);
  finally
    StringStream.Free;
  end;
end;

procedure TTorrentReader.ParseFileListV2(Dic: TBencoded; Path: AnsiString; FileData: TFileData);
begin
  if not assigned(Dic.ListData) then exit;
  for var i := 0 to Dic.ListData.Count - 1 do
  begin
    var TmpDic := Dic.ListData.Items[i];
    var Hdr := TmpDic.Header;
    var Str := TmpDic.Data.StringData;
    var Int := TmpDic.Data.IntegerData;
    if (Hdr = '') and (Str = '') then
    begin
      FileData := TFileData.Create;
      FData.Info.FileList.Add(FileData);
      FileData.FullPath := Path;
      FileData.PathList.Delimiter := TORRENTPATHSEPARATOR;
      FileData.PathList.QuoteChar := #0;
      FileData.PathList.StrictDelimiter := True;
      FileData.PathList.DelimitedText := String(Path);
    end
    else if assigned(FileData) then
    begin
     if Hdr = 'length' then FileData.Length := Int;
     if Hdr = 'pieces root' then FileData.PiecesRoot := Str;
    end;
    if assigned(TmpDic.data.ListData) then
      if Path <> '' then
        ParseFileListV2(TmpDic.Data, Path + TORRENTPATHSEPARATOR + Hdr,  FileData)
      else
        ParseFileListV2(TmpDic.Data, Hdr,  FileData);
  end;
end;

procedure TTorrentReader.Parse(Be: TBencoded);
var
  Enc, Info: TBencoded;
  EncStr: AnsiString;
begin

  Info := Be.ListData.FindElement('info'); //Helper;

  //MetaVersion
  FData.Info.MetaVersion := 1;
  Enc := Info.ListData.FindElement('meta version');
  if assigned(Enc) then FData.Info.MetaVersion:= Enc.IntegerData;

  //Announce
  Enc := Be.ListData.FindElement('announce');
  if assigned(Enc) then FData.Announce := Enc.StringData;

  // AnnounceList
  var AnnounceList := Be.ListData.FindElement('announce-list') as TBencoded;
  if assigned(AnnounceList) then
    begin
      var SubList := AnnounceList.ListData[0].Data;
      for var i := 0 to SubList.ListData.Count - 1 do
        FData.AnnounceList.Add(String((SubList.ListData[i].Data).StringData));
    end;

  // Comment
  Enc := Be.ListData.FindElement('comment');
  if assigned(Enc) then FData.Comment := Enc.StringData;

  // CreatedBy
  Enc := Be.ListData.FindElement('created by');
  if assigned(Enc) then FData.CreatedBy := Enc.StringData;

  // CreationDate
  Enc := Be.ListData.FindElement('creation date');
  if assigned(Enc) then FData.CreationDate := TTimeZone.Local.ToLocalTime(UnixToDateTime(Enc.IntegerData));

  // Hash
  var StringBuilder := TStringBuilder.Create;
  TBencoded.Encode(Info, StringBuilder);
  var Ss := TStringStream.Create(StringBuilder.ToString);
  if FData.Info.MetaVersion = 1 then
    FData.Hash := THashSHA1.GetHashString(Ss)
  else
    FData.Hash := THashSHA2.GetHashString(Ss);
  StringBuilder.Free;
  Ss.Free;

  // Name:
  Enc := Info.ListData.FindElement('name') as TBencoded;
  if assigned(Enc) then FData.Info.Name := Enc.StringData;

  // Length:
  Enc := Info.ListData.FindElement('length') as TBencoded;
  if assigned(Enc) then FData.Info.Length := Enc.IntegerData;

  // PieceLength:
  Enc := Info.ListData.FindElement('piece length') as TBencoded;
  if assigned(Enc) then FData.Info.PieceLength := Enc.IntegerData;

  // Pieces:
  Enc := Info.ListData.FindElement('pieces') as TBencoded;
  if assigned(Enc) then FData.Info.Pieces := Enc.StringData;

  //Private
  FData.Info.IsPrivate := False;
  Enc := Info.ListData.FindElement('private') as TBencoded;
  if assigned(Enc) then FData.Info.IsPrivate := Enc.IntegerData = 1;

  //UrlList:
  Enc := Be.ListData.FindElement('url-list') as TBencoded;
  if assigned(Enc) then
    for var i := 0 to Enc.ListData.Count - 1 do
      FData.UrlList.Add(String(Enc.ListData[i].Data.StringData));

  // files :
  if FData.Info.MetaVersion = 1 then
  begin
    // ParseFileListV1
    var FL := Info.ListData.FindElement('files');
    if FL <> nil then
    for var i := 0 to FL.ListData.Count - 1 do
    begin
      // Multiple Files/Folders
      var FileData := TFileData.Create;
      var FLD := FL.ListData.Items[i].Data;
      FileData.Length := FLD.ListData.FindElement('length').IntegerData;
      var FLDP := FLD.ListData.FindElement('path');
      for var j := 0 to FLDP.ListData.Count - 1 do
      FileData.PathList.Add(String(FLDP.ListData.Items[j].Data.StringData));
      FileData.PathList.Delimiter := TORRENTPATHSEPARATOR;
      FileData.PathList.QuoteChar := #0;
      FileData.PathList.StrictDelimiter := True;
      FileData.FullPath := AnsiString(FileData.PathList.DelimitedText);
      FData.Info.FileList.Add(FileData);
      FData.Info.MultiFiles := True;
    end else begin
      // A Single File
      var FileData := TFileData.Create;
      FileData.Length := FData.Info.Length;
      FileData.FullPath := FData.Info.Name;
      FileData.PathList.Add(FData.Info.Name);
      FData.Info.FileList.Add(FileData);
      FData.Info.MultiFiles := False;
    end;
  end else begin
    // ParseFileListV2
    Enc := Info.ListData.FindElement('file tree'); // Dic of Filename
    ParseFileListV2(Enc, EncStr, nil);
  end;
  FData.Info.MultiFiles := True;
  if FData.Info.FileList.Count = 1  then
    if  FData.Info.Name =  FData.Info.FileList[0].FullPath then
    begin
      FData.Info.Name := '';
      FData.Info.MultiFiles := False;
    end;
end;

{ TTorrentDataInfo }

constructor TTorrentDataInfo.Create;
begin
  inherited;
  FileList := TObjectList<TFileData>.Create(True);
end;

destructor TTorrentDataInfo.Destroy;
begin
  FileList.Free;
  inherited;
end;

{ TFileData }

constructor TFileData.Create;
begin
  inherited;
  PathList := TStringList.Create;
end;

destructor TFileData.Destroy;
begin
  PathList.Free;
  inherited;
end;

end.