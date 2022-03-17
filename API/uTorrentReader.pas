///
///  Author: Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///  Version: 1.0.5
///
///  https://github.com/bnzbnz/qBit4Delphi
///  https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)
///  https://www.nayuki.io/page/bittorrent-bencode-format-tools
///

unit uTorrentReader;
interface
uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults,
  uBEncode, DateUtils, SysUtils;

const
  TORRENTREADER_PATH_SEPARATOR = '\';

type
  TTorrentReaderOptions = set of (
    trRaiseException,    // Will raise Exception on error (Default), silent otherwise
    trHybridAsV1,        // Handle hybrid torrent files as V1 (Default)
    trHybridAsV2         // Handle hybrid torrent files as V2
  );

  TFileData = class(TObject)
    FullPath: string;
    Length: Int64;
    PathList: TStringList;
    PiecesRoot: AnsiString; // V2 Only
    PiecesCount: Int64; // V2 Only
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TTorrentDataInfo = class(TObject)
    FileList: TObjectList<TFileData>;
    FilesSize: Int64;
    HasMultipleFiles: Boolean;
    IsHybrid: Boolean;
    IsPrivate: Boolean;
    Length: Int64;
    MetaVersion: Int64;
    Name: string;
    Pieces: AnsiString; // V1 Only
    PiecesCount: Int64; // V1 Only;
    PieceLength: Int64;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TTorrentData = class(TObject)
  public
    Announce: string;
    AnnouncesDict : TDictionary<Integer, TStringList>;
    Comment: TStringList;
    CreatedBy: string;
    CreationDate: TDateTime;
    HashV1: string;
    HashV2: string;
    Info: TTorrentDataInfo;
    WebSeeds: TStringList;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TTorrentReader = class(TObject)
  private
    FBe: TBEncoded;
    FData: TTorrentData;
    procedure Parse(Be: TBencoded; Options: TTorrentReaderOptions);
  public
    class function LoadFromFile(Filename: string; Options: TTorrentReaderOptions = [trRaiseException, trHybridAsV1]): TTorrentReader;
    class function LoadFromMemoryStream(MemStream: TMemoryStream; Options: TTorrentReaderOptions = [trRaiseException, trHybridAsV1]): TTorrentReader;
    class function LoadFromString(Str: string; Options: TTorrentReaderOptions = [trRaiseException, trHybridAsV1]): TTorrentReader;
    constructor Create; overload;
    destructor Destroy; override;
    property BEncoded: TBEncoded read FBe;
    property Data: TTorrentData read FData;
  end;

implementation

procedure RaiseException(Str: string);
begin
  raise Exception.Create('TTorrentReader: ' + Str);
end;

{ TTorrentData }

constructor TTorrentData.Create;
begin
  inherited;
  Info := TTorrentDataInfo.Create;
  AnnouncesDict := TDictionary<Integer, TStringList>.Create;
  WebSeeds := TStringList.Create;
  Comment := TStringList.Create;
  Comment.StrictDelimiter := True;
  Comment.Delimiter := #$A;
end;

destructor TTorrentData.Destroy;
begin
  Comment.Free;
  WebSeeds.Free;
  for var Tier in AnnouncesDict do Tier.Value.Free;
  AnnouncesDict.Free;
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

class function TTorrentReader.LoadFromMemoryStream(MemStream: TMemoryStream; Options: TTorrentReaderOptions = [trRaiseException, trHybridAsV1]): TTorrentReader;
begin
  Result := nil;
  try
    Result := TTorrentReader.Create;
    Result.FBe := TBEncoded.Create(MemStream);
    Result.Parse(Result.FBe, Options);
  except
    on E : Exception do
    begin
      FreeAndNil(Result);
      if trRaiseException in Options then raise;
    end;
  end;
end;

class function TTorrentReader.LoadFromFile(Filename: string; Options: TTorrentReaderOptions = [trRaiseException, trHybridAsV1]): TTorrentReader;
var
  MemStream: TMemoryStream;
begin
  MemStream := nil;
  try
    MemStream := TMemoryStream.Create;
    MemStream.LoadFromFile(Filename);
    Result := LoadFromMemoryStream(MemStream, Options);
  finally
    MemStream.Free;
  end;
end;

class function TTorrentReader.LoadFromString(Str: string; Options: TTorrentReaderOptions = [trRaiseException, trHybridAsV1]): TTorrentReader;
var
  stringStream: TStringStream;
begin
  stringStream := nil;
  try
    stringStream:=TStringStream.Create(Str);
    Result := LoadFromMemoryStream(StringStream, Options);
  finally
    stringStream.Free;
  end;
end;

procedure TTorrentReader.Parse(Be: TBencoded; Options: TTorrentReaderOptions);

  procedure ParseFileListV2(Dic: TBencoded; const Path: string; FileData: TFileData);
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
        FileData.PathList.Delimiter := TORRENTREADER_PATH_SEPARATOR;
        FileData.PathList.QuoteChar := #0;
        FileData.PathList.StrictDelimiter := True;
        FileData.PathList.DelimitedText := string(Path);
      end
      else if assigned(FileData) then
      begin
       if Hdr = 'length' then FileData.Length := Int;
       if Hdr = 'pieces root' then
       begin
        FileData.PiecesRoot := Str;
        FileData.PiecesCount := Length(Str) div 32;
       end;
      end;
      if assigned(TmpDic.data.ListData) then
        if Path <> '' then
          ParseFileListV2(TmpDic.Data, Path + TORRENTREADER_PATH_SEPARATOR + UTF8ToString(Hdr),  FileData)
        else
          ParseFileListV2(TmpDic.Data, UTF8ToString(Hdr),  FileData);
    end;
  end;

var
  Enc, Info: TBencoded;
  EncStr: string;
  SList: TStringList;

begin
  Info := Be.ListData.FindElement('info'); //Helper;

  //MetaVersion
  FData.Info.MetaVersion := 1;
  Enc := Info.ListData.FindElement('meta version');
  if assigned(Enc) then FData.Info.MetaVersion:= Enc.IntegerData;

  // IsHybrid
  FData.Info.IsHybrid := (Info.ListData.FindElement('files') <> nil)
                          and (Info.ListData.FindElement('file tree') <> nil);

  if (FData.Info.IsHybrid) and (trHybridAsV1 in Options) then FData.Info.MetaVersion := 1;
  if (FData.Info.IsHybrid) and (trHybridAsV2 in Options) then FData.Info.MetaVersion := 2;

  //Announce
  Enc := Be.ListData.FindElement('announce');
  if assigned(Enc) then FData.Announce := UTF8ToString(Enc.StringData);

  // AnnounceList : http://bittorrent.org/beps/bep_0012.html
  var AnnounceList := Be.ListData.FindElement('announce-list') as TBencoded;
  if assigned(AnnounceList) then
  begin
    for var SubA := 0 to AnnounceList.ListData.Count - 1 do
      case AnnounceList.ListData[SubA].Data.Format of
        befList: // Multi Tiers
          begin
            if not FData.AnnouncesDict.TryGetValue( FData.AnnouncesDict.Count , SList) then
            begin
              SList := TStringList.Create;
              FData.AnnouncesDict.Add(FData.AnnouncesDict.Count, SList);
            end;
            for var SubL := 0 to  AnnounceList.ListData[SubA].Data.ListData.Count - 1 do
              SList.AddObject(UTF8ToString(AnnounceList.ListData[SubA].Data.ListData[SubL].Data.StringData), Pointer(FData.AnnouncesDict.Count - 1));
          end;
        befString: // Single Tier
          begin
            if not FData.AnnouncesDict.TryGetValue( 0, SList) then
            begin
              SList := TStringList.Create;
              FData.AnnouncesDict.Add(0, SList);
            end;
            SList.AddObject(UTF8ToString((AnnounceList.ListData[SubA].Data).StringData), Pointer(0));
          end;
      end;
  end;

  // Comment
  Enc := Be.ListData.FindElement('comment');
  if assigned(Enc) then FData.Comment.DelimitedText := UTF8ToString(Enc.StringData);

  // CreatedBy
  Enc := Be.ListData.FindElement('created by');
  if assigned(Enc) then FData.CreatedBy := UTF8ToString(Enc.StringData);

  // CreationDate
  Enc := Be.ListData.FindElement('creation date');
  if assigned(Enc) then FData.CreationDate := TTimeZone.Local.ToLocalTime(UnixToDateTime(Enc.IntegerData));

  // Hash
  if (FData.Info.IsHybrid) then
  begin
    FData.HashV1 := Info.SHA1;
    FData.HashV2 := Info.SHA256;
  end else
    case  FData.Info.MetaVersion of
      1: FData.HashV1 := Info.SHA1;
      2: FData.HashV2 := Info.SHA256;
    else
      RaiseException('Unknown Format : ' +  FData.Info.MetaVersion.ToString);
    end;

  // Name:
  Enc := Info.ListData.FindElement('name') as TBencoded;
  if assigned(Enc) then FData.Info.Name := UTF8ToString(Enc.StringData);

  // Length:
  Enc := Info.ListData.FindElement('length') as TBencoded;
  if assigned(Enc) then FData.Info.Length := Enc.IntegerData;

  // PieceLength:
  Enc := Info.ListData.FindElement('piece length') as TBencoded;
  if assigned(Enc) then FData.Info.PieceLength := Enc.IntegerData;

  // Pieces:
  Enc := Info.ListData.FindElement('pieces') as TBencoded;
  if assigned(Enc) then
  begin
    FData.Info.Pieces := Enc.StringData;
    FData.Info.PiecesCount := Length(FData.Info.Pieces) div 20;
  end;

  //Private
  FData.Info.IsPrivate := False;
  Enc := Info.ListData.FindElement('private') as TBencoded;
  if assigned(Enc) then FData.Info.IsPrivate := Enc.IntegerData = 1;

  //UrlList:
  Enc := Be.ListData.FindElement('url-list') as TBencoded;
  if assigned(Enc) then
    if Enc.ListData = nil then
      FData.WebSeeds.Add(String(Enc.StringData))
    else
      for var i := 0 to Enc.ListData.Count - 1 do
        FData.WebSeeds.Add(UTF8ToString(Enc.ListData[i].Data.StringData));

  // files :
  if FData.Info.MetaVersion = 1 then
  begin // V1
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
        FileData.PathList.Add(UTF8ToString(FLDP.ListData.Items[j].Data.StringData));
      FileData.PathList.Delimiter := TORRENTREADER_PATH_SEPARATOR;
      FileData.PathList.QuoteChar := #0;
      FileData.PathList.StrictDelimiter := True;
      FileData.FullPath := FileData.PathList.DelimitedText;
      FData.Info.FileList.Add(FileData);
    end else begin
      // A Single File
      var FileData := TFileData.Create;
      FileData.Length := FData.Info.Length;
      FileData.FullPath := FData.Info.Name;
      FileData.PathList.Add(FData.Info.Name);
      FData.Info.FileList.Add(FileData);
    end;
  end else // V2
    ParseFileListV2(Info.ListData.FindElement('file tree'), EncStr, nil);

  // **************** Helpers **************** //

  // HasMultipleFiles
  FData.Info.HasMultipleFiles := True;
  if FData.Info.FileList.Count = 1  then
    if  FData.Info.Name =  FData.Info.FileList[0].FullPath then
    begin
      FData.Info.Name := '';
      FData.Info.HasMultipleFiles := False;
    end;

  // FilesSize & PiecesCount
  Data.Info.FilesSize := 0;
  for var fle in Data.Info.FileList do
  begin
    Data.Info.FilesSize := Data.Info.FilesSize + fle.Length;
    if Data.Info.MetaVersion > 1 then
      Data.Info.PiecesCount := Data.Info.PiecesCount + fle.PiecesCount;
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
