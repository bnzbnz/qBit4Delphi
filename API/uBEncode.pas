///
///  Authors: ShareNET Networks, Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///  Version: 2.0.1
///
///  https://github.com/bnzbnz/qBit4Delphi
///  https://torry.net/pages.php?id=650
///  https://stackoverflow.com/questions/34157958/delphi-how-to-use-bencode-to-get-info-hash
///

unit uBEncode;

interface

uses
  Classes, Contnrs, SysUtils, System.Generics.Collections, System.Generics.Defaults;

type
  TBEncoded = class;

  TBEncodedFormat = (befEmpty, befString, befInteger, befList, befDictionary);

  TBEncodedData = class(TObject)
  public
    Data: TBEncoded;
    Header: AnsiString;
    constructor Create(Data: TBEncoded);
    destructor Destroy; override;
  end;

  TBEncodedDataList = class(TObjectList<TBEncodedData>)
    function FindElement(Header: AnsiString): TBEncoded;
  end;

  TBEncoded = class(TObject)
  private
    FFormat: TBEncodedFormat;
    FMemStream: TMemoryStream;
    FMemStart: Int64;
    FMemEnd: Int64;
    FCachedSHA1: string;
    FCachedSHA256: string;
    procedure SetFormat(Format: TBEncodedFormat);
    function GetSHA1: string;
    function GetSHA256: string;
  public
    IntegerData: Int64;
    ListData: TBEncodedDataList;
    StringData: AnsiString;
    class procedure Encode(Encoded: TBEncoded; Output: TStringBuilder);
    constructor Create(MemStream: TMemoryStream);
    destructor Destroy; override;
    property Format: TBEncodedFormat read FFormat write SetFormat;
    property SHA1: string read GetSHA1;
    property SHA256: string read GetSHA256;
  end;

implementation
uses System.Types, System.Hash, Windows;

procedure RaiseException(Str: string);
begin
  raise Exception.Create('TBEncoded: ' + Str);
end;

procedure FormatException;
begin
  RaiseException('TBEncoded : Invalid Format');
end;

destructor TBEncodedData.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;

constructor TBEncodedData.Create(Data: TBEncoded);
begin
  inherited Create;
  Self.Data := Data;
end;

destructor TBEncoded.Destroy;
begin
  if ListData <> nil then ListData.Free;
  inherited Destroy;
end;

constructor TBEncoded.Create(MemStream: TMemoryStream);

  function GetString(Buffer: AnsiString): AnsiString;
  var
    X: AnsiChar;
  begin
    repeat
      if MemStream.Read(X, 1) <> 1 then FormatException;
      if not ((X in ['0'..'9']) or (x = ':')) then FormatException;
      if X = ':' then
      begin
        if Buffer = '' then FormatException;
        if Length(Buffer) > 12 then FormatException;
        SetLength(Result, StrToInt(String(Buffer)));
        if Length(Result) > 0 then
          if MemStream.Read(Result[1], Length(Result)) <> Length(Result) then FormatException;
        Break;
      end
      else
        Buffer := Buffer + X;
    until False;
  end;

var
  X: AnsiChar;
  Buffer: AnsiString;
  Data: TBEncodedData;

begin
  inherited Create;
  FMemStream := MemStream;
  FMemStart := MemStream.Position;
  if MemStream.Read(X, 1) <> 1 then FormatException;
  if X = 'i' then
  begin
    Buffer := '';
    repeat
      if MemStream.Read(X, 1) <> 1 then FormatException;
      if not ((X in ['0'..'9']) or (X = 'e')) then FormatException;
      if X = 'e' then
      begin
        if Buffer = '' then
          raise Exception.Create('Invalid Format')
        else
        begin
          Format := befInteger;
          IntegerData := StrToInt64(String(Buffer));
          Break;
        end;
      end
      else
        Buffer := Buffer + X;
    until False;
  end
  else if X = 'l' then
  begin
    Format := befList;
    repeat
      if MemStream.Read(X, 1) <> 1 then FormatException;
      if X = 'e' then Break;
      MemStream.Seek(-1, soFromCurrent);
      ListData.Add(TBEncodedData.Create(TBEncoded.Create(MemStream)));
    until False;
  end
  else if X = 'd' then
  begin
    Format := befDictionary;
    repeat
      if MemStream.Read(X, 1) <> 1 then FormatException;
      if X = 'e' then Break;
      if not (X in ['0'..'9']) then FormatException;
      Buffer := GetString(X);
      Data := TBEncodedData.Create(TBEncoded.Create(MemStream));
      Data.Header := Buffer;
      ListData.Add(Data);
    until False;
  end
  else if X in ['0'..'9'] then
  begin
    StringData := GetString(X);
    Format := befString;
  end
  else FormatException;
  FMemEnd := MemStream.Position;
end;

class procedure TBEncoded.Encode(Encoded: TBEncoded; Output: TStringBuilder);
begin
  with Encoded do
  begin
    case Format of
      befString:
          Output.Append(Length(StringData)).Append(':').Append(StringData);
      befInteger:
          Output.Append('i').Append(IntegerData).Append('e');
      befList:
        begin
          Output.Append('l');
          for var i := 0 to ListData.Count - 1 do
            Encode(TBEncoded(ListData[i].Data), Output);
          Output.Append('e');
        end;
      befDictionary:
        begin
          Output.Append( 'd');
          for var i := 0 to ListData.Count - 1 do
          begin
            Output.Append(Length(ListData[i].Header)).Append(':').Append(ListData[i].Header);
            Encode(TBEncoded(ListData[i].Data), Output);
          end;
          Output.Append( 'e');
        end;
    end;
  end;
end;

procedure TBEncoded.SetFormat(Format: TBEncodedFormat);
begin
  if Format in [befList, befDictionary] then ListData := TBEncodedDataList.Create;
  FFormat := Format;
end;

function TBEncoded.GetSHA1: string;
begin
  if FCachedSHA1 = '' then
  begin
    var SHA := THashSHA1.Create;
    SHA.Update( PByte(Cardinal(FMemStream.Memory) + FMemStart)^ ,  FMemEnd - FMemStart);
    FCachedSHA1 := SHA.HashAsString;
  end;
  Result := FCachedSHA1;
end;

function TBEncoded.GetSHA256: string;
begin
  if FCachedSHA256 = '' then
  begin
    var SHA := THashSHA2.Create;
    SHA.Update( PByte(Cardinal(FMemStream.Memory) + FMemStart)^ ,  FMemEnd - FMemStart);
    FCachedSHA256 := SHA.HashAsString;
  end;
  Result := FCachedSHA256;
end;

function TBEncodedDataList.FindElement(Header: AnsiString): TBEncoded;
begin
  Result := nil;
  for var i := 0 to Count - 1 do
    if Items[i].Header = Header then
    begin
      Result := Items[i].Data;
      Break;
    end;
end;

end.
