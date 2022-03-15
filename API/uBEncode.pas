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
  Classes, Contnrs, SysUtils;

type
  TBEncoded = class;

  TBEncodedFormat = (befEmpty, befString, befInteger, befList, befDictionary);

  TBEncodedData = class(TObject)
  public
    Header: AnsiString;
    Data: TBEncoded;
    constructor Create(Data: TBEncoded);
    destructor Destroy; override;
  end;

  TBEncodedDataList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TBEncodedData;
    procedure SetItems(Index: Integer; AClass: TBEncodedData);
  public
    function FindElement(Header: AnsiString): TBEncoded;
    function Add(AClass: TBEncodedData): Integer;
    function Extract(Item: TBEncodedData): TBEncodedData;
    function Remove(AClass: TBEncodedData): Integer;
    function IndexOf(AClass: TBEncodedData): Integer;
    function First: TBEncodedData;
    function Last: TBEncodedData;
    procedure Insert(Index: Integer; AClass: TBEncodedData);
    property Items[Index: Integer]: TBEncodedData read GetItems write SetItems; default;
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
    StringData: AnsiString;
    IntegerData: int64;
    ListData: TBEncodedDataList;
    class procedure Encode(Encoded: TBEncoded; Output: TStringBuilder);
    destructor Destroy; override;
    constructor Create(MemStream: TMemoryStream);
    property Format: TBEncodedFormat read FFormat write SetFormat;
    property SHA1: string read GetSHA1;
    property SHA256: string read GetSHA256;
  end;

implementation
uses System.Types, System.Hash;

procedure RaiseException(Str: string); inline;
begin
  raise Exception.Create('TBEncoded: ' + Str);
end;

procedure FormatException; inline
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
    if LowerCase(String(Items[i].Header)) = LowerCase(String(Header)) then
    begin
      Result := Items[i].Data;
      Break;
    end;
end;

function TBEncodedDataList.Add(AClass: TBEncodedData): Integer;
begin
  Result := inherited Add(AClass);
end;

function TBEncodedDataList.Extract(Item: TBEncodedData): TBEncodedData;
begin
  Result := TBEncodedData(inherited Extract(Item));
end;

function TBEncodedDataList.First: TBEncodedData;
begin
  Result := TBEncodedData(inherited First);
end;

function TBEncodedDataList.GetItems(Index: Integer): TBEncodedData;
begin
  Result := TBEncodedData(inherited Items[Index]);
end;

function TBEncodedDataList.IndexOf(AClass: TBEncodedData): Integer;
begin
  Result := inherited IndexOf(AClass);
end;

procedure TBEncodedDataList.Insert(Index: Integer; AClass: TBEncodedData);
begin
  inherited Insert(Index, AClass);
end;

function TBEncodedDataList.Last: TBEncodedData;
begin
  Result := TBEncodedData(inherited First);
end;

function TBEncodedDataList.Remove(AClass: TBEncodedData): Integer;
begin
  Result := inherited Remove(AClass);
end;

procedure TBEncodedDataList.SetItems(Index: Integer; AClass: TBEncodedData);
begin
  inherited Items[Index] := AClass;
end;

end.
