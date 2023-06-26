///
///  Authors: ShareNET Networks, Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///
///  https://github.com/bnzbnz/qBit4Delphi
///  https://torry.net/pages.php?id=650
///  https://stackoverflow.com/questions/34157958/delphi-how-to-use-bencode-to-get-info-hash
///
///  License: MPL 1.1 / GPL 2.1
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
    FBufferEndPtr: NativeUInt;
    FBufferStartPtr: NativeUInt;
    FFormat: TBEncodedFormat;
  public
    IntegerData: Int64;
    ListData: TBEncodedDataList;
    StringData: AnsiString;
    class procedure Encode(Encoded: TBEncoded; Output: TStringBuilder);
    constructor Create(var BufferPtr: PAnsiChar; BufferEndPtr: PAnsiChar); overload;
    constructor Create; overload;
    destructor Destroy; override;
    property Format: TBEncodedFormat read FFormat write FFormat;
    property BufferStartPtr: NativeUInt read FBufferStartPtr;
    property BufferEndPtr: NativeUInt read FBufferEndPtr;
  end;

implementation

{ Helpers }

procedure RaiseException(Str: string); inline;
begin
  raise Exception.Create('TBEncoded: ' + Str);
end;

procedure FormatException; inline
begin
  RaiseException('Invalid Format');
end;

{ TBEncodedData }

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

{ TBEncoded }

constructor TBEncoded.Create;
begin
  inherited Create;
end;

destructor TBEncoded.Destroy;
begin
  ListData.Free;
  inherited Destroy;
end;

procedure IncPtrCheck(var BufferPtr, BufferEndPtr: PAnsiChar); inline;
begin
  if BufferPtr + 1 > BufferEndPtr then FormatException;
  Inc(BufferPtr);
end;

constructor TBEncoded.Create(var BufferPtr: PAnsiChar; BufferEndPtr: PAnsiChar);

  procedure DecodeString(var AnsiStr: AnsiString);
  var
    Len: NativeUInt;
  begin
    Len := PByte(BufferPtr)^ - 48;
    repeat
      IncPtrCheck(BufferPtr, BufferEndPtr);
      if BufferPtr^ = ':' then
      begin
        if BufferPtr + (Len + 1) > BufferEndPtr then FormatException;
        Inc(BufferPtr);
        SetLength(AnsiStr, Len);
        if Len > 0 then Move(BufferPtr^, AnsiStr[1], Len);
        Inc(BufferPtr, Len);
        Break;
      end
      else
      begin
        if not (BufferPtr^ in ['0'..'9']) then FormatException;
        Len := (Len * 10) + PByte(BufferPtr)^ - 48;
      end;
    until False;
  end;

  procedure DecodeInt64(var IntValue: Int64);
  begin
    IntValue := 0;
    while (BufferPtr^ <> 'e') do
    begin
      if not (BufferPtr^ in ['0'..'9']) then FormatException;
      if BufferPtr^= '-' then
        IntValue := IntValue * -1
      else
        IntValue := (IntValue * 10) +  (PByte(BufferPtr)^ - 48);
      IncPtrCheck(BufferPtr, BufferEndPtr);
    end;
    IncPtrCheck(BufferPtr, BufferEndPtr);
  end;

var
  Header: AnsiString;
  Data: TBEncodedData;

begin
  inherited Create;
  FBufferStartPtr := NativeUInt(BufferPtr);

  case BufferPtr^ of
  'd':
    begin
      FFormat := befDictionary;
      ListData := TBEncodedDataList.Create;
      IncPtrCheck(BufferPtr, BufferEndPtr);
      repeat
        if BufferPtr^ = 'e' then begin Inc(BufferPtr); Break; end;
        DecodeString(Header);
        Data := TBEncodedData.Create(TBEncoded.Create(BufferPtr, BufferEndPtr));
        Data.Header := Header;
        ListData.Add(Data);
      until False;
    end;
  'l':
    begin
      FFormat := befList;
      ListData := TBEncodedDataList.Create;
      IncPtrCheck(BufferPtr, BufferEndPtr);
      repeat
        if BufferPtr^ = 'e' then begin IncPtrCheck(BufferPtr, BufferEndPtr); Break; end;
        ListData.Add(TBEncodedData.Create(TBEncoded.Create(BufferPtr, BufferEndPtr)));
      until False;
    end;
  'i':
    begin
      FFormat := befInteger;
      IncPtrCheck(BufferPtr, BufferEndPtr);
      DecodeInt64(IntegerData);
    end;
  else
    FFormat := befString;
    DecodeString(StringData);
  end;

  FBufferEndPtr := NativeUInt(BufferPtr);
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

{ TBEncodedDataList }

function TBEncodedDataList.FindElement(Header: AnsiString): TBEncoded;
begin
  for var Item in Self do
    if Item.Header = Header then
    begin
      Result := Item.Data;
      exit;
    end;
  Result := nil;
end;

end.
