///
///  Authors: ShareNET Networks, Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///  Version: 2.2.0
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
    FBufferStart: NativeUInt;
    FBufferEnd: NativeUInt;
    procedure SetFormat(Format: TBEncodedFormat);
  public
    IntegerData: Int64;
    ListData: TBEncodedDataList;
    StringData: AnsiString;
    class procedure Encode(Encoded: TBEncoded; Output: TStringBuilder);
    constructor Create(var BufferPtr: PAnsiChar);
    destructor Destroy; override;
    property Format: TBEncodedFormat read FFormat write SetFormat;
    property BufferStart: NativeUInt read FBufferStart;
    property BufferEnd: NativeUInt read FBufferEnd;
  end;

implementation
{$IF defined(MSWINDOWS)}
uses System.Types, System.Hash, Windows;
{$ELSE}
uses System.Types, System.Hash;
{$ENDIF}

{ Helpers }

procedure RaiseException(Str: string);
begin
  raise Exception.Create('TBEncoded: ' + Str);
end;

procedure FormatException;
begin
  RaiseException('TBEncoded : Invalid Format');
end;

function AnsiToUInt(var AnsiStr: AnsiString): NativeUInt; inline
var
  P: PByte;
begin
  Result := 0;
  P := Pointer(AnsiStr);
  while (PByte(P)^ <> 0) do
  begin
    Result := (Result * 10) + PByte(P)^ - 48;
    Inc(P);
  end;
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

destructor TBEncoded.Destroy;
begin
  if ListData <> nil then ListData.Free;
  inherited Destroy;
end;

constructor TBEncoded.Create(var BufferPtr: PAnsiChar);

  procedure GetString(var AnsiStr: AnsiString);
  var
    Len: Int64;
  begin
    Len := PByte(BufferPtr)^ - 48 ;
    repeat
      Inc(BufferPtr);
      if BufferPtr^ = ':' then
      begin
         Inc(BufferPtr);
         SetLength(AnsiStr, Len);
         Move(BufferPtr^, AnsiStr[1] , Len);
         Inc(BufferPtr, Len);
         Break;
      end
      else
        Len := (Len * 10) + PByte(BufferPtr)^ - 48;
    until False;
  end;

  procedure GetInt64(var IntValue: Int64);
  begin
    IntValue := 0;
    while (BufferPtr^ <> 'e') do
    begin
      if BufferPtr^= '-' then
        IntValue := IntValue * -1
      else
        IntValue := (IntValue * 10) +  PByte(BufferPtr)^ - 48;
      Inc(BufferPtr);
    end;
    Inc(BufferPtr);
  end;

var
  Buffer: AnsiString;
  Data: TBEncodedData;

begin
  inherited Create;
  FBufferStart := NativeUInt(BufferPtr);
  if BufferPtr^ = 'i' then
  begin
    Buffer := '';
    Inc(BufferPtr);
    GetInt64(IntegerData);
  end
  else if BufferPtr^ = 'l' then
  begin
    Format := befList;
    Inc(BufferPtr);
    repeat
      if BufferPtr^ = 'e' then begin Inc(BufferPtr); Break; end;
      ListData.Add(TBEncodedData.Create(TBEncoded.Create(BufferPtr)));
    until False;
  end
  else if  BufferPtr^ = 'd' then
  begin
    Format := befDictionary;
    Inc(BufferPtr);
    repeat
      if BufferPtr^ = 'e' then begin Inc(BufferPtr); Break; end;
      GetString(Buffer);
      Data := TBEncodedData.Create(TBEncoded.Create(BufferPtr));
      Data.Header := Buffer;
      ListData.Add(Data);
    until False;
  end
  else if BufferPtr^ in ['0'..'9'] then
  begin
    GetString(StringData);
    Format := befString;
  end
    else FormatException;
  FBufferEnd := NativeUInt(BufferPtr);
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

{ TBEncodedDataList }

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
