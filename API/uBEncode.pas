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
    FBufferEndPtr: NativeUInt;
    FBufferStartPtr: NativeUInt;
    FFormat: TBEncodedFormat;
  public
    IntegerData: Int64;
    ListData: TBEncodedDataList;
    StringData: AnsiString;
    class procedure Encode(Encoded: TBEncoded; Output: TStringBuilder);
    constructor Create(var BufferPtr: PAnsiChar; BufferEndPtr: PAnsiChar);
    destructor Destroy; override;
    property Format: TBEncodedFormat read FFormat;
    property BufferStartPtr: NativeUInt read FBufferStartPtr;
    property BufferEndPtr: NativeUInt read FBufferEndPtr;
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

destructor TBEncoded.Destroy;
begin
  if ListData <> nil then ListData.Free;
  inherited Destroy;
end;

procedure CheckPtrRangeInline(BufferPtr, BufferEndPtr: PAnsiChar; Offset: NativeUInt = 1); inline
begin
  if BufferPtr + Offset > BufferEndPtr then FormatException;
end;

procedure IncPtrCheck(var BufferPtr, BufferEndPtr: PAnsiChar); inline;
begin
   CheckPtrRangeInline(BufferPtr, BufferEndPtr, 1);
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
         CheckPtrRangeInline(BufferPtr, BufferEndPtr, Len + 1);
         Inc(BufferPtr);
         SetLength(AnsiStr, Len);
         Move(BufferPtr^, AnsiStr[1], Len);
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
        IntValue := (IntValue * 10) +  PByte(BufferPtr)^ - 48;
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

  if BufferPtr^ = 'i' then
  begin
    FFormat := befInteger;
    IncPtrCheck(BufferPtr, BufferEndPtr);
    DecodeInt64(IntegerData);
  end
  else if BufferPtr^ = 'l' then
  begin
    FFormat := befList;
    ListData := TBEncodedDataList.Create;
    IncPtrCheck(BufferPtr, BufferEndPtr);
    repeat
      if BufferPtr^ = 'e' then begin IncPtrCheck(BufferPtr, BufferEndPtr); Break; end;
      ListData.Add(TBEncodedData.Create(TBEncoded.Create(BufferPtr, BufferEndPtr)));
    until False;
  end
  else if  BufferPtr^ = 'd' then
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
  end
  else
  begin
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
  Result := nil;
  for var i := 0 to Count - 1 do
    if Items[i].Header = Header then
    begin
      Result := Items[i].Data;
      Break;
    end;
end;

end.
