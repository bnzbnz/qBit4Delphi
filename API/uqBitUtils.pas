unit uqBitUtils;

///
///  Author:  Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///
///  https://github.com/bnzbnz/qBit4Delphi:
///
///  License: MPL 1.1 / GPL 2.1
///

interface
uses  System.Generics.Collections, REST.JsonReflect, system.JSON, REST.Json.Types,
      System.Generics.Defaults, Classes;

const
  BLStr: array[boolean] of string = ('false','true');
  BCStr: array[boolean] of string = ('False','True');
  BUStr: array[boolean] of string = ('FALSE','TRUE');

type

  TqBitUtils = class

    // URL Encoding
    class function  URLEncode(Url: string): string; inline;
    class function  URLEncodeDelimStr(Str: string; Delim: Char): string;
    // IIF
    class function  IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant; overload; inline;
    class function  IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject; overload; inline;
    // Variants
    class procedure MergerVariant(var Src: Variant; Dst, Def: Variant); inline;
    // RTTI
    class function  GetRTTIReadableValues(Instance: Pointer; ObjectClass: TClass): TDictionary<string, Variant>;
    // StringList
    class function  DelimStringList(StringList: TStringList; Delimiter: Char = '|'; DelimitedText: string = ''): TStringList;
    // DateTime Timestamp conversion
    class function  TimestampToDateTime(Timestamp: int64): TDatetime;
    class function  TimestampMsToDateTime(TimestampMs: int64): TDatetime;
    class procedure TimestampToNow(Timestamp: int64; var Days, Hours, Mins, Secs: word);

  end;

implementation
uses RTTI, System.Net.URLClient, Variants, SysUtils, DateUtils;

class function TqBitUtils.URLEncode(Url: string): string;
begin
  // Having issues with TNetEncoding.URL.Encode()   (space being + instead of %20...)
  {$WARNINGS OFF}
  Result := System.Net.URLClient.TURI.URLEncode(Url);
  {$WARNINGS ON}
end;

class function TqBitUtils.URLEncodeDelimStr(Str: string; Delim: Char): string;
begin
  var List := TStringList.Create;
  List.StrictDelimiter := True;
  List.Delimiter := Delim;
  List.DelimitedText := Str;
  for var i := 0 to List.Count - 1 do
    List[i] := TqBitUtils.URLEncode(List[i]);
  Result := List.DelimitedText;
  List.Free;
end;

class function TqBitUtils.IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

class function TqBitUtils.IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

class function TqBitUtils.GetRTTIReadableValues(Instance: Pointer; ObjectClass: TClass): TDictionary<string, Variant>;
begin
  Result := TDictionary<string, Variant>.Create;
  var rttictx := TRttiContext.Create();
  try
    var rttitype := rttictx.GetType(ObjectClass);
    for var field in rttitype.GetFields do
    begin

      if field.FieldType.TypeKind in
          [
            tkInteger, tkChar, tkFloat,
            tkString, tkWChar, tkLString, tkWString,
            tkVariant, tkInt64,  tkUString, tkEnumeration
          ]
      then
        Result.Add(field.Name, field.GetValue(Instance).AsVariant)

    end;
  finally
    rttictx.Free;
  end;
end;

class procedure TqBitUtils.MergerVariant(var Src: Variant; Dst, Def: Variant);
begin
  if not VarIsEmpty(Dst) then Src := Dst else if not VarIsNull(Def) then Src := Def;
end;

class function TqBitUtils.DelimStringList(StringList: TStringList; Delimiter: Char = '|'; DelimitedText: string = ''): TStringList;
begin
  Result := TStringList( IIF(StringList = nil, TObject(TStringList.Create), TObject(StringList)) );
  Result.StrictDelimiter := True;
  Result.Delimiter := Delimiter;
  Result.DelimitedText := DelimitedText;;
end;

class function TqBitUtils.TimestampToDateTime(Timestamp: int64): TDatetime;
begin
  Result := TTimeZone.Local.ToLocalTime(UnixToDateTime(Timestamp));
end;

class function TqBitUtils.TimestampMsToDateTime(TimestampMs: int64): TDatetime;
begin
  Result := TimestampToDateTime(TimestampMs div 1000);
end;

class procedure TqBitUtils.TimestampToNow(Timestamp: int64; var Days, Hours, Mins, Secs: word);
begin
  var Dte := TqBitUtils.TimestampToDateTime(Timestamp);
  var diff := SecondsBetween(Now, Dte);
  days := diff div SecsPerDay;
  diff := diff mod SecsPerDay;
  hours := diff div SecsPerHour;
  diff := diff mod SecsPerHour;
  mins := diff div SecsPerMin;
  diff := diff mod SecsPerMin;
  secs := diff;
end;


end.
