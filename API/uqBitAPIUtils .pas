unit uqBitAPIUtils;

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
  BTStr: array[boolean] of string = ('False','True');
  BUStr: array[boolean] of string = ('FALSE','TRUE');

type

  TqBitAPIUtils = class

    // URL Encoding
    class function  URLEncode(Url: string): string; inline;
    class function  URLEncodeDelimStr(Str: string; Delim: Char): string;
    // IIF
    class function  IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant; overload; inline;
    class function  IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject; overload; inline;
    // Variants
    class procedure MergerVariants(var Src: Variant; Dst, Def: Variant); inline;
    // StringList
    class function  DelimStringList(StringList: TStringList; Delimiter: Char = '|'; DelimitedText: string = ''): TStringList;

  end;

implementation
uses RTTI, System.Net.URLClient, Variants;

class function TqBitAPIUtils.URLEncode(Url: string): string;
begin
  // Having issues with TNetEncoding.URL.Encode()   (space being + instead of %20...)
  {$WARNINGS OFF}
  Result := System.Net.URLClient.TURI.URLEncode(Url);
  {$WARNINGS ON}
end;

class function TqBitAPIUtils.URLEncodeDelimStr(Str: string; Delim: Char): string;
begin
  var List := TStringList.Create;
  List.StrictDelimiter := True;
  List.Delimiter := Delim;
  List.DelimitedText := Str;
  for var i := 0 to List.Count - 1 do
    List[i] := TqBitAPIUtils.URLEncode(List[i]);
  Result := List.DelimitedText;
  List.Free;
end;

class function TqBitAPIUtils.IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

class function TqBitAPIUtils.IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

class procedure TqBitAPIUtils.MergerVariants(var Src: Variant; Dst, Def: Variant);
begin
  if not VarIsEmpty(Dst) then Src := Dst else if not VarIsNull(Def) then Src := Def;
end;

class function TqBitAPIUtils.DelimStringList(StringList: TStringList; Delimiter: Char = '|'; DelimitedText: string = ''): TStringList;
begin
  Result := TStringList( IIF(StringList = nil, TObject(TStringList.Create), TObject(StringList)) );
  Result.StrictDelimiter := True;
  Result.Delimiter := Delimiter;
  Result.DelimitedText := DelimitedText;;
end;

end.
