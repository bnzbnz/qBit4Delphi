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
      System.Generics.Defaults;

const
  BStr: array[boolean] of string = ('false','true');

function URLEncode(Url: string): string; inline;
function URLEncodeStringList(Str: string; Delimiter: Char): string;
function IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant; overload;
function IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject; overload;
function GetRTTIReadableValues(Instance: Pointer; ObjectClass: TClass): TDictionary<string, Variant>;
procedure VariantMerge(var Src: Variant; Dst, Def: Variant); inline;

// RTTI to TDictionary;

implementation
uses Classes, RTTI, System.Net.URLClient, Variants;

function URLEncode(Url: string): string; inline;
begin
  // Having issues with TNetEncoding.URL.Encode()   (space being + instead of %20...)
  {$WARNINGS OFF}
  Result := System.Net.URLClient.TURI.URLEncode(Url);
  {$WARNINGS ON}
end;

function URLEncodeStringList(Str: string; Delimiter: Char): string;
begin
  var List := TStringList.Create;
  List.StrictDelimiter := True;
  List.Delimiter := Delimiter;
  List.DelimitedText := Str;
  for var i := 0 to List.Count - 1 do
    List[i] := URLEncode(List[i]);
  Result := List.DelimitedText;
  List.Free;
end;

function IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

function IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

function GetRTTIReadableValues(Instance: Pointer; ObjectClass: TClass): TDictionary<string, Variant>;
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

procedure VariantMerge(var Src: Variant; Dst, Def: Variant); inline;
begin
  if not VarIsEmpty(Dst) then Src := Dst else if not VarIsNull(Def) then Src := Def;
end;


end.
