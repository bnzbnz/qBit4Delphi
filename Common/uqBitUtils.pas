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

function IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant;
function GetRTTIReadableValues(Instance: Pointer; ObjectClass: TClass): TDictionary<string, Variant>;

// RTTI to TDictionary;

implementation
uses RTTI;

function IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant;
begin
  if Condition then
    Result := IsTrue
  else
    Result := IsFalse;
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

end.
