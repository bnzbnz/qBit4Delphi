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

type

  TqBitUtils = class

    // RTTI
    class function  GetRTTIReadableValues(Instance: Pointer; ObjectClass: TClass): TDictionary<string, Variant>;
    // DateTime Timestamp conversion
    class function  TimestampToDateTime(Timestamp: int64): TDatetime;
    class function  TimestampMsToDateTime(TimestampMs: int64): TDatetime;
    class procedure TimestampToNow(Timestamp: int64; var Days, Hours, Mins, Secs: word);

  end;

implementation
uses RTTI, SysUtils, DateUtils;

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
