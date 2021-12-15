unit uqBitFormat;

interface
uses uqBitAPITypes, uqBitObject;

type
   TVarDataFormater = function(v: variant; Obj: TqBitTorrentBaseType): string;

function VarFormatString(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatDate(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatBKM(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatBKMPerSec(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatPercent(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatFloat2d(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatMulti(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatLimit(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatDeltaSec(v: variant; Obj: TqBitTorrentBaseType = nil): string;
function VarFormatDuration(v: variant; Obj: TqBitTorrentBaseType = nil): string;

 // Other
function TitleCase(const S: string): string;

implementation
uses variants, SysUtils, DateUtils;

function VarFormatString(v: variant; Obj: TqBitTorrentBaseType): string;
begin
  Result := VarToStr(v);

end;

function VarFormatDate(v: variant; Obj: TqBitTorrentBaseType): string;
begin
  Result := '';
  if v <= 0 then Exit;
  Result := DateTimeToStr(TqBitObject.TStoDT(v));
end;

function VarFormatBKM(v: variant; Obj: TqBitTorrentBaseType): string;
var
  x: Double;
begin
    x := Abs(v);
    Result := '0 B';
    if (x / 1099511627776 >= 1) then
    begin
      Result := Format('%.2f', [x / 1099511627776 ])+ ' TiB';
      Exit;
    end else

    if (x / (1024 * 1024 * 1024) >= 1) then
    begin
      Result := Format('%.2f', [x /(1024 * 1024 * 1024)] )+ ' GiB';
      Exit;
    end else
    if (x / (1024 * 1024)>= 1) then
    begin
      Result := Format('%.2f', [x /(1024 * 1024)] )+ ' MiB';
      Exit;
    end else
    if (x / (1024)) >= 1 then
    begin
      Result := Format('%.2f', [x /(1024)] )+ ' KiB';
    end else begin
      Result := Format('%.0f', [x] )+ ' B';
    end;
end;

function VarFormatBKMPerSec(v: variant; Obj: TqBitTorrentBaseType): string;
begin
  Result := VarFormatBKM(v, Obj) + '/s';
end;

function VarFormatPercent(v: variant; Obj: TqBitTorrentBaseType): string;
var
  x: double;
begin
    Result := '0 %';
    if v < 0 then Exit;
    x := v;
    Result := Format('%.2f', [x * 100] )+ ' %';
end;

function VarFormatFloat2d(v: variant; Obj: TqBitTorrentBaseType): string;
var
  x: double;
begin
  x := v;
  Result := Format('%.2f', [x] );
end;

function VarFormatMulti(v: variant; Obj: TqBitTorrentBaseType): string;
var
  x: double;
begin
    Result := '';
    if v < 0 then Exit;
    x := v;
    Result := Format('%.2f', [x] )+ ' x';
end;

function VarFormatLimit(v: variant; Obj: TqBitTorrentBaseType): string;
begin
  Result := '∞';
  if v > 0 then Result := VarFormatBKM(v, Obj);
end;

function VarFormatDeltaSec(v: variant; Obj: TqBitTorrentBaseType): string;
begin
  Result := DateTimeToStr(IncSecond(Now, v));
end;

function VarFormatDuration(v: variant; Obj: TqBitTorrentBaseType): string;
var
  Days, Hours, Mins, Secs: word;
begin
  var totalsecs := Int64(v);
  days := totalsecs div SecsPerDay;
  totalsecs := totalsecs mod SecsPerDay;
  hours := totalsecs div SecsPerHour;
  totalsecs := totalsecs mod SecsPerHour;
  mins := totalsecs div SecsPerMin;
  totalsecs := totalsecs mod SecsPerMin;
  secs := totalsecs;
  if days >0 then
    Result := Result + days.ToString + 'd ';
  Result := Result + hours.ToString + 'h ';
  Result := Result + mins.ToString + 'm ';
  if days = 0 then
    Result := Result + secs.ToString + 's ';
end;

function TitleCase(const S: string): string;
var
  IsUpper: boolean;
  i : Byte;
begin
  IsUpper := true;
  Result := '';
  for i := 1 to Length(S) do
  begin
    if IsUpper then
       Result := Result + UpperCase(S[i])
    else
       Result := Result + S[i];
    IsUpper := (S[i] = ' ')
  end;
end;



end.
