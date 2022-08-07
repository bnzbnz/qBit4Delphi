unit uqBitFormat;

interface
uses uqBitAPITypes, uqBitObject;

type
  TVarDataFormater = function(v: variant): string;

function VarFormatString(v: variant): string;
function VarFormatDate(v: variant): string;
function VarFormatBKM(v: variant): string;
function VarFormatBKMPerSec(v: variant): string;
function VarFormatPercent(v: variant): string;
function VarFormatFloat2d(v: variant): string;
function VarFormatMulti(v: variant): string;
function VarFormatLimit(v: variant): string;
function VarFormatDeltaSec(v: variant): string;
function VarFormatDuration(v: variant): string;

// Specific
function VarFormatTrackerStatus(v: variant): string;

 // Other
function TitleCase(const S: string): string;

implementation
uses variants, SysUtils, DateUtils;

function VarFormatString(v: variant): string;
begin
  Result := VarToStr(v);
end;

function VarFormatDate(v: variant): string;
begin
  Result := '';
  if v <= 0 then Exit;
  Result := DateTimeToStr(TqBitObject.UTimestampToDateTime(v));
end;

function VarFormatBKM(v: variant): string;
var
  x: Double;
begin
    //x := Abs(v);
    Result := '0 B';
    x := v;
    if x < 0 then
    begin
      Result := 'N/A';
      Exit;
    end else
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

function VarFormatBKMPerSec(v: variant): string;
begin
  Result := VarFormatBKM(v) + '/s';
end;

function VarFormatPercent(v: variant): string;
var
  x: double;
begin
    Result := '0 %';
    if v < 0 then Exit;
    x := v;
    Result := Format('%.2f', [x * 100] )+ ' %';
end;

function VarFormatFloat2d(v: variant): string;
var
  x: double;
begin
  x := v;
  Result := Format('%.2f', [x] );
end;

function VarFormatMulti(v: variant): string;
var
  x: double;
begin
    Result := '';
    if v < 0 then Exit;
    x := v;
    Result := Format('%.2f', [x] )+ ' x';
end;

function VarFormatLimit(v: variant): string;
begin
  Result := '∞';
  if v > 0 then Result := VarFormatBKM(v);
end;

function VarFormatDeltaSec(v: variant): string;
begin
  Result := DateTimeToStr(IncSecond(Now, v));
end;

function VarFormatDuration(v: variant): string;
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

function VarFormatTrackerStatus(v: variant): string;
begin
  case v of
    0: Result := 'Disabled';
    1: Result := 'Not Contacted';
    2: Result := 'Working';
    3: Result := 'Updating';
    4: Result := 'Error';
  else
    Result := 'Unknown';
  end;
end;

end.
