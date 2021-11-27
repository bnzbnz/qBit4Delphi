unit uqBitFormat;

interface
uses uqBitAPITypes, uqBitObject;

type
   TVarDataFormater = function(v: variant; Obj: TqBitTorrentBaseType): string;

function VarFormatString(v: variant; Obj: TqBitTorrentBaseType): string;
function VarFormatDate(v: variant; Obj: TqBitTorrentBaseType): string;
function VarFormatBKM(v: variant; Obj: TqBitTorrentBaseType): string;
function VarFormatPercent(v: variant; Obj: TqBitTorrentBaseType): string;
function VarFormatMulti(v: variant; Obj: TqBitTorrentBaseType): string;
function VarFormatLimit(v: variant; Obj: TqBitTorrentBaseType): string;
function VarFormatDetaSec(v: variant; Obj: TqBitTorrentBaseType): string;

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
  x: double;
begin
    x := v;
    Result := '0 B';
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

function VarFormatPercent(v: variant; Obj: TqBitTorrentBaseType): string;
var
  x: double;
begin
    Result := '';
    if v < 0 then Exit;
    x := v;
    Result := Format('%.2f', [x * 100] )+ ' %';
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

function VarFormatDetaSec(v: variant; Obj: TqBitTorrentBaseType): string;
begin
  Result := DateTimeToStr(IncSecond(Now, v));
end;

end.
