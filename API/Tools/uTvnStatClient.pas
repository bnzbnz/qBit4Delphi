///  Author: Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///  version: 1.0.0
//
///  https://humdi.net/vnstat/
///  https://en.wikipedia.org/wiki/VnStat
///

// usage:
// use a server running vnstat, a webserver(nginx, lighthttpd, apache...), php
// create a php script:  <?php passthru('vnstat --json'); ?>
// TEST IT with your web browser (the passthru function may have been disbled in php.ini)
// then provide the full URL to the class function FromURL
// You'll get a TvnStatClient which contains all the stats (nil if it fails)

unit uTvnStatClient;

interface

type

  TvnsDate = class
    Fyear: Int64;
    Fmonth: Int64;
    Fday: Int64;
    function GetDate: TDate;
  end;

  TvnsTime = class
    Fhour: Int64;
    Fminute: Int64;
    function GetTime: TTime;
  end;

  TvnsDateCtx = class
     Fdate: TvnsDate;
     destructor Destroy; override;
     function GetDate: TDate;
  end;

  TVNrxtx = class
    Frx: Int64;
    Ftx: Int64;
  end;

  TvnsInfo= class(TVNrxtx)
    Fid: Int64;
    Fdate: TvnsDate;
    Ftime: TvnsTime;
    // Frx: Int64; << from TVNrxtx
    // Ftx: Int64; << from TVNrxtx
    destructor Destroy; override;
    function GetDateTime: TDateTime;
  end;

  TvnsTraffic = class
    Ftotal: TVNrxtx;
    Ffiveminute: TArray<TvnsInfo>;
    Fhour: TArray<TvnsInfo>;
    Fday: TArray<TvnsInfo>;
    Fmonth: TArray<TvnsInfo>;
    Fyear: TArray<TvnsInfo>;
    Ftop: TArray<TvnsInfo>;
    destructor Destroy; override;
  end;

  TvnsInterface = class
    Fname: string;
    Falias: string;
    Fcreated: TvnsDateCtx;
    Fupdated: TvnsDateCtx;
    Ftraffic: TvnsTraffic;
    destructor Destroy; override;
  end;

  TvnStatClient = class
  public
    Fraw: string; // Custom not a json field
    Fvnstatversion: string;
    Fjsonversion: string;
    Finterfaces: TArray<TvnsInterface>;

    class function FromJSON(JSONStr: string): TvnStatClient;
    class function FromURL(URL: string): TvnStatClient;
    class function BtoTiB(Byte: Int64): real;
    class function BtoTB(Byte: Int64): real;
    destructor Destroy; override;
    function GetInterface(NetInterface: string): TvnsInterface;
    function GetYear(Intf: string; Year: Int64): TvnsInfo;
    function GetMonth(Intf: string; Year, Month: Int64): TvnsInfo;
    function GetDay(Intf: string; Year, Month, Day: Int64): TvnsInfo;
    function GetCurrentYear(Intf: string): TvnsInfo;
    function GetCurrentMonth(Intf: string): TvnsInfo;
    function GetCurrentDay(Intf: string): TvnsInfo;
  end;

implementation
uses  REST.Json,
      System.Net.HttpClient,
      System.SysUtils, System.DateUtils, System.Classes;

{ TvnStatClient }

function TvnStatClient.GetYear(Intf: string; Year : Int64): TvnsInfo;
begin
  Result := nil;
  var Intrface := Self.GetInterface(Intf);
  if Intrface = nil then exit;
  for var i in Intrface.Ftraffic.Fyear do
    if (i.Fdate.Fyear = Year) then
    begin
      Result := i;
      break;
    end;
end;

function TvnStatClient.GetMonth(Intf: string; Year, Month: Int64): TvnsInfo;
begin
  Result := nil;
  var Intrface := Self.GetInterface(Intf);
  if Intrface = nil then exit;
  for var i in Intrface.Ftraffic.Fmonth do
    if (i.Fdate.Fyear = Year) and (i.Fdate.Fmonth = Month) then
    begin
      Result := i;
      break;
    end;
end;

function TvnStatClient.GetDay(Intf: string; Year, Month, Day: Int64): TvnsInfo;
begin
  Result := nil;
  var Intrface := Self.GetInterface(Intf);
  if Intrface = nil then exit;
  for var i in Intrface.Ftraffic.Fmonth do
    if (i.Fdate.Fyear = Year) and (i.Fdate.Fmonth = Month) and (i.Fdate.Fday = Day) then
    begin
      Result := i;
      break;
    end;
end;

function TvnStatClient.GetCurrentMonth(Intf: string): TvnsInfo;
var
  AYear, AMonth, ADay: Word;
begin
  Result := nil;
  var Intrface := Self.GetInterface(Intf);
  if Intrface = nil then exit;
  DecodeDate(Now, AYear, AMonth, ADay);
  Result := GetMonth(Intf, AYear, AMonth);
end;

function TvnStatClient.GetCurrentYear(Intf: string): TvnsInfo;
var
  AYear, AMonth, ADay: Word;
begin
  Result := nil;
  var Intrface := Self.GetInterface(Intf);
  if Intrface = nil then exit;
  DecodeDate(Now, AYear, AMonth, ADay);
  Result := GetMonth(Intf, AYear, AMonth);
end;

function TvnStatClient.GetCurrentDay(Intf: string): TvnsInfo;
var
  AYear, AMonth, ADay: Word;
begin
  Result := nil;
  var Intrface := Self.GetInterface(Intf);
  if Intrface = nil then exit;
  DecodeDate(Now, AYear, AMonth, ADay);
  Result := GetDay(Intf, AYear, AMonth, ADay);
end;

class function TvnStatClient.BtoTB(Byte: Int64): real;
begin
  Result := Byte / 1000000000000;
end;

class function TvnStatClient.BtoTiB(Byte: Int64): real;
begin
  Result := Byte / 1099511627776;
end;

destructor TvnStatClient.Destroy;
begin
  for var i in Finterfaces do i.Free;
  inherited Destroy;
end;

destructor TvnsTraffic.Destroy;
begin
  Ftotal.Free;
  for var i in Ffiveminute do i.Free;
  for var i in Fhour do i.Free;
  for var i in Fday do i.Free;
  for var i in Fmonth do i.Free;
  for var i in Fyear do i.Free;
  for var i in Ftop do i.Free;
  inherited;
end;

{ TVNDateCtx }

destructor TvnsDateCtx.Destroy;
begin
  Self.Fdate.Free;
  inherited;
end;

{ TVNInterface }

destructor TvnsInterface.Destroy;
begin
  Self.Fcreated.Free;
  Self.Fupdated.Free;
  Self.Ftraffic.Free;
  inherited;
end;

{ TVNStatData }

destructor TvnsInfo.Destroy;
begin
  Self.Fdate.Free;
  Self.Ftime.Free;
  inherited;
end;

function TvnStatClient.GetInterface(NetInterface: string): TvnsInterface;
begin
  for var i in Finterfaces do
    if i.Fname = NetInterface then
    begin
        Result := i;
        exit;
    end;
  Result := nil;
end;

class function TvnStatClient.FromJSON(JSONStr: string): TvnStatClient;
begin
  try
    Result := TJSon.JsonToObject<TvnStatClient>(JSONStr);
  except
    Result := nil;
  end;
end;

class function TvnStatClient.FromURL(URL: string): TvnStatClient;
var
  Http: THTTPClient;
  ReqSS: TStringStream;
begin
  Result := nil; Http := nil; ReqSS := nil;
  try
  try
    ReqSS := TStringStream.Create('');
    Http := THTTPClient.Create;
    Http.ConnectionTimeout := 500;
    var Res :=  Http.Get(URL, ReqSS);
    if Res.StatusCode = 200 then
    begin
      Result := TvnStatClient.FromJSON(ReqSS.DataString);
      if Result <> nil then Result.FRaw := ReqSS.DataString;
    end;
  finally
    ReqSS.Free;
    Http.Free;
  end;
  except
  end;
end;

{ TvnsDate }

function TvnsDate.GetDate: TDate;
begin
  Result := EncodeDate(Fyear, Fmonth, Fday);
end;

{ TvnsTime }

function TvnsTime.GetTime: TTime;
begin
  Result := EncodeTime(Fhour, Fminute, 0, 0);
end;

{ TvnsDateCtx }

function TvnsDateCtx.GetDate: TDate;
begin
  Result := EncodeDate(Fdate.Fyear, Fdate.Fmonth, Fdate. Fday);
end;

{ TvnsInfo }

function TvnsInfo.GetDateTime: TDateTime;
begin
  Result := EncodeDateTime(
    Self.Fdate.Fyear,
    Self.Fdate.Fmonth,
    Self.Fdate.Fday,
    Self.Ftime.Fhour,
    Self.Ftime.Fminute,
    0,
    0
  );
end;

end.
