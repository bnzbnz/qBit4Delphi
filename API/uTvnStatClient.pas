// usage:
// use a server with vnstat, webserver(nginx ,lighthttpd, apache...), php
// create aphp script:  <?php passthru('vnstat --json'); ?>
// TEST IT with your web browser (the passthru function may have been disbled in php.ini)
// then provide the full URL to the class function GetURL
// You'll get a TVNStat which contains all the stats (nil if it fails)

// ex: current usage
// var Vns := TVNStat.GetURL('https://www.mydomain.com/vn.php');
// var M := Vns.GetCurrentMonth;
// Memo1.Lines.Add(FloatToStr(TVNStat.BtoTB(M.Ftx)));
// Memo1.Lines.Add(FloatToStr(TVNStat.BtoTB(M.Frx)));
// Memo1.Lines.Add(FloatToStr(TVNStat.BtoTB(M.Ftx + M.Frx)));
// Vns.Free;

unit uTvnStatClient;

interface

type

  TvnsDate = class
    Fyear: Int64;
    Fmonth: Int64;
    Fday: Int64;
  end;

  TvnsTime = class
    Fhour: Int64;
    Fminute: Int64;
  end;

  TvnsDateCtx = class
     Fdate: TvnsDate;
     destructor Destroy; override;
  end;

  TVNrxtx = class
    Frx: Int64;
    Ftx: Int64;
  end;

  TvnsInfo= class(TVNrxtx)
    Fid: Int64;
    Fdate: TvnsDate;
    Ftime: TvnsTime;
    destructor Destroy; override;
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
  private
    FRaw: string;
  public
    Fvnstatversion: string;
    Fjsonversion: string;
    Finterfaces: TArray<TvnsInterface>;
    class function GetURL(URL: string): TvnStatClient;
    class function BtoTiB(Byte: Int64): real;
    class function BtoTB(Byte: Int64): real;
    destructor Destroy; override;
    function GetInterface(NetInterface: string): TvnsInterface;
    function GetMonth(Year, Month: Int64): TvnsInfo;
    function GetCurrentMonth: TvnsInfo;

    property CurrentMonth: TvnsInfo read GetCurrentMonth;
    property Raw: string read FRaw;
  end;

implementation
uses REST.Json, System.Net.HttpClient, System.SysUtils, System.Classes;


function TvnStatClient.GetMonth(Year, Month: Int64): TvnsInfo;
begin
  Result := nil;
  var Intf := Self.GetInterface('ens2f0');
  if Intf = nil then exit;
  for var i in Intf.Ftraffic.Fmonth do
    if (i.Fdate.Fyear = Year) and (i.Fdate.Fmonth = Month) then
    begin
      Result := i;
      exit;
    end;
end;

function TvnStatClient.GetCurrentMonth: TvnsInfo;
var
  AYear, AMonth, ADay: Word;
begin
  Result := nil;
  var Intf := Self.GetInterface('ens2f0');
  if Intf = nil then exit;
  DecodeDate(Now, AYear, AMonth, ADay);
  Result := GetMonth(AYear, AMonth);
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
  Self.Ftotal.Free;
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

class function TvnStatClient.GetURL(URL: string): TvnStatClient;
var
   Http: THTTPClient;
   ReqSS: TStringStream;
begin
  Result := nil; Http := nil; ReqSS := nil;
  try
    ReqSS := TStringStream.Create('');
    Http := THTTPClient.Create;
    Http.ConnectionTimeout := 500;
    var Res :=  Http.Get(URL, ReqSS);
    if Res.StatusCode = 200 then
    begin
      Result := TJSon.JsonToObject<TvnStatClient>(ReqSS.DataString);
      if Result <> nil then Result.FRaw := ReqSS.DataString;
    end;
  finally
    ReqSS.Free;
    Http.Free;
  end;
end;

end.
