unit uExternalIP;

interface

type

  TExternalIP = class
    Fip: AnsiString;
    Fhostname: AnsiString;
    Fcity: AnsiString;
    Fregion: AnsiString;
    Fcountry: AnsiString;
    Floc: AnsiString;
    Forg: AnsiString;
    Fpostal: AnsiString;
    Ftimezone: AnsiString;
    Freadme: AnsiString;
    class function FromJSON(JSONStr: string): TExternalIP;
    class function FromURL(URL: String = 'http://ipinfo.io/json'): TExternalIP;
  end;

implementation
uses  REST.Json,
      System.Net.HttpClient,
      System.SysUtils, System.DateUtils, System.Classes;

{ TExternalIP }

class function TExternalIP.FromJSON(JSONStr: string): TExternalIP;
begin
  try
    Result := TJSon.JsonToObject<TExternalIP>(JSONStr);
  except
    Result := nil;
  end;
end;

class function TExternalIP.FromURL(URL: String): TExternalIP;
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
    var Res := Http.Get(URL, ReqSS);
    if Res.StatusCode = 200 then
      Result := TExternalIP.FromJSON(ReqSS.DataString);
  finally
    ReqSS.Free;
    Http.Free;
  end;
  except
  end;
end;

end.
