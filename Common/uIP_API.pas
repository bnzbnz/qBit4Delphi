unit uIP_API;

///
///  Author:  Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///
///  https://github.com/bnzbnz/qBit4Delphi:
///  https://ip-api.com/docs/api:json
///
///  License: MPL 1.1 / GPL 2.1
///

interface

type

  TIP_API = class
    Fstatus: string;
    Fmessage: string;
    Fcontinent: string;
    Fcountry: string;
    FcountryCode: string;
    Fregion: string;
    FregionName: string;
    Fcity: string;
    Fdistrict: string;
    Fzip: string;
    Flat: real;
    Flon: real;
    Ftimezone: string;
    Foffset: int64;
    Fcurrency: string;
    fisp: string;
    Forg: string;
    Fas: string;
    Fasname: string;
    Freverse: string;
    Fmobile: boolean;
    Fproxy: boolean;
    Fhosting: boolean;
    Fquery: string;
    class function FromJSON(JSONStr: string): TIP_API;
    class function FromURL(
      IPorDomain: string = '';
      Fields: string = 'status,message,continent,country,countryCode,region,regionName,city,zip,lat,lon,timezone,offset,currency,isp,org,as,asname,reverse,mobile,proxy,hosting,query';
      URL: String = 'http://ip-api.com/json/'
    ): TIP_API;
  end;

implementation
uses  REST.Json,
      System.Net.HttpClient,
      System.SysUtils, System.DateUtils, System.Classes;

{ TIP_API }

class function TIP_API.FromJSON(JSONStr: string): TIP_API;
begin
  try
    Result := TJSon.JsonToObject<TIP_API>(JSONStr);
  except
    Result := nil;
  end;
end;

// IPorDomain = '' is your External IP  - Fields description at https://ip-api.com/docs/api:json
class function TIP_API.FromURL(IPorDomain: string; Fields: string; URL: string): TIP_API;
var
  Http: THTTPClient;
  ReqSS: TStringStream;
begin
  Result := nil; Http := nil; ReqSS := nil;
  try
    try
      ReqSS := TStringStream.Create('', TEncoding.UTF8);
      Http := THTTPClient.Create;
      var Res := Http.Get(URL+ '/' + IPorDomain + '?fields=' + Fields, ReqSS);
      if Res.StatusCode = 200 then
        Result := TIP_API.FromJSON(ReqSS.DataString);
    except
      Result := nil;
    end;
  finally
    ReqSS.Free;
    Http.Free;
  end;
end;

end.
