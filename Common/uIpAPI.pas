unit uIpAPI;

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

  TIpAPI = class
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
    class function FromJSON(JSONStr: string): TIpAPI;
    class function FromURL(
      IPorDomain: string = '';
      Fields: string = 'status,message,continent,country,countryCode,region,regionName,city,zip,lat,lon,timezone,offset,currency,isp,org,as,asname,reverse,mobile,proxy,hosting,query';
      URL: String = 'http://ip-api.com/json/'
    ): TIpAPI;
  end;

implementation
uses  REST.Json,
      System.Net.HttpClient,
      System.SysUtils, System.DateUtils, System.Classes;

{ TIpAPI }

class function TIpAPI.FromJSON(JSONStr: string): TIpAPI;
begin
  try
    Result := TJSon.JsonToObject<TIpAPI>(JSONStr);
  except
    Result := nil;
  end;
end;

// IPorDomain = '' is your External IP  - Fields description at https://ip-api.com/docs/api:json
class function TIpAPI.FromURL(IPorDomain: string; Fields: string; URL: string): TIpAPI;
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
        Result := TIpAPI.FromJSON(ReqSS.DataString);
    except
      Result := nil;
    end;
  finally
    ReqSS.Free;
    Http.Free;
  end;
end;

end.
