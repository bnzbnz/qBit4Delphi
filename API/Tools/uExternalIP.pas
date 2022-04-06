unit uExternalIP;

interface

type

  TExternalIP = class
    Fip: String;
    Fhostname: String;
    Fcity: String;
    Fregion: String;
    Floc: String;
    Forg: String;
    Fpostal: String;
    Ftimezone: String;
    Freadme: String;

    class function Get(URL: String = 'http://ipinfo.io/json'): TExternalIP;
  end;
implementation

{ TExternalIP }

class function TExternalIP.Get(URL: String): TExternalIP;
begin

end;

end.
