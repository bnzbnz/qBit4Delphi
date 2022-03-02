unit uqBitObject;

interface

///  Author: Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///  API v2.8.3 + Hidden/Missing Fields
///
///  https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)
///

uses classes, uqBitAPI, uqBitAPITypes;

type

  TqBitObject = class(TqBitAPI)
  public
    class function Connect(HostPath, Username, Password : string): TqBitObject;

     // Helpers
    function Clone: TqBitObject;
    function qBitAPIVersion: string;
    class function UTimestampToDateTime(Timestamp: int64): TDatetime;
    class function UTimestampMsToDateTime(Timestamp: int64): TDatetime;
    class procedure TSDurationToNow(Timestamp: int64; var Days, Hours, Mins, Secs: word);

    // API Helpers
    function PauseTorrents(Hashes: TStringList): boolean; overload; virtual;
    function ResumeTorrents(Hashes: TStringList): boolean; overload; virtual;
    function DeleteTorrents(Hashes: TStringList; DeleteFiles: boolean = False): boolean; overload; virtual;
    function RecheckTorrents(Hashes: TStringList): boolean; overload; virtual;
    function ReannounceTorrents(Hashes: TStringList): boolean; overload;
    function AddTrackersToTorrent(Hash: string; Urls: TStringList): boolean; overload; virtual;
    function RemoveTrackers(Hash: string;  Urls: TStringList): boolean; overload; virtual;
    function AddPeers(Hashes: string; Peers: TStringList): boolean; overload; virtual;
    function AddPeers(Hashes, Peers: TStringList): boolean; overload; virtual;
    function IncreaseTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function DecreaseTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function MaximalTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function MainimalTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function SetfilesPriority(Hash: string; Ids: TStringList; Priority: integer): boolean; overload; virtual;
    function GetTorrentDownloadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType; overload; virtual;
    function SetTorrentDownloadLimit(Hashes: TStringList; Limit: integer): boolean; overload; virtual;
    function SetTorrentShareLimit(Hashes: TStringList; RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;
    function GetTorrentUploadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType; overload; virtual;
    function SetTorrentUploadLimit(Hashes: TStringList; Limit: integer): boolean; overload; virtual;
    function SetTorrentLocation(Hashes: TStringList; Location: string): boolean; overload; virtual;
    function SetTorrentCategory(Hashes: TStringList; Category: string): boolean; overload; virtual;
    function RemoveCategories(Categories: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Hashes: string; Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Hashes: TStringList; Tags: string): boolean; overload; virtual;
    function AddTorrentTags(Hashes, Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Hashes: string; Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Hashes: TStringList; Tags: string): boolean; overload; virtual;
    function RemoveTorrentTags(Hashes, Tags: TStringList): boolean; overload; virtual;
    function CreateTags(Tags: TStringList): boolean; overload; virtual;
    function DeleteTags(Tags: TStringList): boolean; overload; virtual;
    function AddNewTorrentUrl(Url: string): boolean;
    function SetAutomaticTorrentManagement(Hashes: TStringList; Enable: boolean): boolean; overload; virtual;
    function ToggleSequentialDownload(Hashes: TStringList): boolean; overload; virtual;
    function SetFirstLastPiecePriority(Hashes: TStringList): boolean; overload; virtual;
    function SetForceStart(Hashes: TStringList; value: boolean): boolean; overload; virtual;
    function SetSuperSeeding(Hashes: TStringList; value: boolean): boolean; overload; virtual;

    property HostPath: string read FHostPath;
    property Username: string read FUsername;
    property Password: string read FPassword;
    property Duration: cardinal read FDuration;
    property HTTPStatus: integer read FHTTPStatus;
    property HTTPConnectionTimeout: integer read FHTTPConnectionTimeout;
    property HTTPSendTimeout: integer read FHTTPSendTimeout;
    property HTTPResponseTimeout: integer read FHTTPResponseTimeout;
    property HTTPRetries: integer read FHTTPRetries;
  end;

implementation
uses SysUtils, DateUtils;

{ TqBitObject }

class function TqBitObject.Connect(HostPath, Username, Password : string): TqBitObject;
begin
  Result := TqBitObject.Create(HostPath);
  if not Result.Login(Username, Password) then
    FreeAndNil(Result);
end;

function TqBitObject.Clone: TqBitObject;
begin
  Result := TqBitObject.Create(FHostPath);
  Result.FSID := FSID;
  Result.FHTTPConnectionTimeout := FHTTPConnectionTimeout;
  Result.FHTTPSendTimeout := FHTTPSendTimeout;
  Result.FHTTPResponseTimeout := FHTTPResponseTimeout;
  Result.FUsername := FUsername;
  Result.FPassword := FPassword;
end;

class function TqBitObject.UTimestampMsToDateTime(Timestamp: int64): TDatetime;
begin
  result := TTimeZone.Local.ToLocalTime(UnixToDateTime(Timestamp div 1000));
end;

class function TqBitObject.UTimestampToDateTime(Timestamp: int64): TDatetime;
begin
  result := TTimeZone.Local.ToLocalTime(UnixToDateTime(Timestamp));
end;

class procedure TqBitObject.TSDurationToNow(Timestamp: int64; var Days, Hours, Mins, Secs: word);
begin
  var Dte := UTimestampToDateTime(Timestamp);
  var diff := SecondsBetween(Now, Dte);
  days := diff div SecsPerDay;
  diff := diff mod SecsPerDay;
  hours := diff div SecsPerHour;
  diff := diff mod SecsPerHour;
  mins := diff div SecsPerMin;
  diff := diff mod SecsPerMin;
  secs := diff;
end;

function TqBitObject.PauseTorrents(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := PauseTorrents(Hashes.DelimitedText);
end;

function TqBitObject.qBitAPIVersion: string;
begin
  Result := Const_qBitAPI_Implemented_Version;
end;

function TqBitObject.ResumeTorrents(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := ResumeTorrents(Hashes.DelimitedText);
end;

function TqBitObject.DeleteTorrents(Hashes: TStringList; DeleteFiles: boolean = False): boolean;
begin
  Hashes.Delimiter := '|';
  Result := DeleteTorrents(Hashes.DelimitedText, DeleteFiles);
end;

function TqBitObject.RecheckTorrents(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := RecheckTorrents(Hashes.DelimitedText);
end;

function TqBitObject.ReannounceTorrents(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := ReannounceTorrents(Hashes.DelimitedText);
end;

function TqBitObject.AddTrackersToTorrent(Hash: string;  Urls: TStringList): boolean;
begin
  Urls.Delimiter := Chr($A);
  Result := AddTrackersToTorrent(Hash, Urls.DelimitedText);
end;

function TqBitObject.RemoveTrackers(Hash: string;  Urls: TStringList): boolean;
begin
  Urls.Delimiter := '|';
  Result := RemoveTrackers(Hash, Urls.DelimitedText);
end;

function TqBitObject.AddPeers(Hashes: string; Peers: TStringList): boolean;
begin
  Peers.Delimiter := '|';
  Result := AddPeers(Hashes, Peers.DelimitedText);
end;

function TqBitObject.AddNewTorrentUrl(Url: string): boolean;
var
  NewTorrentUrls: TqBitNewTorrentUrlsType;
begin
  NewTorrentUrls := TqBitNewTorrentUrlsType.Create;
  NewTorrentUrls.Furls.Add(Url);
  Result := AddNewTorrentUrls(NewTorrentUrls);
  NewTorrentUrls.Free;
end;

function TqBitObject.AddPeers(Hashes, Peers: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Peers.Delimiter := '|';
  Result := AddPeers(Hashes.DelimitedText, Peers.DelimitedText);
end;

function TqBitObject.IncreaseTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := IncreaseTorrentPriority(Hashes.DelimitedText);
end;

function TqBitObject.DecreaseTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := DecreaseTorrentPriority(Hashes.DelimitedText);
end;

function TqBitObject.MainimalTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := DecreaseTorrentPriority(Hashes.DelimitedText);
end;

function TqBitObject.MaximalTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := DecreaseTorrentPriority(Hashes.DelimitedText);
end;

function TqBitObject.SetfilesPriority(Hash: string; Ids: TStringList; Priority: integer): boolean;
begin
  Ids.Delimiter := '|';
  Result := SetfilesPriority(Hash, Ids.DelimitedText, Priority);
end;

function TqBitObject.GetTorrentDownloadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType;
begin
  Hashes.Delimiter := '|';
  Result := GetTorrentDownloadLimit(Hashes.DelimitedText);
end;

function TqBitObject.SetTorrentDownloadLimit(Hashes: TStringList; Limit: integer): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentDownloadLimit(Hashes.DelimitedText, Limit);
end;

function TqBitObject.SetTorrentShareLimit(Hashes: TStringList; RatioLimit: double; SeedingTimeLimit: integer): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentShareLimit(Hashes.DelimitedText, RatioLimit, SeedingTimeLimit);
end;

function TqBitObject.GetTorrentUploadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType;
begin
  Hashes.Delimiter := '|';
  Result := GetTorrentUploadLimit(Hashes.DelimitedText);
end;

function TqBitObject.SetTorrentUploadLimit(Hashes: TStringList; Limit: integer): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentUploadLimit(Hashes.DelimitedText, Limit);
end;

function TqBitObject.SetTorrentLocation(Hashes: TStringList;
  Location: string): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentLocation(Hashes.DelimitedText, Location);
end;

function TqBitObject.SetTorrentCategory(Hashes: TStringList; Category: string): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentCategory(Hashes.DelimitedText, Category);
end;

function TqBitObject.RemoveCategories(Categories: TStringList): boolean;
begin
  Categories.Delimiter := Chr($A);
  Result := RemoveCategories(Categories.DelimitedText);
end;

function TqBitObject.AddTorrentTags(Hashes: string; Tags: TStringList): boolean;
begin
  Tags.Delimiter := ',';
  Result := AddTorrentTags(Hashes, Tags.DelimitedText);
end;

function TqBitObject.AddTorrentTags(Hashes: TStringList; Tags: string): boolean;
begin
  Hashes.Delimiter := ',';
  Result := AddTorrentTags(Hashes.DelimitedText, Tags);
end;

function TqBitObject.AddTorrentTags(Hashes, Tags: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Tags.Delimiter := ',';
  Result := AddTorrentTags(Hashes.DelimitedText, Tags.DelimitedText);
end;


function TqBitObject.RemoveTorrentTags(Hashes: string; Tags: TStringList): boolean;
begin
  Tags.Delimiter := ',';
  Result := RemoveTorrentTags(Hashes, Tags.DelimitedText);
end;

function TqBitObject.RemoveTorrentTags(Hashes: TStringList; Tags: string): boolean;
begin
  Hashes.Delimiter := '|';
  Result := RemoveTorrentTags(Hashes.DelimitedText, Tags);
end;

function TqBitObject.RemoveTorrentTags(Hashes, Tags: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Tags.Delimiter := ',';
  Result := RemoveTorrentTags(Hashes.DelimitedText, Tags.DelimitedText);
end;

function TqBitObject.CreateTags(Tags: TStringList): boolean;
begin
  Tags.Delimiter := ',';
  Result := CreateTags(Tags.DelimitedText);
end;

function TqBitObject.DeleteTags(Tags: TStringList): boolean;
begin
  Tags.Delimiter := ',';
  Result := DeleteTags(Tags.DelimitedText);
end;

function TqBitObject.SetAutomaticTorrentManagement(Hashes: TStringList; Enable: boolean): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetAutomaticTorrentManagement(Hashes.DelimitedText, Enable);
end;

function TqBitObject.ToggleSequentialDownload(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := ToggleSequentialDownload(Hashes.DelimitedText);
end;

function TqBitObject.SetFirstLastPiecePriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetFirstLastPiecePriority(Hashes.DelimitedText);
end;

function TqBitObject.SetForceStart(Hashes: TStringList; value: boolean): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetForceStart(Hashes.DelimitedText, value);
end;

function TqBitObject.SetSuperSeeding(Hashes: TStringList; value: boolean): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetSuperSeeding(Hashes.DelimitedText, value);
end;

end.
