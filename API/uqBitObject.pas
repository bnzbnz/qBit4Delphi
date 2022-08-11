unit uqBitObject;

///
///  Author:  Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///
///  https://github.com/bnzbnz/qBit4Delphi:
///  https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)
///
///  License: MPL 1.1 / GPL 2.1
///


interface

uses classes, uqBitAPI, uqBitAPITypes;

type

  TqBitObject = class(TqBitAPI)
  Private
  Protected
  Public

    // Main

    class function Connect(HostPath, Username, Password : string): TqBitObject;
    function Clone: TqBitObject;
    procedure SetHTTPParams(Retries: Integer; Compression: Boolean = True; ConnectionTimeout: Integer = 500; SendTimeout: Integer = 1000; ResponseTimeout: Integer = 2000);

    // API Helpers

    function PauseTorrents(Hashes: TStringList): boolean; overload; virtual;
    function PauseTorrents(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function PauseTorrents(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function PauseAllTorrents: boolean; overload; virtual;

    function ResumeTorrents(Hashes: TStringList): boolean; overload; virtual;
    function ResumeTorrents(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function ResumeTorrents(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function ResumeAllTorrents: boolean; overload; virtual;

    function DeleteTorrents(Hashes: TStringList; DeleteFiles: boolean = False): boolean; overload; virtual;
    function DeleteTorrents(Torrents: TqBitMainDataType; DeleteFiles: boolean = False): boolean; overload; virtual;
    function DeleteTorrents(Torrents: TqBitTorrentListType; DeleteFiles: boolean = False): boolean; overload; virtual;
    function DeleteAllTorrents: boolean; overload; virtual;  // !!!!!!!!!!!!!!!

    function RecheckTorrents(Hashes: TStringList): boolean; overload; virtual;
    function RecheckTorrents(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function RecheckTorrents(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function RecheckAllTorrents: boolean;  overload; virtual;

    function ReannounceTorrents(Hashes: TStringList): boolean; overload;
    function ReannounceTorrents(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function ReannounceTorrents(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function ReannounceAllTorrents: boolean; overload; virtual;

    function AddTrackersToTorrent(Hash: string; Urls: TStringList): boolean; overload; virtual;
    function RemoveTrackers(Hash: string;  Urls: TStringList): boolean; overload; virtual;

    function AddPeers(Hashes: string; Peers: TStringList): boolean; overload; virtual;
    function AddPeers(Hashes, Peers: TStringList): boolean; overload; virtual;
    function AddPeers(Torrents: TqBitMainDataType; Peers: TStringList): boolean; overload; virtual;
    function AddPeers(Torrents: TqBitTorrentListType; Peers: TStringList): boolean; overload; virtual;
    function AddAllPeers(Peers: TStringList): boolean; overload; virtual;

    function BanPeers(Peers: TStringList): boolean; overload; virtual;
    function GetBanPeersList: TStringList;
    function SetBanPeersList(PeersList: TStringList): Boolean; overload;
    function SetBanPeersList(PeersStr: string): Boolean; overload;

    function IncreaseTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function IncreaseTorrentPriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function IncreaseTorrentPriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function IncreaseAllTorrentPriority: boolean;

    function DecreaseTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function DecreaseTorrentPriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function DecreaseTorrentPriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function DecreaseAllTorrentPriority: boolean; overload; virtual;

    function MaximalTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function MaximalTorrentPriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function MaximalTorrentPriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function MaximalAllTorrentPriority: boolean;

    function MinimalTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function MinimalTorrentPriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function MinimalTorrentPriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function MinimalAllTorrentPriority: boolean; overload; virtual;

    function SetFilePriority(Hash: string; Ids: TStringList; Priority: integer): boolean; overload; virtual;

    function GetTorrentDownloadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetTorrentDownloadLimit(Torrents: TqBitMainDataType): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetTorrentDownloadLimit(Torrents: TqBitTorrentListType): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetAllTorrentDownloadLimit: TqBitTorrentSpeedsLimitType; overload; virtual;

    function SetTorrentDownloadLimit(Hashes: TStringList; Limit: integer): boolean; overload; virtual;
    function SetTorrentDownloadLimit(Torrents: TqBitMainDataType; Limit: integer): boolean; overload; virtual;
    function SetTorrentDownloadLimit(Torrents: TqBitTorrentListType; Limit: integer): boolean; overload; virtual;
    function SetAllTorrentDownloadLimit(Limit: integer): boolean; overload; virtual;

    function SetTorrentShareLimit(Hashes: TStringList; RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;
    function SetTorrentShareLimit(Torrents: TqBitMainDataType; RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;
    function SetTorrentShareLimit(Torrents: TqBitTorrentListType; RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;
    function SetAllTorrentShareLimit(RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;

    function GetTorrentUploadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetTorrentUploadLimit(Torrents: TqBitMainDataType): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetTorrentUploadLimit(Torrents: TqBitTorrentListType): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetAllTorrentUploadLimit: TqBitTorrentSpeedsLimitType; overload; virtual;

    function SetTorrentUploadLimit(Hashes: TStringList; Limit: integer): boolean; overload; virtual;
    function SetTorrentUploadLimit(Torrents: TqBitMainDataType; Limit: integer): boolean; overload; virtual;
    function SetTorrentUploadLimit(Torrents: TqBitTorrentListType; Limit: integer): boolean; overload; virtual;
    function SetAllTorrentUploadLimit(Limit: integer): boolean; overload; virtual;

    function SetTorrentLocation(Hashes: TStringList; Location: string): boolean; overload; virtual;
    function SetTorrentLocation(Torrents: TqBitMainDataType; Location: string): boolean; overload; virtual;
    function SetTorrentLocation(Torrents: TqBitTorrentListType; Location: string): boolean; overload; virtual;
    function SetAllTorrentLocation(Location: string): boolean; overload; virtual;

    function SetTorrentCategory(Hashes: TStringList; Category: string): boolean; overload; virtual;
    function SetTorrentCategory(Torrents: TqBitMainDataType; Category: string): boolean; overload; virtual;
    function SetTorrentCategory(Torrents: TqBitTorrentListType; Category: string): boolean; overload; virtual;
    function SetAllTorrentCategory(Category: string): boolean; overload; virtual;

    function RemoveCategories(Categories: TStringList): boolean; overload; virtual;

    function AddTorrentTags(Hashes: string; Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Hashes: TStringList; Tags: string): boolean; overload; virtual;
    function AddTorrentTags(Hashes, Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Torrents: TqBitMainDataType; Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Torrents: TqBitTorrentListType; Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Torrents: TqBitMainDataType; Tags: string): boolean; overload; virtual;
    function AddTorrentTags(Torrents: TqBitTorrentListType; Tags: string): boolean; overload; virtual;
    function AddAllTorrentTags(Tags: TStringList): boolean; overload; virtual;
    function AddAllTorrentTags(Tags: string): boolean; overload; virtual;

    function RemoveTorrentTags(Hashes: string; Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Hashes: TStringList; Tags: string): boolean; overload; virtual;
    function RemoveTorrentTags(Hashes, Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Torrents: TqBitMainDataType; Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Torrents: TqBitTorrentListType; Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Torrents: TqBitMainDataType; Tags: string): boolean; overload; virtual;
    function RemoveTorrentTags(Torrents: TqBitTorrentListType; Tags: string): boolean; overload; virtual;
    function RemoveAllTorrentTags(Tags: TStringList): boolean; overload; virtual;
    function RemoveAllTorrentTags(Tags: string): boolean; overload; virtual;

    function CreateTags(Tags: TStringList): boolean; overload; virtual;
    function DeleteTags(Tags: TStringList): boolean; overload; virtual;
    function AddNewTorrentUrl(Url: string): boolean;

    function SetAutomaticTorrentManagement(Hashes: TStringList; Enable: boolean): boolean; overload; virtual;
    function SetAutomaticTorrentManagement(Torrents: TqBitMainDataType; Enable: boolean): boolean; overload; virtual;
    function SetAutomaticTorrentManagement(Torrents: TqBitTorrentListType; Enable: boolean): boolean; overload; virtual;
    function SetAllAutomaticTorrentManagement(Enable: boolean): boolean; overload; virtual;

    function ToggleSequentialDownload(Hashes: TStringList): boolean; overload; virtual;
    function ToggleSequentialDownload(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function ToggleSequentialDownload(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function ToggleAllSequentialDownload: boolean; overload; virtual;

    function SetFirstLastPiecePriority(Hashes: TStringList): boolean; overload; virtual;
    function SetFirstLastPiecePriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function SetFirstLastPiecePriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;
    function SetAllFirstLastPiecePriority: boolean; overload; virtual;

    function SetForceStart(Hashes: TStringList; Value: boolean): boolean; overload; virtual;
    function SetForceStart(Torrents: TqBitMainDataType; Value: boolean): boolean; overload; virtual;
    function SetForceStart(Torrents: TqBitTorrentListType; Value: boolean): boolean; overload; virtual;
    function SetAllForceStart(Value: boolean): boolean; overload; virtual;

    function SetSuperSeeding(Hashes: TStringList; Value: boolean): boolean; overload; virtual;
    function SetSuperSeeding(Torrents: TqBitMainDataType; Value: boolean): boolean; overload; virtual;
    function SetSuperSeeding(Torrents: TqBitTorrentListType; Value: boolean): boolean; overload; virtual;
    function SetAllSuperSeeding(Value: boolean): boolean; overload; virtual;

    function GetAllTorrentList: TqBitTorrentListType; virtual;

    // Helpers

    class function UTimestampToDateTime(Timestamp: int64): TDatetime;
    class function UTimestampMsToDateTime(Timestamp: int64): TDatetime;
    class procedure TSDurationToNow(Timestamp: int64; var Days, Hours, Mins, Secs: word);
    class function TorrentsToHashesList(Torrents: TqBitMainDataType): TStringList; overload; virtual;
    class function TorrentsToHashesList(Torrents: TqBitTorrentListType): TStringList; overload; virtual;

    class function qBitMajorVersion: Integer; virtual;
    class function qBitMinorVersion: Integer; virtual;
    class function qBitVersion: string; virtual;
    class function qBitWebAPIVersion: string; virtual;
    class function qBitCheckWebAPICompatibility(RemoteWebAPIVersion: string): Boolean; overload; virtual;
    function qBitCheckWebAPICompatibility: Boolean; overload; virtual;

    property HostPath: string read FHostPath;
    property Username: string read FUsername;
    property Password: string read FPassword;
    property Duration: cardinal read FDuration;
    property HTTPStatus: integer read FHTTPStatus;
    property HTTPResponse: string read FHTTPResponse;
    property HTTPDuration: cardinal read FHTTPDuration;
    property HTTPConnectionTimeout: integer read FHTTPConnectionTimeout write FHTTPConnectionTimeout;
    property HTTPSendTimeout: integer read FHTTPSendTimeout write FHTTPSendTimeout;
    property HTTPResponseTimeout: integer read FHTTPResponseTimeout write FHTTPResponseTimeout;
    property HTTPRetries: integer read FHTTPRetries write FHTTPRetries;
    property HTTPCompression: boolean read FHTTPCompression write FHTTPCompression;

  end;

  // Place Holder

  TqBitTorrent = class(TqBitObject);
  TqBit = class(TqBitObject);
  TqNOX = class(TqBitObject);

implementation
uses SysUtils, DateUtils;

class function TqBitObject.Connect(HostPath, Username, Password : string): TqBitObject;
begin
  Result := TqBitObject.Create(HostPath);
  if not Result.Login(Username, Password) then
    FreeAndNil(Result)
end;

// Helpers

class function TqBitObject.UTimestampToDateTime(Timestamp: int64): TDatetime;
begin
  Result := TTimeZone.Local.ToLocalTime(UnixToDateTime(Timestamp));
end;

class function TqBitObject.UTimestampMsToDateTime(Timestamp: int64): TDatetime;
begin
  Result := UTimestampToDateTime(Timestamp div 1000);
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

function TqBitObject.Clone: TqBitObject;
begin
  Result := TqBitObject.Create(FHostPath);
  Result.FSID := FSID;
  Result.FHTTPConnectionTimeout := FHTTPConnectionTimeout;
  Result.FHTTPSendTimeout := FHTTPSendTimeout;
  Result.FHTTPResponseTimeout := FHTTPResponseTimeout;
  Result.FHTTPRetries := FHTTPRetries;
  Result.HTTPCompression := FHTTPCompression;
  Result.FUsername := FUsername;
  Result.FPassword := FPassword;
end;

procedure TqBitObject.SetHTTPParams(Retries: Integer; Compression: Boolean; ConnectionTimeout: Integer; SendTimeout: Integer; ResponseTimeout: Integer);
begin
  Self.HTTPRetries := Retries;
  Self.HTTPConnectionTimeout := ConnectionTimeout;
  Self.HTTPSendTimeout := SendTimeout;
  Self.HTTPResponseTimeout := ResponseTimeout;
  Self.HTTPCompression := Compression;
end;

class function TqBitObject.TorrentsToHashesList(Torrents: TqBitMainDataType): TStringList;
begin
  Result := TStringList.Create;
  for var Torrent in Torrents.Ftorrents do
    Result.Add(Torrent.Key)
end;

class function TqBitObject.TorrentsToHashesList(Torrents: TqBitTorrentListType): TStringList;
begin
  Result := TStringList.Create;
  for var Torrent in Torrents.Ftorrents do
    Result.Add(TqBitTorrentType(Torrent).Fhash);
end;

// PauseTorrents
function TqBitObject.PauseTorrents(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := PauseTorrents(Hashes.DelimitedText);
end;
function TqBitObject.PauseTorrents(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := PauseTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.PauseTorrents(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := PauseTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.PauseAllTorrents: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := PauseTorrents(Res);
  Res.Free;
end;

// ResumeTorrents
function TqBitObject.ResumeTorrents(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := ResumeTorrents(Hashes.DelimitedText);
end;
function TqBitObject.ResumeTorrents(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := ResumeTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.ResumeTorrents(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := ResumeTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.ResumeAllTorrents: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := ResumeTorrents(Res);
  Res.Free;
end;

// DeleteTorrents
function TqBitObject.DeleteTorrents(Hashes: TStringList; DeleteFiles: boolean = False): boolean;
begin
  Hashes.Delimiter := '|';
  Result := DeleteTorrents(Hashes.DelimitedText, DeleteFiles);
end;
function TqBitObject.DeleteTorrents(Torrents: TqBitTorrentListType; DeleteFiles: boolean): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := DeleteTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.DeleteTorrents(Torrents: TqBitMainDataType; DeleteFiles: boolean): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := DeleteTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.DeleteAllTorrents: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := DeleteTorrents(Res);
  Res.Free;
end;

// RecheckTorrents
function TqBitObject.RecheckTorrents(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := RecheckTorrents(Hashes.DelimitedText);
end;
function TqBitObject.RecheckTorrents(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := RecheckTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.RecheckTorrents(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := RecheckTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.RecheckAllTorrents: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := RecheckTorrents(Res);
  Res.Free;
end;

// ReannounceTorrents
function TqBitObject.ReannounceTorrents(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := ReannounceTorrents(Hashes.DelimitedText);
end;
function TqBitObject.ReannounceTorrents(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := ReannounceTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.ReannounceTorrents(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := ReannounceTorrents(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.ReannounceAllTorrents: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := ReannounceTorrents(Res);
  Res.Free;
end;

// AddTrackersToTorrent
function TqBitObject.AddTrackersToTorrent(Hash: string;  Urls: TStringList): boolean;
begin
  Urls.Delimiter := Chr($A);
  Result := AddTrackersToTorrent(Hash, Urls.DelimitedText);
end;

// RemoveTrackers
function TqBitObject.RemoveTrackers(Hash: string;  Urls: TStringList): boolean;
begin
  Urls.Delimiter := '|';
  Result := RemoveTrackers(Hash, Urls.DelimitedText);
end;

// AddPeers
function TqBitObject.AddPeers(Hashes: string; Peers: TStringList): boolean;
begin
  Peers.Delimiter := '|';
  Result := AddPeers(Hashes, Peers.DelimitedText);
end;
function TqBitObject.AddPeers(Hashes, Peers: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Peers.Delimiter := '|';
  Result := AddPeers(Hashes.DelimitedText, Peers.DelimitedText);
end;
function TqBitObject.AddPeers(Torrents: TqBitMainDataType; Peers: TStringList): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := AddPeers(TorrentList, Peers);
  TorrentList.Free;;
end;
function TqBitObject.AddPeers(Torrents: TqBitTorrentListType; Peers: TStringList): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := AddPeers(TorrentList, Peers);
  TorrentList.Free;;
end;
function TqBitObject.AddAllPeers(Peers: TStringList): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := AddPeers(Res, Peers);
  Res.Free;
end;

function TqBitObject.BanPeers(Peers: TStringList): boolean;
begin
  Peers.Delimiter := '|';
  Result := BanPeers(Peers);
end;

function TqBitObject.GetBanPeersList: TStringList;
begin
  Result := nil;
  var Prefs := Self.GetPreferences;
  if Prefs = nil then Exit;
  Result := TStringList.Create;
  Result.Delimiter := #$A;
  Result.DelimitedText := Prefs.Fbanned_IPs;
  Prefs.Free;
end;

function TqBitObject.SetBanPeersList(PeersStr: string): Boolean;
begin
  var Prefs := TqBitPreferencesType.Create;
  Prefs.Fbanned_IPs := PeersStr;
  Result := SetPreferences(Prefs);
end;

function TqBitObject.SetBanPeersList(PeersList: TStringList): Boolean;
begin
   PeersList.Delimiter := #$A;
   Result := SetBanPeersList(PeersList.DelimitedText);
end;

// IncreaseTorrentPriority
function TqBitObject.IncreaseTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := IncreaseTorrentPriority(Hashes.DelimitedText);
end;
function TqBitObject.IncreaseTorrentPriority(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := IncreaseTorrentPriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.IncreaseTorrentPriority(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := IncreaseTorrentPriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.IncreaseAllTorrentPriority: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := IncreaseTorrentPriority(Res);
  Res.Free;
end;

// DecreaseTorrentPriority
function TqBitObject.DecreaseTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := DecreaseTorrentPriority(Hashes.DelimitedText);
end;
function TqBitObject.DecreaseTorrentPriority(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := DecreaseTorrentPriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.DecreaseTorrentPriority(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := DecreaseTorrentPriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.DecreaseAllTorrentPriority: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := DecreaseTorrentPriority(Res);
  Res.Free;
end;

// MinimalTorrentPriority
function TqBitObject.MinimalTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := MinimalTorrentPriority(Hashes.DelimitedText);
end;
function TqBitObject.MinimalTorrentPriority(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := MinimalTorrentPriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.MinimalTorrentPriority(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := MinimalTorrentPriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.MinimalAllTorrentPriority: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := MinimalTorrentPriority(Res);
  Res.Free;
end;

// MaximalTorrentPriority
function TqBitObject.MaximalTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := MaximalTorrentPriority(Hashes.DelimitedText);
end;
function TqBitObject.MaximalTorrentPriority(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := MaximalTorrentPriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.MaximalTorrentPriority(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := MaximalTorrentPriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.MaximalAllTorrentPriority: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := MaximalTorrentPriority(Res);
  Res.Free;
end;

// SetFilePriority
function TqBitObject.SetFilePriority(Hash: string; Ids: TStringList; Priority: integer): boolean;
begin
  Ids.Delimiter := '|';
  Result := SetFilePriority(Hash, Ids.DelimitedText, Priority);
end;

// GetTorrentDownloadLimit
function TqBitObject.GetTorrentDownloadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType;
begin
  Hashes.Delimiter := '|';
  Result := GetTorrentDownloadLimit(Hashes.DelimitedText);
end;
function TqBitObject.GetTorrentDownloadLimit(Torrents: TqBitTorrentListType): TqBitTorrentSpeedsLimitType;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := GetTorrentDownloadLimit(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.GetTorrentDownloadLimit(Torrents: TqBitMainDataType): TqBitTorrentSpeedsLimitType;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := GetTorrentDownloadLimit(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.GetAllTorrentDownloadLimit: TqBitTorrentSpeedsLimitType;
begin
  Result := Nil;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := GetTorrentDownloadLimit(Res);
  Res.Free;
end;

// SetTorrentDownloadLimit
function TqBitObject.SetTorrentDownloadLimit(Hashes: TStringList; Limit: integer): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentDownloadLimit(Hashes.DelimitedText, Limit);
end;
function TqBitObject.SetTorrentDownloadLimit(Torrents: TqBitMainDataType; Limit: integer): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentDownloadLimit(TorrentList, Limit);
  TorrentList.Free;;
end;
function TqBitObject.SetTorrentDownloadLimit(Torrents: TqBitTorrentListType; Limit: integer): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentDownloadLimit(TorrentList, Limit);
  TorrentList.Free;;
end;
function TqBitObject.SetAllTorrentDownloadLimit(Limit: integer): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := SetTorrentDownloadLimit(Res, Limit);
  Res.Free;
end;

// SetTorrentShareLimit
function TqBitObject.SetTorrentShareLimit(Hashes: TStringList; RatioLimit: double; SeedingTimeLimit: integer): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentShareLimit(Hashes.DelimitedText, RatioLimit, SeedingTimeLimit);
end;
function TqBitObject.SetTorrentShareLimit(Torrents: TqBitMainDataType; RatioLimit: double; SeedingTimeLimit: integer): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentShareLimit(TorrentList, RatioLimit, SeedingTimeLimit);
  TorrentList.Free;;
end;
function TqBitObject.SetTorrentShareLimit(Torrents: TqBitTorrentListType; RatioLimit: double; SeedingTimeLimit: integer): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentShareLimit(TorrentList, RatioLimit, SeedingTimeLimit);
  TorrentList.Free;;
end;
function TqBitObject.SetAllTorrentShareLimit(RatioLimit: double; SeedingTimeLimit: integer): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := SetTorrentShareLimit(Res, RatioLimit, SeedingTimeLimit);
  Res.Free;
end;

// GetTorrentUploadLimit
function TqBitObject.GetTorrentUploadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType;
begin
  Hashes.Delimiter := '|';
  Result := GetTorrentUploadLimit(Hashes.DelimitedText);
end;
function TqBitObject.GetTorrentUploadLimit(Torrents: TqBitMainDataType): TqBitTorrentSpeedsLimitType;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := GetTorrentUploadLimit(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.GetTorrentUploadLimit(Torrents: TqBitTorrentListType): TqBitTorrentSpeedsLimitType;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := GetTorrentUploadLimit(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.GetAllTorrentUploadLimit: TqBitTorrentSpeedsLimitType;
begin
  Result := Nil;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := GetTorrentUploadLimit(Res);
  Res.Free;
end;

// SetTorrentUploadLimit
function TqBitObject.SetTorrentUploadLimit(Hashes: TStringList; Limit: integer): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentUploadLimit(Hashes.DelimitedText, Limit);
end;
function TqBitObject.SetTorrentUploadLimit(Torrents: TqBitMainDataType; Limit: integer): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentUploadLimit(TorrentList, Limit);
  TorrentList.Free;;
end;
function TqBitObject.SetTorrentUploadLimit(Torrents: TqBitTorrentListType; Limit: integer): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentUploadLimit(TorrentList, Limit);
  TorrentList.Free;;
end;
function TqBitObject.SetAllTorrentUploadLimit(Limit: integer): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := SetTorrentUploadLimit(Res, Limit);
  Res.Free;
end;

// SetTorrentLocation
function TqBitObject.SetTorrentLocation(Hashes: TStringList;
  Location: string): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentLocation(Hashes.DelimitedText, Location);
end;
function TqBitObject.SetTorrentLocation(Torrents: TqBitMainDataType; Location: string): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentLocation(TorrentList, Location);
  TorrentList.Free;;
end;
function TqBitObject.SetTorrentLocation(Torrents: TqBitTorrentListType; Location: string): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentLocation(TorrentList, Location);
  TorrentList.Free;;
end;
function TqBitObject.SetAllTorrentLocation(Location: string): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := SetTorrentLocation(Res, Location);
  Res.Free;
end;

// SetTorrentCategory
function TqBitObject.SetTorrentCategory(Hashes: TStringList; Category: string): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetTorrentCategory(Hashes.DelimitedText, Category);
end;
function TqBitObject.SetTorrentCategory(Torrents: TqBitMainDataType; Category: string): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentCategory(TorrentList, Category);
  TorrentList.Free;;
end;
function TqBitObject.SetTorrentCategory(Torrents: TqBitTorrentListType; Category: string): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetTorrentLocation(TorrentList, Category);
  TorrentList.Free;;
end;
function TqBitObject.SetAllTorrentCategory(Category: string): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := SetTorrentCategory(Res, Category);
  Res.Free;
end;

// RemoveCategories
function TqBitObject.RemoveCategories(Categories: TStringList): boolean;
begin
  Categories.Delimiter := Chr($A);
  Result := RemoveCategories(Categories.DelimitedText);
end;

// AddTorrentTags
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
function TqBitObject.AddTorrentTags(Torrents: TqBitMainDataType; Tags: TStringList): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := AddTorrentTags(TorrentList, Tags);
  TorrentList.Free;;
end;
function TqBitObject.AddTorrentTags(Torrents: TqBitTorrentListType; Tags: TStringList): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := AddTorrentTags(TorrentList, Tags);
  TorrentList.Free;;
end;
function TqBitObject.AddTorrentTags(Torrents: TqBitMainDataType; Tags: string): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := AddTorrentTags(TorrentList, Tags);
  TorrentList.Free;;
end;
function TqBitObject.AddTorrentTags(Torrents: TqBitTorrentListType; Tags: string): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := AddTorrentTags(TorrentList, Tags);
  TorrentList.Free;;
end;
function TqBitObject.AddAllTorrentTags(Tags: TStringList): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := AddTorrentTags(Res, Tags);
  Res.Free;
end;
function TqBitObject.AddAllTorrentTags(Tags: string): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := AddTorrentTags(Res, Tags);
  Res.Free;
end;

// RemoveTorrentTags
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
function TqBitObject.RemoveTorrentTags(Torrents: TqBitMainDataType; Tags: TStringList): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := RemoveTorrentTags(TorrentList, Tags);
  TorrentList.Free;;
end;
function TqBitObject.RemoveTorrentTags(Torrents: TqBitTorrentListType; Tags: TStringList): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := RemoveTorrentTags(TorrentList, Tags);
  TorrentList.Free;;
end;
function TqBitObject.RemoveTorrentTags(Torrents: TqBitMainDataType; Tags: string): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := RemoveTorrentTags(TorrentList, Tags);
  TorrentList.Free;;
end;
function TqBitObject.RemoveTorrentTags(Torrents: TqBitTorrentListType; Tags: string): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := RemoveTorrentTags(TorrentList, Tags);
  TorrentList.Free;;
end;
function TqBitObject.RemoveAllTorrentTags(Tags: TStringList): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result :=  RemoveTorrentTags(Res, Tags);
  Res.Free;
end;
function TqBitObject.RemoveAllTorrentTags(Tags: string): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result :=  RemoveTorrentTags(Res, Tags);
  Res.Free;
end;

// CreateTags
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
function TqBitObject.AddNewTorrentUrl(Url: string): boolean;
var
  NewTorrentUrls: TqBitNewTorrentUrlsType;
begin
  NewTorrentUrls := TqBitNewTorrentUrlsType.Create;
  NewTorrentUrls.Furls.Add(Url);
  Result := AddNewTorrentUrls(NewTorrentUrls);
  NewTorrentUrls.Free;
end;

// SetAutomaticTorrentManagement
function TqBitObject.SetAutomaticTorrentManagement(Hashes: TStringList; Enable: boolean): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetAutomaticTorrentManagement(Hashes.DelimitedText, Enable);
end;
function TqBitObject.SetAutomaticTorrentManagement(Torrents: TqBitMainDataType; Enable: boolean): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetAutomaticTorrentManagement(TorrentList, Enable);
  TorrentList.Free;;
end;
function TqBitObject.SetAutomaticTorrentManagement(Torrents: TqBitTorrentListType; Enable: boolean): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetAutomaticTorrentManagement(TorrentList, Enable);
  TorrentList.Free;;
end;
function TqBitObject.SetAllAutomaticTorrentManagement(Enable: boolean): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result :=  SetAutomaticTorrentManagement(Res, Enable);
  Res.Free;
end;

// ToggleSequentialDownload
function TqBitObject.ToggleSequentialDownload(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := ToggleSequentialDownload(Hashes.DelimitedText);
end;
function TqBitObject.ToggleSequentialDownload(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := ToggleSequentialDownload(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.ToggleSequentialDownload(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := ToggleSequentialDownload(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.ToggleAllSequentialDownload: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result :=  ToggleSequentialDownload(Res);
  Res.Free;
end;

// SetFirstLastPiecePriority
function TqBitObject.SetFirstLastPiecePriority(Hashes: TStringList): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetFirstLastPiecePriority(Hashes.DelimitedText);
end;
function TqBitObject.SetFirstLastPiecePriority(Torrents: TqBitMainDataType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetFirstLastPiecePriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.SetFirstLastPiecePriority(Torrents: TqBitTorrentListType): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetFirstLastPiecePriority(TorrentList);
  TorrentList.Free;;
end;
function TqBitObject.SetAllFirstLastPiecePriority: boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result :=  SetFirstLastPiecePriority(Res);
  Res.Free;
end;

// SetForceStart
function TqBitObject.SetForceStart(Hashes: TStringList; Value: boolean): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetForceStart(Hashes.DelimitedText, Value);
end;
function TqBitObject.SetForceStart(Torrents: TqBitMainDataType; Value: boolean): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetForceStart(TorrentList, Value);
  TorrentList.Free;;
end;
function TqBitObject.SetForceStart(Torrents: TqBitTorrentListType; Value: boolean): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetForceStart(TorrentList, Value);
  TorrentList.Free;;
end;
function TqBitObject.SetAllForceStart(Value: boolean): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result :=  SetForceStart(Res, Value);;
  Res.Free;
end;

// SetSuperSeeding
function TqBitObject.SetSuperSeeding(Hashes: TStringList; Value: boolean): boolean;
begin
  Hashes.Delimiter := '|';
  Result := SetSuperSeeding(Hashes.DelimitedText, value);
end;
function TqBitObject.SetSuperSeeding(Torrents: TqBitMainDataType; Value: boolean): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetSuperSeeding(TorrentList, Value);
  TorrentList.Free;;
end;
function TqBitObject.SetSuperSeeding(Torrents: TqBitTorrentListType; Value: boolean): boolean;
begin
  var TorrentList := Self.TorrentsToHashesList(Torrents);
  Result := SetSuperSeeding(TorrentList, Value);
  TorrentList.Free;
end;
function TqBitObject.SetAllSuperSeeding(Value: boolean): boolean;
begin
  Result := False;
  var Res := GetAllTorrentList;
  if Res = Nil then exit;
  Result := SetSuperSeeding(Res, Value);
  Res.Free;
end;

function TqBitObject.GetAllTorrentList: TqBitTorrentListType;
var
  Req: TqBitTorrentListRequestType;
begin
  Req:= Nil;
  try
    Req := TqBitTorrentListRequestType.Create;
    Req.FFilter := 'all';
    Result := GetTorrentList(Req);
  finally
    Req.Free;
  end;
end;

class function TqBitObject.qBitVersion: string;
begin
  Result := Format('%d.%.*d.%s', [qBitMajorVersion, 3,qBitMinorVersion, qBitAPI_WebAPIVersion]);
end;

class function TqBitObject.qBitWebAPIVersion: string;
begin
  Result := qBitAPI_WebAPIVersion;
end;

class function TqBitObject.qBitMajorVersion: Integer;
begin
  Result := qBitAPI_MajorVersion;
end;

class function TqBitObject.qBitMinorVersion: Integer;
begin
  Result := qBitAPI_MinorVersion;
end;

class function TqBitObject.qBitCheckWebAPICompatibility(RemoteWebAPIVersion: string): Boolean;
begin
  var DigitsR := RemoteWebAPIVersion.Split(['.']);
  var DigitsL := qBitAPI_WebAPIVersion.Split(['.']);
  Result :=  CompareText(RemoteWebAPIVersion, qBitAPI_WebAPIVersion)  >= 0;
end;

function TqBitObject.qBitCheckWebAPICompatibility: Boolean;
begin
  Result := qBitCheckWebAPICompatibility(GetAPIVersion);
end;


end.

