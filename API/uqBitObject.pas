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
  private
  protected
  public

    // Main

    class function Connect(HostPath, Username, Password : string): TqBitObject;
    function Clone: TqBitObject;

    // API Helpers

    function PauseTorrents(Hashes: TStringList): boolean; overload; virtual;
    function PauseTorrents(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function PauseTorrents(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function ResumeTorrents(Hashes: TStringList): boolean; overload; virtual;
    function ResumeTorrents(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function ResumeTorrents(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function DeleteTorrents(Hashes: TStringList; DeleteFiles: boolean = False): boolean; overload; virtual;
    function DeleteTorrents(Torrents: TqBitMainDataType; DeleteFiles: boolean = False): boolean; overload; virtual;
    function DeleteTorrents(Torrents: TqBitTorrentListType; DeleteFiles: boolean = False): boolean; overload; virtual;

    function RecheckTorrents(Hashes: TStringList): boolean; overload; virtual;
    function RecheckTorrents(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function RecheckTorrents(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function ReannounceTorrents(Hashes: TStringList): boolean; overload;
    function ReannounceTorrents(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function ReannounceTorrents(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function AddTrackersToTorrent(Hash: string; Urls: TStringList): boolean; overload; virtual;
    function RemoveTrackers(Hash: string;  Urls: TStringList): boolean; overload; virtual;

    function AddPeers(Hashes: string; Peers: TStringList): boolean; overload; virtual;
    function AddPeers(Hashes, Peers: TStringList): boolean; overload; virtual;
    function AddPeers(Torrents: TqBitMainDataType; Peers: TStringList): boolean; overload; virtual;
    function AddPeers(Torrents: TqBitTorrentListType; Peers: TStringList): boolean; overload; virtual;

    function BanPeers(Peers: TStringList): boolean; overload; virtual;
    function GetBanPeersList: TStringList; overload; virtual;
    function SetBanPeersList(PeersList: TStringList): Boolean; overload; virtual;
    function SetBanPeersList(PeersStr: string): Boolean; overload; virtual;
    function UnbanPeers(Peers: TStringList): boolean; overload; virtual;
    function UnbanPeers(Peers: string): boolean; overload; virtual;

    function IncreaseTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function IncreaseTorrentPriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function IncreaseTorrentPriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function DecreaseTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function DecreaseTorrentPriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function DecreaseTorrentPriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function MaximalTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function MaximalTorrentPriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function MaximalTorrentPriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function MinimalTorrentPriority(Hashes: TStringList): boolean; overload; virtual;
    function MinimalTorrentPriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function MinimalTorrentPriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function SetFilePriority(Hash: string; Ids: TStringList; Priority: integer): boolean; overload; virtual;

    function GetTorrentDownloadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetTorrentDownloadLimit(Torrents: TqBitMainDataType): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetTorrentDownloadLimit(Torrents: TqBitTorrentListType): TqBitTorrentSpeedsLimitType; overload; virtual;

    function SetTorrentDownloadLimit(Hashes: TStringList; Limit: integer): boolean; overload; virtual;
    function SetTorrentDownloadLimit(Torrents: TqBitMainDataType; Limit: integer): boolean; overload; virtual;
    function SetTorrentDownloadLimit(Torrents: TqBitTorrentListType; Limit: integer): boolean; overload; virtual;

    function SetTorrentShareLimit(Hashes: TStringList; RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;
    function SetTorrentShareLimit(Torrents: TqBitMainDataType; RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;
    function SetTorrentShareLimit(Torrents: TqBitTorrentListType; RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;

    function GetTorrentUploadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetTorrentUploadLimit(Torrents: TqBitMainDataType): TqBitTorrentSpeedsLimitType; overload; virtual;
    function GetTorrentUploadLimit(Torrents: TqBitTorrentListType): TqBitTorrentSpeedsLimitType; overload; virtual;

    function SetTorrentUploadLimit(Hashes: TStringList; Limit: integer): boolean; overload; virtual;
    function SetTorrentUploadLimit(Torrents: TqBitMainDataType; Limit: integer): boolean; overload; virtual;
    function SetTorrentUploadLimit(Torrents: TqBitTorrentListType; Limit: integer): boolean; overload; virtual;

    function SetTorrentLocation(Hashes: TStringList; Location: string): boolean; overload; virtual;
    function SetTorrentLocation(Torrents: TqBitMainDataType; Location: string): boolean; overload; virtual;
    function SetTorrentLocation(Torrents: TqBitTorrentListType; Location: string): boolean; overload; virtual;

    function SetTorrentCategory(Hashes: TStringList; Category: string): boolean; overload; virtual;
    function SetTorrentCategory(Torrents: TqBitMainDataType; Category: string): boolean; overload; virtual;
    function SetTorrentCategory(Torrents: TqBitTorrentListType; Category: string): boolean; overload; virtual;

    function RemoveCategories(Categories: TStringList): boolean; overload; virtual;

    function AddTorrentTags(Hashes: string; Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Hashes: TStringList; Tags: string): boolean; overload; virtual;
    function AddTorrentTags(Hashes, Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Torrents: TqBitMainDataType; Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Torrents: TqBitTorrentListType; Tags: TStringList): boolean; overload; virtual;
    function AddTorrentTags(Torrents: TqBitMainDataType; Tags: string): boolean; overload; virtual;
    function AddTorrentTags(Torrents: TqBitTorrentListType; Tags: string): boolean; overload; virtual;

    function RemoveTorrentTags(Hashes: string; Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Hashes: TStringList; Tags: string): boolean; overload; virtual;
    function RemoveTorrentTags(Hashes, Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Torrents: TqBitMainDataType; Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Torrents: TqBitTorrentListType; Tags: TStringList): boolean; overload; virtual;
    function RemoveTorrentTags(Torrents: TqBitMainDataType; Tags: string): boolean; overload; virtual;
    function RemoveTorrentTags(Torrents: TqBitTorrentListType; Tags: string): boolean; overload; virtual;

    function CreateTags(Tags: TStringList): boolean; overload; virtual;
    function DeleteTags(Tags: TStringList): boolean; overload; virtual;
    function AddNewTorrentUrl(Url: string): boolean;

    function SetAutomaticTorrentManagement(Hashes: TStringList; Enable: boolean): boolean; overload; virtual;
    function SetAutomaticTorrentManagement(Torrents: TqBitMainDataType; Enable: boolean): boolean; overload; virtual;
    function SetAutomaticTorrentManagement(Torrents: TqBitTorrentListType; Enable: boolean): boolean; overload; virtual;

    function ToggleSequentialDownload(Hashes: TStringList): boolean; overload; virtual;
    function ToggleSequentialDownload(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function ToggleSequentialDownload(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function SetFirstLastPiecePriority(Hashes: TStringList): boolean; overload; virtual;
    function SetFirstLastPiecePriority(Torrents: TqBitMainDataType): boolean; overload; virtual;
    function SetFirstLastPiecePriority(Torrents: TqBitTorrentListType): boolean; overload; virtual;

    function SetForceStart(Hashes: TStringList; Value: boolean): boolean; overload; virtual;
    function SetForceStart(Torrents: TqBitMainDataType; Value: boolean): boolean; overload; virtual;
    function SetForceStart(Torrents: TqBitTorrentListType; Value: boolean): boolean; overload; virtual;

    function SetSuperSeeding(Hashes: TStringList; Value: boolean): boolean; overload; virtual;
    function SetSuperSeeding(Torrents: TqBitMainDataType; Value: boolean): boolean; overload; virtual;
    function SetSuperSeeding(Torrents: TqBitTorrentListType; Value: boolean): boolean; overload; virtual;

    function GetAllTorrentList: TqBitTorrentListType; virtual;

    // Helpers

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
uses SysUtils, DateUtils, uqBitUtils;

class function TqBitObject.Connect(HostPath, Username, Password : string): TqBitObject;
begin
  Result := TqBitObject.Create(HostPath);
  if not Result.Login(Username, Password) then
    FreeAndNil(Result)
end;

// Helpers

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

(*
  **************************
  ****  WebAPI Helpers  ****
  **************************
*)

// PauseTorrents
function TqBitObject.PauseTorrents(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// ResumeTorrents
function TqBitObject.ResumeTorrents(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// DeleteTorrents
function TqBitObject.DeleteTorrents(Hashes: TStringList; DeleteFiles: boolean = False): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// RecheckTorrents
function TqBitObject.RecheckTorrents(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// ReannounceTorrents
function TqBitObject.ReannounceTorrents(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// AddTrackersToTorrent
function TqBitObject.AddTrackersToTorrent(Hash: string;  Urls: TStringList): boolean;
begin
  Urls.StrictDelimiter := True; Urls.Delimiter := Chr($A);
  Result := AddTrackersToTorrent(Hash, Urls.DelimitedText);
end;

// RemoveTrackers
function TqBitObject.RemoveTrackers(Hash: string;  Urls: TStringList): boolean;
begin
  Urls.StrictDelimiter := True; Urls.Delimiter := '|';
  Result := RemoveTrackers(Hash, Urls.DelimitedText);
end;

// AddPeers
function TqBitObject.AddPeers(Hashes: string; Peers: TStringList): boolean;
begin
  Peers.StrictDelimiter := True; Peers.Delimiter := '|';
  Result := AddPeers(Hashes, Peers.DelimitedText);
end;
function TqBitObject.AddPeers(Hashes, Peers: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
  Peers.StrictDelimiter := True; Peers.Delimiter := '|';
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

// Ban/Unban Peers
function TqBitObject.BanPeers(Peers: TStringList): boolean;
begin
  Peers.StrictDelimiter := True; Peers.Delimiter := '|';
  Result := BanPeers(Peers);
end;

function TqBitObject.UnbanPeers(Peers: TStringList): boolean;
begin
  Result := False;
  var BanPeers := GetBanPeersList;
  if BanPeers = nil then exit;
  for var Peer in Peers do
    if BanPeers.IndexOf(Peer) <> -1 then
      BanPeers.Delete(BanPeers.IndexOf(Peer));
  Result := SetBanPeersList(BanPeers);
  BanPeers.Free;
end;

function TqBitObject.UnbanPeers(Peers: string): boolean;
begin
  var BanPeers := TqBitUtils.DelimStringList(nil, '|', Peers);
  Result := UnbanPeers(BanPeers);
  BanPeers.Free;
end;

function TqBitObject.GetBanPeersList: TStringList;
begin
  Result := nil;
  var Prefs := Self.GetPreferences;
  if Prefs = nil then Exit;
  Result := TStringList.Create;
  Result.StrictDelimiter := True; Result.Delimiter := #$A;
  Result.DelimitedText := Prefs.Fbanned_IPs;
  Prefs.Free;
end;

function TqBitObject.SetBanPeersList(PeersStr: string): Boolean;
begin
  var Prefs := TqBitPreferencesType.Create;
  Prefs.Fbanned_IPs := PeersStr;
  Result := SetPreferences(Prefs);
  Prefs.Free;
end;

function TqBitObject.SetBanPeersList(PeersList: TStringList): Boolean;
begin
   PeersList.StrictDelimiter := True; PeersList.Delimiter := #$A;
   Result := SetBanPeersList(PeersList.DelimitedText);
end;

// IncreaseTorrentPriority
function TqBitObject.IncreaseTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// DecreaseTorrentPriority
function TqBitObject.DecreaseTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// MinimalTorrentPriority
function TqBitObject.MinimalTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// MaximalTorrentPriority
function TqBitObject.MaximalTorrentPriority(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// SetFilePriority
function TqBitObject.SetFilePriority(Hash: string; Ids: TStringList; Priority: integer): boolean;
begin
  Ids.StrictDelimiter := True; Ids.Delimiter := '|';
  Result := SetFilePriority(Hash, Ids.DelimitedText, Priority);
end;

// GetTorrentDownloadLimit
function TqBitObject.GetTorrentDownloadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// SetTorrentDownloadLimit
function TqBitObject.SetTorrentDownloadLimit(Hashes: TStringList; Limit: integer): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// SetTorrentShareLimit
function TqBitObject.SetTorrentShareLimit(Hashes: TStringList; RatioLimit: double; SeedingTimeLimit: integer): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// GetTorrentUploadLimit
function TqBitObject.GetTorrentUploadLimit(Hashes: TStringList): TqBitTorrentSpeedsLimitType;
begin
 Hashes.StrictDelimiter := True;  Hashes.Delimiter := '|';
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

// SetTorrentUploadLimit
function TqBitObject.SetTorrentUploadLimit(Hashes: TStringList; Limit: integer): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// SetTorrentLocation
function TqBitObject.SetTorrentLocation(Hashes: TStringList;
  Location: string): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// SetTorrentCategory
function TqBitObject.SetTorrentCategory(Hashes: TStringList; Category: string): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// RemoveCategories
function TqBitObject.RemoveCategories(Categories: TStringList): boolean;
begin
  Categories.StrictDelimiter := True; Categories.Delimiter := Chr($A);
  Result := RemoveCategories(Categories.DelimitedText);
end;

// AddTorrentTags
function TqBitObject.AddTorrentTags(Hashes: string; Tags: TStringList): boolean;
begin
  Tags.StrictDelimiter := True; Tags.Delimiter := ',';
  Result := AddTorrentTags(Hashes, Tags.DelimitedText);
end;
function TqBitObject.AddTorrentTags(Hashes: TStringList; Tags: string): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := ',';
  Result := AddTorrentTags(Hashes.DelimitedText, Tags);
end;
function TqBitObject.AddTorrentTags(Hashes, Tags: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
  Hashes.StrictDelimiter := True; Tags.Delimiter := ',';
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

// RemoveTorrentTags
function TqBitObject.RemoveTorrentTags(Hashes: string; Tags: TStringList): boolean;
begin
  Tags.StrictDelimiter := True; Tags.Delimiter := ',';
  Result := RemoveTorrentTags(Hashes, Tags.DelimitedText);
end;
function TqBitObject.RemoveTorrentTags(Hashes: TStringList; Tags: string): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
  Result := RemoveTorrentTags(Hashes.DelimitedText, Tags);
end;
function TqBitObject.RemoveTorrentTags(Hashes, Tags: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
  Hashes.StrictDelimiter := True; Tags.Delimiter := ',';
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

// CreateTags
function TqBitObject.CreateTags(Tags: TStringList): boolean;
begin
  Tags.StrictDelimiter := True; Tags.Delimiter := ',';
  Result := CreateTags(Tags.DelimitedText);
end;
function TqBitObject.DeleteTags(Tags: TStringList): boolean;
begin
  Tags.StrictDelimiter := True; Tags.Delimiter := ',';
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
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// ToggleSequentialDownload
function TqBitObject.ToggleSequentialDownload(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True;  Hashes.Delimiter := '|';
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

// SetFirstLastPiecePriority
function TqBitObject.SetFirstLastPiecePriority(Hashes: TStringList): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// SetForceStart
function TqBitObject.SetForceStart(Hashes: TStringList; Value: boolean): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

// SetSuperSeeding
function TqBitObject.SetSuperSeeding(Hashes: TStringList; Value: boolean): boolean;
begin
  Hashes.StrictDelimiter := True; Hashes.Delimiter := '|';
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

