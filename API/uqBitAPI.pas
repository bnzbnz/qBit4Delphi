unit uqBitAPI;

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
uses uqBitAPITypes, windows, classes;

const

  qBitAPI_MajorVersion = 1;
  qBitAPI_MinorVersion = 101;

type

  TqBitAPI = class(TObject)
  private
  protected

    FSID: string;
    FHostPath: string;
    FUsername: string;
    FPassword: string;
    FDuration: cardinal;
    FHTTPStatus: integer;
    FHTTPResponse: string;
    FHTTPConnectionTimeout: integer;
    FHTTPSendTimeout: integer;
    FHTTPResponseTimeout: integer;
    FHTTPRetries: integer;
    FHTTPDuration: cardinal;
    FHTTPCompression: Boolean;

    function qBPost(MethodPath: string; ReqST, ResST: TStringStream; ContentType: string): integer; overload; virtual;
    function qBPost(MethodPath: string; var Body: string): integer; overload; virtual;
    function qBPost(MethodPath: string): integer; overload; virtual;

  public

    constructor Create(HostPath: string); virtual;

    /////////////////////////////////////////////////////////////////////////////////////////
    // WebAPI: https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1) //
    /////////////////////////////////////////////////////////////////////////////////////////

  // Authentication :
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#login
    function Login(Username, Password: string): Boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#logout
    function Logout: Boolean; virtual;

  // Application :
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-application-version
    function GetVersion: string; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-api-version
    function GetAPIVersion: string; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-build-info
    function GetBuildInfo: TqBitBuildInfoType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#shutdown-application
    function Shutdown: Boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-application-preferences
    function GetPreferences: TqBitPreferencesType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-application-preferences
    function SetPreferences(Prefs: TqBitPreferencesType): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-default-save-path
    function GetDefaultSavePath: string; virtual;
        // UNDOCUMENTED
    function GetNetworkInterfaces: TqBitNetworkInterfacesType;
        // UNDOCUMENTED
    function GetNetworkInterfaceAddress(Iface: string = ''): TqBitNetworkInterfaceAddressesType;

  // Log :
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#log
    function GetLog(LastKnownId: int64 = -1; Normal: boolean = false;
              Info: boolean = false; Warning: boolean = true; Critical: boolean = true) : TqBitLogsType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-peer-log
    function GetPeerLog(LastKnownId: int64 = -1): TqBitPeerLogsType; virtual;

  // Sync :
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-main-data
    function GetMainData(Rid: int64 = 0): TqBitMainDataType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-peers-data
    function GetTorrentPeersData(Hash: string; Rid: int64 = 0): TqBitTorrentPeersDataType; virtual;

  // Transfer :
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-global-transfer-info
    function GetGlobalTransferInfo: TqBitGlobalTransferInfoType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-alternative-speed-limits-state
    function GetAlternativeSpeedLimitsState: boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#toggle-alternative-speed-limits
    function ToggleAlternativeSpeedLimits: boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-global-download-limit
    function GetGlobalDownloadLimit: integer; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-global-download-limit
    function SetGlobalDownloadLimit( GlobalDownloadLimit: integer): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-global-upload-limit
    function GetGlobalUploadLimit: integer; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-global-upload-limit
    function SetGlobalUploadLimit(GlobalUploafimit: integer): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#ban-peers
    function BanPeers(PeerListStr: string): boolean; overload; virtual;

  // Torrent management :
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-list
    function GetTorrentList(TorrentListRequest : TqBitTorrentListRequestType): TqBitTorrentsListType; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-generic-properties
    function GetTorrentGenericProperties(Hash: string): TqBitTorrentInfoType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-trackers
    function GetTorrentTrackers(Hash: string): TqBitTrackersType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-web-seeds
    function GetTorrentWebSeeds(Hash: string): TqBitWebSeedsType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-contents
    function GetTorrentContents(Hash: string; Indexes: string = ''): TqBitContentsType;  virtual;
        // https://github.co m/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-pieces-states
    function GetTorrentPiecesStates(Hash: string): TqBitPiecesStatesType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#pause-torrents
    function PauseTorrents(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#resume-torrents
    function ResumeTorrents(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#delete-torrents
    function DeleteTorrents(Hashes: string; DeleteFiles: boolean = False): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#recheck-torrents
    function RecheckTorrents(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#reannounce-torrents
    function ReannounceTorrents(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#add-new-torrent
    function AddNewTorrentUrls(NewTorrentUrls: TqBitNewTorrentUrlsType): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#add-new-torrent
    function AddNewTorrentFile(NewTorrentFile: TqBitNewTorrentFileType): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#add-trackers-to-torrent
    function AddTrackersToTorrent(Hash: string; Urls: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#edit-trackers
    function EditTracker(Hash, OrigUrl, NewUrl: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#remove-trackers
    function RemoveTrackers(Hash, Urls: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#add-peers
    function AddPeers(Hashes, Peers: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#increase-torrent-priority
    function IncreaseTorrentPriority(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#decrease-torrent-priority
    function DecreaseTorrentPriority(Hashes: string): boolean; overload; virtual;
        // lient-tunnel.canalplus.com/resiliation-abonnement/selection-motif?contractId=1
    function MaximalTorrentPriority(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#minimal-torrent-priority
    function MinimalTorrentPriority(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-file-priority
    function SetFilePriority(Hash, Ids: string; Priority: integer): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-download-limit
    function GetTorrentDownloadLimit(Hashes: string): TqBitTorrentSpeedsLimitType; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-torrent-download-limit
    function SetTorrentDownloadLimit(Hashes: string; Limit: integer): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-torrent-share-limit
    function SetTorrentShareLimit(Hashes: string; RatioLimit: double; SeedingTimeLimit: integer): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-torrent-upload-limit
    function GetTorrentUploadLimit(Hashes: string): TqBitTorrentSpeedsLimitType; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-torrent-upload-limit
    function SetTorrentUploadLimit(Hashes: string; Limit: integer): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-torrent-location
    function SetTorrentLocation(Hashes, Location: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-torrent-name
    function SetTorrentName(Hash, Name: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-torrent-category
    function SetTorrentCategory(Hashes, Category: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-all-categories
    function GetAllCategories: TqBitCategoriesType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#add-new-category
    function AddNewCategory(Category, SavePath: string): boolean;  virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#edit-category
    function EditCategory(Category, SavePath: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#remove-categories
    function RemoveCategories(Categories: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#add-torrent-tags
    function AddTorrentTags(Hashes, Tags: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#remove-torrent-tags
    function RemoveTorrentTags(Hashes, Tags: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-all-tags
    function GetAllTags: TqBitTagsType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#create-tags
    function CreateTags(Tags: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#delete-tags
    function DeleteTags(Tags: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-automatic-torrent-management
    function SetAutomaticTorrentManagement(Hashes: string; Enable: boolean): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#toggle-sequential-download
    function ToggleSequentialDownload(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-firstlast-piece-priority
    function SetFirstLastPiecePriority(Hashes: string): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-force-start
    function SetForceStart(Hashes: string; value: boolean): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-super-seeding
    function SetSuperSeeding(Hashes: string; value: boolean): boolean; overload; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#rename-file
    function RenameFile(Hash, OldPath, NewPath: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#rename-folder
    function RenameFolder(Hash, OldPath, NewPath: string): boolean; virtual;

  // RSS : EXPERIMENTAL & Not Tested
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#add-folder
    function RSSAddFolder(Path: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#add-feed
    function RSSAddFeed(Url, Path: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#remove-item
    function RSSRemoveItem(Path: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#move-item
    function RSSMoveItem(ItemPath, DestPath: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-all-items
    function RSSGetAllItems(WithData: boolean): TqBitRSSAllItemsType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#mark-as-read
    function RSSMarkAsRead(ItemPath, ArticleId: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#refresh-item
    function RSSRefreshItem(ItemPath: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#set-auto-downloading-rule
    function RSSSetAutoDownloadingRules(RuleName: string; RuleDef: TqBitRSSRuleType): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#rename-auto-downloading-rule
    function RSSRenameAutoDownloadingRules(RuleName, NewRuleName: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#remove-auto-downloading-rule
    function RSSRemoveAutoDownloadingRules(RuleName: string): boolean; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-all-auto-downloading-rules
    function RSSGetAllAutoDownloadingRules: TqBitAutoDownloadingRulesType; virtual;
        // https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#get-all-articles-matching-a-rule
    function RSSGetMatchingArticles(RuleName: string): TqBitRSSArticlesType; virtual;

  // Search : will be implemented if requested.

  end;

implementation
uses REST.Json, NetEncoding, SysUtils, wininet, System.Net.URLClient,
     System.Net.HttpClient, System.Net.HttpClientComponent, NetConsts, System.Hash,
     uqBitAPIUtils;

constructor TqBitAPI.Create(HostPath: string);
begin
  inherited Create;
  FSID := '';
  FHostPath := HostPath;
  FHTTPConnectionTimeout := 250;
  FHTTPSendTimeout := 500;
  FHTTPResponseTimeout := 2000;
  FHTTPRetries := 3;
  FHTTPResponse := '';
  FHTTPStatus := 0;
  FDuration := 0;
  FHTTPCompression := True;
end;

function TqBitAPI.qBPost(MethodPath: string; ReqST, ResST: TStringStream; ContentType: string): integer;
var
  Res: IHTTPResponse;
  Http: THTTPClient;
begin
  FHTTPResponse := '';
  FHTTPStatus := -1;
  FHTTPDuration := GetTickCount;
  Http := THTTPClient.Create;
  try
    Http.UserAgent :=
      Format(
        'qBittorrent WebAPI for Delphi (qBit4Delphi, qB4D) - Version: %s.%d.%.*d - Laurent Meyer (qBit4Delphi@ea4d.com)',
        [qBitAPI_WebAPIVersion, qBitAPI_MajorVersion, 3, qBitAPI_MinorVersion]
      );
    if FHTTPCompression then Http.AutomaticDecompression := [THTTPCompressionMethod.Any];
    Http.ContentType := ContentType;
    Http.CustomHeaders['Referer'] := FHostPath;
    Http.CookieManager.AddServerCookie(Format('SID=%s', [FSID]), FHostPath);
    Http.ConnectionTimeout := FHTTPConnectionTimeout;
    Http.SendTimeout := FHTTPSendTimeout;
    Http.ResponseTimeout := FHTTPResponseTimeout;
    var Retries := FHTTPRetries;
    repeat
      Dec(Retries);
      var Url := Format('%s/api/v2%s?%s', [FHostPath, MethodPath, TqBitAPIUtils.URLEncode(THash.GetRandomString)]);
      ReqST.Position := 0; ResST.Position := 0;
      try Res := Http.Post(Url, ReqST, ResST);  except end;
    until ((Res <> nil) or (Retries <= 0));
    if Res = nil then Exit;
    FHTTPStatus := Res.StatusCode;
    FHTTPResponse := ResST.DataString;
    for var Cookie in  Http.CookieManager.Cookies do
      if Cookie.Name = 'SID' then FSID := Cookie.Value;
  finally
    Result := FHTTPStatus;
    HTTP.Free;
    FHTTPDuration := GetTickcount - FHTTPDuration;
  end;
end;

function TqBitAPI.qBPost(MethodPath: string; var Body: string): integer;
var
  ReqSS: TStringStream;
  ResSS: TStringStream;
begin
  ReqSS := nil;
  ResSS := nil;
  try
  try
    ReqSS := TStringStream.Create(Body, TEncoding.UTF8);
    ResSS := TStringStream.Create('', TEncoding.UTF8);
    Result := qBPost(MethodPath, ReqSS, ResSS, 'application/x-www-form-urlencoded; charset=UTF-8');
    if Result = 200 then Body := ResSS.DataString else Body := '';
  except
    Result := -1;
  end;
  finally
    ResSS.Free;
    ReqSS.Free;
  end;
end;

function TqBitAPI.qBPost(MethodPath: string): integer;
begin
  var NoBody := '';
  Result := qBPost(MethodPath, NoBody);
end;

(*
  **************************
  ****      WebAPI      ****
  **************************
*)

function TqBitAPI.Login(Username, Password: string): Boolean;
begin
  FDuration := GetTickCount;
  FUsername := Username;
  FPassword := Password;
  var Body := Format('username=%s&password=%s',[ TqBitAPIUtils.URLEncode(Username), TqBitAPIUtils.URLEncode(Password) ]);
  Result := (qBPost('/auth/login', Body) = 200)  and (Body = 'Ok.');
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.Logout: Boolean;
begin
  FDuration := GetTickCount;
  Result := qBPost('/auth/logout') = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetVersion: string;
begin
  FDuration := GetTickCount;
  qBPost('/app/version', Result);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetAPIVersion: string;
begin
  qBPost('/app/webapiVersion', Result);
end;

function TqBitAPI.GetBuildInfo: TqBitBuildInfoType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := '';
  if qBPost('/app/buildInfo', Body) = 200 then
    Result := TJson.JsonToObject<TqBitBuildInfoType>(Body, []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.Shutdown: Boolean;
begin
  FDuration := GetTickCount;
  Result := qBPost('/app/shutdown') = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetPreferences: TqBitPreferencesType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := '';
  if qBPost('/app/preferences', Body) = 200 then
    Result := TJson.JsonToObject<TqBitPreferencesType>(Body, []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetPreferences(Prefs: TqBitPreferencesType): boolean;
begin
  FDuration := GetTickCount;
  var Body := 'json='+TqBitAPIUtils.URLEncode(Prefs.ToJson);
  Result := qBPost('/app/setPreferences', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetDefaultSavePath: string;
begin
  FDuration := GetTickCount;
  qBPost('/app/defaultSavePath', Result);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetLog(LastKnownId: int64 = -1; Normal: boolean = false;
      Info: boolean = false; Warning: boolean = true; Critical: boolean = true) : TqBitLogsType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := Format(
                'normal=%s&info=%s&warning=%s&critical=%s&last_known_id=%d',
                [ BStr[Normal],  BStr[Info],  BStr[Warning],  BStr[Critical], LastKnownId ]
              );
  if  qBPost('/log/main', Body) = 200 then
    Result := TJson.JsonToObject<TqBitLogsType>('{"logs":' + Body + '}', []);
  // Helper
  if (Result <> nil) and (Result.Flogs <> nil) then
    for var T in Result.Flogs do
       TqBitLogType(T)._Key :=  TqBitLogType(T).Fid;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetPeerLog(LastKnownId: int64 = -1): TqBitPeerLogsType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body :=  Format('last_known_id=%d', [lastKnownId]);
  if  qBPost('/log/peers', Body) = 200 then
    Result := TJson.JsonToObject<TqBitPeerLogsType>('{"logs":' + Body + '}', []);
  // Helper
  if (Result <> nil) and (Result.Flogs <> nil) then
    for var T in Result.Flogs do
       TqBitPeerLogType(T)._Key :=  TqBitPeerLogType(T).Fid;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetMainData(Rid: int64 = 0): TqBitMainDataType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := Format('rid=%d', [ Rid ]);
  if  qBPost('/sync/maindata', Body) = 200 then
    Result := TJson.JsonToObject<TqBitMainDataType>(Body, []);
  // Helper: this call does not fill the Fhash field up
  if (Result <> nil) and (Result.Ftorrents <> nil) then
    for var T in Result.Ftorrents do
      TqBitTorrentType(T.Value).Fhash := T.Key;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetTorrentPeersData(Hash: string;
  Rid: int64 = 0): TqBitTorrentPeersDataType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := Format('hash=%s&rid=%d', [Hash, Rid]);
  if  qBPost('/sync/torrentPeers', Body) = 200 then
    Result := TJson.JsonToObject<TqBitTorrentPeersDataType>(Body, []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetGlobalTransferInfo: TqBitGlobalTransferInfoType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := '';
  if  qBPost('/transfer/info', Body) = 200 then
    Result := TJson.JsonToObject<TqBitGlobalTransferInfoType>(Body, []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetAlternativeSpeedLimitsState: boolean;
begin
  FDuration := GetTickCount;
  Result := False;
  var Body := '';
  if  qBPost('/transfer/speedLimitsMode', Body) = 200 then
    Result := Body = '1';
end;

function TqBitAPI.ToggleAlternativeSpeedLimits: boolean;
begin
  FDuration := GetTickCount;
  Result :=  qBPost('/transfer/toggleSpeedLimitsMode' ) = 200 ;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetGlobalDownloadLimit: integer;
begin
  FDuration := GetTickCount;
  Result := 0;
  var Body := '';
  if  qBPost('/transfer/downloadLimit', Body) = 200 then
    TryStrToInt(Body, Result);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetGlobalDownloadLimit(GlobalDownloadLimit: integer): boolean;
begin
  FDuration := GetTickCount;
  var Body :=  Format('limit=%d', [GlobalDownloadLimit]);
  Result := qBPost('/transfer/setDownloadLimit', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetGlobalUploadLimit: integer;
begin
  FDuration := GetTickCount;
  Result := 0;
  var Body := '';
  if qBPost('/transfer/uploadLimit', Body) = 200 then
    TryStrToInt(Body, Result);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetGlobalUploadLimit(GlobalUploafimit: integer): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('limit=%d', [GlobalUploafimit]);
  Result := qBPost('/transfer/setUploadLimit', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.BanPeers(PeerListStr: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('peers=%s', [TqBitAPIUtils.URLEncodeDelimStr(PeerListStr, '|')]);
  Result := qBPost('/transfer/banPeers', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetNetworkInterfaces: TqBitNetworkInterfacesType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := '';
  if (qBPost('/app/networkInterfaceList', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitNetworkInterfacesType>('{"ifaces":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetNetworkInterfaceAddress(Iface: string): TqBitNetworkInterfaceAddressesType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := Format('iface=%s', [TqBitAPIUtils.URLEncode(Iface)]);
  if (qBPost('/app/networkInterfaceAddressList', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitNetworkInterfaceAddressesType>('{"adresses":' + Body + '}', []);
  FDuration := GetTickcount - FDuration
end;

function TqBitAPI.GetTorrentList(TorrentListRequest: TqBitTorrentListRequestType): TqBitTorrentsListType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := TorrentListRequest.ToParams;
  if (qBPost('/torrents/info', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitTorrentsListType>('{"torrents":' + Body + '}', []);
  // Helper
  if (Result <> nil) and (Result.Ftorrents <> nil) then
    for var T in Result.Ftorrents do
       TqBitTorrentType(T)._Key :=  TqBitTorrentType(T).Fhash;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetTorrentGenericProperties(Hash: string): TqBitTorrentInfoType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body :=  Format('hash=%s', [Hash]);
  if (qBPost('/torrents/properties', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitTorrentInfoType>(Body, []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetTorrentTrackers(Hash: string): TqBitTrackersType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := Format('hash=%s', [Hash]);
  if (qBPost('/torrents/trackers', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitTrackersType>('{"trackers":' + Body + '}', []);
  // Helper
  if (Result <> nil) and (Result.Ftrackers <> nil) then
    for var T in Result.Ftrackers do
       TqBitTrackerType(T)._Key :=  string(TqBitTrackerType(T).Ftier) + '|' + TqBitTrackerType(T).Furl;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetTorrentWebSeeds(Hash: string): TqBitWebSeedsType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := Format('hash=%s', [Hash]);
  if (qBPost('/torrents/webseeds', Body) = 200) and (Body <> '')  then
     Result := TJson.JsonToObject<TqBitWebSeedsType>('{"urls":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetTorrentContents(Hash: string; Indexes: string = ''): TqBitContentsType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := '';
  if Indexes = '' then
    Body := Format('hash=%s', [Hash])
  else
    Body := Format('hash=%s&indexes=%s', [Hash, TqBitAPIUtils.URLEncodeDelimStr(Indexes, '|')]);
  if (qBPost('/torrents/files', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitContentsType>('{"contents":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetTorrentPiecesStates(Hash: string): TqBitPiecesStatesType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body :=  Format('hash=%s', [Hash]);
  if (qBPost('/torrents/pieceStates', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitPiecesStatesType>('{"states":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.PauseTorrents(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/pause', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.ResumeTorrents(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/resume', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.DeleteTorrents(Hashes: string; DeleteFiles: boolean = False): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&deleteFiles=%s', [Hashes, BStr[DeleteFiles]]);
  Result := qBPost('/torrents/delete', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RecheckTorrents(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/recheck', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.ReannounceTorrents(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/reannounce', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.AddTrackersToTorrent(Hash, Urls: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hash=%s&urls=%s', [Hash, TqBitAPIUtils.URLEncodeDelimStr(Urls, #$A)]);
  Result := qBPost('/torrents/addTrackers', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.EditTracker(Hash, OrigUrl, NewUrl: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hash=%s&origUrl=%s&newUrl=%s',[Hash, TqBitAPIUtils.URLEncode(OrigUrl), TqBitAPIUtils.URLEncode(NewUrl)]);
  Result := qBPost('/torrents/editTracker', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RemoveTrackers(Hash, Urls: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hash=%s&urls=%s', [Hash, TqBitAPIUtils.URLEncodeDelimStr(Urls, '|')]);
  Result := qBPost('/torrents/removeTrackers', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.AddPeers(Hashes, Peers: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&peers=%s', [Hashes, TqBitAPIUtils.URLEncodeDelimStr(Peers, '|')]);
  Result := qBPost('/torrents/addPeers', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.IncreaseTorrentPriority(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/increasePrio', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.DecreaseTorrentPriority(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/decreasePrio', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.MaximalTorrentPriority(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/topPrio', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.MinimalTorrentPriority(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/bottomPrio', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetFilePriority(Hash, Ids: string; Priority: integer): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hash=%s&id=%s&priority=%d', [Hash,  TqBitAPIUtils.URLEncodeDelimStr(Ids, '|'), Priority]);
  Result := qBPost('/torrents/filePrio', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetTorrentDownloadLimit(Hashes: string): TqBitTorrentSpeedsLimitType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body :=  Format('hashes=%s', [Hashes]);
  if (qBPost('/torrents/downloadLimit', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitTorrentSpeedsLimitType>('{"speeds":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetTorrentDownloadLimit(Hashes: string;  Limit: integer): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&limit=%d', [Hashes, Limit]);
  Result := qBPost('/torrents/setDownloadLimit', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetTorrentShareLimit(Hashes: string; RatioLimit: double; SeedingTimeLimit: integer): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&ratioLimit=%0.2f&seedingTimeLimit=%d', [Hashes, RatioLimit, SeedingTimeLimit]);
  Result := qBPost('/torrents/setShareLimits', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetTorrentUploadLimit(Hashes: string): TqBitTorrentSpeedsLimitType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body :=   Format('hashes=%s', [Hashes]);
  if (qBPost('/torrents/uploadLimit', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitTorrentSpeedsLimitType>('{"speeds":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetTorrentUploadLimit(Hashes: string;  Limit: integer): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&limit=%d', [Hashes, Limit]);
  Result := qBPost('/torrents/setUploadLimit', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetTorrentLocation(Hashes, Location: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&location=%s', [Hashes, TqBitAPIUtils.URLEncode(Location)]);
  Result := qBPost('/torrents/setLocation', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetTorrentName(Hash, Name: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hash=%s&name=%s', [Hash, TqBitAPIUtils.URLEncode(Name)]);
  Result := qBPost('/torrents/rename', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetTorrentCategory(Hashes, Category: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&category=%s', [Hashes, TqBitAPIUtils.URLEncode(Category)]);
  Result := qBPost('/torrents/setCategory', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetAllCategories: TqBitCategoriesType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body :=   '';
  if (qBPost('/torrents/categories', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitCategoriesType>('{"categories":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.AddNewCategory(Category, SavePath: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('category=%s&savePath=%s', [TqBitAPIUtils.URLEncode(Category), TqBitAPIUtils.URLEncode(SavePath)]);
  Result := qBPost('/torrents/createCategory', Body) = 200;;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.EditCategory(Category, SavePath: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('category=%s&savePath=%s', [TqBitAPIUtils.URLEncode(Category), TqBitAPIUtils.URLEncode(SavePath)]);
  Result := qBPost('/torrents/editCategory', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RemoveCategories(Categories: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('categories=%s', [TqBitAPIUtils.URLEncodeDelimStr(Categories, #$A)]);
  Result := qBPost('/torrents/removeCategories', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.AddTorrentTags(Hashes, Tags: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&tags=%s', [Hashes, TqBitAPIUtils.URLEncodeDelimStr(Tags, '|')]);
  Result := qBPost('/torrents/addTags', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RemoveTorrentTags(Hashes, Tags: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&tags=%s', [Hashes, TqBitAPIUtils.URLEncodeDelimStr(Tags, '|')]);
  Result := qBPost('/torrents/removeTags', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.GetAllTags: TqBitTagsType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body :=   '';
  if (qBPost('/torrents/tags', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitTagsType>('{"tags":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.CreateTags(Tags: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('tags=%s', [TqBitAPIUtils.URLEncodeDelimStr(Tags, ',')]);
  Result := qBPost('/torrents/createTags', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.DeleteTags(Tags: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('tags=%s', [TqBitAPIUtils.URLEncodeDelimStr(Tags, ',')]);
  Result := qBPost('/torrents/deleteTags', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetAutomaticTorrentManagement(Hashes: string; Enable: boolean): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&enable=%s', [Hashes, BStr[Enable]]);
  Result := qBPost('/torrents/setAutoManagement', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.ToggleSequentialDownload(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/toggleSequentialDownload', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetFirstLastPiecePriority(Hashes: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s', [Hashes]);
  Result := qBPost('/torrents/toggleFirstLastPiecePrio', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetForceStart(Hashes: string; value: boolean): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&value=%s', [Hashes, BStr[Value]]);
  Result := qBPost('/torrents/setForceStart', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.SetSuperSeeding(Hashes: string; value: boolean): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hashes=%s&value=%s', [Hashes, BStr[Value]]);
  Result := qBPost('/torrents/setSuperSeeding', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RenameFile(Hash, OldPath, NewPath: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hash=%s&oldPath=%s&newPath=%s', [Hash, TqBitAPIUtils.URLEncode(OldPath), TqBitAPIUtils.URLEncode(NewPath)]);
  Result := qBPost('/torrents/renameFile', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RenameFolder(Hash, OldPath, NewPath: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('hash=%s&origUrl=%s&newUrl=%s', [Hash, TqBitAPIUtils.URLEncode(OldPath), TqBitAPIUtils.URLEncode(NewPath)]);
  Result := qBPost('/torrents/renameFolder', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.AddNewTorrentUrls(NewTorrentUrls: TqBitNewTorrentUrlsType): boolean;
var
  GUID: TGUID;
  Boundary: string;
  SS: TStringStream;
begin
  FDuration := GetTickCount;
  SS := nil;
  try
    SS := TStringStream.Create('', TEncoding.ASCII);
    CreateGUID(GUID);
    Boundary := '----' +
                Format(
                   '%0.8X%0.4X%0.4X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X',
                   [GUID.D1, GUID.D2, GUID.D3,
                   GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
                   GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]
                );
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="urls"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentUrls.Furls.Text);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="autoTMM"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentUrls.FautoTMM]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="savepath"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentUrls.Fsavepath);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="cookie"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentUrls.Fcookie);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="rename"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentUrls.Frename);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="category"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentUrls.Fcategory);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="paused"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentUrls.Fpaused]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="skip_checking"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentUrls.Fskip_Checking]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="contentLayout"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentUrls.FcontentLayout);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="sequentialDownload"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentUrls.FsequentialDownload]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="firstLastPiecePrio"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentUrls.FfirstLastPiecePrio]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="dlLimit"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString( TqBitAPIUtils.IIF(NewTorrentUrls.FdlLimit < 0, 'NaN', NewTorrentUrls.FdlLimit.ToString) );
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="upLimit"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString( TqBitAPIUtils.IIF(NewTorrentUrls.FupLimit < 0, 'NaN', NewTorrentUrls.FupLimit.ToString) );
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary +'--');
    SS.WriteString(#$D#$A);
    var Res := TStringStream.Create('', TEncoding.ASCII);
    Result := (qBPost('/torrents/add', SS, Res, Format('multipart/form-data; boundary=%s', [Boundary])) = 200) and (Res.DataString = 'Ok.');
    Res.Free;
  finally
    SS.Free;
  end;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.AddNewTorrentFile(NewTorrentFile: TqBitNewTorrentFileType): boolean;
var
  GUID: TGUID;
  Boundary: string;
  SS :TStringStream;
begin
  FDuration := GetTickCount;
  SS := TStringStream.Create('', TEncoding.ASCII);
  try
    CreateGUID(GUID);
    Boundary := '----' +
                Format(
                   '%0.8X%0.4X%0.4X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X',
                   [GUID.D1, GUID.D2, GUID.D3,
                   GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
                   GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]
                );
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString(Format('Content-Disposition: form-data; name="fileselect[]"; filename="%s"', [ ExtractFileName(NewTorrentFile.Ffilename) ]));
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Type: application/x-bittorrent');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    var FileStream := TFileStream.Create(NewTorrentFile.Ffilename, fmOpenRead or fmShareDenyWrite);
    SS.CopyFrom(FileStream, FileStream.Size);
    FileStream.Free;
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="autoTMM"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentFile.FautoTMM]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="savepath"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentFile.Fsavepath);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="rename"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentFile.Frename);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="category"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(NewTorrentFile.Fcategory);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="paused"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentFile.Fpaused]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="skip_checking"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentFile.Fskip_Checking]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="contentLayout"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString((NewTorrentFile.FcontentLayout));
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="sequentialDownload"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentFile.FsequentialDownload]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="firstLastPiecePrio"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString(BStr[NewTorrentFile.FfirstLastPiecePrio]);
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="dlLimit"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString( TqBitAPIUtils.IIF(NewTorrentFile.FdlLimit < 0, 'NaN', NewTorrentFile.FdlLimit.ToString) );
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary);
    SS.WriteString(#$D#$A);
    SS.WriteString('Content-Disposition: form-data; name="upLimit"');
    SS.WriteString(#$D#$A);
    SS.WriteString('');
    SS.WriteString(#$D#$A);
    SS.WriteString( TqBitAPIUtils.IIF(NewTorrentFile.FupLimit < 0, 'NaN', NewTorrentFile.FupLimit.ToString) );
    SS.WriteString(#$D#$A);
    SS.WriteString('--' + Boundary+'--');
    SS.WriteString(#$D#$A);
    var Res := TStringStream.Create('', TEncoding.ASCII);
    Result := (qBPost('/torrents/add', SS, Res, Format('multipart/form-data; boundary=%s', [Boundary])) = 200) and (Res.DataString = 'Ok.');
    Res.Free;
  finally
    SS.Free;
  end;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSAddFolder(Path: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('path=%s', [TqBitAPIUtils.URLEncode(Path)]);
  Result := qBPost('/rss/addFolder', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSAddFeed(Url, Path: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('url=%s&path=%s', [TqBitAPIUtils.URLEncode(Url), TqBitAPIUtils.URLEncode(Path)]);
  Result := qBPost('/rss/addFeed', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSRemoveItem(Path: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('path=%s', [TqBitAPIUtils.URLEncode(Path)]);
  Result := qBPost('/rss/removeItem', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSMoveItem(ItemPath, DestPath: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('itemPath=%s&destPath=%s', [TqBitAPIUtils.URLEncode(ItemPath), TqBitAPIUtils.URLEncode(DestPath)]);
  Result := qBPost('/rss/moveItem', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSGetAllItems(WithData: boolean): TqBitRSSAllItemsType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body :=   Format('withData=%s', [TqBitAPIUtils.URLEncode(BStr[WithData])]);
  if (qBPost('/rss/items', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitRSSAllItemsType>('{"items":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSMarkAsRead(ItemPath, ArticleId: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('itemPath=%s&articleId=%s', [TqBitAPIUtils.URLEncode(ItemPath), TqBitAPIUtils.URLEncode(ArticleId)]);
  Result := qBPost('/rss/markAsRead', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSRefreshItem(ItemPath: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('itemPath=%s', [TqBitAPIUtils.URLEncode(ItemPath)]);
  Result := qBPost('/rss/refreshItem', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSSetAutoDownloadingRules(RuleName: string;
  RuleDef: TqBitRSSRuleType): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('ruleName=%s&ruleDef={}', [TqBitAPIUtils.URLEncode(RuleName)]);
  if assigned(RuleDef) then
    Body :=  Format('ruleName=%s&ruleDef=%s', [TqBitAPIUtils.URLEncode(RuleName), TqBitAPIUtils.URLEncode(RuleDef.ToJSON)]);
  Result := qBPost('/rss/setRule', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSRenameAutoDownloadingRules(RuleName, NewRuleName: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('ruleName=%s&newRuleName=%s', [TqBitAPIUtils.URLEncode(RuleName), TqBitAPIUtils.URLEncode(NewRuleName)]);
  Result := qBPost('/rss/renameRule', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSRemoveAutoDownloadingRules(RuleName: string): boolean;
begin
  FDuration := GetTickCount;
  var Body := Format('ruleName=%s', [TqBitAPIUtils.URLEncode(RuleName)]);
  Result := qBPost('/rss/removeRule', Body) = 200;
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSGetAllAutoDownloadingRules: TqBitAutoDownloadingRulesType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := '';
  if (qBPost('/rss/rules', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitAutoDownloadingRulesType>('{"rules":' + Body + '}', []);
  FDuration := GetTickcount - FDuration;
end;

function TqBitAPI.RSSGetMatchingArticles(RuleName: string): TqBitRSSArticlesType;
begin
  FDuration := GetTickCount;
  Result := nil;
  var Body := Format('ruleName=%s', [TqBitAPIUtils.URLEncode(RuleName)]);
  if (qBPost('/rss/matchingArticles', Body) = 200) and (Body <> '')  then
    Result := TJson.JsonToObject<TqBitRSSArticlesType>('{"articles":' + Body + '}', []);
  FDuration := GetTickcount - FDuration
end;


end.

