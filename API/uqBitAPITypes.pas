unit uqBitAPITypes;

///
///  Author:  Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///
///  API v2.8.3 + Hidden/Missing Fields
///  https://github.com/bnzbnz/qBit4Delphi:
///  https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)
///
///  License: MPL 1.1 / GPL 2.1
///

interface
uses System.Generics.Collections, REST.JsonReflect, system.JSON, REST.Json.Types,
     System.Generics.Defaults, Classes, SyncObjs;

const

  qBitAPI_WebAPIVersion = '2.8.19';
  qBitAPI_Developer = 'Laurent Meyer (qBit4Delphi@ea4d.com)';

type

  TJsonVarHelper = class
    class function StrToVar( Str: string ): variant;
    class function VarToJsonStr( V: Variant ): string;
  end;

  TJsonUserRec = record
    Val: variant;
    OwnObj: Boolean;
    Obj: TObject;
    procedure SetObject(aObject: TObject; aOwnObject: Boolean = False);
  end;

  TJsonBaseType = class
  protected
    procedure ClonePropertiesTo(T : TJsonBaseType); virtual;
    procedure MergePropertiesFrom(T: TJsonBaseType);
  public
    [JSONMarshalled(False)]
    _Key: variant;
    [JSONMarshalled(False)]
    _UserRec: TJsonUserRec;
    function Clone: TJsonBaseType; virtual;
    constructor Create; overload;
    destructor Destroy; override;
    procedure Merge(T: TJsonBaseType); virtual;
    procedure Clear; virtual;
    function ToJSON: string; virtual;
    function ToParams: string; virtual;
  end;

  {$REGION 'Custom Types Intf.'}

  TqBitNewTorrentUrlsType = class
    Furls: TStringList;
    FautoTMM: boolean;
    FsavePath : string;
    Fcookie: string;
    Frename: string;
    Fcategory: string;
    Fpaused: boolean;
    Fskip_Checking: boolean;
    FcontentLayout: string;
    FsequentialDownload: boolean;
    FfirstLastPiecePrio: boolean;
    FupLimit: integer;
    FdlLimit: integer;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TqBitNewTorrentFileType = class
    Ffilename: string;
    FautoTMM: boolean;
    FsavePath : string;
    Frename: string;
    Fcategory: string;
    Fpaused: boolean;
    Fskip_Checking: boolean;
    FcontentLayout: string;
    FsequentialDownload: boolean;
    FfirstLastPiecePrio: boolean;
    FupLimit: integer;
    FdlLimit: integer;
    constructor Create; overload;
  end;

  {$ENDREGION} // Custom Types Intf.

  {$REGION 'JSON Interceptor Types Intf.'}

  TqBitObjectListInterceptor = class(TJSONInterceptor)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    function StringConverter(Data: TObject; Field: string): string; override;
    function ProcessReverter<T: class, constructor>(DataClass: TClass; Data: TObject; FieldName : string; Field, Arg: string): Boolean;
    function ProcessConverter<T: class, constructor>(DataClass: TClass; Data: TObject; FieldName: string; Field: string; var JSONStr: string): boolean;
  end;

  TqBitVariantListInterceptor = class(TJSONInterceptor)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    function StringConverter(Data: TObject; Field: string): string; override;
  end;

  TqBitObjectDictionaryInterceptor = class(TJSONInterceptor)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    function StringConverter(Data: TObject; Field: string): string; override;
    function ProcessReverter<T: class, constructor>(DataClass: TClass; Data: TObject; FieldName: string; Field, Arg: string): Boolean;
    function ProcessConverter<T: class, constructor>(DataClass: TClass; Data: TObject; FieldName: string; Field: string; var JSONStr: string): boolean;
  end;

  TqBitVariantDictionaryInterceptor = class(TJSONInterceptor)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    function StringConverter(Data: TObject; Field: string): string; override;
  end;

  TqBitStringListDictionaryInterceptor = class(TJSONInterceptor)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    function StringConverter(Data: TObject; Field: string): string; override;
  end;

  {$ENDREGION} // JSON Interceptor Types Intf.

  {$REGION 'Generic Types Intf.'}

  TqBitList<A> = class(TList<variant>)
    function Clone: TqBitList<A>; overload;
    function Merge(From: TqBitList<A>): variant; overload;
    function Merge(From: TqBitList<A>; var  Added: TqBitList<variant>): variant; overload;
  end;

  TqBitObjectDictionary<A, B>= class(TObjectDictionary<variant, TJsonBaseType>)
    function Clone: TqBitObjectDictionary<A, B>;
    function Merge(From: TqBitObjectDictionary<A, B>; var Added: TqBitList<variant>; var Modified: TqBitList<variant>): variant;
  end;

  TqBitStringListDictionary<A, B>= class(TObjectDictionary<variant, TStringList>)
    function Clone: TqBitStringListDictionary<A, B>;
    function Merge(From: TqBitStringListDictionary<A, B>; var  Added: TqBitList<variant>; var Modified: TqBitList<variant>): variant;
  end;

  TqBitVariantDictionary<A,B> = class(TObjectDictionary<variant, variant>)
     function Clone: TqBitVariantDictionary<A, B>;
     function Merge(From: TqBitVariantDictionary<A, B>; var Added: TqBitList<variant>; var Modified: TqBitList<variant>): variant;
  end;

  TqBitObjectList<A> = class(TObjectList<TJsonBaseType>)
    function Clone: TqBitObjectList<A>;
    function Merge(From: TqBitObjectList<A>): variant; overload;
    function Merge(From: TqBitObjectList<A>; var Added: TqBitObjectList<A>): variant; overload;
    function Merge(From: TqBitObjectList<A>; var Added, Modified, Removed: TqBitObjectList<A>): variant; overload;
  end;

  {$ENDREGION} // 'Generic Types Intf.'

  {$REGION 'JSON Types Intf.'}

  TqBitBuildInfoType = class(TJsonBaseType)
    Fbitness : variant;
    Fboost : variant;
    Flibtorrent : variant;
    Fopenssl : variant;
    Fqt : variant;
    Fzlib: variant;
  end;

  TqBitPreferencesType = class(TJsonBaseType)
    Ftorrent_content_layout: variant;
    Fstart_paused_enabled: variant;
    Fauto_delete_mode: variant;
    Fpreallocate_all: variant;
    Fincomplete_files_ext: variant;
    Fauto_tmm_enabled: variant;
    Ftorrent_changed_tmm_enabled: variant;
    Fsave_path_changed_tmm_enabled: variant;
    Fcategory_changed_tmm_enabled: variant;
    Fsave_path: variant;
    Ftemp_path_enabled: variant;
    Ftemp_path: variant;
    Fexport_dir: variant;
    Fmail_notification_enabled: variant;
    Fmail_notification_sender: variant;
    Fmail_notification_email: variant;
    Fmail_notification_smtp: variant;
    Fmail_notification_ssl_enabled: variant;
    Fmail_notification_auth_enabled: variant;
    Fmail_notification_username: variant;
    Fmail_notification_password: variant;
    Fautorun_enabled: variant;
    Fautorun_program: variant;
    Flisten_port: variant;
    Fupnp: variant;
    Frandom_port: variant;
    Fmax_connec: variant;
    Fmax_connec_per_torrent: variant;
    Fmax_uploads: variant;
    Fmax_uploads_per_torrent: variant;
    Fproxy_type: variant;
    Fproxy_auth_enabled: variant;
    Fproxy_ip: variant;
    Fproxy_port: variant;
    Fproxy_peer_connections: variant;
    Fproxy_torrents_only: variant;
    Fproxy_username: variant;
    Fproxy_password: variant;
    Fip_filter_enabled: variant;
    Fip_filter_path: variant;
    Fip_filter_trackers: variant;
    Fbanned_IPs: variant;
    Fup_limit: variant;
    Fdl_limit: variant;
    Falt_up_limit: variant;
    Falt_dl_limit: variant;
    Fbittorrent_protocol: variant;
    Flimit_utp_rate: variant;
    Flimit_tcp_overhead: variant;
    Flimit_lan_peers: variant;
    Fscheduler_enabled: variant;
    Fdht: variant;
    Fpex: variant;
    Flsd: variant;
    Fencryption: variant;
    Fanonymous_mode: variant;
    Fqueueing_enabled: variant;
    Fmax_active_downloads: variant;
    Fmax_active_uploads: variant;
    Fmax_active_torrents: variant;
    Fdont_count_slow_torrents: variant;
    Fslow_torrent_dl_rate_threshold: variant;
    Fslow_torrent_ul_rate_threshold: variant;
    Fslow_torrent_inactive_timer: variant;
    Fmax_ratio_enabled: variant;
    Fmax_ratio: variant;
    Fmax_seeding_time_enabled: variant;
    Fmax_seeding_time: variant;
    Fmax_ratio_act: variant;
    Fexport_dir_fin: variant;
    Fadd_trackers_enabled: variant;
    Fadd_trackers: variant;
    Frss_processing_enabled: variant;
    Frss_refresh_interval: variant;
    Frss_max_articles_per_feed: variant;
    Frss_auto_downloading_enabled: variant;
    Frss_download_repack_proper_episodes: variant;
    Frss_smart_episode_filters: variant;
    Flocale: variant;
    Fweb_ui_domain_list: variant;
    Fweb_ui_address: variant;
    Fweb_ui_port: variant;
    Fweb_ui_upnp: variant;
    Fuse_https: variant;
    Fweb_ui_https_cert_path: variant;
    Fweb_ui_https_key_path: variant;
    Fweb_ui_username: variant;
    Fbypass_local_auth: variant;
    Fbypass_auth_subnet_whitelist_enabled: variant;
    Fbypass_auth_subnet_whitelist: variant;
    Fweb_ui_max_auth_fail_count: variant;
    Fweb_ui_ban_duration: variant;
    Fweb_ui_session_timeout: variant;
    Falternative_webui_enabled: variant;
    Falternative_webui_path: variant;
    Fweb_ui_clickjacking_protection_enabled: variant;
    Fweb_ui_csrf_protection_enabled: variant;
    Fweb_ui_secure_cookie_enabled: variant;
    Fweb_ui_host_header_validation_enabled: variant;
    Fweb_ui_use_custom_http_headers_enabled: variant;
    Fweb_ui_custom_http_headers: variant;
    Fdyndns_enabled: variant;
    Fdyndns_service: variant;
    Fdyndns_domain: variant;
    Fdyndns_username: variant;
    Fdyndns_password: variant;
    Fcurrent_network_interface: variant;
    Fcurrent_interface_address: variant;
    Fsave_resume_data_interval: variant;
    Frecheck_completed_torrents: variant;
    Fresolve_peer_countries: variant;
    Fasync_io_threads: variant;
    Fhashing_threads: variant;
    Ffile_pool_size: variant;
    Fchecking_memory_use: variant;
    Fdisk_cache: variant;
    Fdisk_cache_ttl: variant;
    Fenable_os_cache: variant;
    Fenable_coalesce_read_write: variant;
    Fenable_piece_extent_affinity: variant;
    Fenable_upload_suggestions: variant;
    Fsend_buffer_watermark: variant;
    Fsend_buffer_low_watermark: variant;
    Fsend_buffer_watermark_factor: variant;
    Fsocket_backlog_size: variant;
    Foutgoing_ports_min: variant;
    Foutgoing_ports_max: variant;
    Fupnp_lease_duration: variant;
    Fpeer_tos: variant;
    Futp_tcp_mixed_mode: variant;
    Fidn_support_enabled: variant;
    Fenable_multi_connections_from_same_ip: variant;
    Fvalidate_https_tracker_certificate: variant;
    Fssrf_mitigation: variant;
    Fblock_peers_on_privileged_ports: variant;
    Fenable_embedded_tracker: variant;
    Fembedded_tracker_port: variant;
    Fupload_slots_behavior: variant;
    Fupload_choking_algorithm: variant;
    Fannounce_to_all_trackers: variant;
    Fannounce_to_all_tiers: variant;
    Fannounce_ip: variant;
    Fmax_concurrent_http_announces: variant;
    Fstop_tracker_timeout: variant;
    Fpeer_turnover: variant;
    Fpeer_turnover_cutoff: variant;
    Fpeer_turnover_interval: variant;
    Fdownload_in_scan_dirs: variant;
    Fexport_dir_enabled: variant;
    Fschedule_from_hour: variant;
    Fschedule_from_min: variant;
    Fschedule_to_hour: variant;
    Fschedule_to_min: variant;
    Fscheduler_days: variant;
    Fweb_ui_password: variant;
    [JsonReflect(ctstring, rtString, TqBitVariantDictionaryInterceptor)]
    Fscan_dirs: TqBitVariantDictionary<variant, variant>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
  end;

  TqBitLogType = class(TJsonBaseType)
    Fid: variant;
    Fmessage: variant;
    Ftimestamp: variant;
    Ftype: variant;
    // inherited Merge/Clone
  end;

  TqBitLogsType = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Flogs: TqBitObjectList<TqBitLogType>;
    function Clone: TJsonBaseType; override;
    procedure Merge(From: TJsonBaseType); override;
    destructor Destroy; override;
  end;

  TqBitPeerLogType = class(TJsonBaseType)
    Fid: variant;
    Fip: variant;
    Ftimestamp: variant;
    Fblocked: variant;
    Freason: variant;
    // inherited Merge/Clone
  end;

  TqBitPeerLogsType = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Flogs: TqBitObjectList<TqBitPeerLogType>;
    function Clone: TJsonBaseType; override;
    procedure Merge(From: TJsonBaseType); override;
    destructor Destroy; override;
  end;

  TqBitTorrentType = class(TJsonBaseType)
    Fhash: string;
    Fadded_on: variant;
    Famount_left: variant;
    Fauto_tmm: variant;
    Favailability: variant;
    Fcategory: variant;
    Fcompleted: variant;
    Fcompletion_on: variant;
    Fcontent_path: variant;
    Fdl_limit: variant;
    Fdlspeed: variant;
    Fdownloaded: variant;
    Fdownloaded_session: variant;
    Feta: variant;
    Ff_l_piece_prio: variant;
    Fforce_start: variant;
    Finfohash_v1: variant;
    Finfohash_v2: variant;
    Flast_activity: variant;
    Fmagnet_uri: variant;
    Fmax_ratio: variant;
    Fmax_seeding_time: variant;
    Fname: variant;
    Fnum_complete: variant;
    Fnum_incomplete: variant;
    Fnum_leechs: variant;
    Fnum_seeds: variant;
    Fpriority: variant;
    Fprogress: variant;
    Fratio: variant;
    Fratio_limit: variant;
    Fsave_path: variant;
    Fseeding_time: variant;
    Fseeding_time_limit: variant;
    Fseen_complete: variant;
    Fseq_dl: variant;
    Fsize: variant;
    Fstate: variant;
    Fsuper_seeding: variant;
    Ftags: variant;
    Ftime_active: variant;
    Ftotal_size: variant;
    Ftracker: variant;
    Ftrackers_count: variant;
    Fup_limit: variant;
    Fuploaded: variant;
    Fuploaded_session: variant;
    Fupspeed: variant;
     // inherited Merge/Clone
  end;

  TqBitCategoryType = class(TJsonBaseType)
    Fname: variant;
    FsavePath: variant;
    Fdownload_path: variant; // Default = null, No = false, Yes = string value
     // inherited Merge/Clone
  end;

  TqBitserver_stateType = class(TJsonBaseType)
    Falltime_dl: variant;
    Falltime_ul: variant;
    Faverage_time_queue: variant;
    Fconnection_status: variant;
    Fdht_nodes: variant;
    Fdl_info_data: variant;
    Fdl_info_speed: variant;
    Fdl_rate_limit: variant;
    Ffree_space_on_disk: variant;
    Fglobal_ratio: variant;
    Fqueued_io_jobs: variant;
    Fqueueing: variant;
    Fread_cache_hits: variant;  // Libtorrent 1x Only
    Fread_cache_overload: variant;
    Frefresh_interval: variant;
    Ftotal_buffers_size: variant;
    Ftotal_peer_connections: variant;
    Ftotal_queued_size: variant;
    Ftotal_wasted_session: variant;
    Fup_info_data: variant;
    Fup_info_speed: variant;
    Fup_rate_limit: variant;
    Fuse_alt_speed_limits: variant;
    Fwrite_cache_overload: variant;
     // inherited Merge/Clone
  end;

  TqBitMainDataType = class(TJsonBaseType)
    Ffull_update: variant;
    Frid: variant;
    Fserver_state: TqBitserver_stateType;
    [JsonReflect(ctString, rtString, TqBitObjectDictionaryInterceptor)]
    Fcategories: TqBitObjectDictionary<variant, TqBitCategoryType>;
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Fcategories_removed: TqBitList<variant>;
    [JsonMarshalled(false)]
    _Fcategories_added: TqBitList<variant>; // Custom Internal
    [JsonMarshalled(false)]
    _Fcategories_modified: TqBitList<variant>; // Custom Internal
    [JsonMarshalled(false)]
    _Fcategories_changed: variant; // Custom Internal
    [JsonMarshalled(false)]
    _Fcategories_count_changed: variant; // Custom Internal
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Ftags: TqBitList<variant>;
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Ftags_removed: TqBitList<variant>;
    [JsonMarshalled(false)]
    _Ftags_added: TqBitList<variant>;  // Custom Internal
    [JsonMarshalled(false)]
    _Ftags_modified: TqBitList<variant>;  // Custom Internal
    [JsonMarshalled(false)]
    _Ftags_changed: variant; // Custom Internal
    [JsonMarshalled(false)]
    _Ftags_count_changed: variant; // Custom Internal
    [JsonReflect(ctString, rtString, TqBitObjectDictionaryInterceptor)]
    Ftorrents: TqBitObjectDictionary<variant, TqBitTorrentType>;
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Ftorrents_removed: TqBitList<variant>;
    [JsonMarshalled(false)]
    _Ftorrents_added: TqBitList<variant>;  // Custom Internal
    [JsonMarshalled(false)]
    _Ftorrents_modified: TqBitList<variant>;  // Custom Internal
    [JsonMarshalled(false)]
    _Ftorrents_changed: variant; // Custom Internal
    [JsonMarshalled(false)]
    _Ftorrents_count_changed: variant; // Custom Internal
    [JsonReflect(ctString, rtString, TqBitStringListDictionaryInterceptor)]
    Ftrackers: TqBitStringListDictionary<variant, TStringList>;
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Ftrackers_removed: TqBitList<variant>;
    [JsonMarshalled(false)]
    _Ftrackers_added: TqBitList<variant>;  // Custom Internal
    [JsonMarshalled(false)]
    _Ftrackers_modified: TqBitList<variant>;  // Custom Internal
    [JsonMarshalled(false)]
    _Ftrackers_changed: variant; // Custom Internal
    [JsonMarshalled(false)]
    _Ftrackers_count_changed: variant; // Custom Internal
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
    function Clone: TJsonBaseType; override;
  end;

  TqBitTorrentPeerDataType = class(TJsonBaseType)
    Fclient: variant;
    Fconnection: variant;
    Fcountry: variant;
    Fcountry_code: variant;
    Fdl_speed: variant;
    Fdownloaded: variant;
    Ffiles: variant;
    Fflags: variant;
    Fflags_desc: variant;
    Fip: variant;
    Fport: variant;
    Fprogress:variant;
    Frelevance: variant;
    Fup_speed: variant;
    Fuploaded: variant;
     // inherited Merge/Clone
  end;

  TqBitTorrentPeersDataType = class(TJsonBaseType)
    Ffull_update: variant;
    Frid: variant;
    [JsonReflect(ctString, rtString, TqBitObjectDictionaryInterceptor)]
    Fpeers: TqBitObjectDictionary<variant, TqBitTorrentPeerDataType>;
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Fpeers_removed: TqBitList<variant>;
    [JsonMarshalled(false)]
    _Fpeers_added: TqBitList<variant>;  // Custom Internal
    [JsonMarshalled(false)]
    _Fpeers_modified: TqBitList<variant>;  // Custom Internal
    [JsonMarshalled(false)]
    _Fpeers_changed: variant; // Custom Internal
    [JsonMarshalled(false)]
    _Fpeers_count_changed: variant; // Custom Internal
    Fshow_flags: variant;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
    function Clone: TJsonBaseType; override;
  end;

  TqBitGlobalTransferInfoType = class(TJsonBaseType)
    Fdl_info_speed: variant;
    Fdl_info_data	: variant;
    Fup_info_speed: variant;
    Fup_info_data: variant;
    Fdl_rate_limit: variant;
    Fup_rate_limit: variant;
    Fdht_nodes: variant;
    Fconnection_status: variant;
    Fqueueing: variant;
    Fuse_alt_speed_limits: variant;
    Frefresh_interval: variant;
     // inherited Merge/Clone
  end;

  TqBitTorrentListRequestType = class(TJsonBaseType)
    Ffilter: variant;
    Fcategory: variant;
    Ftag: variant;
    Fsort: variant;
    Freverse: variant;
    Flimit: variant;
    Foffset: variant;
    Fhashes: TStringList;
    constructor Create; overload;
    destructor Destroy; override;
    procedure Clear; override;
    function ToParams: string; override;
     // inherited Merge/Clone
  end;

  TqBitTorrentsListType  = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Ftorrents: TqBitObjectList<TqBitTorrentType>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitTorrentInfoType = class(TJsonBaseType)
    Fsave_path: variant;
    Fcreation_date: variant;
    Fpiece_size: variant;
    Fcomment: variant;
    Ftotal_wasted: variant;
    Ftotal_uploaded: variant;
    Ftotal_uploaded_session: variant;
    Ftotal_downloaded: variant;
    Ftotal_downloaded_session: variant;
    Fup_limit: variant;
    Fdl_limit: variant;
    Ftime_elapsed: variant;
    Fseeding_time: variant;
    Fnb_connections: variant;
    Fnb_connections_limit: variant;
    Fshare_ratio: variant;
    Faddition_date: variant;
    Fcompletion_date: variant;
    Fcreated_by: variant;
    Fdl_speed_avg: variant;
    Fdl_speed: variant;
    Feta: variant;
    Flast_seen: variant;
    Fpeers: variant;
    Fpeers_total: variant;
    Fpieces_have: variant;
    Fpieces_num: variant;
    Freannounce: variant;
    Fseeds: variant;
    Fseeds_total: variant;
    Ftotal_size: variant;
    Fup_speed_avg: variant;
    Fup_speed: variant;
     // inherited Merge/Clone
  end;

  TqBitTrackerType  = class(TJsonBaseType)
    Furl: variant;
    Fstatus: variant;
    Ftier: variant;
    Fnum_peers: variant;
    Fnum_seeds: variant;
    Fnum_leeches: variant;
    Fnum_downloaded: variant;
    Fmsg: variant;
     // inherited Merge/Clone
  end;

  TqBitTrackersType  = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Ftrackers: TqBitObjectList<TqBitTrackerType>;
    procedure Merge(From: TJsonBaseType); override;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
  end;

  TqBitWebSeedType  = class(TJsonBaseType)
    Furl: variant;
     // inherited Merge/Clone
  end;

  TqBitWebSeedsType  = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Furls: TqBitObjectList<TqBitWebSeedType>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitContentType = class(TJsonBaseType)
    Findex: variant;
    Fname: variant;
    Fsize: variant;
    Fprogress: variant;
    Fpriority: variant;
    Fis_seed: variant;
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Fpiece_range: TqBitList<variant>;
    Favailability: variant;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitContentsType = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Fcontents: TqBitObjectList<TqBitContentType>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitPiecesStatesType = class(TJsonBaseType)
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Fstates: TqBitList<variant>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitTorrentSpeedsLimitType = class(TJsonBaseType)
    [JsonReflect(ctString, rtString, TqBitVariantDictionaryInterceptor)]
    Fspeeds: TqBitVariantDictionary<variant, variant>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitCategoriesType  = class(TJsonBaseType)
    [JsonReflect(ctString, rtString, TqBitObjectDictionaryInterceptor)]
    Fcategories: TqBitObjectDictionary<variant, TqBitCategoryType>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitTagsType = class(TJsonBaseType)
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Ftags: TqBitList<variant>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitRSSArticleType = class(TJsonBaseType)
    Fcategory: variant;
    Fdata: variant;
    Fdescription: variant;
    Fid: variant;
    Flink: variant;
    Ftitle: variant;
    FtorrentURL: variant;
    // inherited Merge/Clone
  end;

  TqBitRSSItemType = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Farticles: TqBitObjectList<TqBitRSSArticleType>;
    FisLoading: variant;
    lastBuildDate: variant;
    Ftitle: variant;
    Fuid: variant;
    Furl: variant;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitRSSAllItemsType = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectDictionaryInterceptor)]
    Fitems: TqBitObjectDictionary<variant, TqBitRSSItemType>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitRSSRuleType  = class(TJsonBaseType)
    Fenabled: variant;
    FmustContain: variant;
    FmustNotContain: variant;
    FuseRegex: variant;
    FepisodeFilter: variant;
    FsmartFilter: variant;
    FpreviouslyMatchedEpisodes: variant;
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    FaffectedFeeds: TqBitList<variant>;
    FignoreDays: variant;
    FlastMatch : variant;
    FaddPaused: variant;
    FassignedCategory: variant;
    FsavePath: variant;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitRSSAllRulesType = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectDictionaryInterceptor)]
    Frules: TqBitObjectDictionary<variant, TqBitRSSRuleType>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitAutoDownloadingRulesType = class(TJsonBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectDictionaryInterceptor)]
    Frules: TqBitObjectDictionary<variant, TqBitRSSRuleType>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitRSSArticlesType = class(TJsonBaseType)
    [JsonReflect(ctString, rtString, TqBitStringListDictionaryInterceptor)]
    Farticles: TqBitStringListDictionary<variant, TStringList>;
    function Clone: TJsonBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TJsonBaseType); override;
  end;

  TqBitNetworkInterfaceType = class(TJsonBaseType)
    Fname: variant;
    Fvalue: variant;
  end;

  TqBitNetworkInterfacesType = class(TJsonBaseType)
   [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Fifaces: TqBitObjectList<TqBitNetworkInterfaceType>;
    destructor Destroy; override;
  end;

  TqBitNetworkInterfaceAddressesType = class(TJsonBaseType)
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Fadresses: TqBitList<variant>;
    destructor Destroy; override;
  end;

  {$ENDREGION} // 'JSON Types Intf.'

implementation
uses  SysUtils, REST.Json, NetEncoding, Variants, RTTI, uqBitAPIUtils, StrUtils, Math;

{$REGION 'Helpers Impl.'}

type

  TJsonRawPatcher = class(TObject)
    FRaw: TDictionary<string, string>;
    FKeys: TList<string>;
    FLock: TCriticalSection;
    constructor Create; overload;
    destructor Destroy; override;
    function Encode(JsonStr: string; Header: string = '"'; Footer: string = '"'): string;
    procedure Decode(var JsonStr: string);
  end;

var

  JsonRawPatcher: TJsonRawPatcher;

procedure TJsonUserRec.SetObject(aObject: TObject; aOwnObject: Boolean);
begin
  Self.OwnObj := aOwnObject;
  Self.Obj := aObject;
end;

class function TJsonVarHelper.StrToVar( Str: string ): variant;
begin
  var i64 := Int64(0);
  var Ext := Extended(0);
  if Int64.TryParse(Str, i64) then Result := i64 else
  if Extended.TryParse(Str, Ext) then Result := Ext else
  Result := Str;
end;

class function TJsonVarHelper.VarToJsonStr( V: Variant ): string;
begin
  Result := VarToStr(V);
  if VarIsStr(V) then Result := '"' + VarToStr(V) + '"';
  Result := TJson.JsonEncode(Result)
end;

{ TJsonRawPatcher }

constructor TJsonRawPatcher.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FKeys := TList<string>.Create;
  FRaw := TDictionary<string, string>.Create;
end;

destructor TJsonRawPatcher.Destroy;
begin
  FKeys.Free;
  FRaw.Free;
  FLock.Free;
  inherited;
end;

procedure TJsonRawPatcher.decode(var JsonStr: string);

  function RPos(const aSubStr, aString : string; const aStartPos: Integer): Integer;
  begin
    for Result := aStartPos - length(aSubStr) + 1 downto 1 do
      if CompareMem(Pointer(aSubStr), @(aString[Result]), Length(aSubStr)) then Exit;
    Result := 0;
  end;

  function ReplaceBackwardPattern(var InString: string; WhatToReplace, WhatToReplaceWith: string; var Position: Integer): Boolean;
  begin
    Position  := RPos(WhatToReplace, Instring, Position);
    if position > 0 then
    begin
      Move(WhatToReplaceWith[1], InString[Position], Length(WhatToReplaceWith) * SizeOf(Char));
      Position := Position + Length(WhatToReplaceWith);
      Result := True
    end else begin
      Position := Pos(WhatToReplace, InString);
      if Position > 0 then
      begin
        Move(WhatToReplaceWith[1], InString[Position], Length(WhatToReplaceWith) * SizeOf(Char));
        Position := Position - Length(WhatToReplaceWith);
        Result := True
      end else
        Result := False;
    end;
  end;

begin
  var Del := TList<string>.Create;
  FLock.Acquire;
  var Key := TList<string>.Create(FKeys);
  var Raw := TDictionary<string, string>.Create(FRaw);
  FLock.Release;

  var Value := '';
  var Position := 0;
  for var KeyIdx := Key.count - 1 downto 0 do
    if Raw.TryGetValue(Key[KeyIdx], Value) then
      if ReplaceBackwardPattern(JsonStr, Key[KeyIdx], Value, Position) then
        Del.Add(Key[KeyIdx]);

  FLock.Acquire;
  for var d in Del do
  begin
    FKeys.Remove(d);
    FRaw.Remove(d);
  end;
  FLock.Release;
  Raw.Free;
  Key.Free;
  Del.Free;
end;

function TJsonRawPatcher.Encode(JsonStr: string; Header: string = '"'; Footer: string = '"'): string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := Format(
    '%0.8X%0.4X%0.4X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X',
    [Guid.D1, Guid.D2, Guid.D3,
    Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
    Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]
  );
  var x := Length(Header + Result + Footer) - Length(JsonStr);
  if x > 0 then JsonStr := JsonStr + StringOfChar(' ', Abs(x)) else
  if x < 0 then Result := Result + StringOfChar(' ', Abs(x));
  var Key := Header + Result + Footer;
  FLock.Acquire;
  FRaw.Add(Key, JsonStr);
  FKeys.Add(Key);
  FLock.Release;
end;

{$ENDREGION} // 'Helpers Impl.'

{$REGION 'JSON Interceptor Impl.'}

procedure TqBitStringListDictionaryInterceptor.StringReverter(Data: TObject; Field: string; Arg: string);
var
  ctx: TRttiContext;
begin

  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  var SLDic := TqBitStringListDictionary<variant, TStringList>.Create([doOwnsValues]);
  RTTIField.SetValue(Data, SLDic);

  var JSONObj:= TJSONObject.ParseJSONValue(Arg) as TJSONObject;
  for var JSONPair in JSONObj do
  begin
    var List := TStringList.Create;
    for var kp in  JSONPair.JsonValue as TJSONArray do List.Add(kp.Value);
    SLDic.Add(JSONPair.JsonString.Value, List);
  end;
  JSONObj.Free;

end;

function TqBitStringListDictionaryInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRttiContext;
begin

  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  var SLDic := TqBitStringListDictionary<variant, TStringList>( RTTIField.GetValue(Data).AsObject );

  var SL := TqBitAPIUtils.DelimStringList(nil, ',', '');
  for var kv in SLDic do
  begin
    var v := TqBitAPIUtils.DelimStringList(nil, ',', '');
    for var i in Kv.Value do v.Add('"' + i + '"');
    SL.Add( '"' + kv.Key + '":[' + v.DelimitedText + ']' );
    v.Free;
  end;
  Result:= JsonRawPatcher.Encode('{' + SL.DelimitedText + '}');
  SL.Free;

end;

{ TqBitObjectListInterceptor }

function TqBitObjectListInterceptor.ProcessReverter<T>(DataClass: TClass; Data: TObject; FieldName : string; Field, Arg: string): boolean;
var
  ctx: TRttiContext;
begin
  Result := False;
  if (not (Data is DataClass)) or (FieldName <> Field) then Exit;
  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  if RTTIField = nil then Exit;

  var v :=  TqBitObjectList<T>.Create(True);
  var JSONArr := TJSONObject.ParseJSONValue(Arg) as TJSONArray;
  for var i:= 0 to JSONArr.Count -1 do
    v.Add( TJsonBaseType(TJSON.JsonToObject<T>( JSONArr.Items[i] as TJSONObject )) );
  JSONArr.Free;

  RTTIField.SetValue(Data, v);
  Result := True;
end;

procedure TqBitObjectListInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin

  if not ProcessReverter<TqBitNetworkInterfaceType>(TqBitNetworkInterfacesType, Data, 'Fifaces', Field, Arg) then
  if not ProcessReverter<TqBitRSSArticleType>(TqBitRSSItemType, Data, 'Farticles', Field, Arg) then
  if not ProcessReverter<TqBitLogType>(TqBitLogsType, Data, 'Flogs', Field, Arg) then
  if not ProcessReverter<TqBitPeerLogType>(TqBitPeerLogsType, Data, 'Flogs', Field, Arg) then
  if not ProcessReverter<TqBitTorrentType>(TqBitTorrentsListType, Data, 'Ftorrents', Field, Arg) then
  if not ProcessReverter<TqBitTrackerType>(TqBitTrackersType, Data, 'Ftrackers', Field, Arg) then
  if not ProcessReverter<TqBitWebSeedType>(TqBitWebSeedsType, Data, 'Furls', Field, Arg) then
  if not ProcessReverter<TqBitContentType>(TqBitContentsType, Data, 'Fcontents', Field, Arg) then
  TqBitAPIUtils.RaiseException(Format(
      'Class: %s, %s - %s not implemented.',
      [Data.ClassName, Field, 'TqBitObjectListInterceptor.StringReverter']
    ));

end;

function TqBitObjectListInterceptor.ProcessConverter<T>(DataClass: TClass; Data: TObject;  FieldName: string; Field: string; var JSONStr: string): boolean;
var
  ctx: TRttiContext;
begin
  Result := False;
  JSONStr := '';
  if not (Data is DataClass) or (Field <> Fieldname) then Exit;
  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  if RTTIField = nil then Exit;

  var SL := TqBitAPIUtils.DelimStringList(nil, ',', '');
  var v := TObjectList<T>(RTTIField.GetValue(Data).AsObject);
  if v <> nil then
    for var i := 0 to v.Count - 1 do
      SL.Add(TJson.ObjectToJsonString(v[i]));
  JSONStr := SL.DelimitedText;
  SL.Free;
  Result := True;
end;

function TqBitObjectListInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := '';
  var Header := '{"' + Copy(Field, 2, MAXINT) + '":"';
  var Footer := '"}';
  var SL := TqBitAPIUtils.DelimStringList(nil, ',', '');

  if not ProcessConverter<TqBitTorrentType>(TqBitTorrentsListType, Data, 'Ftorrents', Field, Result) then
  if not ProcessConverter<TqBitNetworkInterfaceType>(TqBitNetworkInterfacesType, Data, 'Fifaces', Field, Result) then
  if not ProcessConverter<TqBitLogType>(TqBitLogsType, Data, 'Flogs', Field, Result) then
  if not ProcessConverter<TqBitTrackerType>(TqBitTrackersType, Data, 'Ftrackers', Field, Result) then
  if not ProcessConverter<TqBitRSSArticleType>(TqBitRSSItemType, Data, 'Farticles', Field, Result) then
  if not ProcessConverter<TqBitContentType>(TqBitContentsType, Data, 'Fcontents', Field, Result) then
  TqBitAPIUtils.RaiseException(Format(
      'Class: %s, %s - %s not implemented.',
      [Data.ClassName, Field, 'TqBitObjectListInterceptor.StringConverter']
    ));
  //Result := JsonRawPatcher.Encode('[' + Result + ']', Header, Footer );
  Result := JsonRawPatcher.Encode('[' + Result + ']' );
  SL.Free;
end;

{ TqBitVariantListInterceptor }

procedure TqBitVariantListInterceptor.StringReverter(Data: TObject; Field, Arg: string);
var
  ctx: TRttiContext;
  i64: Int64;
begin
  var VList :=  TqBitList<variant>.Create;
  ctx.GetType(Data.ClassInfo).GetField(Field).SetValue(Data, VList);
  var JSONArray := TJSONObject.ParseJSONValue(arg) as TJSONArray;
  for var a in JSONArray do // << always a string...
    if tryStrToInt64(a.Value, i64) then VList.Add(i64) else
    VList.Add( a.Value );
  JSONArray.Free;
end;

function TqBitVariantListInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRttiContext;
begin
  var SL := TqBitAPIUtils.DelimStringList(nil, ',', '');
  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  var VList := TqBitList<variant>( RTTIField.GetValue(Data).AsObject );
  for var value in VList do
    SL.Add(TJsonVarHelper.VarToJsonStr(value));
  Result := JsonRawPatcher.Encode( '"' +  Copy(Field, 2, MaxInt) + '":[' + SL.DelimitedText + ']', '"' +  Copy(Field, 2, MaxInt) + '":"', '"');
  SL.Free;
end;

{ TqBitObjectDictionaryInterceptor }

function TqBitObjectDictionaryInterceptor.ProcessReverter<T>(DataClass: TClass; Data: TObject; FieldName: string; Field, Arg: string): Boolean;
var
  ctx: TRttiContext;
begin
  Result := False;
  if (not (Data is DataClass)) or (FieldName <> Field) then Exit;

  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  if RTTIField = nil then Exit;

  var v :=  TqBitObjectDictionary<variant, T>.Create([doOwnsValues]);
  var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
  for var JSONPair in JSONObj do
  v.Add(
      JSONPair.JsonString.Value,
      TJsonBaseType(TJson.JsonToObject<T>(JSONPair.JsonValue.toString))
    );
  JSONObj.Free;
  RTTIField.SetValue(Data, v);

  if ctx.GetType(Data.ClassInfo).GetField('_Key') <> nil then
    for var Element in v do
      TJsonBaseType(Element.Value)._Key := Element.Key;
  Result := True;
end;

procedure TqBitObjectDictionaryInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  if not ProcessReverter<TqBitRSSRuleType>(TqBitAutoDownloadingRulesType, Data, 'Frules', Field, Arg) then
  if not ProcessReverter<TqBitCategoryType>(TqBitMainDataType, Data, 'Fcategories', Field, Arg) then
  if not ProcessReverter<TqBitTorrentType>(TqBitMainDataType, Data, 'Ftorrents', Field, Arg) then
  if not ProcessReverter<TqBitTorrentPeerDataType>(TqBitTorrentPeersDataType, Data, 'Fpeers', Field, Arg) then
  if not ProcessReverter<TqBitCategoryType>(TqBitCategoriesType, Data, 'Fcategories', Field, Arg) then
  if not ProcessReverter<TqBitRSSItemType>(TqBitRSSAllItemsType, Data, 'Fitems', Field, Arg) then
  TqBitAPIUtils.RaiseException(Format(
      'Class: %s, %s - %S not implemented.',
      [Data.ClassName, Field, 'TqBitObjectDictionaryInterceptor.StringReverter']
    ));
end;

function TqBitObjectDictionaryInterceptor.ProcessConverter<T>(DataClass: TClass; Data: TObject; FieldName: string; Field: string; var JSONStr: string): boolean;
var
  ctx: TRttiContext;
begin
  Result := False;
  if (not (Data is DataClass)) or (FieldName <> Field) then Exit;
  var SL := TqBitAPIUtils.DelimStringList(nil, ',', '');
  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  var ODic := TqBitObjectDictionary<variant, TObject>(RTTIField.GetValue(Data).AsObject);
  for var kv in ODic do
      SL.Add( TJsonVarHelper.VarToJsonStr(kv.Key) + ':' + TJson.ObjectToJsonString(kv.Value) );
  JSONStr := SL.DelimitedText;
  SL.Free;
  Result := True;
end;

function TqBitObjectDictionaryInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  var SL := TqBitAPIUtils.DelimStringList(nil, ',', '');
  if not ProcessConverter<TqBitCategoryType>(TqBitCategoriesType, Data, 'Fcategories', Field, Result) then
  if not ProcessConverter<TqBitCategoryType>(TqBitMainDataType, Data, 'Fcategories', Field, Result) then
  if not ProcessConverter<TqBitTorrentType>(TqBitMainDataType, Data, 'Ftorrents', Field, Result) then
  if not ProcessConverter<TqBitRSSItemType>(TqBitRSSAllItemsType, Data, 'Fitems', Field, Result) then
  TqBitAPIUtils.RaiseException(Format(
      'Class: %s, %s - %s not implemented.',
      [Data.ClassName, Field, 'TqBitObjectDictionaryInterceptor.StringConverter']
    ));
  Result:= JsonRawPatcher.Encode('{' + Result + '}');
  SL.Free;
end;

{ TqBitVariantDictionaryInterceptor }

procedure TqBitVariantDictionaryInterceptor.StringReverter(Data: TObject; Field: string; Arg: string);
var
  ctx: TRttiContext;
begin
  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  RTTIField.SetValue(Data, TqBitVariantDictionary<variant, variant>.Create);
  var VDic := TqBitVariantDictionary<variant, variant>( RTTIField.GetValue(Data).AsObject );
  if VDic = nil then Exit;
  var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
  for var JSONPair in JSONObj do
    VDic.Add(JSONPair.JsonString.Value, TJsonVarHelper.StrToVar(JSONPair.JsonValue.Value));
  JSONObj.Free;
end;

function TqBitVariantDictionaryInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  ctx: TRttiContext;
begin
  var RTTIField := ctx.GetType(Data.ClassInfo).GetField(Field);
  var VDic := TqBitVariantDictionary<variant, variant>( RTTIField.GetValue(Data).AsObject );
  if VDic = nil then Exit;
  var SL := TqBitAPIUtils.DelimStringList(nil, ',', '');
  for var v in VDic do
    SL.Add( TJsonVarHelper.VarToJsonStr(v.Key) + ':' + TJsonVarHelper.VarToJsonStr(v.Value) );
  Result := JsonRawPatcher.Encode( '{' + SL.DelimitedText + '}' );
  SL.Free;
end;

{$ENDREGION} // 'JSON Interceptor Impl.'

{$REGION 'Generic Types Impl.'}

{ qBitList }

function TqBitList<A>.Clone: TqBitList<A>;
begin
  Result := TqBitList<A>.Create;
  for var v  in Self do
    Result.Add(v);
end;

function TqBitList<A>.Merge(From: TqBitList<A>): variant;
begin
  var dum :=  TqBitList<Variant>.Create;
  Result := Merge(From, dum);
  dum.Free;
end;

function TqBitList<A>.Merge(From: TqBitList<A>; var Added: TqBitList<variant>): variant;
begin
  Result := False;
  if not assigned(From) then exit;
  for var v in From do
  begin
    Self.Add(v);
    if not assigned(Added) then Added := TqBitList<variant>.Create;
    Added.Add(v);
    Result := True;
  end;
end;

{ TqBitObjectDictionary }

function TqBitObjectDictionary<A, B>.Clone: TqBitObjectDictionary<A, B>;
begin
  Result := TqBitObjectDictionary<A, B>.Create([doOwnsValues]);
  for var v in Self do
    Result.Add(v.Key, v.Value.clone);
end;

function TqBitObjectDictionary<A, B>.Merge(From: TqBitObjectDictionary<A, B>; var  Added: TqBitList<variant>; var Modified: TqBitList<variant>): variant;
begin
  if not assigned(From) then exit;
  Result := False;
  for var v in From do
  begin
    if Self.ContainsKey(v.Key) then
    begin
      var w := Self.Items[v.key];
      w.Merge(v.Value);
      if not assigned(Modified) then Modified := TqBitList<variant>.Create;
      Modified.add(v.Key);
    end else begin
      Self.Add(v.Key, v.Value.Clone);
      if not assigned(Added) then Added := TqBitList<variant>.Create;
      Added.Add(v.Key);
      Result := True;
    end;
  end;
end;

{TqBitStringListDictionary}

function TqBitStringListDictionary<A, B>.Clone: TqBitStringListDictionary<A, B>;
begin
  Result := TqBitStringListDictionary<A, B>.Create([doOwnsValues]);
  for var v in Self do
  begin
    var sl := TStringList.create;
    sl.Assign(v.value);
    Result.Add(v.Key,sl);
  end;
end;

function TqBitStringListDictionary<A, B>.Merge(From: TqBitStringListDictionary<A, B>; var  Added: TqBitList<variant>; var Modified: TqBitList<variant>): variant;
begin
  Result := False;
  if not assigned(From) then exit;
  for var p in From do
  if Self.ContainsKey(p.key) then
  begin
    var v := Self.Items[p.key];
    v.Assign(p.Value);
    if not assigned(Modified) then Modified := TqBitList<variant>.Create;
    Modified.add(p.Key);
  end else begin
    var sl := TStringList.create;
    sl.Assign(p.Value);
    Self.Add(p.Key, sl);
    if not assigned(Added) then Added := TqBitList<variant>.Create;
    Added.Add(p.Key);
    Result := True;
  end;
end;

{ TqBitVariantDictionary<A, B> }

function TqBitVariantDictionary<A, B>.Clone: TqBitVariantDictionary<A, B>;
begin
  Result := TqBitVariantDictionary<A, B>.Create([doOwnsValues]);
  for var v in Self do
    Result.Add(v.Key, v.Value.clone);
end;

function TqBitVariantDictionary<A, B>.Merge(From: TqBitVariantDictionary<A, B>;
  var Added, Modified: TqBitList<variant>): variant;
begin
  Result := False;
  if not assigned(From) then exit;
  for var p in From do
  if Self.ContainsKey(p.key) then
  begin
    var v := Self.Items[p.key];
    v.Assign(p.Value);
    if not assigned(Modified) then Modified := TqBitList<variant>.Create;
    Modified.add(p.Key);
    Result := True;
  end else begin
    Self.Add(p.Key, p.Value);
    if not assigned(Added) then Added := TqBitList<variant>.Create;
    Added.Add(p.Key);
    Result := True;
  end;
end;

{ TqBitObjectList<A> }
function TqBitObjectList<A>.Merge(From: TqBitObjectList<A>; var Added: TqBitObjectList<A>): variant;
begin
  Result := False;
  if not assigned(From) then exit;
  for var v in From do
  begin
    Self.Add(v.Clone);
    if not assigned(Added) then Added := TqBitObjectList<A>.Create;
    Added.Add(v.Clone);
    Result := True;
  end;;
end;

function TqBitObjectList<A>.Clone: TqBitObjectList<A>;
begin
  Result := TqBitObjectList<A>.Create;
  for var v in Self do
    Result.Add(v.Clone);
end;

function TqBitObjectList<A>.Merge(From: TqBitObjectList<A>): variant;
begin
  var dummy := TqBitObjectList<A>.Create;
  Result := Self.Merge(From, dummy);
  dummy.Free;
end;

function TqBitObjectList<A>.Merge(From: TqBitObjectList<A>; var Added, Modified,
  Removed: TqBitObjectList<A>): variant;
begin
  Result := False;
  if not assigned(From) then exit;
  for var v in Self do // Removed
    if not From.IndexOf(v) = -1 then
    begin
      if not assigned(Removed) then Removed := TqBitObjectList<A>.Create;
      Removed.Add(v.Clone);
    end;
  for var v in From do
  begin
    Self.Add(v.Clone);
    if not assigned(Added) then Added := TqBitObjectList<A>.Create;
    Added.Add(v.Clone);
    Result := True;
  end;;
end;
{$ENDREGION}

{$REGION 'JSON Types Impl.'}

{ TJsonBaseType }

constructor TJsonBaseType.Create;
begin
  _UserRec.OwnObj := False;
  _UserRec.Obj := nil;
end;

destructor TJsonBaseType.Destroy;
begin
  if _UserRec.OwnObj then _UserRec.Obj.Free;
  inherited;
end;

procedure TJsonBaseType.ClonePropertiesTo(T : TJsonBaseType);
begin
  if T = nil then exit;
  var rttictx := TRttiContext.Create();
  var rttitype := rttictx.GetType(Self.ClassType);
  for var field in rttitype.GetFields do
    if field.FieldType.TypeKind = tkVariant then
    begin
      var v :=  field.GetValue(Self);
      if not VarIsEmpty(v.AsVariant) then field.SetValue(T, v);
    end;
  rttictx.Free;
end;

procedure TJsonBaseType.MergePropertiesFrom(T: TJsonBaseType);
begin
  if T = nil then exit;
  if Self.ClassType <> T.ClassType then exit;
  var rttictx := TRttiContext.Create();
  var rttitype := rttictx.GetType(T.ClassType);
  for var field in rttitype.GetFields do
    if field.FieldType.TypeKind = tkVariant then
    begin
      var v :=  field.GetValue(T);
      if not VarIsEmpty(v.AsVariant) then field.SetValue(Self, v);
    end;
  rttictx.Free;
end;

procedure TJsonBaseType.Merge(T: TJsonBaseType);
begin
  MergePropertiesFrom(T);
end;

function TJsonBaseType.Clone: TJsonBaseType;
begin
  Result := TJsonBaseType(Self.ClassType.Create);
  Self.ClonePropertiesTo(Result);
  Result._UserRec.Val := Self._UserRec.Val;
  Result._UserRec.OwnObj := False;
  Result._UserRec.Obj := Self._UserRec.Obj;
end;

function TJsonBaseType.toJSON: string;
begin
  Result := TJson.ObjectToJsonString(Self, [] );
  JsonRawPatcher.Decode(Result);
end;

procedure TJsonBaseType.Clear;
begin
  TqBitAPIUtils.RaiseException(Format(
      '%s not implemented.',
      ['TJsonBaseType.Clear']
    ));
end;

function TJsonBaseType.toParams: string;
begin
  TqBitAPIUtils.RaiseException(Format(
      '%s not implemented.',
      ['TJsonBaseType.toParams']
    ));
end;

{ TqBitPreferencesType }

function TqBitPreferencesType.Clone: TJsonBaseType;
begin
  var P := TqBitPreferencesType.Create;
  Self.ClonePropertiesTo(P);
  if assigned(Self.Fscan_dirs) then
  begin
    P.Fscan_dirs :=  TqBitVariantDictionary<variant, variant>.Create;
    for var v in Self.Fscan_dirs do
      P.Fscan_dirs.Add(v.Key, v.Value);
  end;
  Result := P;
end;

destructor TqBitPreferencesType.Destroy;
begin
  FreeAndNil(Self.Fscan_dirs);
  inherited;
end;

{ TqBitLogsType }

destructor TqBitLogsType.Destroy;
begin
  Self.Flogs.Free;
  inherited Destroy;
end;
procedure TqBitLogsType.Merge(From: TJsonBaseType);
begin
  var T := TqBitLogsType(From);
  if T.Flogs <> nil then
  begin
    if Self.Flogs = nil then Self.Flogs := TqBitObjectList<TqBitLogType>.Create(True);
    Self.Flogs.Merge(T.Flogs);
  end;
end;

function TqBitLogsType.Clone: TJsonBaseType;
begin
  var T := TqBitLogsType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Flogs <> nil then T.Flogs := Self.Flogs.Clone;
  Result := T;
end;

{ TqBitPeerLogsType }

procedure TqBitPeerLogsType.Merge(From: TJsonBaseType);
begin
  var T := TqBitPeerLogsType(From);
  if T.Flogs <> nil then
  begin
    if Self.Flogs = nil then Self.Flogs := TqBitObjectList<TqBitPeerLogType>.Create(True);
    Self.Flogs.Merge(T.Flogs);
  end;
end;

function TqBitPeerLogsType.Clone: TJsonBaseType;
begin
  var T := TqBitPeerLogsType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Flogs <> nil then T.Flogs := Self.Flogs.Clone;
  Result := T;
end;

destructor TqBitPeerLogsType.Destroy;
begin
  Self.Flogs.Free;
  inherited Destroy;
end;

{ TqBitMainDataType }

function TqBitMainDataType.Clone: TJsonBaseType;
begin
  var M := TqBitMainDataType.Create;
  /// Common Properties
  ///
  Self.ClonePropertiesTo(M);
  /// Fserver_state
  ///
  if Self.Fserver_state <> nil then
    M.Fserver_state := TqBitserver_stateType( Self.Fserver_state.Clone );
  //// Fcategories
  ///
  if Self.Fcategories <> nil then
    M.Fcategories := Self.Fcategories.Clone;
  if Self._Fcategories_added <> nil then
    M._Fcategories_added := Self._Fcategories_added.Clone;
  if Self._Fcategories_modified <> nil then
    M._Fcategories_modified := Self._Fcategories_modified.Clone;
  if Self.Fcategories_removed <> nil then
    M.Fcategories_removed := Self.Fcategories_removed.Clone;
  M._Fcategories_count_changed := Self._Fcategories_count_changed;
  M._Fcategories_changed := Self._Fcategories_changed;
  //// Ftags
  ///
  if Self.FTags <> nil then
    M.FTags := Self.FTags.Clone;
  if Self._Ftags_added <> nil then
    M._Ftags_added := Self._Ftags_added.Clone;
  if Self.Ftags_removed <> nil then
    M.Ftags_removed := Self.Ftags_removed.Clone;
  M._Ftags_count_changed := Self._Ftags_count_changed;
  M._Ftags_changed := Self._Ftags_changed;
  //// Ftorrents
  ///
  if Self.Ftorrents <> nil then
    M.Ftorrents := Self.Ftorrents.Clone;
  if Self._Ftorrents_added <> nil then
    M._Ftorrents_added := Self._Ftorrents_added.Clone;
  if Self._Ftorrents_modified <> nil then
    M._Ftorrents_modified := Self._Ftorrents_modified.Clone;
  if Self.Ftorrents_removed <> nil then
    M.Ftorrents_removed := Self.Ftorrents_removed.Clone;
  M._Ftorrents_count_changed := Self._Ftorrents_count_changed;
  M._Ftorrents_changed := Self._Ftorrents_changed;
  //// Ftrackers
  ///
  if Self.FTrackers <> nil then
    M.FTrackers := Self.FTrackers.Clone;
  if Self.Ftrackers_removed <> nil then
    M.Ftrackers_removed := Self.Ftrackers_removed.Clone;
  M._Ftrackers_count_changed := Self._Ftrackers_count_changed;
  M._Ftrackers_changed := Self._Ftrackers_changed;
  Result := M;
end;

destructor TqBitMainDataType.Destroy;
begin
  Self._Ftrackers_added.Free;
  Self._Ftrackers_modified.Free;
  Self.Ftrackers_Removed.Free;
  Self.Ftrackers.Free;
  Self._Ftorrents_added.Free;
  Self._Ftorrents_modified.Free;
  Self.Ftorrents_removed.Free;
  Self.Ftorrents.Free;
  Self._Fcategories_added.Free;
  Self._Fcategories_modified.Free;
  Self.Fcategories_removed.Free;
  Self.Fcategories.Free;
  Self._Ftags_added.Free;
  Self._Ftags_modified.Free;
  Self.Ftags_removed.Free;
  Self.FTags.Free;
  Self.Fserver_state.Free;
  inherited Destroy;
end;

procedure TqBitMainDataType.Merge(From: TJsonBaseType);
var
  M: TqBitMainDataType;
begin
  if From = Nil then Exit;
  inherited Merge(From);
  M := TqBitMainDataType(From);
  TqBitAPIUtils.MergerVariants(Self.Ffull_update, M.Ffull_update, False);
  if M.Fserver_state <> nil then
  begin
    if Self.Fserver_state = nil then Fserver_state := TqBitserver_stateType.Create;
    Self.Fserver_state.Merge(M.Fserver_state);
  end;
  //// Fcategories
  ///
  FreeAndNil(Self._Fcategories_added);
  FreeAndNil(Self._Fcategories_modified);
  if M.Fcategories <> nil then
  begin
    if Self.Fcategories = nil then Self.Fcategories := TqBitObjectDictionary<variant, TqBitCategoryType>.Create([doOwnsValues]);
    Self.Fcategories.Merge(M.Fcategories, Self._Fcategories_added, Self._Fcategories_modified);
  end;
  FreeAndNil(Self.Fcategories_removed);
  if M.Fcategories_removed <> Nil then
  begin
    if Self.Fcategories_removed = nil then Self.Fcategories_removed := TqBitList<variant>.Create;
    Self.Fcategories_removed.Merge(M.Fcategories_removed);
    for var v in M.Fcategories_removed do
      Self.Fcategories.Remove(v);
  end;
  _Fcategories_count_changed := assigned(Self._Fcategories_added) or assigned(Self.Fcategories_removed);
  _Fcategories_changed := _Fcategories_count_changed or assigned(Self._Fcategories_modified);
  //// Ftags
  ///
  FreeAndNil(Self._Ftags_added);
  FreeAndNil(Self._Ftags_modified);
  FreeAndNil(Self.Ftags_removed);
  if M.FTags <> Nil then
  begin
    if Self.FTags = nil then Self.FTags := TqBitList<variant>.Create;
    Self.Ftags.Merge(M.Ftags, Self._Ftags_added);
  end;
  if M.Ftags_removed <> nil then
  begin
    if Self.Ftags_removed = nil then Self.Ftags_removed := TqBitList<variant>.Create;
    Self.Ftags_removed.Merge(M.Ftags_removed);
    for var v in M.Ftags_removed do
      Self.Ftags.Remove(v);
  end;
  _Ftags_count_changed := assigned(Self._Ftags_added) or assigned(Self.Ftags_removed);
  _Ftags_changed := _Ftags_count_changed or assigned(Self._Ftags_modified);
  //// Ftorrents
  ///
  FreeAndNil(Self._Ftorrents_added);
  FreeAndNil(Self._Ftorrents_modified);
  if M.Ftorrents <> nil then
  begin
    if Self.Ftorrents = nil then Self.Ftorrents := TqBitObjectDictionary<variant, TqBittorrentType>.Create([doOwnsValues]);
    Self.Ftorrents.Merge(M.Ftorrents, Self._Ftorrents_added, Self._Ftorrents_modified);
  end;
  FreeAndNil(Self.Ftorrents_removed);
  if M.Ftorrents_removed <> Nil then
  begin
    if Self.Ftorrents_removed = nil then Self.Ftorrents_removed := TqBitList<variant>.Create;
    Self.Ftorrents_removed.Merge(M.Ftorrents_removed);
    for var v in M.Ftorrents_removed do
      Self.Ftorrents.Remove(v);
  end;
  _Ftorrents_count_changed := assigned(Self._Ftorrents_added) or assigned(Self.Ftorrents_removed);
  _Ftorrents_changed :=  _Ftorrents_count_changed or assigned(Self._Ftorrents_modified);
  //// Ftrackers
  ///
  FreeAndNil(Self._Ftrackers_added);
  FreeAndNil(Self._Ftrackers_modified);
  if M.FTrackers <> nil then
  begin
    if Self.FTrackers = nil then Self.Ftrackers := TqBitStringListDictionary<variant, TStringList>.Create([doOwnsValues]);
    Self.FTrackers.Merge(M.FTrackers, Self._FTrackers_added, Self._FTrackers_modified);
  end;
  if M.Ftrackers_removed <> Nil then
  begin
    if Self.Ftrackers_removed = nil then Self.Ftrackers_removed := TqBitList<variant>.Create;
    Self.Ftrackers_removed.Merge(M.Ftrackers_removed);
    for var v in M.Ftrackers_removed do
      Self.Ftrackers.Remove(v)
  end;
   _Ftrackers_count_changed := assigned(Self._Ftrackers_added) or assigned(Self.Ftrackers_removed);
  _Ftrackers_changed :=  _Ftrackers_count_changed or assigned(Self._Ftrackers_modified);
end;

{ TqBitTorrentPeersDataType }

destructor TqBitTorrentPeersDataType.Destroy;
begin
  Self._Fpeers_added.Free;
  Self._Fpeers_modified.Free;
  Self.Fpeers_removed.Free;
  Self.Fpeers.Free;
  inherited;
end;

{ TqBitTorrentListRequestType }

procedure TqBitTorrentListRequestType.Clear;
begin
  Ffilter := Unassigned;
  Fcategory := Unassigned;;
  Ftag := Unassigned;
  Fsort := Unassigned;
  Freverse := Unassigned;
  Flimit := Unassigned;
  Foffset := Unassigned;
  Fhashes.Clear;
end;

constructor TqBitTorrentListRequestType.Create;
begin
  inherited;
  FHashes := TStringList.Create;
  FHashes.StrictDelimiter := True;
  FHashes.QuoteChar := #0;
  FHashes.Delimiter:='|';
end;

destructor TqBitTorrentListRequestType.Destroy;
begin
  FHashes.Free;
  inherited;
end;

function TqBitTorrentListRequestType.ToParams: string;
begin
  Result := '';
  var SL := TqBitAPIUtils.DelimStringList(nil,'&');
  if not VarIsEmpty(Ffilter) then
    SL.Add( 'filter='+  TNetEncoding.URL.Encode(Ffilter) );
  if not VarIsEmpty(Fcategory) then
    SL.Add( 'category='+  TNetEncoding.URL.Encode(Fcategory) );
  if not VarIsEmpty(Ftag) then
    SL.Add( 'tag='+  TNetEncoding.URL.Encode(Ftag) );
  if not VarIsEmpty(Self.Fsort) then
    SL.Add( 'sort='+  TNetEncoding.URL.Encode(Fsort) );
  if not VarIsEmpty(Freverse) then
    SL.Add( 'reverse='+  TNetEncoding.URL.Encode(Freverse) );
  if not VarIsEmpty(Flimit) then
    SL.Add( 'limit='+  TNetEncoding.URL.Encode(Flimit) );
  if not VarIsEmpty(Foffset) then
    SL.Add( 'offset='+  TNetEncoding.URL.Encode(Foffset) );
  if Fhashes.Count > 0 then
    SL.Add( 'hashes='+  TNetEncoding.URL.Encode(Fhashes.DelimitedText) );
  Result := SL.DelimitedText;
  SL.Free;
end;

function TqBitTorrentsListType.Clone: TJsonBaseType;
begin
  var T := TqBitTorrentsListType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Ftorrents <> nil then T.Ftorrents := Self.Ftorrents.Clone;
  Result := T;
end;

destructor TqBitTorrentsListType.Destroy;
begin
  Self.Ftorrents.Free;
  inherited Destroy;
end;

procedure TqBitTorrentsListType.Merge(From: TJsonBaseType);
begin
  var T := TqBitTorrentsListType(From);
  if T.Ftorrents <> nil then
  begin
    if Self.Ftorrents = nil then Self.Ftorrents := TqBitObjectList<TqBitTorrentType>.Create(True);
    Self.Ftorrents.Merge(T.Ftorrents);
  end;
end;

{ TqBitTrackersType }

destructor TqBitTrackersType.Destroy;
begin
  Self.Ftrackers.Free;
  inherited Destroy;
end;

{ TqBitWebSeedsType }

function TqBitWebSeedsType.Clone: TJsonBaseType;
begin
  var T := TqBitWebSeedsType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Furls <> nil then T.Furls := Self.Furls.Clone;
  Result := T;
end;

destructor TqBitWebSeedsType.Destroy;
begin
  Self.Furls.Free;
  inherited Destroy;
end;

procedure TqBitWebSeedsType.Merge(From: TJsonBaseType);
begin
  var T := TqBitWebSeedsType(From);
  if T.Furls <> nil then
  begin
    if Self.Furls = nil then Self.Furls := TqBitObjectList<TqBitWebSeedType>.Create(True);
    Self.Furls.Merge(T.Furls);
  end;
end;

{ TqBitContentsType }

function TqBitContentsType.Clone: TJsonBaseType;
begin
  var T := TqBitContentsType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Fcontents <> nil then T.Fcontents := Self.Fcontents.Clone;
  Result := T;
end;

procedure TqBitContentsType.Merge(From: TJsonBaseType);
begin
  var T := TqBitContentsType(From);
  if T.Fcontents <> nil then
  begin
    if Self.Fcontents = nil then Self.Fcontents := TqBitObjectList<TqBitContentType>.Create(True);
    Self.Fcontents.Merge(T.Fcontents);
  end;
end;

destructor TqBitContentsType.Destroy;
begin
  Self.Fcontents.Free;
  inherited Destroy
end;

{ TqBitTorrentSpeedsLimit }

function TqBitTorrentSpeedsLimitType.Clone: TJsonBaseType;
begin
  var T := TqBitTorrentSpeedsLimitType.Create;
  Self.ClonePropertiesTo(T);
  // if Self.Fspeeds <> nil then T.Fspeeds := Self.Fspeeds.Clone;
  Result := T;
end;

destructor TqBitTorrentSpeedsLimitType.Destroy;
begin
  Fspeeds.Free;
  inherited Destroy;
end;

procedure TqBitTorrentSpeedsLimitType.Merge(From: TJsonBaseType);
begin
  var T := TqBitTorrentSpeedsLimitType(From);
  if T.Fspeeds <> nil then
  begin
    if Self.Fspeeds = nil then Self.Fspeeds := TqBitVariantDictionary<variant, variant>.Create([doOwnsValues]);
    var Added := TqBitList<variant>.Create;
    var Modified := TqBitList<variant>.Create;
    Self.Fspeeds.Merge(T.Fspeeds, Added, Modified);
    Modified.Free;
    Added.Free;
  end;
end;

{ TqBitCategoriesType }

function TqBitCategoriesType.Clone: TJsonBaseType;
begin
  var T := TqBitCategoriesType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Fcategories <> nil then T.Fcategories := Self.Fcategories.Clone;
  Result := T;
end;

destructor TqBitCategoriesType.Destroy;
begin
  Fcategories.Free;
  inherited;
end;

procedure TqBitCategoriesType.Merge(From: TJsonBaseType);
begin
  inherited;
  var T := TqBitCategoriesType(From);
  if T.Fcategories <> nil then
  begin
    if Self.Fcategories = nil then Self.Fcategories := TqBitObjectDictionary<variant, TqBitCategoryType>.Create([doOwnsValues]);
    var Added := TqBitList<variant>.Create;
    var Modified := TqBitList<variant>.Create;
    Self.Fcategories.Merge(T.Fcategories, Added, Modified);
    Modified.Free;
    Added.Free;
  end;
end;

{ TqBitAddTorrentType }

constructor TqBitNewTorrentUrlsType.Create;
begin
  inherited;
  Furls := TStringList.Create;
  FupLimit := -1;
  FdlLimit := -1;
  FcontentLayout := 'Original';
end;

destructor TqBitNewTorrentUrlsType.Destroy;
begin
  Furls.Free;
  inherited;
end;

{ TqBitNewTorrentFileType }

constructor TqBitNewTorrentFileType.Create;
begin
  inherited;
  FupLimit := -1;
  FdlLimit := -1;
  FcontentLayout := 'Original';
end;

{ TqBitContentType }

function TqBitContentType.Clone: TJsonBaseType;
begin
  var T := TqBitContentType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Fpiece_range <> nil then T.Fpiece_range := Self.Fpiece_range.Clone;
  Result := T;
end;

destructor TqBitContentType.Destroy;
begin
  Self.Fpiece_range.Free;
  inherited;
end;

procedure TqBitContentType.Merge(From: TJsonBaseType);
begin
  var T := TqBitContentType(From);
  if T.Fpiece_range <> nil then
  begin
    if Self.Fpiece_range = nil then Self.Fpiece_range := TqBitList<variant>.Create;
    Self.Fpiece_range.Merge(T.Fpiece_range);
  end;
end;

{ TqBitPiecesStatesType }

function TqBitPiecesStatesType.Clone: TJsonBaseType;
begin
  var T := TqBitPiecesStatesType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Fstates <> nil then T.Fstates := Self.Fstates.Clone;
  Result := T;
end;

destructor TqBitPiecesStatesType.Destroy;
begin
  Self.Fstates.Free;
  inherited;
end;

procedure TqBitPiecesStatesType.Merge(From: TJsonBaseType);
begin
  var T := TqBitPiecesStatesType(From);
  if T.Fstates <> nil then
  begin
    if Self.Fstates = nil then Self.Fstates :=TqBitList<variant>.Create;
    Self.Fstates.Merge(T.Fstates);
  end;
end;

{ TqBitTagsType }

function TqBitTagsType.Clone: TJsonBaseType;
begin
  var T := TqBitTagsType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Ftags <> nil then T.Ftags := Self.Ftags.Clone;
  Result := T;
end;

destructor TqBitTagsType.Destroy;
begin
  Self.Ftags.Free;
  inherited;
end;

procedure TqBitTagsType.Merge(From: TJsonBaseType);
begin
  var T := TqBitTagsType(From);
  if T.Ftags <> nil then
  begin
    if Self.Ftags = nil then Self.Ftags :=TqBitList<variant>.Create;
    Self.Ftags.Merge(T.Ftags);
  end;
end;

{ TqBitTorrentPeersDataType }

procedure TqBitTorrentPeersDataType.Merge(From: TJsonBaseType);
var
  P: TqBitTorrentPeersDataType;
begin
  if From = Nil then Exit;
  inherited Merge(From);
  P := TqBitTorrentPeersDataType(From);
  TqBitAPIUtils.MergerVariants(Self.Ffull_update, P.Ffull_update, False);
  //// Fpeers
  FreeAndNil(Self._Fpeers_added);
  FreeAndNil(Self._Fpeers_modified);
  if P.Fpeers <> nil then
  begin
    if Self.Fpeers = nil then Self.Fpeers := TqBitObjectDictionary<variant, TqBitTorrentPeerDataType>.Create([doOwnsValues]);
    Self.Fpeers.Merge(P.Fpeers, Self._Fpeers_added, Self._Fpeers_modified);
  end;
  FreeAndNil(Self.Fpeers_removed);
  if P.Fpeers_removed <> Nil then
  begin
    if Self.Fpeers_removed = nil then Self.Fpeers_removed := TqBitList<variant>.Create;
    Self.Fpeers_removed.Merge(P.Fpeers_removed);
    for var v in P.Fpeers_removed do
      Self.Fpeers.Remove(v);
  end;
  _Fpeers_count_changed := assigned(Self._Fpeers_added) or assigned(Self.Fpeers_removed);
  _Fpeers_changed :=  _Fpeers_count_changed or assigned(Self._Fpeers_modified);
end;

function TqBitTorrentPeersDataType.Clone: TJsonBaseType;
begin
  var P := TqBitTorrentPeersDataType.Create;
  Self.ClonePropertiesTo(P);
  if Self.Fpeers <> nil then
    P.Fpeers := Self.Fpeers.Clone;
  if Self._Fpeers_added <> nil then
    P._Fpeers_added := Self._Fpeers_added.Clone;
  if Self._Fpeers_modified <> nil then
    P._Fpeers_modified := Self._Fpeers_modified.Clone;
  if Self.Fpeers_removed <> nil then
    P.Fpeers_removed := Self.Fpeers_removed.Clone;
  P._Fpeers_count_changed := Self._Fpeers_count_changed;
  P._Fpeers_changed := Self._Fpeers_changed;
  Result := P;
end;

{ TqBitRSSAllItemsType }

function TqBitRSSAllItemsType.Clone: TJsonBaseType;
begin
  var T := TqBitRSSAllItemsType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Fitems <> nil then T.Fitems := Self.Fitems.Clone;
  Result := T;
end;

destructor TqBitRSSAllItemsType.Destroy;
begin
  Fitems.Free;
  inherited;
end;

procedure TqBitRSSAllItemsType.Merge(From: TJsonBaseType);
begin
  var T := TqBitRSSAllItemsType(From);
  if T.Fitems <> nil then
  begin
    if Self.Fitems = nil then Self.Fitems := TqBitObjectDictionary<variant, TqBitRSSItemType>.Create([doOwnsValues]);
    var Added := TqBitList<variant>.Create;
    var Modified := TqBitList<variant>.Create;
    Self.Fitems.Merge(T.Fitems, Added, Modified);
    Modified.Free;
    Added.Free;
  end;
end;

{ TqBitRSSItemType }

function TqBitRSSItemType.Clone: TJsonBaseType;
begin
  var T := TqBitRSSItemType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Farticles <> nil then T.Farticles := Self.Farticles.Clone;
  Result := T;
end;

destructor TqBitRSSItemType.Destroy;
begin
  Farticles.Free;
  inherited;
end;

procedure TqBitRSSItemType.Merge(From: TJsonBaseType);
begin
  var T := TqBitRSSItemType(From);
  if T.Farticles <> nil then
  begin
    if Self.Farticles = nil then Self.Farticles := TqBitObjectList<TqBitRSSArticleType>.Create(True);
    Self.Farticles.Merge(T.Farticles);
  end;
end;

{ TqBitRSSAllRulesType }

function TqBitRSSAllRulesType.Clone: TJsonBaseType;
begin
var T := TqBitRSSAllRulesType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Frules <> nil then T.Frules := Self.Frules.Clone;
  Result := T;
end;

destructor TqBitRSSAllRulesType.Destroy;
begin
  Frules.Free;
  inherited;
end;

procedure TqBitRSSAllRulesType.Merge(From: TJsonBaseType);
begin
  var T := TqBitRSSAllRulesType(From);
  if T.Frules <> nil then
  begin
    if Self.Frules = nil then Self.Frules := TqBitObjectDictionary<variant, TqBitRSSRuleType>.Create([doOwnsValues]);
    var Added := TqBitList<variant>.Create;
    var Modified := TqBitList<variant>.Create;
    Self.Frules.Merge(T.Frules, Added, Modified);
    Modified.Free;
    Added.Free;
  end;
end;

{ TqBitRSSArticles }

function TqBitRSSArticlesType.Clone: TJsonBaseType;
begin
  var T := TqBitRSSArticlesType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Farticles <> nil then T.Farticles := Self.Farticles.Clone;
  Result := T;
end;

destructor TqBitRSSArticlesType.Destroy;
begin
  Farticles.Free;
  inherited;
end;

procedure TqBitRSSArticlesType.Merge(From: TJsonBaseType);
begin
  var T := TqBitRSSArticlesType(From);
  if T.Farticles <> nil then
  begin
    if Self.Farticles = nil then Self.Farticles := TqBitStringListDictionary<variant, TStringList>.Create([doOwnsValues]);
    var Added := TqBitList<variant>.Create;
    var Modified := TqBitList<variant>.Create;
    Self.Farticles.Merge(T.Farticles, Added, Modified);
    Modified.Free;
    Added.Free;
  end;
end;

{ TqBitRSSRuleType }

function TqBitRSSRuleType.Clone: TJsonBaseType;
begin
  var T := TqBitRSSRuleType.Create;
  Self.ClonePropertiesTo(T);
  if Self.FaffectedFeeds <> nil then T.FaffectedFeeds := Self.FaffectedFeeds.Clone;
  Result := T;
end;

destructor TqBitRSSRuleType.Destroy;
begin
  FaffectedFeeds.Free;
  inherited;
end;

procedure TqBitRSSRuleType.Merge(From: TJsonBaseType);
begin
  var T := TqBitRSSRuleType(From);
  if T.FaffectedFeeds <> nil then
  begin
    if Self.FaffectedFeeds = nil then Self.FaffectedFeeds := TqBitList<variant>.Create;
    Self.FaffectedFeeds.Merge(T.FaffectedFeeds);
  end;
end;

{ TqBitAutoDownloadingRulesType }

function TqBitAutoDownloadingRulesType.Clone: TJsonBaseType;
begin
  var T := TqBitAutoDownloadingRulesType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Frules <> nil then T.Frules := Self.Frules.Clone;
  Result := T;
end;

destructor TqBitAutoDownloadingRulesType.Destroy;
begin
  Frules.Free;
  inherited;
end;

procedure TqBitAutoDownloadingRulesType.Merge(From: TJsonBaseType);
begin
  var T := TqBitAutoDownloadingRulesType(From);
  if T.Frules <> nil then
  begin
    if Self.Frules = nil then Self.Frules := TqBitObjectDictionary<variant, TqBitRSSRuleType>.Create([doOwnsValues]);
    var Added := TqBitList<variant>.Create;
    var Modified := TqBitList<variant>.Create;
    Self.Frules.Merge(T.Frules, Added, Modified);
    Modified.Free;
    Added.Free;
  end;
end;

{ TqBitTrackersType }

procedure TqBitTrackersType.Merge(From: TJsonBaseType);
begin
  var T := TqBitTrackersType(From);
  if T.FTrackers <> nil then
  begin
    if Self.FTrackers = nil then Self.Ftrackers := TqBitObjectList<TqBitTrackerType>.Create(True);
    Self.FTrackers.Merge(T.FTrackers);
  end;
end;

function TqBitTrackersType.Clone: TJsonBaseType;
begin
  var T := TqBitTrackersType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Ftrackers <> nil then T.Ftrackers := Self.Ftrackers.Clone;
  Result := T;
end;

{ TqBitNetworkInterfaceAddresses }

destructor TqBitNetworkInterfaceAddressesType.Destroy;
begin
  FreeAndNil(Self.Fadresses);
  inherited;
end;

{ TqBitNetworkInterfaces }

destructor TqBitNetworkInterfacesType.Destroy;
begin
  FreeAndNil(Self.Fifaces);
  inherited;
end;

{$ENDREGION} // 'JSON Types Intf.'

initialization
  JsonRawPatcher := TJsonRawPatcher.Create;
finalization
  JsonRawPatcher.Free;
end.
