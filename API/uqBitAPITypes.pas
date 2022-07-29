unit uqBitAPITypes;

///  Author: Laurent Meyer
///  Contact: qBit4Delphi@ea4d.com
///  API v2.8.3.dev.025 + Hidden/Missing Fields
///
///  https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)
///
///  ToDo : Search (if requested)

interface
uses System.Generics.Collections, REST.JsonReflect, system.JSON, REST.Json.Types,
     System.Generics.Defaults, Classes;

const
  qBitAPI_Version = '2.8.3';
  qBitAPI_Developer = 'Laurent Meyer, qBit4Delphi@ea4d.com';

type

  // Parent Type :
  TqBitTorrentBaseType = class
  private
    [JsonMarshalled(false)]
    _RawJsonData: TDictionary<string, string>;
    function RawJsonDecode(RawJson: string): string;
    function RawJsonEncode(Header, Value, Footer: string): string;
  protected
    procedure ClonePropertiesTo(T : TqBitTorrentBaseType); virtual;
    procedure MergePropertiesFrom(T: TqBitTorrentBaseType);
  public
    function Clone: TqBitTorrentBaseType; virtual;
    constructor Create; overload;
    destructor Destroy; override;
    procedure Merge(T: TqBitTorrentBaseType); virtual;
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

  {$ENDREGION}

  {$REGION 'JSON Interceptor Types Intf.'}

  TqBitObjectListInterceptor = class(TJSONInterceptor)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    function StringConverter(Data: TObject; Field: string): string; override;
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
  end;

  TqBitStringDictionaryInterceptor = class(TJSONInterceptor)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    function StringConverter(Data: TObject; Field: string): string; override;
  end;

  TqBitRSSObjectDictionaryInterceptor = class(TJSONInterceptor)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
    function StringConverter(Data: TObject; Field: string): string; override;
  end;

  {$ENDREGION}

  {$REGION 'Generic Types Intf.'}

  TqBitList<A> = class(TList<variant>)
    function Clone: TqBitList<A>; overload;
    function Merge(From: TqBitList<A>): variant; overload;
    function Merge(From: TqBitList<A>; var  Added: TqBitList<variant>): variant; overload;
  end;


  TqBitObjectDictionary<A, B>= class(TObjectDictionary<variant, TqBitTorrentBaseType>)
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

  TqBitObjectList<A> = class(TObjectList<TqBitTorrentBaseType>)
    function Clone: TqBitObjectList<A>;
    function Merge(From: TqBitObjectList<A>): variant; overload;
    function Merge(From: TqBitObjectList<A>; var Added: TqBitObjectList<A>): variant; overload;
    function Merge(From: TqBitObjectList<A>; var Added, Modified, Removed: TqBitObjectList<A>): variant; overload;
  end;

  {$ENDREGION}

  {$REGION 'JSON Types Intf.'}

  TqBitBuildInfoType = class(TqBitTorrentBaseType)
    Fbitness : variant;
    Fboost : variant;
    Flibtorrent : variant;
    Fopenssl : variant;
    Fqt : variant;
    Fzlib: variant;
  end;

  TqBitPreferencesType = class(TqBitTorrentBaseType)
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
    [JsonReflect(ctstring, rtString, TqBitStringDictionaryInterceptor)]
    Fscan_dirs: TqBitVariantDictionary<variant, variant>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
  end;

  TqBitLogType = class(TqBitTorrentBaseType)
    Fid: variant;
    Fmessage: variant;
    Ftimestamp: variant;
    Ftype: variant;
    // inherited Merge/Clone
  end;

  TqBitLogsType = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Flogs: TqBitObjectList<TqBitLogType>;
    function Clone: TqBitTorrentBaseType; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
    destructor Destroy; override;
  end;

  TqBitPeerLogType = class(TqBitTorrentBaseType)
    Fid: variant;
    Fip: variant;
    Ftimestamp: variant;
    Fblocked: variant;
    Freason: variant;
    // inherited Merge/Clone
  end;

  TqBitPeerLogsType = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Flogs: TqBitObjectList<TqBitPeerLogType>;
    function Clone: TqBitTorrentBaseType; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
    destructor Destroy; override;
  end;

  TqBitTorrentType = class(TqBitTorrentBaseType)
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
    Fhash: variant;
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

  TqBitCategoryType = class(TqBitTorrentBaseType)
    Fname: variant;
    FsavePath: variant;
     // inherited Merge/Clone
  end;

  TqBitserver_stateType = class(TqBitTorrentBaseType)
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
    Fread_cache_hits: variant;
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

  TqBitMainDataType = class(TqBitTorrentBaseType)
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

    [JsonReflect(ctString, rtString, TqBitObjectDictionaryInterceptor)]
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
    procedure Merge(From: TqBitTorrentBaseType); override;
    function Clone: TqBitTorrentBaseType; override;
  end;

  TqBitTorrentPeerDataType = class(TqBitTorrentBaseType)
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

  TqBitTorrentPeersDataType = class(TqBitTorrentBaseType)
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
    procedure Merge(From: TqBitTorrentBaseType); override;
    function Clone: TqBitTorrentBaseType; override;
  end;

  TqBitGlobalTransferInfoType = class(TqBitTorrentBaseType)
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

  TqBitTorrentListRequestType = class(TqBitTorrentBaseType)
    Ffilter: variant;
    Fcategory: variant;
    Ftag: variant;
    Fsort: variant;
    Freverse: variant;
    Flimit: variant;
    Foffset: variant;
    Fhashes : variant;
    FhashesList: TStringList;
    procedure Clear; override;
    function ToParams: string; override;
     // inherited Merge/Clone
  end;

  TqBitTorrentListType  = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Ftorrents: TqBitObjectList<TqBitTorrentType>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitTorrentInfoType = class(TqBitTorrentBaseType)
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

  TqBitTrackerType  = class(TqBitTorrentBaseType)
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

  TqBitTrackersType  = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Ftrackers: TqBitObjectList<TqBitTrackerType>;
    procedure Merge(From: TqBitTorrentBaseType); override;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
  end;

  TqBitWebSeedType  = class(TqBitTorrentBaseType)
    Furl: variant;
     // inherited Merge/Clone
  end;

  TqBitWebSeedsType  = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Furls: TqBitObjectList<TqBitWebSeedType>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitContentType = class(TqBitTorrentBaseType)
    Findex: variant;
    Fname: variant;
    Fsize: variant;
    Fprogress: variant;
    Fpriority: variant;
    Fis_seed: variant;
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Fpiece_range: TqBitList<variant>;
    Favailability: variant;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitContentsType = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Fcontents: TqBitObjectList<TqBitContentType>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitPiecesStatesType = class(TqBitTorrentBaseType)
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Fstates: TqBitList<variant>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitTorrentSpeedsLimitType = class(TqBitTorrentBaseType)
    [JsonReflect(ctString, rtString, TqBitObjectDictionaryInterceptor)]
    Fspeeds: TqBitVariantDictionary<variant, variant>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitCategoriesType  = class(TqBitTorrentBaseType)
    [JsonReflect(ctString, rtString, TqBitObjectDictionaryInterceptor)]
    Fcategories: TqBitObjectDictionary<variant, TqBitCategoryType>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitTagsType = class(TqBitTorrentBaseType)
    [JsonReflect(ctString, rtString, TqBitVariantListInterceptor)]
    Ftags: TqBitList<variant>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitRSSArticleType = class(TqBitTorrentBaseType)
    Fcategory: variant;
    Fdata: variant;
    Fdescription: variant;
    Fid: variant;
    Flink: variant;
    Ftitle: variant;
    ftorrentURL: variant;
    // inherited Merge/Clone
  end;

  TqBitRSSItemType = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectListInterceptor)]
    Farticles: TqBitObjectList<TqBitRSSArticleType>;
    FisLoading: variant;
    lastBuildDate: variant;
    Ftitle: variant;
    Fuid: variant;
    Furl: variant;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitRSSAllItemsType = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitRSSObjectDictionaryInterceptor)]
    Fitems: TqBitObjectDictionary<variant, TqBitRSSItemType>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitRSSRuleType  = class(TqBitTorrentBaseType)
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
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitRSSAllRulesType = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectDictionaryInterceptor)]
    Frules: TqBitObjectDictionary<variant, TqBitRSSRuleType>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitAutoDownloadingRulesType = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectDictionaryInterceptor)]
    Frules: TqBitObjectDictionary<variant, TqBitRSSRuleType>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  TqBitRSSArticles = class(TqBitTorrentBaseType)
    [JsonReflect(ctstring, rtString, TqBitObjectDictionaryInterceptor)]
    Farticles: TqBitStringListDictionary<variant, TStringList>;
    function Clone: TqBitTorrentBaseType; override;
    destructor Destroy; override;
    procedure Merge(From: TqBitTorrentBaseType); override;
  end;

  {$ENDREGION}

implementation
uses SysUtils, REST.Json, NetEncoding, Variants, RTTI;

{$REGION 'Helpers Impl.'}

const
  bstr: array[boolean] of string = ('false','true');

procedure VarMrg(var src: variant; dst, def: variant);
begin
  if not VarIsEmpty(dst) then src := dst else
    if not VarIsNull(def) then src := def;
end;

{$ENDREGION}

{$REGION 'JSON Interceptor Impl.'}

{ TqBitObjectListInterceptor }

procedure TqBitObjectListInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  if (Data is TqBitRSSItemType) and (Field = 'Farticles') then
  begin
    TqBitRSSItemType(Data).Farticles := TqBitObjectList<TqBitRSSArticleType>.Create(True);
    var JSONArr := TJSONObject.ParseJSONValue(Arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitRSSItemType(Data).Farticles.Add(
        TJSON.JsonToObject<TqBitRSSArticleType>( JSONArr.Items[i] as TJSONObject )
      );
    JSONArr.Free;
  end else
  if (Data is TqBitLogsType) and (Field = 'Flogs') then
  begin
    TqBitLogsType(Data).Flogs := TqBitObjectList<TqBitLogType>.Create(True);
    var JSONArr := TJSONObject.ParseJSONValue(Arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitLogsType(Data).Flogs.Add(
        TJSON.JsonToObject<TqBitLogType>( JSONArr.Items[i] as TJSONObject )
      );
    JSONArr.Free;
  end else
  if (Data is TqBitPeerLogsType) and (Field = 'Flogs') then
  begin
    TqBitPeerLogsType(Data).Flogs := TqBitObjectList<TqBitPeerLogType>.Create(True);
    var JSONArr := TJSONObject.ParseJSONValue(Arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitPeerLogsType(Data).Flogs.Add(
        TJSON.JsonToObject<TqBitPeerLogType>( JSONArr.Items[i] as TJSONObject )
      );
    JSONArr.Free;
  end else
  if (Data is TqBitTorrentListType) and (Field = 'Ftorrents') then
  begin
    TqBitPeerLogsType(Data).Flogs := TqBitObjectList<TqBitPeerLogType>.Create(True);
    var JSONArr := TJSONObject.ParseJSONValue(Arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitTorrentListType(Data).Ftorrents.Add(
        TJSON.JsonToObject<TqBitTorrentType>( JSONArr.Items[i] as TJSONObject )
      );
    JSONArr.Free;
  end else
  if (Data is TqBitTrackersType) and (Field = 'Ftrackers') then
  begin
    TqBitTrackersType(Data).Ftrackers := TqBitObjectList<TqBitTrackerType>.Create(True);
    var JSONArr := TJSONObject.ParseJSONValue(Arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitTrackersType(Data).Ftrackers.Add(
        TJSON.JsonToObject<TqBitTrackerType>( JSONArr.Items[i] as TJSONObject )
      );
    JSONArr.Free;
  end else
  if (Data is TqBitWebSeedsType) and (Field = 'Furls') then
  begin
    TqBitWebSeedsType(Data).Furls := TqBitObjectList<TqBitWebSeedType>.Create(True);
    var JSONArr := TJSONObject.ParseJSONValue(Arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitWebSeedsType(Data).Furls.Add(
        TJSON.JsonToObject<TqBitWebSeedType>( JSONArr.Items[i] as TJSONObject )
      );
    JSONArr.Free;
  end else
  if (Data is TqBitContentsType) and (Field = 'Fcontents') then
  begin
    TqBitContentsType(Data).Fcontents := TqBitObjectList<TqBitContentType>.Create(True);
    var JSONArr := TJSONObject.ParseJSONValue(Arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitContentsType(Data).Fcontents.Add(
        TJSON.JsonToObject<TqBitContentType>( JSONArr.Items[i] as TJSONObject )
      );
    JSONArr.Free;
  end else
  raise
    Exception.Create(Format(
        'Class: %s, %s not implemented.',
        [Data.ClassName, 'TqBitObjectListInterceptor.StringConverter']
      ));
end;

function TqBitObjectListInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := '';
  var SB := TStringBuilder.Create;
  SB.Append('[');
  if (Data is TqBitLogsType) and (Field = 'Flogs') then
    if TqBitLogsType(Data).Flogs <> nil then
      for var i := 0 to TqBitLogsType(Data).FLogs.Count - 1 do
      begin
        if SB.Length > 1 then SB.Append(',');
        SB.Append(TJson.ObjectToJsonString( TqBitLogsType(Data).Flogs[i] ));
      end;
  SB.Append(']');
  Result := TqBitTorrentBaseType(Data).RawJsonEncode('"logs":"',SB.ToString, '"');
  SB.Free;
end;

{ TqBitVariantListInterceptor }

procedure TqBitVariantListInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  if (Data is TqBitRSSRuleType) and (Field = 'FaffectedFeeds') then
  begin
    TqBitRSSRuleType(Data).FaffectedFeeds := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitRSSRuleType(Data).FaffectedFeeds.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitMainDataType) and (Field = 'Ftags') then
  begin
    TqBitMainDataType(Data).Ftags := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitMainDataType(Data).Ftags.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitMainDataType) and (Field = 'Ftags_removed') then
  begin
    TqBitMainDataType(Data).Ftags_removed := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitMainDataType(Data).Ftags_removed.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitMainDataType) and (Field = 'Fcategories_removed') then
  begin
    TqBitMainDataType(Data).Fcategories_removed := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitMainDataType(Data).Fcategories_removed.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitMainDataType) and (Field = 'Ftorrents_removed') then
  begin
    TqBitMainDataType(Data).Ftorrents_removed := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitMainDataType(Data).Ftorrents_removed.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitTorrentPeersDataType) and (Field = 'Fpeers_removed') then
  begin
    TqBitTorrentPeersDataType(Data).Fpeers_removed := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitTorrentPeersDataType(Data).Fpeers_removed.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitMainDataType) and (Field = 'Ftrackers_removed') then
  begin
    TqBitMainDataType(Data).Ftrackers_removed := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitMainDataType(Data).Ftrackers_removed.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitContentType) and (Field = 'Fpiece_range') then
  begin
    TqBitContentType(Data).Fpiece_range := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitContentType(Data).Fpiece_range.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitPiecesStatesType) and (Field = 'Fstates') then
  begin
    TqBitPiecesStatesType(Data).Fstates := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitPiecesStatesType(Data).Fstates.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  if (Data is TqBitTagsType) and (Field = 'Ftags') then
  begin
    TqBitTagsType(Data).Ftags := TqBitList<variant>.Create;
    var JSONArr := TJSONObject.ParseJSONValue(arg) as TJSONArray;
    for var i:= 0 to JSONArr.Count -1 do
      TqBitTagsType(Data).Ftags.Add(  JSONArr.Items[i].Value );
    JSONArr.Free;
  end else
  raise
    Exception.Create(Format(
        'Class: %s, %s not implemented.',
        [Data.ClassName, 'TqBitVariantListInterceptor.StringReverter']
      ));
end;

function TqBitVariantListInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  Result := '';
  if (Data is TqBitRSSRuleType) and (Field = 'FaffectedFeeds') then
  begin
    var SB := TStringBuilder.Create('[');
    for var value in TqBitRSSRuleType(Data).FaffectedFeeds do
    begin
      if SB.Length > 1 then SB.Append(',');
      if VarType(value) = varUString then
        SB.Append('"').Append(string(value)).Append('"')
      else
        SB.Append(string(value));
    end;
    SB.Append(']');
    Result := TqBitTorrentBaseType(Data).RawJsonEncode('"',SB.ToString, '"');
    SB.Free;
  end else
  raise
    Exception.Create(Format(
        'Class: %s, %s not implemented.',
        [Data.ClassName, 'TqBitVariantListInterceptor.StringConverter']
      ));
end;

{ TqBitObjectDictionaryInterceptor }

procedure TqBitObjectDictionaryInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  if (Data is TqBitRSSArticles) and (Field = 'Farticles') then
  begin
    TqBitRSSArticles(Data).Farticles := TqBitStringListDictionary<variant, TStringList>.Create([doOwnsValues]);
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
    begin
      var JSONArr := JSONPair.JsonValue as TJSONArray;
      var t := TStringList.Create;
      for var p in  JSONArr do T.Add(p.Value);
      TqBitRSSArticles(Data).Farticles.Add(JSONPair.JsonString.Value, t);
    end;
    JSONObj.Free;
  end else
  if (Data is TqBitAutoDownloadingRulesType) and (Field = 'Frules') then
  begin
    TqBitAutoDownloadingRulesType(Data).Frules := TqBitObjectDictionary<variant, TqBitRSSRuleType>.Create([doOwnsValues]);
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
      TqBitAutoDownloadingRulesType(Data).Frules.Add(
          JSONPair.JsonString.Value,
          TJson.JsonToObject<TqBitRSSRuleType>(JSONPair.JsonValue.toString)
		    );
    JSONObj.Free;
  end else
  if (Data is TqBitMainDataType) and (Field = 'Fcategories') then
  begin
    TqBitMainDataType(Data).Fcategories := TqBitObjectDictionary<variant, TqBitCategoryType>.Create([doOwnsValues]);
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
      TqBitMainDataType(Data).Fcategories.Add(
          JSONPair.JsonString.Value,
          TJson.JsonToObject<TqBitCategoryType>(JSONPair.JsonValue.toString)
		    );
    JSONObj.Free;
  end else
  if (Data is TqBitMainDataType) and (Field = 'Ftorrents') then
  begin
    TqBitMainDataType(Data).Ftorrents := TqBitObjectDictionary<variant, TqBitTorrentType>.Create([doOwnsValues]);
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
      TqBitMainDataType(Data).Ftorrents.Add(
          JSONPair.JsonString.Value,
          TJson.JsonToObject<TqBitTorrentType>(JSONPair.JsonValue.toString)
		    );
    JSONObj.Free;
  end else
  if (Data is TqBitTorrentPeersDataType) and (Field = 'Fpeers') then
  begin
    TqBitTorrentPeersDataType(Data).Fpeers := TqBitObjectDictionary<variant, TqBitTorrentPeerDataType>.Create([doOwnsValues]);
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
      TqBitTorrentPeersDataType(Data).Fpeers.Add(
          JSONPair.JsonString.Value,
          TJson.JsonToObject<TqBitTorrentPeerDataType>(JSONPair.JsonValue.toString)
		    );
    JSONObj.Free;
  end else
  if (Data is TqBitMainDataType) and (Field = 'Ftrackers') then
  begin
    TqBitMainDataType(Data).Ftrackers := TqBitStringListDictionary<variant, TStringList>.Create([doOwnsValues]);
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
    begin
      var JSONArr := JSONPair.JsonValue as TJSONArray;
      var t := TStringList.Create;
      for var p in  JSONArr do T.Add(p.Value);
      TqBitMainDataType(Data).Ftrackers.Add(JSONPair.JsonString.Value, t);
    end;
    JSONObj.Free;
  end else
  if (Data is TqBitCategoriesType) and (Field = 'Fcategories') then
  begin
      TqBitCategoriesType(Data).Fcategories := TqBitObjectDictionary<variant, TqBitCategoryType>.Create([doOwnsValues]);
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
      TqBitCategoriesType(Data).Fcategories.Add(
          JSONPair.JsonString.Value,
          TJson.JsonToObject<TqBitCategoryType>(JSONPair.JsonValue.toString)
		    );
    JSONObj.Free
  end else
  if (Data is TqBitTorrentSpeedsLimitType) and (Field = 'Fspeeds') then
  begin
    TqBitTorrentSpeedsLimitType(Data).Fspeeds := TqBitVariantDictionary<variant, variant>.Create;
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
      TqBitTorrentSpeedsLimitType(Data).Fspeeds.Add(
          JSONPair.JsonString.Value,
          JSONPair.JsonValue.Value.ToInteger
		    );
    JSONObj.Free
  end else
  raise
  Exception.Create(Format(
      'Class: %s, %s not implemented.',
      [Data.ClassName, 'TqBitObjectDictionaryInterceptor.StringReverter']
    ));
end;

function TqBitObjectDictionaryInterceptor.StringConverter(Data: TObject; Field: string): string;
begin
  raise
    Exception.Create(Format(
        'Class: %s, %s not implemented.',
        [Data.ClassName, 'TqBitObjectDictionaryInterceptor.StringConverter']
      ));
end;

{ TqBitStringDictionaryInterceptor }

procedure TqBitStringDictionaryInterceptor.StringReverter(Data: TObject; Field: string; Arg: string);
begin
  if (Data is TqBitPreferencesType) and (Field = 'Fscan_dirs') then
  begin
    TqBitPreferencesType(Data).Fscan_dirs := TqBitVariantDictionary<variant, variant>.Create;
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
      TqBitPreferencesType(Data).Fscan_dirs.Add(JSONPair.JsonString.Value, JSONPair.JsonValue.Value);
    JSONObj.Free
  end else
  raise
  Exception.Create(Format(
      'Class: %s, %s not implemented.',
      [Data.ClassName, 'TqBitStringDictionaryInterceptor.StringReverter']
    ));
end;

function TqBitStringDictionaryInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  Arr: array of string;
begin
  Result := '';
  if (Data is TqBitPreferencesType) and (Field = 'Fscan_dirs') then
  begin
    if assigned(TqBitPreferencesType(Data).Fscan_dirs) then
    begin
      for var v in TqBitPreferencesType(Data).Fscan_dirs do
      begin
        SetLength(Arr, Length(Arr) + 1);
        var p := TJSONPair.Create(v.Key, string(v.Value));
        Arr[ Length(Arr) - 1 ] := p.ToString;
        p.Free;
      end;
      Result :=('{' + string.Join(',', Arr) + '}');
      Result := TqBitPreferencesType(Data).RawJsonEncode('"', Result, '"');
    end;
  end else
  raise
    Exception.Create(Format(
        'Class: %s, %s not implemented.',
        [Data.ClassName, 'TqBitObjectDictionaryInterceptor.StringConverter']
      ));
end;

{ TqBitRSSObjectDictionaryInterceptor }

procedure TqBitRSSObjectDictionaryInterceptor.StringReverter(Data: TObject;
  Field, Arg: string);

  procedure RSSRecurse(Dic: TqBitObjectDictionary<variant, TqBitRSSItemType>; JsonStr: string; var Path: string);//; var Item: TqBitRSSItemType );
  begin
    var JSONObj := TJSONObject.ParseJSONValue(JsonStr) as TJSONObject;
    if JSONObj.GetValue('url') <> nil then
    begin
       Dic.Add(
        ExcludeTrailingPathDelimiter( Path ),
        TJson.JsonToObject<TqBitRSSItemType>(JsonStr)
       );
    end else begin
      for var JSONPair in JSONObj do
      begin
        var LPath := Path + JSONPair.JSonString.Value + '\';
        RSSRecurse(Dic, JSONPair.JsonValue.toString, LPath);
      end;
    end;
    JSONObj.Free;
  end;

begin
  if (Data is TqBitRSSAllItemsType) and (Field = 'Fitems') then
  begin
    TqBitRSSAllItemsType(Data).Fitems := TqBitObjectDictionary<variant, TqBitRSSItemType>.Create([doOwnsValues]);
    var JSONObj := TJSONObject.ParseJSONValue(Arg) as TJSONObject;
    for var JSONPair in JSONObj do
    begin
      var Path := JSONPair.JSonString.Value + '\';
      RSSRecurse(TqBitRSSAllItemsType(Data).Fitems, JSONPair.JsonValue.toString, Path);
    end;
    JSONObj.Free;
  end else
  raise
    Exception.Create(Format(
        'Class: %s, %s not implemented.',
        [Data.ClassName, 'TqBitRSSObjectDictionaryInterceptor.StringReverter']
      ));
end;

function TqBitRSSObjectDictionaryInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  raise
    Exception.Create(Format(
        'Class: %s, %s not implemented.',
        [Data.ClassName, 'TqBitRSSObjectDictionaryInterceptor.StringConverter']
      ));
end;

{$ENDREGION}

{$REGION 'TqBitTorrentBaseType Impl.'}

{ TqBitTorrentBaseType }

constructor TqBitTorrentBaseType.Create;
begin
  _RawJsonData := TDictionary<string, string>.Create;
  _RawJsonData.Clear;
end;

destructor TqBitTorrentBaseType.Destroy;
begin
  _RawJsonData.Free;
  inherited;
end;

procedure TqBitTorrentBaseType.ClonePropertiesTo(T : TqBitTorrentBaseType);
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

procedure TqBitTorrentBaseType.MergePropertiesFrom(T: TqBitTorrentBaseType);
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

procedure TqBitTorrentBaseType.Merge(T: TqBitTorrentBaseType);
begin
  MergePropertiesFrom(T);
end;

function TqBitTorrentBaseType.Clone: TqBitTorrentBaseType;
begin
  Result := TqBitTorrentBaseType(Self.ClassType.Create);
  Self.ClonePropertiesTo(Result);
  // Debug :
  // raise Exception.Create(Format('%s.Clone is not Implemented',[Self.ClassName]));
end;

function TqBitTorrentBaseType.RawJsonEncode(Header, Value, Footer: string): string;
var
  MyGuid0: TGUID;
begin
  CreateGUID(MyGuid0);
  Result := Format(
       '%0.8X%0.4X%0.4X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X',
       [MyGuid0.D1, MyGuid0.D2, MyGuid0.D3,
       MyGuid0.D4[0], MyGuid0.D4[1], MyGuid0.D4[2], MyGuid0.D4[3],
       MyGuid0.D4[4], MyGuid0.D4[5], MyGuid0.D4[6], MyGuid0.D4[7]]);
  _RawJsonData.Add(Header + Result + Footer, Value);
end;

function TqBitTorrentBaseType.RawJsonDecode(RawJson: string): string;
begin
  Result := RawJson;
  for var Tag in _RawJsonData do
    if Tag.Key <> '' then
      Result := StringReplace(Result, Tag.Key, Tag.Value, []);
end;

function TqBitTorrentBaseType.toJSON: string;
begin
  _RawJsonData.Clear;
  Result := TJson.ObjectToJsonString(Self, [joIgnoreEmptyStrings, joIgnoreEmptyArrays] );
  Result := RawJsonDecode(Result);
end;

procedure TqBitTorrentBaseType.Clear;
begin
  //
end;

function TqBitTorrentBaseType.toParams: string;
begin
  Result := '';
end;

{$ENDREGION}

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

{ TqBitPreferencesType }

function TqBitPreferencesType.Clone: TqBitTorrentBaseType;
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

procedure TqBitLogsType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitLogsType(From);
  if T.Flogs <> nil then
  begin
    if Self.Flogs = nil then Self.Flogs := TqBitObjectList<TqBitLogType>.Create(True);
    Self.Flogs.Merge(T.Flogs);
  end;
end;

function TqBitLogsType.Clone: TqBitTorrentBaseType;
begin
  var T := TqBitLogsType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Flogs <> nil then T.Flogs := Self.Flogs.Clone;
  Result := T;
end;

{ TqBitPeerLogsType }

procedure TqBitPeerLogsType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitPeerLogsType(From);
  if T.Flogs <> nil then
  begin
    if Self.Flogs = nil then Self.Flogs := TqBitObjectList<TqBitPeerLogType>.Create(True);
    Self.Flogs.Merge(T.Flogs);
  end;
end;

function TqBitPeerLogsType.Clone: TqBitTorrentBaseType;
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

function TqBitMainDataType.Clone: TqBitTorrentBaseType;
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

procedure TqBitMainDataType.Merge(From: TqBitTorrentBaseType);
var
  M: TqBitMainDataType;
begin

  if From = Nil then Exit;
  inherited Merge(From);
  M := TqBitMainDataType(From);
  VarMrg(Self.Ffull_update, M.Ffull_update, False);

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
  Fhashes := Unassigned;
  FhashesList := Nil;
end;

function TqBitTorrentListRequestType.ToParams: string;
begin
  Result := '';
  var sl := TStringList.Create;
  sl.StrictDelimiter :=True;
  sl.QuoteChar := #0;
  sl.Delimiter:='&';
  if not VarIsEmpty(Self.Ffilter) then
    sl.Add( 'filter='+  TNetEncoding.URL.Encode(Self.Ffilter) );
  if not VarIsEmpty(Self.Fcategory) then
    sl.Add( 'category='+  TNetEncoding.URL.Encode(Self.Fcategory) );
  if not VarIsEmpty(Self.Ftag) then
    sl.Add( 'tag='+  TNetEncoding.URL.Encode(Self.Ftag) );
  if not VarIsEmpty(Self.Fsort) then
    sl.Add( 'sort='+  TNetEncoding.URL.Encode(Self.Fsort) );
  if not VarIsEmpty(Self.Freverse) then
    sl.Add( 'reverse='+  TNetEncoding.URL.Encode(Self.Freverse) );
  if not VarIsEmpty(Self.Flimit) then
    sl.Add( 'limit='+  TNetEncoding.URL.Encode(Self.Flimit) );
  if not VarIsEmpty(Self.Foffset) then
    sl.Add( 'offset='+  TNetEncoding.URL.Encode(Self.Foffset) );
  if not VarIsEmpty(Self.Fhashes) then
    sl.Add( 'hashes='+  TNetEncoding.URL.Encode(Self.Fhashes) );
  if assigned(Self.FhashesList) then
  begin
    Self.Fhashes := Unassigned;
    FhashesList.StrictDelimiter := True;
    FhashesList.QuoteChar := #0;
    FhashesList.Delimiter:='|';
    sl.Add( 'hashes='+  TNetEncoding.URL.Encode(Self.FhashesList.DelimitedText) );
  end;
  Result := sl.DelimitedText;
  sl.Free;
end;

function TqBitTorrentListType.Clone: TqBitTorrentBaseType;
begin
  var T := TqBitTorrentListType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Ftorrents <> nil then T.Ftorrents := Self.Ftorrents.Clone;
  Result := T;
end;

destructor TqBitTorrentListType.Destroy;
begin
  Self.Ftorrents.Free;
  inherited Destroy;
end;

procedure TqBitTorrentListType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitTorrentListType(From);
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

function TqBitWebSeedsType.Clone: TqBitTorrentBaseType;
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

procedure TqBitWebSeedsType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitWebSeedsType(From);
  if T.Furls <> nil then
  begin
    if Self.Furls = nil then Self.Furls := TqBitObjectList<TqBitWebSeedType>.Create(True);
    Self.Furls.Merge(T.Furls);
  end;
end;

{ TqBitContentsType }

function TqBitContentsType.Clone: TqBitTorrentBaseType;
begin
  var T := TqBitContentsType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Fcontents <> nil then T.Fcontents := Self.Fcontents.Clone;
  Result := T;
end;

procedure TqBitContentsType.Merge(From: TqBitTorrentBaseType);
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

function TqBitTorrentSpeedsLimitType.Clone: TqBitTorrentBaseType;
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

procedure TqBitTorrentSpeedsLimitType.Merge(From: TqBitTorrentBaseType);
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

function TqBitCategoriesType.Clone: TqBitTorrentBaseType;
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

procedure TqBitCategoriesType.Merge(From: TqBitTorrentBaseType);
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

function TqBitContentType.Clone: TqBitTorrentBaseType;
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

procedure TqBitContentType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitContentType(From);
  if T.Fpiece_range <> nil then
  begin
    if Self.Fpiece_range = nil then Self.Fpiece_range := TqBitList<variant>.Create;
    Self.Fpiece_range.Merge(T.Fpiece_range);
  end;
end;

{ TqBitPiecesStatesType }

function TqBitPiecesStatesType.Clone: TqBitTorrentBaseType;
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

procedure TqBitPiecesStatesType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitPiecesStatesType(From);
  if T.Fstates <> nil then
  begin
    if Self.Fstates = nil then Self.Fstates :=TqBitList<variant>.Create;
    Self.Fstates.Merge(T.Fstates);
  end;
end;

{ TqBitTagsType }

function TqBitTagsType.Clone: TqBitTorrentBaseType;
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

procedure TqBitTagsType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitTagsType(From);
  if T.Ftags <> nil then
  begin
    if Self.Ftags = nil then Self.Ftags :=TqBitList<variant>.Create;
    Self.Ftags.Merge(T.Ftags);
  end;
end;

{ TqBitTorrentPeersDataType }

procedure TqBitTorrentPeersDataType.Merge(From: TqBitTorrentBaseType);
var
  P: TqBitTorrentPeersDataType;
begin
  if From = Nil then Exit;
  inherited Merge(From);
  P := TqBitTorrentPeersDataType(From);
  VarMrg(Self.Ffull_update, P.Ffull_update, False);

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

function TqBitTorrentPeersDataType.Clone: TqBitTorrentBaseType;
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

function TqBitRSSAllItemsType.Clone: TqBitTorrentBaseType;
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

procedure TqBitRSSAllItemsType.Merge(From: TqBitTorrentBaseType);
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

function TqBitRSSItemType.Clone: TqBitTorrentBaseType;
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

procedure TqBitRSSItemType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitRSSItemType(From);
  if T.Farticles <> nil then
  begin
    if Self.Farticles = nil then Self.Farticles := TqBitObjectList<TqBitRSSArticleType>.Create(True);
    Self.Farticles.Merge(T.Farticles);
  end;
end;

{ TqBitRSSAllRulesType }

function TqBitRSSAllRulesType.Clone: TqBitTorrentBaseType;
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

procedure TqBitRSSAllRulesType.Merge(From: TqBitTorrentBaseType);
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

function TqBitRSSArticles.Clone: TqBitTorrentBaseType;
begin
  var T := TqBitRSSArticles.Create;
  Self.ClonePropertiesTo(T);
  if Self.Farticles <> nil then T.Farticles := Self.Farticles.Clone;
  Result := T;
end;

destructor TqBitRSSArticles.Destroy;
begin
  Farticles.Free;
  inherited;
end;

procedure TqBitRSSArticles.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitRSSArticles(From);
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

function TqBitRSSRuleType.Clone: TqBitTorrentBaseType;
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

procedure TqBitRSSRuleType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitRSSRuleType(From);
  if T.FaffectedFeeds <> nil then
  begin
    if Self.FaffectedFeeds = nil then Self.FaffectedFeeds := TqBitList<variant>.Create;
    Self.FaffectedFeeds.Merge(T.FaffectedFeeds);
  end;
end;

{ TqBitAutoDownloadingRulesType }

function TqBitAutoDownloadingRulesType.Clone: TqBitTorrentBaseType;
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

procedure TqBitAutoDownloadingRulesType.Merge(From: TqBitTorrentBaseType);
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

procedure TqBitTrackersType.Merge(From: TqBitTorrentBaseType);
begin
  var T := TqBitTrackersType(From);
  if T.FTrackers <> nil then
  begin
    if Self.FTrackers = nil then Self.Ftrackers := TqBitObjectList<TqBitTrackerType>.Create(True);
    Self.FTrackers.Merge(T.FTrackers);
  end;
end;

function TqBitTrackersType.Clone: TqBitTorrentBaseType;
begin
  var T := TqBitTrackersType.Create;
  Self.ClonePropertiesTo(T);
  if Self.Ftrackers <> nil then T.Ftrackers := Self.Ftrackers.Clone;
  Result := T;
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


end.

