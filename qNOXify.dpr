program qNOXify;

uses
  {$IFDEF VER340}
    REST.Json.Types in 'JSON\Sydney.10.4.2\REST.Json.Types.pas',
    REST.JsonReflect in 'JSON\Sydney.10.4.2\REST.JsonReflect.pas',
    System.JSON in 'JSON\Sydney.10.4.2\System.JSON.pas',
    REST.Json in 'JSON\Sydney.10.4.2\REST.Json.pas',
  {$ENDIF }
  {$IFDEF VER350}
    REST.Json.Types in 'JSON\Alexandria.11.0\REST.Json.Types.pas',
    REST.JsonReflect in 'JSON\Alexandria.11.0\REST.JsonReflect.pas',
    System.JSON in 'JSON\Alexandria.11.0\System.JSON.pas',
    REST.Json in 'JSON\Alexandria.11.0\REST.Json.pas',
  {$ENDIF}

  uqBitAPITypes in 'API\uqBitAPITypes.pas',
  uqBitAPI in 'API\uqBitAPI.pas',
  uqBitObject in 'API\uqBitObject.pas',

  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uqNOXify in 'Units\uqNOXify.pas' {qNOXifyFrm},
  uqBitFormat in 'Units\uqBitFormat.pas',
  uSelectServer in 'Units\uSelectServer.pas',
  uAddServer in 'Units\uAddServer.pas',
  uSetLocation in 'Units\uSetLocation.pas',
  uSpeedLimitsDlg in 'Units\uSpeedLimitsDlg.pas',
  uAppTrackMenus in 'Units\uAppTrackMenus.pas',
  uAddEditCat in 'Units\uAddEditCat.pas',
  uAddTorrent in 'Units\uAddTorrent.pas' {AddTorrentDlg};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TqNOXifyFrm, qNOXifyFrm);
  Application.CreateForm(TAddServerDlg, AddServerDlg);
  Application.CreateForm(TSelectServerDlg, SelectServerDlg);
  Application.CreateForm(TSetLocationDlg, SetLocationDlg);
  Application.CreateForm(TSetLocationDlg, SetLocationDlg);
  Application.CreateForm(TSpeedLimitsDlg, SpeedLimitsDlg);
  Application.CreateForm(TAddEditCatDlg, AddEditCatDlg);
  Application.CreateForm(TAddTorrentDlg, AddTorrentDlg);
  Application.Run;
end.

(*
  {$IFDEF VER340}
    REST.Json.Types in 'JSON\Sydney.10.4.2\REST.Json.Types.pas',
    REST.JsonReflect in 'JSON\Sydney.10.4.2\REST.JsonReflect.pas',
    System.JSON in 'JSON\Sydney.10.4.2\System.JSON.pas',
    REST.Json in 'JSON\Sydney.10.4.2\REST.Json.pas',
  {$ENDIF }
  {$IFDEF VER350}
    REST.Json.Types in 'JSON\Alexandria.11.0\REST.Json.Types.pas',
    REST.JsonReflect in 'JSON\Alexandria.11.0\REST.JsonReflect.pas',
    System.JSON in 'JSON\Alexandria.11.0\System.JSON.pas',
    REST.Json in 'JSON\Alexandria.11.0\REST.Json.pas',
  {$ENDIF}

  uqBitAPITypes in 'API\uqBitAPITypes.pas',
  uqBitAPI in 'API\uqBitAPI.pas',
  uqBitObject in 'API\uqBitObject.pas',
*)
