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

  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,

  uqBitAPITypes in 'API\uqBitAPITypes.pas',
  uqBitAPI in 'API\uqBitAPI.pas',
  uqBitObject in 'API\uqBitObject.pas',
  uqNOXify in 'DemoFiles\uqNOXify.pas' {qNOXifyFrm},
  uqBitFormat in 'DemoFiles\uqBitFormat.pas',
  uSelectServer in 'DemoFiles\uSelectServer.pas',
  uAddServer in 'DemoFiles\uAddServer.pas',
  uSetLocation in 'DemoFiles\uSetLocation.pas',
  uSpeedLimitsDlg in 'DemoFiles\uSpeedLimitsDlg.pas',
  uAppTrackMenus in 'DemoFiles\uAppTrackMenus.pas',
  uAddEditCat in 'DemoFiles\uAddEditCat.pas';

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
