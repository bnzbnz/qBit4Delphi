program MiniThin;

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
  Vcl.Grids,
  uMiniThin in 'DemoFiles\uMiniThin.pas' {MiniThinForm},
  uqBitFormat in 'DemoFiles\uqBitFormat.pas',
  uSelectServer in 'DemoFiles\uSelectServer.pas',
  uAddServer in 'DemoFiles\uAddServer.pas',
  uSetLocation in 'DemoFiles\uSetLocation.pas' {SetLocationDlg};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.CreateForm(TMiniThinForm, MiniThinForm);
  Application.CreateForm(TAddServerDlg, AddServerDlg);
  Application.CreateForm(TSelectServerDlg, SelectServerDlg);
  Application.CreateForm(TSetLocationDlg, SetLocationDlg);
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
