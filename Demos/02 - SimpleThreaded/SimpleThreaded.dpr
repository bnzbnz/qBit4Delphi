program SimpleThreaded;

uses
  {$IFDEF VER340}
    REST.Json.Types in '..\..\JSON\21\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\JSON\21\REST.JsonReflect.pas',
    System.JSON in '..\..\JSON\21\System.JSON.pas',
    REST.Json in '..\..\JSON\21\REST.Json.pas',
  {$ENDIF }
  {$IFDEF VER350}
    REST.Json.Types in '..\..\JSON\22\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\JSON\22\REST.JsonReflect.pas',
    System.JSON in '..\..\JSON\22\System.JSON.pas',
    REST.Json in '..\..\JSON\22\REST.Json.pas',
  {$ENDIF}

  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',

  Vcl.Forms,
  uSimpleThreaded in 'uSimpleThreaded.pas' {SimpleThreadedDlg},
  uAddServer in '..\common\uAddServer.pas' {AddServerDlg},
  uSelectServer in '..\common\uSelectServer.pas' {SelectServerDlg},
  uPatcherChecker in '..\common\uPatcherChecker.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSimpleThreadedDlg, SimpleThreadedDlg);
  Application.CreateForm(TAddServerDlg, AddServerDlg);
  Application.CreateForm(TSelectServerDlg, SelectServerDlg);
  Application.Run;
end.

(*

  ReportMemoryLeaksOnShutdown := True;

  {$IFDEF VER340}
    REST.Json.Types in '..\..\JSON\21\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\JSON\21\REST.JsonReflect.pas',
    System.JSON in '..\..\JSON\21\System.JSON.pas',
    REST.Json in '..\..\JSON\21\REST.Json.pas',
  {$ENDIF }
  {$IFDEF VER350}
    REST.Json.Types in '..\..\JSON\22\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\JSON\22\REST.JsonReflect.pas',
    System.JSON in '..\..\JSON\22\System.JSON.pas',
    REST.Json in '..\..\JSON\22\REST.Json.pas',
  {$ENDIF}

  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',
*)
