program SimpleThreaded;
uses
  {$IFDEF VER340}
  REST.Json.Types in '..\..\API\JSON\21\REST.Json.Types.pas',
  {$ENDIF }
  {$IFDEF VER350}
  REST.Json.Types in '..\..\API\JSON\22\REST.Json.Types.pas',
  REST.JsonReflect in '..\..\API\JSON\22\REST.JsonReflect.pas',
  System.JSON in '..\..\API\JSON\22\System.JSON.pas',
  REST.Json in '..\..\API\JSON\22\REST.Json.pas',
  {$ENDIF }
  Vcl.Forms,
  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',
  uSimpleThreaded in 'uSimpleThreaded.pas' {FrmSimpleThreaded},
  uqBitAddServerDlg in '..\common\uqBitAddServerDlg.pas' {qBitAddServerDlg},
  uqBitSelectServerDlg in '..\common\uqBitSelectServerDlg.pas' {qBitSelectServerDlg},
  uqBitPatchChecker in '..\common\uqBitPatchChecker.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmSimpleThreaded, FrmSimpleThreaded);
  Application.CreateForm(TqBitAddServerDlg, qBitAddServerDlg);
  Application.CreateForm(TqBitSelectServerDlg, qBitSelectServerDlg);
  Application.Run;
end.

  {$IFDEF VER340}
    REST.Json.Types in '..\..\API\JSON\21\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\API\JSON\21\REST.JsonReflect.pas',
    System.JSON in '..\..\API\JSON\21\System.JSON.pas',
    REST.Json in '..\..\API\JSON\21\REST.Json.pas',
  {$ENDIF }
  {$IFDEF VER350}
    REST.Json.Types in '..\..\API\JSON\22\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\API\JSON\22\REST.JsonReflect.pas',
    System.JSON in '..\..\API\JSON\22\System.JSON.pas',
    REST.Json in '..\..\API\JSON\22\REST.Json.pas',
  {$ENDIF}
