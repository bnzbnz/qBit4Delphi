program SimpleThreadedGrid;
uses

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
  {$IFDEF VER360}
    REST.Json.Types in '..\..\API\JSON\23\REST.Json.Types.pas',
    REST.JsonReflect in '..\..\API\JSON\23\REST.JsonReflect.pas',
    System.JSON in '..\..\API\JSON\23\System.JSON.pas',
    REST.Json in '..\..\API\JSON\23\REST.Json.pas',
  {$ENDIF} 
  uqBitAPIUtils  in '..\..\API\uqBitAPIUtils .pas',
  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',

  Vcl.Forms,
  uqBitUtils in '..\..\Common\uqBitUtils.pas',
  uqBitPatchChecker in '..\..\Common\uqBitPatchChecker.pas',
  uqBitFormat in '..\..\Common\uqBitFormat.pas',
  uqBitAddServerDlg in '..\Common\Dialogs\uqBitAddServerDlg.pas' {qBitAddServerDlg},
  uqBitSelectServerDlg in '..\Common\Dialogs\uqBitSelectServerDlg.pas' {qBitSelectServerDlg},
  uKobicAppTrackMenus in '..\Common\uKobicAppTrackMenus.pas',
  uqBitGrid in '..\Common\uqBitGrid.pas' {qBitFrame: TFrame},
  uqBitThreads in '..\..\Common\uqBitThreads.pas',
  uSimpleThreadedGrid in 'uSimpleThreadedGrid.pas' {FrmSTG};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmSTG, FrmSTG);
  Application.CreateForm(TqBitAddServerDlg, qBitAddServerDlg);
  Application.CreateForm(TqBitSelectServerDlg, qBitSelectServerDlg);
  Application.Run;
end.

