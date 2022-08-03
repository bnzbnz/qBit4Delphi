program Simple;

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
  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',
  Vcl.Forms,
  uSimple in 'uSimple.pas' {Form2},
  uAddServer in '..\common\uAddServer.pas' {AddServerDlg},
  uSelectServer in '..\common\uSelectServer.pas' {SelectServerDlg},
  uPatcherChecker in '..\common\uPatcherChecker.pas',
  uqBitSelectServerDlg in '..\common\uqBitSelectServerDlg.pas' {qBitServerSelectDlg},
  uqBitAddServerDlg in '..\common\uqBitAddServerDlg.pas' {qBitAddServerDlg};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TAddServerDlg, AddServerDlg);
  Application.CreateForm(TSelectServerDlg, SelectServerDlg);
  Application.CreateForm(TqBitServerSelectDlg, qBitServerSelectDlg);
  Application.CreateForm(TqBitAddServerDlg, qBitAddServerDlg);
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

