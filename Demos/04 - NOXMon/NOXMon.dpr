program NOXMon;
uses

  {$IFDEF DEBUG}
    FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
  {$ENDIF}

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

  Vcl.Forms,
  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',
  uNOXMon in 'uNOXMon.pas' {NOXMonDlg},
  uqBitFormat in '..\..\Common\uqBitFormat.pas',
  uqBitPatchChecker in '..\..\Common\uqBitPatchChecker.pas',
  uqBitAddServerDlg in '..\..\Common\Dialogs\uqBitAddServerDlg.pas' {qBitAddServerDlg},
  uqBitSelectServerDlg in '..\..\Common\Dialogs\uqBitSelectServerDlg.pas' {qBitSelectServerDlg};

{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TNOXMonDlg, NOXMonDlg);
  Application.CreateForm(TqBitSelectServerDlg, qBitSelectServerDlg);
  Application.CreateForm(TqBitAddServerDlg, qBitAddServerDlg);
  Application.Run;
end.

  {$IFDEF DEBUG}
    FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
  {$ENDIF}

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
