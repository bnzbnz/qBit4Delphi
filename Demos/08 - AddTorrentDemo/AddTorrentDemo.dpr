program AddTorrentDemo;
uses


  {$INCLUDE ..\Defines.inc}
  {$IFDEF FASTMM4}
    {$IFDEF DEBUG}
      FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
    {$ENDIF}
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
  uqBitAPIUtils  in '..\..\API\uqBitAPIUtils .pas',
  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',

  Vcl.Forms,
  uAddTorrentDemo in 'uAddTorrentDemo.pas' {FrmAddTorrent},
  uqBitUtils  in '..\..\Common\uqBitUtils.pas',
  uqBitPatchChecker in '..\..\Common\uqBitPatchChecker.pas',
  uqBitFormat in '..\..\Common\uqBitFormat.pas',
  uTorrentReader in '..\..\Common\uTorrentReader.pas',
  uBEncode in '..\..\Common\uBEncode.pas',

  uqBitAddServerDlg in '..\..\Common\Dialogs\uqBitAddServerDlg.pas' {qBitAddServerDlg},
  uqBitAddTorrentDlg in '..\..\Common\Dialogs\uqBitAddTorrentDlg.pas' {qBitAddTorrentDlg},
  uqBitCategoriesDlg in '..\..\Common\Dialogs\uqBitCategoriesDlg.pas' {qBitCatDlg},
  uqBitSelectServerDlg in '..\..\Common\Dialogs\uqBitSelectServerDlg.pas' {qBitSelectServerDlg};

{$R *.res}


begin
  {$IFNDEF FASTMM4} {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := True; {$ENDIF} {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmAddTorrent, FrmAddTorrent);
  Application.CreateForm(TqBitAddServerDlg, qBitAddServerDlg);
  Application.CreateForm(TqBitSelectServerDlg, qBitSelectServerDlg);
  Application.CreateForm(TqBitCategoriesDlg, qBitCategoriesDlg);
  Application.CreateForm(TqBitAddServerDlg, qBitAddServerDlg);
  Application.CreateForm(TqBitAddTorrentDlg, qBitAddTorrentDlg);
  Application.Run;
end.

  // Place Holder :


  {$INCLUDE ..\Defines.inc}
  {$IFDEF FASTMM4}
    {$IFDEF DEBUG}
      FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
    {$ENDIF}
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
  uqBitAPIUtils  in '..\..\API\uqBitAPIUtils .pas',
  uqBitAPITypes in '..\..\API\uqBitAPITypes.pas',
  uqBitAPI in '..\..\API\uqBitAPI.pas',
  uqBitObject in '..\..\API\uqBitObject.pas',


  {$IFNDEF FASTMM4} {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := True; {$ENDIF} {$ENDIF}

