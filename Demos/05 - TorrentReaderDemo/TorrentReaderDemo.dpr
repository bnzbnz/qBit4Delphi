program TorrentReaderDemo;
uses

  {$INCLUDE ..\Defines.inc}
  {$IFDEF FASTMM4}
    {$IFDEF DEBUG}
      FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
    {$ENDIF}
  {$ENDIF}

  Vcl.Forms,
  uTorrentReaderDemo in 'uTorrentReaderDemo.pas' {Form2},

  uTorrentReader in '..\..\Common\uTorrentReader.pas',
  uBEncode in '..\..\Common\uBEncode.pas';

{$R *.res}

begin
  {$IFNDEF FASTMM4} {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := True; {$ENDIF} {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

  // Place Holder:

  {$INCLUDE ..\Defines.inc}
  {$IFDEF FASTMM4}
    {$IFDEF DEBUG}
      FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF FASTMM4} {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := True; {$ENDIF} {$ENDIF}
