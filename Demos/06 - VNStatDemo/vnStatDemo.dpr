program vnStatDemo;

uses

  {$IFDEF FASTMM4}
    FastMM4,  //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)
  {$ENDIF}

  Vcl.Forms,
  uTvnStatDemo in 'uTvnStatDemo.pas' {vnStatDemoFrm},
  uTvnStatClient in '..\..\Common\uTvnStatClient.pas';

{$R *.res}

begin
  {$IFNDEF FASTMM4} {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := True; {$ENDIF}{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TvnStatDemoFrm, vnStatDemoFrm);
  Application.Run;
end.

  // Place Holder:

  {$IFDEF FASTMM4}
    FastMM4,  //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)
  {$ENDIF}

  {$IFNDEF FASTMM4} {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := True; {$ENDIF}{$ENDIF}

