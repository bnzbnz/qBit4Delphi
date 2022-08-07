program vnStatDemo;

uses

  {$IFDEF DEBUG}
    FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
  {$ENDIF}

  Vcl.Forms,
  uTvnStatDemo in 'uTvnStatDemo.pas' {vnStatDemoFrm},
  uTvnStatClient in '..\..\Common\uTvnStatClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TvnStatDemoFrm, vnStatDemoFrm);
  Application.Run;
end.
