program vnStatDemo;

uses

  Vcl.Forms,
  uTvnStatDemo in 'uTvnStatDemo.pas' {vnStatDemoFrm},
  uTvnStatClient in '..\..\Common\uTvnStatClient.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TvnStatDemoFrm, vnStatDemoFrm);
  Application.Run;
end.


