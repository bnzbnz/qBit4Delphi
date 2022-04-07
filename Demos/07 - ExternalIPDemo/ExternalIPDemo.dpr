program ExternalIPDemo;

uses
  Vcl.Forms,
  uExternalIPDemo in 'uExternalIPDemo.pas' {Form1},
  uExternalIP in '..\..\API\Tools\uExternalIP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
