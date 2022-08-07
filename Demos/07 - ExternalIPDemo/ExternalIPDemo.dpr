program ExternalIPDemo;

uses

  {$IFDEF DEBUG}
    FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
  {$ENDIF}

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
