program Patcher;

uses
  Vcl.Forms,
  uPAtcher in 'uPatcher.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
