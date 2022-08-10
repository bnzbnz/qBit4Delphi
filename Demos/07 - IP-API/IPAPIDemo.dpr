program IPAPIDemo;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  uIPAPIDemo in 'uIPAPIDemo.pas' {Form2},
  uIP_API in '..\..\Common\uIP_API.pas',
  uqBitUtils in '..\..\Common\uqBitUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
