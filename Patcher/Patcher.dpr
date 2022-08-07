program Patcher;
uses
  {$IFDEF DEBUG}
    FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
  {$ENDIF}
  Vcl.Forms,
  uPAtcher in 'uPatcher.pas' {Form2};
{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
