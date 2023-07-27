program IPAPIDemo;

{$R 'CountryFlags.res' '..\..\Common\uCountryFlags\CountryFlags.rc'}

uses

  Vcl.Forms,
  uIPAPIDemo in 'uIPAPIDemo.pas' {Form2},
  uqBitUtils in '..\..\Common\uqBitUtils.pas',
  uCountryFlags in '..\..\Common\uCountryFlags\uCountryFlags.pas',
  uIpAPI in '..\..\Common\uIpAPI.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.


