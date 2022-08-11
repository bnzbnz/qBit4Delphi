program IPAPIDemo;

{$R 'CountryFlags.res' '..\..\Common\uCountryFlags\CountryFlags.rc'}

uses

  {$IFDEF FASTMM4}
  FastMM4,
  {$ENDIF }

  Vcl.Forms,
  uIPAPIDemo in 'uIPAPIDemo.pas' {Form2},
  uIP_API in '..\..\Common\uIP_API.pas',
  uqBitUtils in '..\..\Common\uqBitUtils.pas',
  uCountryFlags in '..\..\Common\uCountryFlags\uCountryFlags.pas';

{$R *.res}

begin
  {$IFNDEF FASTMM4} {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := True; {$ENDIF}{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

  // Place Holder:

  {$IFDEF FASTMM4}
    FastMM4,  //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)
  {$ENDIF}

  {$IFNDEF FASTMM4} {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := True; {$ENDIF}{$ENDIF}

