program TorrentReaderDemo;
uses
  {$IFDEF DEBUG}
    FastMM4,    //  MPL 1.1, LGPL 2.1 (https://github.com/pleriche/FastMM4)  << Can be removed if not used
  {$ENDIF}
  Vcl.Forms,
  uTorrentReaderDemo in 'uTorrentReaderDemo.pas' {Form2},
  uTorrentReader in '..\..\API\Tools\uTorrentReader.pas',
  uBEncode in '..\..\API\Tools\uBEncode.pas';
{$R *.res} 
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
