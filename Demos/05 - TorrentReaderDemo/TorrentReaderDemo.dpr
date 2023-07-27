program TorrentReaderDemo;
uses

  Vcl.Forms,
  uTorrentReaderDemo in 'uTorrentReaderDemo.pas' {Form2},
  uTorrentReader in '..\..\Common\uTorrentReader.pas',
  uBEncode in '..\..\Common\uBEncode.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

