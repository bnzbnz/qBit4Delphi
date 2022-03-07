unit uTorrentReaderDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uTorrentReader,

  uBEncode, DateUtils, System.Generics.Collections, System.Generics.Defaults,
  Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    FileOpenDialog1: TFileOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

// Helpers
const
  BoolToStr: array[boolean] of string = ('False','True');

function VarFormatBKM(v: variant): string;
var
  x: Double;
begin
    Result := '0 B';
    x := v;
    if x < 0 then
    begin
      Result := 'N/A';
      Exit;
    end else
    if (x / 1099511627776 >= 1) then
    begin
      Result := Format('%.2f', [x / 1099511627776 ])+ ' TiB';
      Exit;
    end else

    if (x / (1024 * 1024 * 1024) >= 1) then
    begin
      Result := Format('%.2f', [x /(1024 * 1024 * 1024)] )+ ' GiB';
      Exit;
    end else
    if (x / (1024 * 1024)>= 1) then
    begin
      Result := Format('%.2f', [x /(1024 * 1024)] )+ ' MiB';
      Exit;
    end else
    if (x / (1024)) >= 1 then
    begin
      Result := Format('%.2f', [x /(1024)] )+ ' KiB';
    end else begin
      Result := Format('%.0f', [x] )+ ' B';
    end;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  Torrent: TTorrentReader;
begin
  Torrent := nil;
  Memo1.Clear;
  if not FileOpenDialog1.Execute then exit;
  try
    var Tme := GetTickCount64;
    Torrent := TTorrentReader.LoadFromFile(FileOpenDialog1.FileName);
    Tme := GetTickCount64 - Tme;
    Memo1.Lines.add( 'Filename : ' + FileOpenDialog1.FileName + ' (' + Tme.ToString + 'ms)');
    Memo1.Lines.add( 'Version : ' + Torrent.Data.Info.MetaVersion.ToString );
    Memo1.Lines.add( 'Hash : ' + Torrent.Data.Hash );
    Memo1.Lines.add( 'Announce: ' + Torrent.Data.Announce); // AnnounceList for multiple Annouces
    Memo1.Lines.add( 'Created By: ' + Torrent.Data.CreatedBy);
    Memo1.Lines.add( 'Creation Date: ' + DateTimeToStr(Torrent.Data.CreationDate));
    Memo1.Lines.add( 'Comment: ' + Torrent.Data.Comment);
    Memo1.Lines.add( 'Private: ' + BoolToStr[Torrent.Data.Info.IsPrivate]);
    Memo1.Lines.Add( '' );
    Memo1.Lines.Add( 'Multi Announce List  : ');
    for var Url in Torrent.Data.AnnounceList do Memo1.Lines.Add(Url);
    Memo1.Lines.Add( '' );
    Memo1.Lines.Add( 'Multi Web Seed Url List: ');
    for var Url in Torrent.Data.UrlList do Memo1.Lines.Add(Url);
    Memo1.Lines.Add( '' );
    Memo1.Lines.add( 'Root Folder : ' + Torrent.Data.Info.Name );
    Memo1.Lines.add( 'Files :' );
    for var FileData in Torrent.Data.Info.FileList do
    begin
      Memo1.Lines.add( '   -> ' + FileData.FullPath );
      Memo1.Lines.add( '       Size :' + VarFormatBKM(FileData.Length));
    end;
  finally
    Torrent.Free;
  end;
end;

end.
