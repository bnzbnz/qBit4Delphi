unit uTorrentReaderDemo;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uTorrentReader,
  uBEncode, DateUtils, System.Generics.Defaults, System.Generics.Collections,
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

{ Helpers }

const
  BoolToStr: array[boolean] of string = ('False','True');

function Int64FormatBKM(v: Int64): string;
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
  StringBuilder: TStringBuilder;
begin
  Torrent := nil; StringBuilder := nil;
  Memo1.Clear;
  if not FileOpenDialog1.Execute then exit;
  try
    Torrent := TTorrentReader.LoadFromFile(FileOpenDialog1.FileName);
    if Torrent = nil then
    begin
      Memo1.Text := 'Invalid Torrent File Format';
      Exit;
    end;
    StringBuilder := TStringBuilder.Create;
    StringBuildeR.AppendLine( 'Filename : ' + FileOpenDialog1.FileName);
    StringBuildeR.AppendLine( 'Parsing Duration : ' + (Torrent.ProcessingTimeMS).ToString + 'ms');
    StringBuildeR.AppendLine( 'Version : ' + Torrent.Data.Info.MetaVersion.ToString );
    StringBuildeR.AppendLine( 'Hybrid : ' + BoolToStr[Torrent.Data.Info.IsHybrid]);
    StringBuildeR.AppendLine( 'HashV1 : ' + Torrent.Data.HashV1 );
    StringBuildeR.AppendLine( 'HashV2 : ' + Torrent.Data.HashV2 );
    StringBuildeR.AppendLine( 'Announce : ' + Torrent.Data.Announce); // AnnounceList for multiple Annouces
    StringBuildeR.AppendLine( 'Created By : ' + Torrent.Data.CreatedBy);
    StringBuildeR.AppendLine( 'Creation Date : ' + DateTimeToStr(Torrent.Data.CreationDate));
    StringBuildeR.AppendLine( 'Comments : ');
    StringBuildeR.AppendLine(  Torrent.Data.Comment.Text);
    StringBuildeR.AppendLine( 'Private : ' + BoolToStr[Torrent.Data.Info.IsPrivate]);
    StringBuildeR.AppendLine( '' );
    StringBuildeR.AppendLine( 'Announce Tier/URLs : ');
    for var Tier in Torrent.Data.AnnouncesDict do
    begin
      StringBuildeR.AppendLine( '  Tier : ' + Tier.Key.ToString  );
      for var Url in Tier.Value do
        StringBuildeR.AppendLine( '         ' +  Url );
    end;
    StringBuildeR.AppendLine( '' );
    StringBuildeR.AppendLine( 'Web Seeds : ');
    for var Url in Torrent.Data.WebSeeds do StringBuildeR.AppendLine(Url);
    StringBuildeR.AppendLine( '' );
    StringBuildeR.AppendLine( 'Root Folder : ' + Torrent.Data.Info.Name );
    StringBuildeR.AppendLine( 'Files Count : ' + Torrent.Data.Info.FileList.Count.ToString);
    StringBuildeR.AppendLine( 'Pieces Length : ' + Int64FormatBKM(Torrent.Data.Info.PieceLength));
    StringBuildeR.AppendLine( 'Pieces Count (Total): ' + Torrent.Data.Info.PiecesCount.ToString);
    StringBuildeR.AppendLine( 'Files Size (Total): ' + Int64FormatBKM(Torrent.Data.Info.FilesSize));
    StringBuildeR.AppendLine;
    for var FileData in Torrent.Data.Info.FileList do
    begin
      StringBuildeR.AppendLine('   -> ' + FileData.FullPath);
      StringBuildeR.AppendLine( '       Size :' + Int64FormatBKM(FileData.Length));
    end;
    Memo1.Text := StringBuilder.ToString;
    Memo1.Lines.Insert(0, '');
  finally
    StringBuilder.Free;
    Torrent.Free;
  end;
end;

end.
