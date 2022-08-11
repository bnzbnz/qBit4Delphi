unit uSimple;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uqBitObject, uqBitAPI, uqBitAPITypes,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFrmSimple = class(TForm)
    Timer1: TTimer;
    Warning: TMemo;
    LBTorrents: TListBox;
    Panel1: TPanel;
    LinkLabel1: TLinkLabel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private declarations }
  public
    { Public declarations }
    qB: TqBitObject;
    qBMain: TqBitMainDataType;
    procedure UpdateUI;
  end;
var
  FrmSimple: TFrmSimple;

implementation
{$R *.dfm}
uses ShellAPI, uqBitPatchChecker, uqBitSelectServerDlg, uqBitFormat;

procedure TFrmSimple.LinkLabel1LinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
 ShellExecute(0, 'Open', PChar(Link), PChar(''), nil, SW_SHOWNORMAL);
end;

procedure TFrmSimple.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  qBMain.Free;
  qB.Free;
end;

procedure TFrmSimple.FormShow(Sender: TObject);
begin
  Warning.Visible := False;
  ShowMessage('In order to run this demo locally, start qBittorrent.exe -> Parameters -> Web UI -> ENABLE : "WebUI Remote Interface (Remote Control)" and "bypass atuhentification for clients on localhost"' + #$D#$A + 'NOX users know what to do...');
  if qBitSelectServerDlg.ShowModal = mrOk then
  begin
    var Server := qBitSelectServerDlg.GetServer;
    qB := TqBitObject.Connect(Server.FHP, Server.FUN, Server.FPW);
    qBMain := qB.GetMainData(0); // >> Full Data
    UpdateUI;
    Timer1.Interval := qBMain.Fserver_state.Frefresh_interval; // The update interval is defined by the server
    Timer1.Enabled := True;
  end else
    PostMessage(Handle, WM_CLOSE,0 ,0);
end;

procedure TFrmSimple.UpdateUI;
begin
  ////////////////  Few Properties...
  Caption := Format('Torrents : %d', [qBMain.Ftorrents.Count]);
  Caption := Caption + ' / ';
  Caption := Caption + Format('Dl : %s', [VarFormatBKMPerSec(qBMain.Fserver_state.Fdl_info_speed)]);
  Caption := Caption + ' / ';
  Caption := Caption + Format('Up : %s', [VarFormatBKMPerSec(qBMain.Fserver_state.Fup_info_speed)]);
  LBTorrents.Clear;
  for var T in qBMain.Ftorrents do
    LBTorrents.Items.Add( TqBitTorrentType(T.Value).Fname );
end;

procedure TFrmSimple.Timer1Timer(Sender: TObject);
begin
  var Update := qb.GetMainData(qBMain.Frid); // >> Get The Data since the last getMainData
  if Update <> Nil then
  begin
    qBMain.Merge(Update); // we merge the update : qBMain is now up to date
  end else begin
    Timer1.Enabled := False;
    LBTorrents.Clear;
    LBTorrents.Items.Add('Disconnected...');
    Exit;
  end;
  Update.Free;
  UpdateUI;
end;

end.
