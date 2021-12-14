unit uSimple;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uqBitObject, uqBitAPI, uqBitAPITypes,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Timer1: TTimer;
    Warning: TMemo;
    LBTorrents: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    qB: TqBitObject;
    qBMain: TqBitMainDataType;
    procedure UpdateUI;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses uSelectServer, uPatcherChecker;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  qBMain.Free;
  qB.Free;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
 Warning.Visible := False;
 ShowMessage('In order to run this demo locally, start qBittorrent.exe -> Parameters -> Web UI -> ENABLE : "WebUI Remote Interface (Remote Control)" and "bypass atuhentification for clients on localhost"' + #$D#$A + 'NOX users know whar to do...');
 if SelectServerDlg.ShowModal = mrOk then
 begin
  var Server := SelectServerDlg.GetServer;
  qB := TqBitObject.Connect( Server.FHP, Server.FUN, Server.FPW);
  qBMain :=qB.GetMainData(0); // >> Full Data
  UpdateUI;
  Timer1.Interval := qBMain.Fserver_state.Frefresh_interval; // The update interval is defined by the server
  Timer1.Enabled := True;
 end;
end;

procedure TForm2.UpdateUI;
begin
  ////////////////  Few Properties...
  Caption := Format('Torrents : %d', [qBMain.Ftorrents.Count]);
  Caption := Caption + ' / ';
  Caption := Caption + Format('Dl : %s KiB/s', [qBMain.Fserver_state.Fdl_info_speed div 1024 ]);
  Caption := Caption + ' / ';
  Caption := Caption + Format('Up : %s KiB/s', [qBMain.Fserver_state.FUp_info_speed div 1024 ]);

  LBTorrents.Clear;
  for var T in qBMAin.Ftorrents do
      LBTorrents.Items.Add( TqBitTorrentType(T.Value).Fname );
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  var Update := qb.GetMainData(qBMain.Frid); // >> Get The Data since the last getMain;
  qBMain.Merge(Update); // we merge the update : qBMain is now up to date
  Update.Free;
  UpdateUI;
end;


initialization
  PatcherChecker; // Check if JSON Libs have been patched
end.
