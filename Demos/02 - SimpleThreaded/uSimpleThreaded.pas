unit uSimpleThreaded;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uqBitObject, uqBitAPI, uqBitAPITypes,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TqBitThread = class(TThread)
    qB: TqBitObject;
    qBMainTh: TqBitMainDataType;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TFrmSimpleThreaded = class(TForm)
    LBTorrents: TListBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    qB: TqBitObject;
    qBMain: TqBitMainDataType;
    Th: TqBitThread;
    procedure SyncThread(Sender: TqBitThread);
    procedure UpdateUI;
  end;

var
  FrmSimpleThreaded: TFrmSimpleThreaded;

implementation

{$R *.dfm}

uses uqBitSelectServerDlg;

{ TqBitThread }

destructor TqBitThread.Destroy;
begin
  qB.Free;
  inherited;
end;

procedure TqBitThread.Execute;
begin
  qBMainTh := qb.GetMainData(0); // Full server data update
  while not Terminated do
  begin
    var tme := GetTickCount;
    var U := qB.GetMainData(qBMainTh.Frid); // get differebtial data from last call
    qBMainTh.Merge(U); // Merge to qBMain to be uodate to date
    U.Free;
    Synchronize(
      procedure
      begin
        FrmSimpleThreaded.SyncThread(Self);
      end
    );
    while
      (GetTickCount - Tme < qBMainTh.Fserver_state.Frefresh_interval)
      and (not Terminated)
    do
      Sleep(100);
  end;
  qBMainTh.Free;
end;

procedure TFrmSimpleThreaded.FormShow(Sender: TObject);
begin
  if qBitSelectServerDlg.ShowModal = mrOk then
  begin
    var Server := qBitSelectServerDlg.GetServer;
    qB := TqBitObject.Connect( Server.FHP, Server.FUN, Server.FPW);
    Th := TqBitThread.Create;
    Th.qB := qB.Clone; // We clone qB for the Thread, in this demo this is not necessary
  end else
    PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TFrmSimpleThreaded.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not Assigned(qB) then Exit;
  Th.Terminate;
  Th.WaitFor;
  Th.Free;
  qB.Free;
end;

procedure TFrmSimpleThreaded.SyncThread(Sender: TqBitThread);
begin
  qBMain := Sender.qBMainTh;
  UpdateUI;
end;

procedure TFrmSimpleThreaded.UpdateUI;
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

end.
