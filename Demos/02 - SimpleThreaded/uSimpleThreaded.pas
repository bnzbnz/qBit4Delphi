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
    procedure Disconnected(Sender: TqBitThread);
    procedure UpdateUI;
  end;
var
  FrmSimpleThreaded: TFrmSimpleThreaded;
implementation
{$R *.dfm}
uses uqBitSelectServerDlg, uqBitFormat;
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
    if U = Nil then
    begin
      Synchronize(
        procedure
        begin
          FrmSimpleThreaded.Disconnected(Self);
        end
      );
      Terminate;
    end else begin
      qBMainTh.Merge(U); // Merge to qBMain to be uodate to date
      U.Free;
      Synchronize(
        procedure
        begin
          FrmSimpleThreaded.SyncThread(Self);
        end
      );
    end;
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
    qB := TqBitObject.Connect(Server.FHP, Server.FUN, Server.FPW);
    Th := TqBitThread.Create;
    Th.qB := qB.Clone; // We clone qB for the Thread (safe)
  end else Close;
end;
procedure TFrmSimpleThreaded.Disconnected(Sender: TqBitThread);
begin
  LBTorrents.Clear;
  LBTorrents.Items.Add('Disconnected...');
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
  caption := Sender.qB.Duration.ToString;
  UpdateUI;
end;
procedure TFrmSimpleThreaded.UpdateUI;
begin
  ////////////////  Few Properties...
  LBTorrents.Clear;
  for var T in qBMAin.Ftorrents do
      LBTorrents.Items.Add( TqBitTorrentType(T.Value).Fname + ' / ' + TqBitTorrentType(T.Value).Fstate);
end;
end.
