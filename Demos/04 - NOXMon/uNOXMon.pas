unit uNOXMon;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Generics.Collections,
  uqBitFormat, uqBitAPITypes, uqBitAPI, uqBitObject, Vcl.Grids;

type

  TqBitThread = class(TThread)
    qB: TqBitObject;
    qBMainTh: TqBitMainDataType;
    RowIndex: integer;
    procedure Execute; override;
  end;

  TNOXMonDlg = class(TForm)
    SG: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    ThList: TObjectList<TqBitThread>;
    procedure SyncThread(Sender: TqBitThread);
    procedure UpdateHeaders;
    procedure UpdateRow(Thread: TqBitThread);
  end;

var
  NOXMonDlg: TNOXMonDlg;

implementation
uses uPatcherChecker, uSelectServer, Math;
{$R *.dfm}

procedure TNOXMonDlg.FormShow(Sender: TObject);
var
  Srvs: TObjectList<TqBitServer>;
  Th : TqBitThread;
begin
  SelectServerDlg.MultiSelect := True;
  if SelectServerDlg.ShowModal = mrOk then
  begin
    UpdateHeaders;
    ThList := TObjectList<TqBitThread>.Create(False);
    Srvs :=  SelectServerDlg.GetMultiServers;
    var RowIndex := 1;
    for var Srv in Srvs do
    begin
      Th := TqBitThread.Create(True);
      Th.RowIndex := RowIndex;
      Inc(RowIndex);
      Th.qB := TqBitObject.Connect(Srv.FHP, Srv.FUN, Srv.FPW);
      ThList.Add(Th);
      Th.Resume;
    end;
    Srvs.Free;
  end;
end;

procedure TNOXMonDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  for var Th in ThList do
  begin
    TH.Terminate;
    Th.WaitFor;
    Th.Free;
  end;
  ThList.Free;
end;

procedure TNOXMonDlg.SyncThread(Sender: TqBitThread);
begin
  UpdateRow(Sender);
end;

procedure TNOXMonDlg.UpdateHeaders;
begin
  var C := -1;
  Inc(C); SG.Cells[C, 0] := 'Host :'; SG.ColWidths[C]:=180;
  Inc(C); SG.Cells[C, 0] := 'State :'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Ratio (Session/AllTime) :'; SG.ColWidths[C]:=130;
  Inc(C); SG.Cells[C, 0] := 'Downloaded:'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Uploaded :'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Traffic :'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Delta :'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Efficiency :'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Down Speed :'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Up Speed :'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Cache Hits :'; SG.ColWidths[C]:=80;
  Inc(C); SG.Cells[C, 0] := 'Free disk :'; SG.ColWidths[C]:=80;

end;

procedure TNOXMonDlg.UpdateRow(Thread: TqBitThread);
begin
  Var Q := Thread.qB;
  var M := Thread.qBMainTh;

  var Traffic := M.Fserver_state.Fdl_info_data + M.Fserver_state.Fup_info_data;
  var Delta := Abs( M.Fserver_state.Fup_info_data - M.Fserver_state.Fdl_info_data);
  var Efficiency := Delta / Traffic; ;

  var C := -1;
  Inc(C); SG.Cells[C, Thread.RowIndex] := Q.HostPath;
  Inc(C); SG.Cells[C, Thread.RowIndex] := M.Fserver_state.Fconnection_status;
  Inc(C); SG.Cells[C, Thread.RowIndex] :=
    Format('%.2f (%.2f)', [
      StrToFloat(M.Fserver_state.Fup_info_data / Max(Int64(M.Fserver_state.Fdl_info_data), 1)),
      StrToFloat(M.Fserver_state.Falltime_ul / M.Fserver_state.Falltime_dl)
    ]);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatBKM(M.Fserver_state.Fdl_info_data);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatBKM(M.Fserver_state.Fup_info_data);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatBKM(Traffic);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatBKM(Delta);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatPercent(Efficiency);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatBKMperSec(M.Fserver_state.Fdl_info_speed);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatBKMperSec(M.Fserver_state.Fup_info_speed);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatPercent(M.Fserver_state.Fread_cache_hits / 100);
  Inc(C); SG.Cells[C, Thread.RowIndex] := VarFormatBKM(M.Fserver_state.Ffree_space_on_disk);
end;

{ TqBitThread }

procedure TqBitThread.Execute;
begin
  qBMainTh := qB.GetMainData(0); // Full server data update
  while not Terminated do
  begin
    var tme := GetTickCount;
    var U := qB.GetMainData(qBMainTh.Frid); // get differebtial data from last call
    qBMainTh.Merge(U); // Merge to qBMain to be uodated to date
    U.Free;
    Synchronize(
      procedure
      begin
        NOXMonDlg.SyncThread(Self);
      end
    );
    while
      (GetTickCount - Tme < qBMainTh.Fserver_state.Frefresh_interval)
      and (not Terminated)
    do
      Sleep(100);
  end;
  qBMainTh.Free;
  qB.Free;
end;

initialization
  PatcherChecker; // Check if JSON Libs have been patched
end.
