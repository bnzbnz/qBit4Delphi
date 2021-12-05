unit uNOXMonitor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections, uqBitObject, uqBitAPI, uqBitAPITypes,
  Vcl.ExtCtrls, Vcl.Grids, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TqBitObjectEx = class(TqBitObject)
    M: TqBitMainDataType;
    P: TqBitPreferencesType;
    class function Connect(HostPath, Username, Password : string): TqBitObjectEx;
    destructor Destroy; override;
  end;

  TqBitThread = class(TThread)
  protected
    FHostPath, FUsername, FPassword : string;
    procedure Execute; override;
  public
    [volatile] qBit: TqBitObjectEx;
    Updt: TqBitMaindataType;
    constructor Create(HostPath, Username, Password: string); overload;
    destructor Destroy; override;
  end;

  TMonForm = class(TForm)
    Timer1: TTimer;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    StringGrid1: TStringGrid;
    Panel1: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ThreadList: TObjectList<TqBitThread>;
    procedure StartTimer;
    procedure UpdateThreadList;
  end;

var
  MonForm: TMonForm;

implementation
uses Math;

{$R *.dfm}

procedure TMonForm.StartTimer;
begin
  Timer1.Enabled := True;
  //Timer1Timer(self);
end;

procedure TMonForm.Timer1Timer(Sender: TObject);
var
  T: TqBitThread;
  M, U: TqBitMainDataType;
begin
  Timer1.Enabled := False;
  var TotalUpSpeed := 0.0;
  var TotalDlSpeed := 0.0;
  var AvgRatio := 0.0;
  if assigned(ThreadList) then
  for var i:=0 to ThreadList.Count-1 do
  begin
    T := ThreadList[i];
    if assigned(T.qBit)then
    begin
      M := T.qBit.M;
      U := T.Updt;

      var Traffic := (M.Fserver_state.Fdl_info_data + M.Fserver_state.Fup_info_data) / (1024 * 1024 * 1024);
      var Delta := M.Fserver_state.Fup_info_data / (1024 * 1024 * 1024) - M.Fserver_state.Fdl_info_data / (1024 * 1024 * 1024);
      var Efficiency := Delta / Traffic * 100;


      StringGrid1.Cells[0, 0] := 'Server';
      StringGrid1.Cells[0, i + 1] := T.qBit.FHostPath;
      Self.StringGrid1.Cells[0,  ThreadList.Count + 1] := 'Total / Avg';

      StringGrid1.Cells[1, 0] := 'Ratio';
      StringGrid1.Cells[1, i + 1] := Format('%.2f (%.2f)', [
           StrToFloat(M.Fserver_state.Fup_info_data / Max(Int64(M.Fserver_state.Fdl_info_data), 1)),
           StrToFloat(M.Fserver_state.Falltime_ul / M.Fserver_state.Falltime_dl)
        ]);
      AvgRatio := AvgRatio + (M.Fserver_state.Fup_info_data / Max(Int64(M.Fserver_state.Fdl_info_data), 1));
      Self.StringGrid1.Cells[1,  ThreadList.Count + 1] := Format('%.2f', [  Extended(AvgRatio / ThreadList.Count) ]);

      Self.StringGrid1.Cells[2, 0] := 'Traffic';
      Self.StringGrid1.Cells[2, i + 1] := Format('%.0f', [ StrToFloat(Traffic) ]);

      Self.StringGrid1.Cells[3, 0] := 'Delta';
      Self.StringGrid1.Cells[3, i + 1] := Format('%.0f', [ StrToFloat(Delta) ]);

      Self.StringGrid1.Cells[4, 0] := 'Efficiency';
      Self.StringGrid1.Cells[4, i + 1] := Format('%.2f%%', [ StrToFloat(Efficiency) ]);

      Self.StringGrid1.Cells[5, 0] := 'Disk MiB';
      Self.StringGrid1.Cells[5, i + 1] := Format('%.0f', [  StrToFloat(M.Fserver_state.Ffree_space_on_disk / (1024*1024*1024)) ]);

      Self.StringGrid1.Cells[6, 0] := 'Up MiB';
      Self.StringGrid1.Cells[6, i + 1] := Format('%.2f', [  StrToFloat(M.Fserver_state.Fup_info_speed / (1024*1024)) ]);
      TotalUpSpeed := TotalUpSpeed + M.Fserver_state.Fup_info_speed / (1024*1024);
      Self.StringGrid1.Cells[6,  ThreadList.Count + 1] := Format('%.2f', [  Extended(TotalUpSpeed) ]);

      Self.StringGrid1.Cells[7, 0] := 'Dl MiB';
      Self.StringGrid1.Cells[7, i + 1] := Format('%.2f', [  StrToFloat(M.Fserver_state.Fdl_info_speed / (1024*1024)) ]);
      TotalDlSpeed := TotalDlSpeed + M.Fserver_state.Fdl_info_speed / (1024*1024);
      Self.StringGrid1.Cells[7,  ThreadList.Count + 1] := Format('%.2f', [  Extended(TotalDlSpeed) ]);

      Self.StringGrid1.Cells[8, 0] := 'Cache Hits';
      Self.StringGrid1.Cells[8, i + 1] := Format('%.2f%%', [  StrToFloat(M.Fserver_state.Fread_cache_hits) ]);

      Self.StringGrid1.Cells[9, 0] := 'Torrents Count';
      Self.StringGrid1.Cells[9, i + 1] := Format('%d', [  M.Ftorrents.Count ]);
    end;
  end;
end;

procedure TMonForm.UpdateThreadList;
var
  Arr : TArray<string>;
begin
  FreeAndNil(ThreadList);
  ThreadList:= TObjectList<TqBitThread>.Create(True);
  for var c := 0 to Pred(StringGrid1.ColCount) do
    for var r := 0 to Pred(StringGrid1.RowCount) do
      StringGrid1.Cells[c, r] := '';
  for var i in Memo1.Lines do
  begin
     Arr := string(i).split(['|']);
     if Length(Arr) = 3 then
      ThreadList.Add(TqBitThread.Create(Trim(Arr[0]), Trim(Arr[1]), Trim(Arr[2])));
  end;
end;

procedure TMonForm.Button1Click(Sender: TObject);
begin
   UpdateThreadList;
end;

procedure TMonForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Memo1.Lines.SaveToFile('servers.txt');
  ThreadList.Free;
end;

procedure TMonForm.FormCreate(Sender: TObject);
var
  Th: TqBitThread;
begin
  if fileExists('servers.txt') then
    Memo1.Lines.LoadFromFile('servers.txt');
  UpdateThreadList;
end;

constructor TqBitThread.Create(HostPath, Username, Password: string);
begin
  inherited Create(False);
  FHostPath := HostPath;
  FUsername := Username;
  FPassword := Password;
end;

destructor TqBitThread.Destroy;
begin
  Terminate; 
  WaitFor;
  Updt.Free;
  qBit.Free;
  inherited;
end;

procedure TqBitThread.Execute;
begin
  qBit := TqBitObjectEx.Connect(FHostPath, FUsername, FPassword);
  if not assigned(qBit) then exit;
  while not terminated do
  begin
    Updt := qBit.GetMainData(qBit.M.Frid);
    if assigned(Updt) then
    begin
      qBit.M.Merge( Updt );
      Synchronize(MonForm.StartTimer);
      FreeAndNil(Updt);
    end;
    for var i := 1 to 10 do
    begin
      if Terminated then Exit;
      sleep(qBit.M.Fserver_state.Frefresh_interval div 10);
    end;
  end;
end;

class function TqBitObjectEx.Connect(HostPath, Username, Password: string): TqBitObjectEx;
begin
  Result := TqBitObjectEx.Create(HostPath);
  if not Result.Login(Username, Password) then
    FreeAndNil(Result)
  else begin
    Result.M := Result.GetMainData();
    Result.P := Result.GetPreferences();
  end;
end;

destructor TqBitObjectEx.Destroy;
begin
  P.Free;
  M.Free;
  inherited;
end;

end.
