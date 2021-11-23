unit uDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask,
  Vcl.ExtCtrls, Vcl.ComCtrls, uqBitObject, uqBitAPI, uqBitAPITypes, Vcl.Grids;

type

  TqBitObjectEx = class(TqBitObject)
    M: TqBitMainDataType;
    P: TqBitPreferencesType;
    class function Connect(HostPath, Username, Password : string): TqBitObjectEx;
    destructor Destroy; override;
  end;

  TForm2 = class(TForm)
    Panel1: TPanel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    Button1: TButton;
    Timer1: TTimer;
    PageControl1: TPageControl;
    Server: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Torrents: TTabSheet;
    StringGrid1: TStringGrid;
    Panel2: TPanel;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    Button4: TButton;
    Label13: TLabel;
    Label14: TLabel;
    Button5: TButton;
    Memo2: TMemo;
    Memo3: TMemo;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
  public
    { Public declarations }
    qB : TqBitObjectEx;
    procedure UpdateUI(ForcedRefresh: boolean = False);
  end;

var
  Form2: TForm2;

implementation
uses DateUtils, ShellAPI, Math;
{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  if assigned(qB) then
  begin
    qB.Logout;
    FreeAndNil(qB);
  end;
  qB := TqBitObjectEx.Connect(
          Self.LabeledEdit1.Text,
          Self.LabeledEdit2.Text,
          Self.LabeledEdit3.Text
        );
  if assigned(qB) then
  begin
    Button1.Enabled := False;

    var logs := qB.GetLog();
    var cnt := 0;
    LockWindowUpdate(Memo1.Handle);
    for var log in logs.Flogs do
    begin
      Memo1.Lines.insert(0, Log.Fmessage);
      Memo1.Lines.insert(0, DateTimeToStr(TqBitObject.TStoDT(Log.Ftimestamp)) + ' :');
      Inc(cnt);
      if cnt > 50 then Break;
    end;
    LockWindowUpdate(0);
    logs.Free;

    Timer1.Interval := Qb.M.Fserver_state.Frefresh_interval;
    Caption := Format('%s : Version %s - API %s', [Self.LabeledEdit1.Text, qB.GetVersion, qB.GetAPIVersion]);
    UpdateUI(True); //First Update, UI forced repaint
    Timer1Timer(Self);
    PageControl1.Enabled := True;
  end else
    ShowMessage('Can''t connect to :' + Self.LabeledEdit1.Text);
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  NewTorrentFile: TqBitNewTorrentFileType;
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
    dlg.Filter := 'Torrent files (*.torrent)|*.torrent';
    if dlg.Execute(Handle) then
    begin
      NewTorrentFile := TqBitNewTorrentFileType.Create;
      NewTorrentFile.Ffilename := Dlg.FileName;
      if qB.AddNewTorrentFile(NewTorrentFile) then
        ShowMessage('Torrent File added Successfully');
      NewTorrentFile.Free;
    end;
  Dlg.Free;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  q: TqBitTorrentBaseType;
begin
   var hash := StringGrid1.Cells[0, StringGrid1.Row];
   if qB.M.Ftorrents.TryGetValue(hash, q) then
    if string(TqBitTorrentType(q).Fstate).StartsWith('pause') then
      qB.ResumeTorrents(hash)
    else
      qB.PauseTorrents(hash);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  var hash := StringGrid1.Cells[0, StringGrid1.Row];
  qB.DeleteTorrents(hash, True);
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  Timer1.Enabled := False;
  PageControl1.Enabled := False;
  Button1.Enabled := True;
  qB.Shutdown;
  FreeAndNil(qB);
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  var MagnetURL := VCL.Dialogs.InputBox('Magnet URL :', '', '');
  if MAgnetURL <> '' then
  begin
    var NewMag := TqBitNewTorrentUrlsType.Create;
    NewMag.Furls.Add(MagnetURL);
    qB.AddNewTorrentUrls(NewMag);
    NewMag.Free;
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned(qB) then
  begin
    qB.Logout;
    qB.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  qB := nil;
  DragAcceptFiles(Self.Handle, True);
  PageControl1.ActivePageIndex := 0;
  Memo2.Hide;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, False);
end;

procedure TForm2.WMDropFiles(var Msg: TMessage);
var
  hDrop: THandle;
  FileName: string;
  NewTorrentFile : TqBitNewTorrentFileType;
begin
  hDrop:= Msg.wParam;
  var FileCount:= DragQueryFile (hDrop , $FFFFFFFF, nil, 0);
  NewTorrentFile := TqBitNewTorrentFileType.Create;
  for var i := 0 to FileCount - 1 do
  begin
    var namelen := DragQueryFile(hDrop, I, nil, 0) + 1;
    SetLength(FileName, namelen);
    DragQueryFile(hDrop, I, Pointer(FileName), namelen);
    SetLength(FileName, namelen - 1);
    NewTorrentFile.Ffilename := FileName;
    if assigned(qB) then
      qb.AddNewTorrentFile(NewTorrentFile);
  end;
  NewTorrentFile.Free;
  DragFinish(hDrop);
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  UpdateUI;
  var TmpMain := qB.GetMainData(qB.M.Frid);
  if TmpMain <> nil then
          qB.M.Merge( TmpMain );
  TmpMain.Free;
  Timer1.Enabled := True;
end;

procedure TForm2.UpdateUI(ForcedRefresh: boolean);
var
  Torrent: TqBitTorrentBaseType;
begin

  if not assigned(qB) then exit;
  var M := qB.M;

  var TrafficUp := M.Fserver_state.Fup_info_data;
  var TrafficDl := M.Fserver_state.Fdl_info_data;
  var TotalTraffic := M.Fserver_state.Fdl_info_data + M.Fserver_state.Fup_info_data;

  var Delta := TrafficUp - TrafficDl;
  var Efficiency := 0;
  if TotalTraffic > 0 then Efficiency := (Delta / TotalTraffic) * 100;

  Label1.Caption := Format('Traffic DL : %.4f MiB',[StrToFloat(TrafficDl / (1024 * 1024 * 1024) )]);
  Label2.Caption := Format('Traffic UP : %.4f MiB',[StrToFloat(TrafficUp / (1024 * 1024 * 1024) )]);
  Label3.Caption := Format('Total Traffic (Up+Dl) : %.2f MiB',[StrToFloat(TotalTraffic / (1024 * 1024 * 1024) )]);
  Label4.Caption := Format('Delta (Gain) : %.2f MiB',[StrToFloat(Delta / (1024 * 1024 * 1024) )]);
  Label5.Caption := Format('Efficiency : %.2f %%',[real( Efficiency )]);
  Label6.Caption :=  Format('DL : %.2f KiB/s', [  StrToFloat(M.Fserver_state.Fdl_info_speed / (1024)) ]);
  Label7.Caption :=  Format('UL : %.2f KiB/s', [  StrToFloat(M.Fserver_state.Fup_info_speed / (1024)) ]);
  Label8.Caption := M.Fserver_state.Fconnection_status;
  Label9.Caption :=  Format('Disk Space : %.2f MiB', [ StrToFloat(M.Fserver_state.Ffree_space_on_disk / (1024*1024*1024)) ]);
  Label10.Caption :=  Format('DHT Nodes : %d', [ StrToInt(M.Fserver_state.Fdht_nodes) ]);
  Label11.Caption :=  Format('Torrents Count : %d', [ M.Ftorrents.Count ]);
  var TorrentSize := int64(0);

  if ForcedRefresh or( M.Ftorrents_added<>nil) or (M.Ftorrents_removed<>nil) then
  begin
    StringGrid1.ColWidths[0] := 0;
    StringGrid1.Cells[0,0] := 'Hash :';
    StringGrid1.Cells[1,0] := 'Name :';
    StringGrid1.Cells[2,0] := 'Progress :';
    StringGrid1.Cells[3,0] := 'Dlownload speed : ';
    StringGrid1.Cells[4,0] := 'Upload speed :';
    StringGrid1.Cells[5,0] := 'Seeds :';
    StringGrid1.Cells[6,0] := 'Perrs :';
    StringGrid1.Cells[7,0] := 'State :';
    var RowIndex := 1;
    for var t in  M.Ftorrents do
    begin
      StringGrid1.Cells[0,RowIndex] := T.Key;
      StringGrid1.Cells[1,RowIndex] := TqBitTorrentType(t.Value).Fname;
      StringGrid1.Cells[2,RowIndex] := Format('%.2f%%', [ StrToFloat(TqBitTorrentType(t.Value).Fprogress * 100)]);
      StringGrid1.Cells[3,RowIndex] := TqBitTorrentType(t.Value).Fnum_seeds;
      StringGrid1.Cells[4,RowIndex] := TqBitTorrentType(t.Value).Fnum_leechs;
      StringGrid1.Cells[5,RowIndex] :=TqBitTorrentType(t.Value).Fstate;
      Inc(RowIndex);
    end;
    StringGrid1.RowCount := RowIndex;
  end else begin
    if M.Ftorrents_changed then
    begin
      for var i := 1 to StringGrid1.RowCount - 1 do
      begin
        if M.Ftorrents.TryGetValue(StringGrid1.Cells[0, i], Torrent) then
        begin
          StringGrid1.Cells[0,i] := StringGrid1.Cells[0, i];
          StringGrid1.Cells[1,i] := TqBitTorrentType(Torrent).Fname;
          StringGrid1.Cells[2,i] := Format('%.2f%%', [ StrToFloat(TqBitTorrentType(Torrent).Fprogress * 100)]);
          StringGrid1.Cells[3,i] := Format('%.2f KiB/s', [ StrToFloat(TqBitTorrentType(Torrent).Fdlspeed / 1024)]);
          StringGrid1.Cells[4,i] := Format('%.2f KiB/s', [ StrToFloat(TqBitTorrentType(Torrent).Fupspeed / 1024)]);
          StringGrid1.Cells[5,i] := TqBitTorrentType(Torrent).Fnum_seeds;
          StringGrid1.Cells[6,i] := TqBitTorrentType(Torrent).Fnum_leechs;
          StringGrid1.Cells[7,i] := TqBitTorrentType(Torrent).Fstate;
        end;
      end;

    end;
  end;

  for var t in  M.Ftorrents do
  begin
    TorrentSize := TorrentSize + TqBitTorrentType(t.Value).Fsize;
  end;
  Label12.Caption :=  Format('Torrents Total Size : %.2f GiB', [ TorrentSize / (1024 * 1024*1024) ]);
end;

{ TqBitObjectEx }

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
