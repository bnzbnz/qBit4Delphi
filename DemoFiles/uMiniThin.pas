unit uMiniThin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uqBitAPITypes, uqBitAPI, uqBitObject,
  Vcl.ExtCtrls, Vcl.Grids, Vcl.StdCtrls, uqBitFormat, uSelectServer, Vcl.Menus;

type

  TGridData = class
    // Master
    SortField: string;
    SortReverse: Boolean;
    // Col
    Width: Integer;
    Name: string;
    Field: string;
    Formater: TVarDataFormater;
    // Row
    Height: Integer;
    Hash: string;
  end;

  TMiniThinForm = class(TForm)
    Timer: TTimer;
    Panel1: TPanel;
    CBCat: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    CBTag: TComboBox;
    SG: TStringGrid;
    PMGrid: TPopupMenu;
    Pause: TMenuItem;
    Resume1: TMenuItem;
    PauseSelected: TMenuItem;
    All1: TMenuItem;
    ResumeSelected: TMenuItem;
    ResumeAll: TMenuItem;
    ToggleForceResume: TMenuItem;
    Warning: TMemo;
    N1: TMenuItem;
    Delete1: TMenuItem;
    DeleteTorrentOnly: TMenuItem;
    DeleteWithData: TMenuItem;
    N2: TMenuItem;
    SetLocation1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerTimer(Sender: TObject);
    procedure SGDblClick(Sender: TObject);
    procedure CBCatSelect(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure PauseSelectedClick(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure ResumeSelectedClick(Sender: TObject);
    procedure ResumeAllClick(Sender: TObject);
    procedure ForceResumeSelectedClick(Sender: TObject);
    procedure ToggleForceResumeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DeleteTorrentOnlyClick(Sender: TObject);
    procedure DeleteWithDataClick(Sender: TObject);
    procedure SGFixedCellClick(Sender: TObject; ACol, ARow: Integer);
    procedure SetLocation1Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
  public
    { Public declarations }
    qB: TqBitObject;
    M: TqBitMainDataType;

    function GetColData(Index: Integer): TGridData;
    function GetRowData(Index: Integer): TGridData;
    function GetSelectedTorrent: TqBitTorrentType;
    function GetMasterData: TGridData;
    procedure UpdateUI(Init : Boolean);
    procedure SelRow(Row: integer);
    procedure AddCol(Index: Integer; Name, Field: string; Fmt: TVarDataFormater; Width: integer);
  end;

  TMyStringGrid = class(TStringGrid);

var
  MiniThinForm: TMiniThinForm;

implementation
uses System.Generics.Collections,  System.Generics.Defaults, ShellAPI, RTTI,
  uSetLocation;

{$R *.dfm}

procedure TMiniThinForm.AddCol(Index: Integer; Name, Field: string; Fmt: TVarDataFormater; Width: integer);
begin
  var CD := GetColData(Index);
  CD.Name := Name;
  CD.Field := Field;
  CD.Formater := Fmt;
  SG.Cells[Index, 0] := Name;
  SG.ColWidths[Index] := Width;
  SG.RowHeights[Index] := 24;
end;

procedure TMiniThinForm.FormShow(Sender: TObject);
begin
  if SelectServerDlg.ShowModal = mrCancel then
  begin
    Close;
    Exit;
  end;
  var Srv := SelectServerDlg.GetServer;
  qB := TqBitObject.Connect(Srv.FHP, Srv.FUN, Srv.FPW);
  if not assigned(qB) then Exit;
  M := qB.GetMainData;

  SG.ColCount := 100;
  SG.RowCount := 1000;
  SG.FixedCols := 2;
  SG.FixedRows := 1;

  var GD := TGridData.Create;
  GD.Formater := VarFormatString;
  GD.SortField := 'Fname';
  GD.SortReverse := False;

  GD.Width := -1;
  SG.ColWidths[0] := GD.Width;
  SG.Objects[0, 0] := GD;

  for var i:=1 to SG.ColCount - 1 do
  begin
    GD := TGridData.Create;
    GD.Formater := nil;
    GD.Width := -1;
    SG.ColWidths[i] := GD.Width;
    SG.Objects[i, 0] := GD;
  end;

  for var i:=1 to SG.RowCount - 1 do
  begin
    GD := TGridData.Create;
    GD.Formater := nil;
    GD.Height := -1;
    SG.RowHeights[i] := GD.Height;
    SG.Objects[0, i] := GD;
  end;

  var Row := 0;
  Inc(Row); AddCol(Row, 'Name', 'Fname', VarFormatString, 240);
  Inc(Row); AddCol(Row, 'Size', 'Fsize', VarFormatBKM, 84);
  Inc(Row); AddCol(Row, 'Progress', 'Fprogress', VarFormatPercent, 84);
  Inc(Row); AddCol(Row, 'Status', 'Fstate', VarFormatString, 84);
  Inc(Row); AddCol(Row, 'Seeds', 'Fnum_seeds', VarFormatString, 84);
  Inc(Row); AddCol(Row, 'Peers', 'Fnum_leechs', VarFormatString, 84);
  Inc(Row); AddCol(Row, 'Down Speed', 'Fdlspeed', VarFormatBKMPerSec, 84);
  Inc(Row); AddCol(Row, 'Upload Speed', 'Fupspeed', VarFormatBKMPerSec, 84);
  Inc(Row); AddCol(Row, 'ETA', 'Feta', VarFormatDeltaSec, 84);
  Inc(Row); AddCol(Row, 'Ratio', 'Fratio', VarFormatFloat2d, 84);
  Inc(Row); AddCol(Row, 'Category', 'Fcategory', VarFormatString, 84);
  Inc(Row); AddCol(Row, 'Tags', 'Ftags', VarFormatString, 84);
  Inc(Row); AddCol(Row, 'Added On', 'Fadded_on', VarFormatDate, 84);
  Inc(Row); AddCol(Row, 'Availability', 'Favailability', VarFormatMulti, 84);
  {
  var rttictx := TRttiContext.Create();
  var rttitype := rttictx.GetType(TqBitTorrentType);
  for var field in rttitype.GetFields do
  begin
    var Title := Uppercase(Copy(field.Name, 2, 1)) + Copy(field.Name, 3, 9999);
    Inc(Row); AddCol(Row, Title, field.Name, VarFormatString, 64);
  end;
  rttictx.Free;
  }

  UpdateUI(True);
  SelRow(2);
  SelRow(1);
  Timer.Interval := M.Fserver_state.Frefresh_interval;
  Timer.Enabled := True;
end;

procedure TMiniThinForm.PauseSelectedClick(Sender: TObject);
begin
  var T := GetSelectedTorrent;
  if assigned(T) then
    qB.PauseTorrents(T.Fhash);
end;

procedure TMiniThinForm.SelRow(Row: integer);
begin
  if (SG.Row = Row) or (Row < 1) then Exit;
  var R : TGridRect;
  R.Left := 0;
  R.Top := Row;
  R.Bottom := Row;
  R.Right := SG.ColCount - 1;
  SG.Selection := R;
  Invalidate;
end;

procedure TMiniThinForm.SGSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  SelRow(aRow);
  CanSelect := False;
end;

procedure TMiniThinForm.SGDblClick(Sender: TObject);
var
  P : TPoint;
  ACol, ARow : integer;
begin
  GetCursorPos(P) ;
  SG.MouseToCell(SG.ScreenToClient(P).X, SG.ScreenToClient(P).Y, ACol, ARow);
  if aCol < 0 then Exit;
  var Current := Self.GetMasterData.SortField;
  var Field :=  Self.GetColData(ACol).Field;
  if Current <> field then
  begin
    Self.GetMasterData.SortField := Field;
    Self.GetMasterData.SortReverse := False;
    Self.UpdateUI(False);
  end else
    Self.GetMasterData.SortReverse := not Self.GetMasterData.SortReverse;
  UpdateUI(False);
end;

procedure TMiniThinForm.SGFixedCellClick(Sender: TObject; ACol, ARow: Integer);
begin
//
end;

procedure TMiniThinForm.SGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  aCol, ARow: integer;
begin
  SG.MouseToCell(X, Y, ACol, ARow);
  //SelRow(ARow);
end;

procedure AssignFormater(GD: TGridData; Formater: TVarDataFormater; const Fields: array of string);
begin
  for var i := Low(Fields) to High(Fields) do
    if GD.Field = Fields[i] then
    begin
      GD.Formater := Formater;
      break;
    end;
end;

procedure TMiniThinForm.UpdateUI(Init: Boolean);
begin
  //LockWindowUpdate(SG.Handle);
  Timer.Enabled := False;

  var SelHash := GetRowData(SG.Row).Hash;
  var RttiCtx := TRttiContext.Create();
  var RttiType := RttiCtx.GetType(TqBitTorrentType);

  var TL := TObjectList<TqBitTorrentType>.Create(False);
  for var T in M.Ftorrents do
  begin
    TqBitTorrentType(T.Value).Fhash := T.Key;
    TL.Add(TqBitTorrentType(T.Value));
  end;

  // Filtering

  for var i := TL.Count - 1 downto 0 do
  begin
    var ToDelete := False;
    if CBCat.ItemIndex > 0  then
    begin
      if (CBCat.ItemIndex = 1)
          and (TqBitTorrentType(TL[i]).Fcategory <> '')
      then
        ToDelete := True;
      if (CBCat.ItemIndex > 1)
          and (TqBitTorrentType(TL[i]).Fcategory <> CBCat.Items[CBCat.ItemIndex])
      then
        ToDelete := True;
     end;
     if CBTag.ItemIndex > 0  then
     begin
      var Tag := TqBitTorrentType(TL[i]).Ftags;
      if (CBTag.ItemIndex = 1) then
          if (TqBitTorrentType(TL[i]).Ftags <> '')
      then
        ToDelete := True;
      if (CBTag.ItemIndex > 1)
          and (Pos(CBTag.Items[CBTag.ItemIndex], TqBitTorrentType(TL[i]).Ftags) = 0)
      then
        ToDelete := True;
     end;
     if ToDelete then TL.Delete(i);
  end;

  // Sorting

  TL.Sort(TComparer<TqBitTorrentType>.Construct(
      function (const L, R: TqBitTorrentType): integer
      begin
        var MD := GetMasterData;
        for var Field in RttiType.GetFields do
        begin
          if Field.Name = MD.SortField then
            begin
              var LVal := Field.GetValue(L).asVariant;
              var RVal := Field.GetValue(R).asVariant;
              Result := 0;
              if LVal > RVal then
                if MD.SortReverse then Result := -1 else Result := 1;
              if RVal > LVal then
                if MD.SortReverse then Result := 1 else Result := -1;
           end;
        end;
      end
  ));

  // Displaying

  for var Col := 1 to SG.RowCount - 1 do
  begin
    var GD := Self.GetColData(Col);
    if assigned(GD) then
    begin
      if GetColData(Col).Field = GetMasterData.SortField then
      begin
        if not GetMasterData.SortReverse then
          SG.Cells[Col, 0] := GetColData(Col).Name + ' 🡻'
        else
          SG.Cells[Col, 0] := GetColData(Col).Name + ' 🡹';
      end else
        SG.Cells[Col, 0] := GetColData(Col).Name;
      var Row := 1;
      for var T in TL do
      begin
        GetRowData(Row).Hash := T.Fhash;

        if T.Fhash = SelHash then
           SelRow(Row);
        for var Field in RttiType.GetFields do
          if  Field.Name = GD.Field then
          begin
            SG.Cells[Col, Row] := GD.Formater( Field.GetValue(T).asVariant, nil);
            SG.RowHeights[Row] := 24;
            break;
          end;
        Inc(Row);
      end;
    end;
  end;

  // Caption

  Caption := 'qBitMiniThin : ' + qb.HostPath + ' : ' + IntToStr(M.Ftorrents.Count);

  // Categories
  if M.Fcategories_changed or Init then
  begin
    CBCat.Items.Clear;
    CBCat.Items.Add('All');
    CBCat.Items.Add('Unassigned');
    for var C in M.Fcategories do
      CBCat.Items.Add(C.Key);
    CBCat.ItemIndex := 0;
  end;

  // Tags

  if M.Ftags_changed or Init then
  begin
    CBTag.Items.Clear;
    CBTag.Items.Add('All');
    CBTag.Items.Add('Unassigned');
    for var C in M.Ftags do
      CBTag.Items.Add(C);
    CBTag.ItemIndex := 0;
  end;
  // Row Cleaning

  for var i := TL.count+1 to SG.ColCount - 1  do
    if SG.RowHeights[i] > -1 then SG.RowHeights[i] := -1;

  TL.Free;
  RttiCtx.Free;

  Timer.Enabled := True;
  LockWindowUpdate(0);
end;

procedure TMiniThinForm.WMDropFiles(var Msg: TMessage);
var
  hDrop: THandle;
  FileName: string;
  NewTorrentFile : TqBitNewTorrentFileType;
begin
  hDrop:= Msg.wParam;
  var FileCount := DragQueryFile (hDrop , $FFFFFFFF, nil, 0);
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

function TMiniThinForm.GetColData(Index: Integer): TGridData;
begin
  Result := TGridData(SG.Objects[Index, 0]);
end;

function TMiniThinForm.GetMasterData: TGridData;
begin
  Result := TGridData(SG.Objects[0, 0]);
end;

function TMiniThinForm.GetRowData(Index: Integer): TGridData;
begin
  Result := TGridData(SG.Objects[0, Index]);
end;

function TMiniThinForm.GetSelectedTorrent: TqBitTorrentType;
var
  Res : TqBitTorrentBaseType;
begin
  Result := Nil;
  if SG.Row <1 then Exit;
  M.Ftorrents.TryGetValue(GetRowData(SG.Row).Hash, Res);
  Result := TqBitTorrentType(Res);
end;

procedure TMiniThinForm.ResumeAllClick(Sender: TObject);
begin
  var LS := TStringList.Create;
  for var T in M.Ftorrents do
    LS.Add(T.Key);
  qB.ResumeTorrents(LS);
  LS.Free;
  UpdateUI(False);
end;

procedure TMiniThinForm.ResumeSelectedClick(Sender: TObject);
begin
  var T := GetSelectedTorrent;
  if assigned(T) then
    qB.ResumeTorrents(T.Fhash);
  UpdateUI(False);
end;

procedure TMiniThinForm.ForceResumeSelectedClick(Sender: TObject);
begin
  var LS := TStringList.Create;
  for var T in M.Ftorrents do
    LS.Add(T.Key);
  qB.PauseTorrents(LS);
  LS.Free;
  UpdateUI(False);
end;

procedure TMiniThinForm.TimerTimer(Sender: TObject);
begin
  var U := qB.GetMainData(M.Frid);
  M.Merge(U);
  U.Free;
  UpdateUI(False);
end;

procedure TMiniThinForm.ToggleForceResumeClick(Sender: TObject);
begin
 var T := GetSelectedTorrent;
 if assigned(T) then
   qB.SetForceStart(T.Fhash, T.Fforce_start and False);
 UpdateUI(False);
end;

procedure TMiniThinForm.All1Click(Sender: TObject);
begin
  var LS := TStringList.Create;
  for var T in M.Ftorrents do
    LS.Add(T.Key);
  qB.PauseTorrents(LS);
  LS.Free;
  UpdateUI(False);
end;

procedure TMiniThinForm.SetLocation1Click(Sender: TObject);
begin
  var T := GetSelectedTorrent;
  if not assigned(T) then Exit;

  SetLocationDlg.Location.Text := T.Fsave_path;
  if SetLocationDlg.ShowModal = mrOk then
     qB.SetTorrentLocation(T.Fhash, SetLocationDlg.Location.Text);
end;

procedure TMiniThinForm.CBCatSelect(Sender: TObject);
begin
  UpdateUI(False);
end;

procedure TMiniThinForm.DeleteTorrentOnlyClick(Sender: TObject);
begin
  var T := GetSelectedTorrent;
  if assigned(T) then
    qB.DeleteTorrents(T.Fhash, False);
  UpdateUI(False);
end;

procedure TMiniThinForm.DeleteWithDataClick(Sender: TObject);
begin
  var T := GetSelectedTorrent;
  if assigned(T) then
    qB.DeleteTorrents(T.Fhash, True);
  UpdateUI(False);
end;

procedure TMiniThinForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SG.Objects[0, 0].Free;
  for var i:=1 to SG.ColCount - 1 do
    SG.Objects[i, 0].Free;
  for var i:=1 to SG.RowCount - 1 do
     SG.Objects[0, i].Free;
  M.Free;
  qB.Free;
end;

procedure TMiniThinForm.FormCreate(Sender: TObject);
begin
  Warning.Visible := False;
  DragAcceptFiles ( self.handle, True );
end;

end.
