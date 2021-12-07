unit uqNOXify;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uqBitAPITypes, uqBitAPI, uqBitObject,
  Vcl.ExtCtrls, Vcl.Grids, Vcl.StdCtrls, uqBitFormat, uSelectServer, Vcl.Menus,
  Vcl.ComCtrls, System.UITypes, Vcl.CheckLst, uAppTrackMenus,
  System.Generics.Collections,  System.Generics.Defaults, uAddEditCat,
  Vcl.Buttons;

const
  MAXCOL = 100;
  MAXROW = 2000;
  ROWHEIGHT = 18;

type
  TGridData = class
    // Cols
    Name: string;
    Field: string;
    Formater: TVarDataFormater;
    Width: integer;
    Visible: Boolean;
    // Rows
    Selected: Boolean;
    Hash: string;
    // Global
    LastSelected: integer;
    SortField: string;
    SortReverse: Boolean;
    HintX: integer;
    HintY: integer;
  end;

  TqNOXifyFrm = class(TForm)
    Timer: TTimer;
    PMHdrCol: TPopupMenu;
    Hide: TMenuItem;
    ShowAll: TMenuItem;
    PMIShowHide: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    PMISortCol: TMenuItem;
    PMCol: TPopupMenu;
    PMIPause: TMenuItem;
    PMIResume: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    PMIDeleteTorrent: TMenuItem;
    PMIDeleteData: TMenuItem;
    StatusBar: TStatusBar;
    PMStatus: TPopupMenu;
    PMIToggleSpeedLimits: TMenuItem;
    Panel1: TPanel;
    SG: TStringGrid;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    N5: TMenuItem;
    SetLocation1: TMenuItem;
    Rename1: TMenuItem;
    PMIEditTrackers: TMenuItem;
    N6: TMenuItem;
    PMICategory: TMenuItem;
    PMITags: TMenuItem;
    PMISpeedLimits: TMenuItem;
    N7: TMenuItem;
    New1: TMenuItem;
    Reset1: TMenuItem;
    Reset2: TMenuItem;
    AAAA1: TMenuItem;
    BBB1: TMenuItem;
    Assign1: TMenuItem;
    Delete1: TMenuItem;
    Warning: TMemo;
    PMICatReset: TMenuItem;
    N8: TMenuItem;
    New2: TMenuItem;
    N9: TMenuItem;
    orrentManagement1: TMenuItem;
    Enable1: TMenuItem;
    Disable1: TMenuItem;
    EditSearch: TEdit;
    Label1: TLabel;
    CBTag: TComboBox;
    CBCat: TComboBox;
    CBStatus: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn1: TBitBtn;
    N10: TMenuItem;
    ForceRecheck1: TMenuItem;
    ForceReannounce1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure SGDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure SGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure SGMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure SGDblClick(Sender: TObject);
    procedure HideClick(Sender: TObject);
    procedure ShowAllClick(Sender: TObject);
    procedure SGMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PMHdrColPopup(Sender: TObject);
    procedure PMISortColClick(Sender: TObject);
    procedure PMIDeleteTorrentClick(Sender: TObject);
    procedure PMIDeleteDataClick(Sender: TObject);
    procedure StatusBarContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure PMIToggleSpeedLimitsClick(Sender: TObject);
    procedure CBStatusSelect(Sender: TObject);
    procedure SetLocation1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PMISpeedLimitsClick(Sender: TObject);
    procedure StatusBarClick(Sender: TObject);
    procedure SGKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Reset1Click(Sender: TObject);
    procedure PMColPopup(Sender: TObject);
    procedure AAAA1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure PMIResumeClick(Sender: TObject);
    procedure PMIPauseClick(Sender: TObject);
    procedure PMICatResetClick(Sender: TObject);
    procedure New2Click(Sender: TObject);
    procedure Enable1Click(Sender: TObject);
    procedure Disable1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ForceRecheck1Click(Sender: TObject);
    procedure ForceReannounce1Click(Sender: TObject);
  private
    procedure TrackMenuNotifyHandler(Sender: TMenu; Item: TMenuItem; var CanClose: Boolean);
    //function GetLastGridHashe: string;
  protected
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WndProc(var Msg: TMessage); override;
  public
    qB: TqBitObject;
    qBMain: TqBitMainDataType;
    qBTInfo: TqBitTorrentInfoType;
    qBPrefs: TqBitPreferencesType;
    function GetColData(Index: Integer): TGridData;
    function GetRowData(Index: Integer): TGridData;

    function GetSelectedGridHashes: TStringList;
    function GetAllGridHashes: TStringList;
    function GetAllServerHashes: TStringList;
    function GetLastGridHash: string;

    function GetSelectedTorrents: TObjectList<TqBitTorrentType>;
    function GetLastSelectedTorrent: TqBitTorrentType;

    procedure AddCol(Index: Integer; Name, Field: string; Fmt: TVarDataFormater; Width: Integer; Visible: boolean);
    procedure HideCol(ACol: integer);
    procedure ShowCol(ACol: integer);
    procedure ShowAllCol;
    procedure UpdateUI;
    procedure ShowHideItemClicked(Sender: TObject);
    procedure CatItemClicked(Sender: TObject);
    procedure SetCategoryClicked(Sender : TObject);
    procedure DeleteCategoryClicked(Sender : TObject);
    procedure ToggleSortCol(ACol: integer);
  end;

var
  qNOXifyFrm: TqNOXifyFrm;

implementation
uses Math, ShellAPI, RTTI, uSetLocation, uSpeedLimitsDlg;

{$R *.dfm}

function TqNOXifyFrm.GetColData(Index: Integer): TGridData;
begin
  Result := TGridData(SG.Objects[Index, 0]);
end;

function TqNOXifyFrm.GetRowData(Index: Integer): TGridData;
begin
  Result := TGridData(SG.Objects[0, Index]);
end;

function TqNOXifyFrm.GetSelectedGridHashes: TStringList;
begin
  Result := TStringList.Create;
  for var Row := 0 to SG.RowCount - 1 do
    if SG.RowHeights[Row] > 0 then
    begin
      var RD := GetRowData(Row);
      if RD.Selected then
        Result.Add(RD.Hash);
    end;
end;

function TqNOXifyFrm.GetSelectedTorrents: TObjectList<TqBitTorrentType>;
begin
  Result := TObjectList<TqBitTorrentType>(False);
  var HL := GetSelectedGridHashes;
    for var T in qBMain.Ftorrents do
      if HL.IndexOf(T.Key) <> -1 then
        Result.Add(TqBitTorrentType(T.Value));
end;

function TqNOXifyFrm.GetAllGridHashes: TStringList;
begin
  Result := TStringList.Create;
  for var Row := 0 to SG.RowCount - 1 do
    if SG.RowHeights[Row] > 0 then
    begin
      var RD := GetRowData(Row);
      Result.Add(RD.Hash);
    end;
end;

function TqNOXifyFrm.GetAllServerHashes: TStringList;
begin
  Result := TStringList.Create;
  for var T in qBMain.Ftorrents do
    Result.Add(T.Key);
end;

function TqNOXifyFrm.GetLastGridHash: string;
begin
  Result := GetRowData( GetColData(0).LastSelected ).Hash;
end;

function TqNOXifyFrm.GetLastSelectedTorrent: TqBitTorrentType;
var
  Res : TqBitTorrentBaseType;
begin
  Result := Nil;
  if qBMain.Ftorrents.TryGetValue(GetLastGridHash, Res) then
    Result := TqBitTorrentType(Res);
end;

procedure TqNOXifyFrm.HideCol(ACol: integer);
begin
  var GD := GetColData(ACol);
  if GD.Visible then
  begin
    GD.Width := SG.ColWidths[ACol];
    SG.ColWidths[ACol] := -1;
    GD.Visible := False;
  end;
end;

procedure TqNOXifyFrm.New1Click(Sender: TObject);
begin
  if AddEditCatDlg.ShowModal = mrOK then
    qB.AddNewCategory(AddEditCatDlg.EDName.Text, AddEditCatDlg.EDPath.Text);
end;

procedure TqNOXifyFrm.New2Click(Sender: TObject);
begin
  var IMsg := InputBox('Add Tags (comma separated)', 'Tags','');
  if IMsg <> '' then qb.CreateTags(IMsg);
end;

procedure TqNOXifyFrm.PMIToggleSpeedLimitsClick(Sender: TObject);
begin
  qB.ToggleAlternativeSpeedLimits;
end;

procedure TqNOXifyFrm.Rename1Click(Sender: TObject);
begin
  var T := GetLastSelectedTorrent;
  if not assigned(T) then Exit;
  var NewName := InputBox('Rename', 'New Name :', T.Fname);
  if  NewName <> '' then qB.SetTorrentName(T.Fhash, NewName);
end;

procedure TqNOXifyFrm.Reset1Click(Sender: TObject);
begin
  var SH := GetSelectedGridHashes;
  qB.SetTorrentCategory(SH, '');
  SH.Free;
end;

procedure TqNOXifyFrm.PMICatResetClick(Sender: TObject);
begin
  var SH := GetSelectedGridHashes;
  for var i := 0 to SH.Count - 1 do
    qB.RemoveTorrentTags(SH,'');
  SH.Free
end;

procedure TqNOXifyFrm.PMIDeleteDataClick(Sender: TObject);
begin
  var SH := GetSelectedGridHashes;
  for var i := 0 to SH.Count - 1 do
    qB.DeleteTorrents(SH, True);
  SH.Free;
end;

procedure TqNOXifyFrm.PMIDeleteTorrentClick(Sender: TObject);
begin
  var SH := GetSelectedGridHashes;
  for var i := 0 to SH.Count - 1 do
    qB.DeleteTorrents(SH, False);
  SH.Free;
end;

procedure TqNOXifyFrm.PMIPauseClick(Sender: TObject);
begin
  var HL := GetSelectedGridHashes;
  if assigned(HL) then qB.PauseTorrents(HL);
  HL.Free;
end;

procedure TqNOXifyFrm.ShowCol(ACol: integer);
begin
  var GD := GetColData(ACol);
  if not GD.Visible then
  begin
    SG.ColWidths[ACol] := GD.Width;
    GD.Visible := True;
  end;
end;

procedure TqNOXifyFrm.ShowHideItemClicked(Sender: TObject);
begin
  if GetColData(TMenuItem(Sender).Tag).visible then
    HideCol(TMenuItem(Sender).Tag)
  else
    ShowCol(TMenuItem(Sender).Tag);
end;

procedure TqNOXifyFrm.CatItemClicked(Sender: TObject);
begin
  var HL := GetSelectedGridHashes;
  if HL.Count > 0 then
    if TMenuItem(Sender).Checked then
    begin
      qB.AddTorrentTags(HL, TMenuItem(Sender).Caption);
    end else begin
      qB.RemoveTorrentTags(HL, TMenuItem(Sender).Caption);
    end;
  HL.Free;
end;

procedure TqNOXifyFrm.StatusBarClick(Sender: TObject);
var
  P : TPoint;
begin
  GetCursorPos(P);
  PMStatus.Popup(
    P.X,
    P.Y
  );
end;

procedure TqNOXifyFrm.StatusBarContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  PMStatus.Popup(
    StatusBar.ClientToScreen(MousePos).X,
    StatusBar.ClientToScreen(MousePos).Y
  );
end;

procedure TqNOXifyFrm.PMIResumeClick(Sender: TObject);
begin
  var SH := GetSelectedGridHashes;
  qB.ResumeTorrents(SH);
  SH.Free
end;

procedure TqNOXifyFrm.PMISortColClick(Sender: TObject);
begin
  ToggleSortCol(PMHdrCol.Tag);
end;

procedure TqNOXifyFrm.PMISpeedLimitsClick(Sender: TObject);
begin
  SpeedLimitsDlg.SetSpeedLimits(
    qBPrefs.Fup_limit,
    qBPrefs.Fdl_limit,
    qBPrefs.Falt_up_limit,
    qBPrefs.Falt_dl_limit
  );
  if SpeedLimitsDlg.ShowModal = mrOk then
  begin
    var NewPrefs := TqBitPreferencesType.Create;
      SpeedLimitsDlg.GetSpeedLimits(
      NewPrefs.Fup_limit,
      NewPrefs.Fdl_limit,
      NewPrefs.Falt_up_limit,
      NewPrefs.Falt_dl_limit
      );
    qB.SetPreferences(NewPrefs);
    qBPrefs.Free;
    qBPrefs := qB.GetPreferences;
    NewPrefs.Free;
  end;
end;

procedure TqNOXifyFrm.DeleteCategoryClicked(Sender : TObject);
var
  Cat: string;
begin
  Cat := TMenuItem(TMenuItem(Sender).Parent).Caption;
  var HL := TStringList.Create;
  for var T in qBMain.Ftorrents do
      if TqBitTorrentType(T.Value).Fcategory = Cat then
        HL.Add(TqBitTorrentType(T.Value).Fhash);
  var SH := GetSelectedGridHashes;
  if qB.RemoveCategories(Cat) then
    qB.SetTorrentCategory(HL, Cat);
  HL.Free;
  SH.Free;
end;

procedure TqNOXifyFrm.Disable1Click(Sender: TObject);
begin
  var HL := GetSelectedGridHashes;
  qB.SetAutomaticTorrentManagement(HL, False);
  HL.Free;
end;

procedure TqNOXifyFrm.Enable1Click(Sender: TObject);
begin
  var HL := GetSelectedGridHashes;
  qB.SetAutomaticTorrentManagement(HL, True);
  HL.Free
end;

procedure TqNOXifyFrm.SetCategoryClicked(Sender : TObject);
var
  Cat: string;
begin
  Cat := TMenuItem(TMenuItem(Sender).Parent).Caption;
  var SH := GetSelectedGridHashes;
  if SH.Count > 0 then
    qB.SetTorrentCategory(SH, TMenuItem(TMenuItem(Sender).Parent).Caption);
  SH.Free;
end;

procedure TqNOXifyFrm.PMColPopup(Sender: TObject);
begin

    // Categories

    for var i := PMICategory.Count -1 downto 3 do
      PMICategory.Items[i].Free;

    for var C in qBMain.Fcategories do
      begin
       var NewItem := TMenuItem.Create(PMHdrCol);
      NewItem.Caption := C.Key;
      NewItem.Tag := 0;
      PMICategory.Add(NewItem);


      var NewItem2 := TMenuItem.Create(PMHdrCol);
      NewItem2.Caption := 'Assign';
      NewItem2.OnClick := SetCategoryClicked;
      NewItem.Add(NewItem2);

      var NewItem3 := TMenuItem.Create(PMHdrCol);
      NewItem3.Caption := 'Delete';
      NewItem3.OnClick := DeleteCategoryClicked;;
      NewItem.Add(NewItem3);
    end;

    // Tags
    for var i := PMITags.Count -1 downto 3 do
      PMITags.Items[i].Free;

    for var i := 0 to qBMain.Ftags.Count - 1  do
    begin
      var NewItem := TMenuItem.Create(PMHdrCol);
      NewItem.AutoCheck := True;
      NewItem.Caption := qBMain.Ftags.Items[i];
      NewItem.Tag := i + 1;
      NewItem.GroupIndex := 0;
      NewItem.OnClick := CatItemClicked;
      PMITags.Add(NewItem);
    end;

end;

procedure TqNOXifyFrm.PMHdrColPopup(Sender: TObject);
begin
   for var i := PMIShowHide.Count -1 downto 0 do
    PMIShowHide.Items[i].Free;

  for var i:= 1 to SG.ColCount -1 do
    if GetColData(i).Name <> '' then
    begin
      var NewItem := TMenuItem.Create(PMHdrCol);
      NewItem.AutoCheck := True;
      NewItem.Caption := GetColData(i).Name;
      NewItem.Checked := GetColData(i).Visible;
      NewItem.Tag := i;
      NewItem.GroupIndex := 0;
      NewItem.OnClick := ShowHideItemClicked;
      PMIShowHide.Add(NewItem);
    end;
end;

procedure TqNOXifyFrm.TrackMenuNotifyHandler(Sender: TMenu; Item: TMenuItem; var CanClose: Boolean);
begin
  CanClose := Item.Tag = 0;
end;

procedure TqNOXifyFrm.WndProc(var Msg: TMessage);
begin
  FormMainMenuWndProcMessage(Msg, Self);
  inherited;
end;

procedure TqNOXifyFrm.ShowAllClick(Sender: TObject);
begin
  for var i := 0 to SG.RowCount -1 do
      if ( SG.ColWidths[i] = -1) and ( GetColData(i).Visible = False) then
        ShowCol(i);
end;

procedure TqNOXifyFrm.SGMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  P : TPoint;
  ACol, ARow : integer;
begin
  GetCursorPos(P);
  SG.MouseToCell(SG.ScreenToClient(P).X, SG.ScreenToClient(P).Y, ACol, ARow);
  if ( (GetColData(0).HintX <> P.X) or (GetColData(0).HintY <> P.Y) ) then
  begin
    if (ACol<1) or (ARow<1) then Exit;
    SG.Hint := SG.Cells[0, ARow] + #$D#$A + SG.Cells[ACol, 0] + ' : ' +SG.Cells[ACol, ARow];
    Application.ActivateHint(P);
    Application.HintPause := 2000;
    Application.HintHidePause := 10000;
    GetColData(0).HintX := P.X;
    GetColData(0).HintY := P.Y;
    SG.ShowHint := True;
  end;
  // Caption := X.ToString + ' - ' + Y.ToString +' - '+ random(9999).ToString;
end;

procedure TqNOXifyFrm.HideClick(Sender: TObject);
begin
  HideCol(PMHdrCol.Tag);
end;

procedure TqNOXifyFrm.AAAA1Click(Sender: TObject);
begin
 Caption := TMenuItem(Sender).Caption;
end;

procedure TqNOXifyFrm.AddCol(Index: Integer; Name, Field: string; Fmt: TVarDataFormater; Width: Integer; Visible: Boolean);
begin
  var GD := TGridData(SG.Objects[Index, 0]);
  GD.Name := Name;
  GD.Field := Field;
  GD.Formater := Fmt;
  GD.Width := Width;
  GD.Visible := Visible;
  SG.Cells[Index, 0] := Name;
  SG.ColWidths[Index] := -1;
  if Visible then SG.ColWidths[Index] := Width;
  SG.RowHeights[Index] := ROWHEIGHT;
  var NewItem := TMenuItem.Create(PMHdrCol);
  NewItem.Caption := Name;
  PMIShowHide.Add(NewItem);
end;

procedure TqNOXifyFrm.BitBtn1Click(Sender: TObject);
begin
  Self.EditSearch.Clear;
end;

procedure TqNOXifyFrm.CBStatusSelect(Sender: TObject);
begin
  UpdateUI;
end;

procedure TqNOXifyFrm.ForceReannounce1Click(Sender: TObject);
begin
  var HL := GetSelectedGridHashes;
  qB.ReannounceTorrents(HL);
  HL.Free
end;

procedure TqNOXifyFrm.ForceRecheck1Click(Sender: TObject);
begin
  var HL := GetSelectedGridHashes;
  qB.RecheckTorrents(HL);
  HL.Free
end;

procedure TqNOXifyFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Action := caFree;
end;

procedure TqNOXifyFrm.FormCreate(Sender: TObject);
begin
  Warning.Visible := False;
  PMHdrCol.TrackMenu := True;
  PMHdrCol.OnTrackMenuNotify := TrackMenuNotifyHandler;
  PMCol.TrackMenu := True;
  PMCol.OnTrackMenuNotify := TrackMenuNotifyHandler;


  SG.RowCount := MAXROW;
  SG.ColCount := MAXCOL;
  for var i := 0 to SG.ColCount - 1 do
    for var j := 0 to SG.RowCount - 1 do
    begin
      SG.ColWidths[i] := -1;
      SG.RowHeights[j] := -1;
      SG.Objects[i, j] := TGridData.Create;;
    end;

  var Row := -1;
  Inc(Row); AddCol(Row, 'Name', 'Fname', VarFormatString, 240, True);
  Inc(Row); AddCol(Row, 'Size', 'Fsize', VarFormatBKM, 84, True);
  Inc(Row); AddCol(Row, 'Progress', 'Fprogress', VarFormatPercent, 84, True);
  Inc(Row); AddCol(Row, 'Status', 'Fstate', VarFormatString, 84, True);
  Inc(Row); AddCol(Row, 'Seeds', 'Fnum_seeds', VarFormatString, 84, True);
  Inc(Row); AddCol(Row, 'Peers', 'Fnum_leechs', VarFormatString, 84, True);
  Inc(Row); AddCol(Row, 'Down Speed', 'Fdlspeed', VarFormatBKMPerSec, 84, True);
  Inc(Row); AddCol(Row, 'Upload Speed', 'Fupspeed', VarFormatBKMPerSec, 84, True);
  Inc(Row); AddCol(Row, 'ETA', 'Feta', VarFormatDeltaSec, 128, True);
  Inc(Row); AddCol(Row, 'Ratio', 'Fratio', VarFormatFloat2d, 84, True);
  Inc(Row); AddCol(Row, 'Category', 'Fcategory', VarFormatString, 84, True);
  Inc(Row); AddCol(Row, 'Tags', 'Ftags', VarFormatString, 84, True);
  Inc(Row); AddCol(Row, 'Added On', 'Fadded_on', VarFormatDate, 128, True);
  Inc(Row); AddCol(Row, 'Availability', 'Favailability', VarFormatMulti, 84, True);

  var rttictx := TRttiContext.Create();
  var rttitype := rttictx.GetType(TqBitTorrentType);
  for var field in rttitype.GetFields do
  begin
    var Title := 'Raw: ' + field.Name;
    Inc(Row); AddCol(Row, Title, field.Name, VarFormatString, 84, False);
  end;
  rttictx.Free;

  GetColData(0).SortField := 'Fname';
  GetColData(0).SortReverse := False;
end;

procedure TqNOXifyFrm.FormShow(Sender: TObject);
begin
  if SelectServerDlg.ShowModal = mrCancel then
  begin
    Close;
    Exit;
  end;
  var Srv := SelectServerDlg.GetServer;
  qB := TqBitObject.Connect(Srv.FHP, Srv.FUN, Srv.FPW);
  if not assigned(qB) then Exit;
  qBMain := qB.GetMainData;
  qBPrefs := qB.GetPreferences;
  DragAcceptFiles (Self.handle, True);
  Timer.Interval := qBMain.Fserver_state.Frefresh_interval;
  UpdateUI;
  Timer.OnTimer(Nil);
end;

procedure TqNOXifyFrm.FormDestroy(Sender: TObject);
begin
  for var i := 0 to SG.RowCount - 1 do
    for var j := 0 to SG.ColCount - 1 do
      TGridData(SG.Objects[j, i]).Free;
  qBPrefs.Free;
  qBTInfo.Free;
  qBMain.Free;
  qB.Free;
end;

procedure TqNOXifyFrm.SGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  ACol, ARow : integer;
begin

  SG.MouseToCell(X, Y, ACol, ARow);
  if Button = mbRight then
  begin
    if ARow = 0 then
    begin
      P.X := X; P.Y :=Y;
      PMHdrCol.Tag := ACol;
      PMHdrCol.Popup(SG.ClientToScreen(P).X - 8, SG.ClientToScreen(P).Y - 2);
    end else begin
      P.X := X; P.Y :=Y;
      PMCol.Tag := ACol;
      PMCol.Popup(SG.ClientToScreen(P).X - 8, SG.ClientToScreen(P).Y - 2);
    end;
  end;

  if Button <> mbLeft then Exit;

  if (ARow = 0) and (GetKeyState(VK_SHIFT) < 0) then
  begin

  end;
  if ARow < SG.FixedRows then Exit;
  var GD := TGridData(SG.Objects[0, ARow]);
  if GetKeyState(VK_CONTROL) < 0 then
  begin
    GD.Selected :=  not GD.Selected;
    GetColData(0).LastSelected := ARow;
    qBTInfo.Free;
    qBTInfo := qB.GetTorrentGenericProperties(GetRowData(ARow).Hash);
  end else
  if GetKeyState(VK_SHIFT) < 0 then
  begin
   for var i := Min(Arow, GetColData(0).LastSelected) to  Max(Arow, GetColData(0).LastSelected) do
     GetRowData(i).Selected := True;
  end else begin
    for var i := 0 to SG.RowCount - 1 do
      GetRowData(i).Selected := False;
    GetColData(0).LastSelected := ARow;
    qBTInfo.Free;
    qBTInfo := qB.GetTorrentGenericProperties(GetRowData(ARow).Hash);
    GD.Selected := True;
  end;
  SG.Invalidate;
end;

procedure TqNOXifyFrm.SGMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  var MaxRow := 1;
  while( SG.RowHeights[MaxRow] <> -1 ) do Inc(MaxRow);
  if SG.VisibleRowCount < MaxRow then SG.TopRow := SG.TopRow + 4;
  Handled := True;
end;

procedure TqNOXifyFrm.SGMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
   SG.TopRow := Max(SG.TopRow - 4, 1);
   Handled := True;
end;

procedure TqNOXifyFrm.ShowAllCol;
begin

end;

procedure TqNOXifyFrm.TimerTimer(Sender: TObject);
begin
  var U := qB.GetMainData(qBMain.Frid);
  qBMain.Merge(U);
  for var T in qBMain.Ftorrents do
    TqBitTorrentType(T.Value).Fhash := T.Key;
  U.Free;
  UpdateUI;
end;

procedure TqNOXifyFrm.WMDropFiles(var Msg: TMessage);
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
      qB.AddNewTorrentFile(NewTorrentFile);
  end;
  NewTorrentFile.Free;
  DragFinish(hDrop);
end;

procedure TqNOXifyFrm.ToggleSortCol(ACol: integer);
begin
  if aCol < 0 then Exit;
  var Current := GetRowData(0).SortField;
  var Field :=  GetColData(ACol).Field;
  if Current <> field then
  begin
    GetColData(0).SortField := Field;
    GetColData(0).SortReverse := False;
  end else
    GetColData(0).SortReverse := not GetColData(0).SortReverse;
  UpdateUI;
end;

procedure TqNOXifyFrm.SetLocation1Click(Sender: TObject);
begin
  var SH := GetSelectedGridHashes;
  if SH.Count = 1 then
    SetLocationDlg.Location.Text := Self.GetLastSelectedTorrent.Fsave_path
  else
    SetLocationDlg.Location.Text := qBPrefs.Fsave_path;
  if (SetLocationDlg.ShowModal = mrOk) then
     qB.SetTorrentLocation(SH, SetLocationDlg.Location.Text);
  SH.Free;
end;

procedure TqNOXifyFrm.SGDblClick(Sender: TObject);
var
  P : TPoint;
  ACol, ARow : integer;
begin
  GetCursorPos(P) ;
  SG.MouseToCell(SG.ScreenToClient(P).X, SG.ScreenToClient(P).Y, ACol, ARow);
  ToggleSortCol(ACol);
end;

procedure TqNOXifyFrm.SGDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
  State: TGridDrawState);
begin

  var FontColor := SG.Canvas.Font.Color;
  var BrushColor := SG.Canvas.Brush.Color;

  if Arow mod 2 = 1 then
    SG.Canvas.Brush.Color := clWhite
  else
    SG.Canvas.Brush.Color := clCream;

  if (ARow = 0) or (ACol = 0) then
      SG.Canvas.Brush.Color := clMenu;

  var GD := TGridData(SG.Objects[0, ARow]);
  if GD.Selected  then
  begin
    SG.Canvas.Brush.Color := clNavy;
    SG.Canvas.Font.Color := clWhite;
  end else begin
    SG.Canvas.Font.Color:=clBlack;
  end;

  SG.Canvas.FillRect(Rect);
  SG.Canvas.TextRect (Rect, Rect.Left+4, Rect.Top+2, SG.Cells[ACol,ARow]);

  SG.Canvas.Brush.Color := BrushColor;
  SG.Canvas.Font.Color := FontColor;
end;

procedure TqNOXifyFrm.SGKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 65) and ( ssCtrl in Shift) then
    for var i:= 1 to SG.RowCount  - 1 do
      if assigned(GetRowData(i)) then
        GetRowData(i).Selected := True;
  UpdateUI;
end;

procedure TqNOXifyFrm.UpdateUI;
begin
  Timer.Enabled := False;
  SG.BeginUpdate;
  var SH := GetSelectedGridHashes;
  var RttiCtx := TRttiContext.Create();
  var RttiType := RttiCtx.GetType(TqBitTorrentType);

  var TL := TObjectList<TqBitTorrentType>.Create(False);
  for var T in qBMain.Ftorrents do
  begin
    TqBitTorrentType(T.Value).Fhash := T.Key;
    TL.Add(TqBitTorrentType(T.Value));
  end;

  // Filtering Status
  for var j := 0 to CBStatus.Items.Count -1 do
    CBStatus.Items.Objects[j] := Nil;
  for var i := TL.Count - 1 downto 0 do
  begin
    var ToKeep := False;
    var Status := lowerCase(TqBitTorrentType(TL[i]).Fstate);
    for var j := 0 to CBStatus.Items.Count -1 do
    begin
      case j of
        0: begin // All
          CBStatus.Items.Objects[j] := Pointer(Integer(CBStatus.Items.Objects[j]) + 1);
          ToKeep := ToKeep or (CBStatus.ItemIndex = j);
        end;
        1: begin // downloading
          if (Status = 'downloading') or (Status = 'stalleddl') then
          begin
            CBStatus.Items.Objects[j] := Pointer(Integer(CBStatus.Items.Objects[j]) + 1);
            ToKeep := ToKeep or (CBStatus.ItemIndex = j);
          end;
        end;
        2: begin // uploading
          if (Status = 'uploading') or (Status = 'stalledup') then
          begin
            CBStatus.Items.Objects[j] := Pointer(Integer(CBStatus.Items.Objects[j]) + 1);
            ToKeep := ToKeep or (CBStatus.ItemIndex = j);;
          end;
        end;
        3: begin // completed
          if (Status = 'uploading') or (Status = 'stalledup') or (Status = 'pausedup') then
          begin
            CBStatus.Items.Objects[j] := Pointer(Integer(CBStatus.Items.Objects[j]) + 1);
            ToKeep := ToKeep or (CBStatus.ItemIndex = j);
          end;
        end;
        4: begin // paused;
          if (Status = 'pausedup') or (Status = 'pauseddl') then
          begin
            CBStatus.Items.Objects[j] := Pointer(Integer(CBStatus.Items.Objects[j]) + 1);
            ToKeep := ToKeep or (CBStatus.ItemIndex = j);
          end;
        end;
        5: begin // Stalled
          if (Status = 'stalledup') or (Status = 'stalleddl') then
          begin
            CBStatus.Items.Objects[j] := Pointer(Integer(CBStatus.Items.Objects[j]) + 1);
            ToKeep := ToKeep or (CBStatus.ItemIndex = j);;
          end;
        end;
        6: begin // Errored
          if (Status = 'missingfiles') then
          begin
            CBStatus.Items.Objects[j] := Pointer(Integer(CBStatus.Items.Objects[j]) + 1);
            ToKeep := ToKeep or (CBStatus.ItemIndex = j);;
          end;
        end;

      end;
    end;
    if not ToKeep then TL.Delete(i);
  end;

  // Filtering Categories

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

  // Filtering String
  if EditSearch.Text <> '' then
    for var i := TL.Count - 1 downto 0 do
    begin
      if Pos(EditSearch.Text, TqBitTorrentType(TL[i]).Fname) = 0 then
        TL.Delete(i);
    end;

  // Sorting
  TL.Sort(TComparer<TqBitTorrentType>.Construct(
      function (const L, R: TqBitTorrentType): integer
      begin
        Result := 0;
        var MD := GetColData(0);
        for var Field in RttiType.GetFields do
        begin
          if Field.Name = MD.SortField then
            begin
              var LVal := Field.GetValue(L).asVariant;
              var RVal := Field.GetValue(R).asVariant;
              if LVal > RVal then
                if MD.SortReverse then Result := -1 else Result := 1;
              if RVal > LVal then
                if MD.SortReverse then Result := 1 else Result := -1;
           end;
        end;
      end
  ));

  // Displaying Grid
  for var Col := 0 to SG.RowCount - 1 do
  begin
    var GD := GetColData(Col);
    if assigned(GD) then
    begin
      if GD.Field = GetColData(0).SortField then
      begin
        if not GetColData(0).SortReverse then
          SG.Cells[Col, 0] := '🡻 ' + GetColData(Col).Name
        else
          SG.Cells[Col, 0] := '🡹 ' + GetColData(Col).Name ;
      end else
        SG.Cells[Col, 0] := GetColData(Col).Name;
      var Row := 1;
      for var T in TL do
      begin
        for var Field in RttiType.GetFields do
          if  Field.Name = GD.Field then
          begin
            SG.Cells[Col, Row] := GD.Formater( Field.GetValue(T).asVariant, nil);
            SG.RowHeights[Row] := ROWHEIGHT;
            GetRowData(Row).Hash := T.Fhash;
            break;
          end;
        GetRowData(Row).Selected := SH.IndexOf(T.Fhash) <> -1;
        Inc(Row);
      end;
    end;
  end;
  for var i := TL.count+1 to SG.RowCount - 1  do
    if SG.RowHeights[i] > -1 then SG.RowHeights[i] := -1;

  // MainForm
  var HL := Self.GetAllGridHashes;
  Caption:= Format('qNOXify :  %s (%d/%d Torrents)', [
              qB.HostPath,
              (HL.Count - 1),
              qbMain.Ftorrents.Count
            ]);
  HL.Free;

  // Displaying Status

  var StatusIndex := CBStatus.ItemIndex;
  for var i := 0 to CBStatus.Items.Count -1 do
  case i of
    0: CBStatus.Items[i] := Format('All (%d)', [Integer(CBStatus.Items.Objects[i])]);
    1: CBStatus.Items[i] := Format('Downloading (%d)', [Integer(CBStatus.Items.Objects[i])]);
    2: CBStatus.Items[i] := Format('Seeding (%d)', [Integer(CBStatus.Items.Objects[i])]);
    3: CBStatus.Items[i] := Format('Completed (%d)', [Integer(CBStatus.Items.Objects[i])]);
    4: CBStatus.Items[i] := Format('Paused (%d)', [Integer(CBStatus.Items.Objects[i])]);
    5: CBStatus.Items[i] := Format('Stalled (%d)', [Integer(CBStatus.Items.Objects[i])]);
    6: CBStatus.Items[i] := Format('Errored (%d)', [Integer(CBStatus.Items.Objects[i])]);
  end;
  CBStatus.ItemIndex := StatusIndex;

  // Displaying Categories

  if qBMain.Ffull_update or qBMain.Fcategories_changed then
  begin
    CBCat.Items.Clear;
    CBCat.Items.Add('All');
    CBCat.Items.Add('Unassigned');
    for var Cat in qBMain.Fcategories do CBCat.Items.Add(Cat.Key);
    CBCat.ItemIndex := 0;
  end;

  // Displaying Tags

  if qBMain.Ffull_update or qBMain.Ftags_changed then
  begin
    CBTag.Items.Clear;
    CBTag.Items.Add('All');
    CBTag.Items.Add('Unassigned');
    for var Tag in qBMain.Ftags do CBTag.Items.Add(Tag);
    CBTag.ItemIndex := 0;
  end;

  // StatusBar;

  StatusBar.Panels[0].Text := TitleCase( qBMain.Fserver_state.Fconnection_status );
  StatusBar.Panels[1].Text := 'Free space: ' + VarFormatBKM(qBMain.Fserver_state.Ffree_space_on_disk);
  StatusBar.Panels[2].Text := Format('DHT: %s nodes', [VarToStr(qBMain.Fserver_state.Fdht_nodes)]);
  if qBMain.Fserver_state.Fuse_alt_speed_limits then
    StatusBar.Panels[3].Text := ' @ ' // ' ⬊ '
  else
    StatusBar.Panels[3].Text := '  '; // ' ⬈ ';
  if qBMain.Fserver_state.Fdl_rate_limit > 0 then
    StatusBar.Panels[4].Text :=
      Format('🡻 %s [%s]', [
         VarFormatBKMPerSec(qBMain.Fserver_state.Fdl_info_speed),
         VarFormatBKMPerSec(qBMain.Fserver_state.Fdl_rate_limit)
      ])
  else
    StatusBar.Panels[4].Text :=
      Format('🡻 %s [∞]', [
         VarFormatBKMPerSec(qBMain.Fserver_state.Fdl_info_speed)
      ]);
  if qBMain.Fserver_state.Fup_rate_limit > 0 then
    StatusBar.Panels[5].Text :=
      Format('🡹 %s [%s]', [
         VarFormatBKMPerSec(qBMain.Fserver_state.Fup_info_speed),
         VarFormatBKMPerSec(qBMain.Fserver_state.Fup_rate_limit)
      ])
  else
    StatusBar.Panels[5].Text :=
      Format('🡹 %s [∞]', [
         VarFormatBKMPerSec(qBMain.Fserver_state.Fup_info_speed)
      ]);


  // Selecting First Row

  if qBMain.Ffull_update and TL.count > 0 then
  begin
    GetRowData(1).Selected := True;
    GetColData(0).LastSelected := 1;
  end;

  SH.Free;
  RttiCtx.Free;
  TL.Free;
  SG.EndUpdate;
  Timer.Enabled := True;
end;

end.
