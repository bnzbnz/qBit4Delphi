unit uqBitAddTorrentDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,  Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls,
  System.Generics.Collections, System.Generics.Defaults,
  uqBitAPITypes, uqBitAPI, uqBitObject,
  uTorrentReader, uqBitFormat;

type
  TTNewTorrentState = (ntsSuccess, ntsMissing, ntsInvalid, ntsDuplicate, ntsError);
  TNewTorrentInfo = class
    Status: TTNewTorrentState;
    Filename: string;
    FileData: TTorrentReader;
    destructor Destroy; override;
  end;

  TqBitAddTorrentDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    TTM: TComboBox;
    SFL: TEdit;
    RT: TEdit;
    CBCat: TComboBox;
    CBCL: TComboBox;
    Label3: TLabel;
    ChkDefault: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    CBST: TCheckBox;
    CBSHT: TCheckBox;
    CBDSO: TCheckBox;
    CBFLP: TCheckBox;
    ComboBox1: TComboBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    ComboBox2: TComboBox;
    BtnCancel: TButton;
    BtnOK: TButton;
    Bevel1: TBevel;
    GroupBox1: TGroupBox;
    TILblName: TLabel;
    TILblSize: TLabel;
    TIEditName: TEdit;
    TIEditSize: TEdit;
    TIEditHashV1: TEdit;
    Label8: TLabel;
    TIEditHashV2: TEdit;
    Label11: TLabel;
    TIEditComment: TMemo;
    Panel4: TPanel;
    LBFiles: TListBox;
    Label12: TLabel;
    BtnMgeCat: TButton;

    procedure FormShow(Sender: TObject);
    procedure TTMChange(Sender: TObject);
    procedure LBFilesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnMgeCatClick(Sender: TObject);
  private
    { Private declarations }
    FqB: TqBitObject;
    procedure UploadTorrents;
    procedure SetUseDefault(DMode: Boolean);
    function GetUseDefault: Boolean;
    procedure UpdateCategories(CurCat: string = '');
  public
    { Public declarations }
    NewTorrents: TObjectList<TNewTorrentInfo>;
    function ShowModal: Integer; override;
    function ShowAsModal(qB: TqBitObject; FileList: TStrings): TModalResult;
    property UseDefault: Boolean read GetUseDefault write SetUseDefault;
  end;

var
  qBitAddTorrentDlg: TqBitAddTorrentDlg;

implementation
uses Math, uqBitUtils, uqBitCategoriesDlg;

{$R *.dfm}

// https://stackoverflow.com/questions/12946150/how-to-bring-my-application-to-the-front
procedure ForceForegroundNoActivate(hWnd : THandle);
begin
 if IsIconic(Application.Handle) then
  ShowWindow(Application.Handle, SW_SHOWNOACTIVATE);
 SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOMOVE);
 SetWindowPos(hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOMOVE);
end;

procedure TqBitAddTorrentDlg.FormShow(Sender: TObject);
begin
  ForceForegroundNoActivate(Self.Handle);
end;

procedure TqBitAddTorrentDlg.BtnOKClick(Sender: TObject);
begin
  UpLoadTorrents;
  ModalResult := mrOk;
  BtnOk.ModalResult := mrOk;
end;

procedure TqBitAddTorrentDlg.BtnMgeCatClick(Sender: TObject);
begin
  if qBitCategoriesDlg.ShowAsModal(FqB, True) = mrOk then
    UpdateCategories(qBitCategoriesDlg.Selected.Fname);
end;

procedure TqBitAddTorrentDlg.FormCreate(Sender: TObject);
begin
  NewTorrents := TObjectList<TNewTorrentInfo>.Create(True);
end;

procedure TqBitAddTorrentDlg.FormDestroy(Sender: TObject);
begin
  NewTorrents.Free;
end;

function TqBitAddTorrentDlg.GetUseDefault: Boolean;
begin
  Result := Self.ChkDefault.Checked;
end;

procedure TqBitAddTorrentDlg.SetUseDefault(DMode: Boolean);
begin
  Self.ChkDefault.Checked := DMode;
end;

procedure TqBitAddTorrentDlg.LBFilesClick(Sender: TObject);
begin
  var Index := LBFiles.ItemIndex;
  if Index = -1  then  Exit;
  var NewT := TNewTorrentInfo(LBFiles.Items.Objects[Index]);
  TIEditName.Text := NewT.FileData.Data.NiceName;
  TIEditSize.Text := VarFormatBKM(NewT.FileData.Data.Info.FilesSize);
  TIEditHashV1.Text := TqBitUtils.IIF(NewT.FileData.Data.HashV1 <> '', NewT.FileData.Data.HashV1, 'N/A');
  TIEditHashV2.Text := TqBitUtils.IIF(NewT.FileData.Data.HashV2 <> '', NewT.FileData.Data.HashV2, 'N/A');
  TIEditComment.Text := NewT.FileData.Data.Comment.Text;
end;

procedure TqBitAddTorrentDlg.UpdateCategories(CurCat: string);
begin
  CBCat.Clear;
  CBCat.Sorted := True;
  CBCat.items.Add('');
  var Categories := FqB.GetAllCategories;
  if Categories <> nil then
  begin
    for var Cat in Categories.Fcategories do
      CBCat.items.Add(Cat.Key);
    for var i := 0 to  CBCat.Items.Count - 1 do
      if CBCat.Items[i] = CurCat then CBCat.ItemIndex := i;
  end;
  FreeAndNil(Categories);
end;

function TqBitAddTorrentDlg.ShowAsModal(qB: TqBitObject; FileList: TStrings): TModalResult;
var
  DuplicateList: TqBitTorrentListType;
begin
  Result := mrNone;
  DuplicateList := Nil;
  if (GetKeyState(VK_CONTROL) < 0) or  (GetKeyState(VK_SHIFT) < 0) then
    UseDefault := False;
  try

    FqB := qB;
    Self.NewTorrents.Clear;
    if FileList.Count = 0 then Exit;

    // Get Torrentds Info.
    for var Filename in FileList do
    begin
      var NewT := TNewTorrentInfo.Create;
      NewT.Filename := Filename;
      if not FileExists(Filename) then
      begin
        NewT.Status := ntsMissing;
      end else begin
        NewT.FileData := TTorrentReader.LoadFromFile(FileName, [trNoPiece]);
        if NewT.FileData = Nil then NewT.Status := ntsInvalid;
      end;
      NewTorrents.Add(NewT);
    end;

    //Check for Duplicates
    var FilterList := TqBitTorrentListRequestType.Create;
    for var NewT in NewTorrents do FilterList.Fhashes.Add(NewT.FileData.Data.KeyHash);
    DuplicateList := qB.GetTorrentList(FilterList);
    FilterList.Free;

    if (DuplicateList = nil) then Exit;

    for var NewT in NewTorrents do
      for var Dup in DuplicateList.Ftorrents do
        if (TqBitTorrentType(Dup).Fhash = NewT.FileData.Data.KeyHash) then // and (NewT.FileData.Data.Info.IsPrivate) then
        begin
          NewT.Status := ntsDuplicate;
          break;
        end;

    // UpdateListBox
    LBFiles.Clear;
    for var NewT in NewTorrents do
      if NewT.Status = ntsSuccess  then
        LBFiles.Items.AddObject( NewT.Filename + '  ==>  ' + NewT.FileData.Data.NiceName, NewT);
    if LBFiles.Count = 0 then
    begin
      Result := mrOk;
      Exit;
    end;
    LBFiles.ItemIndex := 0; LBFilesClick(Self);

    //Rename Edit
    RT.Text := '';
    RT.Enabled := LBFiles.Count < 2;

    // Categories
    UpdateCategories(CBCat.Text);

    if UseDefault then
    begin
      UploadTorrents;
      Result := mrOk;
      exit;
    end;

    Result := inherited ShowModal;

  finally
     FreeAndNil(DuplicateList);
  end;
end;

function TqBitAddTorrentDlg.ShowModal: Integer;
begin
  ShowMessage('Please Call qBitAddTorrentDlg.ShowAsModal instead of ShowModal...');
  Result := mrNone;
end;

procedure TqBitAddTorrentDlg.TTMChange(Sender: TObject);
begin
  SFL.Enabled := TTM.ItemIndex = 0;
end;

procedure TqBitAddTorrentDlg.UploadTorrents;
begin

  for var i := 0 to LBFiles.Items.Count - 1 do
  begin
    var TInfo := TNewTorrentInfo(LBFiles.Items.Objects[i]);
    var NewTorrent := TqBitNewTorrentFileType.Create;
    NewTorrent.Ffilename:= TInfo.Filename;
    NewTorrent.FautoTMM := TTM.ItemIndex = 1;
    NewTorrent.FsavePath := SFL.Text;
    NewTorrent.Frename := RT.Text;
    NewTorrent.Fcategory := CBCat.Text;
    NewTorrent.Fpaused :=  not CBST.Checked;
    NewTorrent.Fskip_Checking := CBSHT.Checked;
    NewTorrent.FcontentLayout := CBCL.Text;
    NewTorrent.FsequentialDownload := CBDSO.Checked;
    NewTorrent.FfirstLastPiecePrio := CBFLP.Checked;
    NewTorrent.FdlLimit := 1024 * SpinEdit1.Value * Max(ComboBox1.ItemIndex * 1024, 1);
    NewTorrent.FupLimit := 1024 * SpinEdit2.Value * Max(ComboBox2.ItemIndex * 1024, 1);
    TInfo.Status := TqBitUtils.IIF(FqB.AddNewTorrentFile(NewTorrent), ntsSuccess, ntsError);
    NewTorrent.Free;
  end;

end;

{ TNewTorrentInfo }

destructor TNewTorrentInfo.Destroy;
begin
  Self.FileData.Free;
  inherited;
end;

end.
