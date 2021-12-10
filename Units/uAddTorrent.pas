unit uAddTorrent;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uqBitAPITypes, uqBitAPI, uqBitObject,
  Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls;

type
  TAddTorrentDlg = class(TForm)
    TTM: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SFL: TEdit;
    RT: TEdit;
    Label4: TLabel;
    CBCat: TComboBox;
    Button1: TButton;
    CBST: TCheckBox;
    CBSHT: TCheckBox;
    CBCL: TComboBox;
    CBDSO: TCheckBox;
    CBFLP: TCheckBox;
    ComboBox1: TComboBox;
    Label5: TLabel;
    SpinEdit1: TSpinEdit;
    Label6: TLabel;
    SpinEdit2: TSpinEdit;
    ComboBox2: TComboBox;
    Label7: TLabel;
    LBFiles: TListBox;
    Button2: TButton;
    Bevel1: TBevel;
    procedure FormShow(Sender: TObject);
    procedure TTMChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    qB: TqBitObject;
    Prefs : TqBitPreferencesType;
    FileList: TStringList;
  end;

var
  AddTorrentDlg: TAddTorrentDlg;

implementation
uses Math;

const
  bstr: array[boolean] of string = ('false','true');

{$R *.dfm}

procedure TAddTorrentDlg.Button1Click(Sender: TObject);
begin
  for var F in FileList do
  begin
    var NewTorrent := TqBitNewTorrentFileType.Create;
    NewTorrent.Ffilename:= F;
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
    if not qB.AddNewTorrentFile(NewTorrent) then
      ShowMessage(F + ' has Failed');
    NewTorrent.Free;
  end;
end;

procedure TAddTorrentDlg.FormShow(Sender: TObject);
begin
  LBFiles.Items.Clear;
  for var F in FileList do
    LBFiles.Items.Add(F);
  RT.Text := '';
  RT.Enabled := FileList.Count = 1;


  Prefs := qB.GetPreferences;
  SFL.Text := Prefs.Fsave_path;
  var Cats := qB.GetAllCategories;
  if assigned(Cats) then
  begin
    CBCat.Clear;
    CBCat.items.Add('');
    for var Cat in Cats.Fcategories do
      CBCat.items.Add(Cat.Key);
  end;
  Cats.Free;
  Prefs.Free;
end;

procedure TAddTorrentDlg.TTMChange(Sender: TObject);
begin
  SFL.Enabled := TTM.ItemIndex =0;
end;

end.
