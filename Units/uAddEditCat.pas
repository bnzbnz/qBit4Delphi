unit uAddEditCat;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.ImageList,
  Vcl.ImgList, Vcl.Buttons;

type
  TAddEditCatDlg = class(TForm)
    EDName: TEdit;
    EDPath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    BtnCancel: TButton;
    BBFolder: TBitBtn;
    BtnOK: TButton;
    ImageList: TImageList;
    procedure BBFolderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddEditCatDlg: TAddEditCatDlg;

implementation

{$R *.dfm}

procedure TAddEditCatDlg.BBFolderClick(Sender: TObject);
begin
 with TFileOpenDialog.Create(nil) do
  try
    Options := [fdoPickFolders];
    DefaultFolder := EDPath.Text;
    if Execute then
      EDPath.Text := Filename;
  finally
    Free;
  end;
end;

end.
