unit uSetLocation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.Buttons;

type
  TSetLocationDlg = class(TForm)
    Location: TEdit;
    BBFolder: TBitBtn;
    ImageList: TImageList;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure BBFolderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SetLocationDlg: TSetLocationDlg;

implementation

{$R *.dfm}

procedure TSetLocationDlg.BBFolderClick(Sender: TObject);
begin
 with TFileOpenDialog.Create(nil) do
  try
    Options := [fdoPickFolders];
    DefaultFolder := Location.Text;
    if Execute then
      Location.Text := Filename;
  finally
    Free;
  end;
end;

end.
