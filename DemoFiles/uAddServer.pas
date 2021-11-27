unit uAddServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  uqBitAPITypes, uqBitAPI, uqBitObject;

type
  TAddServerDlg = class(TForm)
    Bevel1: TBevel;
    BtnOK: TButton;
    BtnCancel: TButton;
    HP: TLabeledEdit;
    UN: TLabeledEdit;
    PW: TLabeledEdit;
    procedure BtnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddServerDlg: TAddServerDlg;

implementation

{$R *.dfm}

procedure TAddServerDlg.BtnOKClick(Sender: TObject);
begin
  BtnOK.Caption := '...Checking...'; BtnOK.Enabled := False;
  ModalResult := mrNone;
  var qB := TqBitObject.Connect(HP.Text, UN.Text, PW.Text);
  if assigned(qB) then
    ModalResult := mrOK
  else
    ShowMessage('Can not connect to : ' + HP.Text);
  qB.Free;
  BtnOK.Caption := 'Ok'; BtnOK.Enabled := True;
end;

end.
