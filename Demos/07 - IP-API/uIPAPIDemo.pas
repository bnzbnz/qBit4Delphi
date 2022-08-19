unit uIPAPIDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    LabeledEdit1: TLabeledEdit;
    Button1: TButton;
    LinkLabel2: TLinkLabel;
    Panel2: TPanel;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure LinkLabel2LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses uIpAPI, uqBitUtils, uCountryFlags, ShellAPI;
{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin

  var IP := TIpAPI.FromURL(Self.LabeledEdit1.Text);
  if IP = nil then Exit;

  Memo1.Clear;
  var Props := TqBitUtils.GetRTTIReadableValues(IP, TIpAPI);
  for var Prop in Props do
    Memo1.Lines.Add('  ' + Prop.Key + ' : ' +  varToStr(Prop.Value));
  Props.Free;

  LoadCountryFlags(Image1.Picture, IP.FcountryCode);

  IP.Free;

end;

procedure TForm2.LinkLabel2LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), PChar(''), nil, SW_SHOWNORMAL);
end;

end.
