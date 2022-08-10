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
uses uIP_API, RTTI, ShellAPI;
{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
  var IP := TIP_API.FromURL(Self.LabeledEdit1.Text);
  if IP = nil then Exit;
   var rttictx := TRttiContext.Create();
   var rttitype := rttictx.GetType(TIP_API);
    for var field in rttitype.GetFields do
      begin
       var v :=  field.GetValue(IP).asVariant;
        Memo1.Lines.Add('  ' + field.Name + ' : ' +  varToStr(v));
      end;
  rttictx.Free;
  IP.Free;
end;

procedure TForm2.LinkLabel2LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), PChar(''), nil, SW_SHOWNORMAL);
end;

end.
