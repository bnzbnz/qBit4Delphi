unit uExternalIPDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses uExternalIP;
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  var IPInfo := TExternalIP.FromURL();
  if assigned(IPInfo) then
  begin
    Label1.Caption := 'IP : ' + IPInfo.Fip;
    Label2.Caption := 'Hostname : ' + UTF8Decode(IPInfo.Fhostname);
    Label3.Caption := 'City : ' + UTF8Decode(IPInfo.Fcity);
    Label4.Caption := 'Region : ' + UTF8Decode(IPInfo.Fregion);
    Label5.Caption := 'Location : ' + UTF8Decode(IPInfo.Floc);
    Label6.Caption := 'Organization : ' + UTF8Decode(IPInfo.Forg);
    Label7.Caption := 'ZipCode : ' + UTF8Decode(IPInfo.Fpostal);
    Label8.Caption := 'Timezone : ' + UTF8Decode(IPInfo.Ftimezone);
    IPInfo.Free;
  end;
end;

end.
