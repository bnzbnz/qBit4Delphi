unit uTvnStatDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TvnStatDemoFrm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  vnStatDemoFrm: TvnStatDemoFrm;

implementation
uses REST.Json, System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent, uTvnStatClient;

{$R *.dfm}

procedure TvnStatDemoFrm.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
  var Vns := TvnStatClient.FromURL(Edit1.Text);
  if Vns = nil then
  begin
    Memo1.Lines.Add('Please check the URL / php file setup...');
    exit;
  end;
  var M := Vns.GetCurrentMonth(Edit2.Text);
  if M = nil then
  begin
    Memo1.Lines.Add('Wrong Interface name...');
    Vns.Free;
    exit;
  end;
  Memo1.Lines.Add(Format('tx: %8.2f TiB',[TvnStatClient.BtoTiB(M.Ftx)]));
  Memo1.Lines.Add(Format('rx: %8.2f TiB',[TvnStatClient.BtoTiB(M.Frx)]));
  Memo1.Lines.Add(Format('Total: %8.2f TiB, %8.2f TB', [
    TvnStatClient.BtoTiB(M.Ftx + M.Frx),
    TvnStatClient.BtoTB(M.Ftx + M.Frx)
  ]));
  Memo1.lines.Add('');
  Memo1.lines.Add('Raw Data : copy/paste in https://jsonformatter.curiousconcept.com/ to get a cute view :');
  Memo1.Lines.Add(Vns.Fraw);
  Memo1.Lines.Insert(0,''); // Scroll to Top
  Vns.Free;
end;

end.
