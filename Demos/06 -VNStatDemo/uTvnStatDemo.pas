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
  var Vns := TvnStatClient.GetURL(Edit1.Text);
  var M := Vns.GetCurrentMonth;
  Memo1.Clear;
  Memo1.Lines.Add(Format('tx: %8.2f TiB',[TvnStatClient.BtoTiB(M.Ftx)]));
  Memo1.Lines.Add(Format('rx: %8.2f TiB',[TvnStatClient.BtoTiB(M.Frx)]));
  Memo1.Lines.Add(Format('Toral: %8.2f TiB',[TvnStatClient.BtoTiB(M.Ftx + M.Frx)]));
  Memo1.lines.Add('');
  Memo1.lines.Add('Raw Data : copy/paste in https://jsonformatter.curiousconcept.com/ to get a cute view :');
  Memo1.Lines.Add(Vns.Raw);
  Memo1.Lines.Insert(0,''); // Scroll to Top
  Vns.Free;
end;

end.
