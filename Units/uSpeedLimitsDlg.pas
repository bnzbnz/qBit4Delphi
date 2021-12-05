unit uSpeedLimitsDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TSpeedLimitsDlg = class(TForm)
    Label1: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function TrackBarToNiceValue(T: TTrackBar): string;
    procedure SetSpeedLimits(Up, Dl, AUp, ADl : integer);
    procedure GetSpeedLimits(var Up, Dl, AUp, ADl: variant);
  end;

var
  SpeedLimitsDlg: TSpeedLimitsDlg;

implementation

{$R *.dfm}

procedure TSpeedLimitsDlg.GetSpeedLimits(var Up, DL, AUp, ADl: variant);
  function GetTrackBarRealSpeed(T: TTrackbar): integer;
  begin
    if T.Position > 1023 then
      Result := (T.Position - 1023) * 1024 * 1024
    else
      if T.Position > 0 then
        Result := T.Position *1024
      else
        Result := 0;
  end;
begin
  Up := GetTrackBarRealSpeed(TrackBar1);
  Dl := GetTrackBarRealSpeed(TrackBar2);
  AUp := GetTrackBarRealSpeed(TrackBar3);
  ADl := GetTrackBarRealSpeed(TrackBar4);
end;

procedure TSpeedLimitsDlg.SetSpeedLimits(Up, Dl, AUp, ADl: integer);
  function GetTrackBarPos(S: integer): integer;
  begin
    if (S div (1024*1024))>0 then
    begin
      Result := 1023 + (S div 1024*1024);
    end else
    if (S div 1024)>0 then
    begin
      Result := S div 1024;
    end else
      Result := 0;
  end;
begin
  TrackBar1.Position := GetTrackBarPos(Up);
  TrackBar2.Position := GetTrackBarPos(Dl);
  TrackBar3.Position := GetTrackBarPos(AUp);
  TrackBar4.Position := GetTrackBarPos(ADl);
end;

procedure TSpeedLimitsDlg.TrackBar1Change(Sender: TObject);
begin
  Label1.Caption := 'Uplocad Limit : ' + TrackBarToNiceValue(TrackBar1);
  Label2.Caption := 'Download Limit : ' + TrackBarToNiceValue(TrackBar2);
  Label3.Caption := 'Alt. Uplocad Limit : ' + TrackBarToNiceValue(TrackBar3);
  Label4.Caption := 'Alt. Download Limit : ' + TrackBarToNiceValue(TrackBar4);
end;

function TSpeedLimitsDlg.TrackBarToNiceValue(T: TTrackBar): string;
begin
  if T.Position = 0 then
  begin
    Result := '∞';
  end else
  if T.Position<1024 then
  begin
    Result :=  T.Position.ToString + ' KiB';
  end else begin
    Result :=  (T.Position - 1023).ToString + ' MiB';
  end;
end;

end.
