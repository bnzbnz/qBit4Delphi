unit uFMXReport;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls,
  uqBitObject, uqBitAPI, uqBitAPITypes, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TFrmFMXReport = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmFMXReport: TFrmFMXReport;

implementation
uses RTTI, uqBitUtils, uqBitPatchChecker;

{$R *.fmx}

procedure TFrmFMXReport.Button1Click(Sender: TObject);
var
  qB: TqBitObject;
  M: TqBitMainDataType;
begin
  qB := TqBitObject.Connect(Edit1.Text, Edit2.Text, Edit3.Text);
  if not assigned(qB) then
  begin
    ShowMessage('Can''t Connect to ' + Edit1.Text);
    Exit;
  end;
  M := qB.GetMainData;

    Memo1.ClearContent;
    Memo1.Lines.Add(Format('qBit4Delphi : %s ==>', [qB.qBitVersion]));
    Memo1.Lines.Add(Format('******************* Server : %s *******************',[qB.HostPath]));

    Memo1.Lines.Add(Format('  Version : %s', [qB.GetVersion]));
    Memo1.Lines.Add(Format('  API : %s', [qB.GetAPIVersion]));
    var B := qB.GetBuildInfo;
    Memo1.Lines.Add(Format('  Libtorrent : %s', [B.Flibtorrent]));
    Memo1.Lines.Add(Format('  OpenSSL : %s', [B.Fopenssl]));
    Memo1.Lines.Add(Format('  Qt : %s', [B.Fqt]));
    Memo1.Lines.Add(Format('  Boost : %s', [B.Fboost]));
    B.Free;

    var Props := TqBitUtils.GetRTTIReadableValues(M.Fserver_state, TqBitserver_stateType);
    for var Prop in Props do
      Memo1.Lines.Add('  ' + Prop.key + ' : ' +  VarToStr(Prop.Value));
    Props.Free;

    Memo1.Lines.Add('******************* Preferences *******************');
    var Q := qB.GetPreferences;
    Props := TqBitUtils.GetRTTIReadableValues(Q, TqBitPreferencesType);
    for var Prop in Props do
      Memo1.Lines.Add('  ' + Prop.key + ' : ' +  VarToStr(Prop.Value));
    Props.Free;
    Q.Free;

    Memo1.Lines.Add(Format('******************* Torrents : %d *******************',[M.Ftorrents.Count]));
    for var T in M.Ftorrents do
    begin
      Memo1.Lines.Add(Format('  ************* Torrent : %s *******************',[TqBitTorrentType(T.Value).Fname]));
      Props := TqBitUtils.GetRTTIReadableValues(T.Value, TqBitTorrentType);
      for var Prop in Props do
      Memo1.Lines.Add('  ' + Prop.key + ' : ' +  VarToStr(Prop.Value));
      Props.Free;
    end;

    Memo1.Lines.Add(Format('******************* Categories : %d *******************',[M.Fcategories.Count]));
    for var C in M.Fcategories do
    begin
      Memo1.Lines.Add(Format('  ************* Categorie : %s *******************',[TqBitCategoryType(C.Value).Fname]));
      Props := TqBitUtils.GetRTTIReadableValues(C.Value, TqBitCategoryType);
      for var Prop in Props do
      Memo1.Lines.Add('  ' + Prop.key + ' : ' +  VarToStr(Prop.Value));
      Props.Free;
    end;

    Memo1.Lines.Add(Format('******************* Tags : %d *******************',[M.Ftags.Count]));
    for var G in M.Ftags do
    Memo1.Lines.Add('  ' + G);

  M.Free;
  qB.Free;
end;

end.
