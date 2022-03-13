unit uFMXReport;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls,
  uqBitObject, uqBitAPI, uqBitAPITypes, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Warning: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses RTTI, uPatcherChecker;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
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
  Memo1.Lines.Add(Format('qBit4Delphi : %s ==>', [qB.qBitAPIVersion]));
  Memo1.Lines.Add(Format('******************* Server : %s *******************',[qB.HostPath]));

  Memo1.Lines.Add(Format('  Version : %s', [qB.GetVersion]));
  Memo1.Lines.Add(Format('  API : %s', [qB.GetAPIVersion]));
  var B := qB.GetBuildInfo;
  Memo1.Lines.Add(Format('  Libtorrent : %s', [B.Flibtorrent]));
  Memo1.Lines.Add(Format('  OpenSSL : %s', [B.Fopenssl]));
  Memo1.Lines.Add(Format('  Qt : %s', [B.Fqt]));
  Memo1.Lines.Add(Format('  Boost : %s', [B.Fboost]));
  B.Free;
  var rttictx := TRttiContext.Create();
  try
    var rttitype := rttictx.GetType(TqBitserver_stateType);
    for var field in rttitype.GetFields do
      if field.FieldType.TypeKind = tkVariant then
      begin
        var v :=  field.GetValue(M.Fserver_state).asVariant;
        Memo1.Lines.Add('  ' + field.Name + ' : ' +  varToStr(v));
      end;

      Memo1.Lines.Add('******************* Preferences *******************');
      var Q := qB.GetPreferences;
        var rttitype4:= rttictx.GetType(TqBitPreferencesType);
     for var field in rttitype4.GetFields do
      if field.FieldType.TypeKind = tkVariant then
      begin
       var v :=  field.GetValue(Q).asVariant;
        Memo1.Lines.Add('  ' + field.Name + ' : ' +  varToStr(v));
      end;
      Q.Free;

     Memo1.Lines.Add(Format('******************* Torrents : %d *******************',[M.Ftorrents.Count]));
     var rttitype2 := rttictx.GetType(TqBitTorrentType);
     for var T in M.Ftorrents do
     begin
       Memo1.Lines.Add(Format('  ************* Torrent : %s *******************',[TqBitTorrentType(T.Value).Fname]));
       for var field in rttitype2.GetFields do
        if field.FieldType.TypeKind = tkVariant then
        begin
         var v :=  field.GetValue(T.Value).asVariant;
          Memo1.Lines.Add('  ' + field.Name + ' : ' +  varToStr(v));
        end;
     end;

     Memo1.Lines.Add(Format('******************* Categories : %d *******************',[M.Fcategories.Count]));
     var rttitype3 := rttictx.GetType(TqBitCategoryType);
     for var C in M.Fcategories do
     begin
       Memo1.Lines.Add(Format('  ************* Categorie : %s *******************',[TqBitTorrentType(C.Value).Fname]));
       for var field in rttitype3.GetFields do
        if field.FieldType.TypeKind = tkVariant then
        begin
         var v :=  field.GetValue(C.Value).asVariant;
          Memo1.Lines.Add('  ' + field.Name + ' : ' +  varToStr(v));
        end;
     end;

     Memo1.Lines.Add(Format('******************* Tags : %d *******************',[M.Ftags.Count]));
     for var G in M.Ftags do
      Memo1.Lines.Add('  ' + G);

  finally
    rttictx.Free;
  end;

  M.Free;
  qB.Free;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
  Warning.Visible := False;
end;

initialization
  PatcherChecker;
end.
