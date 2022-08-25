unit uSimpleThreadedGrid;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uqBitObject, uqBitAPI, uqBitAPITypes,
  Vcl.ExtCtrls, Vcl.StdCtrls, uqBitGrid, uqBitThreads, Vcl.Menus;

type
  TFrmSTG = class(TForm)
    Panel1: TPanel;
    MainFrame: TqBitFrame;
    MainPopup: TPopupMenu;
    Pause1: TMenuItem;
    Pause2: TMenuItem;
    N1: TMenuItem;
    ShowSelection1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PauseClick(Sender: TObject);
    procedure ResumeClick(Sender: TObject);
    procedure ShowSelection1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    qB: TqBitObject;
    MainThread: TqBitMainThread;
    procedure MainThreadEvent(qBitThread: TThread; EventType: TqBitThreadEventCode);
    procedure MainFramUpdateEvent(Sender: TObject);
    procedure MainFramePopupEvent(Sender: TObject; X, Y, aCol, aRow: integer);
  end;

var
  FrmSTG: TFrmSTG;

{$R *.dfm}

implementation
uses ShellAPI, uqBitPatchChecker, uqBitSelectServerDlg, uqBitFormat,
     RTTI,  System.Generics.Collections, System.Generics.Defaults;

procedure TFrmSTG.FormShow(Sender: TObject);
begin
  if qBitSelectServerDlg.ShowModal = mrOk then
  begin
    var Server := qBitSelectServerDlg.GetServer;
    qB := TqBitObject.Connect(Server.FHP, Server.FUN, Server.FPW);

    MainFrame.DoCreate;
    MainFrame.OnUpdateUIEvent := MainFramUpdateEvent;
    MainFrame.OnPopupEvent := self.MainFramePopupEvent;

    MainFrame.AddCol('Name', 'Fname', VarFormatString, 320, True);

    MainFrame.AddCol('Size', 'Fsize', VarFormatBKM, 84, True);
    MainFrame.AddCol('Total Size', 'Ftotal_size', VarFormatBKM, -1, True);
    MainFrame.AddCol('Progress', 'Fprogress', VarFormatPercent, 84, True);
    MainFrame.AddCol('Status', 'Fstate', VarFormatString, 84, True);
    MainFrame.AddCol('Seeds', 'Fnum_seeds', VarFormatString, 84, True);
    MainFrame.AddCol('Peers', 'Fnum_leechs', VarFormatString, 84, True);
    MainFrame.AddCol('Ratio', 'Fratio', VarFormatFloat2d, 36, True);
    MainFrame.AddCol('Down Speed', 'Fdlspeed', VarFormatBKMPerSec, 84, True);
    MainFrame.AddCol('Upload Speed', 'Fupspeed', VarFormatBKMPerSec, 84, True);
    MainFrame.AddCol('ETA', 'Feta', VarFormatDeltaSec, 128, True);
    MainFrame.AddCol('Category', 'Fcategory', VarFormatString, 84, True);
    MainFrame.AddCol('Tags', 'Ftags', VarFormatString, 84, True);
    MainFrame.AddCol('Added On', 'Fadded_on', VarFormatDate, 128, True);
    MainFrame.AddCol('Completed On', 'Fcompletion_on', VarFormatDate, -1, True);
    MainFrame.AddCol('Tracker', 'Ftracker', VarFormatString, -1, True);
    MainFrame.AddCol('Down Limit', 'Fdl_limit', VarFormatLimit, -1, True);
    MainFrame.AddCol('Up Limit', 'Fdl_limit', VarFormatLimit, -1, True);
    MainFrame.AddCol('Downloaded', 'Fdownloaded', VarFormatBKM, -1, True);
    MainFrame.AddCol('Uploaded  ', 'Fuploaded', VarFormatBKM, -1, True);
    MainFrame.AddCol('Session Downloaded', 'Fdownloaded_session', VarFormatBKM, -1, True);
    MainFrame.AddCol('Session Uploaded  ', 'Fuploaded_session', VarFormatBKM, -1, True);
    MainFrame.AddCol('Availability', 'Favailability', VarFormatMulti, -1, True);

    MainFrame.SortField := 'Fname';
    MainFrame.SortReverse := False;

    var rttictx := TRttiContext.Create();
    var rttitype := rttictx.GetType(TqBitTorrentType);
    for var field in rttitype.GetFields do
    begin
      var Title := 'Raw: ' + field.Name;
      if pos('_', field.Name) = 1 then continue;
      MainFrame.AddCol(Title, field.Name, VarFormatString, -2, False);
    end;
    rttictx.Free;

    MainThread := TqBitMainThread.Create(qB.Clone, MainThreadEvent);
  end else
    PostMessage(Handle, WM_CLOSE,0 ,0);
end;

procedure TFrmSTG.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainFrame.DoDestroy;
  MainThread.Free;
  qB.Free;
end;

procedure TFrmSTG.MainFramePopupEvent(Sender: TObject; X, Y, aCol, aRow: integer);
begin
  MainThread.Pause := True;  //Thread Safe;
  var Sel := MainFrame.GetGridSel;
  try
    if Sel.Count = 0 then Exit;
    MainPopup.Popup(X,Y);
  finally
    Sel.Free;
    MainThread.Pause := False;  //Thread Safe;
  end;
end;

procedure TFrmSTG.PauseClick(Sender: TObject);
begin
  var Keys := MainFrame.GetSelectedKeys;
  try
    if Keys.Count = 0 then Exit;
    qB.PauseTorrents(Keys);
  finally
    Keys.Free;
  end;
end;

procedure TFrmSTG.ResumeClick(Sender: TObject);
begin
  var Keys := MainFrame.GetSelectedKeys;
  try
    if Keys.Count = 0 then Exit;
    qB.ResumeTorrents(Keys);
  finally
    Keys.Free;
  end;
end;

procedure TFrmSTG.ShowSelection1Click(Sender: TObject);
begin
  var Sel := MainFrame.GetGridSel;
  try
    if Sel.Count = 0 then Exit;
    for var GridData in Sel do
    begin
      var Data := TqBitGridData(GridData);
      var Key :=  Data.Key;
      var Torrent := TqBitTorrentType(Data.Obj);
      ShowMessage( Torrent.Fhash + ' : ' + Torrent.Fname );
    end;
  finally
    Sel.Free;
  end;
end;

procedure TFrmSTG.MainFramUpdateEvent(Sender: TObject);
begin
  MainThread.Refresh := True; //Thread Safe;
end;

procedure TFrmSTG.MainThreadEvent(qBitThread: TThread; EventType: TqBitThreadEventCode);
begin
  //  qtetInit, qtetLoaded, qtetError, qtetBeforeMerge, qtetAfterMerge, qtetIdle, qtetExit
  var M := TqBitMainThread( qBitThread );
  case EventType of
    qtetLoaded, qtetAfterMerging:
    begin
      var SortList := TObjectList<TqBitTorrentType>.Create(False);

      if Assigned(M.Main.Ftorrents) then
      for var T in M.Main.Ftorrents do
        SortList.Add(TqBitTorrentType(T.Value));

      //Sorting
      var rttictx := TRttiContext.Create();
      var rttitype := rttictx.GetType(TqBitTorrentType);
      SortList.Sort(TComparer<TqBitTorrentType>.Construct(
          function (const L, R: TqBitTorrentType): integer
          begin
            Result := 0;
            for var Field in RttiType.GetFields do
            begin
              if Field.Name = Self.MainFrame.SortField then
              begin
                var LVal := Field.GetValue(L).asVariant;
                var RVal := Field.GetValue(R).asVariant;
                if LVal > RVal then
                  if Self.MainFrame.SortReverse then Result := -1 else Result := 1;
                if RVal > LVal then
                  if Self.MainFrame.SortReverse then Result := 1 else Result := -1;
              end;
            end;
          end
      ));
      rttictx.Free;

      // Displaying Grid
      MainFrame.RowUpdateStart;
      for var T in SortList do
        MainFrame.AddRow(TqBitTorrentType(T).Fhash, T);
      MainFrame.RowUpdateEnd;

      FreeAndNil(SortList);

    end;
    qtetError:
      begin
        ShowMessage('Disconnected');
        M.MustExit := True;
        PostMessage(Self.Handle, WM_CLOSE, 0, 0);;
      end;
    qtetIdle: ;
  end;
end;

end.
