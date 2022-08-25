unit uMultiThreadedGrids;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uqBitObject, uqBitAPI, uqBitAPITypes,
  Vcl.ExtCtrls, Vcl.StdCtrls, uqBitGrid, uqBitThreads, Vcl.Menus, Vcl.ComCtrls;

type
  TFrmSTG = class(TForm)
    MainFrame: TqBitFrame;
    MainPopup: TPopupMenu;
    Pause1: TMenuItem;
    Pause2: TMenuItem;
    N1: TMenuItem;
    ShowSelection1: TMenuItem;
    PageControl1: TPageControl;
    PeersTabSheet: TTabSheet;
    PeersFrame: TqBitFrame;
    TrakersTabSheet: TTabSheet;
    PeersPopup: TPopupMenu;
    BanPeers1: TMenuItem;
    UnbanAll1: TMenuItem;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    TrackersFrame: TqBitFrame;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PauseClick(Sender: TObject);
    procedure ResumeClick(Sender: TObject);
    procedure ShowSelection1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure UnbanAll1Click(Sender: TObject);
    procedure BanPeers1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    qB: TqBitObject;
    MainThread: TqBitMainThread;
    PeersThread: TqBitPeersThread;
    TrackersThread: TqBitTrackersThread;
    ActiveKeyHash: string;
    procedure MainThreadEvent(qBitThread: TThread; EventType: TqBitThreadEventCode);
    procedure MainFrameUpdateEvent(Sender: TObject);
    procedure MainFramePopupEvent(Sender: TObject; X, Y, aCol, aRow: integer);
    procedure MainFrameSelectEvent(Sender: TObject);

    procedure PeersFrameUpdateEvent(Sender: TObject);
    procedure PeersThreadEvent(qBitThread: TThread; EventType: TqBitThreadEventCode);
    procedure PeersFramePopupEvent(Sender: TObject; X, Y, aCol, aRow: integer);

    procedure TrackersThreadEvent(qBitThread: TThread; EventType: TqBitThreadEventCode);
    procedure TrackersFrameUpdateEvent(Sender: TObject);
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
    MainFrame.SortField := 'Fname';
    MainFrame.SortReverse := False;
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
    var rttictx := TRttiContext.Create();
    var rttitype := rttictx.GetType(TqBitTorrentType);
    for var field in rttitype.GetFields do
    begin
      var Title := 'Raw: ' + field.Name;
      if pos('_', field.Name) = 1 then continue;
      MainFrame.AddCol(Title, field.Name, VarFormatString, -2, False);
    end;
    rttictx.Free;
    MainFrame.OnUpdateUIEvent := MainFrameUpdateEvent;
    MainFrame.OnPopupEvent := self.MainFramePopupEvent;
    MainFrame.OnRowsSelectedEvent := self.MainFrameSelectEvent;

    PeersFrame.DoCreate;
    PeersFrame.SortField := 'Fip';
    PeersFrame.SortReverse := False;
    PeersFrame.AddCol('IP', 'Fip', VarFormatString, 104, True);
    PeersFrame.AddCol('Port', 'Fport', VarFormatString, 84, True);
    PeersFrame.AddCol('Country', 'Fcountry', VarFormatString, 84, True);
    PeersFrame.AddCol('Connection', 'Fconnection', VarFormatString, 72, True);
    PeersFrame.AddCol('Flags', 'Fflags', VarFormatString, 72, True);
    PeersFrame.AddCol('Client', 'Fclient', VarFormatString, 100, True);
    PeersFrame.AddCol('Progress', 'Fprogress', VarFormatPercent, 72, True);
    PeersFrame.AddCol('Down Speed', 'Fdl_speed', VarFormatBKMPerSec, 72, True);
    PeersFrame.AddCol('Up Speed', 'Fup_speed', VarFormatBKMPerSec, 72, True);
    PeersFrame.AddCol('Downloaded', 'Fdownloaded', VarFormatBKM, 72, True);
    PeersFrame.AddCol('Uploaded', 'Fuploaded', VarFormatBKM, 72, True);
    PeersFrame.SortField := 'Fip';
    PeersFrame.SortReverse := False;
    PeersFrame.OnPopupEvent := Self.PeersFramePopupEvent;

    TrackersFrame.DoCreate;

    TrackersFrame.AddCol('URL', 'Furl', VarFormatString, 208, True);
    TrackersFrame.AddCol('Tier', 'Ftier', VarFormatString, 32, True);
    TrackersFrame.AddCol('Status', 'Fstatus', VarFormatTrackerStatus, 84, True);
    TrackersFrame.AddCol('Peers', 'Fnum_peers', VarFormatString, 84, True);
    TrackersFrame.AddCol('Seeds', 'Fnum_seeds', VarFormatString, 84, True);
    TrackersFrame.AddCol('Leeches', 'Fnum_leeches', VarFormatString, 84, True);
    TrackersFrame.AddCol('Donwloaded', 'Fnum_downloaded', VarFormatBKM, 84, True);
    TrackersFrame.AddCol('Message', 'Fmsg', VarFormatString, 128, True);
    TrackersFrame.SortField := 'Furl';
    TrackersFrame.SortReverse := False;
    TrackersFrame.OnUpdateUIEvent := TrackersFrameUpdateEvent;


    MainThread := TqBitMainThread.Create(qB.Clone, MainThreadEvent);
    PeersThread := TqBitPeersThread.Create(qB.Clone, PeersThreadEvent);
    TrackersThread := TqBitTrackersThread.Create(qB.Clone, TrackersThreadEvent);
    TrackersThread.Pause := True;
  end else
    PostMessage(Handle, WM_CLOSE,0 ,0);
end;

procedure TFrmSTG.BanPeers1Click(Sender: TObject);
begin
  var Sel := PeersFrame.GetSelectedKeys;
  try
    if Sel.Count = 0 then Exit;
    qB.BanPeers(Sel);
  finally
    Sel.Free;
  end;
end;

procedure TFrmSTG.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TrackersThread.Free;
  PeersThread.Free;
  MainThread.Free;
  TrackersFrame.DoDestroy;
  PeersFrame.DoDestroy;
  MainFrame.DoDestroy;
  qB.Free;
end;

procedure TFrmSTG.MainFramePopupEvent(Sender: TObject; X, Y, aCol, aRow: integer);
begin
  var Sel := MainFrame.GetSelectedKeys;
  try
    if Sel.Count = 0 then Exit;
    MainPopup.Popup(X,Y);
  finally
    Sel.Free;
  end;
end;

procedure TFrmSTG.MainFrameSelectEvent(Sender: TObject);
begin
  var Keys := MainFrame.GetSelectedKeys;
  ActiveKeyHash := Keys[Keys.Count - 1];
  Keys.Free;
end;

procedure TFrmSTG.PageControl1Change(Sender: TObject);
begin
  PeersThread.KeyHash := Self.ActiveKeyHash;
  PeersThread.Pause := not (PageControl1.ActivePage = Self.PeersTabSheet);
  TrackersThread.KeyHash := Self.ActiveKeyHash;
  TrackersThread.Pause := not (PageControl1.ActivePage = Self.TrakersTabSheet);
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
  Self.MainThread.Pause := True;
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
    Self.MainThread.Pause := False;
  end;
end;

procedure TFrmSTG.UnbanAll1Click(Sender: TObject);
begin
  qB.UnbanAllPeers;
end;

procedure TFrmSTG.MainFrameUpdateEvent(Sender: TObject);
begin
  MainThread.Refresh := True; //Thread Safe;
end;

procedure TFrmSTG.TrackersFrameUpdateEvent(Sender: TObject);
begin
  TrackersThread.Refresh := True; //Thread Safe;
end;

procedure TFrmSTG.PeersFramePopupEvent(Sender: TObject; X, Y, aCol, aRow: integer);
begin
  PeersThread.Pause := True;
  var Sel := PeersFrame.GetSelectedKeys;
  try
    if Sel.Count = 0 then Exit;
    PeersPopup.Popup(X,Y);
  finally
    Sel.Free;
    PeersThread.Pause := False;
  end;
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

      // Display Grid
      MainFrame.RowUpdateStart;
      for var T in SortList do
        MainFrame.AddRow(TqBitTorrentType(T)._Key, T);
      MainFrame.RowUpdateEnd;

      FreeAndNil(SortList);

    end;
    qtetError:
      begin
        ShowMessage('Disconnected');
        M.MustExit := True;
        PostMessage(Self.Handle, WM_CLOSE, 0, 0);
      end;
    qtetIdle: ;
  end;
end;

procedure TFrmSTG.PeersFrameUpdateEvent(Sender: TObject);
begin
    PeersThread.Refresh := True;
end;

procedure TFrmSTG.PeersThreadEvent(qBitThread: TThread; EventType: TqBitThreadEventCode);
begin
  //  qtetInit, qtetLoaded, qtetError, qtetBeforeMerge, qtetAfterMerge, qtetIdle, qtetExit
  var P := TqBitPeersThread( qBitThread );
  case EventType of
    qtetLoaded, qtetAfterMerging:
    begin

      var SortList := TObjectList<TqBitTorrentPeerDataType>.Create(False);

      if Assigned(P.Peers.Fpeers) then
      for var T in P.Peers.Fpeers do
        SortList.Add(TqBitTorrentPeerDataType(T.Value));

      //Sorting
      var rttictx := TRttiContext.Create();
      var rttitype := rttictx.GetType(TqBitTorrentPeerDataType);
      SortList.Sort(TComparer<TqBitTorrentPeerDataType>.Construct(
          function (const L, R: TqBitTorrentPeerDataType): integer
          begin
            Result := 0;
            for var Field in RttiType.GetFields do
            begin
              if Field.Name = Self.PeersFrame.SortField then
              begin
                var LVal := Field.GetValue(L).asVariant;
                var RVal := Field.GetValue(R).asVariant;
                if LVal > RVal then
                  if Self.PeersFrame.SortReverse then Result := -1 else Result := 1;
                if RVal > LVal then
                  if Self.PeersFrame.SortReverse then Result := 1 else Result := -1;
              end;
            end;
          end
      ));
      rttictx.Free;

      // Display Grid
      PeersFrame.RowUpdateStart;
      for var T in SortList do
        PeersFrame.AddRow(TqBitTorrentPeerDataType(T)._Key, T);
      PeersFrame.RowUpdateEnd;

      FreeAndNil(SortList);

    end;
    qtetError: ;
    qtetIdle: P.KeyHash := ActiveKeyHash;
  end;
end;

procedure TFrmSTG.TrackersThreadEvent(qBitThread: TThread; EventType: TqBitThreadEventCode);
begin
  //  qtetInit, qtetLoaded, qtetError, qtetBeforeMerge, qtetAfterMerge, qtetIdle, qtetExit
  var R := TqBitTrackersThread( qBitThread );
  case EventType of
    qtetLoaded, qtetAfterMerging:
    begin

      var SortList := TObjectList<TqBitTrackerType>.Create(False);

      if Assigned(R.Trackers.Ftrackers) then
      for var T in R.Trackers.Ftrackers do
        SortList.Add(TqBitTrackerType(T));

      //Sorting
      var rttictx := TRttiContext.Create();
      var rttitype := rttictx.GetType(TqBitTrackerType);
      SortList.Sort(TComparer<TqBitTrackerType>.Construct(
          function (const L, R: TqBitTrackerType): integer
          begin
            Result := 0;
            for var Field in RttiType.GetFields do
            begin
              if Field.Name = Self.TrackersFrame.SortField then
              begin
                var LVal := Field.GetValue(L).asVariant;
                var RVal := Field.GetValue(R).asVariant;
                if LVal > RVal then
                  if Self.TrackersFrame.SortReverse then Result := -1 else Result := 1;
                if RVal > LVal then
                  if Self.TrackersFrame.SortReverse then Result := 1 else Result := -1;
              end;
            end;
          end
      ));
      rttictx.Free;

      // Display Grid
      TrackersFrame.RowUpdateStart;
      for var T in SortList do
        TrackersFrame.AddRow(TqBitTrackerType(T)._Key, T);
      TrackersFrame.RowUpdateEnd;

      FreeAndNil(SortList);

    end;
    qtetError: ;
    qtetIdle: R.KeyHash := ActiveKeyHash;
  end;
end;
end.
