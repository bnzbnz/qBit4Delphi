object FrmSimpleThreaded: TFrmSimpleThreaded
  Left = 0
  Top = 0
  Caption = 'Simple Threaded'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 13
  object LBTorrents: TListBox
    Left = 0
    Top = 0
    Width = 635
    Height = 299
    Align = alClient
    DoubleBuffered = True
    ItemHeight = 13
    Items.Strings = (
      'Loading...')
    ParentDoubleBuffered = False
    TabOrder = 0
  end
end
