object FrmSTG: TFrmSTG
  Left = 0
  Top = 0
  Caption = 'Simple Threaded Grid'
  ClientHeight = 544
  ClientWidth = 1050
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
  object Splitter1: TSplitter
    Left = 0
    Top = 325
    Width = 1050
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 328
  end
  inline MainFrame: TqBitFrame
    Left = 0
    Top = 0
    Width = 1050
    Height = 325
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 1050
    ExplicitHeight = 325
    inherited SG: TStringGrid
      Width = 1050
      Height = 325
      ExplicitWidth = 1050
      ExplicitHeight = 325
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 328
    Width = 1050
    Height = 216
    ActivePage = PeersTabSheet
    Align = alBottom
    TabOrder = 1
    OnChange = PageControl1Change
    object PeersTabSheet: TTabSheet
      Caption = 'Peers'
      inline PeersFrame: TqBitFrame
        Left = 0
        Top = 0
        Width = 1042
        Height = 169
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 1042
        ExplicitHeight = 169
        inherited SG: TStringGrid
          Width = 1042
          Height = 169
          ExplicitWidth = 1042
          ExplicitHeight = 169
        end
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 169
        Width = 1042
        Height = 19
        Panels = <>
      end
    end
    object TrakersTabSheet: TTabSheet
      Caption = 'Trackers'
      ImageIndex = 1
      inline TrackersFrame: TqBitFrame
        Left = 0
        Top = 0
        Width = 1042
        Height = 188
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 1042
        ExplicitHeight = 188
        inherited SG: TStringGrid
          Width = 1042
          Height = 188
          ExplicitWidth = 1042
          ExplicitHeight = 188
        end
      end
    end
  end
  object MainPopup: TPopupMenu
    Left = 152
    Top = 128
    object Pause1: TMenuItem
      Caption = 'Pause'
      OnClick = PauseClick
    end
    object Pause2: TMenuItem
      Caption = 'Resume'
      OnClick = ResumeClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ShowSelection1: TMenuItem
      Caption = 'Show Selection'
      OnClick = ShowSelection1Click
    end
  end
  object PeersPopup: TPopupMenu
    Left = 460
    Top = 424
    object BanPeers1: TMenuItem
      Caption = 'Ban Peers'
      OnClick = BanPeers1Click
    end
    object UnbanAll1: TMenuItem
      Caption = 'Unban All'
      OnClick = UnbanAll1Click
    end
  end
end
