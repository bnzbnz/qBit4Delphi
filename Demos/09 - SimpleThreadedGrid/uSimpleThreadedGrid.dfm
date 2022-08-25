object FrmSTG: TFrmSTG
  Left = 0
  Top = 0
  Caption = 'Simple Threaded Grid'
  ClientHeight = 431
  ClientWidth = 961
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 961
    Height = 26
    Align = alTop
    Color = clGradientActiveCaption
    ParentBackground = False
    TabOrder = 0
  end
  inline MainFrame: TqBitFrame
    Left = 0
    Top = 26
    Width = 961
    Height = 405
    Align = alClient
    TabOrder = 1
    ExplicitTop = 26
    ExplicitWidth = 961
    ExplicitHeight = 405
    inherited SG: TStringGrid
      Width = 961
      Height = 405
      ExplicitWidth = 961
      ExplicitHeight = 405
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
end
