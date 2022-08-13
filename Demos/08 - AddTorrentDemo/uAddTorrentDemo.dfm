object FrmAddTorrent: TFrmAddTorrent
  Left = 0
  Top = 0
  Caption = 'Add Torrent Demo'
  ClientHeight = 342
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    Caption = 'Drag&&Drop your torrents file on this form'
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Upload'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 635
    Height = 301
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object DlgOpenTorrent: TFileOpenDialog
    DefaultExtension = '.torrent'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Torrent'
        FileMask = '*.torrent'
      end>
    Options = [fdoAllowMultiSelect, fdoPathMustExist, fdoFileMustExist]
    Left = 504
  end
end
