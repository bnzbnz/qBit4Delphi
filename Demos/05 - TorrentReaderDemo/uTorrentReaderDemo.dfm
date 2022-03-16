object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Torrent File Reader :'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Button1: TButton
    Left = 0
    Top = 0
    Width = 635
    Height = 41
    Align = alTop
    Caption = 'Parse Torrent File'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 635
    Height = 258
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object FileOpenDialog1: TFileOpenDialog
    DefaultExtension = '*.torrent'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Torrent'
        FileMask = '*.torrent'
      end>
    Options = []
    Left = 8
    Top = 48
  end
end
