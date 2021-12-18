object NOXMonDlg: TNOXMonDlg
  Left = 0
  Top = 0
  Caption = 'NOXMon'
  ClientHeight = 159
  ClientWidth = 1118
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SG: TStringGrid
    Left = 0
    Top = 0
    Width = 1118
    Height = 159
    Align = alClient
    ColCount = 20
    RowCount = 10
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goColMoving, goFixedRowDefAlign]
    TabOrder = 0
  end
end
