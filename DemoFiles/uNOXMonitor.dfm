object MonForm: TMonForm
  Left = 0
  Top = 0
  Caption = 'NOX / qBittorrent Monitor'
  ClientHeight = 348
  ClientWidth = 1024
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1024
    Height = 348
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Servers'
      object Panel1: TPanel
        Left = 0
        Top = 277
        Width = 1016
        Height = 41
        Align = alBottom
        TabOrder = 1
        DesignSize = (
          1016
          41)
        object Button1: TButton
          Left = 896
          Top = 6
          Width = 109
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Update'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 1016
        Height = 277
        Align = alClient
        Lines.Strings = (
          '| |==> List of : HostPath | Username | Password'
          'http://localhost:8080||')
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Monitor'
      ImageIndex = 1
      object StringGrid1: TStringGrid
        Left = 0
        Top = 0
        Width = 1016
        Height = 318
        Align = alClient
        ColCount = 10
        DefaultColWidth = 100
        RowCount = 10
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goFixedRowDefAlign]
        TabOrder = 0
        ExplicitTop = 334
        ExplicitWidth = 1105
        ExplicitHeight = 232
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 16
    Top = 304
  end
end
