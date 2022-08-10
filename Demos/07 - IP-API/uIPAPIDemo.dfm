object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'IP-API '
  ClientHeight = 475
  ClientWidth = 399
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 81
    Width = 399
    Height = 394
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 360
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 399
    Height = 81
    Align = alTop
    TabOrder = 1
    object LabeledEdit1: TLabeledEdit
      Left = 11
      Top = 23
      Width = 294
      Height = 23
      EditLabel.Width = 291
      EditLabel.Height = 15
      EditLabel.Caption = 'Enter an IP or a Domain (Empty = your own external IP)'
      TabOrder = 0
      Text = ''
    end
    object Button1: TButton
      Left = 311
      Top = 21
      Width = 75
      Height = 23
      Caption = 'Lookup'
      Default = True
      TabOrder = 1
      OnClick = Button1Click
    end
    object LinkLabel2: TLinkLabel
      Left = 123
      Top = 56
      Width = 149
      Height = 19
      Caption = 
        '<a href="https://ip-api.com/docs/api:json">IP-API Json Documenta' +
        'tion</a>'
      TabOrder = 2
      OnLinkClick = LinkLabel2LinkClick
    end
  end
end
