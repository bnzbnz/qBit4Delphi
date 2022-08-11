object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'IP-API '
  ClientHeight = 544
  ClientWidth = 399
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 81
    Width = 399
    Height = 388
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitHeight = 319
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
  object Panel2: TPanel
    Left = 0
    Top = 469
    Width = 399
    Height = 75
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 400
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 397
      Height = 73
      Align = alClient
      Center = True
      Proportional = True
      ExplicitLeft = 160
      ExplicitTop = 8
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
  end
end
