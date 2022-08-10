object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'IP-API '
  ClientHeight = 441
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
    Top = 57
    Width = 399
    Height = 384
    Align = alClient
    Lines.Strings = (
      ''
      'Documentation at : https://ip-api.com/docs/api:json')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 399
    Height = 57
    Align = alTop
    TabOrder = 1
    object LabeledEdit1: TLabeledEdit
      Left = 11
      Top = 23
      Width = 294
      Height = 23
      EditLabel.Width = 292
      EditLabel.Height = 15
      EditLabel.Caption = 'Enter an IP or a Domain (Empry means your external IP)'
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
  end
end
