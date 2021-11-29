object AddServerDlg: TAddServerDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add a qBittorrent Server :'
  ClientHeight = 204
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 281
    Height = 157
  end
  object BtnOK: TButton
    Left = 133
    Top = 171
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 3
    OnClick = BtnOKClick
  end
  object BtnCancel: TButton
    Left = 214
    Top = 171
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object HP: TLabeledEdit
    Left = 24
    Top = 32
    Width = 257
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = 'HostPath:'
    EditLabel.DragMode = dmAutomatic
    TabOrder = 0
    Text = 'http://127.0.0.1:8080'
  end
  object UN: TLabeledEdit
    Left = 24
    Top = 82
    Width = 257
    Height = 21
    EditLabel.Width = 55
    EditLabel.Height = 13
    EditLabel.Caption = 'Username :'
    EditLabel.DragMode = dmAutomatic
    TabOrder = 1
  end
  object PW: TLabeledEdit
    Left = 24
    Top = 132
    Width = 257
    Height = 21
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'Password :'
    EditLabel.DragMode = dmAutomatic
    TabOrder = 2
  end
end
