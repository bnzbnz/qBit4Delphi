object qBitCategoriesDlg: TqBitCategoriesDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Manage Categories :'
  ClientHeight = 209
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object Bevel2: TBevel
    Left = 135
    Top = 8
    Width = 257
    Height = 153
  end
  object Label1: TLabel
    Left = 151
    Top = 21
    Width = 34
    Height = 13
    Caption = 'Name :'
  end
  object Label2: TLabel
    Left = 151
    Top = 67
    Width = 56
    Height = 13
    Caption = 'Save path :'
  end
  object LblDefault: TLabel
    Left = 213
    Top = 67
    Width = 43
    Height = 13
    Caption = '(Default)'
  end
  object LBCat: TListBox
    Left = 8
    Top = 8
    Width = 121
    Height = 192
    ExtendedSelect = False
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBCatClick
    OnDblClick = LBCatDblClick
  end
  object EditName: TEdit
    Left = 151
    Top = 40
    Width = 225
    Height = 21
    TabOrder = 1
  end
  object EditPath: TEdit
    Left = 151
    Top = 86
    Width = 225
    Height = 21
    TabOrder = 2
    OnChange = EditPathChange
  end
  object Button1: TButton
    Left = 288
    Top = 125
    Width = 88
    Height = 25
    Caption = 'Add / Update'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 151
    Top = 125
    Width = 88
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = Button2Click
  end
  object BtnSelect: TButton
    Left = 210
    Top = 175
    Width = 88
    Height = 25
    Caption = 'Select'
    ModalResult = 1
    TabOrder = 5
    OnClick = BtnSelectClick
  end
  object BtnCancel: TButton
    Left = 304
    Top = 175
    Width = 88
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object BtnClose: TButton
    Left = 304
    Top = 176
    Width = 89
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 7
    Visible = False
  end
end
