object AddTorrentDlg: TAddTorrentDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Torrents'
  ClientHeight = 392
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 72
    Width = 505
    Height = 281
  end
  object Label1: TLabel
    Left = 16
    Top = 80
    Width = 137
    Height = 13
    Caption = 'Torrent Management Mode :'
  end
  object Label2: TLabel
    Left = 16
    Top = 130
    Width = 111
    Height = 13
    Caption = 'Save Files to Location :'
  end
  object Label3: TLabel
    Left = 16
    Top = 180
    Width = 85
    Height = 13
    Caption = 'Rename Torrent :'
  end
  object Label4: TLabel
    Left = 16
    Top = 230
    Width = 52
    Height = 13
    Caption = 'Category :'
  end
  object Label5: TLabel
    Left = 287
    Top = 284
    Width = 104
    Height = 13
    Caption = 'Limit Download Rate :'
  end
  object Label6: TLabel
    Left = 287
    Top = 314
    Width = 90
    Height = 13
    Caption = 'Limit Upload Rate :'
  end
  object Label7: TLabel
    Left = 16
    Top = 280
    Width = 40
    Height = 13
    Caption = 'Layout :'
  end
  object TTM: TComboBox
    Left = 31
    Top = 98
    Width = 210
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'Manual'
    OnChange = TTMChange
    Items.Strings = (
      'Manual'
      'Automatic')
  end
  object SFL: TEdit
    Left = 31
    Top = 148
    Width = 210
    Height = 21
    TabOrder = 1
    Text = 'SFL'
  end
  object RT: TEdit
    Left = 31
    Top = 198
    Width = 210
    Height = 21
    TabOrder = 2
  end
  object CBCat: TComboBox
    Left = 31
    Top = 248
    Width = 210
    Height = 21
    Style = csDropDownList
    TabOrder = 3
  end
  object Button1: TButton
    Left = 357
    Top = 359
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 4
    OnClick = Button1Click
  end
  object CBST: TCheckBox
    Left = 287
    Top = 100
    Width = 97
    Height = 17
    Caption = 'Start Torrent'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object CBSHT: TCheckBox
    Left = 287
    Top = 150
    Width = 97
    Height = 17
    Caption = 'Skip Hash Check'
    TabOrder = 6
  end
  object CBCL: TComboBox
    Left = 31
    Top = 297
    Width = 210
    Height = 21
    ItemIndex = 0
    TabOrder = 7
    Text = 'Original'
    Items.Strings = (
      'Original'
      'Subfolder'
      'NoSubfolder')
  end
  object CBDSO: TCheckBox
    Left = 287
    Top = 200
    Width = 193
    Height = 17
    Caption = 'Download in Sequential Order'
    TabOrder = 8
  end
  object CBFLP: TCheckBox
    Left = 287
    Top = 250
    Width = 193
    Height = 17
    Caption = 'Download First and Last Pieces First'
    TabOrder = 9
  end
  object ComboBox1: TComboBox
    Left = 455
    Top = 284
    Width = 41
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 10
    Text = 'KiB'
    Items.Strings = (
      'KiB'
      'MiB')
  end
  object SpinEdit1: TSpinEdit
    Left = 397
    Top = 284
    Width = 52
    Height = 22
    MaxValue = 1024
    MinValue = -1
    TabOrder = 11
    Value = -1
  end
  object SpinEdit2: TSpinEdit
    Left = 397
    Top = 312
    Width = 52
    Height = 22
    MaxValue = 1024
    MinValue = -1
    TabOrder = 12
    Value = -1
  end
  object ComboBox2: TComboBox
    Left = 455
    Top = 311
    Width = 41
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 13
    Text = 'KiB'
    Items.Strings = (
      'KiB'
      'MiB')
  end
  object LBFiles: TListBox
    Left = 8
    Top = 8
    Width = 505
    Height = 58
    ItemHeight = 13
    TabOrder = 14
  end
  object Button2: TButton
    Left = 438
    Top = 359
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 15
  end
end
