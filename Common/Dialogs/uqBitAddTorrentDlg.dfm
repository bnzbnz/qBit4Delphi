object qBitAddTorrentDlg: TqBitAddTorrentDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Torrents :'
  ClientHeight = 634
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 503
    Height = 634
    Align = alClient
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 595
      Width = 501
      Height = 38
      Align = alBottom
      TabOrder = 0
      object Label12: TLabel
        Left = 21
        Top = 21
        Width = 194
        Height = 11
        Caption = '( Press CTRL while opening to show up again )'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object ChkDefault: TCheckBox
        Left = 20
        Top = 4
        Width = 97
        Height = 15
        Hint = 
          'Hide this dialog with these current parameters as default. Reena' +
          'ble by "shifting" while opening.'
        Caption = 'Use as Default'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object BtnCancel: TButton
        Left = 415
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object BtnOK: TButton
        Left = 329
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Upload'
        ModalResult = 1
        TabOrder = 2
        OnClick = BtnOKClick
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 97
      Width = 501
      Height = 498
      Align = alClient
      TabOrder = 1
      object Bevel1: TBevel
        Left = 8
        Top = 6
        Width = 485
        Height = 283
      end
      object Label1: TLabel
        Left = 20
        Top = 22
        Width = 137
        Height = 13
        Caption = 'Torrent Management Mode :'
      end
      object Label2: TLabel
        Left = 20
        Top = 76
        Width = 118
        Height = 13
        Caption = 'Custom Download Path :'
      end
      object Label4: TLabel
        Left = 20
        Top = 130
        Width = 52
        Height = 13
        Caption = 'Category :'
      end
      object Label3: TLabel
        Left = 20
        Top = 183
        Width = 85
        Height = 13
        Caption = 'Rename Torrent :'
      end
      object Label5: TLabel
        Left = 290
        Top = 170
        Width = 104
        Height = 13
        Caption = 'Limit Download Rate :'
      end
      object Label6: TLabel
        Left = 290
        Top = 225
        Width = 90
        Height = 13
        Caption = 'Limit Upload Rate :'
      end
      object Label7: TLabel
        Left = 20
        Top = 233
        Width = 40
        Height = 13
        Caption = 'Layout :'
      end
      object Label11: TLabel
        Left = 20
        Top = 406
        Width = 52
        Height = 13
        Caption = 'Comment :'
      end
      object ComboBox2: TComboBox
        Left = 393
        Top = 244
        Width = 51
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 13
        Text = 'MiB'
        Items.Strings = (
          'KiB'
          'MiB')
      end
      object ComboBox1: TComboBox
        Left = 393
        Top = 189
        Width = 51
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 11
        Text = 'MiB'
        Items.Strings = (
          'KiB'
          'MiB')
      end
      object SpinEdit2: TSpinEdit
        Left = 335
        Top = 244
        Width = 52
        Height = 22
        MaxValue = 1024
        MinValue = 0
        TabOrder = 12
        Value = 0
      end
      object SpinEdit1: TSpinEdit
        Left = 335
        Top = 189
        Width = 52
        Height = 22
        MaxValue = 1024
        MinValue = 0
        TabOrder = 10
        Value = 0
      end
      object TTM: TComboBox
        Left = 20
        Top = 41
        Width = 219
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
        Left = 20
        Top = 95
        Width = 219
        Height = 21
        TabOrder = 1
      end
      object RT: TEdit
        Left = 20
        Top = 202
        Width = 219
        Height = 21
        TabOrder = 4
      end
      object CBCat: TComboBox
        Left = 20
        Top = 149
        Width = 219
        Height = 21
        Style = csDropDownList
        TabOrder = 2
      end
      object CBCL: TComboBox
        Left = 20
        Top = 252
        Width = 219
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 5
        Text = 'Original'
        Items.Strings = (
          'Original'
          'Subfolder'
          'NoSubfolder')
      end
      object CBST: TCheckBox
        Left = 290
        Top = 45
        Width = 97
        Height = 17
        Caption = 'Start Torrent'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object CBSHT: TCheckBox
        Left = 290
        Top = 75
        Width = 97
        Height = 17
        Caption = 'Skip Hash Check'
        TabOrder = 7
      end
      object CBDSO: TCheckBox
        Left = 290
        Top = 138
        Width = 193
        Height = 17
        Caption = 'Download in Sequential Order'
        TabOrder = 9
      end
      object CBFLP: TCheckBox
        Left = 290
        Top = 107
        Width = 193
        Height = 17
        Caption = 'Download First and Last Pieces First'
        TabOrder = 8
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 303
        Width = 485
        Height = 97
        Caption = 'Torrent Info :'
        TabOrder = 14
        object TILblName: TLabel
          Left = 12
          Top = 28
          Width = 34
          Height = 13
          Caption = 'Name :'
        end
        object TILblSize: TLabel
          Left = 12
          Top = 47
          Width = 26
          Height = 13
          Caption = 'Size :'
        end
        object Label8: TLabel
          Left = 12
          Top = 66
          Width = 69
          Height = 13
          Caption = 'Info Hash V1 :'
        end
        object Label9: TLabel
          Left = 12
          Top = 85
          Width = 69
          Height = 13
          Caption = 'Info Hash V2 :'
        end
        object Label10: TLabel
          Left = 13
          Top = 96
          Width = 69
          Height = 13
          Caption = 'Info Hash V2 :'
        end
        object Edit1: TEdit
          Left = -188
          Top = -32
          Width = 1000
          Height = 21
          BevelInner = bvNone
          BevelOuter = bvNone
          ParentColor = True
          ReadOnly = True
          TabOrder = 0
          Text = 'Edit1'
        end
        object TIEditName: TEdit
          Left = 84
          Top = 28
          Width = 410
          Height = 17
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 1
          Text = 'TIEditName'
        end
        object TIEditSize: TEdit
          Left = 84
          Top = 47
          Width = 121
          Height = 17
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 2
          Text = 'TIEditName'
        end
        object TIEditHashV1: TEdit
          Left = 84
          Top = 66
          Width = 401
          Height = 17
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 3
          Text = 'TIEditName'
        end
        object TIEditHashV2: TEdit
          Left = 84
          Top = 85
          Width = 398
          Height = 17
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clBtnFace
          ReadOnly = True
          TabOrder = 4
          Text = 'TIEditName'
        end
      end
      object TIEditComment: TMemo
        Left = 92
        Top = 406
        Width = 398
        Height = 79
        Lines.Strings = (
          'TIEditComment')
        ParentColor = True
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 15
      end
      object BtnMgeCat: TButton
        Left = 237
        Top = 149
        Width = 19
        Height = 21
        Margins.Right = 6
        Caption = '....'
        TabOrder = 3
        OnClick = BtnMgeCatClick
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 501
      Height = 96
      Align = alTop
      Caption = 'Panel4'
      TabOrder = 2
      object LBFiles: TListBox
        Left = 8
        Top = 8
        Width = 485
        Height = 82
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBFilesClick
      end
    end
  end
end
