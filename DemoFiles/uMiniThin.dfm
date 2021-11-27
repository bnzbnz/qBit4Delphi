object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 555
  ClientWidth = 936
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 936
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 49
      Height = 13
      Caption = 'Category:'
    end
    object Label2: TLabel
      Left = 288
      Top = 14
      Width = 34
      Height = 13
      Caption = 'Tags: :'
    end
    object CBCat: TComboBox
      Left = 88
      Top = 11
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'All'
      OnSelect = CBCatSelect
      Items.Strings = (
        'All'
        'Unassigned')
    end
    object CBTag: TComboBox
      Left = 344
      Top = 11
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'All'
      OnSelect = CBCatSelect
      Items.Strings = (
        'All'
        'Unassigned')
    end
  end
  object SG: TStringGrid
    Left = 0
    Top = 41
    Width = 936
    Height = 514
    Align = alClient
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goColMoving, goFixedRowDefAlign]
    PopupMenu = PMGrid
    TabOrder = 1
    OnDblClick = SGDblClick
    OnSelectCell = SGSelectCell
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 16
    Top = 128
  end
  object PMGrid: TPopupMenu
    Left = 512
    Top = 112
    object Pause: TMenuItem
      Caption = 'Pause'
      object PauseSelected: TMenuItem
        Caption = 'Selected'
        OnClick = PauseSelectedClick
      end
      object All1: TMenuItem
        Caption = 'All'
        OnClick = All1Click
      end
    end
    object Resume1: TMenuItem
      Caption = 'Resume'
      object ResumeSelected: TMenuItem
        Caption = 'Selected'
        OnClick = ResumeSelectedClick
      end
      object ResumeAll: TMenuItem
        Caption = 'All'
        OnClick = ResumeAllClick
      end
    end
    object ToggleForceResume: TMenuItem
      Caption = 'Toggle Force Resume'
      OnClick = ToggleForceResumeClick
    end
  end
end
