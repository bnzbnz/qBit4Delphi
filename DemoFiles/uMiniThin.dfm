object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MiniThin'
  ClientHeight = 476
  ClientWidth = 724
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 724
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 936
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
    Width = 724
    Height = 435
    Align = alClient
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goColMoving, goFixedRowDefAlign]
    PopupMenu = PMGrid
    TabOrder = 1
    OnDblClick = SGDblClick
    OnSelectCell = SGSelectCell
    ExplicitWidth = 936
    ExplicitHeight = 514
  end
  object Warning: TMemo
    Left = 55
    Top = 175
    Width = 618
    Height = 282
    Lines.Strings = (
      'WARNING... WARNING... WARNING... (this is said) :'
      ''
      
        'In order to build and use qBit4Delphi some Embarcadero units nee' +
        'd to be patched (Fixes and Enhancements)'
      
        'This version is tested and patched against Delphi 10.4.2 (Sydney' +
        ') and Delphi 11 (Alexandria) :'
      ''
      'Copy :'
      #9'REST.Json.pas'
      #9'REST.Json.Types.pas'
      #9'REST.JsonReflect.pas'
      #9'System.JSON.pas'
      ''
      'from the original source folder : '
      
        #9'C:\Program Files (x86)\Embarcadero\Studio\XX\source\...   (XX b' +
        'eing 21.0 or 22.0)'#9
      'to'
      #9'JSON/Sydney.10.4.2 or JSON/Alexandria.11.0'
      ''
      
        #9'then go in that folder and execute PatchDelphiUnits.bat: Build ' +
        'and Run this app., it should work.'
      #9'(Remember to always add this patched units to your projects)'
      ''
      'qBit4Delphi@ea4d.com')
    TabOrder = 2
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 8
    Top = 48
  end
  object PMGrid: TPopupMenu
    Left = 8
    Top = 96
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
