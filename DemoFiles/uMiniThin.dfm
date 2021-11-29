object MiniThinForm: TMiniThinForm
  Left = 0
  Top = 0
  Caption = 'MiniThin'
  ClientHeight = 645
  ClientWidth = 1014
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 1014
    Height = 604
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 2
    ExplicitWidth = 991
    object Splitter1: TSplitter
      Left = 1
      Top = 309
      Width = 1012
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 1
      ExplicitWidth = 511
    end
    object Panel3: TPanel
      Left = 1
      Top = 312
      Width = 1012
      Height = 291
      Align = alBottom
      Caption = 'Panel3'
      TabOrder = 0
      ExplicitWidth = 989
      object PageControl1: TPageControl
        Left = 1
        Top = 1
        Width = 1010
        Height = 289
        ActivePage = TabSheet1
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 987
        ExplicitHeight = 225
        object TabSheet1: TTabSheet
          Caption = 'General'
          object SGDetails: TStringGrid
            Left = 0
            Top = 0
            Width = 1002
            Height = 261
            Align = alClient
            Color = clBtnFace
            ColCount = 10
            DefaultColWidth = 192
            DefaultRowHeight = -1
            FixedCols = 0
            RowCount = 20
            FixedRows = 0
            Options = [goVertLine, goHorzLine]
            ScrollBars = ssVertical
            TabOrder = 0
            OnSelectCell = SGDetailsSelectCell
            ExplicitWidth = 979
            ExplicitHeight = 197
          end
        end
      end
    end
    object SG: TStringGrid
      Left = 1
      Top = 1
      Width = 1012
      Height = 308
      Align = alClient
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goColMoving, goFixedColClick, goFixedRowClick, goFixedRowDefAlign]
      PopupMenu = PMGrid
      TabOrder = 1
      OnDblClick = SGDblClick
      OnFixedCellClick = SGFixedCellClick
      OnSelectCell = SGSelectCell
      ExplicitWidth = 989
      ExplicitHeight = 372
      RowHeights = (
        24
        24
        24
        24
        24)
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1014
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 991
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
  object Warning: TMemo
    Left = 183
    Top = 95
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
      'Any questions : qBit4Delphi@ea4d.com')
    TabOrder = 1
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
    object N1: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
      object DeleteTorrentOnly: TMenuItem
        Caption = 'Torrent Only'
        OnClick = DeleteTorrentOnlyClick
      end
      object DeleteWithData: TMenuItem
        Caption = 'With Data'
        OnClick = DeleteWithDataClick
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SetLocation1: TMenuItem
      Caption = 'Set Location'
      OnClick = SetLocation1Click
    end
    object Rename1: TMenuItem
      Caption = 'Rename'
      OnClick = Rename1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object oDo1: TMenuItem
      Caption = 'ToDo...'
    end
  end
end
