object qNOXifyFrm: TqNOXifyFrm
  Left = 0
  Top = 0
  Caption = 'qNOXifyFrm'
  ClientHeight = 524
  ClientWidth = 961
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 505
    Width = 961
    Height = 19
    Panels = <
      item
        Text = 'Connection Status'
        Width = 100
      end
      item
        Text = 'Free Space'
        Width = 164
      end
      item
        Text = 'DHT'
        Width = 120
      end
      item
        Text = 'AltS'
        Width = 26
      end
      item
        Text = 'DSpeed'
        Width = 164
      end
      item
        Text = 'UlSpeed'
        Width = 164
      end>
    ParentShowHint = False
    ShowHint = False
    OnClick = StatusBarClick
    OnContextPopup = StatusBarContextPopup
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 961
    Height = 505
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 0
      Top = 461
      Width = 961
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 97
      ExplicitTop = 41
      ExplicitWidth = 423
    end
    object SG: TStringGrid
      Left = 9
      Top = 41
      Width = 952
      Height = 420
      Align = alClient
      Color = clCream
      ColCount = 100
      DefaultColWidth = 92
      DefaultRowHeight = 18
      DefaultDrawing = False
      RowCount = 100
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goColMoving, goThumbTracking, goFixedColClick, goFixedRowClick, goFixedHotTrack]
      TabOrder = 0
      OnDblClick = SGDblClick
      OnDrawCell = SGDrawCell
      OnKeyUp = SGKeyUp
      OnMouseDown = SGMouseDown
      OnMouseMove = SGMouseMove
      OnMouseWheelDown = SGMouseWheelDown
      OnMouseWheelUp = SGMouseWheelUp
      RowHeights = (
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18)
    end
    object Panel2: TPanel
      Left = 0
      Top = 41
      Width = 9
      Height = 420
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 961
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
    end
    object Panel4: TPanel
      Left = 0
      Top = 464
      Width = 961
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
    end
  end
  object CBTag: TComboBox
    Left = 310
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 2
    Text = 'All'
    OnSelect = CBStatusSelect
    Items.Strings = (
      'All')
  end
  object CBCat: TComboBox
    Left = 159
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 3
    Text = 'All'
    OnSelect = CBStatusSelect
    Items.Strings = (
      'All')
  end
  object CBStatus: TComboBox
    Left = 8
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    DropDownCount = 16
    ItemIndex = 0
    TabOrder = 4
    Text = 'All'
    OnSelect = CBStatusSelect
    Items.Strings = (
      'All'
      'Downloading'
      'Seeding'
      'Completed'
      'Paused'
      'Stalled'
      'Errored')
  end
  object Warning: TMemo
    Left = 64
    Top = 111
    Width = 841
    Height = 298
    Lines.Strings = (
      
        'WARNING... WARNING... WARNING... (this is said) : THIS A WORK IN' +
        ' PROGRESS'
      ''
      
        'In order to build and use qBit4Delphi some Embarcadero units nee' +
        'd to be patched (Fixes and Enhancements)'
      
        'This version is tested and patched against Delphi 10.4.2 (Sydney' +
        '/Community Edition) and Delphi 11 (Alexandria) :'
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
      'Any questions : qBit4Delphi@ea4d.com'
      'Laurent Meyer.')
    TabOrder = 5
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 16
    Top = 96
  end
  object PMHdrCol: TPopupMenu
    OnPopup = PMHdrColPopup
    Left = 16
    Top = 48
    object PMISortCol: TMenuItem
      Caption = 'Sort '#55358#56441#55358#56443
      OnClick = PMISortColClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Hide: TMenuItem
      Caption = 'Hide'
      OnClick = HideClick
    end
    object ShowAll: TMenuItem
      Caption = 'Show All (Debug)'
      OnClick = ShowAllClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object PMIShowHide: TMenuItem
      Caption = 'Show / Hide'
    end
  end
  object PMCol: TPopupMenu
    AutoHotkeys = maManual
    OnPopup = PMColPopup
    Left = 328
    Top = 48
    object PMIPause: TMenuItem
      Caption = 'Pause'
      OnClick = PMIPauseClick
    end
    object PMIResume: TMenuItem
      Caption = 'Resume'
      OnClick = PMIResumeClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object N4: TMenuItem
      Caption = 'Delete'
      object PMIDeleteTorrent: TMenuItem
        Caption = 'Torrent Only'
        OnClick = PMIDeleteTorrentClick
      end
      object PMIDeleteData: TMenuItem
        Caption = #9888' With Data '#9888
        OnClick = PMIDeleteDataClick
      end
    end
    object N5: TMenuItem
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
    object PMIEditTrackers: TMenuItem
      Caption = 'Edit Trackers'
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object PMICategory: TMenuItem
      Caption = 'Category'
      object New1: TMenuItem
        Caption = 'New...'
        OnClick = New1Click
      end
      object Reset1: TMenuItem
        Caption = 'Reset'
        OnClick = Reset1Click
      end
      object Reset2: TMenuItem
        Caption = '-- Categorries : --'
        Enabled = False
      end
      object AAAA1: TMenuItem
        AutoCheck = True
        Caption = 'AAAA'
        OnClick = AAAA1Click
        object Assign1: TMenuItem
          Caption = 'Assign'
        end
        object Delete1: TMenuItem
          Caption = 'Delete'
        end
      end
      object BBB1: TMenuItem
        AutoCheck = True
        Caption = 'BBB'
      end
    end
    object PMITags: TMenuItem
      Caption = 'Tags'
      object New2: TMenuItem
        Caption = 'New...'
        OnClick = New2Click
      end
      object PMICatReset: TMenuItem
        Caption = 'Reset'
        OnClick = PMICatResetClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
    end
  end
  object PMStatus: TPopupMenu
    TrackButton = tbLeftButton
    Left = 16
    Top = 408
    object PMIToggleSpeedLimits: TMenuItem
      Caption = 'Toggle Speed Limits'
      OnClick = PMIToggleSpeedLimitsClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object PMISpeedLimits: TMenuItem
      Caption = 'Set Speed Limits'
      OnClick = PMISpeedLimitsClick
    end
  end
end
