object qNOXifyFrm: TqNOXifyFrm
  Left = 0
  Top = 0
  Caption = 'qNOXifyFrm'
  ClientHeight = 469
  ClientWidth = 742
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
    Top = 450
    Width = 742
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
    ExplicitTop = 595
    ExplicitWidth = 1110
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 742
    Height = 450
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitWidth = 1110
    ExplicitHeight = 595
    object Splitter1: TSplitter
      Left = 0
      Top = 164
      Width = 742
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 97
      ExplicitTop = 41
      ExplicitWidth = 423
    end
    object SG: TStringGrid
      Left = 9
      Top = 57
      Width = 733
      Height = 107
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
      ExplicitWidth = 1101
      ExplicitHeight = 252
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
      Top = 57
      Width = 9
      Height = 107
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 252
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 742
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitWidth = 1110
      object Label1: TLabel
        Left = 9
        Top = 8
        Width = 59
        Height = 13
        Caption = 'String Filter:'
      end
      object Label2: TLabel
        Left = 192
        Top = 9
        Width = 38
        Height = 13
        Caption = 'Status :'
      end
      object Label3: TLabel
        Left = 359
        Top = 10
        Width = 52
        Height = 13
        Caption = 'Category :'
      end
      object Label4: TLabel
        Left = 526
        Top = 12
        Width = 25
        Height = 13
        Caption = 'Tag :'
      end
      object EditSearch: TEdit
        Left = 9
        Top = 28
        Width = 145
        Height = 21
        TabOrder = 0
      end
      object CBTag: TComboBox
        Left = 526
        Top = 31
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'All'
        OnSelect = CBStatusSelect
        Items.Strings = (
          'All')
      end
      object CBCat: TComboBox
        Left = 359
        Top = 29
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
      object CBStatus: TComboBox
        Left = 192
        Top = 28
        Width = 145
        Height = 21
        Style = csDropDownList
        DropDownCount = 16
        ItemIndex = 0
        TabOrder = 3
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
      object BitBtn1: TBitBtn
        Left = 145
        Top = 27
        Width = 32
        Height = 24
        Caption = 'X'
        TabOrder = 4
        OnClick = BitBtn1Click
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 167
      Width = 742
      Height = 283
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitTop = 312
      ExplicitWidth = 1110
      object PageControl1: TPageControl
        Left = 0
        Top = 0
        Width = 742
        Height = 283
        ActivePage = TabSheet1
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 1110
        object TabSheet1: TTabSheet
          Caption = 'General'
          object SGDetails: TStringGrid
            Left = 0
            Top = 0
            Width = 734
            Height = 255
            Align = alClient
            Color = clBtnFace
            ColCount = 10
            DefaultColWidth = 192
            DefaultRowHeight = 18
            FixedCols = 0
            RowCount = 20
            FixedRows = 0
            Options = []
            ScrollBars = ssVertical
            TabOrder = 0
            OnSelectCell = SGDetailsSelectCell
            ExplicitWidth = 1102
          end
        end
        object TabSheet2: TTabSheet
          Caption = 'TabSheet2'
          ImageIndex = 1
        end
      end
    end
  end
  object Warning: TMemo
    Left = 102
    Top = 85
    Width = 569
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
    TabOrder = 2
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
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      OnClick = SelectAll1Click
    end
    object N11: TMenuItem
      Caption = '-'
    end
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
    object Add1: TMenuItem
      Caption = 'Add'
      object PMIAddFile: TMenuItem
        Caption = 'Torrent File'
        OnClick = PMIAddFileClick
      end
      object AddMagnet: TMenuItem
        Caption = 'Magnet URI'
        OnClick = AddMagnetClick
      end
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
    object N9: TMenuItem
      Caption = '-'
    end
    object orrentManagement1: TMenuItem
      Caption = 'Torrent Management'
      object Enable1: TMenuItem
        Caption = 'Enable'
        OnClick = Enable1Click
      end
      object Disable1: TMenuItem
        Caption = 'Disable'
        OnClick = Disable1Click
      end
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object ForceRecheck1: TMenuItem
      Caption = 'Force Recheck'
      OnClick = ForceRecheck1Click
    end
    object ForceReannounce1: TMenuItem
      Caption = 'Force Reannounce'
      OnClick = ForceReannounce1Click
    end
  end
  object PMStatus: TPopupMenu
    TrackButton = tbLeftButton
    Left = 16
    Top = 232
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
  object OpenTorrent: TFileOpenDialog
    DefaultExtension = '.torrent'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Torrent'
        FileMask = '*.torrent'
      end>
    Options = []
    Left = 392
    Top = 56
  end
end
