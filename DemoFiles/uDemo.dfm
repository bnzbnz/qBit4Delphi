object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Form2'
  ClientHeight = 518
  ClientWidth = 1038
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1038
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      1038
      57)
    object LabeledEdit1: TLabeledEdit
      Left = 16
      Top = 18
      Width = 184
      Height = 23
      EditLabel.Width = 55
      EditLabel.Height = 15
      EditLabel.Caption = 'HostPath :'
      TabOrder = 0
      Text = 'http://127.0.0.1:8080'
    end
    object LabeledEdit2: TLabeledEdit
      Left = 206
      Top = 18
      Width = 98
      Height = 23
      EditLabel.Width = 59
      EditLabel.Height = 15
      EditLabel.Caption = 'Username :'
      TabOrder = 1
    end
    object LabeledEdit3: TLabeledEdit
      Left = 310
      Top = 18
      Width = 98
      Height = 23
      EditLabel.Width = 56
      EditLabel.Height = 15
      EditLabel.Caption = 'Password :'
      PasswordChar = '*'
      TabOrder = 2
    end
    object Button1: TButton
      Left = 430
      Top = 17
      Width = 101
      Height = 25
      Anchors = [akTop, akRight, akBottom]
      Caption = '==> Connect'
      TabOrder = 3
      OnClick = Button1Click
    end
    object Memo3: TMemo
      Left = 560
      Top = 4
      Width = 474
      Height = 47
      Lines.Strings = (
        
          'Start qBittorent, Enable "Web User Interface" and "Bypass auth. ' +
          'for '
        
          'clients on localhost" in options/Web UI . For qBittorrent-nox us' +
          'ers connect as usual.')
      TabOrder = 4
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 57
    Width = 1038
    Height = 461
    ActivePage = Server
    Align = alClient
    Enabled = False
    TabOrder = 1
    object Server: TTabSheet
      Caption = 'Server'
      object Label1: TLabel
        Left = 24
        Top = 48
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label2: TLabel
        Left = 24
        Top = 69
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label3: TLabel
        Left = 24
        Top = 90
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label4: TLabel
        Left = 24
        Top = 111
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label5: TLabel
        Left = 24
        Top = 132
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label6: TLabel
        Left = 24
        Top = 153
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label7: TLabel
        Left = 24
        Top = 174
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label8: TLabel
        Left = 24
        Top = 27
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label9: TLabel
        Left = 24
        Top = 216
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label10: TLabel
        Left = 24
        Top = 195
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label11: TLabel
        Left = 24
        Top = 237
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label12: TLabel
        Left = 24
        Top = 258
        Width = 9
        Height = 15
        Caption = '...'
      end
      object Label13: TLabel
        Left = 224
        Top = 8
        Width = 125
        Height = 15
        Caption = 'Warning , Critical Logs :'
      end
      object Memo1: TMemo
        Left = 232
        Top = 29
        Width = 795
        Height = 388
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Button5: TButton
        Left = 24
        Top = 392
        Width = 172
        Height = 25
        Caption = 'SERVER SHUTDOWN'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = Button5Click
      end
      object Memo2: TMemo
        Left = 80
        Top = 45
        Width = 881
        Height = 317
        Lines.Strings = (
          'WARNING... WARNING... WARNING... (this is said) :'
          ''
          
            'In order to build and use qBitVCL some Embarcadero units need to' +
            ' be patched (Fixes and Enhancements)'
          
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
            'and Run the demo, it should work.'
          #9'(Remember to always add this patched units to your projects)'
          ''
          'qBitVCL@ea4d.com')
        TabOrder = 2
      end
    end
    object Torrents: TTabSheet
      Caption = 'Torrents'
      ImageIndex = 2
      object StringGrid1: TStringGrid
        Left = 0
        Top = 0
        Width = 1030
        Height = 378
        Align = alClient
        BorderStyle = bsNone
        ColCount = 8
        DefaultColWidth = 128
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goFixedRowDefAlign]
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 378
        Width = 1030
        Height = 53
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object Label14: TLabel
          Left = 0
          Top = 37
          Width = 208
          Height = 15
          Caption = '( You may also Drag&Drop a .torrent file )'
        end
        object Button2: TButton
          Left = 56
          Top = 6
          Width = 97
          Height = 25
          Caption = 'Add Torrent File'
          TabOrder = 0
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 920
          Top = 6
          Width = 99
          Height = 25
          Caption = 'Pause / Resume'
          TabOrder = 1
          OnClick = Button3Click
        end
        object Button4: TButton
          Left = 813
          Top = 6
          Width = 101
          Height = 25
          Caption = 'Delete (with Data)'
          TabOrder = 2
          OnClick = Button4Click
        end
        object Button6: TButton
          Left = 231
          Top = 6
          Width = 141
          Height = 25
          Caption = 'Add Magnet URL'
          TabOrder = 3
          OnClick = Button6Click
        end
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 208
    Top = 48
  end
end
