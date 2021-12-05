object SpeedLimitsDlg: TSpeedLimitsDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Speed Limits'
  ClientHeight = 343
  ClientWidth = 571
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 8
    Top = 159
    Width = 548
    Height = 145
  end
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 548
    Height = 145
  end
  object Label1: TLabel
    Left = 22
    Top = 20
    Width = 64
    Height = 13
    Caption = 'Upload Limit :'
  end
  object Label2: TLabel
    Left = 22
    Top = 85
    Width = 78
    Height = 13
    Caption = 'Download Limit :'
  end
  object Label3: TLabel
    Left = 22
    Top = 176
    Width = 115
    Height = 13
    Caption = 'Alternate Upload Limite:'
  end
  object Label4: TLabel
    Left = 22
    Top = 240
    Width = 126
    Height = 13
    Caption = 'Alternate Download Limit :'
  end
  object TrackBar1: TTrackBar
    Left = 32
    Top = 40
    Width = 513
    Height = 45
    LineSize = 10
    Max = 2047
    PageSize = 10
    ShowSelRange = False
    TabOrder = 0
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 32
    Top = 105
    Width = 513
    Height = 45
    LineSize = 10
    Max = 2047
    PageSize = 10
    ShowSelRange = False
    TabOrder = 1
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object TrackBar3: TTrackBar
    Left = 32
    Top = 187
    Width = 513
    Height = 45
    LineSize = 10
    Max = 2047
    PageSize = 10
    ShowSelRange = False
    TabOrder = 2
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object TrackBar4: TTrackBar
    Left = 32
    Top = 251
    Width = 513
    Height = 45
    LineSize = 10
    Max = 2047
    PageSize = 10
    ShowSelRange = False
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object Button1: TButton
    Left = 392
    Top = 310
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 4
  end
  object Button2: TButton
    Left = 481
    Top = 310
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
