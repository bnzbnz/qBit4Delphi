object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 299
  ClientWidth = 635
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
  object LBTorrents: TListBox
    Left = 0
    Top = 0
    Width = 635
    Height = 299
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object Warning: TMemo
    Left = 8
    Top = 8
    Width = 619
    Height = 283
    Lines.Strings = (
      
        'WARNING... WARNING... WARNING... (this is said) : THIS A WORK IN' +
        ' PROGRESS'
      ''
      
        'In order to build and use qBit4Delphi some Embarcadero units nee' +
        'd to be patched (Fixes and Enhancements)'
      
        'This version is tested and patched against Delphi 10.4.2 (Sydney' +
        '/Community Edition) and Delphi 11 (Alexandria) :'
      ''
      'TO DO SO EXECUTE : Patcher.exe in the main directory'
      'The patched units will be located in JSON/21 and/or JSON/22'
      'Please add these units in you project.'
      ''
      'Any questions : qBit4Delphi@ea4d.com'
      'Laurent Meyer.')
    TabOrder = 0
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 16
    Top = 16
  end
end
