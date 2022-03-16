object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'qBit4Delphi Patcher'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 0
    Top = 274
    Width = 635
    Height = 25
    Align = alBottom
    Caption = 'Patch NOW'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 635
    Height = 274
    Align = alClient
    Lines.Strings = (
      'qBit4Delphi:'
      ''
      
        'In order to compile and run qBit4Delphi, some REST/JSON original' +
        ' .pas unist must be patched (fix and '
      'enhancements)'
      ''
      
        'This patcher will do the job for you. The patched files will be ' +
        'copied in the JSON folder (21: Community/Sydney '
      'Edition, 22: Alexendria).'
      ''
      
        'Remember to use these patched files in your projects (see demo'#39's' +
        ' source project file)'
      ''
      'Laurent Meyer / qBit4Delphi@ea4d.com'
      '')
    TabOrder = 1
    ExplicitTop = -6
  end
end
