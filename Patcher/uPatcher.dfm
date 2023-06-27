object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'qBit4Delphi Patcher'
  ClientHeight = 340
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object Button1: TButton
    Left = 0
    Top = 315
    Width = 635
    Height = 25
    Align = alBottom
    Caption = 'Patch NOW'
    TabOrder = 0
    OnClick = Button1Click
    ExplicitTop = 314
    ExplicitWidth = 631
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 635
    Height = 315
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsItalic]
    Lines.Strings = (
      'qBit4Delphi:'
      ''
      
        'As Delphi source units are copyrighted, modified version counld ' +
        'not be '
      'distributed:'
      ''
      
        'In order to compile and run qBit4Delphi, some REST/JSON original' +
        ' units must'
      'be patched (fixes and enhancements)'
      ''
      
        'This patcher will do the job for you. The patched files will be ' +
        'copied in the '
      'API/JSON folder (21: Community/Sydney Editions, 22: Alexendria).'
      ''
      
        'Remember to use these patched files in your projects (see demo'#39's' +
        ' source '
      'project file)'
      ''
      'Laurent Meyer / qBit4Delphi@ea4d.com'
      ''
      '')
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 631
    ExplicitHeight = 314
  end
end
