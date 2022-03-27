object vnStatDemoFrm: TvnStatDemoFrm
  Left = 0
  Top = 0
  Caption = 'vnStat Client Demo'
  ClientHeight = 355
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 30
    Height = 15
    Caption = 'URL : '
  end
  object Memo1: TMemo
    Left = 8
    Top = 59
    Width = 608
    Height = 286
    Lines.Strings = (
      
        'In order to run this demo, you need to own a dedicated or shared' +
        ' linux server'
      'It must runs vnstat, a web server and php'
      'Create a php script :  <?php passthru('#39'vnstat --json'#39'); ?>'
      
        'TEST IT with your web browser (the passthru function may have be' +
        'en disabled in php.ini)'
      'then provide the full php script URL to this demo...')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 608
    Height = 25
    Caption = 'Get vnstat'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 44
    Top = 8
    Width = 572
    Height = 23
    TabOrder = 2
  end
end
