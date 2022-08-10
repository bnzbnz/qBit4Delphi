object vnStatDemoFrm: TvnStatDemoFrm
  Left = 0
  Top = 0
  Caption = 'vnStat Client Demo'
  ClientHeight = 381
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 65
    Height = 15
    Caption = 'Server URL : '
  end
  object Label2: TLabel
    Left = 350
    Top = 8
    Width = 52
    Height = 15
    Caption = 'Interface :'
  end
  object Memo1: TMemo
    Left = 8
    Top = 91
    Width = 414
    Height = 286
    Lines.Strings = (
      
        'In order to run this demo, you need to own a dedicated or shared' +
        ' linux '
      'server'
      'It must runs vnstat, a web server and php'
      'Create a php script :  <?php passthru('#39'vnstat --json'#39'); ?>'
      
        'TEST IT with your web browser (the passthru function may have be' +
        'en '
      'disabled in php.ini)'
      
        'then provide the full php script URL and the interface name in w' +
        'hich '
      'you are  interrested in.')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button1: TButton
    Left = 6
    Top = 60
    Width = 416
    Height = 25
    Caption = 'Get Monthly Usage'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 29
    Width = 337
    Height = 23
    TabOrder = 0
    Text = 'https://www.example.com/vnstat.php'
  end
  object Edit2: TEdit
    Left = 351
    Top = 29
    Width = 71
    Height = 23
    TabOrder = 1
    Text = 'ens2f0'
  end
end
