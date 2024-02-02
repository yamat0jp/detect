object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 697
  ClientWidth = 1237
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Image1: TImage
    Left = 48
    Top = 56
    Width = 300
    Height = 300
  end
  object SpeedButton1: TSpeedButton
    Left = 48
    Top = 616
    Width = 73
    Height = 22
    AllowAllUp = True
    GroupIndex = 1
    Down = True
    Caption = 'detect'
    Flat = True
    OnClick = SpeedButton1Click
  end
  object VideoWindow1: TVideoWindow
    Left = 649
    Top = 56
    Width = 300
    Height = 300
    FilterGraph = FilterGraph1
    VMROptions.Mode = vmrWindowed
    Color = clBlack
  end
  object ComboBox1: TComboBox
    Left = 168
    Top = 592
    Width = 145
    Height = 23
    TabOrder = 1
    Text = 'detect.tflite'
    OnChange = ComboBox1Change
  end
  object FilterGraph1: TFilterGraph
    Mode = gmCapture
    GraphEdit = False
    LinearVolume = True
    Left = 488
    Top = 360
  end
  object Filter1: TFilter
    BaseFilter.data = {00000000}
    FilterGraph = FilterGraph1
    Left = 400
    Top = 360
  end
  object SampleGrabber1: TSampleGrabber
    FilterGraph = FilterGraph1
    MediaType.data = {
      7669647300001000800000AA00389B717DEB36E44F52CE119F530020AF0BA770
      FFFFFFFF0000000001000000809F580556C3CE11BF0100AA0055595A00000000
      0000000000000000}
    Left = 536
    Top = 304
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 64
    Top = 376
  end
end
