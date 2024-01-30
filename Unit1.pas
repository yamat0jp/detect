unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  DSPack, DirectShow9, DXSUtil, Vcl.AppEvnts;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    FilterGraph1: TFilterGraph;
    VideoWindow1: TVideoWindow;
    Filter1: TFilter;
    SampleGrabber1: TSampleGrabber;
    ApplicationEvents1: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses TfLite;

var
  core: TInterpreter;
  VideoDevices: TSysDevEnum;
  bmp: TBitmap;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  Button1Click(Sender);
  Done:=false;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SampleGrabber1.GetBitmap(bmp);
  Image1.Picture.Bitmap.Canvas.StretchDraw(Rect(0, 0, 300, 300), bmp);
  core.Detect;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  core := TInterpreter.Create(Self);
  core.Image := Image1;
  core.FileName := 'detect.tflite';
  bmp := TBitmap.Create;
  bmp.Width := 300;
  bmp.Height := 300;
  Image1.Picture.Assign(bmp);
  VideoDevices := TSysDevEnum.Create(CLSID_VideoInputDeviceCategory);
  FilterGraph1.Active := false;
  Filter1.BaseFilter.Moniker := VideoDevices.GetMoniker(0);
  FilterGraph1.ClearGraph;
  FilterGraph1.Active := True;
  with FilterGraph1 as ICaptureGraphBuilder2 do
    RenderStream(@PIN_CATEGORY_PREVIEW, nil, Filter1 as IBaseFilter,
      SampleGrabber1 as IBaseFilter, VideoWindow1 as IBaseFilter);
  FilterGraph1.Play;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  core.Free;
  bmp.Free;
  VideoDevices.Free;
  FilterGraph1.Stop;
  FilterGraph1.Active := false;
  FilterGraph1.ClearGraph;
end;

end.
