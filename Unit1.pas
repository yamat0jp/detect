unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  DSPack, DirectShow9, DXSUtil, Vcl.AppEvnts, Vcl.Buttons;

type
  TForm1 = class(TForm)
    Image1: TImage;
    FilterGraph1: TFilterGraph;
    VideoWindow1: TVideoWindow;
    Filter1: TFilter;
    SampleGrabber1: TSampleGrabber;
    ApplicationEvents1: TApplicationEvents;
    SpeedButton1: TSpeedButton;
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
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

const
  size = 300;

var
  core: TInterpreter;
  VideoDevices: TSysDevEnum;
  bmp: TBitmap;

procedure makelist;
var
  rec: TSearchRec;
  i: integer;
begin
  i := FindFirst('.\*.tflite', faNormal, rec);
  while i = 0 do
  begin
    if ExtractFileExt(rec.Name) = '.tflite' then
      Form1.ComboBox1.Items.Add(rec.Name);
    i := FindNext(rec);
  end;
  FindClose(rec);
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  SpeedButton1Click(Sender);
  Done := false;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  core.FileName:=ComboBox1.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Clear;
  makelist;
  VideoWindow1.Width := size;
  VideoWindow1.Height := size;
  Image1.Width := size;
  Image1.Height := size;
  core := TInterpreter.Create(Self);
  core.Image := Image1;
  core.FileName := ComboBox1.Text;
  bmp := TBitmap.Create;
  bmp.Width := size;
  bmp.Height := size;
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

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  SampleGrabber1.GetBitmap(bmp);
  Image1.Picture.Bitmap.Canvas.StretchDraw(Rect(0, 0, size, size), bmp);
  if SpeedButton1.Down then
    core.Detect;
end;

end.
