unit TfLite;

interface

uses System.SysUtils, System.Classes, Vcl.ExtCtrls, Vcl.Graphics,
  Winapi.Windows;

const
  PixelCount = 300 * 300;

type
  PRGBArray = ^TRGBArray;
  TRGBArray = array [0 .. PixelCount - 1] of TRGBTriple;
  TfLiteStatus = (kTfLiteOk, kTfLiteError, kTfLiteDelegateError);

  TfLiteType = (kTfLiteNoType = 0, kTfLiteFloat32 = 1, kTfLiteInt32 = 2,
    kTfLiteUInt8 = 3, kTfLiteInt64 = 4, kTfLiteString = 5, kTfLiteBool = 6,
    kTfLiteInt16 = 7, kTfLiteComplex64 = 8, kTfLiteInt8 = 9,
    kTfLiteFloat16 = 10, kTfLiteFloat64 = 11);

  PInputArray = ^TInputArray;
  TInputArray = array [0 .. PixelCount - 1] of array [0 .. 3 - 1] of UInt8;

  TInterpreter = class;

  TTensor = class(TComponent)
  protected
    FTensorType: TfLiteType;
    FByteSize: SIZE_T;
    FName: string;
    FNumDims: Int32;
    FCount: Int32;
    FTensor: Pointer;
  public
    property NumDims: Int32 read FNumDims write FNumDims;
    property Count: Int32 read FCount write FCount;
  end;

  TInputTensor = class(TTensor)
  public
    function Execute: TfLiteStatus; virtual;
  end;

  TOutputTensor = class(TTensor)
  private
    FImage: TImage;
  protected
    FLocations: array [0 .. 10 - 1] of array [0 .. 4 - 1] of Float32;
    FClasses: array [0 .. 10 - 1] of Float32;
    FScores: array [0 .. 10 - 1] of Float32;
    property Image: TImage read FImage write FImage;
  public
    procedure Draw; virtual;
    function Execute(index: integer): TfLiteStatus; virtual;
  end;

  TInterpreter = class(TComponent)
  private
    FFileName: TFileName;
    FInterpreter: Pointer;
    FImage: TImage;
    FOutputTensor: TOutputTensor;
    FInputTensor: TInputTensor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Detect; virtual;
  published
    property FileName: TFileName read FFileName write FFileName;
    property Image: TImage read FImage write FImage;
    property InputTensor: TInputTensor read FInputTensor write FInputTensor;
    property OutpuTensor: TOutputTensor read FOutputTensor write FOutputTensor;
  end;

implementation

uses Vcl.Dialogs;

const
  LibraryName = 'tflite.dll';
  map = 'map.txt';

function TfLiteModelCreateFromFile(const model_path: PAnsiChar): Pointer;
  stdcall; external LibraryName;

function TfLiteInterpreterCreate(model: Pointer; optional_options: Pointer)
  : Pointer; stdcall; external LibraryName;

function TfLiteInterpreterOptionsCreate(): Pointer; stdcall;
  external LibraryName;

function TfLiteInterpreterAllocateTensors(Interpreter: Pointer): TfLiteStatus;
  stdcall; external LibraryName;

function TfLiteTensorNumDims(tensor: Pointer): Int32; stdcall;
  external LibraryName;

function TfLiteTensorName(tensor: Pointer): PAnsiChar; stdcall;
  external LibraryName;

function TfLiteTensorType(tensor: Pointer): TfLiteType; stdcall;
  external LibraryName;

function TfLiteTensorByteSize(tensor: Pointer): SIZE_T; stdcall;
  external LibraryName;

function TfLiteInterpreterGetInputTensor(Interpreter: Pointer;
  input_index: Int32): Pointer; stdcall; external LibraryName;

function TfLiteInterpreterGetOutputTensor(Interpreter: Pointer;
  output_index: Int32): Pointer; stdcall; external LibraryName;

function TfLiteInterpreterResizeInputTensor(Interpreter: Pointer;
  input_index: Int32; input_dims: PInteger; input_dims_size: Int32)
  : TfLiteStatus; stdcall; external LibraryName;

function TfLiteInterpreterGetInputTensorCount(Interpreter: Pointer): Int32;
  stdcall; external LibraryName;

function TfLiteInterpreterGetOutputTensorCount(Interpreter: Pointer): Int32;
  stdcall; external LibraryName;

function TfLiteTensorCopyFromBuffer(tensor: Pointer; input_data: Pointer;
  input_data_size: SIZE_T): TfLiteStatus; stdcall; external LibraryName;

function TfLiteTensorCopyToBuffer(output_tensor: Pointer; output_data: Pointer;
  output_data_size: SIZE_T): TfLiteStatus; stdcall; external LibraryName;

procedure TfLiteInterpreterOptionsSetNumThreads(options: Pointer;
  num_threads: Int32); stdcall; external LibraryName;

procedure TfLiteInterpreterOptionsDelete(options: Pointer); stdcall;
  external LibraryName;

procedure TfLiteModelDelete(model: Pointer); stdcall; external LibraryName;

function TfLiteInterpreterInvoke(Interpreter: Pointer): TfLiteStatus; stdcall;
  external LibraryName;

{ TInterpreter }

constructor TInterpreter.Create(AOwner: TComponent);
begin
  inherited;
  FInputTensor := TInputTensor.Create(Self);
  FOutputTensor := TOutputTensor.Create(Self);
end;

destructor TInterpreter.Destroy;
begin
  FInputTensor.Free;
  FOutputTensor.Free;
  inherited;
end;

procedure TInterpreter.Detect;
var
  fStatus: TfLiteStatus;
  model: Pointer;
  option: Pointer;
begin
  model := TfLiteModelCreateFromFile(PAnsiChar(AnsiString(FFileName)));
  if model = nil then
  begin
    Showmessage('Error: Create model from file-' +
      SysErrorMessage(GetLastError));
    Exit;
  end;
  option := TfLiteInterpreterOptionsCreate;
  if option = nil then
    Exit;
  TfLiteInterpreterOptionsSetNumThreads(option, 2);
  FInterpreter := TfLiteInterpreterCreate(model, option);
  TfLiteInterpreterOptionsDelete(option);
  TfLiteModelDelete(model);
  if FInterpreter = nil then
    Exit;
  TfLiteInterpreterAllocateTensors(FInterpreter);
  FInputTensor.Execute;
  fStatus := FInputTensor.Execute;
  if fStatus = kTfLiteOk then
    fStatus := TfLiteInterpreterInvoke(FInterpreter);
  if fStatus = kTfLiteOk then
  begin
    FOutputTensor.Execute(0);
    FOutputTensor.Execute(1);
    fStatus := FOutputTensor.Execute(2);
    if fStatus = kTfLiteOk then
      FOutputTensor.Draw;
  end;
end;

{ TInputTensor }

function TInputTensor.Execute: TfLiteStatus;
var
  X, Y: DWORD;
  pic: TPicture;
  data: PInputArray;
  colors: PRGBArray;
  Interpreter: TInterpreter;
begin
  Interpreter := Owner as TInterpreter;
  FCount := TfLiteInterpreterGetInputTensorCount(Interpreter.FInterpreter);
  FTensor := TfLiteInterpreterGetInputTensor(Interpreter.FInterpreter, 0);
  FNumDims := TfLiteTensorNumDims(FTensor);
  FByteSize := TfLiteTensorByteSize(FTensor);
  pic := Interpreter.Image.Picture;
  GetMem(data, FByteSize);
  try
    for Y := 0 to pic.Bitmap.Height - 1 do
    begin
      colors := PRGBArray(pic.Bitmap.ScanLine[Y]);
      for X := 0 to pic.Bitmap.Width - 1 do
      begin
        data[X + pic.Bitmap.Width * Y][0] := colors[X].rgbtRed;
        data[X + pic.Bitmap.Width * Y][1] := colors[X].rgbtGreen;
        data[X + pic.Bitmap.Width * Y][2] := colors[X].rgbtBlue;
      end;
    end;
    result := TfLiteTensorCopyFromBuffer(FTensor, data, FByteSize);
  finally
    FreeMem(data, FByteSize);
  end;
end;

{ TOutputTensor }

procedure TOutputTensor.Draw;
var
  value: Extended;
  labelMap: TStringList;
begin
  labelMap := TStringList.Create;
  try
    labelMap.LoadFromFile(map);
    for var i := 0 to Length(FLocations[0]) - 1 do
    begin
      FImage.Canvas.Brush.Style := bsClear;
      FImage.Canvas.FillRect(FImage.Canvas.ClipRect);
      FImage.Canvas.Pen.Color := clRed;
      value := StrToFloat(Copy(FloatToStr(FScores[i]), 1, 4));
      if value >= 0.6 then
      begin
        Image.Canvas.Rectangle(Round(FLocations[i][1] * 300),
          Round(FLocations[i][0] * 300), Round(FLocations[i][3] * 300),
          Round(FLocations[i][2] * 300));
        FImage.Canvas.Brush.Style := bsSolid;
        FImage.Canvas.Brush.Color := clRed;
        FImage.Canvas.Font.Color := clWhite;
        FImage.Canvas.TextOut(Round(FLocations[i][1] * 300) + 2,
          Round(FLocations[i][0] * 300) + 1,
          labelMap.Strings[Round(FClasses[i] + 1)] + ' - ' + FloatToStr(value));
      end;
    end;
  finally
    labelMap.Free;
  end;
end;

function TOutputTensor.Execute(index: integer): TfLiteStatus;
var
  // fNumDetections: array [0 .. 1 - 1] of Float32;
  p: Pointer;
  Interpreter: TInterpreter;
begin
  Interpreter := Owner as TInterpreter;
  FImage := Interpreter.Image;
  FTensor := TfLiteInterpreterGetOutputTensor(Interpreter.FInterpreter, index);
  FByteSize := TfLiteTensorByteSize(FTensor);
  case index of
    0:
      p := @FLocations;
    1:
      p := @FClasses;
    2:
      p := @FScores;
  end;
  result := TfLiteTensorCopyToBuffer(FTensor, p, FByteSize);
end;

{ Register }

procedure Register;
begin
  RegisterComponents('TensorLite', [TInterpreter]);
end;

end.
