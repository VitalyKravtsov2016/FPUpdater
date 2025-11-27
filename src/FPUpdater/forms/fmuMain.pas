unit fmuMain;

interface

uses
  // VCL
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, DateUtils,
  // This
  FirmwareUpdater, fmuUnsupported, untVInfo;

type
  { TfmMain }

  TfmMain = class(TForm)
    btnProperties: TButton;
    btnStart: TButton;
    MemoInfo: TMemo;
    btnStop: TButton;
    Timer: TTimer;
    btnClose: TButton;
    lblTime: TLabel;
    edtStatus: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    FUpdater: TFirmwareUpdater;
    function GetUpdater: TFirmwareUpdater;
    procedure CheckStarted;
    property Updater: TFirmwareUpdater read GetUpdater;
  public
    destructor Destroy; override;
    procedure UpdatePage;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

{ TfmMain }

destructor TfmMain.Destroy;
begin
  FUpdater.Free;
  inherited Destroy;
end;

function TfmMain.GetUpdater: TFirmwareUpdater;
begin
  if FUpdater = nil then
    FUpdater := TFirmwareUpdater.Create;
  Result := FUpdater;
end;

procedure TfmMain.UpdatePage;
var
  Status: TUpdateStatus;
begin
  Updater.UpdateStatus;
  Status := Updater.Status;
  MemoInfo.Text := Status.InfoText;
  edtStatus.Text := Status.Text;
  lblTime.Caption := Status.TimeText;
  btnStop.Enabled := Status.IsStarted;
  btnStart.Enabled := (not Status.IsStarted)and(Status.UpdateAvailable);
  btnProperties.Enabled := not Status.IsStarted;
end;

procedure TfmMain.CheckStarted;
begin
  if Updater.Status.IsStarted then
  begin
    if MessageBox(Handle, 'Идет обновление прошивки. Прервать?', 'Внимание',
      MB_YESNO or MB_ICONEXCLAMATION) = ID_NO then Abort;
  end;
end;

procedure TfmMain.btnCloseClick(Sender: TObject);
begin
  CheckStarted;
  Close;
end;

procedure TfmMain.btnPropertiesClick(Sender: TObject);
begin
  Updater.ShowProperties;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' ver. ' + GetFileVersionInfoStr;
  Updater.LoadParameters;
  UpdatePage;
end;

procedure TfmMain.btnStartClick(Sender: TObject);
begin
  btnStart.Enabled := False;
  btnStop.Enabled := True;
  try
    if not Updater.CheckEcrUpdateable then
    begin
      fmUnsupported.ShowModal;
      Exit;
    end;
    Updater.Start;
    Timer.Enabled := True;
  except
    on E: Exception do
    begin
      btnStart.Enabled := True;
      btnStop.Enabled := False;
      raise;
    end;
  end;
end;

procedure TfmMain.btnStopClick(Sender: TObject);
begin
  CheckStarted;

  btnStop.Enabled := False;
  Updater.Stop;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
  Timer.Enabled := False;
  UpdatePage;
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  UpdatePage;
end;


end.
