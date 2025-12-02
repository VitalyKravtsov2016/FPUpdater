unit fmuMain;

interface

uses
  // VCL
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, DateUtils,
  // This
  FirmwareUpdater, fmuUnsupported, untVInfo, untDriver, UpdateItem,
  NotifyThread, JvComponentBase, JvThread;

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
    FThread: TNotifyThread;
    FUpdater: TFirmwareUpdater;
    FUpdateAvailable: Boolean;

    procedure CheckStarted;
    procedure UpdateEcrInfo;
    procedure UpdateCompleted(Sender: TObject);

    function GetUpdater: TFirmwareUpdater;
    function GetInfoText(EcrInfo: TEcrInfo): string;

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
  Timer.Enabled := False;
  FUpdater.Free;
  FreeDriver;
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
  TimeText: string;
  Status: TUpdateStatus;
  ElapsedSeconds: Integer;
begin
  Status := Updater.Status;
  btnStop.Enabled := Status.IsStarted;
  if Status.IsStarted then
  begin
    ElapsedSeconds := SecondsBetween(Now, Status.StartTime);
    TimeText := Format('Время начала: %s, прошло секунд, %d', [
      TimeToStr(Status.StartTime), ElapsedSeconds]);
  end;
  lblTime.Caption := TimeText;
  edtStatus.Text := Status.Text;
  btnStart.Enabled := (not Status.IsStarted)and(FUpdateAvailable);
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
  Driver.ShowProperties;
  Driver.SaveParams;
  Driver.Disconnect;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' ver. ' + GetFileVersionInfoStr;
  Updater.LoadParameters;
  UpdateEcrInfo;
  UpdatePage;
end;

function TfmMain.GetInfoText(EcrInfo: TEcrInfo): string;
var
  Lines: TStrings;
  Item: TUpdateItem;
  LoaderLine: string;
  FirmwareLine: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Add(Format('Версия загрузчика ККМ: %d', [EcrInfo.BootVer]));
    Lines.Add(Format('Версия ПО ККМ: %s, сборка %d от %s', [
      EcrInfo.FirmwareVersion,
      EcrInfo.FirmwareBuild,
      DateToStr(EcrInfo.FirmwareDate)]));
    Lines.Add('');

    LoaderLine := '';
    FirmwareLine := '';
    FUpdateAvailable := False;
    if Updater.FindLastLoader(EcrInfo, Item) then
    begin
      FUpdateAvailable := True;
      LoaderLine := Format('Загрузчик до версии %d', [Item.NewBootVer]);
      EcrInfo.BootVer := Item.NewBootVer;
    end;
    if Updater.FindFirmware(EcrInfo, Item) then
    begin
      FUpdateAvailable := True;
      FirmwareLine := Format('ПО ККМ до версии %s, сборка: %d от %s',
          [Item.fwver, Item.fwbuild, DateToStr(Item.fwdate)]);
    end;

    if FUpdateAvailable then
    begin
      Lines.Add('Программа обновит:');
      if LoaderLine <> '' then Lines.Add(LoaderLine);
      if FirmwareLine <> '' then Lines.Add(FirmwareLine);
      Lines.Add('');
      Lines.Add('Для начала нажмите кнопку "Обновить"');
    end else
    begin
      Lines.Add('Обновления не найдены');
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
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
    Updater.OnComplete := UpdateCompleted;
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

procedure TfmMain.UpdateCompleted(Sender: TObject);
begin
  UpdateEcrInfo;
end;

procedure TfmMain.UpdateEcrInfo;
begin
  try
    MemoInfo.Text := GetInfoText(Updater.ReadEcrInfo);
  except
    on E: Exception do
    begin
      MemoInfo.Text := 'Ошибка: ' + E.Message;
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
