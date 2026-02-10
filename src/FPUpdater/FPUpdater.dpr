program FPUpdater;

{$R 'FPUpdater.res' 'FPUpdater.rc'}

uses
  SysUtils,
  Vcl.Forms,
  fmuMain in 'forms\fmuMain.pas' {fmMain},
  fmuUnsupported in 'forms\fmuUnsupported.pas' {fmUnsupported},
  untDriver in 'units\untDriver.pas',
  DrvFRLib_TLB in 'units\DrvFRLib_TLB.pas',
  DriverError in 'units\DriverError.pas',
  LogFile in 'units\LogFile.pas',
  SystemUtils in 'units\SystemUtils.pas',
  FirmwareUpdater in 'units\FirmwareUpdater.pas',
  untVInfo in 'units\untVInfo.pas',
  NotifyThread in 'units\NotifyThread.pas',
  UpdateItem in 'units\UpdateItem.pas',
  StringUtils in 'units\StringUtils.pas',
  BinUtils in 'units\BinUtils.pas',
  FptrTypes in 'units\FptrTypes.pas',
  LangUtils in 'units\LangUtils.pas',
  DriverTypes in 'units\DriverTypes.pas',
  GlobalConst in 'units\GlobalConst.pas',
  XModem in 'units\XModem.pas',
  AsyncSerialPort in 'units\AsyncSerialPort.pas',
  gnugettext in 'units\gnugettext.pas',
  DeviceSearch in 'units\DeviceSearch.pas',
  SearchPort in 'units\SearchPort.pas',
  PrinterTypes in 'units\PrinterTypes.pas',
  untUtil in 'units\untUtil.pas',
  ComportUtils in 'units\ComportUtils.pas';

{$R *.res}

procedure AutoUpdadeEcr;
var
  Updater: TFirmwareUpdater;
begin
  Updater := TFirmwareUpdater.Create;
  try
    Updater.UpdateFirmware;
  except
    on E: Exception do
    begin
      //
    end;
  end;
  Updater.Free;
end;

begin
  Logger.Info(LogFile.Separator);
  Logger.Info('FPUpdater ' + GetModuleVersion + ', утилита для обновления ФР, ООО «Торговый Баланс М», 2026');

  if FindCmdLineSwitch('SILENT', ['-', '/'], False) then
  begin
    AutoUpdadeEcr;
  end else
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'Прошивальщик ' + GetFileVersionInfoStr;
    Application.CreateForm(TfmMain, fmMain);
    Application.CreateForm(TfmUnsupported, fmUnsupported);
    fmMain.Position := poDesktopCenter;
    Application.Run;
  end;
end.
