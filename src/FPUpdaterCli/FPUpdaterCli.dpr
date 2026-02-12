program FPUpdaterCli;

{$R 'FPUpdaterCli.res' 'FPUpdaterCli.rc'}

uses
  SysUtils,
  untDriver in '..\FPUpdater\units\untDriver.pas',
  DrvFRLib_TLB in '..\FPUpdater\units\DrvFRLib_TLB.pas',
  DriverError in '..\FPUpdater\units\DriverError.pas',
  LogFile in '..\FPUpdater\units\LogFile.pas',
  SystemUtils in '..\FPUpdater\units\SystemUtils.pas',
  FirmwareUpdater in '..\FPUpdater\units\FirmwareUpdater.pas',
  untVInfo in '..\FPUpdater\units\untVInfo.pas',
  NotifyThread in '..\FPUpdater\units\NotifyThread.pas',
  UpdateItem in '..\FPUpdater\units\UpdateItem.pas',
  StringUtils in '..\FPUpdater\units\StringUtils.pas',
  BinUtils in '..\FPUpdater\units\BinUtils.pas',
  FptrTypes in '..\FPUpdater\units\FptrTypes.pas',
  LangUtils in '..\FPUpdater\units\LangUtils.pas',
  DriverTypes in '..\FPUpdater\units\DriverTypes.pas',
  GlobalConst in '..\FPUpdater\units\GlobalConst.pas',
  XModem in '..\FPUpdater\units\XModem.pas',
  AsyncSerialPort in '..\FPUpdater\units\AsyncSerialPort.pas',
  gnugettext in '..\FPUpdater\units\gnugettext.pas',
  DeviceSearch in '..\FPUpdater\units\DeviceSearch.pas',
  SearchPort in '..\FPUpdater\units\SearchPort.pas',
  PrinterTypes in '..\FPUpdater\units\PrinterTypes.pas',
  untUtil in '..\FPUpdater\units\untUtil.pas',
  ComportUtils in '..\FPUpdater\units\ComportUtils.pas';

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
  Logger.FileName := ChangeFileExt(ParamStr(0), '.log');
  Logger.Enabled := True;
  Logger.WriteConsole := True;
  Logger.Info(LogFile.Separator);
  Logger.Info('FPUpdaterCli ' + GetModuleVersion + ', утилита для обновления ФР, ООО «Торговый Баланс М», 2026');

  AutoUpdadeEcr;
end.
