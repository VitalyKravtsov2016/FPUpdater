program FPUpdaterTst;
uses
  Forms,
  TestFramework,
  GUITestRunner,
  untDriver in '..\..\src\FPUpdater\units\untDriver.pas',
  DrvFRLib_TLB in '..\..\src\FPUpdater\units\DrvFRLib_TLB.pas',
  DriverError in '..\..\src\FPUpdater\units\DriverError.pas',
  LogFile in '..\..\src\FPUpdater\units\LogFile.pas',
  SystemUtils in '..\..\src\FPUpdater\units\SystemUtils.pas',
  FirmwareUpdater in '..\..\src\FPUpdater\units\FirmwareUpdater.pas',
  untVInfo in '..\..\src\FPUpdater\units\untVInfo.pas',
  NotifyThread in '..\..\src\FPUpdater\units\NotifyThread.pas',
  StringUtils in '..\..\src\FPUpdater\units\StringUtils.pas',
  BinUtils in '..\..\src\FPUpdater\units\BinUtils.pas',
  FptrTypes in '..\..\src\FPUpdater\units\FptrTypes.pas',
  DriverTypes in '..\..\src\FPUpdater\units\DriverTypes.pas',
  GlobalConst in '..\..\src\FPUpdater\units\GlobalConst.pas',
  XModem in '..\..\src\FPUpdater\units\XModem.pas',
  AsyncSerialPort in '..\..\src\FPUpdater\units\AsyncSerialPort.pas',
  duFirmwareUpdater in 'units\duFirmwareUpdater.pas',
  DeviceSearch in '..\..\src\FPUpdater\units\DeviceSearch.pas',
  SearchPort in '..\..\src\FPUpdater\units\SearchPort.pas',
  untUtil in '..\..\src\FPUpdater\units\untUtil.pas',
  gnugettext in '..\..\src\FPUpdater\units\gnugettext.pas',
  LangUtils in '..\..\src\FPUpdater\units\LangUtils.pas',
  PrinterTypes in '..\..\src\FPUpdater\units\PrinterTypes.pas',
  ComportUtils in '..\..\src\FPUpdater\units\ComportUtils.pas',
  UpdateItem in '..\..\src\FPUpdater\units\UpdateItem.pas';

{$R *.RES}

begin
  TGUITestRunner.RunTest(RegisteredTests);
end.
