unit duFirmwareUpdater;

interface

uses
  // VCL
  Windows, ActiveX, ComObj, SysUtils,
  // DUnit
  TestFramework,
  // This
  FirmwareUpdater, DrvFRLib_TLB, untDriver, UpdateItem, LogFile,
  StringUtils, XModem, EcrManager;

type

  { TFirmwareUpdaterTest }

  TFirmwareUpdaterTest = class(TTestCase)
  private
    Updater: TFirmwareUpdater;
    procedure SaveEcrStatus;
    procedure SaveTables(const FileName: string);
    procedure CheckPortNumber(PortNumber: Integer);

    procedure CheckConnection;
    procedure CheckRndisConnection;
    procedure UpdateLoader(const Serial, FileName: string; BootVer: Integer);
    procedure CheckFirmwareUpdated;
    procedure ClearFiscalStorage;
    procedure LoadFiles;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindUpdateItem;
    procedure TestFindDeviceLocal;
    procedure TestDfuUpload;
    procedure TestFirmwareUpdateCom;
    procedure TestFirmwareUpdateVCom;
    procedure TestFirmwareUpdateRNDIS;
    procedure TestFirmwareUpdateRNDIS2;
    procedure TestWriteLicenses;
    procedure TestRestoreTables;
    procedure TestUploadFirmware;
    procedure TestSetConnectionType;
    procedure TestGetOfdParams;
    procedure TestLoadParams;
    procedure TestUpdateFirmwareShtrih;
    procedure TestCreateShtrihEcr;
    procedure TestFirmwareUpdateRNDIS3;
    procedure TestWaitForDFUDevice;
    procedure TestUpdateFFD;
    procedure SetComConnection;
    procedure SetVComConnection;
    procedure SetRndisConnection;
    procedure TestFNFiscalization;
    procedure TestDiscoverDevice;

    procedure TestReadEcrState;
  end;

implementation

function GenerateRNM(ANumber, AINN, ASerial: AnsiString): AnsiString;
var
  S: AnsiString;
begin
   S := AddLeadingZeros(ANumber, 10) +
     AddLeadingZeros(AINN, 12) + AddLeadingZeros(ASerial, 20);
   Result := AddLeadingZeros(ANumber, 10) +
     AddLeadingZeros(IntToStr(CRCCITT16(S, $1021, $FFFF)), 6);
end;

function GetFilesPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'data\';
end;

function GetTablesFileName: string;
begin
  Result := GetFilesPath + 'Tables.csv';
end;

{ TFirmwareUpdaterTest }

procedure TFirmwareUpdaterTest.SetUp;
begin
  Updater := TFirmwareUpdater.Create;
  //Updater.Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'data\';
  Updater.Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'smtest\';
  Updater.DeleteLog;
  Updater.LoadFiles(Updater.Path);
end;

procedure TFirmwareUpdaterTest.TearDown;
begin
  Updater.Free;
end;

procedure TFirmwareUpdaterTest.TestFindUpdateItem;
var
  Path: string;
  Ecr: TEcrInfo;
  Index: Integer;
  Item: TUpdateItem;
begin
  Updater.Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'test\';
  Updater.LoadParameters;

  Ecr.BootVer := 153;
  Index := Updater.FindItemIndex(Ecr, ACTION_UPDATE_LOADER);
  CheckEquals(0, Index, 'FindItemIndex');
  Item := Updater.Items[0];
  CheckEquals(0, Item.CurrBootVer, 'Item.CurrBootVer');
  CheckEquals(155, Item.NewBootVer, 'Item.NewBootVer');

  Ecr.BootVer := 155;
  Index := Updater.FindItemIndex(Ecr, ACTION_UPDATE_LOADER);
  CheckEquals(1, Index, 'FindItemIndex');
  Item := Updater.Items[1];
  CheckEquals(155, Item.CurrBootVer, 'Item.CurrBootVer');
  CheckEquals(1939, Item.NewBootVer, 'Item.NewBootVer');


  Ecr.BootVer := 1939;
  Ecr.FirmwareVersion := 'C.3';
  Ecr.FirmwareBuild := 62776;
  Index := Updater.FindItemIndex(Ecr, ACTION_UPDATE_FIRMWARE);
  CheckEquals(2, Index, 'FindItemIndex');
  Item := Updater.Items[2];
  CheckEquals(1939, Item.CurrBootVer, 'Item.CurrBootVer');
end;

procedure TFirmwareUpdaterTest.TestWriteLicenses;
var
  Ecr: TEcrInfo;
begin
  Updater.Path := GetFilesPath;
  Ecr := Updater.ReadEcrInfo;
  Updater.LoadFiles(Updater.Path);
  Updater.WriteLicenses(Ecr);
end;

procedure TFirmwareUpdaterTest.SaveTables(const FileName: string);
begin
  Driver.FileName := FileName;
  if FileExists(Driver.FileName) then
  begin
    if not DeleteFile(Driver.FileName) then
      RaiseLastWin32Error;
  end;
  Driver.Check(Driver.ExportTables);
end;

procedure TFirmwareUpdaterTest.CheckPortNumber(PortNumber: Integer);
begin
  Driver.Check(Driver.GetECRStatus);
  CheckEquals(PortNumber, Driver.PortNumber, 'PortNumber <> Driver.PortNumber');
end;

procedure TFirmwareUpdaterTest.SaveEcrStatus;
begin
end;


procedure TFirmwareUpdaterTest.TestDfuUpload;
var
  Path: string;
  Ecr: TEcrInfo;
const
  IsComConnection = false;
begin
  Path := GetFilesPath;
  Ecr := Updater.ReadEcrInfo;
  // Loader
  Updater.DfuUploadFile(Path, 'tb_ldr_1939.bin');
  Updater.DelayInMs(LoaderRebootDelay);
  Updater.WaitForDevice(Ecr.Serial, LoaderRebootTimeout);
  Updater.CheckLoaderUpdated(1939);
  // Firmware
  Updater.DfuUploadFile(Path, 'tb_app_7098.bin');
  Updater.DelayInMs(FirmwareRebootDelay);
  Updater.WaitForDevice(Ecr.Serial, FirmwareRebootTimeout);
end;

procedure TFirmwareUpdaterTest.CheckConnection;
begin
  Logger.Debug('CheckConnection');
  Driver.Check(Driver.Connect);
end;

procedure TFirmwareUpdaterTest.SetComConnection;
var
  Ecr: TEcrInfo;
  SearchParams: TSearchParams;
begin
  CheckConnection;
  Logger.Debug('SetComConnection');
  Ecr := Updater.ReadEcrInfo;
  if Ecr.PortNumber = PORT_COM then Exit;

  if Driver.ConnectionType <> CT_LOCAL then
  begin
    Driver.WriteTableInt(21, 1, 1, 0); // Режим ppp
    Driver.WriteTableInt(21, 1, 9, 0); // Rndis active = 0

    Driver.RebootKKT;
    Driver.Disconnect;
    Driver.ConnectionType := CT_LOCAL;

    SearchParams.Port := PORT_COM;
    SearchParams.Serial := Ecr.Serial;
    SearchParams.Timeout := FirmwareRebootTimeout;
    Updater.DiscoverDevice(SearchParams);
    Driver.SaveParams;
  end;
  CheckPortNumber(PORT_COM);
end;

procedure TFirmwareUpdaterTest.SetVComConnection;
var
  Ecr: TEcrInfo;
  SearchParams: TSearchParams;
begin
  CheckConnection;
  Ecr := Updater.ReadEcrInfo;
  if Driver.ConnectionType <> CT_LOCAL then
  begin
    Driver.WriteTableInt(21, 1, 1, 0); // Режим ppp
    Driver.WriteTableInt(21, 1, 9, 0); // Rndis active = 0
    Driver.RebootKKT;
    Driver.Disconnect;
    Driver.ConnectionType := CT_LOCAL;

    SearchParams.Port := PORT_VCOM;
    SearchParams.Serial := Ecr.Serial;
    SearchParams.Timeout := FirmwareRebootTimeout;
    Updater.DiscoverDevice(SearchParams);
    Driver.SaveParams;
  end;
  CheckPortNumber(PORT_VCOM);
end;

procedure TFirmwareUpdaterTest.SetRndisConnection;
var
  Ecr: TEcrInfo;
begin
  Logger.Debug('SetRndisConnection');

  CheckConnection;
  Ecr := Updater.ReadEcrInfo;
  if Driver.ConnectionType = CT_LOCAL then
  begin
    // Rndis active = 1
    Driver.WriteTableInt(21, 1, 9, 1);
    Driver.RebootKKT;
    Driver.Disconnect;

    Driver.ConnectionType := CT_TCPSOCKET;
    Driver.IPAddress := '192.168.137.111';
    Driver.TCPPort := 7778;
    Updater.DelayInMs(FirmwareRebootDelay);
    Updater.WaitForDevice(Ecr.Serial, FirmwareRebootTimeout);
    Driver.SaveParams;
  end;
  CheckRndisConnection;
end;

procedure TFirmwareUpdaterTest.CheckRndisConnection;
var
  PppMode: Integer;
  RndisActive: Boolean;
begin
  CheckPortNumber(PORT_TCP);
  PppMode := Driver.ReadTableInt(21, 1, 1); // Режим ppp
  RndisActive := Driver.ReadTableInt(21, 1, 9) = 1; // Режим Rndis
  CheckEquals(PPP_NONE, PppMode, 'PppMode <> PPP_NONE');
  CheckEquals(True, RndisActive, 'RndisActive=False');
end;

procedure TFirmwareUpdaterTest.TestFirmwareUpdateCom;
begin
  //SetComConnection;
  Updater.UpdateFirmware;
  CheckPortNumber(PORT_COM);
end;

procedure TFirmwareUpdaterTest.TestFirmwareUpdateVCom;
begin
  SetVComConnection;
  Updater.UpdateFirmware;
  CheckPortNumber(PORT_VCOM);
end;

procedure TFirmwareUpdaterTest.TestFirmwareUpdateRNDIS;
begin
  SetRndisConnection;
  Updater.UpdateFirmware;
  CheckRndisConnection;
end;

procedure TFirmwareUpdaterTest.TestFirmwareUpdateRNDIS2;
begin
  SetRndisConnection;
  Updater.Start;
  Updater.Wait;
  CheckRndisConnection;
end;

procedure TFirmwareUpdaterTest.TestRestoreTables;
var
  Ecr: TEcrInfo;
begin
  Ecr := Updater.ReadEcrInfo;
  Driver.WriteTableInt(21, 1, 9, 1);
  Updater.ReadTables(Ecr.Serial);

  Driver.WriteTableInt(21, 1, 9, 0);
  CheckEquals(0, Driver.ReadTableInt(21, 1, 9), 'RndisActive');
  Updater.WriteTablesFile(Ecr.Serial);
  CheckEquals(1, Driver.ReadTableInt(21, 1, 9), 'RndisActive');
end;

procedure TFirmwareUpdaterTest.TestFindDeviceLocal;
var
  SearchParams: TSearchParams;
begin
  SearchParams.Port := PORT_VCOM;
  SearchParams.Serial := '';
  //SearchParams.Serial := '0478110006079411';
  //SearchParams.Serial := '0374360004103321';
  SearchParams.Timeout := FirmwareRebootTimeout;
  if not Updater.FindDeviceLocal(SearchParams) then
    raise Exception.Create('Устройство не найдено');
end;

procedure TFirmwareUpdaterTest.TestUploadFirmware;
var
  Path: string;
  Ecr: TEcrInfo;
begin
  Path := GetFilesPath;
  Ecr := Updater.ReadEcrInfo;
  // Firmware
  Updater.DfuUploadFile(Path, 'tb_app_7098.bin');
  Updater.DelayInMs(FirmwareRebootDelay);
  Updater.WaitForDevice(Ecr.Serial, FirmwareRebootTimeout);
end;

procedure TFirmwareUpdaterTest.TestSetConnectionType;
begin
  SetVComConnection;
  SetRndisConnection;
  SetVComConnection;
end;

procedure TFirmwareUpdaterTest.UpdateLoader(const Serial, FileName: string;
  BootVer: Integer);
begin
  Updater.DfuUploadFile(Updater.Path, FileName);
  Updater.DelayInMs(LoaderRebootDelay);
  Updater.WaitForDevice(Serial, LoaderRebootTimeout);
  Updater.CheckLoaderUpdated(BootVer);
end;

// 153 -> 155 -> 1939 -> 3000 -> 153
procedure TFirmwareUpdaterTest.TestUpdateFirmwareShtrih;
var
  Ecr: TEcrInfo;
  SearchParams: TSearchParams;
  IsTehnoTestKeys: Boolean;
begin
  Logger.Debug('TestUpdateFirmwareShtrih.0');
  Ecr := Updater.ReadEcrInfo;
  if Ecr.SigningKey = SigningKeyTehnoWork then
    raise Exception.Create('Боевой софт откатить нельзя');

  if Ecr.BootVer <> 153 then
  begin
    // VCOM для скорости
    //SetVComConnection;
    // Обнуление ФН для скорости перезапуска ФР
    //ClearFiscalStorage;

    if Ecr.BootVer = 155 then
    begin
      UpdateLoader(Ecr.Serial, 'tb_ldr_1939_test.bin', 1939);
      Ecr.BootVer := 1939;
    end;
    if Ecr.BootVer = 1939 then
    begin
      UpdateLoader(Ecr.Serial, 'tb_ldr_3000_test.bin', 3000);
      Ecr.BootVer := 3000;
    end;
    if Ecr.BootVer = 3000 then
    begin
      UpdateLoader(Ecr.Serial, 'sm_ldr_153.bin', 153);
      Ecr.BootVer := 153;
    end;
  end;
  if Ecr.BootVer <> 153 then
    raise Exception.CreateFmt('Неверная версия загрузчика, %d', [Ecr.BootVer]);

  // Версия ПО: C.3
  // Сборка ПО: 62776
  // Дата ПО: 19.08.2025

  if (Ecr.FirmwareVersion <> 'C.1')or(Ecr.FirmwareBuild <> 62922) then
  begin
    Updater.DfuUploadFile(Updater.Path, 'sm_app_62922_c1.bin');
    Updater.DelayInMs(LoaderRebootDelay);
    SearchParams.Serial := Ecr.Serial;
    SearchParams.Timeout := 300000;
    SearchParams.Port := PORT_VCOM;
    Updater.DiscoverDevice(SearchParams);
  end;
  Logger.Debug('TestUpdateFirmwareShtrih.1');
end;

procedure TFirmwareUpdaterTest.TestFNFiscalization;
var
  IsTehnoTestKeys: Boolean;
begin
  // Отключить печать и звук
  //1,1,28,1,0,0,1,'Отключение звука при ошибках','0'
  Driver.WriteTableInt(1,1,28, 0);
  //17,1,7,1,0,0,3,'Rus не печатать документ','0'
  Driver.WriteTableInt(17,1,7, 0);
  // Сервер ОФД
  Driver.WriteTableStr(19,1,1, 'k-server.1-ofd-test.ru');
  Driver.WriteTableInt(19,1,2, 7777);
  Driver.WriteTableStr(18,1,10, 'Первый ОФД (АО "ЭСК")');
  Driver.WriteTableStr(18,1,11, '1-ofd.ru');
  Driver.WriteTableStr(18,1,12, '7709364346');

  // Проверка что ФН отладочный
  Driver.Check(Driver.FNGetVersion);
  if Driver.FNSoftType <> 0 then
    raise Exception.Create('ФН должен быть отладочным');

  ClearFiscalStorage;

  // Фискализация ФН
  Driver.Check(Driver.FNGetStatus);
  if Driver.FNLifeState = 1 then
  begin
    // Для тестовых устройств - фискализируем по 1.05
    // Если поле "Версия ФД" поддерживает значение 2 (ФФД 1.05)
    Driver.TableNumber := 17;
    Driver.RowNumber := 1;
    Driver.FieldNumber := 17;
    Driver.Check(Driver.GetFieldStruct);
    if Driver.MINValueOfField <= 2 then
    begin
      //17,1,17,1,0,2,2,'Rus формат фд','2'
      Driver.WriteTableInt(17,1,17, 2);
    end else
    begin
      if Driver.MAXValueOfField >= 4 then
      begin
        Driver.WriteTableInt(17,1,17, 4);
      end;
    end;

    Driver.Check(Driver.ReadSerialNumber);

    Driver.Inn := '9706048530';
    Driver.KKTRegistrationNumber := GenerateRNM('', Driver.Inn, Driver.SerialNumber);
    Driver.TaxType := 1;
    Driver.WorkMode := 0;
    Driver.WorkModeEx := 16;
    Driver.Timeout := 10000;
    Driver.Check(Driver.FNBuildRegistrationReport);
    Driver.WaitForPrinting;
    Driver.Timeout := 1000;
  end;
end;

procedure TFirmwareUpdaterTest.ClearFiscalStorage;
begin
  // Сбросить, если ФН фискализирован по ФФД 1.2 или был закрыт
  Driver.Check(Driver.FNGetStatus);
  if Driver.FNLifeState > 1 then
  begin
    // Сброс ФН
    Driver.RequestType := 22;
    Driver.Check(Driver.FNResetState);
  end;
end;

procedure TFirmwareUpdaterTest.TestCreateShtrihEcr;
begin
  try
    TestUpdateFirmwareShtrih;
    TestFNFiscalization;
    SetRndisConnection;
  except
    on E: Exception do
    begin
      Logger.Error(E.Message);
      raise;
    end;
  end;
end;

procedure TFirmwareUpdaterTest.LoadFiles;
var
  Path: string;
  IsTehnoTestKeys: Boolean;
begin
  Driver.Check(Driver.GetECRStatus);
  IsTehnoTestKeys := Pos('T', Driver.FMSoftVersion) > 0;
  Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  if IsTehnoTestKeys then
    Updater.Path := Path + 'test\'
  else
    Updater.Path := Path + 'data\';
  Updater.LoadFiles(Updater.Path);
end;

procedure TFirmwareUpdaterTest.TestFirmwareUpdateRNDIS3;
begin
  try
    LoadFiles;
    CheckRndisConnection;
    Updater.UpdateFirmware;
    CheckFirmwareUpdated;
    CheckRndisConnection;
  except
    on E: Exception do
    begin
      Logger.Error(E.Message);
      raise;
    end;
  end;
end;

procedure TFirmwareUpdaterTest.CheckFirmwareUpdated;
var
  Ecr: TEcrInfo;
  FirmwareDate: string;
begin
  Ecr := Updater.ReadEcrInfo;
  CheckEquals(1939, Ecr.BootVer, 'Ecr.BootVer <> 1939');
  CheckEquals('T.3', Ecr.FirmwareVersion, 'Ecr.FirmwareVersion <> T.3');
  CheckEquals(7099, Ecr.FirmwareBuild, 'Ecr.FirmwareBuild <> 7099');
  FirmwareDate := FormatDateTime('dd.mm.yyyy', Ecr.FirmwareDate);
  CheckEquals('20.11.2025', FirmwareDate, 'Ecr.FirmwareDate <> 20.11.2025');
end;

procedure TFirmwareUpdaterTest.TestGetOfdParams;
var
  IsFound: Boolean;
  Item: TUpdateItem;
  OfdParams: TOfdParams;
begin
  IsFound := False;
  for Item in Updater.Items do
  begin
    if Item.Action = ACTION_UPDATE_FIRMWARE then
    begin
      IsFound := True;
      Break;
    end;
  end;
  Check(IsFound, 'Не найден элемент обновления');
  Check(Updater.GetOfdParams(Item, '', OfdParams), 'GetOfdParams');
  CheckEquals('192.168.144.138', OfdParams.ServerKM, 'OfdParams.ServerKM');
  CheckEquals(8789, OfdParams.PortKM, 'OfdParams.PortKM');
end;


procedure TFirmwareUpdaterTest.TestLoadParams;
begin
  CheckEquals(True, Updater.Params.SaveTables, 'Params.SaveTables');
  CheckEquals(False, Updater.Params.PrintStatus, 'Params.PrintStatus');
  CheckEquals(Ord(FFD12), Ord(Updater.Params.FFDNeedUpdate), 'Params.FFDNeedUpdate');
  CheckEquals(True, Updater.Params.RestoreCashRegister, 'Params.RestoreCashRegister');
  CheckEquals(5, Updater.Params.DocSentTimeoutInSec, 'Params.DocSentTimeoutInSec');
end;

procedure TFirmwareUpdaterTest.TestUpdateFFD;
var
  IsFound: Boolean;
  Item: TUpdateItem;
  OfdParams: TOfdParams;
begin
  IsFound := False;
  for Item in Updater.Items do
  begin
    if Item.Action = ACTION_UPDATE_FIRMWARE then
    begin
      IsFound := True;
      Break;
    end;
  end;
  Check(IsFound, 'Не найден элемент обновления');
  Updater.UpdateFFD(Item);
end;

procedure TFirmwareUpdaterTest.TestWaitForDFUDevice;
var
  TickCount: Integer;
begin
  SetVComConnection;
  Driver.Check(Driver.Connect);
  CheckEquals(False, Updater.WaitForDFUDevice(0), 'WaitForDFUDevice');
  TickCount := GetTickCount;
  Driver.Check(Driver.SetDFUMode);
  CheckEquals(True, Updater.WaitForDFUDevice(DFUDelayTime), 'WaitForDFUDevice');
  TickCount := GetTickCount - TickCount;
  Logger.Debug(Format('DFU device up time: %d ms', [TickCount]));
  Check(TickCount < 3000, 'DFU device up time > 3000 ms');
  // Wait
end;

procedure TFirmwareUpdaterTest.TestDiscoverDevice;
var
  SearchParams: TSearchParams;
begin
  SearchParams.Port := PORT_VCOM;
  SearchParams.Serial := '';
  //SearchParams.Serial := '0478110006079411';
  //SearchParams.Serial := '0374360004103321';
  SearchParams.Timeout := FirmwareRebootTimeout;
  Updater.DiscoverDevice(SearchParams);
end;

procedure TFirmwareUpdaterTest.TestReadEcrState;
var
  Text: string;
  Manager: TEcrManager;
begin
  Manager := TEcrManager.Create(Driver);
  try
    Manager.ReadFullStatus;
    Manager.Lines.SaveToFile('EcrStatus.txt');
  finally
    Manager.Free;
  end;
end;

initialization
  RegisterTest('', TFirmwareUpdaterTest.Suite);
end.
