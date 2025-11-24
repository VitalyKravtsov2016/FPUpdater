unit duFirmwareUpdater;

interface

uses
  // VCL
  ActiveX, ComObj, SysUtils,
  // DUnit
  TestFramework,
  // This
  FirmwareUpdater, FileUtils, DrvFRLib_TLB, untDriver, UpdateItem;

type

  { TFirmwareUpdaterTest }

  TFirmwareUpdaterTest = class(TTestCase)
  private
    Updater: TFirmwareUpdater;
    procedure SaveEcrStatus;
    procedure SaveTables(const FileName: string);
    procedure CheckPortNumber(PortNumber: Integer);

    procedure CheckConnection;
    procedure SetRndisConnection;
    procedure SetVComConnection;
    procedure SetComConnection;
    procedure CheckRndisConnection;
    procedure UpdateLoader(const Serial, FileName: string; BootVer: Integer);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindDeviceLocal;
    procedure TestDiscoverDevice;

    procedure TestDfuUpload;
    procedure TestFirmwareUpdateCom;
    procedure TestFirmwareUpdateVCom;
    procedure TestFirmwareUpdateRNDIS;
    procedure TestFirmwareUpdateRNDIS2;
    procedure TestWriteLicenses;
    procedure TestRestoreTables;
    procedure TestUploadFirmware;
    procedure TestSetConnectionType;
    procedure TestUpdateFFD;

    procedure TestUpdateFirmwareShtrih;
    procedure TestFirmwareUpdateRNDIS3;
    procedure TestFindUpdateItem;
  end;

implementation

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
  Updater.Path := GetFilesPath;
  Updater.DeleteLog;
  Updater.LoadParameters;
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
  Updater.LoadFiles;
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
  Driver.Check(Driver.Connect);
end;

procedure TFirmwareUpdaterTest.SetComConnection;
var
  Ecr: TEcrInfo;
  SearchParams: TSearchParams;
begin
  CheckConnection;
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
    //Updater.DelayInMs(FirmwareRebootDelay);
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
  SearchParams.Serial := '0478110006079411';
  SearchParams.Timeout := FirmwareRebootTimeout;
  if not Updater.FindDeviceLocal(SearchParams) then
    raise Exception.Create('Устройство не найдено');
end;

procedure TFirmwareUpdaterTest.TestDiscoverDevice;
var
  SearchParams: TSearchParams;
begin
  SearchParams.Port := PORT_VCOM;
  SearchParams.Serial := '0478110006079411';
  SearchParams.Timeout := FirmwareRebootTimeout;
  Updater.DiscoverDevice(SearchParams);
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

procedure TFirmwareUpdaterTest.TestUpdateFFD;
begin
  Updater.UpdateFFD;
end;

procedure TFirmwareUpdaterTest.UpdateLoader(const Serial, FileName: string;
  BootVer: Integer);
var
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'test\';
  Updater.DfuUploadFile(Path, FileName);
  Updater.DelayInMs(LoaderRebootDelay);
  Updater.WaitForDevice(Serial, LoaderRebootTimeout);
  Updater.CheckLoaderUpdated(BootVer);
end;

// 153 -> 155 -> 1939 -> 3000 -> 153
procedure TFirmwareUpdaterTest.TestUpdateFirmwareShtrih;
var
  Path: string;
  Ecr: TEcrInfo;
begin
  Ecr := Updater.ReadEcrInfo;
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
  if Ecr.BootVer <> 153 then
    raise Exception.CreateFmt('Неверная версия загрузчика, %d', [Ecr.BootVer]);

  // Версия ПО: C.3
  // Сборка ПО: 62776
  // Дата ПО: 19.08.2025
  if (Ecr.FirmwareVersion <> 'C.3')or(Ecr.FirmwareBuild <> 62776) then
  begin
    Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'test\';
    Updater.DfuUploadFile(Path, 'sm_app_62776.bin');
    Updater.DelayInMs(LoaderRebootDelay);
    Updater.WaitForDevice(Ecr.Serial, FirmwareRebootTimeout);
  end;
end;

procedure TFirmwareUpdaterTest.TestFirmwareUpdateRNDIS3;
var
  Ecr: TEcrInfo;
  FirmwareDate: string;
begin
  Updater.Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'test\';
  Updater.LoadParameters;

  TestUpdateFirmwareShtrih;
  SetRndisConnection;
  Updater.UpdateFirmware;
  CheckRndisConnection;

  Ecr := Updater.ReadEcrInfo;
  CheckEquals(1939, Ecr.BootVer, 'Ecr.BootVer <> 1939');
  CheckEquals('T.3', Ecr.FirmwareVersion, 'Ecr.FirmwareVersion <> T.3');
  CheckEquals(7099, Ecr.FirmwareBuild, 'Ecr.FirmwareBuild <> 7099');
  FirmwareDate := FormatDateTime('dd.mm.yyyy', Ecr.FirmwareDate);
  CheckEquals('20.11.2025', FirmwareDate, 'Ecr.FirmwareDate <> 20.11.2025');

end;


(*
  SaveTables(GetFilesPath + 'TablesBefore.csv');
  SaveTables(GetFilesPath + 'TablesAfter.csv');

*)

initialization
  RegisterTest('', TFirmwareUpdaterTest.Suite);
end.
