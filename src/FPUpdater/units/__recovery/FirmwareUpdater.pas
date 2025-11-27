unit FirmwareUpdater;

interface

{DONE: Добавить запись лицензии}
{TODO: Написать тесты для обновления ФР с любыми параметрами}
{TODO: Добавить перерегистрацию ФН}
{TODO: Добавить прошивку по протоколу XModem}
{TODO: Проверить правильность лога прошивальщика}
{TODO: Проверить правильность лога драйвера ФР}
{TODO: Сделать работу интерфейса}
{TODO: Сделать установщик}
{TODO: Отдать на тестирование}


uses
  // VCL
  Windows, SysUtils, Classes, Registry, SyncObjs, ShlObj,
  System.Zip, System.IOUtils, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.DateUtils,
  ActiveX, JvSetupApi,
  // TurboPower
  uTPLb_Codec, uTPLb_CryptographicLibrary,
  // This
  DrvFRLib_TLB, untDriver, SystemUtils, UpdateItem, NotifyThread, StringUtils,
  LogFile, BinUtils, FptrTypes, XModem, DeviceSearch, SearchPort;

const
  DFUDelayTime = 5000;
  //LoaderRebootDelay = 15000;
  //FirmwareRebootDelay = 30000;
  LoaderRebootDelay = 3000;
  FirmwareRebootDelay = 3000;

  RebootTimeout = 6000;
  LoaderRebootTimeout = 60000;
  FirmwareRebootTimeout = 60000;

  // TODO: Обновление параметров сервера КМ для ИНН ОФД ??

  /////////////////////////////////////////////////////////////////////////////
  // PortNumber значения

  PORT_COM  = 0;  // RS-232 - обновление по XModem
  PORT_VCOM = 1;  // USB vCOM - обновление по DFU (или XModem)
  PORT_TCP  = 2;  // TCP сокет (RNDIS, Ethernet, WI-FI, ppp)

  /////////////////////////////////////////////////////////////////////////////
  // Значения режима ppp

  PPP_NONE = 0;         // 0 – нет
  PPP_COM_CLIENT = 1;   // 1 – клиент через COM
  PPP_COM_SERVER = 2;   // 2 – сервер через COM
  PPP_VCOM_CLIENT = 3;  // 3 – клиент через USB
  PPP_VCOM_SERVER = 4;  // 4 – сервер через USB


  /////////////////////////////////////////////////////////////////////////////
  // ConnectionType constants

  CT_LOCAL = 0;
  CT_TCP = 1;
  CT_DCOM = 2;
  CT_ESCAPE = 3;
  CT_PACKETDRV = 4;
  CT_EMULATOR = 5;
  CT_TCPSOCKET = 6;
  CT_PPP = 7;

  // MODE constants
  MODE_DUMPMODE			= $01; // Dump mode
  MODE_24NOTOVER		= $02; // Opened day, 24 hours not left
  MODE_24OVER			  = $03; // Opened day, 24 hours is over
  MODE_CLOSED			  = $04; // Closed day
  MODE_LOCKED			  = $05; // ECR is bloced because of incorrect tax offecer password
  MODE_WAITDATE			= $06; // Waiting for date confirm
  MODE_POINTPOS			= $07; // Change decimal point position permission
  MODE_REC		     	= $08; // Opened document
  MODE_TECH		     	= $09; // Technological reset permission
  MODE_TEST			    = $0A; // Test run
  MODE_FULLREPORT		= $0B; // Full fiscal report printing
  MODE_EKLZREPORT		= $0C; // EJ report printing
  MODE_SLP		     	= $0D; // Opened fiscal slip
  MODE_SLPPRINT			= $0E; // Slip printing
  MODE_SLPREADY			= $0F; // Fiscal slip is ready


  /////////////////////////////////////////////////////////////////////////////
  // BaudRate values

  BAUD_RATE_CODE_2400   = 0;
  BAUD_RATE_CODE_4800   = 1;
  BAUD_RATE_CODE_9600   = 2;
  BAUD_RATE_CODE_19200  = 3;
  BAUD_RATE_CODE_38400  = 4;
  BAUD_RATE_CODE_57600  = 5;
  BAUD_RATE_CODE_115200 = 6;
  BAUD_RATE_CODE_230400 = 7;
  BAUD_RATE_CODE_460800 = 8;
  BAUD_RATE_CODE_921600 = 9;

type
  { TOfdParams }

  TOfdParams = record
    Inn: string;
    ServerKM: string;
    PortKM: Integer;
  end;

  { TSearchParams }

  TSearchParams = record
    Serial: string;
    Timeout: Integer;
    Port: Integer;
  end;

  { TUpdateMode }

  TUpdateMode = (umDFU, umXModem);

  { TEcrModel }

  TEcrModel = record
    Id: Integer;
    Name: string;
  end;

  { TEcrLicense }

  TEcrLicense = record
    Data: string;
    Signature: string;
  end;

  { TECRInfo }

  TECRInfo = record
    FirmwareVersion: string;        // Версия ПО ФР
    FirmwareBuild: Integer;         // Сборка ПО ФР
    FirmwareDate: TDate;            // Дата ПО ФР
    BootVer: Integer;               // Версия загрузчика
    Serial: string;
    FirmwareValid: Boolean;         //
    PortNumber: Integer;            // Тип подключения ККМ
    UpdateMode: TUpdateMode;        // Режим обновления ПО
  end;

  { TUpdateStatus }

  TUpdateStatus = record
    IsStarted: Boolean;
    Text: string;
    InfoText: string;
    StartTime: TDateTime;
    ResultText: string;
    ElapsedSeconds: Integer;
    TimeText: string;
    UpdateAvailable: Boolean;
  end;

  { TFirmwareUpdater }

  TFirmwareUpdater = class
  private
    FPath: string;
    FThread: TNotifyThread;
    FLock: TCriticalsection;
    FStopped: Boolean;
    FStatus: TUpdateStatus;
    FItems: TUpdateItems;
    FParams: TUpdateParams;
  public
    procedure UpdateStatus;
    procedure DeleteLog;
    procedure CheckStopped;
    procedure DownloadFiles;
    procedure UnpackArhiveToDrive;
    procedure Execute(Sender: TObject);
    procedure SetStatusText(const Text: string);
    procedure DecryptFile(const DstFileName, SrcFileName: string);
    procedure CheckFilesExists(const Path: string; Items: TUpdateItems);
    procedure CheckFirmwareUpdated(const Item: TUpdateItem);
    procedure DelayInMs(DelayInMs: Integer);
    procedure SetBaudRate(BaudRate: Integer);
    procedure Feed(ALineCount: Integer);
    procedure PrintText(const AStr: string);
    procedure CheckDocSent;
    procedure PrintUpdateStarted;
    procedure FormatSDCard;
    procedure WriteTablesFile(const Serial: string);
    procedure WriteTables(const Tables: TTableItems);
    procedure ReadTables(const Serial: string);
    procedure DfuUploadFile(const Path, FileName: string);
    procedure PrintCashIn(Amount: Currency);
    procedure CheckLoaderUpdated(NewBootVer: Integer);
    procedure PrintUpdateCompleted;
    procedure XModemUploadFile(ComNumber: Integer; const Path,
      FileName: string);
    procedure WriteLicense(const FileName, Serial: string);
    procedure WriteLicenses(const Ecr: TEcrInfo);
    procedure WaitForDevice(const Serial: string; TimeoutInMs: Integer);

    function Connect: Integer;
    function ReadEcrInfo: TEcrInfo;
    function DownloadArchive: Boolean;
    function GetStatus: TUpdateStatus;
    function ReadSerialNumber: string;
    function CheckUpdateAvailable: Boolean;
    function EcrUpdateable(const Serial: string): Boolean;
    function ReadFFDVersion: Integer;
    function ReadLicense(const FileName, Serial: string;
      var License: TEcrLicense): Boolean;
    function GetUpdateMode(PortNumber: Integer): TUpdateMode;
    function ReadCashRegister: Currency;
    function FindUpdateItem(const Data: TEcrInfo; Action: Integer;
      var Item: TUpdateItem): Boolean;
    function GetLogFileName: string;
    procedure DoUpdateFirmware;
    function FindDevice(const Serial: string): Boolean;
    procedure DiscoverDevice(const Params: TSearchParams);
    function FindDeviceLocal(const Params: TSearchParams): Boolean;
    function FindItemIndex(const Ecr: TEcrInfo; Action: Integer): Integer;
    function FindOfdParams(const OfdInn: string;
      var OfdParams: TOfdParams): Boolean;
    function GetOfdParams(const UpdateItem: TUpdateItem; const OfdInn: string;
      var OfdParams: TOfdParams): Boolean;
    procedure UpdateFFD(const Item: TUpdateItem);
    procedure ChangeFFD(const Item: TUpdateItem; AFFD: TFFDNeedUpdate);
    function WaitDocSent(TimeoutInSec: Integer): Boolean;
    procedure DecryptFiles(const InPath, OutPath: string);
    procedure UnzipArhive(const OutPath, FileName: string);
    procedure LoadFiles(const InPath: string);

    function RunDfuUtil(const Path, Params: string): string;
    function IsDFUDevicePresent: Boolean;
    function WaitForDFUDevice(Timeout: Cardinal): Boolean;
    procedure SetVComConnection;
    procedure UpdateInfoText(EcrInfo: TEcrInfo);
    function ValidUpdateItem(const Ecr: TEcrInfo; const Item: TUpdateItem): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Wait;
    procedure ShowProperties;
    procedure UpdateFirmware;
    procedure LoadParameters;
    function CheckEcrUpdateable: Boolean;

    property Items: TUpdateItems read FItems;
    property Params: TUpdateParams read FParams;
    property Path: string read FPath write FPath;
    property Status: TUpdateStatus read GetStatus;
  end;

implementation

const
  OfdParamsArray: array[1..13] of TOfdParams =
  (
    (Inn: '7728699517'; ServerKM: 'connect.Ofd-ya.ru'; PortKM: 7797),
    (Inn: '7709364346'; ServerKM: 'k-server.1-Ofd.ru'; PortKM: 7788),
    (Inn: '9715260691'; ServerKM: 'Ofdp.platformaOfd.ru'; PortKM: 21102),
    (Inn: '7704211201'; ServerKM: 'f1.taxcom.ru'; PortKM: 8777),
    (Inn: '6658497833'; ServerKM: 'Ofd.kontur.ru'; PortKM: 7778),
    (Inn: '4029017981'; ServerKM: 'Ofd.astralnalog.ru'; PortKM: 7777),
    (Inn: '7841465198'; ServerKM: 'crpt.Ofd.ru'; PortKM: 7000),
    (Inn: '7605016030'; ServerKM: 'kkt.sbis.ru'; PortKM: 7777),
    (Inn: '7704358518'; ServerKM: 'kkt.Ofd.yandex.net'; PortKM: 54321),
    (Inn: '5902034504'; ServerKM: 'kkt.Ofd-initpro.ru'; PortKM: 9996),
    (Inn: '7729642175'; ServerKM: 'crpt.e-Ofd.ru'; PortKM: 5555),
    (Inn: '2310031475'; ServerKM: 'kkt.Ofd-magnit.ru'; PortKM: 7005),
    (Inn: '7713076301'; ServerKM: 'Ofd.beeline.ru'; PortKM: 8765)
  );

function FFDToStr(AFFDVer: Integer): string;
begin
  case AFFDVer of
    2: Result := 'ФФД 1.05';
    4: Result := 'ФФД 1.2';
  end;
end;

{
  16 - "ШТРИХ-MPAY-К" | "АСПД MPAY" | "ШТРИХ-МПЕЙ-Ф"
  19 - Mobile
  20 - "YARUS М2100К" | "АСПД YARUS М21" | "ЯРУС М2100Ф" // ТТ Сухоставский
  21 - "YARUS-ТК" | "АСПД YARUS C21" | "ЯРУС ТФ" // ТТ Сухоставский
  27 - "YARUS-KZ C21" // Казахская ISOFT // с контрольной лентой КЛ-СНГ // ТТ Сухоставский // C2100
  28 - "YARUS-MD C21" // Молдавская // с контрольной лентой КЛ-СНГ // ТТ Сухоставский // C2100
  29 - "YARUS М2100К" | "АСПД YARUS M21" // 44 мм; Partner Windows CE 6.0
  30 - "YARUS М2100К" | "АСПД YARUS M21" // 57 мм; Partner Windows CE 6.0
  32 - "YARUS-TM C21" // Туркменская // без контрольной ленты // ТТ Сухоставский
  33 - "YARUS-MD M21" // Молдавская // с контрольной лентой КЛ-СНГ // ТТ Сухоставский // M2100
  34 - "YARUS-KZ M21" // Казахская ISOFT // с контрольной лентой КЛ-СНГ // ТТ Сухоставский // M2100
  35 - "YARUS-TM M21" // Туркменская // без контрольной ленты // ТТ Сухоставский
  36 - "YARUS-TK-KZ C21" // Казахская dzun // без контрольной ленты КЛ-СНГ // ТТ Сухоставский // C2100
  37 - "YARUS-TK-KZ M21" // Казахская dzun // без контрольной ленты КЛ-СНГ // ТТ Сухоставский // M2100
  38 - "YARUS-KG C21" // Кыргызкая // с контрольной лентой КЛ-СНГ // ТТ Сухоставский // C2100
  39 - "YARUS-KG M21" // Кыргызкая // с контрольной лентой КЛ-СНГ // ТТ Сухоставский // M2100
  40 - "YARUS М2100К" | "АСПД YARUS М21" | "ЯРУС М2100Ф" // Большаков Jibe
  41 - "YARUS М2100К" | "АСПД YARUS М21" | "ЯРУС М2100Ф" // Ляхович M7100
  42 - "ШТРИХ-MPAY-КZ" // Казахская ISOFT // с контрольной лентой КЛ-СНГ // MPAY
  45 - "АЗУР-01Ф" // SZZT KS8223
  45 - "ШТРИХ-СМАРТПОС-Ф" // TELPO/ALPS/ROCO TPS570A
  45 - "ШТРИХ-СМАРТПОС-МИНИ-Ф" // TELPO/ALPS/ROCO TPS900 // CLONTEK CS-10
  46 - "POSCENTER-AND-Ф" // JOLIMARK-IM-78 // 80 мм”
}
// 37 - ШТРИХ-ЗНАК-МЗ
// Модели Андрея Семенова, с другой структурой таблиц

function IsModelType2(Value: Integer): Boolean;
begin
  Result := Value in [16, 19, 20, 21, 27, 28, 29, 30, 32, 33, 34, 35, 36, {37,} 38, 39, 40, 41, 42, 45, 45, 45, 46];
end;


const
  EcrModels: array [0..0] of TEcrModel = (
  (Id: 0; Name: 'Test')
  );

function AddLeadingZeros(const S: string; ACount: Integer): string;
begin
  Result := Copy(S, 1, ACount);
  if ACount < Length(S) then Exit;
  Result := StringOfChar('0', ACount - Length(S)) + S;
end;

function SplitLine(const ALine: string; AIndex: Integer): string;
var
  Strings: TStringList;
begin
  Result := '';
  Strings := TStringList.Create;
  try
    Strings.Delimiter := #9;
    Strings.DelimitedText := ALine;
    if AIndex < Strings.Count then
      Result := Strings[AIndex];
  finally
    Strings.Free;
  end;
end;


///////////////////////////////////////////////////////////////////////////
// PortNumber значения
// 0, RS-232 - обновление по XModem
// 1, USB vCOM - обновление по DFU (или XModem)
// 2, TCP сокет (RNDIS, Ethernet, WI-FI, ppp) -
//    - обновление через DFU, если RNDIS
//    - обновление через DFU, если PPP через VCom
//    - обновление через XModem, если PPP через COM
//
///////////////////////////////////////////////////////////////////////////

const
  CompanyName = 'TorgBalance';

function GetUserCompanyPath: string;
begin
  SetLength(Result, MAX_PATH);
  ShlObj.SHGetSpecialFolderPath(0, @Result[1], CSIDL_APPDATA, false);
  SetLength(Result, Pos(#0, Result) - 1);

  Result := IncludeTrailingPathDelimiter(Result) + CompanyName;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function GetBackupTablesPath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserCompanyPath) + 'Tables';
end;

function CheckDFU: Boolean;
var
  Reg: TRegistry;
const
  GUID_DEVINTERFACE_MBB: TGUID = '{A01674B4-C5F6-485C-AF94-3271701D5FB4}';
begin
  Reg := TRegistry.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := Reg.OpenKey('SYSTEM\CurrentControlSet\Control\DeviceClasses\{a01674b4-c5f6-485c-af94-3271701d5fb4}', False);
  finally
    Reg.Free;
  end;
end;

function GetHttpFileLastModified(const URL: string): TDateTime;
var
  HTTPClient: TNetHTTPClient;
  Response: IHTTPResponse;
begin
  Result := 0;
  HTTPClient := TNetHTTPClient.Create(nil);
  try
    HTTPClient.UserAgent := 'Mozilla/5.0';
    // Делаем HEAD запрос (только заголовки, без тела)
    Response := HTTPClient.Head(URL);
    if Response.StatusCode = 200 then
    begin
      if Response.ContainsHeader('Last-Modified') then
      begin
        // Парсим дату из заголовка
        Result := HTTPToDate(Response.HeaderValue['Last-Modified']);
      end;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function DownloadFile(const URL, SavePath: string;
  OnProgress: TReceiveDataEvent = nil): Boolean;
var
  HTTPClient: TNetHTTPClient;
  Response: IHTTPResponse;
  FileStream: TFileStream;
begin
  HTTPClient := TNetHTTPClient.Create(nil);
  try
    HTTPClient.UserAgent := 'Mozilla/5.0';
    HTTPClient.ConnectionTimeout := 3000;
    HTTPClient.ResponseTimeout := 30000;

    if Assigned(OnProgress) then
      HTTPClient.OnReceiveData := OnProgress;

    FileStream := TFileStream.Create(SavePath, fmCreate);
    try
      Response := HTTPClient.Get(URL, FileStream);
      Result := (Response.StatusCode = 200);
    finally
      FileStream.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;


{ TFirmwareUpdater }

constructor TFirmwareUpdater.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;

  Logger.FileName := GetLogFileName;
  Logger.Enabled := True;

  FParams.SaveTables := True;
  FParams.PrintStatus := False;
  FParams.RestoreCashRegister := False;
end;

destructor TFirmwareUpdater.Destroy;
begin
  Stop;
  FLock.Free;
  inherited Destroy;
end;

function TFirmwareUpdater.GetLogFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.log');
end;

procedure TFirmwareUpdater.DeleteLog;
begin
  DeleteFile(GetLogFileName);
end;

procedure TFirmwareUpdater.ShowProperties;
begin
  Driver.ShowProperties;
end;

procedure TFirmwareUpdater.Start;
begin
  if Status.IsStarted then
    raise Exception.Create('Обновление уже идет.');

  FStopped := False;
  FThread := TNotifyThread.CreateThread(Execute);
end;

procedure TFirmwareUpdater.Stop;
begin
  FStopped := True;
  FThread.Free;
  FThread := nil;
end;

procedure TFirmwareUpdater.Wait;
begin
  if FThread <> nil then
  begin
    FThread.WaitFor;
  end;
end;

procedure TFirmwareUpdater.CheckStopped;
begin
  UpdateStatus;
  if FStopped then
    raise Exception.Create('Прервано пользователем');
end;

const
  Separator = '------------------------------------------------------------';

procedure TFirmwareUpdater.Execute(Sender: TObject);
begin
  try
    CoInitialize(nil);
    try
      UpdateFirmware;
    finally
      CoUninitialize;
    end;
  except
    on E: Exception do
    begin
      SetStatusText('Ошибка: ' + E.Message);
    end;
  end;
end;

procedure TFirmwareUpdater.UpdateFirmware;
begin
  FStatus.IsStarted := True;
  FStatus.StartTime := Now;
  Logger.Debug(Separator);
  try
    try
      DoUpdateFirmware;
    except
      on E: Exception do
      begin
        Logger.Error('Ошибка: ' + E.Message);
        raise;
      end;
    end;
  finally
    UpdateStatus;
    FStatus.IsStarted := False;
  end;
  Logger.Debug(Separator);
end;

procedure TFirmwareUpdater.DoUpdateFirmware;
var
  EcrInfo: TEcrInfo;
  Item: TUpdateItem;
  CashRegister: Currency;
  SearchParams: TSearchParams;
begin
  CashRegister := 0;
  SetStatusText('Обновление устройства');
  CheckStopped;
  Driver.Check(Connect);
  EcrInfo.UpdateMode := umDFU;
  EcrInfo := ReadEcrInfo;
  if EcrInfo.FirmwareValid then
  begin
    EcrInfo.UpdateMode := GetUpdateMode(EcrInfo.PortNumber);
    // Чтение наличных в ККМ
    if FParams.RestoreCashRegister then
    begin
      CashRegister := ReadCashRegister;
    end;
    // Чтение таблиц
    if FParams.SaveTables then
    begin
      ReadTables(EcrInfo.Serial);
    end;
    // Проверка режима ККМ
    if Driver.ECRMode <> MODE_CLOSED then
    begin
      raise Exception.Create('ККТ на связи. Однако в данном режиме перепрошивка невозможна.'#13#10+
        'Режим: ' + Driver.ECRModeDescription);
    end;
    PrintUpdateStarted;
  end;
  // Format SD card
  FormatSDCard;
  if EcrInfo.UpdateMode = umDFU then
  begin
    //SetVComConnection;
    // Update loader
    while FindUpdateItem(EcrInfo, ACTION_UPDATE_LOADER, Item) do
    begin
      DfuUploadFile(FPath, Item.FileName);
      DelayInMs(LoaderRebootDelay);
      WaitForDevice(EcrInfo.Serial, LoaderRebootTimeout);
      CheckLoaderUpdated(Item.NewBootVer);
      EcrInfo.BootVer := Item.NewBootVer;
      if Item.Force then Break;
    end;
    // Up
    if FindUpdateItem(EcrInfo, ACTION_UPDATE_FIRMWARE, Item) then
    begin
      DfuUploadFile(FPath, Item.FileName);
      DelayInMs(FirmwareRebootDelay);
      // Ищем устройство на порту VCOM
      SearchParams.Serial := EcrInfo.Serial;
      SearchParams.Timeout := FirmwareRebootTimeout;
      SearchParams.Port := PORT_VCOM;
      DiscoverDevice(SearchParams);
      // Проверка, обновилось ли ПО
      CheckFirmwareUpdated(Item);
      PrintUpdateCompleted;
    end;
  end else
  begin
    raise Exception.Create('Не реализовано');
  end;

  // Запись лицензий
  WriteLicenses(EcrInfo);
  if EcrInfo.FirmwareValid then
  begin
    if FParams.RestoreCashRegister and (CashRegister <> 0) then
    begin
      PrintCashIn(CashRegister);
    end;
    if FParams.SaveTables then
    begin
      WriteTablesFile(EcrInfo.Serial);
    end;
  end;
  WriteTables(Item.Tables);
  // Перезагрузка
  SetStatusText('Перезагрузка ККМ...');
  Driver.RebootKKT;
  Driver.Disconnect;
  SetStatusText('Перезагрузка ККМ: OK');
  // Читаем параметры подключения
  Driver.Check(Driver.LoadParams);
  DelayInMs(FirmwareRebootDelay);
  WaitForDevice(EcrInfo.Serial, FirmwareRebootTimeout);
  // Перерегистрация ФФД
  UpdateFFD(Item);

  SetStatusText(Format('Обновление выполнено успешно. Время выполнения: %d', [
    SecondsBetween(Now, FStatus.StartTime)]));
end;

procedure TFirmwareUpdater.SetVComConnection;
var
  Ecr: TEcrInfo;
  SearchParams: TSearchParams;
begin
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
    DiscoverDevice(SearchParams);
  end;
end;

procedure TFirmwareUpdater.UpdateFFD(const Item: TUpdateItem);
begin
  ChangeFFD(Item, FParams.FFDNeedUpdate);
end;

procedure TFirmwareUpdater.WriteLicenses(const Ecr: TEcrInfo);
var
  Item: TUpdateItem;
begin
  if FindUpdateItem(Ecr, ACTION_WRITE_LICENSES, Item) then
  begin
    WriteLicense(FPath + Item.FileName, Ecr.Serial);
  end;
end;

procedure TFirmwareUpdater.CheckLoaderUpdated(NewBootVer: Integer);
begin
  Driver.Check(Driver.ReadLoaderVersion);
  if Driver.LoaderVersion <> IntToStr(NewBootVer) then
  begin
    raise Exception.CreateFmt('Версия загрузчика отличается от целевой, %s <> %s',
    [Driver.LoaderVersion, IntToStr(NewBootVer)]);
  end;
end;

// Проверка типа подключения
function TFirmwareUpdater.GetUpdateMode(PortNumber: Integer): TUpdateMode;
var
  PppMode: Integer;
  RndisActive: Boolean;
begin
  Result := umDFU;
  case PortNumber of
    PORT_COM: Result := umXModem;
    PORT_VCOM: Result := umDFU;
    PORT_TCP:
    begin
      PppMode := Driver.ReadTableInt(21, 1, 1); // Режим ppp
      RndisActive := Driver.ReadTableInt(21, 1, 9) = 1; // Режим Rndis
      if RndisActive then
      begin
        Result := umDFU;
      end;
      if not RndisActive then
      begin
        case PppMode of
          PPP_NONE:
            raise Exception.Create('Прошивка по TCP не поддерживается');

          PPP_COM_CLIENT,
          PPP_COM_SERVER: Result := umXModem;

          PPP_VCOM_CLIENT,
          PPP_VCOM_SERVER: Result := umDFU;
        else
          raise Exception.CreateFmt('Неизвестный режим PPP, %d', [PppMode]);
        end;
      end;
    end;
  else
    raise Exception.CreateFmt('Прошивка не поддерживается, PortNumber=%d',
      [PortNumber]);
  end;
end;

function TFirmwareUpdater.ReadEcrInfo: TEcrInfo;
begin
  SetStatusText('Проверка состояния ККМ');

  Result.FirmwareValid := True;
  Driver.GetECRStatus;
  if (Driver.ResultCode = 123) or (Driver.ResultCode = 56) then
  begin
    Logger.Debug('HardwareFailure detected');
    Result.FirmwareValid := False;
    Exit;
  end;

  Result.PortNumber := Driver.PortNumber;
  Result.FirmwareVersion := '';
  Result.FirmwareBuild := -1;
  Result.FirmwareDate := -1;
  Result.BootVer := -1;
  Result.Serial := ReadSerialNumber;
  if Driver.ReadLoaderVersion = 0 then
  begin
    Result.BootVer := StrToInt(Driver.LoaderVersion);
  end else
  begin
    raise Exception.Create('ККТ на связи. Однако не удалось прочитать версию загрузчика, прошивка невозможна.');
  end;
  Result.FirmwareVersion := Driver.ECRSoftVersion;
  Result.FirmwareBuild := Driver.ECRBuild;
  Result.FirmwareDate := Driver.ECRSoftDate;
end;

procedure TFirmwareUpdater.UpdateInfoText(EcrInfo: TEcrInfo);
var
  Lines: TStrings;
  LoaderLine: string;
  FirmwareLine: string;
  Item: TUpdateItem;
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
    FStatus.UpdateAvailable := False;
    while FindUpdateItem(EcrInfo, ACTION_UPDATE_LOADER, Item) do
    begin
      FStatus.UpdateAvailable := True;
      EcrInfo.BootVer := Item.NewBootVer;
      LoaderLine := Format('Загрузчик до версии %d', [Item.NewBootVer]);
      if Item.Force then Break;
    end;
    if FindUpdateItem(EcrInfo, ACTION_UPDATE_FIRMWARE, Item) then
    begin
      FStatus.UpdateAvailable := True;
      FirmwareLine := Format('ПО ККМ до версии %s, сборка: %d от %s',
          [Item.fwver, Item.fwbuild, DateToStr(Item.fwdate)]);
    end;

    if FStatus.UpdateAvailable then
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
    FStatus.InfoText := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TFirmwareUpdater.LoadParameters;
begin
  try
    DownloadFiles;
    LoadFiles(FPath);
    UpdateInfoText(ReadEcrInfo);
    FStatus.Text := '';
  except
    on E: Exception do
    begin
      SetStatusText('Ошибка: ' + E.Message);
    end;
  end;
end;

procedure TFirmwareUpdater.DownloadFiles;
var
  FileName: string;
  ArchiveDownloaded: Boolean;
begin
  FPath := GetEnvironmentVariable('TEMP')+'\'+copy(TPath.GetRandomFileName,1,8)+'\';
  ForceDirectories(FPath);

  FileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'archive.zip';
  if FileExists(FileName) then
  begin
    UnzipArhive(FPath, FileName);
  end else
  begin
    ArchiveDownloaded := False;
    // Если обновления есть - скачиваем архив
    if CheckUpdateAvailable then
    begin
      ArchiveDownloaded := DownloadArchive;
    end;
    // Если не удалось скачать, то распаковываем архив из ресурсов
    if not ArchiveDownloaded then
      UnpackArhiveToDrive;

    UnzipArhive(Path + 'encrypt\', Path + 'arhive.zip');
    DecryptFiles(Path + 'encrypt\', Path);
  end;
end;

procedure TFirmwareUpdater.LoadFiles(const InPath: string);
var
  FileName: string;
begin
  FileName := InPath + 'rules.json';
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File: "%s" not found.', [FileName]);

  UpdateItemsLoadFromFile(FileName, FItems);
  UpdateParamsLoadFromFile(FileName, FParams);
  CheckFilesExists(InPath, FItems);
end;

// Cохраняем таблицы при обновлении приложения
procedure TFirmwareUpdater.ReadTables(const Serial: string);
begin
  CheckStopped;
  SetStatusText('Чтение таблиц...');
  ForceDirectories(GetBackupTablesPath);
  Driver.FileName := IncludeTrailingPathDelimiter(GetBackupTablesPath) + Serial + '.csv';
  if FileExists(Driver.FileName) then
    SysUtils.DeleteFile(Driver.FileName);
  Driver.Check(Driver.ExportTables);
  SetStatusText('Чтение таблиц: OK');
end;

const
  ArhiveURL = 'http://cb.litepass.ru:8080/techno/arhive_22.zip';

//Проверяем обновления на сервере
function TFirmwareUpdater.CheckUpdateAvailable: Boolean;
var
  ServerDate: TDateTime;
begin
  Result := False;
  try
    ServerDate := GetHttpFileLastModified(ArhiveURL);
    Result := ServerDate > 100;
  except
    on E: Exception do
    begin
      Logger.Error(E.Message);
    end;
  end;
end;

function TFirmwareUpdater.DownloadArchive: Boolean;
begin
  Result := False;
  try
    Result := DownloadFile(ArhiveURL, FPath + 'arhive.zip');
  except
    on E: Exception do
    begin
      Logger.Error(E.Message);
    end;
  end;
end;


procedure TFirmwareUpdater.UnpackArhiveToDrive;
var
  Stream: TResourceStream;
begin
  SetStatusText('Сохранение архива');
  Stream := TResourceStream.Create(HInstance,'arhive', RT_RCDATA);
  try
    Stream.SaveToFile(FPath + 'arhive.zip');
  finally
    Stream.Free;
  end;
end;

procedure TFirmwareUpdater.UnzipArhive(const OutPath, FileName: string);
var
  Zip: TZipFile;
begin
  ForceDirectories(OutPath);
  Zip := TZipFile.Create;
  try
    Zip.Open(FileName, zmRead);
    Zip.ExtractAll(OutPath);
  finally
    Zip.Free;
  end;
end;

procedure TFirmwareUpdater.DecryptFiles(const InPath, OutPath: string);
var
  i: Integer;
  FileExt: string;
  FileName: string;
  FileNames: TStrings;
  SearchRec: TSearchRec;
begin
  FileNames := TStringList.Create;
  try
    if FindFirst(InPath + '*.*', faAnyFile, SearchRec) = 0 then
    begin
      repeat
        FileNames.Add(SearchRec.Name);
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
    for i := 0 to FileNames.Count-1 do
    begin
      FileName := FileNames[i];
      FileExt := LowerCase(ExtractFileExt(FileName));
      if (FileExt = '.bin')or(FileExt = '.exe')or(FileExt = '.json') then
      begin
        DecryptFile(OutPath + FileName, InPath + FileName);
      end;
    end;
  finally
    FileNames.Free;
  end;
end;

procedure TFirmwareUpdater.DecryptFile(const DstFileName, SrcFileName: string);
var
  Codec: TCodec;
  Lib: TCryptographicLibrary;
const
  pass = 'alRzKCVueq9KiC6Eey6DuWWrc';
begin
  Codec := TCodec.Create(nil);
  Lib := TCryptographicLibrary.Create(nil);
  try
    Codec.AsymetricKeySizeInBits := 1024;
    Codec.AdvancedOptions2 := [];
    Codec.StreamCipherId := 'native.StreamToBlock';
    Codec.BlockCipherId := 'native.AES-256';
    Codec.ChainModeId := 'native.CBC';
    Codec.CryptoLibrary := Lib;

    Codec.Password := pass;
    Codec.DecryptFile(DstFileName, SrcFileName);
  finally
    Codec.Free;
    Lib.Free;
  end;
end;


function TFirmwareUpdater.EcrUpdateable(const Serial: string): Boolean;
const
  models: array [0..11] of string =
    (
    '0006','0004','0007','0008','0022','0002','0009','0001',
    '0041','0051','0044','0048'
    );
var
  i: Integer;
  Model: string;
begin
  Model := Copy(Serial, 7, 4);
  for i := Low(Models) to High(Models) do
  begin
    Result := Model = Models[i];
    if Result then Break;
  end;
end;

function TFirmwareUpdater.ReadSerialNumber: string;
begin
  Driver.Check(Driver.ReadSerialNumber);
  Result := Driver.SerialNumber;
end;

function TFirmwareUpdater.FindUpdateItem(const Data: TEcrInfo;
  Action: Integer; var Item: TUpdateItem): Boolean;
var
  Index: Integer;
begin
  Index := FindItemIndex(Data, Action);
  Result := Index <> -1;
  if Result then
    Item := FItems[Index];
end;

procedure TFirmwareUpdater.CheckFirmwareUpdated(const Item: TUpdateItem);
begin
  Driver.Check(Driver.GetECRStatus);
  begin
    if Driver.ECRSoftVersion <> Item.fwver then
      raise Exception.CreateFmt('Версия ПО ФР отличается, %s <> %s',
      [Driver.ECRSoftVersion, Item.fwver]);

    if Driver.ECRBuild <> Item.fwbuild then
      raise Exception.CreateFmt('Сборка ПО ФР отличается, %d <> %d',
      [Driver.ECRBuild, Item.fwbuild]);
  end;
end;

procedure TFirmwareUpdater.DfuUploadFile(const Path, FileName: string);
var
  ResultCode: Integer;
  DfuUtilFile: string;
  FirmwareFile: string;
  StdOut: string;
  StartTime: TDateTime;
begin
  StartTime := Now;
  Logger.Debug(Format('DFU, запись файла "%s"', [FileName]));
  // Проверка файлов
  FirmwareFile := Path + FileName;
  if not FileExists(FirmwareFile) then
    raise Exception.CreateFmt('Файл "%s" не найден.', [FirmwareFile]);

  DfuUtilFile := Path + 'dfu-util-static.exe';
  if not FileExists(DfuUtilFile) then
    raise Exception.CreateFmt('Файл "%s" не найден.', [DfuUtilFile]);

  SetStatusText('Проверка драйвера DFU');
  if not CheckDFU then
    raise Exception.Create('Не установлен драйвер DFU, прошивка невозможна.');

  // Переход в DFU
  SetStatusText('Переход в режим DFU');
  if Driver.SetDFUMode <> 0 then
  begin
    raise Exception.CreateFmt('Ошибка перехода в режим DFU. %s', [
      Driver.ResultCodeDescription]);
  end;
  Driver.Disconnect;
  // Ждём переход в DFU
  WaitForDFUDevice(DFUDelayTime);
  // Грузим файл
  SetStatusText('Запись файла ' + FileName);
  ResultCode := SystemUtils.ExecuteProcess(DfuUtilFile,' -D '+ FirmwareFile, StdOut);
  if ResultCode <> 0 then
    raise Exception.CreateFmt('Ошибка загрузки прошивки. %s, %s', [
    SysErrorMessage(GetLastError), StdOut]);

  if Pos('Done!', StdOut) = 0 then
  begin
    raise Exception('Не удалось загрузить прошивку в ККТ.'#13#10 + StdOut);
  end;
  SetStatusText('DFU, запись успешно выполнена');
  Logger.Debug(Format('DFU, запись выполнена за %d мс.', [
    MilliSecondsBetween(Now, StartTime)]));
end;

function TFirmwareUpdater.RunDfuUtil(const Path, Params: string): string;
var
  DfuUtilFile: string;
  ResultCode: Integer;
begin
  DfuUtilFile := Path + 'dfu-util-static.exe';
  if not FileExists(DfuUtilFile) then
    raise Exception.CreateFmt('Файл "%s" не найден.', [DfuUtilFile]);

  ResultCode := SystemUtils.ExecuteProcess(DfuUtilFile, Params, Result);
  if ResultCode <> 0 then
    raise Exception.CreateFmt('Ошибка выполнения. %s, %s', [
    SysErrorMessage(GetLastError), Result]);
end;

function TFirmwareUpdater.WaitForDFUDevice(Timeout: Cardinal): Boolean;
var
  StartTime: Cardinal;
begin
  StartTime := GetTickCount;
  while True do
  begin
    Result := IsDFUDevicePresent;
    if Result then Exit;

    if (GetTickCount - StartTime) >= Timeout then Break;
    Sleep(100); // Проверяем каждые 100 мс
  end;
end;

function TFirmwareUpdater.IsDFUDevicePresent: Boolean;
begin
  Result := Pos('DFU', RunDfuUtil(FPath, '-l')) <> 0;
end;


procedure TFirmwareUpdater.XModemUploadFile(ComNumber: Integer;
  const Path, FileName: string);
var
  Modem: TXModem;
begin
  Logger.Debug('WriteFirmware ' + FileName);
  Modem := TXModem.Create;
  try
    Modem.PortNumber := ComNumber;
    Modem.Baudrate := 115200;
    Modem.Timeout := 3000;
    //Modem.OnCancel := Cancelled;
    //Modem.OnPercent := OnPercent;
    Modem.SendFile(Path + FileName);
  finally
    Modem.Free;
  end;
end;

procedure TFirmwareUpdater.DelayInMs(DelayInMs: Integer);
var
  TickCount: Integer;
begin
  SetStatusText(Format('Задержка %d мс...', [DelayInMs]));
  TickCount := GetTickCount;
  repeat
    CheckStopped;
    Sleep(100);
  until Integer(GetTickCount) > TickCount + DelayInMs;
  SetStatusText(Format('Задержка %d мс: OK', [DelayInMs]));
end;

// По надобности, восстанавливаем таблицы
procedure TFirmwareUpdater.WriteTablesFile(const Serial: string);
begin
  CheckStopped;
  SetStatusText('Запись таблиц...');
  Driver.FileName := IncludeTrailingPathDelimiter(GetBackupTablesPath) + Serial + '.csv';
  Driver.Check(Driver.ImportTables);
  SetStatusText('Запись таблиц: OK');
end;

procedure TFirmwareUpdater.WriteTables(const Tables: TTableItems);
var
  i: Integer;
  Table: TTableItem;
begin
  CheckStopped;
  SetStatusText('Запись дополнительных таблиц...');
  // Дозапись таблиц после обновления
  for i := 0 to Length(Tables)-1 do
  begin
    Table := Tables[i];
    Driver.Password := 30;
    Driver.TableNumber := Table.Table;
    Driver.RowNumber := Table.Row;
    Driver.FieldNumber := Table.Field;
    if Table.FieldType = 0 then
      Driver.ValueOfFieldInteger := Table.IntValue
    else
      Driver.ValueOfFieldString := Table.StrValue;
    Driver.WriteTable;
  end;
  SetStatusText('Запись дополнительных таблиц: OK');
end;

procedure TFirmwareUpdater.WaitForDevice(const Serial: string;
  TimeoutInMs: Integer);
var
  TickCount: Integer;
begin
  SetStatusText('Ожидание устройства...');

  Driver.Timeout := 1000;
  TickCount := GetTickCount;
  repeat
    CheckStopped;
    if FindDevice(Serial) then
    begin
      Logger.Debug(Format('Устройство найдено за %d мс', [
        Integer(GetTickCount) - TickCount]));
      Logger.Debug('Проверка состояния ККМ');
      Driver.Check(Driver.ResetECR);
      Exit;
    end;
    Sleep(1000);
  until Integer(GetTickCount) > TickCount + TimeoutInMs;
  raise Exception.CreateFmt('Устройство %s не найдено', [Serial]);
  SetStatusText('Ожидание устройства: OK');
end;

function TFirmwareUpdater.FindDevice(const Serial: string): Boolean;
begin
  Result := Connect = 0;
  if Result then
  begin
    Result := Driver.ReadSerialNumber = 0;
    if not Result then
    begin
      Logger.Debug(Format('ReadSerialNumber: %d, %s', [
        Driver.ResultCode, Driver.ResultCodeDescription]));
    end;

    if Result then
    begin
      Result := (Serial = Driver.SerialNumber) or (Serial = '');
    end;
  end;
end;

function TFirmwareUpdater.Connect: Integer;
var
  CurrDate: TDateTime;
begin
  Result := Driver.GetShortECRStatus;
  // FPTR_E_RAM_FAIL  = $74; // ошибка в ОЗУ ()
  // FPTR_E_NEW_SOFT  = $78; // замена ПО
  // FPTR_E_FP_CHANGE = $79; // ошибка замена ФП (новая)
  if Result in [$74, $79, $78] then
  begin
    Result := Driver.ResetSettings;
    // FPTR_E_FORMAT_RESTORE_SUCCESS = $93; // восстановление озу успешно
    if Result = 147 then
        Result := Driver.ResetSettings;
    if Result <> 0 then Exit;

    Result := Driver.GetShortECRStatus;
    if Result <> 0 then Exit;
  end;
  if Result = 0 then
  begin
    if Driver.ECRMode = MODE_TECH then
    begin
      CurrDate := Date;
      Driver.Date := CurrDate;
      Driver.Check(Driver.SetDate);
      Driver.Check(Driver.ConfirmDate);
      Driver.Time := Time;
      Driver.Check(Driver.SetTime);
    end;
  end;
end;

procedure TFirmwareUpdater.DiscoverDevice(const Params: TSearchParams);
var
  TickCount: Integer;
begin
  SetStatusText('Поиск устройства...');
  Driver.Timeout := 1000;
  TickCount := GetTickCount;
  repeat
    CheckStopped;
    if FindDeviceLocal(Params) then
    begin
      Logger.Debug(Format('Устройство найдено за %d мс', [
        Integer(GetTickCount) - TickCount]));
      Logger.Debug('Проверка состояния ККМ');
      Exit;
    end;

    Sleep(100);
  until Integer(GetTickCount) > TickCount + Params.Timeout;

  raise Exception.CreateFmt('Устройство %s не найдено', [Params.Serial]);
end;

//////////////////////////////////////////////////////////
// Для подключения COM - 4800
// Для подключения VCOM - любая скорость
//

function TFirmwareUpdater.FindDeviceLocal(const Params: TSearchParams): Boolean;
var
  i: Integer;
  Port: TSearchPort;
  Search: TDeviceSearch;
begin
  Result := False;
  Driver.ConnectionType := CT_LOCAL;
  Search := TDeviceSearch.Create;
  try
    Search.DoTechReset := True;
    Search.ShortSearch := False;

    Search.Start;
    Search.Wait;
    for i := 0 to Search.Ports.Count - 1 do
    begin
      Port := Search.Ports[i];
      if ((Port.SerialNumber = Params.Serial) or (Params.Serial = ''))and
        (Params.Port = Port.DevicePort) then
      begin
        Logger.Debug('Устройство найдено');
        Driver.ComNumber := Port.PortNumber;
        Driver.BaudRate := Port.BaudRate;
        Driver.Timeout := 1000;
        if Driver.BaudRate < BAUD_RATE_CODE_115200 then
        begin
          Logger.Debug('Установка скорости ККМ 115200');
          Driver.BaudRate := BAUD_RATE_CODE_115200;
          Driver.Check(Driver.SetExchangeParam);
          Driver.Disconnect;
        end;
        Result := True;
        Break;
      end;
    end;
  finally
    Search.Free;
  end;
end;


procedure TFirmwareUpdater.SetBaudRate(BaudRate: Integer);
begin
  //Logger.Debug('Set baudrate ' + IntToStr(ABaudrate));
  //Driver.PortNumber := 0;
  Driver.Timeout := 10000;
  Driver.BaudRate := BaudRate;
  Driver.Check(Driver.SetExchangeParam);
  Driver.BaudRate := Baudrate;
end;

procedure TFirmwareUpdater.SetStatusText(const Text: string);
begin
  Logger.Debug(Text);
  FLock.Enter;
  try
    FStatus.Text := Text;
  finally
    FLock.Leave;
  end;
end;

procedure TFirmwareUpdater.UpdateStatus;
begin
  if FStatus.IsStarted then
  begin
    FStatus.ElapsedSeconds := SecondsBetween(Now, FStatus.StartTime);
    FStatus.TimeText := Format('Время начала: %s, прошло секунд, %d', [
      TimeToStr(Status.StartTime), Status.ElapsedSeconds]);
  end;
end;

function TFirmwareUpdater.GetStatus: TUpdateStatus;
begin
  Result := FStatus;
end;

procedure TFirmwareUpdater.CheckFilesExists(const Path: string; Items: TUpdateItems);
var
  I: Integer;
begin
  for I := 0 to Length(Items)-1 do
  begin
    if not FileExists(Path + Items[i].FileName) then
      raise Exception.Create('Недостаточно файлов для работы.');
  end;
end;

function TFirmwareUpdater.CheckEcrUpdateable: Boolean;
begin
  Result := EcrUpdateable(ReadSerialNumber);
end;

function TFirmwareUpdater.ReadLicense(const FileName: string;
  const Serial: string;
  var License: TEcrLicense): Boolean;
var
  i: Integer;
  Strings: TStringList;
begin
  Result := False;
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    for i := 0 to Strings.Count - 1 do
    begin
      if AddLeadingZeros(SplitLine(Strings[i], 0), 16) = AddLeadingZeros(Serial, 16) then
      begin
        License.Data := SplitLine(Strings[i], 1);
        License.Signature := SplitLine(Strings[i], 2);
        Result := True;
        Break;
      end;
    end;
  finally
    Strings.Free;
  end;
end;

procedure TFirmwareUpdater.WriteLicense(const FileName, Serial: string);
var
  License: TEcrLicense;
begin
  if not ReadLicense(FileName, Serial, License) then
  begin
    Logger.Debug(Format('Лицензия для ККМ %s не найдена', [Serial]));
    Exit;
  end;

  Logger.Debug('Запись лицензий');
  Driver.License := License.Data;
  Driver.DigitalSign := License.Signature;
  if Driver.WriteFeatureLicenses <> 0 then
  begin
    raise Exception.CreateFmt('Ошибка записи лицензий, %d, %s', [
      Driver.ResultCode, Driver.ResultCodeDescription]);
  end;
end;

function TFirmwareUpdater.ReadFFDVersion: Integer;
var
  Modelid: Integer;
begin
  Modelid := Driver.GetModelID;
  if Modelid = 152 then
    Result := Driver.ReadTableInt(17, 1, 17)
  else
  begin
    if ModelId = 19 then
      Result := Driver.ReadTableInt(10, 1, 4)
    else
      if IsModelType2(ModelId) then
        Result := Driver.ReadTableInt(10, 1, 29)
      else
        Result := Driver.ReadTableInt(17, 1, 17);
  end;
end;

procedure TFirmwareUpdater.ChangeFFD(const Item: TUpdateItem;
  AFFD: TFFDNeedUpdate);
var
  Text: string;
  OfdInn: string;
  Reason: Integer;
  FFDVer: Integer;
  OfdParams: TOfdParams;
  IsFFDChanged: Boolean;
begin
  CheckDocSent;

  case AFFD of
    NoUpdateNeeded:
    begin
      Logger.Debug('No update needed');
      Exit;
    end;
    FFD105: FFDVer := 2;
    FFD12: FFDVer := 4;
  else
    FFDVer := 4;
  end;

  if FParams.PrintStatus then
  begin
    Feed(2);
    PrintText('ПРОИЗВОДИТСЯ ПЕРЕРЕГИСТРАЦИЯ ККТ НА ФФД 1.2');
    PrintText('НЕ ОТКЛЮЧАЙТЕ ПИТАНИЕ КАССЫ И КОМПЬЮТЕРА');
    PrintText('ДОЖДИТЕСЬ ПЕЧАТИ СООБЩЕНИЯ');
    PrintText('О ЗАВЕРШЕНИИ ПЕРЕРЕГИСТРАЦИИ');
    Feed(14);
  end;
  SetStatusText('Печать отчета о состоянии расчетов...');
  Driver.Check(Driver.FNBuildCalculationStateReport);
  Driver.WaitForPrinting;
  SetStatusText('Печать отчета о состоянии расчетов: OK');

  Text := Format('Перерегистрация ККТ на %s', [FFDToStr(FFDVer)]);
  SetStatusText(Text + '...');
  IsFFDChanged := Driver.ReadTableInt(17, 1, 17) <> FFDVer;
  Driver.WriteTableInt(17, 1, 17, FFDVer);
  OfdInn := Driver.ReadTableStr(18, 1, 12);
  if GetOfdParams(Item, OfdInn, OfdParams) then
  begin
    if OfdParams.ServerKM <> '' then
      Driver.WriteTableStr(19, 1, 5, OfdParams.ServerKM);
    if OfdParams.PortKM <> 0 then
      Driver.WriteTableInt(19, 1, 6, OfdParams.PortKM);
  end;

  if IsFFDChanged then
  begin
    Reason := Driver.ReadTableInt(18, 1, 22) or $200000;
    Driver.WriteTableInt(18, 1, 22, Reason);
  end;

  Driver.RegistrationReasonCode := 4; // Изменение настроек ККТ
  Driver.Inn := Trim(Driver.ReadTableStr(18, 1, 2));
  Driver.KKTRegistrationNumber := Trim(Driver.ReadTableStr(18, 1, 3));
  Driver.TaxType := Driver.ReadTableInt(18, 1, 5);
  Driver.WorkMode := Driver.ReadTableInt(18, 1, 6);
  Driver.Check(Driver.FNBuildReregistrationReport);
  Driver.WaitForPrinting;
  SetStatusText(Text + ': OK');

  if FParams.PrintStatus then
  begin
    Feed(2);
    PrintText('ПРОИЗВЕДЕНА АВТОМАТИЧЕСКАЯ ПЕРЕРЕГИСТРАЦИЯ');
    PrintText('НА ФФД 1.2');
    Feed(12);
    Driver.WaitForPrinting;
    Driver.FinishDocument;
    Driver.WaitForPrinting;
  end;
end;

procedure TFirmwareUpdater.Feed(ALineCount: Integer);
begin
  Driver.Check(Driver.WaitForPrinting);
  Driver.UseReceiptRibbon := True;
  Driver.StringQuantity := ALineCount;
  Driver.Check(Driver.FeedDocument);
end;

procedure TFirmwareUpdater.PrintText(const AStr: string);
begin
  Driver.UseReceiptRibbon := True;
  Driver.StringForPrinting := AStr;
  Driver.Check(Driver.PrintStringWithWrap);
end;

// Ожидание отправки всех документов в ОФД
function TFirmwareUpdater.WaitDocSent(TimeoutInSec: Integer): Boolean;
var
  TickCount: Cardinal;
begin
  SetStatusText('Ожидание отправки сообщений в ОФД');
  TickCount := GetTickCount;
  while True do
  begin
    CheckStopped;

    Driver.Check(Driver.FNGetInfoExchangeStatus);
    Result := Driver.MessageCount = 0;
    if Result then Break;

    if Abs(GetTickCount - TickCount) > (1000 * TimeoutInSec) then Break;
    Sleep(500);
  end;
end;

procedure TFirmwareUpdater.CheckDocSent;
var
  IsTimeout: Boolean;
begin
  SetStatusText('Проверка отправленныз документов...');
  IsTimeout := not WaitDocSent(FParams.DocSentTimeoutInSec);
  if IsTimeout and FParams.PrintStatus then
  begin
    Feed(2);
    PrintText('ВНИМАНИЕ!');
    PrintText('НЕВОЗМОЖНО ПРОИЗВЕСТИ ПЕРЕРЕГИСТРАЦИЮ НА ФФД 1.2');
    PrintText('ИЗ-ЗА ОТСУТСТВИЯ СВЯЗИ С ОФД');
    PrintText('');
    PrintText('ВОССТАНОВИТЕ СВЯЗЬ');
    PrintText('СЛЕДУЮЩАЯ ПОПЫТКА ПЕРЕРЕГИСТРАЦИИ');
    PrintText('БУДЕТ ПРОИЗВЕДЕНА ПРИ ОТКРЫТИИ СМЕНЫ');
    Feed(15);
    Driver.WaitForPrinting;
    Driver.FinishDocument;
    Driver.WaitForPrinting;
  end;
  if IsTimeout then
    raise Exception.Create(
      'Перерегистрация на ФФД 1.2 не может быть произведена. Есть неотправленные в ОФД документы');
  SetStatusText('Проверка отправленныз документов: OK');
end;

function TFirmwareUpdater.ReadCashRegister: Currency;
begin
  CheckStopped;
  SetStatusText('Чтение счетчика наличных');

  Driver.RegisterNumber := 241;
  Driver.Check(Driver.GetCashReg);
  Result := Driver.ContentsOfCashRegister;
end;

procedure TFirmwareUpdater.PrintUpdateStarted;
begin
  if FParams.PrintStatus then
  begin
    CheckStopped;
    SetStatusText('Печать предупреждения');

    Feed(2);
    PrintText('ВНИМАНИЕ!');
    PrintText('ИДЕТ ОБНОВЛЕНИЕ ПРОШИВКИ');
    PrintText('НЕ ОТКЛЮЧАЙТЕ ПИТАНИЕ КАССЫ И КОМПЬЮТЕРА');
    PrintText('ДОЖДИТЕСЬ СООБЩЕНИЯ О ЗАВЕРШЕНИИ ОБНОВЛЕНИЯ');
    Feed(14);
  end;
end;

procedure TFirmwareUpdater.PrintUpdateCompleted;
begin
  if FParams.PrintStatus then
  begin
    CheckStopped;
    SetStatusText('Печать подтверждения');

    Driver.Check(Driver.GetECRStatus);
    PrintText('ОБНОВЛЕНИЕ ПРОШИВКИ ПРОШЛО УСПЕШНО');
    PrintText('ТЕКУЩАЯ ВЕРСИЯ: ' + DateToStr(Driver.ECRSoftDate) + ' ' + Driver.ECRSoftVersion);
    Feed(14);
  end;
end;

procedure TFirmwareUpdater.FormatSDCard;
begin
  CheckStopped;
  SetStatusText('Форматирование SD карты');
  Driver.BinaryConversion := BINARY_CONVERSION_HEX;
  Driver.TransferBytes := 'FE 04 00 00 00 00';
  Driver.ExchangeBytes;
end;

// Восстановление счетчика наличных
procedure TFirmwareUpdater.PrintCashIn(Amount: Currency);
begin
  CheckStopped;
  SetStatusText('Восстановление счетчика наличных');
  Driver.Summ1 := Amount;
  Driver.Check(Driver.CashIncome);
  Driver.Check(Driver.WaitForPrinting);
end;

//Logger.Debug('Обновление параметров сервера КМ для ИНН ОФД ' + OfdInn);
//Logger.Debug('ServerKM: ' + ServerKM);
//Logger.Debug('PortKM: ' + PortKM.ToString);

function TFirmwareUpdater.FindOfdParams(const OfdInn: string;
  var OfdParams: TOfdParams): Boolean;
var
  Ofd: TOfdParams;
begin
  for Ofd in OfdParamsArray do
  begin
    Result := Ofd.Inn = Trim(OfdInn);
    if Result then
    begin
      OfdParams := Ofd;
      Break;
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////////////
//19,1,5,64,1,0,3000,'Сервер км','192.168.144.138'
//19,1,6,2,0,1,65535,'Порт км','8789'

function TFirmwareUpdater.GetOfdParams(const UpdateItem: TUpdateItem;
  const OfdInn: string; var OfdParams: TOfdParams): Boolean;
var
  Ofd: TOfdParams;
  Item: TTableItem;
begin
  Ofd.Inn := OfdInn;
  Ofd.ServerKM := '';
  Ofd.PortKM := 0;
  for Item in UpdateItem.Tables do
  begin
    if (Item.Table = 19)and(Item.Row = 1)and (Item.Field = 5) then
    begin
      Ofd.ServerKM := Item.StrValue;
    end;
    if (Item.Table = 19)and(Item.Row = 1)and (Item.Field = 6) then
    begin
      Ofd.PortKM := Item.IntValue;
    end;
  end;
  Result := (Ofd.ServerKM <> '')and(Ofd.PortKM <> 0);
  if Result then
  begin
    OfdParams := Ofd;
  end else
  begin
    Result := FindOfdParams(OfdInn, OfdParams);
  end;
end;


function TFirmwareUpdater.ValidUpdateItem(const Ecr: TEcrInfo;
  const Item: TUpdateItem): Boolean;
begin
  case Action of
    ACTION_UPDATE_LOADER:
    begin
      // Если нужно переписывать для тестирования
      Result := Item.Force and (Item.NewBootVer = Ecr.BootVer);
      if Result then Exit;

      Result := Item.CurrBootVer = Ecr.BootVer;
      if Result then Exit;

      if Item.CurrBootVer = 0 then
      begin
        Result := (Ecr.BootVer <> -1)and(Item.NewBootVer > Ecr.BootVer) and (Ecr.BootVer > 129);
      end;
    end;

    ACTION_UPDATE_FIRMWARE:
    begin
      if  Ecr.BootVer >= Item.CurrBootVer then
      begin
        Result := True;
        if (Item.fwbuild = Ecr.FirmwareBuild) and
          (Item.fwver = Ecr.FirmwareVersion) then
        begin
          if not Item.Force then
            Result := False;
        end;
      end;
    end;
  else
    Result := True;
  end;
end;

function TFirmwareUpdater.FindItemIndex(const Ecr: TEcrInfo;
  Action: Integer): Integer;
var
  i: Integer;
  Item: TUpdateItem;
begin
  Result := -1;
  for i := Low(FItems) to High(FItems) do
  begin
    Item := FItems[i];
    if (Item.Action = Action)and ValidUpdateItem(Ecr, Item) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

///////////////////////////////////////////////////////////////////////////////
// 1. Если документы не уходят, то можно перезагрузить устройство
// или пропинговать сервер? или еще как-то возобновить сетевое подключение
// или нужно проверить что документы ушли перед обновлением ПО
//
// 2. Протестировать обновление ПО на возможные ошибки - прервать обновление
// на разных этапах.
//
// 3. Проверить переключение в режим VCOM до обновления ПО
//
///////////////////////////////////////////////////////////////////////////////


end.
