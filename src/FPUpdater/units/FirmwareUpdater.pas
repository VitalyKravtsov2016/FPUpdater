unit FirmwareUpdater;

interface

{ DONE: Добавить прошивку по протоколу XModem }
{ TODO: Написать тесты для обновления ФР с любыми параметрами }
{ TODO: Проверить правильность лога прошивальщика - сохранение состояния ФР и ФН }
{ TODO: Сделать проверку записи тестового ПО в ККМ с рабочими ключами и наоборот }
{ TODO: Протестировать обновление ПО на возможные ошибки - прервать обновление на разных этапах }
{ TODO: Перед перерегистрацией нужно синхронизировать время ККМ и ПК }

uses
  // VCL
  Windows, SysUtils, Classes, Registry, SyncObjs, ShlObj, System.Zip,
  System.IOUtils, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.DateUtils, ComObj, ActiveX,
  // JVCL
  JvSetupApi,
  // TurboPower
  uTPLb_Codec, uTPLb_CryptographicLibrary,
  // This
  DrvFRLib_TLB, untDriver, SystemUtils, UpdateItem, NotifyThread, StringUtils,
  LogFile, BinUtils, FptrTypes, XModem, DeviceSearch, SearchPort;

const
  DFUDelayTime = 5000;
  LoaderRebootDelay = 3000;
  FirmwareRebootDelay = 3000;
  RebootTimeout = 6000;
  LoaderRebootTimeout = 60000;
  FirmwareRebootTimeout = 60000;

  // TODO: Обновление параметров сервера КМ для ИНН ОФД ??

  /// //////////////////////////////////////////////////////////////////////////
  // PortNumber значения

  PORT_COM = 0; // RS-232 - обновление по XModem
  PORT_VCOM = 1; // USB vCOM - обновление по DFU (или XModem)
  PORT_TCP = 2; // TCP сокет (RNDIS, Ethernet, WI-FI, ppp)

  /// //////////////////////////////////////////////////////////////////////////
  // Значения режима ppp

  PPP_NONE = 0; // 0 – нет
  PPP_COM_CLIENT = 1; // 1 – клиент через COM
  PPP_COM_SERVER = 2; // 2 – сервер через COM
  PPP_VCOM_CLIENT = 3; // 3 – клиент через USB
  PPP_VCOM_SERVER = 4; // 4 – сервер через USB

  /// //////////////////////////////////////////////////////////////////////////
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
  MODE_DUMPMODE = $01; // Dump mode
  MODE_24NOTOVER = $02; // Opened day, 24 hours not left
  MODE_24OVER = $03; // Opened day, 24 hours is over
  MODE_CLOSED = $04; // Closed day
  MODE_LOCKED = $05; // ECR is bloced because of incorrect tax offecer password
  MODE_WAITDATE = $06; // Waiting for date confirm
  MODE_POINTPOS = $07; // Change decimal point position permission
  MODE_REC = $08; // Opened document
  MODE_TECH = $09; // Technological reset permission
  MODE_TEST = $0A; // Test run
  MODE_FULLREPORT = $0B; // Full fiscal report printing
  MODE_EKLZREPORT = $0C; // EJ report printing
  MODE_SLP = $0D; // Opened fiscal slip
  MODE_SLPPRINT = $0E; // Slip printing
  MODE_SLPREADY = $0F; // Fiscal slip is ready

  /// //////////////////////////////////////////////////////////////////////////
  // BaudRate values

  BAUD_RATE_CODE_2400 = 0;
  BAUD_RATE_CODE_4800 = 1;
  BAUD_RATE_CODE_9600 = 2;
  BAUD_RATE_CODE_19200 = 3;
  BAUD_RATE_CODE_38400 = 4;
  BAUD_RATE_CODE_57600 = 5;
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
    FirmwareVersion: string; // Версия ПО ФР
    FirmwareBuild: Integer; // Сборка ПО ФР
    FirmwareDate: TDate; // Дата ПО ФР
    BootVer: Integer; // Версия загрузчика
    Serial: string;
    FirmwareValid: Boolean; //
    // Тип подключения ККМ, COM, VCOM, TCP (0, 1, 2)
    SoftwarePort: Integer;
    // Физическое подключение ККМ, COM, VCOM, TCP
    // Например, если касса подключена по RNDIS, то после сброса параметров
    // искать ее нужно на порту VCOM, так как физически она подключена через USB
    HardwarePort: Integer; // TCP -> VCOM
    // Номер COM порта, если HardwarePort = COM, VCOM
    ComNumber: Integer;
    SigningKey: Integer; // Ключи для подписи
  end;

  { TUpdateStatus }

  TUpdateStatus = record
    Text: string;
    ResultText: string;
    StartTime: TDateTime;
    UpdateAvailable: Boolean;
    IsStarted: Boolean;
    IsSucceeded: Boolean;
  end;

  { TFirmwareUpdater }

  TFirmwareUpdater = class
  private
    FPath: string;
    FDriver: TDriver;
    FThread: TNotifyThread;
    FLock: TCriticalsection;
    FStopped: Boolean;
    FStatus: TUpdateStatus;
    FItems: TUpdateItems;
    FParams: TUpdateParams;
    FOnComplete: TNotifyEvent;
    FFilesDownloaded: Boolean;

    function GetDriver: TDriver;
    procedure SetStatus(const Value: TUpdateStatus);
    procedure ThreadTerminated(Sender: TObject);
    function ValidLoader(const Ecr: TECRInfo; const Item: TActionUpdateLoader): Boolean;
    function ValidFirmware(const Ecr: TECRInfo; const Item: TActionUpdateFirmware): Boolean;
    procedure CheckFSDebugVersion;
    procedure FiscalizeFS(Action: TActionFiscalizeFS);
    procedure InitializeFS(Item: TActionInitFS);
    procedure RefiscalizeFS(Item: TActionRefiscalizeFS; const Tables: TTableItems);
    procedure UploadFile(const Ecr: TECRInfo; const Path, FileName: string);
    function XModemCancelProc: Boolean;
    function GetHardwarePort(SoftwarePort: Integer): Integer;
    procedure SetCurrentDateTime;
    function IsRefiscalizationNeeded: Boolean;
  public
    procedure ShowProperties;
    procedure DeleteFiles;
    procedure DeleteLog;
    procedure CheckStopped;
    procedure DownloadFiles;
    procedure UnpackArhiveToDrive;
    procedure Execute(Sender: TObject);
    procedure SetStatusText(const Text: string);
    procedure DecryptFile(const DstFileName, SrcFileName: string);
    procedure CheckFilesExists(const Path: string);
    procedure CheckFirmwareUpdated(const Item: TActionUpdateFirmware);
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
    procedure XModemUploadFile(ComNumber: Integer; const FileName: string);
    procedure WriteLicense(const FileName, Serial: string);
    procedure WaitForDevice(const Serial: string; TimeoutInMs: Integer);
    procedure LoadArchive(const FileName: string);

    function Connect: Integer;
    function ReadEcrInfo: TECRInfo;
    function DownloadArchive(const ArchiveURL: string): Boolean;
    function GetStatus: TUpdateStatus;
    function ReadSerialNumber: string;
    function EcrUpdateable(const Serial: string): Boolean;
    function ReadFFDVersion: Integer;
    function ReadLicense(const FileName, Serial: string; var License: TEcrLicense): Boolean;
    function ReadCashRegister: Currency;
    function GetLogFileName: string;
    procedure DoUpdateFirmware;
    function FindDevice(const Serial: string): Boolean;
    procedure DiscoverDevice(const Params: TSearchParams);
    function FindDeviceLocal(const Params: TSearchParams): Boolean;
    function FindOfdParams(const OfdInn: string; var OfdParams: TOfdParams): Boolean;
    function GetOfdParams(const Tables: TTableItems; const OfdInn: string; var OfdParams: TOfdParams): Boolean;
    function WaitDocSent(TimeoutInSec: Integer): Boolean;
    procedure DecryptFiles(const InPath, OutPath: string);
    procedure UnzipArhive(const OutPath, FileName: string);
    procedure LoadFiles(const InPath: string);

    function RunDfuUtil(const Path, Params: string): string;
    function IsDFUDevicePresent: Boolean;
    function WaitForDFUDevice(Timeout: Cardinal): Boolean;
    procedure SetVComConnection;
    procedure UpdateFirmware;
    function CheckEcrUpdateable: Boolean;
    function FindDeviceLocal2(const Params: TSearchParams): Boolean;

    property Items: TUpdateItems read FItems;
    property Path: string read FPath write FPath;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Wait;
    procedure LoadParameters;
    function FindFirmware(Ecr: TECRInfo): TActionUpdateFirmware;
    function FindLastLoader(Ecr: TECRInfo): TActionUpdateLoader;

    property Driver: TDriver read GetDriver;
    property Params: TUpdateParams read FParams;
    property Status: TUpdateStatus read GetStatus write SetStatus;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
  end;

function GenerateRNM(const ANumber, AINN, ASerial: string): string;

implementation

function AddLeadingZeros(const S: string; ACount: Integer): string;
begin
  Result := Copy(S, 1, ACount);
  if ACount < Length(S) then
    Exit;
  Result := StringOfChar('0', ACount - Length(S)) + S;
end;

function GenerateRNM(const ANumber, AINN, ASerial: string): string;
var
  S: string;
begin
  S := AddLeadingZeros(ANumber, 10) + AddLeadingZeros(AINN, 12) + AddLeadingZeros(ASerial, 20);
  Result := AddLeadingZeros(ANumber, 10) + AddLeadingZeros(IntToStr(CRCCITT16(AnsiString(S), $1021, $FFFF)), 6);
end;

const
  OfdParamsArray: array[1..13] of TOfdParams = ((
    Inn: '7728699517';
    ServerKM: 'connect.Ofd-ya.ru';
    PortKM: 7797
  ), (
    Inn: '7709364346';
    ServerKM: 'k-server.1-Ofd.ru';
    PortKM: 7788
  ), (
    Inn: '9715260691';
    ServerKM: 'Ofdp.platformaOfd.ru';
    PortKM: 21102
  ), (
    Inn: '7704211201';
    ServerKM: 'f1.taxcom.ru';
    PortKM: 8777
  ), (
    Inn: '6658497833';
    ServerKM: 'Ofd.kontur.ru';
    PortKM: 7778
  ), (
    Inn: '4029017981';
    ServerKM: 'Ofd.astralnalog.ru';
    PortKM: 7777
  ), (
    Inn: '7841465198';
    ServerKM: 'crpt.Ofd.ru';
    PortKM: 7000
  ), (
    Inn: '7605016030';
    ServerKM: 'kkt.sbis.ru';
    PortKM: 7777
  ), (
    Inn: '7704358518';
    ServerKM: 'kkt.Ofd.yandex.net';
    PortKM: 54321
  ), (
    Inn: '5902034504';
    ServerKM: 'kkt.Ofd-initpro.ru';
    PortKM: 9996
  ), (
    Inn: '7729642175';
    ServerKM: 'crpt.e-Ofd.ru';
    PortKM: 5555
  ), (
    Inn: '2310031475';
    ServerKM: 'kkt.Ofd-magnit.ru';
    PortKM: 7005
  ), (
    Inn: '7713076301';
    ServerKM: 'Ofd.beeline.ru';
    PortKM: 8765
  ));

function FFDToStr(AFFDVer: Integer): string;
begin
  case AFFDVer of
    2:
      Result := 'ФФД 1.05';
    4:
      Result := 'ФФД 1.2';
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
  Result := Value in [16, 19, 20, 21, 27, 28, 29, 30, 32, 33, 34, 35, 36,
  { 37, } 38, 39, 40, 41, 42, 45, 45, 45, 46];
end;

const
  EcrModels: array[0..0] of TEcrModel = ((
    Id: 0;
    Name: 'Test'
  ));

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

/// ////////////////////////////////////////////////////////////////////////
// PortNumber значения
// 0, RS-232 - обновление по XModem
// 1, USB vCOM - обновление по DFU (или XModem)
// 2, TCP сокет (RNDIS, Ethernet, WI-FI, ppp) -
// - обновление через DFU, если RNDIS
// - обновление через DFU, если PPP через VCom
// - обновление через XModem, если PPP через COM
//
/// ////////////////////////////////////////////////////////////////////////

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
    Result := Reg.OpenKey('SYSTEM\CurrentControlSet\Control\DeviceClasses\{a01674b4-c5f6-485c-af94-3271701d5fb4}', false);
  finally
    Reg.Free;
  end;
end;

function GetHttpFileLastModified(const URL: string): TDateTime;
var
  HttpClient: TNetHTTPClient;
  Response: IHTTPResponse;
begin
  Result := 0;
  HttpClient := TNetHTTPClient.Create(nil);
  try
    HttpClient.UserAgent := 'Mozilla/5.0';
    // Делаем HEAD запрос (только заголовки, без тела)
    Response := HttpClient.Head(URL);
    if Response.StatusCode = 200 then
    begin
      if Response.ContainsHeader('Last-Modified') then
      begin
        // Парсим дату из заголовка
        Result := HTTPToDate(Response.HeaderValue['Last-Modified']);
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

function DownloadFile(const URL, SavePath: string; OnProgress: TReceiveDataEvent = nil): Boolean;
var
  HttpClient: TNetHTTPClient;
  Response: IHTTPResponse;
  FileStream: TFileStream;
begin
  HttpClient := TNetHTTPClient.Create(nil);
  try
    HttpClient.UserAgent := 'Mozilla/5.0';
    HttpClient.ConnectionTimeout := 3000;
    HttpClient.ResponseTimeout := 30000;

    if Assigned(OnProgress) then
      HttpClient.OnReceiveData := OnProgress;

    FileStream := TFileStream.Create(SavePath, fmCreate);
    try
      Response := HttpClient.Get(URL, FileStream);
      Result := (Response.StatusCode = 200);
    finally
      FileStream.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

{ TFirmwareUpdater }

constructor TFirmwareUpdater.Create;
begin
  inherited Create;
  FItems := TUpdateItems.Create;
  FLock := TCriticalsection.Create;

  Logger.FileName := GetLogFileName;
  Logger.Enabled := True;

  FParams.SaveTables := True;
  FParams.PrintStatus := false;
  FParams.RestoreCashRegister := false;
end;

destructor TFirmwareUpdater.Destroy;
begin
  Stop;
  FLock.Free;
  FItems.Free;
  FDriver.Free;
  inherited Destroy;
end;

function TFirmwareUpdater.GetDriver: TDriver;
begin
  if FDriver = nil then
    FDriver := TDriver.Create(nil);
  Result := FDriver;
end;

function TFirmwareUpdater.GetLogFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.log');
end;

procedure TFirmwareUpdater.DeleteLog;
begin
  DeleteFile(GetLogFileName);
end;

procedure TFirmwareUpdater.Start;
begin
  if Status.IsStarted then
    raise Exception.Create('Обновление уже идет.');

  Stop;
  FDriver.Free;
  FDriver := nil;

  FStopped := false;
  FThread := TNotifyThread.CreateThread(Execute);
  FThread.OnTerminate := ThreadTerminated;
  FThread.Resume;
end;

procedure TFirmwareUpdater.Stop;
begin
  FStopped := True;
  if FThread <> nil then
  begin
    FThread.WaitFor;
    FThread.Free;
    FThread := nil;
  end;
end;

procedure TFirmwareUpdater.ThreadTerminated(Sender: TObject);
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
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
  if FStopped then
    raise Exception.Create('Прервано пользователем');
end;

const
  Separator = '------------------------------------------------------------';

procedure TFirmwareUpdater.Execute(Sender: TObject);
begin
  try
    UpdateFirmware;
  except
    on E: Exception do
    begin
      //
    end;
  end;
end;

procedure TFirmwareUpdater.UpdateFirmware;
var
  AStatus: TUpdateStatus;
begin
  AStatus := GetStatus;
  AStatus.StartTime := Now;
  AStatus.IsStarted := True;
  AStatus.IsSucceeded := false;
  SetStatus(AStatus);

  Logger.Debug(Separator);
  try
    OleCheck(CoInitialize(nil));
    try
      DoUpdateFirmware;
    finally
      FDriver.Free;
      FDriver := nil;
      CoUninitialize;
    end;

    AStatus := GetStatus;
    AStatus.IsStarted := false;
    AStatus.IsSucceeded := True;
    AStatus.ResultText := 'Обновление выполнено успешно';
    AStatus.Text := Format('Обновление выполнено успешно. Время выполнения: %d c.', [SecondsBetween(Now, AStatus.StartTime)]);

    SetStatus(AStatus);
  except
    on E: Exception do
    begin
      AStatus := GetStatus;
      AStatus.IsStarted := false;
      AStatus.IsSucceeded := false;
      AStatus.ResultText := 'Ошибка: ' + E.Message;
      AStatus.Text := AStatus.ResultText;
      SetStatus(AStatus);

      Logger.Error('Ошибка: ' + E.Message);
      raise;
    end;
  end;
  Logger.Debug(Separator);
end;

procedure TFirmwareUpdater.CheckFSDebugVersion;
begin
  // Проверка что ФН отладочный
  Driver.Check(Driver.FNGetVersion);
  if Driver.FNSoftType <> 0 then
    raise Exception.Create('ФН должен быть отладочным');
end;

procedure TFirmwareUpdater.DoUpdateFirmware;
var
  EcrInfo: TECRInfo;
  Item: TUpdateItem;
  CashRegister: Currency;
  SearchParams: TSearchParams;
  Loader: TActionUpdateLoader;
  Firmware: TActionUpdateFirmware;
  Tables: TTableItems;
begin
  DownloadFiles;

  CashRegister := 0;
  SetStatusText('Обновление устройства');
  CheckStopped;
  Driver.Check(Connect);
  if not CheckEcrUpdateable then
    raise Exception.Create('Нельзя обновить ККМ');

  try
    EcrInfo := ReadEcrInfo;
    if EcrInfo.FirmwareValid then
    begin
      // Проверка режима ККМ
      if Driver.ECRMode <> MODE_CLOSED then
      begin
        raise Exception.Create('ККТ на связи. Однако в данном режиме перепрошивка невозможна.'#13#10 + 'Режим: ' + Driver.ECRModeDescription);
      end;
      // Проверка отправленных документов, если нужна перерегистрация
      if IsRefiscalizationNeeded then
      begin
        CheckDocSent;
      end;
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
      PrintUpdateStarted;
    end;
    // Format SD card
    FormatSDCard;
    // SetVComConnection;
    // Update loader
    for Item in FItems do
    begin
      if Item is TActionUpdateLoader then
      begin
        Loader := Item as TActionUpdateLoader;
        if ValidLoader(EcrInfo, Loader) then
        begin
          UploadFile(EcrInfo, FPath, Loader.FileName);
          DelayInMs(LoaderRebootDelay);
          WaitForDevice(EcrInfo.Serial, LoaderRebootTimeout);
          CheckLoaderUpdated(Loader.NewBootVer);
          EcrInfo.BootVer := Loader.NewBootVer;
        end;
      end;
    end;
    // Up
    Firmware := FindFirmware(EcrInfo);
    if Firmware <> nil then
    begin
      UploadFile(EcrInfo, FPath, Firmware.FileName);
      DelayInMs(FirmwareRebootDelay);

      // Ищем устройство на порту HardwarePort
      SearchParams.Serial := EcrInfo.Serial;
      SearchParams.Timeout := FirmwareRebootTimeout;
      SearchParams.Port := EcrInfo.HardwarePort;
      DiscoverDevice(SearchParams);

      // Проверка, обновилось ли ПО
      CheckFirmwareUpdated(Firmware);
      PrintUpdateCompleted;
      // Запись таблиц
      if FParams.SaveTables then
      begin
        WriteTablesFile(EcrInfo.Serial);
      end;
      // Запись дополнительных таблиц
      for Item in FItems do
      begin
        case Item.Action of
          ACTION_WRITE_TABLES:
            begin
              Tables := (Item as TActionWriteTables).Tables;
              WriteTables(Tables);
            end;
        end;
      end;

      // Перезагрузка для применения таблиц
      SetStatusText('Перезагрузка ККМ...');
      Driver.RebootKKT;
      Driver.Disconnect;
      SetStatusText('Перезагрузка ККМ: OK');
      // Читаем параметры подключения
      Driver.Check(Driver.LoadParams);
      DelayInMs(FirmwareRebootDelay);
      WaitForDevice(EcrInfo.Serial, FirmwareRebootTimeout);
    end;
    for Item in FItems do
    begin
      CheckStopped;
      case Item.Action of
        ACTION_INIT_FS:
          begin
            InitializeFS(Item as TActionInitFS);
          end;
        ACTION_FISCALIZE_FS:
          begin
            FiscalizeFS(Item as TActionFiscalizeFS);
          end;
        ACTION_WRITE_LICENSE:
          begin
            WriteLicense(FPath + TActionWriteLicense(Item).FileName, EcrInfo.Serial);
          end;
        ACTION_REFISCALIZE_FS:
          begin
            RefiscalizeFS(Item as TActionRefiscalizeFS, Tables);
          end;
      end;
    end;
    if EcrInfo.FirmwareValid then
    begin
      if FParams.RestoreCashRegister and (CashRegister <> 0) then
      begin
        PrintCashIn(CashRegister);
      end;
    end;
  finally
    Driver.Disconnect;
  end;
end;

function TFirmwareUpdater.IsRefiscalizationNeeded: Boolean;
var
  Item: TUpdateItem;
  Action: TActionRefiscalizeFS;
begin
  Result := false;
  for Item in FItems do
  begin
    if Item.Action = ACTION_REFISCALIZE_FS then
    begin
      Action := Item as TActionRefiscalizeFS;
      Result := Action.FfdVersion in [2, 4];
    end;
  end;
end;

procedure TFirmwareUpdater.UploadFile(const Ecr: TECRInfo; const Path, FileName: string);
begin
  Driver.Disconnect;
  case Ecr.HardwarePort of
    PORT_COM:
      XModemUploadFile(Ecr.ComNumber, Path + FileName);
    PORT_VCOM:
      DfuUploadFile(Path, FileName);
    PORT_TCP:
      raise Exception.Create('Прошивка по TCP не поддерживается');
  else
    raise Exception.CreateFmt('Прошивка не поддерживается, PortNumber=%d', [Ecr.SoftwarePort]);
  end;
end;

procedure TFirmwareUpdater.SetCurrentDateTime;
var
  CurrDate: TDateTime;
begin
  CurrDate := Date;
  Driver.Date := CurrDate;
  Driver.Check(Driver.SetDate);
  Driver.Check(Driver.ConfirmDate);
  Driver.Time := Time;
  Driver.Check(Driver.SetTime);
end;

procedure TFirmwareUpdater.RefiscalizeFS(Item: TActionRefiscalizeFS; const Tables: TTableItems);
var
  Inn: string;
  Text: string;
  OfdInn: string;
  Code: Integer;
  OfdParams: TOfdParams;
  IsFFDChanged: Boolean;
begin
  IsFFDChanged := Driver.ReadTableInt(17, 1, 17) <> Item.FfdVersion;
  if not IsFFDChanged then
  begin
    Logger.Debug('Версия ФФД не меняется. Перерегистрация не требуется');
    Exit;
  end;

  SetCurrentDateTime;
  // Перерегистрация ФФД
  case Item.FfdVersion of
    0:
      begin
        Logger.Debug('ФФД = 0, обновление не нужно');
        Exit;
      end;
    2:
      ; // ФФД 1.05
    4:
      ; // ФФД 1.2
  else
    raise Exception.CreateFmt('Неверное значение формата ФФД, %d', [Item.FfdVersion]);
  end;
  CheckDocSent;
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

  Text := Format('Перерегистрация ККТ на %s', [FFDToStr(Item.FfdVersion)]);
  SetStatusText(Text + '...');
  Driver.WriteTableInt(17, 1, 17, Item.FfdVersion);
  OfdInn := Driver.ReadTableStr(18, 1, 12);

  if GetOfdParams(Tables, OfdInn, OfdParams) then
  begin
    if OfdParams.ServerKM <> '' then
      Driver.WriteTableStr(19, 1, 5, OfdParams.ServerKM);
    if OfdParams.PortKM <> 0 then
      Driver.WriteTableInt(19, 1, 6, OfdParams.PortKM);
  end;
  // WorkModeEx
  Driver.WriteTableInt(Driver.FSTableNumber, 1, 21, Item.WorkModeEx);
  // RegReasonCodeEx
  Code := Item.RegReasonCodeEx;
  if IsFFDChanged then
  begin
    Code := Code or $200000;
  end;
  Driver.WriteTableInt(Driver.FSTableNumber, 1, 22, Code);

  Inn := Trim(Item.Inn);
  if Inn = '' then
    Inn := Trim(Driver.ReadTableStr(18, 1, 2));

  Driver.RegistrationReasonCode := Item.RegReasonCode;
  Driver.Inn := Inn;
  Driver.KKTRegistrationNumber := Trim(Driver.ReadTableStr(18, 1, 3));
  Driver.TaxType := Item.TaxType;
  Driver.WorkMode := Item.WorkMode;
  Driver.WorkModeEx := Item.WorkModeEx;
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
  if not WaitDocSent(FParams.DocSentTimeoutInSec) then
  begin
    Logger.Debug('Не дождались передачи документов.');
  end;
  // Ожидание отправки отчета о перефискализации
  Driver.Check(Driver.FNGetInfoExchangeStatus);
  Logger.Debug(Format('Неотправленных документов: %d', [Driver.MessageCount]));
end;

procedure TFirmwareUpdater.InitializeFS(Item: TActionInitFS);
begin
  // Инициализировать можно только тестовый ФН
  CheckFSDebugVersion;
  SetStatusText(Item.info + '...');

  SetCurrentDateTime;
  Driver.RequestType := 22;
  Driver.Check(Driver.FNResetState);
  SetStatusText(Item.info + ': OK');
end;

procedure TFirmwareUpdater.FiscalizeFS(Action: TActionFiscalizeFS);
var
  RegNumber: string;
begin
  SetStatusText(Action.info + '...');
  SetCurrentDateTime;
  // Фискализируем только тестовый ФН
  // Слишком ответственная операция для массового применения
  CheckFSDebugVersion;
  // Генерируем РНМ, если он не задан
  RegNumber := Action.RegNumber;
  if RegNumber = '' then
  begin
    Driver.Check(Driver.ReadSerialNumber);
    RegNumber := GenerateRNM('', Action.Inn, Driver.SerialNumber);
  end;
  Driver.WriteTableInt(17, 1, 17, Action.FfdVersion);
  // Фискализируем
  Driver.Timeout := 10000;
  Driver.Inn := Action.Inn;
  Driver.KKTRegistrationNumber := RegNumber;
  Driver.TaxType := Action.TaxType;
  Driver.WorkMode := Action.WorkMode;
  Driver.Check(Driver.FNBuildRegistrationReport);
  Driver.WaitForPrinting;
  Driver.Timeout := 1000;
  SetStatusText(Action.info + ': OK');
end;

procedure TFirmwareUpdater.SetVComConnection;
var
  Ecr: TECRInfo;
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

procedure TFirmwareUpdater.CheckLoaderUpdated(NewBootVer: Integer);
begin
  Driver.Check(Driver.ReadLoaderVersion);
  if Driver.LoaderVersion <> IntToStr(NewBootVer) then
  begin
    raise Exception.CreateFmt('Версия загрузчика отличается от целевой, %s <> %s', [Driver.LoaderVersion, IntToStr(NewBootVer)]);
  end;
end;

// Проверка типа подключения
function TFirmwareUpdater.GetHardwarePort(SoftwarePort: Integer): Integer;
var
  PppMode: Integer;
  RndisActive: Boolean;
begin
  Result := SoftwarePort;
  case SoftwarePort of
    PORT_COM:
      Result := PORT_COM;
    PORT_VCOM:
      Result := PORT_VCOM;
    PORT_TCP:
      begin
        PppMode := Driver.ReadTableInt(21, 1, 1); // Режим ppp
        RndisActive := Driver.ReadTableInt(21, 1, 9) = 1; // Режим Rndis
        if RndisActive then
        begin
          Result := PORT_VCOM;
        end;
        if not RndisActive then
        begin
          case PppMode of
            PPP_NONE:
              Result := PORT_TCP;
            PPP_COM_CLIENT, PPP_COM_SERVER:
              Result := PORT_COM;
            PPP_VCOM_CLIENT, PPP_VCOM_SERVER:
              Result := PORT_VCOM;
          end;
        end;
      end;
  end;
end;

function TFirmwareUpdater.ReadEcrInfo: TECRInfo;

  function GetSigningKey(const Version: string): Integer;
  begin
    Result := SigningKeyUnknown;
    if Pos('W', Version) > 0 then
      Result := SigningKeyTehnoWork;
    if Pos('T', Version) > 0 then
      Result := SigningKeyTehnoTest;
    if Pos('N', Version) > 0 then
      Result := SigningKeyShtrihWork;
    if Pos('P', Version) > 0 then
      Result := SigningKeyShtrihInter;
  end;

begin
  Logger.Debug('Проверка состояния ККМ');
  // Полный запрос состояния
  Result.FirmwareValid := True;
  Driver.GetECRStatus;
  if (Driver.ResultCode = 123) or (Driver.ResultCode = 56) then
  begin
    Logger.Debug('HardwareFailure detected');
    Result.FirmwareValid := false;
    Exit;
  end;
  Result.SoftwarePort := PORT_COM;
  Result.FirmwareVersion := '';
  Result.FirmwareBuild := -1;
  Result.FirmwareDate := -1;
  Result.SigningKey := SigningKeyUnknown;
  if Driver.ResultCode = 0 then
  begin
    Result.SoftwarePort := Driver.PortNumber;
    Result.HardwarePort := GetHardwarePort(Driver.PortNumber);
    Result.ComNumber := Driver.ComNumber;
    // Мы не знаем к какому порту подключен ФР физически
    if Result.SoftwarePort = PORT_TCP then
      Result.ComNumber := 0;

    Result.FirmwareVersion := Driver.ECRSoftVersion;
    Result.FirmwareBuild := Driver.ECRBuild;
    Result.FirmwareDate := Driver.ECRSoftDate;
    Result.Serial := ReadSerialNumber;
    Result.SigningKey := GetSigningKey(Driver.FMSoftVersion);
    // Читаем версию загрузчика
    Result.BootVer := -1;
    if Driver.ReadLoaderVersion = 0 then
    begin
      Result.BootVer := StrToInt(Driver.LoaderVersion);
    end else
    begin
      raise Exception.Create('ККТ на связи. Однако не удалось прочитать версию загрузчика, прошивка невозможна.');
    end;
  end;
end;

procedure TFirmwareUpdater.LoadParameters;
begin
  try
    DownloadFiles;
  except
    on E: Exception do
    begin
      SetStatusText('Ошибка: ' + E.Message);
    end;
  end;
end;

procedure TFirmwareUpdater.LoadArchive(const FileName: string);
begin
  FPath := GetEnvironmentVariable('TEMP') + '\' + Copy(TPath.GetRandomFileName, 1, 8) + '\';
  ForceDirectories(FPath);
  UnzipArhive(FPath, FileName);
  LoadFiles(FPath);
end;

procedure TFirmwareUpdater.DeleteFiles;
begin
  TDirectory.Delete(FPath, True);
end;

procedure TFirmwareUpdater.DownloadFiles;
var
  FileName: string;
begin
  if FFilesDownloaded then
    Exit;

  FPath := GetEnvironmentVariable('TEMP') + '\' + Copy(TPath.GetRandomFileName, 1, 8) + '\';
  ForceDirectories(FPath);
  // Если обновления есть - скачиваем архив
  FileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'archive.zip';
  if not FileExists(FileName) then
    raise Exception.CreateFmt('Файл архива не найден, %s', [FileName]);

  UnzipArhive(FPath, FileName);
  LoadFiles(FPath);

  if FParams.ArchiveURL <> '' then
  begin
    TDirectory.Delete(FPath, True);
    ForceDirectories(FPath);

    if DownloadArchive(FParams.ArchiveURL) then
    begin
      UnzipArhive(Path, Path + 'arhive.zip');
      LoadFiles(FPath);
    end;
  end;
  FFilesDownloaded := True;
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
  CheckFilesExists(InPath);
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

function TFirmwareUpdater.DownloadArchive(const ArchiveURL: string): Boolean;
begin
  Result := false;
  try
    Result := DownloadFile(ArchiveURL, FPath + 'arhive.zip');
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
  Stream := TResourceStream.Create(HInstance, 'arhive', RT_RCDATA);
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
    for i := 0 to FileNames.Count - 1 do
    begin
      FileName := FileNames[i];
      FileExt := LowerCase(ExtractFileExt(FileName));
      if (FileExt = '.bin') or (FileExt = '.exe') or (FileExt = '.json') then
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
  models: array[0..11] of string = ('0006', '0004', '0007', '0008', '0022', '0002', '0009', '0001', '0041', '0051', '0044', '0048');
var
  i: Integer;
  Model: string;
begin
  Model := Copy(Serial, 7, 4);
  for i := Low(models) to High(models) do
  begin
    Result := Model = models[i];
    if Result then
      Break;
  end;
end;

function TFirmwareUpdater.ReadSerialNumber: string;
begin
  Driver.Check(Driver.ReadSerialNumber);
  Result := Driver.SerialNumber;
end;

procedure TFirmwareUpdater.CheckFirmwareUpdated(const Item: TActionUpdateFirmware);
begin
  Driver.Check(Driver.GetECRStatus);
  begin
    if Driver.ECRSoftVersion <> Item.Version then
      raise Exception.CreateFmt('Версия ПО ФР отличается, %s <> %s', [Driver.ECRSoftVersion, Item.Version]);

    if Driver.ECRBuild <> Item.Build then
      raise Exception.CreateFmt('Сборка ПО ФР отличается, %d <> %d', [Driver.ECRBuild, Item.Build]);
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

  (*
    SetStatusText('Проверка драйвера DFU');
    if not CheckDFU then
    raise Exception.Create('Не установлен драйвер DFU, прошивка невозможна.');
  *)

  // Переход в DFU
  SetStatusText('Переход в режим DFU');
  if Driver.SetDFUMode <> 0 then
  begin
    raise Exception.CreateFmt('Ошибка перехода в режим DFU. %s', [Driver.ResultCodeDescription]);
  end;
  Driver.Disconnect;
  // Ждём переход в DFU
  WaitForDFUDevice(DFUDelayTime);
  // Грузим файл
  SetStatusText('Запись файла ' + FileName);
  ResultCode := SystemUtils.ExecuteProcess(DfuUtilFile, ' -D ' + FirmwareFile, StdOut);
  if ResultCode <> 0 then
    raise Exception.CreateFmt('Ошибка загрузки прошивки. %s, %s', [SysErrorMessage(GetLastError), StdOut]);

  if Pos('Done!', StdOut) = 0 then
  begin
    raise Exception('Не удалось загрузить прошивку в ККТ.'#13#10 + StdOut);
  end;
  SetStatusText('DFU, запись успешно выполнена');
  Logger.Debug(Format('DFU, запись выполнена за %d мс.', [MilliSecondsBetween(Now, StartTime)]));
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
    raise Exception.CreateFmt('Ошибка выполнения. %s, %s', [SysErrorMessage(GetLastError), Result]);
end;

function TFirmwareUpdater.WaitForDFUDevice(Timeout: Cardinal): Boolean;
var
  StartTime: Cardinal;
begin
  StartTime := GetTickCount;
  while True do
  begin
    Result := IsDFUDevicePresent;
    if Result then
      Exit;

    if (GetTickCount - StartTime) >= Timeout then
      Break;
    Sleep(100); // Проверяем каждые 100 мс
  end;
end;

function TFirmwareUpdater.IsDFUDevicePresent: Boolean;
begin
  Result := Pos('DFU', RunDfuUtil(FPath, '-l')) <> 0;
end;

procedure TFirmwareUpdater.XModemUploadFile(ComNumber: Integer; const FileName: string);
var
  Modem: TXModem;
  StartTime: TDateTime;
begin
  StartTime := Now;
  SetStatusText('XModem, запись файла ' + FileName);
  Modem := TXModem.Create;
  try
    Modem.PortNumber := ComNumber;
    Modem.BaudRate := 115200;
    Modem.Timeout := 3000;
    Modem.LogOn := false;
    Modem.OnCancel := XModemCancelProc;
    // Modem.OnPercent := OnPercent;
    Modem.SendFile(FileName);
  finally
    Modem.Free;
  end;
  SetStatusText('XModem, запись выполнена успешно');
  Logger.Debug(Format('XModem, запись выполнена за %d мс.', [MilliSecondsBetween(Now, StartTime)]));
end;

function TFirmwareUpdater.XModemCancelProc: Boolean;
begin
  Result := FStopped;
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
  for i := 0 to Length(Tables) - 1 do
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

procedure TFirmwareUpdater.WaitForDevice(const Serial: string; TimeoutInMs: Integer);
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
      Logger.Debug(Format('Устройство найдено за %d мс', [Integer(GetTickCount) - TickCount]));
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
      Logger.Debug(Format('ReadSerialNumber: %d, %s', [Driver.ResultCode, Driver.ResultCodeDescription]));
    end;

    if Result then
    begin
      Result := (Serial = Driver.SerialNumber) or (Serial = '');
    end;
  end;
end;

function TFirmwareUpdater.Connect: Integer;
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
    if Result <> 0 then
      Exit;

    Result := Driver.GetShortECRStatus;
    if Result <> 0 then
      Exit;
  end;
  if Result = 0 then
  begin
    if Driver.ECRMode = MODE_TECH then
    begin
      SetCurrentDateTime;
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
      Logger.Debug(Format('Устройство найдено за %d мс', [Integer(GetTickCount) - TickCount]));
      Logger.Debug('Проверка состояния ККМ');
      Exit;
    end;

    Sleep(100);
  until Integer(GetTickCount) > TickCount + Params.Timeout;

  raise Exception.CreateFmt('Устройство %s не найдено', [Params.Serial]);
end;

/// ///////////////////////////////////////////////////////
// Для подключения COM - 4800
// Для подключения VCOM - любая скорость
//

function TFirmwareUpdater.FindDeviceLocal(const Params: TSearchParams): Boolean;
var
  i: Integer;
  Port: TSearchPort;
  Search: TDeviceSearch;
begin
  Result := false;
  Search := TDeviceSearch.Create;
  try
    Search.DoTechReset := True;
    Search.ShortSearch := false;

    Search.Start;
    Search.Wait;
    for i := 0 to Search.Ports.Count - 1 do
    begin
      Port := Search.Ports[i];
      if ((Port.SerialNumber = Params.Serial) or (Params.Serial = '')) and (Params.Port = Port.DevicePort) then
      begin
        // Logger.Debug('Устройство найдено');
        Driver.ConnectionType := CT_LOCAL;
        Driver.ProtocolType := 0; // Standard
        Driver.ComNumber := Port.PortNumber;
        Driver.BaudRate := Port.BaudRate;
        Driver.Timeout := 1000;
        Driver.Check(Driver.Connect);
        if Driver.BaudRate < BAUD_RATE_CODE_115200 then
        begin
          // Logger.Debug('Установка скорости ККМ 115200');
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

function TFirmwareUpdater.FindDeviceLocal2(const Params: TSearchParams): Boolean;
var
  RC: Integer;
begin
  Driver.ConnectionType := CT_LOCAL;
  RC := Driver.FindDevice;
  if (RC = $74) or (RC = $79) or (RC = $78) then
  begin
    RC := Driver.ResetSettings;
    if RC = 147 then
      Driver.Check(Driver.ResetSettings)
    else
      Driver.Check(RC);

    SetCurrentDateTime;
  end;
  Driver.Check(RC);
  Driver.Check(Driver.ReadSerialNumber);
  Result := Driver.SerialNumber = Params.Serial;
  if not Result then
    Exit;

  Driver.Check(Driver.GetECRStatus);
  Result := Driver.PortNumber = Params.Port;
end;

procedure TFirmwareUpdater.SetBaudRate(BaudRate: Integer);
begin
  // Logger.Debug('Set baudrate ' + IntToStr(ABaudrate));
  // Driver.PortNumber := 0;
  Driver.Timeout := 10000;
  Driver.BaudRate := BaudRate;
  Driver.Check(Driver.SetExchangeParam);
  Driver.BaudRate := BaudRate;
end;

procedure TFirmwareUpdater.SetStatusText(const Text: string);
var
  AStatus: TUpdateStatus;
begin
  Logger.Debug(Text);

  AStatus := GetStatus;
  AStatus.Text := Text;
  SetStatus(AStatus);
end;

function TFirmwareUpdater.GetStatus: TUpdateStatus;
begin
  FLock.Enter;
  try
    Result := FStatus;
  finally
    FLock.Leave;
  end;
end;

procedure TFirmwareUpdater.SetStatus(const Value: TUpdateStatus);
begin
  FLock.Enter;
  try
    FStatus := Value;
  finally
    FLock.Leave;
  end;
end;

procedure TFirmwareUpdater.CheckFilesExists(const Path: string);
var
  Item: TUpdateItem;
begin
  for Item in Items do
  begin
    Item.CheckFileExists(Path)
  end;
end;

function TFirmwareUpdater.CheckEcrUpdateable: Boolean;
begin
  Result := EcrUpdateable(ReadSerialNumber);
end;

function TFirmwareUpdater.ReadLicense(const FileName: string; const Serial: string; var License: TEcrLicense): Boolean;
var
  i: Integer;
  Strings: TStringList;
begin
  Result := false;
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
    raise Exception.CreateFmt('Ошибка записи лицензий, %d, %s', [Driver.ResultCode, Driver.ResultCodeDescription]);
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
    if Modelid = 19 then
      Result := Driver.ReadTableInt(10, 1, 4)
    else if IsModelType2(Modelid) then
      Result := Driver.ReadTableInt(10, 1, 29)
    else
      Result := Driver.ReadTableInt(17, 1, 17);
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
    if Result then
    begin
      SetStatusText('Все документы переданы');
      Break;
    end;

    if Integer(GetTickCount - TickCount) > (1000 * TimeoutInSec) then
    begin
      SetStatusText('Таймаут отправки сообщений в ОФД');
      Break;
    end;
    Sleep(500);
  end;
end;

procedure TFirmwareUpdater.CheckDocSent;
var
  IsTimeout: Boolean;
begin
  SetStatusText('Проверка отправленных документов...');
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
    raise Exception.Create('Перерегистрация на ФФД 1.2 не может быть произведена. Есть неотправленные в ОФД документы');
  SetStatusText('Проверка отправленных документов: OK');
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

// Logger.Debug('Обновление параметров сервера КМ для ИНН ОФД ' + OfdInn);
// Logger.Debug('ServerKM: ' + ServerKM);
// Logger.Debug('PortKM: ' + PortKM.ToString);

function TFirmwareUpdater.FindOfdParams(const OfdInn: string; var OfdParams: TOfdParams): Boolean;
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

/// ////////////////////////////////////////////////////////////////////////////
// 19,1,5,64,1,0,3000,'Сервер км','192.168.144.138'
// 19,1,6,2,0,1,65535,'Порт км','8789'

function TFirmwareUpdater.GetOfdParams(const Tables: TTableItems; const OfdInn: string; var OfdParams: TOfdParams): Boolean;
var
  Ofd: TOfdParams;
  Item: TTableItem;
begin
  Ofd.Inn := OfdInn;
  Ofd.ServerKM := '';
  Ofd.PortKM := 0;
  for Item in Tables do
  begin
    if (Item.Table = 19) and (Item.Row = 1) and (Item.Field = 5) then
    begin
      Ofd.ServerKM := Item.StrValue;
    end;
    if (Item.Table = 19) and (Item.Row = 1) and (Item.Field = 6) then
    begin
      Ofd.PortKM := Item.IntValue;
    end;
  end;
  Result := (Ofd.ServerKM <> '') and (Ofd.PortKM <> 0);
  if Result then
  begin
    OfdParams := Ofd;
  end else
  begin
    Result := FindOfdParams(OfdInn, OfdParams);
  end;
end;

function TFirmwareUpdater.ValidLoader(const Ecr: TECRInfo; const Item: TActionUpdateLoader): Boolean;
begin
  // Если нужно переписывать для тестирования
  Result := Item.Force and (Item.NewBootVer = Ecr.BootVer);
  if Result then
    Exit;

  Result := Item.CurrBootVer = Ecr.BootVer;
  if Result then
    Exit;

  if Item.CurrBootVer = 0 then
  begin
    Result := (Ecr.BootVer <> -1) and (Item.NewBootVer > Ecr.BootVer) and (Ecr.BootVer > 129);
  end;
end;

function TFirmwareUpdater.ValidFirmware(const Ecr: TECRInfo; const Item: TActionUpdateFirmware): Boolean;
begin
  Result := false;
  if Ecr.BootVer >= Item.CurrBootVer then
  begin
    Result := True;
    if (Item.Build = Ecr.FirmwareBuild) and (Item.Version = Ecr.FirmwareVersion) then
    begin
      if not Item.Force then
        Result := false;
    end;
  end;
end;

function TFirmwareUpdater.FindLastLoader(Ecr: TECRInfo): TActionUpdateLoader;
var
  Item: TUpdateItem;
begin
  Result := nil;
  for Item in FItems do
  begin
    if Item is TActionUpdateLoader then
    begin
      if ValidLoader(Ecr, Item as TActionUpdateLoader) then
      begin
        Result := Item as TActionUpdateLoader;
        Ecr.BootVer := Result.NewBootVer;
      end;
    end;
  end;
end;

function TFirmwareUpdater.FindFirmware(Ecr: TECRInfo): TActionUpdateFirmware;
var
  Item: TUpdateItem;
begin
  for Item in FItems do
  begin
    if Item is TActionUpdateFirmware then
    begin
      Result := Item as TActionUpdateFirmware;
      if ValidFirmware(Ecr, Result) then
        Exit;
    end;
  end;
  Result := nil;
end;

procedure TFirmwareUpdater.ShowProperties;
begin
  Driver.ShowProperties;
  Driver.SaveParams;
  Driver.Disconnect;
end;

end.

