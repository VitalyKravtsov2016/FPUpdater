unit DriverTypes;

interface

uses
  // VCL
  Windows, SysUtils, ShlObj,
  // This
  GlobalConst, LogFile;

const
  BARCODE_CODE128A = 0;
  BARCODE_CODE128B = 1;
  BARCODE_CODE128C = 2;
  BARCODE_QRCODE = 3; // 2D
  BARCODE_CODE128AUTO = 4;
  BARCODE_CODE39 = 5;
  BARCODE_CODE93 = 6;
  BARCODE_ITF14 = 7;
  BARCODE_UPCA = 8;
  BARCODE_UPCE = 9;
  BARCODE_PDF417 = 10; //2D
  BARCODE_AZTEC = 11; //2D
  BARCODE_2OF5_INTERLEAVED = 12;

  /////////////////////////////////////////////////////////////////////////////
  // ModelID constants

  MODEL_SHTRIH_MINI_FRK_KAZ = 12;
  MODEL_YARUS_M2100K = 20;

  /////////////////////////////////////////////////////////////////////////////
  // SwapBytesMode

  SwapBytesModeSwap = 0;
  SwapBytesModeNoSwap = 1;
  SwapBytesModeProp = 2;
  SwapBytesModeModel = 3;

resourcestring
  SDriverName = 'Драйвер ККТ';
  SServerVersionUnknown = 'недоступна';
  SDefaultDeviceName = 'Устройство №';

type
  { TECRDateTime }

  TECRDateTime = record
    Day: Byte;
    Month: Byte;
    Year: Byte;
    Hour: Byte;
    Min: Byte;
    Sec: Byte;
  end;

  { TECRDate }

  TECRDate = record
    Day: Byte;
    Month: Byte;
    Year: Byte;
  end;

  { TECRTime }

  TECRTime = record
    Hour: Byte;
    Min: Byte;
    Sec: Byte;
  end;

  { TLicInfoRec }

  TLicInfoRec = record
    ResultCode: Integer;        // Результат
    ResultDesc: string;         // Описание результата
    CashControl: Boolean;       // Разрешено использование CashControl
    RemoteLaunch: Boolean;      // Разрешен удаленный запуск
    KeyCount: Integer;          // Количество ключей
    LicCount: Integer;          // Количество лицензий
  end;

  { TDeviceModel }

  TDeviceModel = (dmUnknown, 					  // Неизвестная модель
    dmShtrihFRF3,				  // ШТРИХ-ФР-Ф (версия 3)
    dmShtrihFRF4,				  // ШТРИХ-ФР-Ф (версия 4)
    dmShtrihFRFKaz,   	  // ШТРИХ-ФР-Ф (Казахстан)
    dmElvesMiniFRF,			  // ЭЛВЕС-МИНИ-ФР-Ф
    dmFelixRF, 					  // ФЕЛИКС-Р Ф
    dmShtrihFRK,				  // ШТРИХ-ФР-К
    dmShtrih950K,				  // ШТРИХ-950К версия 1
    dmShtrih950Kv2,			  // Штрих950K версия 2
    dmElvesFRK, 				  // ЭЛВЕС-ФР-К
    dmShtrihMiniFRK, 		  // ШТРИХ-МИНИ-ФР-К
    dmShtrihMiniFRK2, 	  // ШТРИХ-МИНИ-ФР-К 2
    dmShtrihFRFBel, 		  // ШТРИХ-ФР-Ф (Белоруссия)
    dmShtrihComboFRKv1,   // ШТРИХ-КОМБО-ФР-К версии 1
    dmShtrihComboFRKv2,   // ШТРИХ-КОМБО-ФР-К версии 2
    dmShtrihPOSF,				  // Фискальный блок Штрих-POS-Ф
    dmShtrih500,					// ШТРИХ-500
    dmShtrihMFRK,         // ШТРИХ-М-ФР-К
    dmShtrihLightFRK,    // ШТРИХ-LIGHT-ФР-К
    dmYARUS01K,           // ЯРУС-01К
    dmYARUS02K,           // ЯРУС-02К
    dmYARUSM2100K,          // ЯРУС М2100К
    dmShtrihMobilePTK,  //"ШТРИХ-MOBILE-ПТК"
    dmYarusTK,  //- "YARUS-ТК" | "АСПД YARUS C21"
    dmRetail01K,  //- "Retail-01К"
    dmRR02K,  //- "RR-02К"
    dmRR01K,  //- "RR-01К"
    dmRR04K,  //- "RR-04К"
    dmRR03K  //- "RR-03К"
);

  { TInt64Rec }

  TInt64Rec = record
    Value: Int64;
    IsEmpty: Boolean;
  end;

  { TFRFieldRec }

  TFRFieldRec = record
    FieldSize: Byte;
    FieldName: string;
    IsString: Boolean;
    StrValue: string;
    IntValue: Integer;
    MinValue: Integer;
    MaxValue: Integer;
  end;

  TBanknotes = array[0..23] of Integer;

const
  /////////////////////////////////////////////////////////////////////////////
  // SaveSettingsType constants

  stRegLocalMachine = 0;
  stRegCurrentUser = 1;

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
  REGSTR_KEY_DRIVER = '\SOFTWARE\TorgBalance\DrvFR';
  REGSTR_KEY_PARAMS = REGSTR_KEY_DRIVER + '\Param';
  REGSTR_KEY_TABLEDEFS = REGSTR_KEY_DRIVER + '\TableDefs';
  REGSTR_KEY_DEVICES = REGSTR_KEY_DRIVER + '\Logical Devices';
  REGSTR_KEY_COMMANDS = REGSTR_KEY_DRIVER + '\Timeouts';
  REGSTR_KEY_PARAMS1C = REGSTR_KEY_DRIVER + '\Params1C';
  REGSTR_KEY_TABLEPARAMS = REGSTR_KEY_DRIVER + '\TableParams';
  REGSTR_KEY_PLUGINS = REGSTR_KEY_DRIVER + '\Plugins';
  // параметры устройства
  REGSTR_VAL_TIMEOUT = 'Timeout';
  REGSTR_VAL_PLAINTRANSFERMODE = 'PlainTransferMode';
  REGSTR_VAL_TLSMODE = 'TLSMode';
  REGSTR_VAL_CONNECTIONTIMEOUT = 'ConnectionTimeout';
  REGSTR_VAL_TCPCONNECTIONTIMEOUT = 'TCPConnectionTimeout';
  REGSTR_VAL_SYNCTIMEOUT = 'SyncTimeout';
  REGSTR_VAL_BAUDRATE = 'BaudRate';
  REGSTR_VAL_COMNUMBER = 'ComNumber';
  REGSTR_VAL_PROTOCOLTYPE = 'ProtocolType';
  REGSTR_VAL_CURRENTDEVICE = 'CurrentDevice';
  REGSTR_VAL_COMPUTERNAME = 'ComputerName';
  REGSTR_VAL_TCPPORT = 'TCPPort';
  REGSTR_VAL_IPADDRESS = 'IPAddress';
  REGSTR_VAL_USEIPADDRESS = 'UseIPAddress';
  REGSTR_VAL_CONNECTIONTYPE = 'ConnectionType';
  REGSTR_VAL_ESCAPEIP = 'EscapeIP';
  REGSTR_VAL_ESCAPEPORT = 'EscapePort';
  REGSTR_VAL_ESCAPETIMEOUT = 'EscapeTimeout';
  REGSTR_VAL_RECOVERERROR165 = 'RecoverError165';
  REGSTR_VAL_SYSADMINPASSWORD = 'SysAdminPassword';

  //License Trial TimeStamp
  REGSTR_VAL_TRIALSTAMP = 'LCTStamp';

  // описание таблицы
  REGSTR_VAL_ROWCOUNT = 'RowCount';
  REGSTR_VAL_TABLENAME = 'TableName';
  REGSTR_VAL_FIELDCOUNT = 'FieldCount';
  REGSTR_VAL_TABLENUMBER = 'TableNumber';
  // описание полей таблицы
  REGSTR_VAL_FIELDNAME = 'Name';
  REGSTR_VAL_FIELDSIZE = 'Size';
  REGSTR_VAL_FIELDTYPE = 'Type';
  REGSTR_VAL_FIELDNUMBER = 'Number';
  REGSTR_VAL_FIELDMINVALUE = 'MinValue';
  REGSTR_VAL_FIELDMAXVALUE = 'MaxValue';
  // свойства логиского устройства
  REGSTR_VAL_DEVICENAME = 'Name';
  REGSTR_VAL_DEVICENUMBER = 'Number';
  REGSTR_VAL_DEVICETIMEOUT = 'Timeout';
  REGSTR_VAL_DEVICEBAUDRATE = 'Baudrate';
  REGSTR_VAL_DEVICECOMNUMBER = 'ComNumber';
  REGSTR_VAL_LOCKTIMEOUT = 'LockTimeout';
  // свойства команды
  REGSTR_VAL_COMMAND_CODE = 'Code';
  REGSTR_VAL_COMMAND_NAME = 'Name';
  REGSTR_VAL_COMMAND_TIMEOUT = 'Timeout';
  REGSTR_VAL_COMMAND_DEFTIMEOUT = 'DefTimeout';
  // Положение и размер окна таблицы
  REGSTR_VAL_TABLE_LEFT = 'Left';
  REGSTR_VAL_TABLE_TOP = 'Top';
  REGSTR_VAL_TABLE_WIDTH = 'Width';
  REGSTR_VAL_TABLE_HEIGHT = 'Heigth';
  DefTimeout = 3000;              // Таймаут по умолчанию
  DefConnectionTimeout = 0;               // Таймаут подключения по умолчанию
  DefTCPConnectionTimeout = 10000;
  DefBaudRate = 1;                // Скорость по умолчанию 4800
  DefComNumber = 1;                // Номер COM порта
  DefTCPPort = 7778;              // Порт сервера печати по умолчанию
  DefIPAddress = '192.168.137.111';      // IP адрес сервера печати по умолчанию
  DefConnectionType = CT_LOCAL;         // Тип подключения по умолчанию
  DefEscapeIP = '127.0.0.1';      // IP адрес Escape по умолчанию
  DefEscapePort = 1000;             // UDP порт Escape по умолчанию
  DefEscapeTimeout = 1000;             // Таймаут Escape по умолчанию, мс
  DefSysAdminPassword = 30;
  DefSwapBytesMode = SwapBytesModeModel;
  QuantityFactor: Integer = 1000;
  BoolToInt: array[Boolean] of Byte = (0, 1);
  BoolToStr: array[Boolean] of string = ('0', '1');
  MODE_CHECK_OPENED = 8;

  //
  MaxRepeatCount = 3;

  /////////////////////////////////////////////////////////////////////////////
  // PrintBarcodeText

  PrintBarcodeTextNone = 0;
  PrintBarcodeTextBelow = 1;
  PrintBarcodeTextAbove = 2;
  PrintBarcodeTextBoth = 3;
  DefMaxAnsCount = 5;
  DefCommandRetryCount = 3;
  DefMaxCmdCount = 5;
  DefLogMaxFileSize = 10;
  DefLogMaxFileCount = 10;
  DefStorageType = stRegCurrentUser;

const
  // в драйвере использованы ошибки ККМ
  E_ECR_FMOVERFLOW = $14; // Область сменных итогов ФП переполнена
  E_ECR_PASSWORD = $4F; // Неверный пароль

  // коды ошибок драйвера
  E_NOERROR = 0;
  E_NOHARDWARE = -1;  // Нет связи
  E_NOPORT = -2;  // СOM порт недоступен
  E_PORTBUSY = -3;  // СOM порт занят другим приложением
  E_ANSWERLENGTH = -7;  // Неверная длина ответа
  E_UNKNOWN = -8;
  E_INVALIDPARAM = -9;  // Параметр вне диапазона
  E_NOTSUPPORTED = -12; // Не поддерживается в данной версии драйвера
  E_NOTLOADED = -16; // Не удалось подключиться к серверу
  E_PORTLOCKED = -18; // Порт блокирован
  E_REMOTECONNECTION = -19; // Удаленное подключение запрещено

  E_USERBREAK = -30; // Прервано пользователем
  E_MP_SALEERROR = -31; // Оплата выполнена успешно.
  E_MP_CHECKOPENED = -32; // Чек открыт. Оплата невозможна
  E_MP_PAYERROR = -33;
  E_NOPAPER = -34; // Нет бумаги
  E_RESET = -35; // Не удалось сбросить ККМ
  E_MODELNOTFOUND = -36; // Не найдено описание модели
  E_MODELSFILEERROR = -37; // Не найден или поврежден файл "Models.xml"
  E_SERVERVERSIONERROR = -38; // Несовместимая версия сервера ФР
  E_UNKNOWNTAG = -39;           // Неизвестный тег
  E_FILENOTFOUND = -40; // Файл не найден
  E_DOCUMENTNOTFOUND = -41; // Документ не найден
  E_INCORRECTTLVLENGTH = -42; // Длина данных TLV превышает допустимую

  E_KMSRV_GENERIC_ERROR = -43; // Сервер ЭКМ, общая ошибка
  E_KMSRV_NOT_IMPLEMENTED = -44; // Сервер ЭКМ, не реализовано
  E_KMSRV_UNSUPPORTED_TYPE = -45; // Сервер ЭКМ, неподдерживаемый тип
  E_KMSRV_UNSUPPORTED_VERSION = -46; // Сервер ЭКМ, неподдерживаемая версия
  E_SALE_NOT_ENABLED = -47; // Продажа запрещена
  E_FW_UPDATE_STARTED = -48; // Идет обновление прошивки
  E_DFU_MODE_NOT_SUPPORTED = -49; // Режим DFU не поддерживается данной моделью

  E_VMCSCANNER_ERROR = -50;
  E_DATE_TIME_DIFFER_MORE_THAN_24H = -51;

  E_DRV_CRPT_CHECK_WRONG_PRICE = -107;  // Продажа товара по цене ниже или выше максимальной розничнойцены
  E_DRV_CRPT_CHECK_EXPIRED = -106;  // Срок годности того
  E_DRV_CRPT_CHECK_NOT_REALIZABLE = -105;  // Продажа товара при отсутствии в информационной системе мониторинга сведений о его вводе в оборот (за исключением случаев, когда потребительская или групповая упаковка относится квременно не прослеживаемой)
  E_DRV_CRPT_CHECK_BLOCKED = -104; //Продажа товара, заблокированного или приостановленного для реализации по решению органов власти, принятых в пределах установленных полномочий (по статусу кода идентификации в информационной системемониторинга)
  E_DRV_CRPT_CHECK_SOLD = -103; //уже продано
  E_DRV_CRPT_CHECK_NOT_VERIFIED = -102; // Продажа товара с кодом проверки, который не соответствует характеристикам, в том числе структуре и формату,установленным правиламимаркировки отдельных видовтоваров, в отношении которыхвведена обязательная маркировка, и(или) требованиям к егоформированию и (или) нанесению,установленным указаннымиправилами маркировки
  E_DRV_CRPT_CHECK_NOT_FOUND = -101; //  Продажа товара, сведения о маркировке средствами идентификации которого отсутствуют в информационной системе мониторинга
  E_DRV_CRPT_CHECK_COMMON = -100; // общая ошибка проверки в ЦРПТ

  E_DRV_DATA_TOO_LONG     = -200;  //Данные слишком длинные

  S_DRV_CRPT_CHECK_WRONG_PRICE = 'Продажа товара по цене ниже или выше максимальной розничной цены';
  S_DRV_CRPT_CHECK_EXPIRED = 'Срок годности истек';
  S_DRV_CRPT_CHECK_NOT_REALIZABLE = 'Продажа товара при отсутствии в информационной системе мониторинга сведений о его вводе в оборот (за исключением случаев, когда потребительская или групповая упаковка относится квременно не прослеживаемой)';
  S_DRV_CRPT_CHECK_BLOCKED = 'Продажа товара, заблокированного или приостановленного для реализации по решению органов власти, принятых в пределах установленных полномочий (по статусу кода идентификации в информационной системемониторинга)';
  S_DRV_CRPT_CHECK_SOLD = 'Товар уже продан';
  S_DRV_CRPT_CHECK_NOT_VERIFIED = 'Продажа товара с кодом проверки, который не соответствует характеристикам, в том числе структуре и формату, установленным правилами маркировки отдельных видов товаров'; //, в отношении которыхвведена обязательная маркировка, и(или) требованиям к егоформированию и (или) нанесению, установленным указанными правилами маркировки';
  S_DRV_CRPT_CHECK_NOT_FOUND = 'Продажа товара, сведения о маркировке средствами идентификации которого отсутствуют в информационной системе мониторинга';
  S_DRV_CRPT_CHECK_COMMON = 'Общая ошибка проверки в ЦРПТ';

  CBR_230400 = 230400;
  CBR_460800 = 460800;
  CBR_921600 = 921600;
  FWUPDATE_SUCCESS = 0;
  FWUPDATE_RUNNING = 1;
  FWUPDATE_ERROR = 2;

resourcestring
  SIncorrectTLVLength = 'Длина данных TLV структуры превышает допустимую';
  SDriverNoErrors = 'Ошибок нет';
  SDriverNoHardware = 'Нет связи';
  SDriverNoPort = 'СOM порт недоступен';
  SDriverPortBusy = 'СOM порт занят другим приложением';
  SDriverServerError = 'Ошибка подключения';
  SDriverAbortedByUser = 'Прервано пользователем';
  SDriverAnswerLength = 'Неверная длина ответа';
  SDriverNotSupported = 'Не поддерживается в данной версии драйвера';
  SDriverUnknown = 'Неизвестная ошибка';
  SDriverRemoteConnection = 'Удаленное подключение запрещено';
  SDriverMPSaleError = 'Оплата выполнена успешно';
  SDriverReceiptOpened = 'Чек открыт. Оплата невозможна';
  SDriverReset = 'Не удалось сбросить ККМ';
  SDriverPortLocked = 'Порт блокирован';
  SDriverModelNotFound = 'Не найдено описание модели';
  SDriverModelsMissing = 'Не найден или поврежден файл "Models.xml"';
  SDriverServerVersionError = 'Несовместимая версия сервера ФР';
  SUnknownTag = 'Неизвестный тег';
  SFileNotFound = 'Файл не найден';
  SDocumentNotFound = 'Документ не найден';
  SFwupdateStarted = 'Идет обновление прошивки ККТ. Не отключайте питание и не закрывайте приложение';
  SDfuModeNotSupported = 'Режим DFU не поддерживается моделью';
  S_DATE_TIME_DIFFER_MORE_THAN_24H = 'Дата и время в ПК и ККТ расходятся более, чем на сутки';

resourcestring
  SParamsReadError = 'Ошибка чтения параметров: ';
  SParamsWriteError = 'Ошибка записи параметров: ';

function GetDllFileName: string;

function ECRDateTimeToDateTime(const Value: TECRDateTime): TDateTime;

function GetDefaultLogFileName: string;

function GetUserShtrihPath: string;

function GetUserShtrihPath_def: string;

function GetBackupTablesPath: string;

function GetRegRootKey(RootKeyType: Integer): HKEY;

function GetCommonShtrihPath: string;

function DRV_SUCCESS(Value: Integer): Boolean;

function IsModelType2(Value: Integer): Boolean;

implementation


const
  CompanyName = 'TorgBalance';

function DRV_SUCCESS(Value: Integer): Boolean;
begin
  Result := Value = E_NOERROR;
end;

function GetRegRootKey(RootKeyType: Integer): HKEY;
begin
  if RootKeyType = stRegLocalMachine then
    Result := HKEY_LOCAL_MACHINE
  else
    Result := HKEY_CURRENT_USER;
end;

function ECRDateTimeToDateTime(const Value: TECRDateTime): TDateTime;
begin
  try
    Result := EncodeDate(Value.Year + 2000, Value.Month, Value.Day) + EncodeTime(Value.Hour, Value.Min, Value.Sec, 0);
  except
    Result := 0;
  end;
end;

function GetDllFileName: string;
var
  Buffer: array[0..261] of Char;
begin
  SetString(Result, Buffer, Windows.GetModuleFileName(HInstance, Buffer, SizeOf(Buffer)));
end;

function GetUserShtrihPath: string;
begin
  SetLength(Result, MAX_PATH);
  ShlObj.SHGetSpecialFolderPath(0, @Result[1], CSIDL_APPDATA, false);
  SetLength(Result, Pos(#0, Result) - 1);

  Result := IncludeTrailingPathDelimiter(Result) + CompanyName;
  if not DirectoryExists(Result) then
    CreateDir(Result);
  Result := Result + '\DrvFR';
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function GetCommonShtrihPath: string;
begin
  SetLength(Result, MAX_PATH);
  ShlObj.SHGetSpecialFolderPath(0, @Result[1], CSIDL_COMMON_APPDATA, false);
  SetLength(Result, Pos(#0, Result) - 1);

  Result := IncludeTrailingPathDelimiter(Result) + CompanyName;
  if not DirectoryExists(Result) then
    CreateDir(Result);
  Result := Result + '\DrvFR';
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function GetTempShtrihPath: string;
begin
  SetLength(Result, MAX_PATH);
  ShlObj.SHGetSpecialFolderPath(0, @Result[1], CSIDL_COMMON_APPDATA, false);
  SetLength(Result, Pos(#0, Result) - 1);

  Result := IncludeTrailingPathDelimiter(Result) + CompanyName;
  if not DirectoryExists(Result) then
    CreateDir(Result);
  Result := Result + '\DrvFR';
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function GetUserShtrihPath_def: string;
begin
  SetLength(Result, MAX_PATH);
  ShlObj.SHGetSpecialFolderPath(0, @Result[1], CSIDL_APPDATA, false);
  SetLength(Result, Pos(#0, Result) - 1);

  Result := IncludeTrailingPathDelimiter(Result) + CompanyName;
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function GetDefaultLogFileName: string;
var
  DllFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(GetCommonShtrihPath) + 'Logs\';
  if not DirectoryExists(Result) then
    CreateDir(Result);
  Result := Result + ChangeFileExt(ExtractFileName(DllFileName), '.log');
end;

function GetBackupTablesPath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserShtrihPath) + 'Tables';
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
  GlobalLogger.Debug('IsModelType2 ' + IntToStr(Value) + ' ' + SysUtils.BoolToStr(Result, True));
end;

end.

