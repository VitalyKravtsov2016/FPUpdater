unit EcrManager;

interface

uses
  // VCL
  Classes, SysUtils, Variants,
  // This
  DrvFRLib_TLB, untDriver;

type
  { TEcrManager }

  TEcrManager = class
  private
    FLines: TStrings;
    FDriver: TDriver;
    procedure AddFullStatus;
    procedure AddSeparator;
    procedure AddDeviceFlags;
    procedure AddLine(V1, V2: Variant);
    procedure AddBool(const ValueName: WideString; Value: Boolean);
    procedure AddLineWidth(V1, V2: Variant; TextWidth: Integer);
  public
    constructor Create(ADriver: TDriver);
    destructor Destroy; override;
    procedure ReadFullStatus;

    property Lines: TStrings read FLines;
    property Driver: TDriver read FDriver;
  end;

implementation

const
  DescriptionWidth = 33;

function GetRes(Value: PResStringRec): string;
begin
  Result := LoadResString(Value);
end;

resourcestring
  SBooleanTrue = '[да]';
  SBooleanFalse = '[нет]';
  SFlags = 'Флаги';
  SFMFlags = 'Флаги ФП';
  SFMFlagsEx = 'Доп. Флаги ФП';
  SFMMode = 'Режим ФП';
  SASPDMode = 'Режим АСПД';
  SIsCorruptedFMRecords = 'Повреждены 3 или более записей';
  SIsCorruptedFiscalizationInfo = 'Повреждена запись фискализации';
  SQuantityPointPosition = 'Увеличенная точность количества';
  SPresenterOut = 'Бумага на выходе из накопителя';
  SPresenterIn = 'Бумага на входе в накопитель';
  SIsEKLZOverflow = 'ЭКЛЗ почти заполнена';
  SIsDrawerOpen = 'Денежный ящик открыт';
  SLidPositionSensor = 'Крышка корпуса поднята';
  SReceiptRibbonLever = 'Рычаг термоголовки чека опущен';
  SJournalRibbonLever = 'Рычаг термоголовки журнала опущен';
  SReceiptRibbonOpticalSensor = 'Оптический датчик чека';
  SJournalRibbonOpticalSensor = 'Оптический датчик журнала';
  SEKLZIsPresent = 'ЭКЛЗ есть';
  SPointPosition = '2 знака после запятой в цене';
  SSlipDocumentIsPresent = 'Нижний датчик ПД';
  SSlipDocumentIsMoving = 'Верхний датчик ПД';
  SReceiptRibbonIsPresent = 'Рулон чековой ленты';
  SJournalRibbonIsPresent = 'Рулон контрольной ленты';
  SStatusQuery = ' Запрос состояния:';
  SPrinterMode = ' Режим: ';
  SPrinterSoftwareVersion = 'Версия ПО';
  SPrinterSoftwareBuild = 'Сборка ПО';
  SPrinterSoftwareDate = 'Дата ПО';
  SFMSoftwareVersion = 'Версия ПО ФП';
  SFMSoftwareBuild = 'Сборка ПО ФП';
  SFMSoftwareDate = 'Дата ПО ФП';
  SPrinterSubMode = 'Подрежим';
  SPrinterModeStatus = 'Статус режима';
  SLogicalNumber = 'Номер ККМ в зале';
  SDocumentNumber = 'Номер документа';
  SPortNumber = 'Интерфейс подключения';
  SRegistrationNumber = 'Количество фискализаций';
  SFreeRegistration = 'Осталось фискализаций';
  SSessionNumber = 'Последняя закрытая смена';
  SFreeRecordInFM = 'Свободных записей в ФП';
  SDate = 'Дата';
  SPrinterTime = 'Время';
  SSerialNumber = 'Заводской номер';
  SINN = 'ИНН';
  SIsFM24HoursOver = '24 часа в ФП кончились';
  SIsFMSessionOpen = 'Смена в ФП открыта';
  SIsLastFMRecordCorrupted = 'Последняя запись в ФП повреждена';
  SIsBatteryLow = 'Батарея ФП заряжена более 80 %';
  SFMOverflow = 'Переполнение ФП';
  SLicenseIsPresent = 'Лицензия введена';
  SFM2IsPresent = 'ФП2 есть';
  SFM1IsPresent = 'ФП1 есть';
  SShortStatusQuery = ' Краткий запрос состояния:';
  SQuantityOfOperations = 'Количество операций в чеке';
  SBatteryVoltage = 'Напряжение батареи, В';
  SPowerSourceVoltage = 'Напряжение источника, В';
  SFMResultCode = 'Ошибка ФП';
  SEKLZResultCode = 'Ошибка ЭКЛЗ';
  SLastPrintResult = 'Результат последней печати';
  SPrinterParameters = ' Параметры принтера:';
  SUCodePage = ' Кодовая страница    : ';
  SUDescription = ' Описание устройства : ';
  SUMajorProtocolVersion = ' Версия протокола    : ';
  SUMinorProtocolVersion = ' Подверсия протокола : ';
  SUMajorType = ' Тип устройства      : ';
  SUMinorType = ' Подтип устройства   : ';
  SUModel = ' Модель устройства   : ';
  SPrinterLockedAfterInvalidTaxPassword =
    'Принтер заблокирован из-за ввода неправильного ' +
    'пароля налогового инспектора';
  SPrinterInTechnologicalMode =
    'Принтер находится в режиме технологического обнуления';
  SModelParameters = ' Параметры Модели:';
  SDriverVersionCaption = ' Версия драйвера:';
  SDriverMajorVersion = 'Версия';
  SDriverMinorVersion = 'Подверсия';
  SDriverRelease = 'Релиз';
  SDriverBuild = 'Сборка';

  SPreviousECRMode = 'Предыдущий режим ККТ';
  SPrinterHeadTemperature = 'Температура ТПГ';
  SUpdateKeysResultCode = 'Код ошибки при обновлении ключей';
  UpdateKeysStatus = 'Статус обновления ключей';
  UpdatedKeysCount = 'Кол-во обновленных ключей';

  SDriverVersion = 'Полная версия';
  SFullStatus = ' Полное состояние';

  SUserAbort = '<Прервано пользователем>';
  SErrorAbort = '<Прервано в результате ошибки: %d %s>';

  SCashRegisters      = ' ДЕНЕЖНЫЕ РЕГИСТРЫ:';
  SCashRegistersEX      = ' ДЕНЕЖНЫЕ РЕГИСТРЫ (К):';
  SFNCounters         = ' СЧЕТЧИКИ ФН:';
  SOperationRegisters = ' ОПЕРАЦИОННЫЕ РЕГИСТРЫ:';
  STableInfo = ' ТАБЛИЦА %d. %s. Рядов:%d Полей:%d';
  STableHeader = ' Ряд.Поле. Наименование:Значение';
  STables = ' ТАБЛИЦЫ:';
  SOperatorNumber = 'Номер оператора';
  SCurrentDate = 'Текущая дата';
  SCurrentTime = 'Текущее время';
  SSaleReceiptNumber = 'Количество чеков продаж';
  SBuyReceiptNumber = 'Количество чеков покупок';
  SRetSaleReceiptNumber = 'Количество чеков возврата продаж';
  SRetBuyReceiptNumber = 'Количество чеков возврата покупок';
  SDayOpenDate = 'Дата начала открытой смены';
  SDayOpenTime = 'Время начала открытой смены';
  SCashTotal = 'Наличные в кассе';
  SFlagSerialized = 'Сериализована';
  SFlagFiscalized = 'Фискализирована';
  SFlagActivated = 'Активизирована';
  SFlagDayOpened = 'Смена открыта';
  SFlagDayExausted = '24 часа кончились';
  SPrintBufferEmpty = 'Буфер печати пуст';
  SStatusByte = 'Байт состояния';
  STimeout = 'Таймаут';
  SEnterComputerName = 'Укажите имя компьютера';
  SData = 'Данные';
  SMacNumber = 'Номер КПК';
  SError = 'Ошибка %d: %s'#10#13;
  SBlockNumber = 'Получено блоков: %d';
  SPump = 'ТРК';
  STicket = 'СЧЕТ';
  SPrice = 'Цена';
  SSumm1 = 'Сумма 1';
  SSumm2 = 'Сумма 2';
  SSumm3 = 'Сумма 3';
  SSumm4 = 'Сумма 4';
  SNumber = 'Номер';
  SQuantity = 'Количество';
  SDiscount = 'Скидка на чек';
  SString = 'Строка';
  STax1 = 'Налог 1';
  STax2 = 'Налог 2';
  STax3 = 'Налог 3';
  STax4 = 'Налог 4';
  SGeneral = 'Основные';
  SNo = 'Нет';
  SGroup1 = '1 группа';
  SGroup2 = '2 группа';
  SGroup3 = '3 группа';
  SGroup4 = '4 группа';
  SStringForPrinting = 'Строка для печати';
  SExtended = 'Дополнительные';
  SSlipDocumentWidth = 'Ширина';
  SSlipDocumentLength = 'Длина';
  SPrintingAlignment = 'Ориентация';
  SSlipLineSpacing = 'Межстрочный интервал между строками: %d и %d';
  SExciseCode = 'Код акциза';

  SSlipCopyType = 'Дублирование печати';
  SSlipReceiptType = 'Тип чека';
  SSlipCopyOffset1 = 'Смещение 1';
  SSlipCopyOffset2 = 'Смещение 2';
  SSlipCopyOffset3 = 'Смещение 3';
  SSlipCopyOffset4 = 'Смещение 4';
  SSlipCopyOffset5 = 'Смещение 5';
  SSlipNumberOfCopies = 'Количество копий';
  SLine1 = 'Строка 1';
  SLine2 = 'Строка 2';
  SDepartment = 'Отдел';
  STableName = 'Название таблицы';
  SRowCount = 'Количество рядов: ';
  SFieldCount = 'Количество полей';
  SFieldName = 'Название поля';
  SFieldTypeString = 'Тип поля: строка';
  SFieldTypeNumber = 'Тип поля: число';
  SFieldSize = 'Размер поля';
  SMinValue = 'Мин. значение';
  SMaxValue = 'Макс. значение';
  STableStructure = 'Структура поля';
  SNewTaxPassword = 'Новый пароль НИ';
  SFiscalizationNumber = 'Номер фискализации';
  SStartDay = 'Начальная смена';
  SEndDay = 'Конечная смена';
  STaxPasswordConfirmation = 'Будет введен пароль налогового инспектора (%d)'#13#10 +
    'В случае ошибки принтер будет заблокирован до ввода правильного пароля.'#13#10 +
    'Продолжить?';

  SSale = 'Продажа';
  SBuy = 'Покупка';
  SRetSale = 'Возврат продажи';
  SRetBuy = 'Возврат покупки';
  SSaleStorno = 'Сторно продажи';
  SBuyStorno = 'Сторно покупки';
  SRetSaleStorno = 'Сторно возврата продажи';
  SRetBuyStorno = 'Сторно возврата покупки';

  SReceipt = 'Чек';
  SLine = 'строка';

  SAlignmentText = 'По центру'#13#10'Влево'#13#10'Вправо';
  SPrintWidth576 = 'Полная ширина печати. 576 точек';
  SLeftBound = 'Левая граница. Точка 1 печатается';
  SRightBound = 'Правая граница. Точка 576 печатается';
  SPrintWidth560 = 'Вывод - ширина печати 560 точек, а не 576';
  SPrinterType = 'Тип ККМ';
  SAttention = 'Внимание!';
  SConfirmFiscalization = 'Фискализацию аппарата нельзя отменить.'#13#10+
    'Вы хотите продолжить?';
  SConfirmRegistration = 'Перерегистрацию аппарата нельзя отменить.'#13#10+
    'Вы хотите продолжить?';


  SSKNOStatus = 'Статус СКНО:';
  SSKNOIdentifier = 'Уникальный идентификатор СКНО:';

  SBusy = 'Занят';
  SSKZI = 'СКЗИ';
  SServerConnection = 'Соединение с сервером';
  SSKZIExpired = 'Запрет обслуживания по окончанию сертификата СКЗИ';
  SReportDenied = 'Запрет обслуживания по не переданным суточным (сменным) отчетам (Z-отчетам)';
  SReportOverflow = 'Запрет обслуживания по переполнению памяти СКНО';
  SIdentSuccess = 'Идентификация прошла успешно';
  SSessionOpened = 'Смена открыта';
  SDocumentNotCompleted = 'Не завершена процедура по переданному документу';
  SSKNODocumentNotTransmitted = 'Наличие в памяти СКНО не переданных документов';
  SSKNODocumentLimitExceeded = 'Превышен максимальный размер электронного кассового документа';
  SSKNOGood = 'СКНО исправно';



function BoolToYesNo(AValue: Boolean): WideString;
begin
  if AValue then
    Result := GetRes(@SBooleanTrue)
  else
    Result := GetRes(@SBooleanFalse);
end;

{ TEcrManager }

constructor TEcrManager.Create(ADriver: TDriver);
begin
  inherited Create;
  FDriver := ADriver;
  FLines := TStringList.Create;
end;

destructor TEcrManager.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TEcrManager.AddSeparator;
begin
  Lines.Add(StringOfChar('-', 52));
end;

procedure TEcrManager.AddLine(V1, V2: Variant);
begin
  AddLineWidth(V1, V2, 24);
end;

procedure TEcrManager.AddLineWidth(V1, V2: Variant; TextWidth: Integer);
begin
  Lines.Add(Format('%-*s: %s', [TextWidth, string(V1), string(V2)]));
end;

procedure TEcrManager.AddBool(const ValueName: WideString; Value: Boolean);
begin
  AddLineWidth(ValueName, BoolToYesNo(Value), DescriptionWidth);
end;


procedure TEcrManager.ReadFullStatus;
begin
  Lines.Clear;
  AddFullStatus;
(*
  AddShortStatus;
  AddDeviceMetrics;
  AddTables;
  AddCashTotalizers;
  AddOperationRegisters;
  AddSeparator;
*)
end;


procedure TEcrManager.AddFullStatus;
var
  IsCashCore: Boolean;
begin
  Driver.Check(Driver.GetDeviceMetrics);
  Driver.Check(Driver.GetECRStatus);
  Driver.ModelParamNumber := mpCapCashCore;
  Driver.Check(Driver.ReadModelParamValue);
  IsCashCore := Driver.ModelParamValue;

  Lines.Add('');
  AddSeparator;
  Lines.Add(SStatusQuery);
  AddSeparator;
  Lines.Add(SPrinterMode);
  Lines.Add(' ' + Format('%d, %s', [Driver.ECRMode, Driver.ECRModeDescription]));
  // Printer software
  AddSeparator;
  AddLine(SPrinterSoftwareVersion, Driver.ECRSoftVersion);
  AddLine(SPrinterSoftwareBuild, Driver.ECRBuild);
  AddLine(SPrinterSoftwareDate, Driver.ECRSoftDate);
  // Fiscal memory software
  if not Driver.CapFN then
  begin

    AddSeparator;
    AddLine(SFMSoftwareVersion, Driver.FMSoftVersion);
    AddLine(SFMSoftwareBuild, Driver.FMBuild);
    AddLine(SFMSoftwareDate, Driver.FMSoftDate);
  end;
  // Mode
  AddSeparator;
  AddLine(SPrinterSubMode, Format('%d, %s', [Driver.ECRAdvancedMode, Driver.ECRAdvancedModeDescription]));
  AddLine(SPrinterModeStatus, Driver.ECRModeStatus);
  AddLine(SLogicalNumber, Driver.LogicalNumber);
  AddLine(SDocumentNumber, Driver.OpendocumentNumber);
  AddLine(SPortNumber, Driver.PortNumber);
  if not Driver.CapFN then
  begin
    AddLine(SRegistrationNumber, Driver.RegistrationNumber);
    AddLine(SFreeRegistration, Driver.FreeRegistration);
  end;
  AddLine(SSessionNumber, Driver.SessionNumber);
  if not Driver.CapFN then
    AddLine(SFreeRecordInFM, Driver.FreeRecordInFM);
  AddLine(SDate, Driver.Date);
  AddLine(SPrinterTime, Driver.Time);
  if not Driver.CapFN then
    AddLine(SSerialNumber, Driver.SerialNumber);
  AddLine(SINN, Driver.INN);
  // Flags
  AddDeviceFlags;
  // Fiscal memory flags
  AddSeparator;
  if not Driver.CapFN then
  begin
    AddLine(GetRes(@SFMFlags), Format('%.2xh, %d', [Driver.FMFlags, Driver.FMFlags]));
    if IsCashCore then
      AddLine(GetRes(@SFMFlagsEx), Format('%.2xh, %d', [Driver.FMFlagsEx, Driver.FMFlagsEx]));
  end;

  AddSeparator;
  if not Driver.CapFN then
  begin
    AddBool(GetRes(@SIsFM24HoursOver), Driver.IsFM24HoursOver);
    AddBool(GetRes(@SIsFMSessionOpen), Driver.IsFMSessionOpen);
    AddBool(GetRes(@SIsLastFMRecordCorrupted), Driver.IsLastFMRecordCorrupted);
    AddBool(GetRes(@SIsBatteryLow), Driver.IsBatteryLow);
    AddBool(GetRes(@SFMOverflow), Driver.FMOverflow);
    AddBool(GetRes(@SLicenseIsPresent), Driver.LicenseIsPresent);
    AddBool(GetRes(@SFM2IsPresent), Driver.FM2IsPresent);
    AddBool(GetRes(@SFM1IsPresent), Driver.FM1IsPresent);
  end;
  if IsCashCore then
  begin
    AddBool(GetRes(@SASPDMode), Driver.IsASPDMode);
    if not Driver.CapFN then
    begin
      AddBool(GetRes(@SIsCorruptedFMRecords), Driver.IsCorruptedFMRecords);
      AddBool(GetRes(@SIsCorruptedFiscalizationInfo), Driver.IsCorruptedFiscalizationInfo);
    end;
  end;
  AddSeparator;
end;

procedure TEcrManager.AddDeviceFlags;
begin
  AddSeparator;
  AddLine(GetRes(@SFlags), Format('%.4xh, %d', [Driver.ECRFlags, Driver.ECRFlags]));
  AddSeparator;
  AddBool(GetRes(@SQuantityPointPosition), Driver.QuantityPointPosition);
  AddBool(GetRes(@SPresenterOut), Driver.PresenterOut);
  AddBool(GetRes(@SPresenterIn), Driver.PresenterIn);
  if not Driver.CapFN then
    AddBool(GetRes(@SIsEKLZOverflow), Driver.IsEKLZOverflow);
  AddBool(GetRes(@SIsDrawerOpen), Driver.IsDrawerOpen);
  AddBool(GetRes(@SLidPositionSensor), Driver.LidPositionSensor);
  AddBool(GetRes(@SReceiptRibbonLever), Driver.ReceiptRibbonLever);
  AddBool(GetRes(@SJournalRibbonLever), Driver.JournalRibbonLever);
  AddBool(GetRes(@SReceiptRibbonOpticalSensor), Driver.ReceiptRibbonOpticalSensor);
  AddBool(GetRes(@SJournalRibbonOpticalSensor), Driver.JournalRibbonOpticalSensor);
  if not Driver.CapFN then
    AddBool(GetRes(@SEKLZIsPresent), Driver.EKLZIsPresent);
  AddBool(GetRes(@SPointPosition), Driver.PointPosition);
  AddBool(GetRes(@SSlipDocumentIsPresent), Driver.SlipDocumentIsPresent);
  AddBool(GetRes(@SSlipDocumentIsMoving), Driver.SlipDocumentIsMoving);
  AddBool(GetRes(@SReceiptRibbonIsPresent), Driver.ReceiptRibbonIsPresent);
  AddBool(GetRes(@SJournalRibbonIsPresent), Driver.JournalRibbonIsPresent);
end;

end.
