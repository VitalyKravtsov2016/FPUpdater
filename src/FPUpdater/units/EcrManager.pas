unit EcrManager;

interface

uses
  // VCL
  Classes, SysUtils, Variants, Generics.Collections,
  // This
  DrvFRLib_TLB, untDriver, BinUtils, gnugettext;

const
  CashRegisterMin = $00;
  CashRegisterMax = $FF;

  OperationRegisterMin = $00;
  OperationRegisterMax = $FF;


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
    procedure AddShortStatus;
    procedure AddDeviceMetrics;
    procedure AddTables;
    procedure AddCashTotalizers;
    procedure AddOperationRegisters;
    procedure AddExRegisters;
    procedure AddTaxSystem;
    procedure AddWorkMode;
    procedure AddWorkModeEx(AWorkModeEx: Integer);
    procedure AddRegistrationReasonCodeEx;
  public
    constructor Create(ADriver: TDriver);
    destructor Destroy; override;

    procedure ReadFiscResult;
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

resourcestring
  SChaneFN = 'Замена ФН';
  SChangeOFD = 'Замена ОФД';
  SChangeRequisites = 'Изменение реквизитов';
  SChangeKKTParams = 'Изменение настроек ККТ';
  SUnknownCode = 'Неизвестный код';

function ReasonCodeToStr(ACode: Integer): WideString;
begin
  case ACode of
    0:
      Result := '';
    1:
      Result := GetRes(@SChaneFN);
    2:
      Result := GetRes(@SChangeOFD);
    3:
      Result := GetRes(@SChangeRequisites);
    4:
      Result := GetRes(@SChangeKKTParams);
  else
    Result := SUnknownCode;
  end;
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
  AddShortStatus;
  AddDeviceMetrics;
  AddTables;
  AddCashTotalizers;
  AddExRegisters;
  AddOperationRegisters;
  AddSeparator;
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

procedure TEcrManager.AddShortStatus;
begin
  Driver.Check(Driver.GetDeviceMetrics);
  Driver.Check(Driver.GetShortECRStatus);

  Lines.Add('');
  AddSeparator;
  Lines.Add(GetRes(@SShortStatusQuery));
  AddSeparator;
  Lines.Add(GetRes(@SPrinterMode));
  Lines.Add(Format(' %d, %s', [Driver.ECRMode, Driver.ECRModeDescription]));
  AddSeparator;
  AddLine(GetRes(@SPrinterSubMode), Format('%d, %s', [Driver.ECRAdvancedMode, Driver.ECRAdvancedModeDescription]));
  AddLine(GetRes(@SPrinterModeStatus), Driver.ECRModeStatus);
  AddLine(GetRes(@SQuantityOfOperations), Driver.QuantityOfOperations);
  AddLine(GetRes(@SBatteryVoltage), Driver.BatteryVoltage);
  AddLine(GetRes(@SPowerSourceVoltage), Driver.PowerSourceVoltage);
  if not Driver.CapFN then
  begin
    AddLine(GetRes(@SFMResultCode), Driver.FMResultCode);
    AddLine(GetRes(@SEKLZResultCode), Driver.EKLZResultCode);
  end;
  // Flags
  AddDeviceFlags;
  AddSeparator;
end;

procedure TEcrManager.AddDeviceMetrics;
begin
  Driver.Check(Driver.GetDeviceMetrics);

  Lines.Add('');
  AddSeparator;
  Lines.Add(GetRes(@SPrinterParameters));
  AddSeparator;
  Lines.Add(GetRes(@SUCodePage) + IntToStr(Driver.UCodePage));
  Lines.Add(GetRes(@SUDescription) + Driver.UDescription);
  Lines.Add(GetRes(@SUMajorProtocolVersion) + IntToStr(Driver.UMajorProtocolVersion));
  Lines.Add(GetRes(@SUMinorProtocolVersion) + IntToStr(Driver.UMinorProtocolVersion));
  Lines.Add(GetRes(@SUMajorType) + IntToStr(Driver.UMajorType));
  Lines.Add(GetRes(@SUMinorType) + IntToStr(Driver.UMinorType));
  Lines.Add(GetRes(@SUModel) + IntToStr(Driver.UModel));
end;

procedure TEcrManager.AddTables;

  function AddTable(ATableNumber: Integer): Integer;
  var
    Row: Integer;
    Field: Integer;
    RowN: Integer;
    FieldN: Integer;
    FieldName: WideString;
  begin
    Driver.TableNumber := ATableNumber;
    Result := Driver.GetTableStruct;
    if Result <> 0 then
      Exit;
    RowN := Driver.RowNumber;
    FieldN := Driver.FieldNumber;
    Lines.Add('');
    AddSeparator;
    Lines.Add(Format(GetRes(@STableInfo),
      [Driver.TableNumber, Driver.TableName, RowN, FieldN]));

    Lines.Add(GetRes(@STableHeader));
    AddSeparator;
    Driver.TableNumber := ATableNumber;
    Result := Driver.GetTableStruct;
    if (Result <> 0) then
      Exit;
    RowN := Driver.RowNumber;
    FieldN := Driver.FieldNumber;
    for Row := 1 to RowN do
    begin
      Driver.RowNumber := Row;
      for Field := 1 to FieldN do
      begin
        Driver.FieldNumber := Field;
        Result := Driver.GetFieldStruct;
        if Result <> 0 then
          Exit;
        FieldName := Driver.FieldName;
        Result := Driver.ReadTable;
        if Result <> 0 then
          Exit;
        Lines.Add(Format(' %.2d.%.2d. %s:%s', [Row, Field, FieldName, Driver.ValueOfFieldString]));
        Driver.FieldName;
      end;
      AddSeparator;
    end;
  end;

var
  i: Integer;
  Res: Integer;
begin
  Lines.Add('');
  AddSeparator;
  Lines.Add(GetRes(@STables));
  AddSeparator;
  i := 1;
  while True do
  begin
    Res := AddTable(i);
    if (Res <> 0) then
      Break;
    Inc(i);
  end;
  if Res = $5D then
    Res := 0;
  Driver.Check(Res);
end;

procedure TEcrManager.AddCashTotalizers;
var
  i: Integer;
begin
  Lines.Add('');
  Lines.Add(GetRes(@SCashRegisters));
  Lines.Add(' ' + StringOfChar('-', 54));
  for i := CashRegisterMin to CashRegisterMax do
  begin
    Driver.RegisterNumber := i;
    if Driver.GetCashReg <> 0 then Break;
    Lines.Add(Format(' %3d.%-44s : %s',
      [i, Driver.NameCashReg,
      Driver.AmountToStr(Driver.ContentsOfCashRegister)]));
  end;
end;

procedure TEcrManager.AddExRegisters;
const
  NumericRegs: TArray<Integer> = [
  4228, 4229, 4230, 4243, 4256, 4269, 4282, 4283, 4285, 4287,
  4289, 4291, 4292, 4293, 4306, 4319, 4332, 4345, 4346, 4348,
  4350, 4352, 4354, 4355, 4357, 4359, 4361];
var
  i: Integer;
  ind: Integer;
begin
  Lines.Add(GetRes(@SCashRegistersEx));

{$O-}
  for i := 4096 to 4522 do
  begin
    Driver.RegisterNumber := i;
    if Driver.GetCashRegEx <> 0 then Break;

    if TArray.BinarySearch<Integer>(NumericRegs, i, ind) then
      Lines.Add(Format(' %3d.%-56s : %s',
        [i, Driver.NameCashRegEx, IntToStr(Round(Driver.ContentsOfCashRegister))]))
    else
      Lines.Add(Format(' %3d.%-56s : %s',
        [i, Driver.NameCashRegEx, Driver.AmountToStr(Driver.ContentsOfCashRegister)]));

  end;
{$O+}
end;

procedure TEcrManager.AddOperationRegisters;
var
  i: Integer;
begin
  Lines.Add('');
  AddSeparator;
  Lines.Add(GetRes(@SOperationRegisters));
  AddSeparator;

  for i := OperationRegisterMin to OperationRegisterMax do
  begin
    Driver.RegisterNumber := i;
    if Driver.GetOperationReg <> 0 then Break;

    Lines.Add(Format(' %3d.%-44s : %s', [
      i, Driver.NameOperationReg,
        Driver.AmountToStr(Driver.ContentsOfOperationRegister)]))
  end;
end;

resourcestring
  SWorkMode = 'Режим работы';
  STaxType = 'Код налогообложения';
  SKKTRegistrationNumber = 'Рег. номер ККТ';
  SFNGetFiscalizationResult = 'Запрос итогов фискализации ФН';
  SFNGetFiscalizationResultByNumber = 'Запрос итогов фискализации ФН по номеру';
  SFNSerial = 'Номер ФН';
  SRegistrationReasonCode = 'Код причины перерегистрации';
  SINNOFD = 'ИНН ОФД';
  SWorkModeEx = 'Расширенные признаки работы ККТ';
  SRegistrationReasonCodeEx = 'Код причины изменения сведений о ККТ';
  SFNDocumentNumber = 'Номер ФД';
  SFiscalSign = 'Фискальный признак';
  SFNFiscalization = 'Сформировать отчет о регистрации ККТ';

procedure TEcrManager.ReadFiscResult;
var
  WorkModeEx: Integer;
  Table: Integer;
  InnOfd: string;
begin
  Table := Driver.FSTableNumber;
  Driver.Check(Driver.FNGetFiscalizationResult);
  InnOfd := Driver.INNOFD;
  if Driver.INNOFD = '' then
  begin
    if not Driver.IsModelType2 then
      InnOfd := Driver.ReadTableStr(Table, 1, 12);
  end;

  WorkModeEx := Driver.WorkModeEx;
  if Driver.WorkModeEx = 0 then
  begin
    if not Driver.IsModelType2 then
      WorkModeEx := Driver.ReadTableDef(Table, 1, 21, 0);
  end;
  Lines.Add(GetRes(@SFNGetFiscalizationResult));
  AddSeparator;
  AddLine(GetRes(@SINN), Driver.INN);
  AddLine(GetRes(@SINNOFD), InnOfd);
  AddLine(GetRes(@SKKTRegistrationNumber), Driver.KKTRegistrationNumber);
  AddTaxSystem;
  AddWorkMode;
  AddWorkModeEx(WorkModeEx);
  AddLine(GetRes(@SDate), DateToStr(Driver.Date));
  AddLine(GetRes(@SPrinterTime), TimeToStr(Driver.Time));
  AddLine(GetRes(@SRegistrationReasonCode), Format('%d, %s', [
    Driver.RegistrationReasonCode, ReasonCodeToStr(Driver.RegistrationReasonCode)]));
  AddRegistrationReasonCodeEx;
  AddLine(GetRes(@SFNDocumentNumber), IntToStr(Cardinal(Driver.DocumentNumber)));
  AddLine(GetRes(@SFiscalSign), IntToStr(Cardinal(Driver.FiscalSign)));
end;

resourcestring
  STaxCode = 'Код сист. налогообложения';
  SOSN = 'ОСН';
  SUSND = 'УСН доход';
  SUSNDMR = 'УСН доход минус расход';
  SENVD = 'ЕНВД';
  SESN = 'ЕСХН';
  SPSN = 'Патент';

procedure TEcrManager.AddTaxSystem;
begin
  AddLine(GetRes(@STaxCode), IntToStr(Driver.TaxType));
  AddBool('  ' + GetRes(@SOSN), TestBit(Driver.TaxType, 0));
  AddBool('  ' + GetRes(@SUSND), TestBit(Driver.TaxType, 1));
  AddBool('  ' + GetRes(@SUSNDMR), TestBit(Driver.TaxType, 2));
  AddBool('  ' + GetRes(@SENVD), TestBit(Driver.TaxType, 3));
  AddBool('  ' + GetRes(@SESN), TestBit(Driver.TaxType, 4));
  AddBool('  ' + GetRes(@SPSN), TestBit(Driver.TaxType, 5));
end;

resourcestring
  SChiperMode = 'Шифрование';
  SOfflineMode = 'Автономный режим';
  SAutoMode = 'Автоматический режим';
  SServiceMode = 'Применение в сфере услуг';
  SBSOMode = 'Режим БСО';
  SInternetMode = 'Применение в Интернет';
  SPublicCatering = 'ККТ в общественном питании';
  SWholesale = 'Оптовая торговля с организациями и ИП';

procedure TEcrManager.AddWorkMode;
begin
  AddLine(GetRes(@SWorkMode), IntToStr(Driver.WorkMode));
  AddBool('  ' + GetRes(@SChiperMode), TestBit(Driver.WorkMode, 0));
  AddBool('  ' + GetRes(@SOfflineMode), TestBit(Driver.WorkMode, 1));
  AddBool('  ' + GetRes(@SAutoMode), TestBit(Driver.WorkMode, 2));
  AddBool('  ' + GetRes(@SServiceMode), TestBit(Driver.WorkMode, 3));
  AddBool('  ' + GetRes(@SBSOMode), TestBit(Driver.WorkMode, 4));
  AddBool('  ' + GetRes(@SInternetMode), TestBit(Driver.WorkMode, 5));
  AddBool('  ' + GetRes(@SPublicCatering), TestBit(Driver.WorkMode, 6));
  AddBool('  ' + GetRes(@SWholesale), TestBit(Driver.WorkMode, 7));
end;

procedure TEcrManager.AddWorkModeEx(AWorkModeEx: Integer);
begin
  AddLine(GetRes(@SWorkModeEx), IntToStr(AWorkModeEx));
  AddBool('  ' + _('Продажа подакцизного товара'), TestBit(AWorkModeEx, 0));
  AddBool('  ' + _('Признак проведения азартных игр'), TestBit(AWorkModeEx, 1));
  AddBool('  ' + _('Признак проведения лотереи'), TestBit(AWorkModeEx, 2));
  AddBool('  ' + _('Признак установки принтера в автомате'), TestBit(AWorkModeEx, 3));
  AddBool('  ' + _('Признак торговли маркиров. товарами (1.2)'), TestBit(AWorkModeEx, 4));
  AddBool('  ' + _('Признак ломбардной деятельности (1.2)'), TestBit(AWorkModeEx, 5));
  AddBool('  ' + _('Признак страховой деятельности (1.2)'), TestBit(AWorkModeEx, 6));
  AddBool('  ' + _('ККТ с торговым автоматом (1.2)'), TestBit(AWorkModeEx, 7));
end;

procedure TEcrManager.AddRegistrationReasonCodeEx;

  procedure AddCodeExLine(const Name: WideString; Bit: Integer);
  begin
    if TestBit(Cardinal(Driver.RegistrationReasonCodeEx), Bit) then
      AddBool('  ' + Name, True);
  end;

begin
  AddLine(GetRes(@SRegistrationReasonCodeEx), IntToStr(Cardinal(Driver.RegistrationReasonCodeEx)));
  AddCodeExLine(_('Замена фискального накопителя'), 0);
  AddCodeExLine(_('Замена оператора фискальных данных'), 1);
  AddCodeExLine(_('Изменение наименования пользователя контрольно-кассовой техники'), 2);
  AddCodeExLine(_('Изменение адреса и (или) места установки (применения) контрольно-кассовой техники'), 3);
  AddCodeExLine(_('Перевод ККТ из автономного режима в режим передачи данных'), 4);
  AddCodeExLine(_('Перевод ККТ из режима передачи данных в автономный режим'), 5);
  AddCodeExLine(_('Изменение версии модели ККТ'), 6);
  AddCodeExLine(_('Изменение перечня систем налогообложения, применяемых при осуществлении расчетов'), 7);
  AddCodeExLine(_('Изменение номера автоматического устройства для расчетов, в составе которого применяется ККТ'), 8);
  AddCodeExLine(_('Перевод ККТ из автоматического режима в неавтоматический режим (осуществление расчетов кассиром)'), 9);
  AddCodeExLine(_('Перевод ККТ из неавтоматического режима (осуществление расчетов кассиром) в автоматический режим'), 10);
  AddCodeExLine(_('Перевод ККТ из режима, не позволяющего формировать БСО, в режим, позволяющий формировать БСО'), 11);
  AddCodeExLine(_('Перевод ККТ из режима, позволяющего формировать БСО, в режим, не позволяющий формировать БСО'), 12);
  AddCodeExLine(_('Перевод ККТ из режима расчетов в сети Интернет (позволяющего не печатать кассовый чек и БСО) в режим, позволяющий печатать кассовый чек и БСО'), 13);
  AddCodeExLine(_('Перевод ККТ из режима, позволяющего печатать кассовый чек и БСО, в режим расчетов в сети Интернет (позволяющего не печатать кассовый чек и БСО)'), 14);
  AddCodeExLine(_('Перевод ККТ из режима, позволяющего оказывать услуги платежного агента (субагента) ' + 'или банковского платежного агента, в режим, не позволяющий оказывать услуги платежного агента (субагента) или банковского платежного агента'), 15);
  AddCodeExLine(_('Перевод ККТ из режима, не позволяющего оказывать услуги платежного агента (субагента) ' + 'или банковского платежного агента в режим, позволяющий оказывать услуги платежного агента (субагента) или банковского платежного агента'), 16);
  AddCodeExLine(_('Перевод ККТ из режима, позволяющего применять ККТ при приеме ставок и выплате денежных средств в виде ' + 'выигрыша при осуществлении деятельности по проведению азартных игр, в режим, не позволяющий применять ' + 'ККТ при приеме ставок и выплате денежных средств в виде выигрыша при осуществлении деятельности по проведению азартных игр'), 17);
  AddCodeExLine(_('Перевод ККТ из режима, не позволяющего применять ККТ при приеме ставок и выплате денежных средств в виде ' + 'выигрыша при осуществлении деятельности по проведению азартных игр, в режим, позволяющий применять ККТ при ' + 'приеме ставок и выплате денежных средств в виде выигрыша при осуществлении деятельности по проведению азартных игр'), 18);
  AddCodeExLine(_('Перевод ККТ из режима, позволяющего применять ККТ при приеме денежных средств при реализации лотерейных билетов, ' + 'электронных лотерейных билетов, приеме лотерейных ставок и выплате денежных средств в виде выигрыша при ' + 'осуществлении деятельности по проведению лотерей, в режим, не позволяющий применять ККТ при приеме денежных ' + 'средств при реализации лотерейных билетов, электронных лотерейных билетов, приеме лотерейных ставок и выплате ' + 'денежных средств в виде выигрыша при осуществлении деятельности по проведению лотерей'), 19);
  AddCodeExLine(_('Перевод ККТ из режима, не позволяющего применять ККТ при приеме денежных средств при реализации лотерейных билетов, ' + 'электронных лотерейных билетов, приеме лотерейных ставок и выплате денежных средств в виде выигрыша при осуществлении ' + 'деятельности по проведению лотерей, в режим, позволяющий применять ККТ при приеме денежных средств при реализации ' + 'лотерейных билетов, электронных лотерейных билетов, приеме лотерейных ставок и выплате денежных средств в виде выигрыша ' + 'при осуществлении деятельности по проведению лотерей'), 20);
  AddCodeExLine(_('Изменение версии ФФД'), 21);
  AddCodeExLine(_('Иные причины'), 31);
end;

end.
