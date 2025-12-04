unit UpdateItem;

interface

uses
  // VCL
  System.SysUtils, System.Variants, System.Classes, System.JSON,
  System.Generics.Collections, System.IOUtils,
  // This
  LogFile;

const
  /////////////////////////////////////////////////////////////////////////////
  /// SigningKey values
  ///
  SigningKeyUnknown         = 0; // инфраструктура неизвестна
  SigningKeyTehnoTest       = 1; // инфраструктура tehno test
  SigningKeyTehnoWork       = 2; // инфраструктура tehno work
  SigningKeyShtrihWork      = 3; // инфраструктура shtrih
  SigningKeyShtrihInter     = 4; // инфраструктура shtrih промежуточная

  /////////////////////////////////////////////////////////////////////////////
  /// Виды операций
  ///
  // Обновление загрузчика
  ACTION_UPDATE_LOADER    = 1;

  // Обновление программы
  ACTION_UPDATE_FIRMWARE  = 2;

  // Запись лицензий
  ACTION_WRITE_LICENSE    = 3;

  // Инициализация тестового ФН
  ACTION_INIT_FS          = 4;

  // Фискализация ФН
  ACTION_FISCALIZE_FS     = 5;

  // Запись таблиц
  ACTION_WRITE_TABLES     = 6;

  // Перерегистрация ФН
  ACTION_REFISCALIZE_FS   = 7;


type
  { TUpdateParams }

  TUpdateParams = record
    SaveTables: Boolean;                // Восстанавливать значения таблиц
    PrintStatus: Boolean;               // Печатать на чековой ленте
    DocSentTimeoutInSec: Integer;       // Таймаут отправки документов в ОФД
    RestoreCashRegister: Boolean;       // Восстанавливать регистр наличных
  end;


  { TTableItem }

  TTableItem = record
    Table: Integer;
    Row: Integer;
    Field: Integer;
    FieldType: Integer;
    IntValue: Integer;
    StrValue: string;
  end;
  TTableItems = array of TTableItem;

  { TUpdateItem }

  TUpdateItem = class
  private
    FAction: Integer;
    FInfo: string;
  public
    procedure CheckFileExists(const Path: string); virtual;
    property Action: Integer read FAction;
    property Info: string read FInfo;
  end;

  TActionWriteLicense = class(TUpdateItem)
  private
    FFileName: string;
  public
    procedure CheckFileExists(const Path: string); override;
    property FileName: string read FFileName;
  end;

  { TActionUpdateLoader }

  TActionUpdateLoader = class(TUpdateItem)
  private
    FFileName: string;     // Имя файла
    FCurrBootVer: Integer; // Версия загрузчика устройства
    FNewBootVer: Integer;  // Версия загрузчика в файле
    FForce: Boolean;       // Загружать если равны версии
    FSigningKey: Integer;  // Тип ключей для подписи
  public
    procedure CheckFileExists(const Path: string); override;
    property FileName: string read FFileName;
    property CurrBootVer: Integer read FCurrBootVer;
    property NewBootVer: Integer read FNewBootVer;
    property Force: Boolean read FForce;
    property SigningKey: Integer read FSigningKey;
  end;

  { TActionUpdateFirmware }

  TActionUpdateFirmware = class(TUpdateItem)
  private
    FInfo: string;            // Описание операции
    FFileName: string;        // Имя файла
    FCurrBootVer: Integer;    // Версия загрузчика устройства
    FNewBootVer: Integer;     // Версия загрузчика в файле
    FForce: Boolean;          // Загружать если равны версии
    FSigningKey: Integer;     // Тип ключей для подписи
    FVersion: string;         // Версия ПО ФР, например 'T.3'
    FBuild: Integer;          // Сборка ПО ФР, например '7052'
    FDate: TDate;             // Дата ПО ФР, например '2025-10-31'
    FRestoreTables: Boolean;  // Нужно ли восстановить значения таблиц
  public
    procedure CheckFileExists(const Path: string); override;
    property Info: string read FInfo;
    property Force: Boolean read FForce;
    property FileName: string read FFileName;
    property CurrBootVer: Integer read FCurrBootVer;
    property NewBootVer: Integer read FNewBootVer;
    property SigningKey: Integer read FSigningKey;
    property Version: string read FVersion;
    property Build: Integer read FBuild;
    property Date: TDate read FDate;
    property RestoreTables: Boolean read FRestoreTables;
  end;

  { TActionInitFS }

  TActionInitFS = class(TUpdateItem)
  end;

  { TActionInitFS }

  TActionFiscalizeFS = class(TUpdateItem)
  private
    FInn: string;           // ИНН
    FTaxType: Integer;      // Тип налогообложения
    FWorkMode: Integer;     // Режим работы
    FWorkModeEx: Integer;   // Расширенный режим работы
    FRegNumber: string;     // Регистрационный номер
    FFfdVersion: Integer;     // Версия ФФД, на которую фискализировать
  public
    property Inn: string read FInn;
    property TaxType: Integer read FTaxType;
    property WorkMode: Integer read FWorkMode;
    property WorkModeEx: Integer read FWorkModeEx;
    property RegNumber: string read FRegNumber;
    property FfdVersion: Integer read FFfdVersion;
  end;

  { TActionRefiscalizeFS }

  TActionRefiscalizeFS = class(TUpdateItem)
  private
    FInn: string;             // ИНН
    FTaxType: Integer;        // Тип налогообложения
    FWorkMode: Integer;       // Режим работы
    FWorkModeEx: Integer;     // Расширенный режим работы
    FRegNumber: string;       // Регистрационный номер
    FFfdVersion: Integer;     // Версия ФФД, на которую перерегистрировать
    FRegReasonCode: Integer;  // Код причины перерегистрации
    FRegReasonCodeEx: Integer; // Расширеный код причины перерегистрации
  public
    property Inn: string read FInn;
    property TaxType: Integer read FTaxType;
    property WorkMode: Integer read FWorkMode;
    property WorkModeEx: Integer read FWorkModeEx;
    property RegNumber: string read FRegNumber;
    property FfdVersion: Integer read FFfdVersion;
    property RegReasonCode: Integer read FRegReasonCode;
    property RegReasonCodeEx: Integer read FRegReasonCodeEx;
  end;

  { TActionWriteTables }

  TActionWriteTables = class(TUpdateItem)
  private
    FTables: TTableItems;    // Значения таблиц
  public
    property Tables: TTableItems read FTables;
  end;

  TUpdateItems = TObjectList<TUpdateItem>;

  { TJsonreader }

  TJsonreader = class
  private
    FItems: TUpdateItems;

    procedure LoadItemJson(Json: TJSONObject);
    procedure LoadItemInitFS(Json: TJSONObject);
    procedure LoadItemWriteTables(Json: TJSONObject);
    procedure LoadItemFiscalizeFS(Json: TJSONObject);
    procedure LoadItemRefiscalizeFS(Json: TJSONObject);
    procedure LoadItemWriteLicense(Json: TJSONObject);
    function LoadTables(Json: TJSONValue): TTableItems;
    function GetJsonString(Json: TJSONObject; const Name: string): string;
    procedure LoadItemUpdateLoader(Json: TJSONObject);
    procedure LoadItemUpdateFirmware(Json: TJSONObject);
  public
    constructor Create(AItems: TUpdateItems);
    procedure LoadFromFile(const FileName: string);
  end;

procedure UpdateParamsLoadFromFile(const FileName: string; var Params: TUpdateParams);
procedure UpdateItemsLoadFromFile(const FileName: string; Items: TUpdateItems);

implementation

procedure CheckFile(const FileName: string);
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('Файл не найден, "%s"', [FileName]);
end;

procedure UpdateItemsLoadFromFile(const FileName: string; Items: TUpdateItems);
var
  Reader: TJsonReader;
begin
  Reader := TJsonReader.Create(Items);
  try
    Reader.LoadFromFile(FileName);
  finally
    Reader.Free;
  end;
end;

function JsonGetString(Value: TJSONValue; const APath: string): string;
begin
  Result := '';
  if Value.FindValue(APath) <> nil then
    Result := Value.GetValue<String>(APath);
end;

function JsonGetBoolean(Value: TJSONValue; const APath: string): Boolean;
begin
  Result := Value.GetValue<Boolean>(APath);
end;

function JsonGetInteger(Value: TJSONValue; const APath: string): Integer; overload;
begin
  Result := Value.GetValue<Integer>(APath);
end;

function JsonGetInteger(Json: TJSONValue; const APath: string; DefValue: Integer): Integer; overload;
begin
  Result := DefValue;
  if Json.FindValue(APath) <> nil then
    Result := StrToIntDef(Json.GetValue<String>(APath), DefValue);
end;

// 'YYYY-MM-DD','-'
function JsonStrToDate(const DateStr: string): TDateTime;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DateSeparator := '-';
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  Result := StrToDate(DateStr, FormatSettings);
end;

// Чтение файла json
procedure UpdateParamsLoadFromFile(const FileName: string;
  var Params: TUpdateParams);
var
  i: Integer;
  JSONText: string;
  JSONObject: TJSONObject;
  JSONValue: TJSONValue;
begin
  JSONText := TFile.ReadAllText(FileName, TEncoding.UTF8);
  JSONObject := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;
  try
    JSONValue := JSONObject.FindValue('Params');
    if JSONValue <> nil then
    begin
      Params.SaveTables := JsonGetBoolean(JSONValue, 'SaveTables');
      Params.PrintStatus := JsonGetBoolean(JSONValue, 'PrintStatus');
      Params.DocSentTimeoutInSec := JsonGetInteger(JSONValue, 'DocSentTimeoutInSec');
      Params.RestoreCashRegister := JsonGetBoolean(JSONValue, 'RestoreCashRegister');
    end;
  finally
    JSONObject.Free;
  end;
end;

{ TJsonReader }

constructor TJsonReader.Create(AItems: TUpdateItems);
begin
  inherited Create;
  FItems := AItems;
end;

procedure TJsonReader.LoadFromFile(const FileName: string);
var
  i: Integer;
  JSONText: string;
  JSONObject: TJSONObject;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
begin
  JSONText := TFile.ReadAllText(FileName, TEncoding.UTF8);
  JSONObject := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;
  try
    JSONValue := JSONObject.FindValue('actions');
    if (JSONValue <> nil) and (JSONValue is TJSONArray) then
    begin
      JSONArray := JSONValue as TJSONArray;
      for i := 0 to JSONArray.Count-1 do
      begin
        LoadItemJson(TJSONObject(JSONArray.Items[i]));
      end;
    end;
  finally
    JSONObject.Free;
  end;
end;

procedure TJsonReader.LoadItemJson(Json: TJSONObject);
var
  ActionId: Integer;
begin
  try
    ActionId := JsonGetInteger(Json, 'action');
    case ActionId of
      ACTION_UPDATE_LOADER: LoadItemUpdateLoader(Json);
      ACTION_UPDATE_FIRMWARE: LoadItemUpdateFirmware(Json);
      ACTION_WRITE_LICENSE: LoadItemWriteLicense(Json);
      ACTION_INIT_FS: LoadItemInitFS(Json);
      ACTION_FISCALIZE_FS: LoadItemFiscalizeFS(Json);
      ACTION_REFISCALIZE_FS: LoadItemRefiscalizeFS(Json);
      ACTION_WRITE_TABLES: LoadItemWriteTables(Json);
    end;
  except
    on E: Exception do
    begin
      Logger.Error('LoadItemJson: ' + E.Message);
    end;
  end;
end;

function TJsonReader.GetJsonString(Json: TJSONObject; const Name: string): string;
begin
  Result := '';
  if Json.FindValue(Name) <> nil then
    Result := JsonGetString(Json, Name);
end;

procedure TJsonReader.LoadItemUpdateLoader(Json: TJSONObject);
var
  Item: TActionUpdateLoader;
begin
  Item := TActionUpdateLoader.Create;
  Item.FAction := ACTION_UPDATE_LOADER;
  Item.FInfo := GetJsonString(Json, 'info');
  Item.FFileName := JsonGetString(Json, 'file');
  Item.FCurrBootVer := JsonGetInteger(Json, 'bootver');

  Item.FNewBootVer := -1;
  if Json.FindValue('newbootver')<>nil then
    Item.FNewBootVer := JsonGetInteger(Json, 'newbootver');

  Item.FForce := false;
  if Json.FindValue('force')<>nil then
    Item.FForce := JsonGetBoolean(Json, 'force');

  Item.FSigningKey := SigningKeyUnknown;
  if Json.FindValue('SigningKey')<>nil then
    Item.FSigningKey := JsonGetInteger(Json, 'SigningKey');

  FItems.Add(Item);
end;

procedure TJsonReader.LoadItemUpdateFirmware(Json: TJSONObject);
var
  Item: TActionUpdateFirmware;
begin
  Item := TActionUpdateFirmware.Create;
  Item.FAction := ACTION_UPDATE_FIRMWARE;
  Item.FInfo := GetJsonString(Json, 'info');
  Item.FFileName := JsonGetString(Json, 'file');
  Item.FCurrBootVer := JsonGetInteger(Json, 'bootver');

  Item.FNewBootVer := -1;
  if Json.FindValue('newbootver')<>nil then
    Item.FNewBootVer := JsonGetInteger(Json, 'newbootver');

  Item.FForce := false;
  if Json.FindValue('force')<>nil then
    Item.FForce := JsonGetBoolean(Json, 'force');

  Item.FSigningKey := SigningKeyUnknown;
  if Json.FindValue('SigningKey')<>nil then
    Item.FSigningKey := JsonGetInteger(Json, 'SigningKey');

  // Затем необязательные
  Item.FRestoreTables := False;
  if Json.FindValue('savesetting') <> nil then
    Item.FRestoreTables := JsonGetBoolean(Json, 'savesetting');

  Item.FDate := 0;
  if Json.FindValue('date') <> nil then
    Item.FDate := JsonStrToDate(Json.GetValue<String>('date'));

  Item.FVersion := 'неизвестно';
  if Json.FindValue('fwver')<>nil then
    Item.FVersion := JsonGetString(Json, 'fwver');

  Item.FBuild := -1;
  if Json.FindValue('fwbuild')<>nil then
    Item.FBuild := JsonGetInteger(Json, 'fwbuild');

  Item.FDate := 0;
  if Json.FindValue('fwdate')<>nil then
    Item.FDate := JsonStrToDate(Json.GetValue<String>('fwdate'));

  FItems.Add(Item);
end;

procedure TJsonReader.LoadItemWriteLicense(Json: TJSONObject);
var
  Item: TActionWriteLicense;
begin
  Item := TActionWriteLicense.Create;
  Item.FAction := ACTION_WRITE_LICENSE;
  Item.FInfo := GetJsonString(Json, 'info');
  Item.FFileName := JsonGetString(Json, 'file');
  FItems.Add(Item);
end;

procedure TJsonReader.LoadItemInitFS(Json: TJSONObject);
var
  Item: TActionInitFS;
begin
  Item := TActionInitFS.Create;
  Item.FAction := ACTION_INIT_FS;
  Item.FInfo := GetJsonString(Json, 'info');
  FItems.Add(Item);
end;

procedure TJsonReader.LoadItemFiscalizeFS(Json: TJSONObject);
var
  Item: TActionFiscalizeFS;
begin
  Item := TActionFiscalizeFS.Create;
  Item.FAction := ACTION_FISCALIZE_FS;
  Item.FInfo := GetJsonString(Json, 'info');
  Item.FInn := JsonGetString(Json, 'inn');
  Item.FTaxType := JsonGetInteger(Json, 'TaxType');
  Item.FWorkMode := JsonGetInteger(Json, 'WorkMode');
  Item.FWorkModeEx := JsonGetInteger(Json, 'WorkModeEx');
  Item.FFfdVersion := JsonGetInteger(Json, 'FfdVersion', 2);

  Item.FRegNumber := '';
  if Json.FindValue('RegNumber') <> nil then
    Item.FRegNumber := JsonGetString(Json, 'RegNumber');

  FItems.Add(Item);
end;

procedure TJsonReader.LoadItemRefiscalizeFS(Json: TJSONObject);
var
  Item: TActionRefiscalizeFS;
begin
  Item := TActionRefiscalizeFS.Create;
  Item.FAction := ACTION_REFISCALIZE_FS;
  Item.FInfo := GetJsonString(Json, 'info');
  Item.FInn := JsonGetString(Json, 'inn');
  Item.FTaxType := JsonGetInteger(Json, 'TaxType', 0);
  Item.FWorkMode := JsonGetInteger(Json, 'WorkMode', 0);
  Item.FWorkModeEx := JsonGetInteger(Json, 'WorkModeEx', 0);
  Item.FRegReasonCode := JsonGetInteger(Json, 'RegReasonCode', 0);
  Item.FRegReasonCodeEx := JsonGetInteger(Json, 'RegReasonCodeEx', 0);
  Item.FFfdVersion := JsonGetInteger(Json, 'FfdVersion', 4);

  Item.FRegNumber := '';
  if Json.FindValue('RegNumber') <> nil then
    Item.FRegNumber := JsonGetString(Json, 'RegNumber');

  FItems.Add(Item);
end;

procedure TJsonReader.LoadItemWriteTables(Json: TJSONObject);
var
  Item: TActionWriteTables;
begin
  Item := TActionWriteTables.Create;

  Item.FAction := ACTION_WRITE_TABLES;
  Item.FInfo := GetJsonString(Json, 'info');
  Item.FTables := LoadTables(Json.FindValue('tables'));
  FItems.Add(Item);
end;

function TJsonReader.LoadTables(Json: TJSONValue): TTableItems;
var
  i: Integer;
  Table: TJSONValue;
  JSONArray: TJSONArray;
begin
  if (Json <> nil) and (Json is TJSONArray) then
  begin
    JSONArray := Json as TJSONArray;
    SetLength(Result, JSONArray.Count);
    for i := 0 to JSONArray.Count-1 do
    begin
      Table := JSONArray[i];
      Result[i].Table := Table.GetValue<Integer>('table');
      Result[i].Row := Table.GetValue<Integer>('row');
      Result[i].Field := Table.GetValue<Integer>('field');
      Result[i].FieldType := Table.GetValue<Integer>('type');
      if Result[i].FieldType = 0 then
        Result[i].intvalue := Table.GetValue<Integer>('value')
      else
        Result[i].strvalue := Table.GetValue<String>('value');
    end;
  end;
end;

{ TActionWriteLicense }

procedure TActionWriteLicense.CheckFileExists(const Path: string);
begin
  CheckFile(Path + FileName);
end;

{ TUpdateItem }

procedure TUpdateItem.CheckFileExists(const Path: string);
begin
end;

{ TActionUpdateLoader }

procedure TActionUpdateLoader.CheckFileExists(const Path: string);
begin
  CheckFile(Path + FileName);
end;

{ TActionUpdateFirmware }

procedure TActionUpdateFirmware.CheckFileExists(const Path: string);
begin
  CheckFile(Path + FileName);
end;

(*

    TaxType: Integer;                   // Тип налогообложения
    WorkMode: Integer;                  // Режим работы
    WorkModeEx: Integer;

      Params.FFDNeedUpdate := TFFDNeedUpdate(JsonGetInteger(JSONValue, 'FFDNeedUpdate'));
      Params.TaxType := JsonGetInteger(JSONValue, 'TaxType', 0);
      Params.WorkMode := JsonGetInteger(JSONValue, 'WorkMode', 0);
      Params.WorkModeEx := JsonGetInteger(JSONValue, 'WorkModeEx', 0);
      Params.RegReasonCode := JsonGetInteger(JSONValue, 'RegReasonCode', 0);
      Params.RegReasonCodeEx := JsonGetInteger(JSONValue, 'RegReasonCodeEx', 0);


*)


end.
