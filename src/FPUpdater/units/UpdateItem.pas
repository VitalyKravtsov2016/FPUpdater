unit UpdateItem;

interface

uses
  // VCL
  System.SysUtils, System.Variants, System.Classes, System.JSON,
  System.Generics.Collections, System.IOUtils;

const
  /////////////////////////////////////////////////////////////////////////////
  /// Action values
  // Update loader
  ACTION_UPDATE_LOADER    = 1;
  // Update firmware
  ACTION_UPDATE_FIRMWARE  = 2;
  // Write licenses
  ACTION_WRITE_LICENSES   = 3;

type
  { TFFDNeedUpdate }

  TFFDNeedUpdate = (NoUpdateNeeded, FFD105, FFD12);

  { TUpdateParams }

  TUpdateParams = record
    FFDMarking: Boolean;
    FFDPawnshop: Boolean;
    FFDInsurance: Boolean;
    DocumentSentTimeoutInMin: Integer;
    SaveTables: Boolean;                // Восстанавливать значения таблиц
    RestoreCashRegister: Boolean;       // Восстанавливать регистр наличных
    PrintStatus: Boolean;               // Печатать на чековой ленте
    ServerKM: string;                   // Сервер КМ
    PortKM: Integer;                    // Порт сервера КМ
    FFDNeedUpdate: TFFDNeedUpdate;      // Нужно ли перерегистрировать ФН
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

  TUpdateItem = record
    Action: Integer;        // Действие,
                            // 1 - обновить загрузчик,
                            // 2 - обновить программу,
                            // 3 - записать лицензии
    FileName: string;       // Имя файла
    RestoreTables: Boolean; // Нужно ли восстановить значения таблиц
    CurrBootVer: Integer;   // Версия загрузчика устройства
    NewBootVer: Integer;    // Версия загрузчика в файле
    firmware: string;       // ???
    build: Integer;         // ???
    date: TDate;            // ???
    info: string;           // ???
    fwver: string;          // Версия ПО ФР, например 'T.3'
    fwbuild: Integer;       // Сборка ПО ФР, например '7052'
    fwdate: TDate;          // Дата ПО ФР, например '2025-10-31'
    Tables: TTableItems;    // Значения таблиц
    DownAllowed: Boolean;   // ???
  end;
  TUpdateItems = array of TUpdateItem;

procedure UpdateParamsLoadFromFile(const FileName: string; var Params: TUpdateParams);
procedure UpdateItemsLoadFromFile(const FileName: string; var Items: TUpdateItems);

implementation

function JsonGetString(Value: TJSONValue; const APath: string): string;
begin
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

function JsonGetInteger(Value: TJSONValue; const APath: string; DefValue: Integer): Integer; overload;
begin
  Result := StrToIntDef(JsonGetString(Value, APath), DefValue);
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

procedure ReadUpdateItem(Json: TJSONObject; var Item: TUpdateItem);
var
  i: Integer;
  Table: TJSONValue;
  Tables: TJSONValue;
  JSONArray: TJSONArray;
begin
  // Сначала обязательные
  Item.Action := JsonGetInteger(Json, 'level');
  if Item.Action = ACTION_UPDATE_LOADER then
  begin
    Item.FileName := JsonGetString(Json, 'file');
    Item.CurrBootVer := JsonGetInteger(Json, 'bootloader');
    // Затем необязательные
    Item.RestoreTables := False;
    if Json.FindValue('savesetting') <> nil then
      Item.RestoreTables := JsonGetBoolean(Json, 'savesetting');

    Item.NewBootVer := -1;
    if Json.FindValue('blver')<>nil then
      Item.NewBootVer := JsonGetInteger(Json, 'blver');

    Item.info := 'нет данных';
    if Json.FindValue('info')<>nil then
      Item.info := JsonGetString(Json, 'info');
  end;

  if Item.Action = ACTION_WRITE_LICENSES then
  begin
    Item.FileName := JsonGetString(Json, 'file');
  end;

  if Item.Action = ACTION_UPDATE_FIRMWARE then
  begin
    Item.FileName := JsonGetString(Json, 'file');
    Item.CurrBootVer := JsonGetInteger(Json, 'bootloader');

    Item.Build := 0;
    if Json.FindValue('build') <> nil then
      Item.Build := JsonGetInteger(Json, 'build');

    Tables := Json.FindValue('Tables');
    if (Tables <> nil) and (Tables is TJSONArray) then
    begin
      JSONArray := Tables as TJSONArray;
      SetLength(Item.Tables, JSONArray.Count);
      for i := 0 to JSONArray.Count-1 do
      begin
        Table := JSONArray[i];
        Item.Tables[i].Table := Table.GetValue<Integer>('table');
        Item.Tables[i].Row := Table.GetValue<Integer>('row');
        Item.Tables[i].Field := Table.GetValue<Integer>('field');
        Item.Tables[i].FieldType := Table.GetValue<Integer>('type');
        if Item.Tables[i].FieldType = 0 then
          Item.Tables[i].intvalue := Table.GetValue<Integer>('value')
        else
          Item.Tables[i].strvalue := Table.GetValue<String>('value');
      end;
    end;
    // Затем необязательные
    Item.RestoreTables := False;
    if Json.FindValue('savesetting') <> nil then
      Item.RestoreTables := JsonGetBoolean(Json, 'savesetting');

    Item.firmware := '__';
    if Json.FindValue('firmware') <> nil then
      Item.firmware := JsonGetString(Json, 'firmware');

    Item.Date := 0;
    if Json.FindValue('date') <> nil then
      Item.Date := JsonStrToDate(Json.GetValue<String>('date'));

    Item.NewBootVer := -1;
    if Json.FindValue('blver')<>nil then
      Item.NewBootVer := JsonGetInteger(Json, 'blver');

    Item.fwver := 'неизвестно';
    if Json.FindValue('fwver')<>nil then
      Item.fwver := JsonGetString(Json, 'fwver');

    Item.fwbuild := -1;
    if Json.FindValue('fwbuild')<>nil then
      Item.fwbuild := JsonGetInteger(Json, 'fwbuild');

    Item.fwdate := 0;
    if Json.FindValue('fwdate')<>nil then
      Item.fwdate := JsonStrToDate(Json.GetValue<String>('fwdate'));

    Item.info := 'нет данных';
    if Json.FindValue('info')<>nil then
      Item.info := JsonGetString(Json, 'info');

    Item.downallowed := false;
    if Json.FindValue('downallowed')<>nil then
      Item.downallowed := JsonGetBoolean(Json, 'downallowed');
  end;
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
      Params.FFDMarking := JsonGetBoolean(JSONValue, 'FFDMarking');
      Params.FFDPawnshop := JsonGetBoolean(JSONValue, 'FFDPawnshop');
      Params.FFDInsurance := JsonGetBoolean(JSONValue, 'FFDInsurance');
      Params.DocumentSentTimeoutInMin := JsonGetInteger(JSONValue, 'DocumentSentTimeoutInMin');
      Params.SaveTables := JsonGetBoolean(JSONValue, 'SaveTables');
      Params.RestoreCashRegister := JsonGetBoolean(JSONValue, 'RestoreCashRegister');
      Params.PrintStatus := JsonGetBoolean(JSONValue, 'PrintStatus');
      Params.ServerKM := JsonGetString(JSONValue, 'ServerKM');
      Params.PortKM := JsonGetInteger(JSONValue, 'PortKM');
      Params.FFDNeedUpdate := TFFDNeedUpdate(JsonGetInteger(JSONValue, 'FFDNeedUpdate'));
    end;
  finally
    JSONObject.Free;
  end;
end;

// Чтение файла json
procedure UpdateItemsLoadFromFile(const FileName:string; var Items: TUpdateItems);
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
    JSONValue := JSONObject.FindValue('RULES');
    if (JSONValue <> nil) and (JSONValue is TJSONArray) then
    begin
      JSONArray := JSONValue as TJSONArray;
      SetLength(Items, JSONArray.Count);
      for i := 0 to JSONArray.Count-1 do
      begin
        ReadUpdateItem(TJSONObject(JSONArray.Items[i]), Items[i]);
      end;
    end;
  finally
    JSONObject.Free;
  end;
end;

end.
