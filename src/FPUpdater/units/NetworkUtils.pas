unit NetworkUtils;

interface

uses
  // VCL
  Windows, Winsock, WinSvc, Classes, SysUtils, TlHelp32,
  Winapi.IpHlpApi, Winapi.IpExport, Winapi.IpTypes, Winapi.WinSock2,
  Winapi.IpRtrMib,
  // This
  LogFile;

type
  TNetworkAdapter = record
    Name: string;
    Description: string;
    IPAddress: string;
    SubnetMask: string;
    IsRNDIS: Boolean;
  end;

function IsLocalRNDISDevice(const IPAddress: string): Boolean;
function GetAdapterByIP(const IPAddress: string): TNetworkAdapter;
function GetMACAddress(const AIPAddress: string): string;
function IsRNDISAdapter(const AdapterDesc: string): Boolean;
function GetAdaptersList: TArray<TNetworkAdapter>;

implementation

// Вспомогательная функция для конвертации DWORD в строку IP
function IPAddrToStr(Addr: DWORD): string;
var
  IP: TInAddr;
begin
  IP.S_addr := Addr;
  Result := string(inet_ntoa(IP));
end;

// Проверка, является ли адаптер RNDIS
function IsRNDISAdapter(const AdapterDesc: string): Boolean;
const
  RNDISKeywords: array[0..4] of string = ('RNDIS', 'USB', 'ETHERNET', 'GADGET', 'REMOTE');
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(RNDISKeywords) do
  begin
    if Pos(UpperCase(RNDISKeywords[i]), UpperCase(AdapterDesc)) > 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// Получение списка всех сетевых адаптеров
function GetAdaptersList: TArray<TNetworkAdapter>;
var
  AdaptersInfo: PIP_ADAPTER_INFO;
  Adapter: PIP_ADAPTER_INFO;
  BufferSize: ULONG;
  Status: DWORD;
  AdapterList: TArray<TNetworkAdapter>;
  Count: Integer;
begin
  SetLength(AdapterList, 0);
  Count := 0;
  
  BufferSize := 0;
  GetAdaptersInfo(nil, BufferSize);
  
  if BufferSize = 0 then
    Exit;
    
  GetMem(AdaptersInfo, BufferSize);
  try
    Status := GetAdaptersInfo(AdaptersInfo, BufferSize);
    if Status = ERROR_SUCCESS then
    begin
      Adapter := AdaptersInfo;
      while Assigned(Adapter) do
      begin
        SetLength(AdapterList, Count + 1);
        AdapterList[Count].Name := string(Adapter.AdapterName);
        AdapterList[Count].Description := string(Adapter.Description);
        AdapterList[Count].IPAddress := PAnsiChar(@Adapter.IpAddressList.IpAddress.S);
        AdapterList[Count].SubnetMask := PAnsiChar(@Adapter.IpAddressList.IpMask.S);
        AdapterList[Count].IsRNDIS := IsRNDISAdapter(AdapterList[Count].Description);
        
        Inc(Count);
        Adapter := Adapter.Next;
      end;
    end;
  finally
    FreeMem(AdaptersInfo);
  end;
  
  Result := AdapterList;
end;

// Получение адаптера по IP адресу
function GetAdapterByIP(const IPAddress: string): TNetworkAdapter;
var
  Adapters: TArray<TNetworkAdapter>;
  Adapter: TNetworkAdapter;
begin
  Result.Description := '';
  Adapters := GetAdaptersList;

  for Adapter in Adapters do
  begin
    if Adapter.IPAddress = IPAddress then
    begin
      Result := Adapter;
      Exit;
    end;
  end;
end;

// Получение MAC адреса по IP из ARP таблицы
function GetMACAddress(const AIPAddress: string): string;
var
  DestIP: DWORD;
  MACAddr: array[0..5] of Byte;
  ARPTable: PMIB_IPNETTABLE;
  BufferSize: DWORD;
  i: Integer;
  Status: DWORD;
  NullMac: array[0..5] of Byte;  // Добавляем константный массив для сравнения
begin
  Result := '';

  // Конвертируем IP адрес
  DestIP := inet_addr(PAnsiChar(AnsiString(AIPAddress)));
  if DestIP = INADDR_NONE then
    Exit;

  // Инициализируем нулевой MAC для сравнения
  FillChar(NullMac, SizeOf(NullMac), 0);

  // Получаем необходимый размер буфера
  BufferSize := 0;
  Status := GetIpNetTable(nil, BufferSize, False);
  if Status <> ERROR_INSUFFICIENT_BUFFER then
    Exit;

  // Выделяем память под ARP таблицу
  GetMem(ARPTable, BufferSize);
  try
    Status := GetIpNetTable(ARPTable, BufferSize, False);
    if Status = NO_ERROR then
    begin
      // Ищем запись с нужным IP
      for i := 0 to ARPTable.dwNumEntries - 1 do
      begin
        if ARPTable.table[i].dwAddr = DestIP then
        begin
          // Проверяем, что MAC адрес существует (не все нули)
          // Исправлено: используем NullMac вместо @[0,0,0,0,0,0]
          if not CompareMem(@ARPTable.table[i].bPhysAddr, @NullMac, 6) then
          begin
            Move(ARPTable.table[i].bPhysAddr, MACAddr, 6);
            Result := Format('%.2X:%.2X:%.2X:%.2X:%.2X:%.2X',
              [MACAddr[0], MACAddr[1], MACAddr[2],
               MACAddr[3], MACAddr[4], MACAddr[5]]);
            Break;
          end;
        end;
      end;
    end;
  finally
    FreeMem(ARPTable);
  end;
end;

// Проверка, локально ли подключено устройство
function IsLocalConnection(const AIPAddress: string): Boolean;
var
  BestRoute: MIB_IPFORWARDROW;  // Правильная структура
  SourceIP: DWORD;
  DestIP: DWORD;
begin
  Result := False;

  DestIP := inet_addr(PAnsiChar(AnsiString(AIPAddress)));
  if DestIP = INADDR_NONE then
    Exit;

  // Получаем лучший маршрут
  if GetBestRoute(DestIP, 0, @BestRoute) = NO_ERROR then
  begin
    // Получаем интерфейс для этого IP
    if GetBestInterface(DestIP, SourceIP) = NO_ERROR then
    begin
      // Сравниваем
      Result := (SourceIP = BestRoute.dwForwardNextHop);

      Logger.Debug(Format('IsLocalConnection: DestIP=%s, SourceIP=%s, NextHop=%s, Result=%s',
        [AIPAddress,
         IPAddrToStr(SourceIP),
         IPAddrToStr(BestRoute.dwForwardNextHop),
         BoolToStr(Result, True)]));
    end;
  end;
end;

// Основная функция проверки
function IsLocalRNDISDevice(const IPAddress: string): Boolean;
var
  Adapter: TNetworkAdapter;
  MAC: string;
begin
  Result := False;

  // 1. Проверяем, что устройство в одной подсети с RNDIS адаптером
  Adapter := GetAdapterByIP(IPAddress);
  if (Adapter.Description <> '') and Adapter.IsRNDIS then
  begin
    Result := True;
    Exit;
  end;

  // 2. Проверяем MAC адрес
  MAC := GetMACAddress(IPAddress);
  if MAC <> '' then
  begin
    // Проверяем характерные для USB/RNDIS MAC префиксы
    if (Copy(MAC, 1, 8) = '02:00:00') or  // Common USB MAC
       (Copy(MAC, 1, 8) = '0A:00:00') or  // Common USB MAC
       (Copy(MAC, 1, 8) = '00:15:83') then // Android devices
    begin
      Result := True;
      Exit;
    end;
  end;
  
  // 3. Проверяем через маршрутизацию
  Result := IsLocalConnection(IPAddress);
end;

end.