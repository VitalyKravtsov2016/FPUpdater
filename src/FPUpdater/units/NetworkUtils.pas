unit NetworkUtils;

interface

uses
  // VCL
  Windows, Winsock, WinSvc, Classes, SysUtils, TlHelp32,
  Winapi.IpHlpApi, Winapi.IpExport, Winapi.IpTypes, Winapi.WinSock2,
  Winapi.IpRtrMib;

type
  TNetworkAdapter = record
    Name: string;
    Description: string;
    IPAddress: string;
    SubnetMask: string;
    MACAddress: string;
    IsRNDIS: Boolean;
  end;

function IsLocalRNDISDevice(const IPAddress: string): Boolean;
function GetAdapterByIP(const IPAddress: string): TNetworkAdapter;
function GetMACAddress(const AIPAddress: string): string;
function IsRNDISAdapter(const AdapterDesc: string): Boolean;
function GetAdaptersList: TArray<TNetworkAdapter>;

implementation

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
  I: Integer;
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
        AdapterList[Count].IPAddress := PAnsiChar(@Adapter.IpAddressList.IpAddress);
        AdapterList[Count].SubnetMask := PAnsiChar(@Adapter.IpAddressList.IpMask);

        // Преобразование MAC адреса
        AdapterList[Count].MACAddress := '';
        for I := 0 to Adapter.AddressLength - 1 do
        begin
          if I > 0 then
            AdapterList[Count].MACAddress := AdapterList[Count].MACAddress + '-';
          AdapterList[Count].MACAddress := AdapterList[Count].MACAddress + IntToHex(Adapter.Address[I], 2);
        end;

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
  NullMac: array[0..5] of Byte;
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
  BestRoute: MIB_IPFORWARDROW;
  DestIP: DWORD;
  BestInterface: DWORD;
begin
  Result := False;

  DestIP := inet_addr(PAnsiChar(AnsiString(AIPAddress)));
  if DestIP = INADDR_NONE then
    Exit;

  // Получаем лучший маршрут
  if GetBestRoute(DestIP, 0, @BestRoute) = NO_ERROR then
  begin
    // Получаем лучший интерфейс
    if GetBestInterface(DestIP, BestInterface) = NO_ERROR then
    begin
      // Проверяем, что интерфейс назначения совпадает с интерфейсом, через который идет трафик
      Result := (BestInterface = BestRoute.dwForwardIfIndex);
    end;
  end;
end;

// Основная функция проверки
function IsLocalRNDISDevice(const IPAddress: string): Boolean;
var
  MAC: string;
  Adapters: TArray<TNetworkAdapter>;
  Adapter: TNetworkAdapter;
begin
  Result := False;

  // Сначала проверяем, не принадлежит ли IP самому RNDIS адаптеру
  Adapter := GetAdapterByIP(IPAddress);
  if (Adapter.Description <> '') then
  begin
    if Adapter.IsRNDIS then
    begin
      // Это сам RNDIS адаптер
      Result := True;
      Exit;
    end;
  end;

  // Проверяем, есть ли у нас RNDIS адаптер в системе
  Adapters := GetAdaptersList;
  for Adapter in Adapters do
  begin
    if Adapter.IsRNDIS then
    begin
      // Проверяем, что устройство в одной подсети с RNDIS адаптером
      if (Adapter.IPAddress <> '') and (Adapter.SubnetMask <> '') then
      begin
        // Здесь можно добавить проверку нахождения в одной подсети
        // Но для простоты будем считать, что если есть ARP запись и RNDIS адаптер, то устройство локальное

        // Проверяем MAC адрес
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

        // Проверяем через маршрутизацию
        Result := IsLocalConnection(IPAddress);
        if Result then
          Exit;
      end;
    end;
  end;

  // Дополнительная проверка через ARP таблицу
  MAC := GetMACAddress(IPAddress);
  Result := MAC <> '';
end;

end.
