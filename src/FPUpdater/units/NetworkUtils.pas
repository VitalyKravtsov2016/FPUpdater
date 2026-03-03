unit NetworkUtils;

interface

uses
  Winapi.Windows, Winapi.Winsock2, System.SysUtils, System.Classes,
  Winapi.IpHlpApi, Winapi.IpExport, Winapi.IpTypes, Winapi.IpRtrMib;

type
  TNetworkAdapter = record
    Name: string;
    Description: string;
    IPAddress: string;
    SubnetMask: string;
    MACAddress: string;
    IsRNDIS: Boolean;
    IsUSB: Boolean;
    IfType: DWORD;
    IfIndex: DWORD;
  end;

  TARPEntry = record
    IPAddress: string;
    MACAddress: string;
    IsDynamic: Boolean;
    IfIndex: DWORD;
  end;

function IsLocalRNDISDevice(const IPAddress: string): Boolean;
function GetAdapterByIP(const IPAddress: string): TNetworkAdapter;
function GetMACAddress(const AIPAddress: string): string;
function GetAdaptersList: TArray<TNetworkAdapter>;
function IsRouteViaRNDIS(const IPAddress: string): Boolean;

// Новые функции для работы с RNDIS устройствами
function FindRNDISDeviceIP: string;                                     // Поиск IP активного RNDIS устройства
function GetAllRNDISDevicesInSubnet: TArray<string>;                  // Получить все устройства в подсети RNDIS
function GetRNDISAdapterInfo(out Adapter: TNetworkAdapter): Boolean;  // Получить информацию о RNDIS адаптере
function GetARPTable: TArray<TARPEntry>;                              // Получить ARP таблицу
function IsSameSubnet(const IP1, IP2, Mask: string): Boolean;         // Проверить в одной ли подсети
function IPDWordToString(IP: DWORD): string;                           // Преобразовать DWORD в IP строку
function StringToIPDWord(const IP: string): DWORD;                     // Преобразовать IP строку в DWORD

implementation

const
  IF_TYPE_USB = 160;
  IF_TYPE_IEEE80211 = 71;
  MAX_ADAPTER_ADDRESS_LENGTH = 8;
  ERROR_INSUFFICIENT_BUFFER = 122;
  NO_ERROR = 0;
  INADDR_NONE = $FFFFFFFF;

// Вспомогательная функция для определения RNDIS по описанию
function IsRNDISAdapter(const Description: string; IfType: DWORD; const MACAddress: string): Boolean;
var
  DescUpper: string;
begin
  Result := False;
  DescUpper := UpperCase(Description);

  // Основные признаки RNDIS в имени
  if (Pos('RNDIS', DescUpper) > 0) or
     (Pos('REMOTE NDIS', DescUpper) > 0) or
     (Pos('USB RNDIS', DescUpper) > 0) or
     (Pos('REMOTE NDIS BASED', DescUpper) > 0) then
  begin
    Result := True;
    Exit;
  end;

  // Признаки для Android устройств (часто используют RNDIS)
  if (Pos('ANDROID', DescUpper) > 0) and
     ((Pos('NETWORK', DescUpper) > 0) or (Pos('ETHERNET', DescUpper) > 0) or (Pos('RNDIS', DescUpper) > 0)) then
  begin
    Result := True;
    Exit;
  end;

  // Признаки для Linux USB gadgets
  if (Pos('GADGET', DescUpper) > 0) and (Pos('USB', DescUpper) > 0) then
  begin
    Result := True;
    Exit;
  end;

  // USB интерфейс с характерными признаками сетевого адаптера
  if (IfType = IF_TYPE_USB) and
     ((Pos('USB', DescUpper) > 0) or
      (Pos('ETHERNET', DescUpper) > 0) or
      (Pos('NETWORK', DescUpper) > 0)) and
     (MACAddress <> '') then
  begin
    Result := True;
    Exit;
  end;

  // Проверка по вендорским префиксам MAC адреса (дополнительная проверка)
  if MACAddress <> '' then
  begin
    // Префиксы MAC адресов, часто используемые для виртуальных и USB интерфейсов
    if (Copy(MACAddress, 1, 8) = '02-00-00') or  // Виртуальные интерфейсы
       (Copy(MACAddress, 1, 8) = '0A-00-00') or  // Linux USB gadgets
       (Copy(MACAddress, 1, 8) = '00-15-83') or  // Некоторые Android
       (Copy(MACAddress, 1, 8) = '00-1C-42') then // Встроенные устройства
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// Получение списка адаптеров
function GetAdaptersList: TArray<TNetworkAdapter>;
var
  AdaptersInfo: PIP_ADAPTER_INFO;
  AdapterInfo: PIP_ADAPTER_INFO;
  BufferSize: ULONG;
  Status: DWORD;
  AdapterList: TArray<TNetworkAdapter>;
  Count: Integer;
  iIndex, iMAC: Integer;
  IpAddrString: PIP_ADDR_STRING;
  AdaptersAddresses: PIP_ADAPTER_ADDRESSES;
  AdapterAddr: PIP_ADAPTER_ADDRESSES;
  WorkBufferSize: ULONG;
  TempIP: AnsiString;
begin
  SetLength(AdapterList, 0);
  Count := 0;

  // Получаем базовую информацию через GetAdaptersInfo
  BufferSize := 0;
  GetAdaptersInfo(nil, BufferSize);

  if BufferSize = 0 then
    Exit;

  AdaptersInfo := AllocMem(BufferSize);
  try
    Status := GetAdaptersInfo(AdaptersInfo, BufferSize);
    if Status = ERROR_SUCCESS then
    begin
      AdapterInfo := AdaptersInfo;
      while Assigned(AdapterInfo) do
      begin
        SetLength(AdapterList, Count + 1);

        // Инициализируем запись
        AdapterList[Count].Name := string(AnsiString(AdapterInfo.AdapterName));
        AdapterList[Count].Description := string(AnsiString(AdapterInfo.Description));
        AdapterList[Count].IfIndex := AdapterInfo.Index;
        AdapterList[Count].MACAddress := '';
        AdapterList[Count].IsRNDIS := False;
        AdapterList[Count].IsUSB := False;
        AdapterList[Count].IfType := 0;
        AdapterList[Count].IPAddress := '';
        AdapterList[Count].SubnetMask := '';

        // Формируем MAC адрес
        if AdapterInfo.AddressLength > 0 then
        begin
          for iMAC := 0 to AdapterInfo.AddressLength - 1 do
          begin
            if iMAC > 0 then
              AdapterList[Count].MACAddress := AdapterList[Count].MACAddress + '-';
            AdapterList[Count].MACAddress := AdapterList[Count].MACAddress + IntToHex(AdapterInfo.Address[iMAC], 2);
          end;
        end;

        // Получаем IP адреса (первый значимый)
        IpAddrString := @AdapterInfo.IpAddressList;
        while Assigned(IpAddrString) do
        begin
          TempIP := AnsiString(IpAddrString.IpAddress.S);
          // Проверяем, что IP не пустой и не 0.0.0.0
          if (TempIP <> '0.0.0.0') and (TempIP <> '') and
             (AdapterList[Count].IPAddress = '') then
          begin
            AdapterList[Count].IPAddress := string(TempIP);
            AdapterList[Count].SubnetMask := string(AnsiString(IpAddrString.IpMask.S));
          end;
          IpAddrString := IpAddrString.Next;
        end;

        Inc(Count);
        AdapterInfo := AdapterInfo.Next;
      end;
    end;
  finally
    FreeMem(AdaptersInfo);
  end;

  // Получаем информацию о типе через GetAdaptersAddresses (Windows XP+)
  WorkBufferSize := 0;
  GetAdaptersAddresses(AF_UNSPEC, 0, nil, nil, @WorkBufferSize);
  if WorkBufferSize > 0 then
  begin
    AdaptersAddresses := AllocMem(WorkBufferSize);
    try
      Status := GetAdaptersAddresses(AF_UNSPEC, 0, nil, AdaptersAddresses, @WorkBufferSize);
      if Status = ERROR_SUCCESS then
      begin
        AdapterAddr := AdaptersAddresses;
        while Assigned(AdapterAddr) do
        begin
          // Ищем соответствующий адаптер по индексу
          for iIndex := 0 to Count - 1 do
          begin
            if AdapterList[iIndex].IfIndex = AdapterAddr.Union.IfIndex then
            begin
              AdapterList[iIndex].IfType := AdapterAddr.IfType;
              AdapterList[iIndex].IsUSB := (AdapterAddr.IfType = IF_TYPE_USB);
              Break;
            end;
          end;
          AdapterAddr := AdapterAddr.Next;
        end;
      end;
    finally
      FreeMem(AdaptersAddresses);
    end;
  end;

  // ОПРЕДЕЛЕНИЕ RNDIS ПО ИМЕНИ (DESCRIPTION) И ДРУГИМ ПРИЗНАКАМ
  for iIndex := 0 to Count - 1 do
  begin
    AdapterList[iIndex].IsRNDIS := IsRNDISAdapter(
      AdapterList[iIndex].Description,
      AdapterList[iIndex].IfType,
      AdapterList[iIndex].MACAddress
    );
  end;

  Result := AdapterList;
end;

// Получение адаптера по IP
function GetAdapterByIP(const IPAddress: string): TNetworkAdapter;
var
  Adapters: TArray<TNetworkAdapter>;
  i: Integer;
begin
  // Инициализируем результат
  Result.Description := '';
  Result.IPAddress := '';
  Result.IsRNDIS := False;
  Result.IfIndex := 0;
  Result.MACAddress := '';
  Result.Name := '';
  Result.SubnetMask := '';
  Result.IsUSB := False;
  Result.IfType := 0;

  Adapters := GetAdaptersList;
  for i := 0 to High(Adapters) do
  begin
    if Trim(Adapters[i].IPAddress) = Trim(IPAddress) then
    begin
      Result := Adapters[i];
      Exit;
    end;
  end;
end;

// Получение MAC адреса из ARP таблицы
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

  DestIP := inet_addr(PAnsiChar(AnsiString(AIPAddress)));
  if Integer(DestIP) = Integer(INADDR_NONE) then
    Exit;

  FillChar(NullMac, SizeOf(NullMac), 0);

  // Получаем размер буфера
  BufferSize := 0;
  Status := GetIpNetTable(nil, BufferSize, False);
  if Status <> ERROR_INSUFFICIENT_BUFFER then
    Exit;

  ARPTable := AllocMem(BufferSize);
  try
    Status := GetIpNetTable(ARPTable, BufferSize, False);
    if Status = NO_ERROR then
    begin
      for i := 0 to ARPTable.dwNumEntries - 1 do
      begin
        if ARPTable.table[i].dwAddr = DestIP then
        begin
          // Проверяем, что MAC адрес не нулевой
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

// Проверка маршрута до IP через RNDIS
function IsRouteViaRNDIS(const IPAddress: string): Boolean;
var
  DestIP: DWORD;
  BestInterface: DWORD;
  Status: DWORD;
  Adapters: TArray<TNetworkAdapter>;
  i: Integer;
begin
  Result := False;

  DestIP := inet_addr(PAnsiChar(AnsiString(IPAddress)));
  if Integer(DestIP) = Integer(INADDR_NONE) then
    Exit;

  // Получаем индекс наилучшего интерфейса для этого IP
  Status := GetBestInterface(DestIP, BestInterface);
  if Status <> NO_ERROR then
    Exit;

  // Получаем список адаптеров и ищем по индексу
  Adapters := GetAdaptersList;
  for i := 0 to High(Adapters) do
  begin
    if (Adapters[i].IfIndex = BestInterface) and Adapters[i].IsRNDIS then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// Основная функция проверки RNDIS устройства
function IsLocalRNDISDevice(const IPAddress: string): Boolean;
var
  Adapter: TNetworkAdapter;
  MAC: string;
  Adapters: TArray<TNetworkAdapter>;
  i: Integer;
begin
  Result := False;

  // Проверка 1: Сам IP принадлежит RNDIS адаптеру
  Adapter := GetAdapterByIP(IPAddress);
  if (Adapter.IPAddress <> '') and Adapter.IsRNDIS then
  begin
    Result := True;
    Exit;
  end;

  // Проверка 2: Маршрут до IP идет через RNDIS адаптер
  if IsRouteViaRNDIS(IPAddress) then
  begin
    Result := True;
    Exit;
  end;

  // Проверка 3: Проверка MAC адреса в ARP таблице
  MAC := GetMACAddress(IPAddress);
  if MAC <> '' then
  begin
    // Получаем список адаптеров
    Adapters := GetAdaptersList;
    for i := 0 to High(Adapters) do
    begin
      // Проверяем, что это RNDIS адаптер и MAC совпадает
      if Adapters[i].IsRNDIS and
         (Adapters[i].MACAddress = StringReplace(MAC, ':', '-', [rfReplaceAll])) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// ============== НОВЫЕ ФУНКЦИИ ==============

// Преобразование DWORD в IP строку
function IPDWordToString(IP: DWORD): string;
var
  ipBytes: array[0..3] of Byte;
begin
  ipBytes[0] := IP and $FF;
  ipBytes[1] := (IP shr 8) and $FF;
  ipBytes[2] := (IP shr 16) and $FF;
  ipBytes[3] := (IP shr 24) and $FF;
  Result := Format('%d.%d.%d.%d', [ipBytes[0], ipBytes[1], ipBytes[2], ipBytes[3]]);
end;

// Преобразование IP строки в DWORD
function StringToIPDWord(const IP: string): DWORD;
var
  Parts: TArray<string>;
  b: array[0..3] of Byte;
begin
  Result := INADDR_NONE;
  Parts := IP.Split(['.']);
  if Length(Parts) = 4 then
  begin
    b[0] := StrToIntDef(Parts[0], 0);
    b[1] := StrToIntDef(Parts[1], 0);
    b[2] := StrToIntDef(Parts[2], 0);
    b[3] := StrToIntDef(Parts[3], 0);
    Result := (b[3] shl 24) or (b[2] shl 16) or (b[1] shl 8) or b[0];
  end;
end;

// Проверка, находятся ли IP адреса в одной подсети
function IsSameSubnet(const IP1, IP2, Mask: string): Boolean;
var
  ip1dw, ip2dw, maskdw: DWORD;
begin
  Result := False;

  ip1dw := StringToIPDWord(IP1);
  ip2dw := StringToIPDWord(IP2);
  maskdw := StringToIPDWord(Mask);

  if (ip1dw <> INADDR_NONE) and (ip2dw <> INADDR_NONE) and (maskdw <> INADDR_NONE) then
  begin
    Result := (ip1dw and maskdw) = (ip2dw and maskdw);
  end;
end;

// Получение ARP таблицы
function GetARPTable: TArray<TARPEntry>;
var
  pIpNetTable: PMibIpNetTable;
  BufLen: ULONG;
  Status: DWORD;
  i: Integer;
  Entries: TArray<TARPEntry>;
begin
  SetLength(Result, 0);

  // Получаем размер буфера
  BufLen := 0;
  Status := GetIpNetTable(nil, BufLen, False);
  if Status <> ERROR_INSUFFICIENT_BUFFER then
    Exit;

  // Выделяем память
  pIpNetTable := AllocMem(BufLen);
  try
    Status := GetIpNetTable(pIpNetTable, BufLen, True);
    if Status <> NO_ERROR then
      Exit;

    SetLength(Entries, pIpNetTable.dwNumEntries);

    for i := 0 to pIpNetTable.dwNumEntries - 1 do
    begin
      Entries[i].IPAddress := IPDWordToString(pIpNetTable.table[i].dwAddr);
      Entries[i].MACAddress := Format('%.2X-%.2X-%.2X-%.2X-%.2X-%.2X',
        [pIpNetTable.table[i].bPhysAddr[0], pIpNetTable.table[i].bPhysAddr[1],
         pIpNetTable.table[i].bPhysAddr[2], pIpNetTable.table[i].bPhysAddr[3],
         pIpNetTable.table[i].bPhysAddr[4], pIpNetTable.table[i].bPhysAddr[5]]);
      Entries[i].IsDynamic := (pIpNetTable.table[i].dwType = MIB_IPNET_TYPE_DYNAMIC);
      Entries[i].IfIndex := pIpNetTable.table[i].dwIndex;
    end;

    Result := Entries;
  finally
    FreeMem(pIpNetTable);
  end;
end;

// Получение информации о RNDIS адаптере
function GetRNDISAdapterInfo(out Adapter: TNetworkAdapter): Boolean;
var
  Adapters: TArray<TNetworkAdapter>;
  i: Integer;
begin
  Result := False;
  Adapters := GetAdaptersList;

  for i := 0 to High(Adapters) do
  begin
    if Adapters[i].IsRNDIS and (Adapters[i].IPAddress <> '') then
    begin
      Adapter := Adapters[i];
      Result := True;
      Exit;
    end;
  end;
end;

// Получение всех устройств в подсети RNDIS
function GetAllRNDISDevicesInSubnet: TArray<string>;
var
  ARPEntries: TArray<TARPEntry>;
  RNDISAdapter: TNetworkAdapter;
  i: Integer;
  Devices: TArray<string>;
begin
  SetLength(Result, 0);

  // Получаем информацию о RNDIS адаптере
  if not GetRNDISAdapterInfo(RNDISAdapter) then
    Exit;

  // Если маска не определена, используем стандартную для Class C
  if RNDISAdapter.SubnetMask = '' then
    RNDISAdapter.SubnetMask := '255.255.255.0';

  // Получаем ARP таблицу
  ARPEntries := GetARPTable;

  SetLength(Devices, 0);

  for i := 0 to High(ARPEntries) do
  begin
    // Проверяем записи для нашего адаптера
    if ARPEntries[i].IfIndex <> RNDISAdapter.IfIndex then
      Continue;

    // Проверяем критерии для устройства:
    // 1. В той же подсети, что и адаптер
    // 2. Не IP самого адаптера
    // 3. Не нулевой MAC адрес
    // 4. Не широковещательный MAC
    // 5. Не multicast
    // 6. Динамическая запись
    if IsSameSubnet(ARPEntries[i].IPAddress, RNDISAdapter.IPAddress, RNDISAdapter.SubnetMask) and
       (ARPEntries[i].IPAddress <> RNDISAdapter.IPAddress) and
       (ARPEntries[i].MACAddress <> '00-00-00-00-00-00') and
       (ARPEntries[i].MACAddress <> 'FF-FF-FF-FF-FF-FF') and
       (StringToIPDWord(ARPEntries[i].IPAddress) and $F0000000 <> $E0000000) and
       ARPEntries[i].IsDynamic then
    begin
      SetLength(Devices, Length(Devices) + 1);
      Devices[High(Devices)] := ARPEntries[i].IPAddress;
    end;
  end;

  Result := Devices;
end;

// Поиск IP активного RNDIS устройства
function FindRNDISDeviceIP: string;
var
  ARPEntries: TArray<TARPEntry>;
  RNDISAdapter: TNetworkAdapter;
  i: Integer;
begin
  Result := '';

  // Получаем информацию о RNDIS адаптере
  if not GetRNDISAdapterInfo(RNDISAdapter) then
    Exit;

  // Если маска не определена, используем стандартную для Class C
  if RNDISAdapter.SubnetMask = '' then
    RNDISAdapter.SubnetMask := '255.255.255.0';

  // Получаем ARP таблицу
  ARPEntries := GetARPTable;

  for i := 0 to High(ARPEntries) do
  begin
    // Проверяем записи для нашего адаптера
    if ARPEntries[i].IfIndex <> RNDISAdapter.IfIndex then
      Continue;

    // Проверяем критерии для устройства
    if IsSameSubnet(ARPEntries[i].IPAddress, RNDISAdapter.IPAddress, RNDISAdapter.SubnetMask) and
       (ARPEntries[i].IPAddress <> RNDISAdapter.IPAddress) then
    begin
      // Нашли подходящую запись
      Result := ARPEntries[i].IPAddress;
      Exit;
    end;
  end;
end;

end.
