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

function IsLocalRNDISDevice(const IPAddress: string): Boolean;
function GetAdapterByIP(const IPAddress: string): TNetworkAdapter;
function GetMACAddress(const AIPAddress: string): string;
function GetAdaptersList: TArray<TNetworkAdapter>;
function IsRouteViaRNDIS(const IPAddress: string): Boolean;

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

end.
