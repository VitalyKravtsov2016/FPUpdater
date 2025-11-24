unit RndisAdapter;

interface

uses
  // VCL
  Windows, SysUtils, Classes, Registry, IniFiles, ComObj, ActiveX, Variants;

type
  { TRndisAdapterInfo }

  TRndisAdapterInfo = record
    AdapterName: string;
    Description: string;
    InterfaceGUID: string;
    DeviceID: string;
    MACAddress: string;
    IPAddress: string;
    SubnetMask: string;
    DefaultGateway: string;
    DNSServers: string;
    IsEnabled: Boolean;
    ConnectionStatus: Integer;
    RegistryPath: string;
  end;

  { TRndisAdapterManager }

  TRndisAdapterManager = class
  private
    FAdapterInfo: TRndisAdapterInfo;
    FIsFound: Boolean;
    function FindAdapter: Boolean;
    function ExecuteCommand(const Command: string): Cardinal;
    function ExecuteDevconCommand: Boolean;
    function ExecuteNetshCommand: Boolean;
    function GetDeviceID: string;
    function LocateAdapterByGUID: Boolean;
    function ResetAdapterViaWMI: Boolean;
    function ResetNetworkAdapter: Boolean;
    function RestoreAdapterParameters: Boolean;
    function RestoreNetworkSettings: Boolean;
  public
    constructor Create;
    function LocateAdapter: Boolean;
    function SaveToFile(const FileName: string): Boolean;
    function LoadFromFile(const FileName: string): Boolean;
    function RestoreSettings: Boolean;

    property AdapterInfo: TRndisAdapterInfo read FAdapterInfo;
    property IsFound: Boolean read FIsFound;
  end;

implementation

procedure GetAdapterMACAddress(var AdapterInfo: TRndisAdapterInfo);
var
  Reg: TRegistry;
  AdapterKey: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    AdapterKey := 'SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}\' +
                  ExtractFileName(AdapterInfo.RegistryPath);

    if Reg.OpenKey(AdapterKey, False) then
    begin
      try
        if Reg.ValueExists('NetworkAddress') then
          AdapterInfo.MACAddress := Reg.ReadString('NetworkAddress')
        else if Reg.ValueExists('MAC') then
          AdapterInfo.MACAddress := Reg.ReadString('MAC');
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure GetAdapterNetworkSettings(var AdapterInfo: TRndisAdapterInfo);
var
  Reg: TRegistry;
  TCPKey: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Ищем настройки TCP/IP для этого адаптера
    TCPKey := 'SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\Interfaces\' + AdapterInfo.InterfaceGUID;

    if Reg.OpenKey(TCPKey, False) then
    begin
      try
        if Reg.ValueExists('IPAddress') then
          AdapterInfo.IPAddress := Reg.ReadString('IPAddress');

        if Reg.ValueExists('SubnetMask') then
          AdapterInfo.SubnetMask := Reg.ReadString('SubnetMask');

        if Reg.ValueExists('DefaultGateway') then
          AdapterInfo.DefaultGateway := Reg.ReadString('DefaultGateway');

        if Reg.ValueExists('NameServer') then
          AdapterInfo.DNSServers := Reg.ReadString('NameServer');
      finally
        Reg.CloseKey;
      end;
    end;

    // Получаем MAC-адрес
    GetAdapterMACAddress(AdapterInfo);

  finally
    Reg.Free;
  end;
end;

function GetRndisAdapterInfo: TRndisAdapterInfo;
var
  Reg: TRegistry;
  KeyList: TStringList;
  i: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  Reg := TRegistry.Create;
  KeyList := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}', False) then
    begin
      Reg.GetKeyNames(KeyList);
      Reg.CloseKey;

      for i := 0 to KeyList.Count - 1 do
      begin
        if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}\' + KeyList[i], False) then
        begin
          try
            if Reg.ValueExists('DriverDesc') and (Pos('RNDIS', UpperCase(Reg.ReadString('DriverDesc'))) > 0) then
            begin
              // Основная информация
              Result.Description := Reg.ReadString('DriverDesc');
              Result.AdapterName := Reg.ReadString('DriverDesc');

              if Reg.ValueExists('NetCfgInstanceId') then
                Result.InterfaceGUID := Reg.ReadString('NetCfgInstanceId');

              if Reg.ValueExists('NetLuidIndex') then
                Result.DeviceID := Reg.ReadString('NetLuidIndex');

              Result.RegistryPath := 'SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}\' + KeyList[i];

              // Получаем сетевые настройки
              GetAdapterNetworkSettings(Result);
              Break;
            end;
          finally
            Reg.CloseKey;
          end;
        end;
      end;
    end;
  finally
    KeyList.Free;
    Reg.Free;
  end;
end;

{ TRndisAdapterManager }

constructor TRndisAdapterManager.Create;
begin
  inherited Create;
  FillChar(FAdapterInfo, SizeOf(FAdapterInfo), 0);
  FIsFound := False;
end;

function TRndisAdapterManager.LocateAdapter: Boolean;
begin
  FIsFound := FindAdapter;
  Result := FIsFound;
end;

function TRndisAdapterManager.FindAdapter: Boolean;
var
  Reg: TRegistry;
  KeyList: TStringList;
  i: Integer;
begin
  Result := False;
  Reg := TRegistry.Create;
  KeyList := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}', False) then
    begin
      Reg.GetKeyNames(KeyList);
      Reg.CloseKey;

      for i := 0 to KeyList.Count - 1 do
      begin
        if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}\' + KeyList[i], False) then
        begin
          try
            if Reg.ValueExists('DriverDesc') and
               (Pos('RNDIS', UpperCase(Reg.ReadString('DriverDesc'))) > 0) then
            begin
              FAdapterInfo.Description := Reg.ReadString('DriverDesc');
              FAdapterInfo.AdapterName := Reg.ReadString('DriverDesc');

              if Reg.ValueExists('NetCfgInstanceId') then
                FAdapterInfo.InterfaceGUID := Reg.ReadString('NetCfgInstanceId');

              FAdapterInfo.RegistryPath := 'SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}\' + KeyList[i];

              Result := True;
              Break;
            end;
          finally
            Reg.CloseKey;
          end;
        end;
      end;
    end;
  finally
    KeyList.Free;
    Reg.Free;
  end;
end;

function TRndisAdapterManager.SaveToFile(const FileName: string): Boolean;
var
  IniFile: TIniFile;
begin
  Result := False;
  try
    IniFile := TIniFile.Create(FileName);
    try
      IniFile.WriteString('RNDIS_Adapter', 'Description', FAdapterInfo.Description);
      IniFile.WriteString('RNDIS_Adapter', 'InterfaceGUID', FAdapterInfo.InterfaceGUID);
      IniFile.WriteString('RNDIS_Adapter', 'DeviceID', FAdapterInfo.DeviceID);
      IniFile.WriteString('RNDIS_Adapter', 'MACAddress', FAdapterInfo.MACAddress);
      IniFile.WriteString('RNDIS_Adapter', 'IPAddress', FAdapterInfo.IPAddress);
      IniFile.WriteString('RNDIS_Adapter', 'SubnetMask', FAdapterInfo.SubnetMask);
      IniFile.WriteString('RNDIS_Adapter', 'DefaultGateway', FAdapterInfo.DefaultGateway);
      IniFile.WriteString('RNDIS_Adapter', 'DNSServers', FAdapterInfo.DNSServers);
      IniFile.WriteString('RNDIS_Adapter', 'RegistryPath', FAdapterInfo.RegistryPath);
      IniFile.WriteBool('RNDIS_Adapter', 'IsEnabled', FAdapterInfo.IsEnabled);
      IniFile.WriteInteger('RNDIS_Adapter', 'ConnectionStatus', FAdapterInfo.ConnectionStatus);

      Result := True;
    finally
      IniFile.Free;
    end;
  except
    on E: Exception do
      // Логирование ошибки
  end;
end;

function TRndisAdapterManager.LoadFromFile(const FileName: string): Boolean;
var
  IniFile: TIniFile;
begin
  Result := False;
  try
    if not FileExists(FileName) then Exit;

    IniFile := TIniFile.Create(FileName);
    try
      FAdapterInfo.Description := IniFile.ReadString('RNDIS_Adapter', 'Description', '');
      FAdapterInfo.InterfaceGUID := IniFile.ReadString('RNDIS_Adapter', 'InterfaceGUID', '');
      FAdapterInfo.DeviceID := IniFile.ReadString('RNDIS_Adapter', 'DeviceID', '');
      FAdapterInfo.MACAddress := IniFile.ReadString('RNDIS_Adapter', 'MACAddress', '');
      FAdapterInfo.IPAddress := IniFile.ReadString('RNDIS_Adapter', 'IPAddress', '');
      FAdapterInfo.SubnetMask := IniFile.ReadString('RNDIS_Adapter', 'SubnetMask', '');
      FAdapterInfo.DefaultGateway := IniFile.ReadString('RNDIS_Adapter', 'DefaultGateway', '');
      FAdapterInfo.DNSServers := IniFile.ReadString('RNDIS_Adapter', 'DNSServers', '');
      FAdapterInfo.RegistryPath := IniFile.ReadString('RNDIS_Adapter', 'RegistryPath', '');
      FAdapterInfo.IsEnabled := IniFile.ReadBool('RNDIS_Adapter', 'IsEnabled', True);
      FAdapterInfo.ConnectionStatus := IniFile.ReadInteger('RNDIS_Adapter', 'ConnectionStatus', 0);

      Result := True;
      FIsFound := True;
    finally
      IniFile.Free;
    end;
  except
    on E: Exception do
      // Логирование ошибки
  end;
end;

function TRndisAdapterManager.RestoreSettings: Boolean;
begin
  Result := False;
  if not FIsFound then
  begin
    // Пытаемся найти адаптер по сохраненному GUID
    if not LocateAdapterByGUID then
      Exit;
  end;

  // Восстанавливаем сетевые настройки
  if not RestoreNetworkSettings then
    Exit;

  // Восстанавливаем дополнительные параметры
  if not RestoreAdapterParameters then
    Exit;

  // Перезапускаем адаптер для применения настроек
  Result := ResetNetworkAdapter;
end;

function TRndisAdapterManager.LocateAdapterByGUID: Boolean;
var
  Reg: TRegistry;
  KeyList: TStringList;
  i: Integer;
begin
  Result := False;

  // Если GUID не сохранен, выходим
  if FAdapterInfo.InterfaceGUID = '' then
    Exit;

  Reg := TRegistry.Create;
  KeyList := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Ищем в интерфейсах TCP/IP
    if Reg.OpenKey('SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\Interfaces\' +
                   FAdapterInfo.InterfaceGUID, False) then
    begin
      Reg.CloseKey;

      // Находим описание адаптера
      if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}', False) then
      begin
        Reg.GetKeyNames(KeyList);
        Reg.CloseKey;

        for i := 0 to KeyList.Count - 1 do
        begin
          if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}\' +
                         KeyList[i], False) then
          begin
            try
              if Reg.ValueExists('NetCfgInstanceId') and
                 (Reg.ReadString('NetCfgInstanceId') = FAdapterInfo.InterfaceGUID) then
              begin
                FAdapterInfo.RegistryPath :=
                  'SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}\' + KeyList[i];

                if Reg.ValueExists('DriverDesc') then
                  FAdapterInfo.Description := Reg.ReadString('DriverDesc');

                FIsFound := True;
                Result := True;
                Break;
              end;
            finally
              Reg.CloseKey;
            end;
          end;
        end;
      end;
    end;
  finally
    KeyList.Free;
    Reg.Free;
  end;
end;

function TRndisAdapterManager.RestoreNetworkSettings: Boolean;
var
  Reg: TRegistry;
  TCPKey: string;
begin
  Result := False;

  if FAdapterInfo.InterfaceGUID = '' then
    Exit;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.Access := KEY_WRITE;

    // Ключ TCP/IP настроек адаптера
    TCPKey := 'SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\Interfaces\' + FAdapterInfo.InterfaceGUID;

    if Reg.OpenKey(TCPKey, False) then
    begin
      try
        // Восстанавливаем IP-адрес
        if FAdapterInfo.IPAddress <> '' then
          Reg.WriteString('IPAddress', FAdapterInfo.IPAddress)
        else
          Reg.DeleteValue('IPAddress');

        // Восстанавливаем маску подсети
        if FAdapterInfo.SubnetMask <> '' then
          Reg.WriteString('SubnetMask', FAdapterInfo.SubnetMask)
        else
          Reg.DeleteValue('SubnetMask');

        // Восстанавливаем шлюз по умолчанию
        if FAdapterInfo.DefaultGateway <> '' then
          Reg.WriteString('DefaultGateway', FAdapterInfo.DefaultGateway)
        else
          Reg.DeleteValue('DefaultGateway');

        // Восстанавливаем DNS-серверы
        if FAdapterInfo.DNSServers <> '' then
          Reg.WriteString('NameServer', FAdapterInfo.DNSServers)
        else
          Reg.DeleteValue('NameServer');

        // Устанавливаем DHCP в зависимости от наличия статического IP
        if FAdapterInfo.IPAddress <> '' then
          Reg.WriteBool('EnableDHCP', False)
        else
          Reg.WriteBool('EnableDHCP', True);

        Result := True;

      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TRndisAdapterManager.RestoreAdapterParameters: Boolean;
var
  Reg: TRegistry;
  AdapterKey: string;
begin
  Result := False;

  if FAdapterInfo.RegistryPath = '' then
    Exit;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.Access := KEY_WRITE;

    AdapterKey := Copy(FAdapterInfo.RegistryPath, Pos('\Class\', FAdapterInfo.RegistryPath) + 7,
                      Length(FAdapterInfo.RegistryPath));

    if Reg.OpenKey('SYSTEM\CurrentControlSet\Control\Class\{4d36e972-e325-11ce-bfc1-08002be10318}\' +
                   AdapterKey, False) then
    begin
      try
        // Восстанавливаем MAC-адрес если нужно
        if FAdapterInfo.MACAddress <> '' then
          Reg.WriteString('NetworkAddress', FAdapterInfo.MACAddress);

        // Дополнительные параметры можно добавить здесь
        Result := True;

      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TRndisAdapterManager.ResetNetworkAdapter: Boolean;
begin
  Result := False;

  // Способ 1: Использование netsh (простой и надежный)
  if ExecuteNetshCommand then
    Result := True
  // Способ 2: Использование devcon (альтернативный)
  else if ExecuteDevconCommand then
    Result := True
  // Способ 3: Использование WMI
  else
    Result := ResetAdapterViaWMI;
end;

function TRndisAdapterManager.ExecuteNetshCommand: Boolean;
var
  Command: string;
  ExitCode: Cardinal;
begin
  // Отключаем адаптер
  Command := 'netsh interface set interface name="' + FAdapterInfo.Description + '" admin=disable';
  ExitCode := ExecuteCommand(Command);

  // Небольшая задержка
  Sleep(2000);

  // Включаем адаптер
  Command := 'netsh interface set interface name="' + FAdapterInfo.Description + '" admin=enable';
  ExitCode := ExecuteCommand(Command);

  Result := (ExitCode = 0);
end;

function TRndisAdapterManager.ExecuteDevconCommand: Boolean;
var
  Command: string;
  ExitCode: Cardinal;
  DeviceID: string;
begin
  Result := False;

  // Получаем ID устройства
  DeviceID := GetDeviceID;
  if DeviceID = '' then
    Exit;

  // Перезапускаем устройство
  Command := 'devcon restart "' + DeviceID + '"';
  ExitCode := ExecuteCommand(Command);

  Result := (ExitCode = 0);
end;

function TRndisAdapterManager.GetDeviceID: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey(FAdapterInfo.RegistryPath, False) then
    begin
      try
        if Reg.ValueExists('MatchingDeviceId') then
          Result := Reg.ReadString('MatchingDeviceId');
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TRndisAdapterManager.ResetAdapterViaWMI: Boolean;
var
  FSWbemLocator: OLEVariant;
  FWMIService: OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject: OLEVariant;
  oEnum: IEnumVariant;
  iValue: LongWord;
begin
  Result := False;

  try
    FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
    FWMIService := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');

    // Находим адаптер по описанию
    FWbemObjectSet := FWMIService.ExecQuery(
      'SELECT * FROM Win32_NetworkAdapter WHERE Description = "' + FAdapterInfo.Description + '"'
    );

    oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;

    while oEnum.Next(1, FWbemObject, iValue) = 0 do
    begin
      try
        // Отключаем
        FWbemObject.Disable;
        Sleep(2000);
        // Включаем
        FWbemObject.Enable;
        Result := True;
        Break;
      except
        on E: Exception do
        begin
          // Логируем ошибку
          OutputDebugString(PChar('WMI Error: ' + E.Message));
        end;
      end;
      FWbemObject := Unassigned;
    end;

  except
    on E: Exception do
    begin
      OutputDebugString(PChar('WMI Connection Error: ' + E.Message));
    end;
  end;
end;

function TRndisAdapterManager.ExecuteCommand(const Command: string): Cardinal;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;

  if CreateProcess(nil, PChar('cmd.exe /C ' + Command), nil, nil, False,
                   CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil,
                   StartupInfo, ProcessInfo) then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end
  else
    Result := $FFFFFFFF;
end;
end.
