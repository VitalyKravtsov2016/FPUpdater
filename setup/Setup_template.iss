[Code]
#include "services_unicode.iss"

#ifdef UNICODE
  #define AW "W"
#else
  #define AW "A"
#endif

function GetLastError: DWORD;
  external 'GetLastError@kernel32.dll stdcall setuponly';

function SetupCopyOEMInf(SourceInfFileName, OEMSourceMediaLocation: String;
  OEMSourceMediaType, CopyStyle: DWORD; DestinationInfFileName: String;
  DestinationInfFileNameSize: DWORD; var RequiredSize: DWORD;
  DestinationInfFileNameComponent: String): BOOL;
  external 'SetupCopyOEMInf{#AW}@setupapi.dll stdcall setuponly';

function UpdateDriverForPlugAndPlayDevices( hwndParent: HWND;
                                            HardwareId: String;
                                            FullInfPath: String;
                                            InstallFlags: Integer;
                                            var bRebootRequired: Boolean): Boolean;
external 'UpdateDriverForPlugAndPlayDevicesA@newdev.dll stdcall delayload';

function CM_Connect_Machine(  notNeed0: Integer ; var hMachine: Integer ): Integer ;
external 'CM_Connect_MachineA@setupapi.dll stdcall';

function CM_Locate_DevNode_Ex(  var DevInst: Integer ;
                                notNeed0: Integer ;
                                Flags: Integer ;
                                hMachine: Integer ): Integer ;
external 'CM_Locate_DevNode_ExA@setupapi.dll stdcall';

function CM_Reenumerate_DevNode_Ex(  DevInst: Integer ;
                            Flags: Integer ;
                            hMachine: Integer ): Integer ;
external 'CM_Reenumerate_DevNode_Ex@setupapi.dll stdcall';

function CM_Disconnect_Machine(hMachine: Integer ): Integer ;
external 'CM_Disconnect_Machine@setupapi.dll stdcall';

function IsWow64: Boolean;
external 'IsWow64Process@kernel32.dll stdcall';

function GetWindowsPlatform: String;
var
  Is64: Boolean;
begin
  // Проверка разрядности Windows
  if Is64BitInstallMode then
    Result := '64-bit'
  else
    Result := '32-bit';
    
  // Дополнительная проверка через IsWow64 для точности
  if not Is64BitInstallMode then
  begin
    if IsWow64 then
      Result := '64-bit (32-bit процесс)';
  end;
end;

function GetServicePack: String;
var
  Version: TWindowsVersion;
begin
  GetWindowsVersionEx(Version);
  
  // Для Windows XP и старше
  if (Version.Major = 5) and (Version.Minor = 1) then // XP
  begin
    if Version.Build >= 2600 then
    begin
      if Version.Build >= 2600 then Result := 'SP0';
      if Version.Build >= 2600 then Result := 'SP1';
      if Version.Build >= 2600 then Result := 'SP2';
      if Version.Build >= 2600 then Result := 'SP3';
    end;
  end
  else if (Version.Major = 5) and (Version.Minor = 2) then // Server 2003
    Result := 'SP2'
  else if (Version.Major = 6) and (Version.Minor = 0) then // Vista/Server 2008
    Result := 'SP2'
  else if (Version.Major = 6) and (Version.Minor = 1) then // 7/Server 2008 R2
    Result := 'SP1'
  else if (Version.Major = 6) and (Version.Minor = 2) then // 8/Server 2012
    Result := 'N/A'
  else if (Version.Major = 6) and (Version.Minor = 3) then // 8.1/Server 2012 R2
    Result := 'N/A'
  else if (Version.Major = 10) and (Version.Minor = 0) then // 10/11
    Result := 'N/A'
  else
    Result := 'Unknown';
end;

const
  MAX_PATH = 260;
  SPOST_NONE = 0;
  SPOST_PATH = 1;
  SPOST_URL = 2;
  SP_COPY_DELETESOURCE = $0000001;
  SP_COPY_REPLACEONLY = $0000002;
  SP_COPY_NOOVERWRITE = $0000008;
  SP_COPY_OEMINF_CATALOG_ONLY = $0040000;

function InstallDriver(PathLocation, FileName : String) : Boolean;
var
  RequiredSize: DWORD;
  DestinationInfFileName: String;
  DestinationInfFileNameComponent: String;
  LastError: DWORD;
  ErrorMsg: String;
begin
  Result := False;
  Log('[DRIVER] ===== Начало установки драйвера через SetupCopyOEMInf =====');
  Log('[DRIVER] Путь к драйверу: ' + PathLocation);
  Log('[DRIVER] Файл драйвера: ' + FileName);
  
  if FileExists(PathLocation+'\'+FileName) then
  begin
    Log('[DRIVER] Файл драйвера найден');
    SetLength(DestinationInfFileName, MAX_PATH);
    SetLength(DestinationInfFileNameComponent, MAX_PATH);
    
    Log('[DRIVER] Вызов SetupCopyOEMInf...');
    Result := SetupCopyOEMInf(PathLocation+'\'+FileName,
                              PathLocation, SPOST_PATH, 0,
                              DestinationInfFileName, MAX_PATH, RequiredSize,
                              DestinationInfFileNameComponent);
    
    if Result then
    begin
      Log('[DRIVER] ✓ SetupCopyOEMInf успешно выполнен');
      Log('[DRIVER]   DestinationInfFileName: ' + DestinationInfFileName);
      Log('[DRIVER]   RequiredSize: ' + IntToStr(RequiredSize));
    end
    else
    begin
      LastError := GetLastError;
      ErrorMsg := SysErrorMessage(LastError);
      Log('[DRIVER] ✗ Ошибка установки драйвера через SetupCopyOEMInf');
      Log('[DRIVER]   Код ошибки: ' + IntToStr(LastError));
      Log('[DRIVER]   Описание: ' + ErrorMsg);
      
      // Дополнительная информация об ошибке
      case LastError of
        2: Log('[DRIVER]   Ошибка 2: Файл не найден');
        3: Log('[DRIVER]   Ошибка 3: Путь не найден');
        5: Log('[DRIVER]   Ошибка 5: Отказано в доступе (нужны права администратора)');
        87: Log('[DRIVER]   Ошибка 87: Неправильный параметр');
        1156: Log('[DRIVER]   Ошибка 1156: Файл уже существует');
        1223: Log('[DRIVER]   Ошибка 1223: Операция отменена пользователем');
      end;
    end;
  end
  else
  begin
    Log('[DRIVER] ✗ Файл драйвера НЕ НАЙДЕН: ' + PathLocation+'\'+FileName);
    Log('[DRIVER]   Проверьте содержимое папки:');
    Log('[DRIVER]   ' + PathLocation);
  end;
  
  Log('[DRIVER] ===== Завершение установки драйвера =====');
end;

function IsWindowsVersionXP(Version: TWindowsVersion): Boolean;
begin
  Result := (Version.Major = 5) and (Version.Minor = 1);
  if Result then
    Log('[SYS] Обнаружена Windows XP');
end;

function IsWindowsVersionNew(Version: TWindowsVersion): Boolean;
begin
  Result := ((Version.Major = 5) and (Version.Minor > 1)) or (Version.Major > 5);
  if Result then
    Log('[SYS] Обнаружена современная Windows: ' + IntToStr(Version.Major) + '.' + IntToStr(Version.Minor));
end;

procedure EnumerateNodes;
var
  hMachine: Integer;
  DevInst: Integer;
  ResultCode: Integer;
begin
  Log('[PNP] ===== Перечисление PnP узлов =====');
  
  Log('[PNP] Подключение к машине...');
  ResultCode := CM_Connect_Machine(0, hMachine);
  Log('[PNP] CM_Connect_Machine результат: ' + IntToStr(ResultCode));
  
  if ResultCode = 0 then
  begin
    Log('[PNP] Поиск DevNode...');
    ResultCode := CM_Locate_DevNode_Ex(DevInst, 0, 0, hMachine);
    Log('[PNP] CM_Locate_DevNode_Ex результат: ' + IntToStr(ResultCode) + ', DevInst: ' + IntToStr(DevInst));
    
    Log('[PNP] Перечисление DevNode...');
    ResultCode := CM_Reenumerate_DevNode_Ex(DevInst, 0, hMachine);
    Log('[PNP] CM_Reenumerate_DevNode_Ex результат: ' + IntToStr(ResultCode));
    
    Log('[PNP] Отключение от машины...');
    ResultCode := CM_Disconnect_Machine(hMachine);
    Log('[PNP] CM_Disconnect_Machine результат: ' + IntToStr(ResultCode));
  end
  else
  begin
    Log('[PNP] ✗ Ошибка подключения к машине: ' + IntToStr(ResultCode));
  end;
  
  Log('[PNP] ===== Завершение перечисления PnP узлов =====');
end;

function InstallDriverXP(PathLocation, FileName : String) : Boolean;
var
  NeedReset: Boolean;
  H: HWND;
  FullPath: String;
begin
  Log('[XP] ===== Начало установки драйвера для Windows XP =====');
  Log('[XP] Путь: ' + PathLocation);
  Log('[XP] Файл: ' + FileName);
  
  FullPath := PathLocation+'\'+FileName;
  Log('[XP] Полный путь: ' + FullPath);
  
  // Перечисляем PnP узлы
  EnumerateNodes;
  
  H := StrToInt(ExpandConstant('{wizardhwnd}'));
  Log('[XP] Handle окна установки: ' + IntToStr(H));
  
  Log('[XP] Первая попытка UpdateDriverForPlugAndPlayDevices...');
  Log('[XP] Hardware ID: USB\VID_1FC9&PID_0089');
  Result := UpdateDriverForPlugAndPlayDevices(H, 
              'USB\VID_1FC9&PID_0089', FullPath, 1, NeedReset);
  
  if Result then
    Log('[XP] ✓ Первая попытка успешна')
  else
  begin
    Log('[XP] ✗ Первая попытка неудачна, код ошибки: ' + IntToStr(GetLastError));
    Log('[XP]   ' + SysErrorMessage(GetLastError));
  end;
  
  Log('[XP] Установка через SetupCopyOEMInf...');
  Result := InstallDriver(PathLocation, FileName);
  
  if Result then
    Log('[XP] ✓ SetupCopyOEMInf успешен')
  else
    Log('[XP] ✗ SetupCopyOEMInf неудачен');
  
  Log('[XP] Вторая попытка UpdateDriverForPlugAndPlayDevices...');
  Result := UpdateDriverForPlugAndPlayDevices(H, 
              'USB\VID_1FC9&PID_0089', FullPath, 1, NeedReset);
  
  if Result then
  begin
    Log('[XP] ✓ Вторая попытка успешна');
    if NeedReset then
      Log('[XP]   Требуется перезагрузка');
  end
  else
  begin
    Log('[XP] ✗ Вторая попытка неудачна, код ошибки: ' + IntToStr(GetLastError));
    Log('[XP]   ' + SysErrorMessage(GetLastError));
  end;
  
  Log('[XP] ===== Завершение установки драйвера для XP =====');
end;

procedure InstallDrivers;
var
  Version: TWindowsVersion;
  DriverPath: String;
  DriverFile: String;
  ServicePackStr: String;
begin
  Log(' ');
  Log('====================================================================');
  Log('=           НАЧАЛО УСТАНОВКИ ДРАЙВЕРА DFU                         =');
  Log('====================================================================');
  
  GetWindowsVersionEx(Version);
  
  // Определение Service Pack (упрощенная версия)
  if Version.ServicePackMajor > 0 then
    ServicePackStr := 'SP' + IntToStr(Version.ServicePackMajor)
  else if Version.ServicePackMinor > 0 then
    ServicePackStr := 'SP' + IntToStr(Version.ServicePackMinor)
  else
    ServicePackStr := 'N/A';
  
  Log('[SYS] Версия Windows: ' + IntToStr(Version.Major) + '.' + IntToStr(Version.Minor) + 
        ' (Build: ' + IntToStr(Version.Build) + ')');
  Log('[SYS] Платформа: ' + GetWindowsPlatform);
  Log('[SYS] Service Pack: ' + ServicePackStr);
  //Log('[SYS] NT Version: ' + IntToStr(Version.NTVersionMajor) + '.' + IntToStr(Version.NTVersionMinor));
  
  // Проверка прав администратора
  if IsAdminLoggedOn then
    Log('[SYS] ✓ Установка запущена с правами администратора')
  else
    Log('[SYS] ⚠ ВНИМАНИЕ: Установка запущена без прав администратора!');
  
  if IsWindowsVersionXP(Version) then
  begin
    DriverPath := ExpandConstant('{app}') + '\Bin\DFU\Driver\xp32';
    DriverFile := 'lpc-composite89-dfu.inf';
    Log('[SYS] Выбрана ветка: Windows XP (32-bit)');
    Log('[SYS] Путь к драйверам XP: ' + DriverPath);
    
    // Проверка наличия папки с драйверами
    if DirExists(DriverPath) then
      Log('[SYS] ✓ Папка с драйверами XP существует')
    else
      Log('[SYS] ✗ Папка с драйверами XP НЕ существует: ' + DriverPath);
    
    InstallDriverXP(DriverPath, DriverFile);
  end
  else
    if IsWindowsVersionNew(Version) then
    begin
      DriverPath := ExpandConstant('{app}') + '\Bin\DFU\Driver\new';
      DriverFile := 'lpc-composite89-dfu.inf';
      Log('[SYS] Выбрана ветка: Современная Windows');
      Log('[SYS] Путь к драйверам: ' + DriverPath);
      
      // Проверка наличия папки с драйверами
      if DirExists(DriverPath) then
        Log('[SYS] ✓ Папка с драйверами существует')
      else
        Log('[SYS] ✗ Папка с драйверами НЕ существует: ' + DriverPath);
      
      InstallDriver(DriverPath, DriverFile);
    end
    else
    begin
      Log('[SYS] ⚠ Неподдерживаемая версия Windows');
    end;
  
  // Проверка результата установки
  Log(' ');
  Log('====================================================================');
  Log('=           ЗАВЕРШЕНИЕ УСТАНОВКИ ДРАЙВЕРА DFU                     =');
  Log('====================================================================');
  Log(' ');
end;

[CustomMessages]
version2=1.0
; English
en.CompanyName=TorgBalance
en.AppName=Fiscal printer updater
en.AppComments=Retail equipment
en.DefaultDirName=\TorgBalance\FPUpdater
en.DefaultGroupName=TorgBalance FPUpdater
en.HistoryIcon=Version history
en.UninstallShortcutText=Uninstall
en.StartApplication=Start application
en.AppPublisher=TorgBalance
en.AppPublisherURL=http://www.torgbalance.com
en.AppSupportURL=http://www.torgbalance.com
en.AppUpdatesURL=http://www.torgbalance.com
en.AppCopyright=Copyright © 2025 TorgBalance
en.VersionHistory=Versions history
en.HistoryFileName=History_en.txt
en.RunApplication=Start application

; Russian
ru.CompanyName=ТОРГОВЫЙ БАЛАНС М
ru.AppName=Прошивальщик
ru.AppComments=Торговое оборудование от производителя, автоматизация торговли
ru.DefaultDirName=\TorgBalance\FPUpdater
ru.DefaultGroupName=Прошивальщик
ru.HistoryIcon=История версий
ru.StartApplication=Запустить приложение
ru.DesktopIconDescription=Создать ярлык на &рабочем столе
ru.QuickLaunchIconDescription=Создать &ярлык в панели быстрого запуска
ru.DesktopGroupDescription=Создание ярлыков
ru.AppPublisher=ТОРГОВЫЙ БАЛАНС М
ru.AppPublisherURL=http://www.torgbalance.com
ru.AppSupportURL=http://www.torgbalance.com
ru.AppUpdatesURL=http://www.torgbalance.com
ru.AppCopyright=Copyright © 2025 ТОРГОВЫЙ БАЛАНС М
ru.VersionHistory=История версий
ru.UninstallShortcutText=Удалить
ru.HistoryFileName=History_ru.txt
ru.RunApplication=Запустить приложение

[Setup]
AppName= {cm:AppName}
AppVerName= {cm:AppName} ${version2}_${build}"
DefaultDirName= {pf}\{cm:DefaultDirName}
DefaultGroupName= {cm:DefaultGroupName} ${version2}_${build} 
UninstallDisplayIcon= {app}\Uninstall.exe
AllowNoIcons=Yes
OutputDir="."
UsePreviousAppDir=No
UsePreviousGroup=No
${Architecture}
AppVersion=${version}
AppPublisher= {cm:AppPublisher}
AppPublisherURL= {cm:AppPublisherURL}
AppSupportURL= {cm:AppSupportURL}
AppUpdatesURL= {cm:AppUpdatesURL}
AppComments= {cm:AppComments}
AppReadmeFile=History.txt
AppCopyright= {cm:AppCopyright}
;Version
VersionInfoCompany=TorgBalance
VersionInfoDescription=Fiscal printer updater
VersionInfoTextVersion="${version}"
VersionInfoVersion=${version}
UsePreviousLanguage=no
OutputBaseFilename=setup
[Languages]
Name: en; MessagesFile: compiler:Default.isl;
Name: ru; MessagesFile: "compiler:Languages\Russian.isl"
[Tasks]
Name: "desktopicon"; Description: {cm:DesktopIconDescription}; GroupDescription: {cm:DesktopGroupDescription}; Flags: unchecked
Name: "quicklaunchicon"; Description: {cm:QuickLaunchIconDescription}; GroupDescription: {cm:DesktopGroupDescription}; Flags: unchecked
[Files]
; Version history
Source: "Bin\History_ru.txt"; DestDir: "{app}"; DestName: "History.txt"; Languages: ru; Flags: ${Archbit}; 
Source: "Bin\History_en.txt"; DestDir: "{app}"; DestName: "History.txt"; Languages: en; Flags: ${Archbit}; 
; Application
Source: "Bin\${Arch}\FPUpdater.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; 
Source: "Bin\${Arch}\FPUpdaterCli.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; 
Source: "Bin\Archive.zip"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; 
; DFU drivers
Source: "Bin\dfu\*"; DestDir: "{app}\Bin\dfu"; Flags: recursesubdirs createallsubdirs; AfterInstall: InstallDrivers
; DFU utility 
Source: "Bin\dfu-util\${Arch}\dfu-util-static.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; 
[Icons]
; Version history
Name: "{group}\{cm:VersionHistory}"; Filename: "{app}\History.txt"; 
; Main
Name: "{group}\{cm:AppName} ${version2} ${Arch2}"; Filename: "{app}\Bin\FPUpdater.exe"; WorkingDir: "{app}\Bin"; 
; Shortcuts
Name: "{userdesktop}\{cm:AppName} ${version2} ${Arch2} "; Filename: "{app}\Bin\FPUpdater.exe"; Tasks: desktopicon
[UninstallDelete]
Type: files; Name: "{app}\Bin\*.log"
[Run]
Filename: "{app}\Bin\FPUpdater.exe"; Description: {cm:RunApplication}; Flags: postinstall nowait skipifsilent