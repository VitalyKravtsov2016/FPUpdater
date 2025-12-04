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
begin
  Result := False;
  if FileExists(PathLocation+'\'+FileName) then begin
    SetLength(DestinationInfFileName, MAX_PATH);
    SetLength(DestinationInfFileNameComponent, MAX_PATH);
    Result := SetupCopyOEMInf(PathLocation+'\'+FileName,
                              PathLocation, SPOST_PATH, 0,
                              DestinationInfFileName, MAX_PATH, RequiredSize,
                              DestinationInfFileNameComponent);
    if not Result then
      Log('Error installing driver: ' + SysErrorMessage(GetLastError));
  end;
end;

function IsWindowsVersionXP(Version: TWindowsVersion): Boolean;
begin
  Result := (Version.Major = 5) and (Version.Minor = 1);
end;

function IsWindowsVersionNew(Version: TWindowsVersion): Boolean;
begin
  Result := ((Version.Major = 5) and (Version.Minor > 1)) or (Version.Major > 5);
end;

procedure EnumerateNodes;
var
  hMachine: Integer;
  DevInst: Integer;
begin
  CM_Connect_Machine( 0, hMachine);
  CM_Locate_DevNode_Ex( DevInst , 0, 0, hMachine);
  CM_Reenumerate_DevNode_Ex( DevInst , 0,  hMachine);
  CM_Disconnect_Machine ( hMachine );
end;

function InstallDriverXP(PathLocation, FileName : String) : Boolean;
var
  NeedReset: Boolean;
  H: HWND;
begin
  Log('Install driver XP');
  EnumerateNodes;
  H := StrToInt(ExpandConstant('{wizardhwnd}')); 
  Result := UpdateDriverForPlugAndPlayDevices(H, 
              'USB\VID_1FC9&PID_0089', PathLocation+'\'+FileName, 1, NeedReset);

  Result := InstallDriver(PathLocation, FileName);
  Result := UpdateDriverForPlugAndPlayDevices(H, 
              'USB\VID_1FC9&PID_0089', PathLocation+'\'+FileName, 1, NeedReset);
end;

procedure InstallDrivers;
var
  Version: TWindowsVersion;
begin
  GetWindowsVersionEx(Version);
  Log('Win ver ' + IntToStr(Version.Major) + '.' + IntToStr(Version.Minor));
  if IsWindowsVersionXP(Version) then
    InstallDriverXP(ExpandConstant('{app}') + '\Bin\DFU\Driver\xp32', 'lpc-composite89-dfu.inf')
  else
    if IsWindowsVersionNew(Version) then
    begin
      InstallDriver(ExpandConstant('{app}') + '\Bin\DFU\Driver\new', 'lpc-composite89-dfu.inf');
      InstallDriver(ExpandConstant('{app}') + '\Bin\Drivers\usb_drivers', 'BOOTLOADER_DFU_FS_Mode.inf');
      InstallDriver(ExpandConstant('{app}') + '\Bin\Drivers\usb_drivers', 'FR_USB-Serial_port_(IF).inf');
      InstallDriver(ExpandConstant('{app}') + '\Bin\Drivers\usb_drivers', 'FR_USB-Serial_port_(IF_with_cdc-eth).inf');
      InstallDriver(ExpandConstant('{app}') + '\Bin\Drivers\usb_drivers', 'FR_USB-Serial_port_(IF_with_ncm).inf');
      InstallDriver(ExpandConstant('{app}') + '\Bin\Drivers\usb_drivers', 'FR_USB-Serial_port_(PPP).inf');
    end;
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
Source: "Bin\Archive.zip"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; 
; DFU drivers
Source: "Bin\DFU\Driver\*"; DestDir: "{app}\Bin\DFU\Driver"; Flags: recursesubdirs createallsubdirs; AfterInstall: InstallDrivers
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
