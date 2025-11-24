[Code]
#include "services_unicode.iss"

const
  KKTPROXY_SERVICE_NAME = 'kktproxy';
  KKTPROXY_SERVICE_DISPLAY_NAME = 'SHTRIH-M: kktproxy';
  KKTPROXY_SERVICE_EXE = 'kktproxy.exe';

  PPP_SERVICE_NAME = 'pppnetservice';
  PPP_SERVICE_DISPLAY_NAME = 'SHTRIH-M: PPP net service';
  PPP_SERVICE_EXE = 'pppnetsvc.exe';

var
  ServiceStarted: Boolean;


const
  NET_FW_SCOPE_ALL = 0;
  NET_FW_IP_VERSION_ANY = 2;

function SwitchHasValue(Name: string; Value: string): Boolean;
begin
  Result := CompareText(ExpandConstant('{param:' + Name + '}'), Value) = 0;
end;

function CmdLineParamExists(const Value: string): Boolean;
var
  I: Integer;  
begin
  Result := False;
  for I := 1 to ParamCount do
    if CompareText(ParamStr(I), Value) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

procedure SetFirewallException(AppName,FileName:string);
var
  FirewallObject: Variant;
  FirewallManager: Variant;
  FirewallProfile: Variant;
  ResultCode: Integer;
begin
  try
    Exec(
    'cmd.exe', '/C netsh advfirewall firewall add rule name=L2TP_TCP protocol=TCP localport=1080 action=allow dir=IN', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
                           
    FirewallObject := CreateOleObject('HNetCfg.FwAuthorizedApplication');
    FirewallObject.ProcessImageFileName := FileName;
    FirewallObject.Name := AppName;
    FirewallObject.Scope := NET_FW_SCOPE_ALL;
    FirewallObject.IpVersion := NET_FW_IP_VERSION_ANY;
    FirewallObject.Enabled := True;
    FirewallManager := CreateOleObject('HNetCfg.FwMgr');
    FirewallProfile := FirewallManager.LocalPolicy.CurrentProfile;
    FirewallProfile.AuthorizedApplications.Add(FirewallObject);
  except
  end;
end;

procedure RemoveFirewallException( FileName:string );
var
  FirewallManager: Variant;
  FirewallProfile: Variant;
begin
  try
    FirewallManager := CreateOleObject('HNetCfg.FwMgr');
    FirewallProfile := FirewallManager.LocalPolicy.CurrentProfile;
    FireWallProfile.AuthorizedApplications.Remove(FileName);
  except
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  (*if CurStep = ssPostInstall then 
  begin        
    MsgBox('postinstall', mbInformation, MB_OK);
    if CmdLineParamExists('uflp_server_url') then
    begin
      MsgBox('uflp_server_url exists', mbInformation, MB_OK);
      MsgBox('uflp_server_url = ' + ExpandConstant('{param:uflp_server_url}'), mbInformation, MB_OK);
      if SwitchHasValue('storagetype', 'hklm') then
        MsgBox('hkey machine', mbInformation, MB_OK);
        RegWriteStringValue(HKEY_LOCAL_MACHINE, 
        'Software\ShtrihM\DrvFR\UpdateFeatureLicPlugin', 
        'ServerUrl', ExpandConstant('{param:uflp_server_url}'))
      else
      begin
        MsgBox('hkey user', mbInformation, MB_OK);
        RegWriteStringValue(HKEY_CURRENT_USER, 
        'Software\ShtrihM\DrvFR\UpdateFeatureLicPlugin', 
        'ServerUrl', 
        ExpandConstant('{param:uflp_server_url}'));
      end
    end;
  end;*)

  //Log('CurStepChanged(' + IntToStr(Ord(CurStep)) + ') called');
  if pos('kktproxy', WizardSelectedComponents(False)) > 0 then
  begin
 	  if CurStep = ssInstall then 
    begin
      ServiceStarted := False;
      if ServiceExists(KKTPROXY_SERVICE_NAME) then 
      begin
        try
          if SimpleQueryService(KKTPROXY_SERVICE_NAME) = SERVICE_RUNNING then 
          begin
            ServiceStarted := True;
            try
              SimpleStopService(KKTPROXY_SERVICE_NAME, True, True);
              Sleep(3000);
            except
            end;
          end;
          SimpleDeleteService(KKTPROXY_SERVICE_NAME);
        except
        end;
      end;
    end
    else if CurStep = ssPostInstall then 
    begin        
      
      SetFirewallException('SHTRIH-M: kktproxy', ExpandConstant('{app}\Bin\kktproxy\' + KKTPROXY_SERVICE_EXE));                                 
      SimpleCreateService(KKTPROXY_SERVICE_NAME, KKTPROXY_SERVICE_DISPLAY_NAME, ExpandConstant('{app}\Bin\kktproxy\' + KKTPROXY_SERVICE_EXE), SERVICE_AUTO_START, '', '', False, False);
      //if ServiceStarted then
      begin
        SimpleStartService(KKTPROXY_SERVICE_NAME, True, True);
      end;
    end;
  end;


  if pos('pppnetservice', WizardSelectedComponents(False)) > 0 then
  begin
 	  if CurStep = ssInstall then 
    begin
      ServiceStarted := False;
      if ServiceExists(PPP_SERVICE_NAME) then 
      begin
        try
          if SimpleQueryService(PPP_SERVICE_NAME) = SERVICE_RUNNING then 
          begin
            ServiceStarted := True;
            try
              SimpleStopService(PPP_SERVICE_NAME, True, True);
              Sleep(3000);
            except
            end;
          end;
          SimpleDeleteService(PPP_SERVICE_NAME);
        except
        end;
      end;
    end
    else if CurStep = ssPostInstall then 
    begin        
      SetFirewallException('SHTRIH-M: PPP net service', ExpandConstant('{app}\Bin\pppnetservice\' + PPP_SERVICE_EXE));                                 
      SimpleCreateService(PPP_SERVICE_NAME, PPP_SERVICE_DISPLAY_NAME, ExpandConstant('{app}\Bin\pppnetservice\' + PPP_SERVICE_EXE), SERVICE_AUTO_START, '', '', False, False);
      //if ServiceStarted then
      begin
        SimpleStartService(PPP_SERVICE_NAME, True, True);
      end;
    end;
  end;

end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
//  Log('CurUninstallStepChanged(' + IntToStr(Ord(CurUninstallStep)) + ') called');
  
 	  if CurUninstallStep = usUninstall then 
    begin
      if ServiceExists(KKTPROXY_SERVICE_NAME) then 
      begin
        if SimpleQueryService(KKTPROXY_SERVICE_NAME) = SERVICE_RUNNING then 
        begin
          SimpleStopService(KKTPROXY_SERVICE_NAME, True, False);
          Sleep(3000);
        end;
        SimpleDeleteService(KKTPROXY_SERVICE_NAME);
      end;
      RemoveFirewallException(ExpandConstant('{app}\Bin\kktproxy\' + KKTPROXY_SERVICE_EXE));
    end;


    if CurUninstallStep = usUninstall then 
    begin
      if ServiceExists(PPP_SERVICE_NAME) then 
      begin
        if SimpleQueryService(PPP_SERVICE_NAME) = SERVICE_RUNNING then 
        begin
          SimpleStopService(PPP_SERVICE_NAME, True, False);
          Sleep(3000);
        end;
        SimpleDeleteService(PPP_SERVICE_NAME);
      end;
      RemoveFirewallException(ExpandConstant('{app}\Bin\pppnetservice\' + PPP_SERVICE_EXE));
    end;
end;

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
; English
en.CompanyName=SHTRIH-M
en.AppName=SHTRIH-M: Fiscal printer driver
en.AppComments=Retail equipment
en.AppCopyright=Copyright © 2022 SHTRIH-M
en.DefaultDirName=\SHTRIH-M\DrvFR
en.DefaultGroupName=SHTRIH-M DrvFR
en.HistoryIcon=Version history
en.UninstallShortcutText=Uninstall
en.StartApplication=Run driver test
en.HistoryFileName=Doc\en\History.txt
en.DesktopIconDescription=Create shortcut on the desktop
en.QuickLaunchIconDescription=Create shortcut on the quick launch panel
en.DesktopGroupDescription=Create shortcuts
en.AppPublisher=SHTRIH-M
en.AppPublisherURL=http://www.shtrih-m.ru
en.AppSupportURL=http://www.shtrih-m.ru
en.AppUpdatesURL=http://www.shtrih-m.ru
en.AppCopyright=Copyright © 2022 SHTRIH-M
en.ComponentsMain=Driver and tests
en.ComponentsSamples=Samples
en.ComponentsOKassa=OKassa service
en.OKassaFolder=OKassa
en.Componentskktproxy=kktproxy service
en.Componentspppnetservice=PPP net service
en.ComponentsDrivers = USB Drivers
en.VersionHistory=Versions history
en.PrintServer=Print server
en.DriverTest=Driver test
en.OKassa=OKassa
en.OKassaMaster=OKassa service setup wizard
en.DriverTest1C=Driver test (1C interface)
en.ServiceProg=Service application
en.TaxProg=Tax inspector utility
en.TaxProg2=Tax inspector utility 2
en.ServerFR=Server FR
en.HistoryFileName=res\History_en.txt
en.StartOKassa=Start "OKassa" setup application
en.FiscalMaster=FiscalMaster
;en.OKassaDoc=OKassa service documentation
; russian
ru.CompanyName=SHTRIH-M
ru.AppName=ШТРИХ-М: Драйвер ККТ
ru.AppComments=Торговое оборудование от производителя, автоматизация торговли
ru.AppCopyright=Copyright © 2022 ШТРИХ-М
ru.DefaultDirName=\SHTRIH-M\DrvFR
ru.DefaultGroupName=ШТРИХ-М Драйвер ККТ
ru.HistoryIcon=История версий
ru.UninstallShortcutText=Удалить
ru.StartApplication=Запустить тест драйвера
ru.HistoryFileName=Doc\ru\History.txt
ru.DesktopIconDescription=Создать ярлык на &рабочем столе
ru.QuickLaunchIconDescription=Создать &ярлык в панели быстрого запуска
ru.DesktopGroupDescription=Создание ярлыков
ru.AppPublisher=ШТРИХ-М
ru.AppPublisherURL=http://www.shtrih-m.ru
ru.AppSupportURL=http://www.shtrih-m.ru
ru.AppUpdatesURL=http://www.shtrih-m.ru
ru.AppCopyright=Copyright © 2022 ШТРИХ-М
ru.ComponentsMain=Драйвер и тесты
ru.ComponentsSamples=Примеры
ru.ComponentsOKassa=Облачная касса
ru.Componentskktproxy=Служба kktproxy
ru.Componentspppnetservice=Служба pppnetservice
ru.ComponentsDrivers = Драйверы USB
ru.VersionHistory=История версий
ru.PrintServer=Сервер печати ККТ
ru.DriverTest=Тест драйвера ККТ
ru.OKassa=Облачная касса
ru.DriverTest1C=Тест драйвера ККТ (интерфейс 1С)
ru.ServiceProg=Сервисная программа
ru.TaxProg=Программа налогового инспектора
ru.TaxProg2=Программа налогового инспектора 2
ru.ServerFR=Сервер ККТ
ru.UninstallShortcutText=Удалить
ru.HistoryFileName=res\History_ru.txt
ru.StartOKassa=Запустить мастер установки сервиса "Облачная касса"
ru.OKassaMaster=Мастер установки сервиса Облачная касса
;ru.OKassaDoc=Документация по настройке сервиса Облачная касса
ru.OKassaFolder=Облачная касса
ru.FiscalMaster=Мастер фискализации
[Setup]
AppName= {cm:AppName}
AppVerName= {cm:AppName} ${version2}"
DefaultDirName= {pf}\{cm:DefaultDirName}5
DefaultGroupName= {cm:DefaultGroupName} ${version2} ${Arch2}
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
AppContact=(495)787-6090
AppReadmeFile=History.txt
AppCopyright= {cm:AppCopyright}
;Version
VersionInfoCompany=SHTRIH-M
VersionInfoDescription=Fiscal printer driver
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
[Components]
Name: "main"; Description: {cm:ComponentsMain}; Types: full compact custom
name: "usbdrivers"; Description: {cm:ComponentsDrivers}; Types: full compact custom
Name: "kktproxy"; Description: {cm:Componentskktproxy}; Types: full compact custom
Name: "pppnetservice"; Description: {cm:Componentspppnetservice}; Types: full compact custom
Name: "okassa"; Description: {cm:ComponentsOkassa}; Types: full compact custom; 
Name: "samples"; Description: {cm:ComponentsSamples}; 
Name: "images"; Description: "Images"; Types: full
                                                                                  
[Files]
; storagetype
Source: "Res\StorageSettings\hklm\DrvFR.xml"; DestDir: "{userappdata}\SHTRIH-M\DrvFR"; Check: SwitchHasValue('storagetype', 'hklm')
Source: "Res\StorageSettings\hklm\DrvFR.xml"; DestDir: "{commonappdata}\SHTRIH-M\DrvFR"; Check: SwitchHasValue('storagetype', 'hklm_common')
Source: "Res\StorageSettings\hkcu\DrvFR.xml"; DestDir: "{userappdata}\SHTRIH-M\DrvFR"; Check: SwitchHasValue('storagetype', 'hkcu')
Source: "Res\StorageSettings\hkcu\DrvFR.xml"; DestDir: "{commonappdata}\SHTRIH-M\DrvFR"; Check: SwitchHasValue('storagetype', 'hkcu_common')

; Version history
Source: "Res\History_ru.txt"; DestDir: "{app}"; DestName: "History.txt"; Flags: ${Archbit}; Components: main; Languages: ru
Source: "Res\History_en.txt"; DestDir: "{app}"; DestName: "History.txt"; Flags: ${Archbit}; Components: main; Languages: en
; Lcalization params
Source: "Res\Locales\ru\locale.ini"; DestDir: "{userappdata}\SHTRIH-M\DrvFR"; DestName: "locale.ini"; Flags: ${Archbit}; Components: main; Languages: ru
Source: "Res\Locales\en\locale.ini"; DestDir: "{userappdata}\SHTRIH-M\DrvFR"; DestName: "locale.ini"; Flags: ${Archbit}; Components: main; Languages: en
; VCL70
;Source: "res\vcl70.bpl"; DestDir: "{sys}"; Flags: onlyifdoesntexist sharedfile ${Archbit};
;Source: "res\rtl70.bpl"; DestDir: "{sys}"; Flags: onlyifdoesntexist sharedfile ${Archbit};
;Source: "res\BORLNDMM.DLL"; DestDir: "{sys}"; Flags: onlyifdoesntexist sharedfile ${Archbit};
;SQLite
Source: "res\${Arch}\sqlite3.dll"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main
;kktproxy
Source: "Bin\Win32\kktproxy.exe"; DestDir: "{app}\Bin\kktproxy"; Flags: sharedfile ${Archbit}; Components: kktproxy
Source: "res\kktproxy.ini"; DestDir: "{app}\Bin\kktproxy"; Flags: onlyifdoesntexist ${Archbit}; Components: kktproxy

;pppnetservice
Source: "Bin\Win32\pppnetsvc.exe"; DestDir: "{app}\Bin\pppnetservice"; Flags: sharedfile ${Archbit}; Components: pppnetservice
Source: "res\pppnetsvc.ini"; DestDir: "{app}\Bin\pppnetservice"; Flags: onlyifdoesntexist ${Archbit}; Components: pppnetservice
Source: "res\${Arch}\kktnetd.dll"; DestDir: "{app}\Bin\pppnetservice"; Flags: ${Archbit}; Components: pppnetservice

; Driver and test
Source: "Bin\${Arch}\DrvFR.dll"; DestDir: "{app}\Bin"; Flags: ignoreversion regserver ${Archbit}; Components: main
Source: "Bin\${Arch}\DrvFRTst.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; Components: main
Source: "Bin\${Arch}\SrvFR.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; Components: main
Source: "res\1C\32\shtrih-m_32_x32_64.zip"; DestDir: "{app}\Bin\1C"; Flags: ignoreversion ${Archbit}; Components: main
Source: "res\1C\40\shtrih-m_41_x32_64.zip"; DestDir: "{app}\Bin\1C"; Flags: ignoreversion ${Archbit}; Components: main

;Source: "Bin\SMDrvFR1CLib.dll"; DestDir: "{app}\Bin"; Flags: ignoreversion regserver ${Archbit}; Components: main
;Source: "Bin\SMDrvFR1CLib24.dll"; DestDir: "{app}\Bin"; Flags: ignoreversion regserver ${Archbit}; Components: main
;Source: "Bin\SMDrvFR1CNative.dll"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; Components: main
;Source: "..\..\Source\DrvFR\DrvFR.lic"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main


;Source: "Bin\TaxProg.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; Components: main
;Source: "Bin\TaxProg2.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; Components: main
;Source: "Bin\ServiceProg.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; Components: main

;Source: "Bin\DrvFRTst1C.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; Components: main
;Source: "res\1C\shtrih-m_15.zip"; DestDir: "{app}\Bin\1C"; Flags: ignoreversion ${Archbit}; Components: main
;Source: "res\1C\20\shtrih-m_20.zip"; DestDir: "{app}\Bin\1C"; Flags: ignoreversion ${Archbit}; Components: main
;Source: "res\1C\22\shtrih-m_22.zip"; DestDir: "{app}\Bin\1C"; Flags: ignoreversion ${Archbit}; Components: main
;Source: "res\1C\24\shtrih-m_25.zip"; DestDir: "{app}\Bin\1C"; Flags: ignoreversion ${Archbit}; Components: main

; OpenSSL
Source: "res\${Arch}\libeay32.dll"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main
Source: "res\${Arch}\ssleay32.dll"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main

; Fonts
Source: "res\Fonts\*"; DestDir: "{app}\Fonts"; Components: main

; PPP
;Source: "res\${Arch}\kktnetd.dll"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main
;Source: "res\${Arch}\kktnetd.dll"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main


;Source: "res\${Arch}\libeay32.dll"; DestDir: "{win}\System32"; Flags: onlyifdoesntexist sharedfile ${Archbit}; Components: main
;Source: "res\${Arch}\ssleay32.dll"; DestDir: "{win}\System32"; Flags: onlyifdoesntexist sharedfile ${Archbit}; Components: main

;Source: "res\${Arch}\libeay32.dll"; DestDir: "{win}\SysWOW64"; Flags: onlyifdoesntexist sharedfile ${Archbit}; Components: main
;Source: "res\${Arch}\ssleay32.dll"; DestDir: "{win}\SysWOW64"; Flags: onlyifdoesntexist sharedfile ${Archbit}; Components: main


;FiscalMaster
Source: "Bin\${Arch}\FiscalMaster.exe"; DestDir: "{app}\Bin"; Flags: ignoreversion ${Archbit}; Components: main
Source: "res\FiscalMaster\Templates\*.xml"; DestDir: "{commonappdata}\SHTRIH-M\FiscalMaster\Templates\"; Flags: ignoreversion createallsubdirs recursesubdirs; Components: main

; OKassa
Source: "Res\MROK\x32\MROK.exe"; DestDir: "{app}\Bin\OKassa"; Flags: ignoreversion ${Archbit}; Components: okassa; Check: not Is64BitInstallMode
Source: "res\${Arch}\libeay32.dll"; DestDir: "{app}\Bin\OKassa"; Flags: ${Archbit}; Components: okassa; Check: not Is64BitInstallMode
Source: "res\${Arch}\ssleay32.dll"; DestDir: "{app}\Bin\OKassa"; Flags: ${Archbit}; Components: okassa; Check: not Is64BitInstallMode

Source: "Res\MROK\x64\MROKx64.exe"; DestDir: "{app}\Bin\OKassa"; Flags: ignoreversion ${Archbit}; Components: okassa; Check: Is64BitInstallMode
Source: "res\${Arch}\libeay32.dll"; DestDir: "{app}\Bin\OKassa"; Flags: ${Archbit}; Components: okassa; Check: Is64BitInstallMode
Source: "res\${Arch}\ssleay32.dll"; DestDir: "{app}\Bin\OKassa"; Flags: ${Archbit}; Components: okassa; Check: Is64BitInstallMode

Source: "Res\DrvFR.lic"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main
;Source: "Res\OKassa\OKassaMaster.exe"; DestDir: "{app}\Bin\OKassa"; Flags: ignoreversion ${Archbit}; Components: okassa
;Source: "Res\OKassa\Doc\*"; DestDir: "{app}\Bin\OKassa\Doc"; Flags: recursesubdirs createallsubdirs; Components: okassa
; Payment system
;Source: "Bin\PaymentDrv.dll"; DestDir: "{app}\Bin"; Flags: ignoreversion regserver ${Archbit}; Components: main
;Source: "Res\PaymentDrv.xml"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main
; Models
Source: "Res\Models.xml"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main
Source: "Res\TextMap.xml"; DestDir: "{app}\Bin"; Flags: ${Archbit}; Components: main
Source: "Res\Languages\*"; DestDir: "{app}\Bin\Languages\"; Flags: ${Archbit}; Components: main
; Escape library
;Source: "Res\Escape.dll"; DestDir: "{sys}"; Flags: ${Archbit}
; Samples
Source: "..\..\Samples\*"; DestDir: "{app}\Samples"; Flags: recursesubdirs createallsubdirs ${Archbit}; Components: Samples;
; Samples - 1C version 7.5
;Source: "Bin\DrvFR.dll"; DestDir: "{app}\Samples\1C\1C 7.5"; Flags: ignoreversion ${Archbit}; Components: Samples
;Source: "..\..\Source\DrvFR\DrvFR.lic"; DestDir: "{app}\Samples\1C\1C 7.5"; Flags: ${Archbit}; Components: Samples
;Source: "Res\Models.xml"; DestDir: "{app}\Samples\1C\1C 7.5"; Flags: ${Archbit}; Components: Samples
; Samples - 1C version 7.7
;Source: "Bin\DrvFR.dll"; DestDir: "{app}\Samples\1C\1C 7.7"; Flags: ignoreversion ${Archbit}; Components: Samples
;Source: "..\..\Source\DrvFR\DrvFR.lic"; DestDir: "{app}\Samples\1C\1C 7.7"; Flags: ${Archbit}; Components: Samples
;Source: "Res\Models.xml"; DestDir: "{app}\Samples\1C\1C 7.7"; Flags: ${Archbit}; Components: Samples
; Images
Source: "Res\Images\Logo.bmp"; DestDir: "{app}\Images\"; Flags: ${Archbit}; Components: images
Source: "Res\Images\Testpic_50x50.bmp"; DestDir: "{app}\Images\"; Flags: ${Archbit}; Components: images
Source: "Res\Images\Testpic_100x100.bmp"; DestDir: "{app}\Images\"; Flags: ${Archbit}; Components: images
Source: "Res\Images\Black_320x1200.bmp"; DestDir: "{app}\Images\"; Flags: ${Archbit}; Components: images
; Locales
Source: "..\..\Bin\Win32\Release\locale\en\LC_MESSAGES\DrvFR.mo"; DestDir: "{app}\Bin\locale\en\LC_MESSAGES\"; Flags: ${Archbit}; Components: main
Source: "..\..\Bin\Win32\Release\locale\en\LC_MESSAGES\DrvFRTst.mo"; DestDir: "{app}\Bin\locale\en\LC_MESSAGES\"; Flags: ${Archbit}; Components: main
; DFU drivers
Source: "Res\DFU\Driver\*"; DestDir: "{app}\Bin\DFU\Driver"; Flags: recursesubdirs createallsubdirs ${Archbit}; Components: usbdrivers; AfterInstall: InstallDrivers
Source: "Res\DFU\Util\*"; DestDir: "{app}\Bin\DFU\Util"; Flags: recursesubdirs createallsubdirs ${Archbit}; Components: main 
; usb drivers
Source: "Res\Drivers\*"; DestDir: "{app}\Bin\Drivers"; Flags: recursesubdirs createallsubdirs ${Archbit}; Components: usbdrivers; AfterInstall: InstallDrivers
[Icons]
; Version history
Name: "{group}\{cm:VersionHistory}"; Filename: "{app}\History.txt"; Components: main
; Main
Name: "{group}\{cm:DriverTest} ${version2} ${Arch2}"; Filename: "{app}\Bin\DrvFRTst.exe"; WorkingDir: "{app}\Bin"; Components: main
;Name: "{group}\{cm:DriverTest1C} ${version2}"; Filename: "{app}\Bin\DrvFRTst1C.exe"; WorkingDir: "{app}\Bin"; Components: main
;Name: "{group}\{cm:ServiceProg}"; Filename: "{app}\Bin\ServiceProg.exe"; WorkingDir: "{app}\Bin"; Components: main
Name: "{group}\{cm:FiscalMaster} ${version2} ${Arch2}"; Filename: "{app}\Bin\FiscalMaster.exe"; WorkingDir: "{app}\Bin"; Components: main
;Name: "{group}\{cm:TaxProg2}"; Filename: "{app}\Bin\TaxProg2.exe"; WorkingDir: "{app}\Bin"; Components: main
Name: "{group}\{cm:ServerFR} ${version2} ${Arch2}"; Filename: "{app}\Bin\SrvFR.exe"; WorkingDir: "{app}\Bin"; Components: main
Name: "{group}\{cm:OKassaFolder}\{cm:OKassaMaster}"; Filename: "{app}\Bin\OKassa\MROK.exe"; WorkingDir: "{app}\Bin\OKassa"; Components: okassa; Check: not Is64BitInstallMode
Name: "{group}\{cm:OKassaFolder}\{cm:OKassaMaster}"; Filename: "{app}\Bin\OKassa\MROKx64.exe"; WorkingDir: "{app}\Bin\OKassa"; Components: okassa; Check: Is64BitInstallMode

;Name: "{group}\{cm:OKassaFolder}\{cm:OKassaMaster}"; Filename: "{app}\Bin\OKassa\OKassaMaster.exe"; WorkingDir: "{app}\Bin\OKassa"; Components: okassa
;Name: "{group}\{cm:OKassaFolder}\{cm:OKassaDoc}"; Filename: "{app}\Bin\OKassa\Doc\Облачная Касса (Инструкция администратора личного кабинета)_Версия_1.3_310315.htm";Components: okassa
Name: "{group}\{cm:UninstallShortcutText} Драйвер ККТ ${version2} ${Arch2}"; Filename: "{uninstallexe}"
;Samples
Name: "{group}\Samples"; Filename: "{app}\Samples"; WorkingDir: "{app}"; Components: Samples
;Name: "{group}\Samples\Borland C++ Builder 6.0"; Filename: "{app}\Samples\Borland C++ Builder 6.0"; WorkingDir: "{app}"; Components: Samples
;Name: "{group}\Samples\1Cv7.5"; Filename: "{app}\Samples\1C\1C 7.5"; WorkingDir: "{app}"; Components: Samples
;Name: "{group}\Samples\1Cv7.7"; Filename: "{app}\Samples\1C\1C 7.7"; WorkingDir: "{app}"; Components: Samples
; Shortcuts
Name: "{userdesktop}\{cm:DriverTest} ${version2} ${Arch2}"; Filename: "{app}\Bin\DrvFRTst.exe"; Tasks: desktopicon
Name: "{userdesktop}\{cm:FiscalMaster} ${version2} ${Arch2}"; Filename: "{app}\Bin\FiscalMaster.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{cm:DriverTest} ${version2} ${Arch2}"; Filename: "{app}\Bin\DrvFRTst.exe"; Tasks: quicklaunchicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{cm:FiscalMaster} ${version2} ${Arch2}"; Filename: "{app}\Bin\FiscalMaster.exe"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\Bin\SrvFR.exe"; Parameters: "/regserver"; Flags: nowait; Components: main
Filename: "{app}\Bin\DrvFRTst.exe"; Description: {cm:StartApplication}; Flags: postinstall nowait skipifsilent skipifdoesntexist; Components: main
Filename: "{app}\Bin\OKassa\MROK.exe"; Description: {cm:StartOkassa}; Flags: postinstall nowait skipifsilent skipifdoesntexist; Components: okassa; Check: not Is64BitInstallMode
Filename: "{app}\Bin\OKassa\MROKx64.exe"; Description: {cm:StartOkassa}; Flags: postinstall nowait skipifsilent skipifdoesntexist; Components: okassa; Check: Is64BitInstallMode
[Registry]
; 1C samples menu
Root: HKCU; Subkey: "Software\1C\1Cv7\7.5\Titles"; ValueType: string; ValueName: "{app}\Samples\1C\1C 7.5\"; ValueData: {cm:AppName}; Flags: uninsdeletevalue; Components: Samples
Root: HKCU; Subkey: "Software\1C\1Cv7\7.7\Titles"; ValueType: string; ValueName: "{app}\Samples\1C\1C 7.7\"; ValueData: {cm:AppName}; Flags: uninsdeletevalue; Components: Samples
; Clear trial timestamp
Root: HKLM; Subkey: "Software\ShtrihM\DrvFR\Param"; ValueType: none; ValueName: "LCTStamp"; Flags: deletevalue uninsdeletevalue;
; Driver version
Root: HKCU; Subkey: "Software\ShtrihM\DrvFR\Param"; ValueType: string; ValueName: "DriverVersion"; ValueData: "${version}"; Flags: uninsdeletevalue
