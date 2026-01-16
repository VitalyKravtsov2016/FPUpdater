unit ComportUtils;

interface

uses
  Windows, Classes, SysUtils, jvSetupAPI, Winapi.Messages,
  System.Generics.Collections, System.Win.Registry, logfile;

type
  TSerialPortRec = record
    Name: string;
    FriendlyName: string;
    HWStr: string;

    ComNumber: Integer;
    function vid: Integer;
    function pid: Integer;
  end;

  TSerialPorts = class(TList<TSerialPortRec>)
  public
    function IndexOfComNumber(AComNumber: Integer): Integer;
  end;



//function EnumerateUsbCom(VID, PID: Integer; Ports: TStrings): Integer;
//procedure HWStrToVidPid(const AStr: string; var Vid, Pid: Integer);
//function HWStrToComNames(const AStr: string): string;

procedure EnumPorts(Ports: TSerialPorts);

implementation

const
  PortsGUID: TGUID = '{4D36E978-E325-11CE-BFC1-08002BE10318}'; // ports
// \\?\USB#Vid_abcd&Pid_1980#5&351c4ca5&0&2#{a5dcbf10-6530-11d2-901f-00c04fb951ed}

function HexToIntDef(const AStr: string; ADefValue: Integer): Integer;
var
  Code: Integer;
begin
  Val('$' + AStr, Result, Code);
  if Code <> 0 then
    Result := 0;
end;

procedure HWStrToVidPid(const AStr: string; var Vid, Pid: Integer);
var
  k: Integer;
  S: string;
begin
  Vid := 0;
  Pid := 0;
  k := Pos('vid_', AnsiLowercase(AStr));
  if k <= 0 then
    Exit;
  S := Copy(AStr, k + 4, 4);

  Vid := HexToIntDef(S, 0);

  k := Pos('pid_', AnsiLowerCase(AStr));
  if k <= 0 then
    Exit;
  S := Copy(AStr, k + 4, 4);
  Pid := HexToIntDef(S, 0);
end;


{ TSerialPorts }

function TSerialPorts.IndexOfComNumber(AComNumber: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].ComNumber = AComNumber then
      Exit(i);
  end;
end;

function ExtractPortNumber(const AName: string): Integer;
var
  k: Integer;
  s: string;
begin
  Result := -1;
  if Pos('(LPT', AName) > 0 then
    Exit;
  k := Pos('(COM', AName);
  if k = 0 then
    Exit;
  s := Copy(AName, k + 4, Length(AName));
  k := Pos(')', s);
  if k = 0 then
    Exit;
  s := Copy(s, 1, k - 1);
  Result := StrToIntDef(s, -1);
end;

procedure EnumPorts(Ports: TSerialPorts);
var
  RequiredSize: Cardinal;
  GUIDSize: DWORD;
  Guid: TGUID;
  DevInfoHandle: HDEVINFO;
  DeviceInfoData: TSPDevInfoData;
  MemberIndex: Cardinal;
  PropertyRegDataType: DWord;
  RegProperty: Cardinal;
  S1: string;
  PortRec: TSerialPortRec;
begin
  Ports.Clear;
  if not LoadsetupAPI then
    exit;
  GUIDSize := 1;
  if SetupDiClassGuidsFromName('Ports', @Guid, GUIDSize, RequiredSize) then
  begin
    DevInfoHandle := SetupDiGetClassDevs(@Guid, nil, 0, DIGCF_PRESENT);
    if Cardinal(DevInfoHandle) <> Invalid_Handle_Value then
    begin
      try
        MemberIndex := 0;
         //iterate device list
        repeat
          PortRec.ComNumber := -1;
          FillChar(DeviceInfoData, SizeOf(DeviceInfoData), 0);
          DeviceInfoData.cbSize := SizeOf(DeviceInfoData);
           //get device info that corresponds to the next memberindex
          if not SetupDiEnumDeviceInfo(DevInfoHandle, MemberIndex, DeviceInfoData) then
            break;
          RegProperty := SPDRP_FriendlyName; {SPDRP_Driver, SPDRP_SERVICE, SPDRP_ENUMERATOR_NAME,SPDRP_PHYSICAL_DEVICE_OBJECT_NAME,SPDRP_FRIENDLYNAME,}

          SetupDiGetDeviceRegistryProperty(DevInfoHandle, DeviceInfoData, RegProperty, PropertyRegDataType, nil, 0, RequiredSize);
          SetLength(S1, RequiredSize);

          if SetupDiGetDeviceRegistryProperty(DevInfoHandle, DeviceInfoData, RegProperty, PropertyRegDataType, @S1[1], RequiredSize, RequiredSize) then
          begin
            PortRec.FriendlyName := TrimRight(PChar(@S1[1]));
           // GlobalLogger.Debug('S1: ' + S1);
            PortRec.ComNumber := ExtractPortNumber(PortRec.FriendlyName);
            PortRec.Name := 'COM' + PortRec.ComNumber.ToString;

          end
          else
            Continue;

          RegProperty := SPDRP_HARDWAREID;

          SetupDiGetDeviceRegistryProperty(DevInfoHandle, DeviceInfoData, RegProperty, PropertyRegDataType, nil, 0, RequiredSize);
          SetLength(S1, RequiredSize);

          if SetupDiGetDeviceRegistryProperty(DevInfoHandle, DeviceInfoData, RegProperty, PropertyRegDataType, @S1[1], RequiredSize, RequiredSize) then
          begin
            PortRec.HWStr := TrimRight(PChar(@S1[1]));
           // GlobalLogger.Debug('S1: ' + S1);
          end
          else
            Continue;

          if PortRec.ComNumber >= 0 then
          begin
            Ports.Add(PortRec);
            //GlobalLogger.Debug('  ' + PortRec.Name);
          end;
          Inc(MemberIndex);
        until False;
      finally
        SetupDiDestroyDeviceInfoList(DevInfoHandle);
      end;
    end;
  end;
end;

{ TSerialPortRec }

function TSerialPortRec.Pid: Integer;
var
  v: Integer;
begin
  HWStrToVidPid(HWStr, v, Result);
end;

function TSerialPortRec.Vid: Integer;
var
  p: Integer;
begin
  HWStrToVidPid(HWStr, Result, p);
end;

end.

