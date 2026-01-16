unit SearchPort;

{$J+}

interface

uses
  // VCL
  Windows, SysUtils, Classes, ComObj, ActiveX, SyncObjs, Types,
  // This
  NotifyThread, DrvFRLib_TLB, LogFile, LangUtils, DriverTypes, PrinterTypes,
  untUtil;

type
  { TSearchPortRec }

  TSearchPortRec = record
    PortName: string;
    FriendlyName: string;
    PortNumber: Integer;
  end;

  TSearchPort = class;

  { TSearchPorts }

  TSearchPorts = class
  private
    FList: TList;
    FLock: TCriticalSection;

    procedure InsertItem(AItem: TSearchPort);
    procedure RemoveItem(AItem: TSearchPort);

    function GetCount: Integer;
    function GetItem(Index: Integer): TSearchPort;
    function ValidIndex(AIndex: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Lock;
    procedure Unlock;
    procedure ClearState;
    function Add: TSearchPort;
    function Completed: Boolean;
    function ItemByID(AID: Integer): TSearchPort;
    function ItemByIndex(AIndex: Integer): TSearchPort;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSearchPort read GetItem; default;
  end;

  { TSearchPort }

  TSearchPort = class
  private
    FID: Integer;
    FText: WideString;
    FStopFlag: Boolean;
    FSelected: Boolean;
    FBaudRate: Integer;
    FDevicePort: Integer;
    FDeviceFound: Boolean;
    FBaudRateText: WideString;
    FOwner: TSearchPorts;
    FData: TSearchPortRec;
    FCompleted: Boolean;
    FThread: TNotifyThread;
    FDefSysPassword: Integer;
    FDoTechReset: Boolean;
    FSerialNumber: string;
    FShortSearch: Boolean;
    FConnectionType: Integer;
    FNoBaudrate: Boolean;

    function GetRes: WideString;
    function GetBaudRateText: WideString;
    procedure ThreadProc(Sender: TObject);
    procedure SetText(const Value: WideString);
    procedure SetOwner(AOwner: TSearchPorts);
    procedure SetData(const Value: TSearchPortRec);
    procedure SetBaudRateText(const Value: WideString);
    procedure SearchByBaudRates(Driver: IDrvFR48);
  public
    constructor Create(AOwner: TSearchPorts);
    destructor Destroy; override;

    procedure Start;
    procedure Wait;
    procedure Stop;

    procedure Lock;
    procedure Unlock;
    procedure ClearState;
    procedure SearchDevice;

    property ID: Integer read FID;
    property BaudRate: Integer read FBaudRate;
    property Completed: Boolean read FCompleted;
    property PortName: string read FData.PortName;
    property FriendlyName: string read FData.FriendlyName;
    property Text: WideString read GetRes write SetText;
    property PortNumber: Integer read FData.PortNumber;
    property Data: TSearchPortRec read FData write SetData;
    property Selected: Boolean read FSelected write FSelected;
    property DeviceFound: Boolean read FDeviceFound write FDeviceFound;
    property BaudRateText: WideString read GetBaudRateText write SetBaudRateText;
    property DefSysPassword: Integer read FDefSysPassword write FDefSysPassword;
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    property DoTechReset: Boolean read FDoTechReset write FDoTechReset;
    property ShortSearch: Boolean read FShortSearch write FShortSearch; // поиск на 115200 и 4800
    property ConnectionType: Integer read FConnectionType write FConnectionType;
    property NoBaudrate: Boolean read FNoBaudrate write FNoBaudrate;
    property DevicePort: Integer read FDevicePort;
  end;

implementation

resourcestring
  SDeviceNotFound = 'не найдено';
  SSearchIsInProgress = 'идет поиск...';

{ TSearchPorts }

constructor TSearchPorts.Create;
begin
  inherited Create;
  FList := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TSearchPorts.Destroy;
begin
  Clear;
  FList.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TSearchPorts.Clear;
begin
  while Count > 0 do Items[0].Free;
end;

function TSearchPorts.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSearchPorts.GetItem(Index: Integer): TSearchPort;
begin
  Result := FList[Index];
end;

procedure TSearchPorts.InsertItem(AItem: TSearchPort);
begin
  FList.Add(AItem);
  AItem.FOwner := Self;
end;

procedure TSearchPorts.RemoveItem(AItem: TSearchPort);
begin
  AItem.FOwner := nil;
  FList.Remove(AItem);
end;

procedure TSearchPorts.Lock;
begin
  FLock.Enter;
end;

procedure TSearchPorts.Unlock;
begin
  FLock.Leave;
end;

function TSearchPorts.ItemByID(AID: Integer): TSearchPort;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if Result.ID = AID then Exit;
  end;
  Result := nil;
end;

function TSearchPorts.ValidIndex(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0)and(AIndex < Count);
end;

function TSearchPorts.ItemByIndex(AIndex: Integer): TSearchPort;
begin
  Result := nil;
  if ValidIndex(AIndex) then
    Result := Items[AIndex];
end;

function TSearchPorts.Add: TSearchPort;
begin
  Result := TSearchPort.Create(Self);
end;

procedure TSearchPorts.ClearState;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Items[i].ClearState;
end;

function TSearchPorts.Completed: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count-1 do
  begin
    Result := Items[i].Completed;
    if not Result then Break;
  end;
end;

{ TSearchPort }

constructor TSearchPort.Create(AOwner: TSearchPorts);
const
  LastID: Integer = 0;
begin
  inherited Create;
  SetOwner(AOwner);
  FDoTechReset := True;
  FShortSearch := False;
  Inc(LastID); FID := LastID;
end;

destructor TSearchPort.Destroy;
begin
  Stop;
  FThread.Free;
  SetOwner(nil);
  inherited Destroy;
end;

procedure TSearchPort.SetOwner(AOwner: TSearchPorts);
begin
  if AOwner <> FOwner then
  begin
    if FOwner <> nil then FOwner.RemoveItem(Self);
    if AOwner <> nil then AOwner.InsertItem(Self);
  end;
end;

procedure TSearchPort.Lock;
begin
  FOwner.Lock;
end;

procedure TSearchPort.Unlock;
begin
  FOwner.Unlock;
end;

procedure TSearchPort.Wait;
begin
  if FThread <> nil then
    FThread.WaitFor;
end;

procedure TSearchPort.SetData(const Value: TSearchPortRec);
begin
  Lock;
  try
    FData := Value;
  finally
    Unlock;
  end;
end;

function TSearchPort.GetRes: WideString;
begin
  Lock;
  try
    Result := FText;
  finally
    Unlock;
  end;
end;

procedure TSearchPort.SetText(const Value: WideString);
begin
  Lock;
  try
    FText := Value;
  finally
    Unlock;
  end;
end;

procedure TSearchPort.ClearState;
begin
  Text := '';
  FCompleted := False;
  BaudRateText := '';
  DeviceFound := False;
  DefSysPassword := 30;
end;

function TSearchPort.GetBaudRateText: WideString;
begin
  Lock;
  try
    Result := FBaudRateText;
  finally
    Unlock;
  end;
end;

procedure TSearchPort.SetBaudRateText(const Value: WideString);
begin
  Lock;
  try
    FBaudRateText := Value;
  finally
    Unlock;
  end;
end;

procedure TSearchPort.SearchDevice;
var
  Driver: IDrvFR49;
  ResultCode: Integer;
begin
  if not Selected then Exit;

  FStopFlag := False;
  Driver := CreateComObject(CLASS_DrvFR) as IDrvFR49;
  try
    Driver.OFDExchangeSuspended := True;
    Driver.UpdateFirmwareSuspended := True;
    Driver.PluginsEnabled := False;
    Driver.ConnectionType := FConnectionType;
    Driver.ComNumber := PortNumber;
    ResultCode := Driver.LockPort;
    if ResultCode = 0 then
    begin
      Text := LangUtils.GetRes(@SSearchIsInProgress);
      try
        SearchByBaudRates(Driver);
      finally
        Driver.UnlockPort;
      end;
    end else
    begin
      Text := Driver.ResultCodeDescription;
    end;
  finally
    Driver.Disconnect;
    Driver := nil;
  end;
end;

procedure TSearchPort.SearchByBaudRates(Driver: IDrvFR48);

procedure Check(ACode: Integer);
begin
  if ACode <> 0 then
    raise Exception.Create(IntToStr(ACode) + ', ' + Driver.ResultCodeDescription);
end;

var
  BaudRate: Integer;
  ResultCode: Integer;
  Res: Integer;
  Dat: TDateTime;
  i: Integer;
begin
  DeviceFound := False;
  Driver.ConnectionType := CT_LOCAL;
  Driver.ProtocolType := 0; // Standard
  for i := Low(BAUD_RATE_CODE_SEARCH_ORDER) to High(BAUD_RATE_CODE_SEARCH_ORDER) do
  begin
    if FStopFlag then
      raise EAbort.Create(SDeviceNotFound);
    if (ShortSearch) and (i = 3) then
      raise EAbort.Create(SDeviceNotFound);
    if (NoBaudrate) and (i = 2) then
      raise EAbort.Create(SDeviceNotFound);

    BaudRate := BAUD_RATE_CODE_SEARCH_ORDER[i];
    Driver.BaudRate := BaudRate;
    FBaudRate := BaudRate;
    BaudRateText := BaudRateToStr(BaudRate);

    Driver.Disconnect;
    Driver.Timeout := Driver.SearchTimeout;
    ResultCode := Driver.GetDeviceMetrics;
    if ResultCode = E_NOERROR then
    begin
      SerialNumber := '';
      if FDoTechReset then
      begin
        Logger.Debug('Разрешено техобнуление: ' + BoolToStr[DoTechReset]);
        try
          Driver.Timeout := 5000;
          Res := Driver.ReadSerialNumber;
          if Res = 0 then
          begin
            SerialNumber := Driver.SerialNumber;
          end;
          if (Res = $74) or (Res = $79) or (Res = $78)  then
          begin
            Res := Driver.ResetSettings;
            if Res = 147 then
              Check(Driver.ResetSettings)
            else
              Check(Res);

            Dat := Date;
            Driver.Date := Dat;
            Check(Driver.SetDate);
            Driver.Date := Dat;
            Check(Driver.ConfirmDate);
            Driver.Time := Time;
            Check(Driver.SetTime);
            Check(Driver.ReadSerialNumber);
            SerialNumber := Driver.SerialNumber;
          end;
          Check(Res);
          Check(Driver.GetECRStatus);
          FDevicePort := Driver.PortNumber;
        except
          on E: Exception do
          begin
            Logger.Error('Ошибка: ' + E.Message);
          end;
        end;
      end;
      DeviceFound := ResultCode >= E_NOERROR;
      Driver.ModelParamNumber := mpDefaultSysPassword;
      Driver.ReadModelParamValue;
      Text := Driver.UDescription; //TrimRight(Driver.UDescription);
      FDefSysPassword := Driver.ModelParamValue;
      Logger.Debug('Устройство найдено, ' + Text);
      Break;
    end
    else
    begin
      if ResultCode <> E_NOHARDWARE then
      begin
        Text := Format('%d: %s', [ResultCode, Driver.ResultCodeDescription]);
        //Logger.Debug('Устройство не найдено, ' + Text);
        Break;
      end;
    end;

  end;
  if ResultCode = E_NOHARDWARE then
    Text := LangUtils.GetRes(@SDeviceNotFound);
end;

procedure TSearchPort.Start;
begin
  ClearState;
  FThread.Free;
  FThread := TNotifyThread.CreateThread(ThreadProc);
  FThread.Resume;
end;

procedure TSearchPort.ThreadProc(Sender: TObject);
begin
  try
    OleCheck(CoInitialize(nil));
    try
      SearchDevice;
    finally
      CoUninitialize;
    end;
  except
    on E: Exception do
    begin
      Logger.Error('Ошибка: ' + E.Message);
      Text := E.Message;
    end;
  end;
  FCompleted := True;
end;

procedure TSearchPort.Stop;
begin
  FStopFlag := True;
end;

end.
