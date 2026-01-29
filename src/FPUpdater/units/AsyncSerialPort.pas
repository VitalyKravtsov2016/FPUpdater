unit AsyncSerialPort;

interface

uses
  // VCL
  Windows, Messages, SysUtils, SyncObjs, Classes;

type
  TCommEvent = procedure(Sender: TObject; Status: dword) of object;

  TCommEventThread = class(TThread)
  private
    FWaitMask: DWORD;
    FCommHandle: THandle;
    FEvent: TSimpleEvent;
    FEventMask: Integer;
    FOnSignal: TCommEvent;
  protected
    procedure Execute; override;
    procedure DoOnSignal;
  public
    constructor Create(Handle: THandle; Events: DWORD);
    destructor Destroy; override;
    property OnSignal: TCommEvent read FOnSignal write FOnSignal;
  end;

  TCustomComm = class;

  TCommEventChars = class(TPersistent)
  private
    FOwner: TCustomComm;
    FXonChar: AnsiChar;
    FXoffChar: AnsiChar;
    FErrorChar: AnsiChar;
    FEofChar: AnsiChar;
    FEvtChar: AnsiChar;
    procedure SetEventChar(Index: Integer; Value: AnsiChar);
  public
    constructor Create(Owner: TCustomComm);
    procedure Assign(Source: TPersistent); override;
  published
    property XonChar: AnsiChar index 1 read FXOnChar write SetEventChar default #17;
    property XoffChar: AnsiChar index 2 read FXOffChar write SetEventChar default #19;
    property ErrorChar: AnsiChar index 3 read FErrorChar write SetEventChar default #0;
    property EofChar: AnsiChar index 4 read FEofChar write SetEventChar default #0;
    property EvtChar: AnsiChar index 5 read FEvtChar write SetEventChar default #0;
  end;

  TFlowControl = (fcNone, fcCTS, fcDTR, fcSoftware, fcDefault);

  TCommOption = (coParityCheck, coDsrSensitivity, coIgnoreXOff,
    coErrorChar, coNullStrip);
  TCommOptions = set of TCommOption;

  TCommRxCharEvent = procedure(Sender: TObject; Count: Integer) of object;
  TCommErrorEvent = procedure(Sender: TObject; Errors: Integer) of object;

  TCustomComm = class
  private
    FDCB: TDCB;
    FHandle: THandle;
    FReadEvent: TSimpleEvent;
    FWriteEvent: TSimpleEvent;
    FCriticalSection: TCriticalSection;
    FReadTimeout: Integer;
    FWriteTimeout: Integer;
    FReadBufSize: Integer;
    FWriteBufSize: Integer;
    FMonitorEvents: DWORD;
    FBaudRate: Integer;
    FParity: Integer;
    FStopbits: Integer;
    FDatabits: Integer;
    FEventThread: TCommEventThread;
    FEventChars: TCommEventChars;
    FOptions: TCommOptions;
    FFlowControl: TFlowControl;
    FOnBreak: TNotifyEvent;
    FOnCts: TNotifyEvent;
    FOnDsr: TNotifyEvent;
    FOnError: TCommErrorEvent;
    FOnRing: TNotifyEvent;
    FOnRlsd: TNotifyEvent;
    FOnRxChar: TCommRxCharEvent;
    FOnRxFlag: TNotifyEvent;
    FOnTxEmpty: TNotifyEvent;
    FPortName: string;
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetBaudRate(Value: Integer);
    procedure SetParity(Value: Integer);
    procedure SetStopbits(Value: Integer);
    procedure SetDatabits(Value: Integer);
    procedure SetOptions(Value: TCommOptions);
    procedure SetFlowControl(Value: TFlowControl);
    function GetModemState(Index: Integer): Boolean;
    function GetComState(Index: Integer): Boolean;
    procedure Lock;
    procedure Unlock;
    procedure EscapeComm(Flag: Integer);
    procedure InitHandshaking(var DCB: TDCB);
    procedure UpdateCommTimeouts;
    procedure SetPortName(Value: string);
    procedure SetSignals;
    //procedure RaiseError(const MethodName: AnsiString);
  public
    procedure CreateHandle;
    procedure DestroyHandle;
    procedure HandleCommEvent(Sender: TObject; Status: dword);
    procedure UpdateDataControlBlock;
    property PortName: string read FPortName write SetPortName;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout default 1000;
    property WriteTimeout: Integer read FWriteTimeout write FWriteTimeout default 1000;
    property ReadBufSize: Integer read FReadBufSize write FReadBufSize default 1024;
    property WriteBufSize: Integer read FWriteBufSize write FWriteBufSize default 1024;
    property MonitorEvents: DWORD read FMonitorEvents write FMonitorEvents;
    property BaudRate: Integer read FBaudRate write SetBaudRate default CBR_9600;
    property Parity: Integer read FParity write SetParity default PARITY_NONE;
    property Stopbits: Integer read FStopbits write SetStopbits default STOPBITS_10;
    property Databits: Integer read FDatabits write SetDatabits default DATABITS_8;
    property EventChars: TCommEventChars read FEventChars;
    property Options: TCommOptions read FOptions write SetOptions;
    property FlowControl: TFlowControl read FFlowControl write SetFlowControl default fcNone;
    property OnBreak: TNotifyEvent read FOnBreak write FOnBreak;
    property OnCts: TNotifyEvent read FOnCts write FOnCts;
    property OnDsr: TNotifyEvent read FOnDsr write FOnDsr;
    property OnRing: TNotifyEvent read FOnRing write FOnRing;
    property OnRlsd: TNotifyEvent read FOnRlsd write FOnRlsd;
    property OnError: TCommErrorEvent read FOnError write FOnError;
    property OnRxChar: TCommRxCharEvent read FOnRxChar write FOnRxChar;
    property OnRxFlag: TNotifyEvent read FOnRxFlag write FOnRxFlag;
    property OnTxEmpty: TNotifyEvent read FOnTxEmpty write FOnTxEmpty;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure Write(Data: AnsiString);
    function Read(Count: Integer): AnsiString;

    function InQueCount: Integer;
    function OutQueCount: Integer;
    procedure PurgeIn;
    procedure PurgeOut;
    {Comm escape functions}
    procedure SetDTRState(State: Boolean);
    procedure SetRTSState(State: Boolean);
    procedure SetBREAKState(State: Boolean);
    procedure SetXONState(State: Boolean);
    {Comm status flags}
    property CTS: Boolean index 1 read GetModemState;
    property DSR: Boolean index 2 read GetModemState;
    property RING: Boolean index 3 read GetModemState;
    property RLSD: Boolean index 4 read GetModemState;

    property CtsHold: Boolean index 1 read GetComState;
    property DsrHold: Boolean index 2 read GetComState;
    property RlsdHold: Boolean index 3 read GetComState;
    property XoffHold: Boolean index 4 read GetComState;
    property XOffSent: Boolean index 5 read GetComState;

    property Handle: THandle read FHandle;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { TAsyncSerialPort }

  TAsyncSerialPort = class(TCustomComm);

function GetProviderSubTypeName(Id: Integer): AnsiString;

implementation

function StrToHex(const S: AnsiString): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    if i <> 1 then Result := Result + ' ';
    Result := Result + IntToHex(Ord(S[i]), 2);
  end;
end;

procedure ODS(const S: string);
begin
{$IFDEF DEBUG}
  //OutputDebugString(PChar(S));
{$ENDIF}
end;

const
  PurgeRead = PURGE_RXABORT + PURGE_RXCLEAR;
  PurgeWrite = PURGE_TXABORT + PURGE_TXCLEAR;
  PurgeReadWrite = PurgeRead + PurgeWrite;

  fBinary = $00000001;
  fParity = $00000002;
  fOutxCtsFlow = $00000004;
  fOutxDsrFlow = $00000008;
  fDtrControl = $00000030;
  fDtrControlDisable = $00000000;
  fDtrControlEnable = $00000010;
  fDtrControlHandshake = $00000020;
  fDsrSensitivity = $00000040;
  fTXContinueOnXoff = $00000080;
  fOutX = $00000100;
  fInX = $00000200;
  fErrorChar = $00000400;
  fNull = $00000800;
  fRtsControl = $00003000;
  fRtsControlDisable = $00000000;
  fRtsControlEnable = $00001000;
  fRtsControlHandshake = $00002000;
  fRtsControlToggle = $00003000;
  fAbortOnError = $00004000;
  fDummy2 = $FFFF8000;

  CommOptions: array[TCommOption] of Integer =
  (fParity, fDsrSensitivity, fTXContinueOnXoff, fErrorChar, fNull);

function GetProviderSubTypeName(Id: Integer): AnsiString;
begin
  case Id of
    PST_FAX: Result := 'FAX device';
    PST_LAT: Result := 'LAT protocol';
    PST_MODEM: Result := 'Modem device';
    PST_NETWORK_BRIDGE: Result := 'Unspecified network bridge';
    PST_PARALLELPORT: Result := 'Parallel port';
    PST_RS232: Result := 'RS-232 serial port';
    PST_RS422: Result := 'RS-422 port';
    PST_RS423: Result := 'RS-423 port';
    PST_RS449: Result := 'RS-449 port';
    PST_SCANNER: Result := 'Scanner device';
    PST_TCPIP_TELNET: Result := 'TCP/IP Telnet protocol';
    PST_UNSPECIFIED: Result := 'Unspecified';
    PST_X25: Result := 'X.25 standards';
  else
    Result := 'Unknown provider id';
  end;
end;

{ TCommEventThread }

constructor TCommEventThread.Create(Handle: THandle; Events: DWORD);
begin
  inherited Create(True);
  FEvent := TSimpleEvent.Create;
  FCommHandle := Handle;
  FWaitMask := Events;
  SetCommMask(FCommHandle, Events);
  Priority := tpHigher;
  Resume;
end;

destructor TCommEventThread.Destroy;
begin
  Terminate;
  FEvent.Free;
  inherited Destroy;
end;

procedure TCommEventThread.Execute;

  procedure ClearError;
  var
    Errors: DWORD;
    ComStat: TComStat;
  begin
    ClearCommError(FCommHandle, Errors, @ComStat);
  end;

var
  Overlapped: TOverlapped;
  WaitEventResult: Boolean;
begin
  try
    FillChar(Overlapped, Sizeof(Overlapped), 0);
    Overlapped.hEvent := FEvent.Handle;
    while not Terminated do
    begin
      WaitEventResult := WaitCommEvent(FCommHandle, DWORD(FEventMask), @Overlapped);
      if (GetLastError = ERROR_IO_PENDING) then
      begin
        WaitEventResult := (FEvent.WaitFor(INFINITE) = wrSignaled);
      end;
      if WaitEventResult then
      begin
        if not Terminated then DoOnSignal;
        FEvent.ResetEvent;
      end else
        ClearError;
      SetCommMask(FCommHandle, FWaitMask);
    end;
    PurgeComm(FCommHandle, PurgeReadWrite);
  except
    on E: Exception do
    begin
      OutputDebugString(PChar('ERROR, ' + E.Message));
    end;
  end;
end;

procedure TCommEventThread.DoOnSignal;
begin
  if Assigned(FOnSignal) then
    FOnSignal(Self, FEventMask);
end;

{TCommEventChars}

constructor TCommEventChars.Create(Owner: TCustomComm);
begin
  inherited Create;
  FOwner := Owner;
  FXonChar := #0;
  FXoffChar := #0;
  FErrorChar := #0;
  FEofChar := #0;
  FEvtChar := #0;
end;

procedure TCommEventChars.SetEventChar(Index: Integer; Value: AnsiChar);
begin
  case Index of
    1: FXOnChar := Value;
    2: FXOffChar := Value;
    3: FErrorChar := Value;
    4: FEofChar := Value;
    5: FEvtChar := Value;
  end;
  if FOwner <> nil then
    FOwner.UpdateDataControlBlock;
end;

procedure TCommEventChars.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TCommEventChars) then
  begin
    FXonChar := TCommEventChars(Source).FXonChar;
    FXoffChar := TCommEventChars(Source).FXoffChar;
    FErrorChar := TCommEventChars(Source).FErrorChar;
    FEofChar := TCommEventChars(Source).FEofChar;
    FEvtChar := TCommEventChars(Source).FEvtChar;
  end else inherited Assign(Source);
end;


{ TCustomComm }

constructor TCustomComm.Create;
begin
  inherited Create;
  FHandle := INVALID_HANDLE_VALUE;
  FPortName := 'COM1';
  FReadTimeout := 1000;
  FWriteTimeout := 1000;
  FReadBufSize := 1024;
  FWriteBufSize := 1024;
  FMonitorEvents := EV_RXCHAR + EV_RXFLAG + EV_TXEMPTY + EV_CTS + EV_DSR +
    EV_RLSD + EV_BREAK + EV_ERR + EV_RING + EV_PERR + EV_RX80FULL +
    EV_EVENT1 + EV_EVENT2;

  FBaudRate := CBR_9600;
  FParity := NOPARITY;
  FStopbits := ONESTOPBIT;
  FDatabits := DATABITS_8;
  FOptions := [];
  FFlowControl := fcNone;
  FEventChars := TCommEventChars.Create(self);
  FReadEvent := TSimpleEvent.Create;
  FWriteEvent := TSimpleEvent.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TCustomComm.Destroy;
begin
  Close;
  FReadEvent.Free;
  FWriteEvent.Free;
  FEventChars.Free;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TCustomComm.SetPortName(Value: string);
begin
  if Value <> PortName then
  begin
    Close;
    FPortName := Value;
  end;
end;

procedure TCustomComm.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TCustomComm.Unlock;
begin
  FCriticalSection.Leave;
end;

function TCustomComm.GetEnabled: Boolean;
begin
  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

procedure TCustomComm.SetEnabled(Value: Boolean);
begin
  if Value <> Enabled then
  begin
    if Value then Open else Close;
  end;
end;

procedure TCustomComm.CreateHandle;
var
  i: Integer;
  Text: string;
  DeviceName: string;
begin
for i := 1 to 3 do
  begin
    DeviceName := Format('\\.\%s', [PortName]);
    FHandle := CreateFile(PChar(DeviceName),
      GENERIC_READ or GENERIC_WRITE, 0, nil,
      OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);

    if not Enabled then
    begin
      Text := SysErrorMessage(GetLastError);
      DestroyHandle;
      if i = 3 then
        raise Exception.CreateFmt('Ошибка открытия порта %s. %s', [PortName, Text])
      else
        Sleep(100);
    end
    else Exit;
  end;
end;

procedure TCustomComm.DestroyHandle;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  end;
end;

procedure TCustomComm.Open;

  procedure CreateEventThread;
  begin
    FEventThread := TCommEventThread.Create(FHandle, FMonitorEvents);
    FEventThread.OnSignal := HandleCommEvent;
  end;

begin
  if not Enabled then
  begin
    CreateHandle;
    if Enabled then
    begin
      UpdateDataControlBlock;
      SetupComm(FHandle, FReadBufSize, FWriteBufSize);
      UpdateCommTimeouts;
      SetSignals;
      CreateEventThread;
    end;
  end;
end;

procedure TCustomComm.SetSignals;
begin
  if not EscapeCommFunction(FHandle, SETDTR) then
    RaiseLastOSError;

  if not EscapeCommFunction(FHandle, CLRRTS) then
    RaiseLastOSError;
end;

procedure TCustomComm.Close;
begin
  if Enabled then
  begin
    DestroyHandle;
    FEventThread.Free;
    FEventThread := nil;
  end;
end;

procedure TCustomComm.Write(Data: AnsiString);
var
  Count: DWORD;
  Overlapped: TOverlapped;
begin
  if Length(Data) = 0 then Exit;
  ODS(Format('%s -> %s', [PortName, StrToHex(Data)]));

  Open;
  Lock;
  try
    FWriteEvent.ResetEvent;
    FillChar(Overlapped, Sizeof(Overlapped), 0);
    Overlapped.hEvent := FWriteEvent.Handle;
    if not WriteFile(FHandle, Data[1], Length(Data), DWORD(Count), @Overlapped) then
    begin
      if GetLastError <> ERROR_IO_PENDING then RaiseLastOSError;
      FWriteEvent.WaitFor(INFINITE);
    end;
  finally
    Unlock;
  end;
end;

function TCustomComm.Read(Count: Integer): AnsiString;
var
  ReadCount: DWORD;
  Overlapped: TOverlapped;
begin
  Result := '';
  if Count = 0 then Exit;

  Lock;
  try
    SetLength(Result, Count);
    FillChar(Overlapped, Sizeof(Overlapped), 0);
    Overlapped.hEvent := FReadEvent.Handle;
    if not ReadFile(FHandle, Result[1], Count, DWORD(ReadCount),
      @Overlapped) and (GetLastError <> ERROR_IO_PENDING) then
      RaiseLastOSError;

    if FReadEvent.WaitFor(FReadTimeout) <> wrSignaled then
      RaiseLastOSError;

    GetOverlappedResult(Handle, Overlapped, DWORD(ReadCount), False);
    SetLength(Result, ReadCount);
    FReadEvent.ResetEvent;
  finally
    Unlock;
  end;
  ODS(Format('%s <- %s', [PortName, StrToHex(Result)]));
end;

function TCustomComm.InQueCount: Integer;
var
  ComStat: TComStat;
  Errors: dword;
begin
  if Enabled then
  begin
    ClearCommError(FHandle, Errors, @ComStat);
    Result := ComStat.cbInQue;
  end else Result := -1;
end;

function TCustomComm.OutQueCount: Integer;
var
  ComStat: TComStat;
  Errors: dword;
begin
  if Enabled then
  begin
    ClearCommError(FHandle, Errors, @ComStat);
    Result := ComStat.cbOutQue;
  end else Result := -1;
end;

procedure TCustomComm.PurgeIn;
begin
  if Enabled then
    PurgeComm(FHandle, PurgeRead);
end;

procedure TCustomComm.PurgeOut;
begin
  if Enabled then
    PurgeComm(FHandle, PurgeWrite);
end;

procedure TCustomComm.SetBaudRate(Value: Integer);
begin
  if FBaudRate <> Value then
  begin
    FBaudRate := Value;
    UpdateDataControlBlock;
  end;
end;

procedure TCustomComm.SetParity(Value: Integer);
begin
  if FParity <> Value then
  begin
    FParity := Value;
    UpdateDataControlBlock;
  end;
end;

procedure TCustomComm.SetStopbits(Value: Integer);
begin
  if FStopBits <> Value then
  begin
    FStopbits := Value;
    UpdateDataControlBlock;
  end;
end;

procedure TCustomComm.SetDataBits(Value: Integer);
begin
  if FDataBits <> Value then
  begin
    FDataBits := Value;
    UpdateDataControlBlock;
  end;
end;

procedure TCustomComm.SetOptions(Value: TCommOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    UpdateDataControlBlock;
  end;
end;

procedure TCustomComm.SetFlowControl(Value: TFlowControl);
begin
  if FFlowControl <> Value then
  begin
    FFlowControl := Value;
    UpdateDataControlBlock;
  end;
end;

procedure TCustomComm.HandleCommEvent(Sender: TObject; Status: dword);
var
  ComStat: TComStat;
  Errors: dword;
begin
  ODS(Format('%s, Status: %.8X', [PortName, Status]));

  ClearCommError(FHandle, Errors, @ComStat);
  //if Status and EV_RXCHAR > 0 then
  if ComStat.cbInQue > 0 then
    if assigned(FOnRxChar) then FOnRxChar(self, ComStat.cbInQue);

  if Status and EV_BREAK > 0 then
    if assigned(FOnBreak) then FOnBreak(self);
  if Status and EV_CTS > 0 then
    if assigned(FOnCts) then FOnCts(self);
  if Status and EV_DSR > 0 then
    if assigned(FOnDsr) then FOnDsr(self);
  if Status and EV_ERR > 0 then
    if assigned(FOnError) then FOnError(self, Errors);
  if Status and EV_RING > 0 then
    if assigned(FOnRing) then FOnRing(self);
  if Status and EV_RLSD > 0 then
    if assigned(FOnRlsd) then FOnRlsd(self);
  if Status and EV_RXFLAG > 0 then
    if assigned(FOnRxFlag) then FOnRxFlag(self);
  if Status and EV_TXEMPTY > 0 then
    if assigned(FOnTxEmpty) then FOnTxEmpty(self);
end;

function TCustomComm.GetModemState(Index: Integer): boolean;
var
  Flag, State: dword;
begin
  case Index of
    1: State := MS_CTS_ON;
    2: State := MS_DSR_ON;
    3: State := MS_RING_ON;
    4: State := MS_RLSD_ON;
  else
    State := 0;
  end;
  Result := false;
  if Enabled then
    if GetCommModemStatus(FHandle, Flag) then
      Result := (Flag and State > 0);
end;

function TCustomComm.GetComState(Index: Integer): Boolean;
var
  Flag: TComStateFlag;
  ComStat: TComStat;
  Errors: dword;
begin
  case Index of
    1: Flag := fCtlHold;
    2: Flag := fDsrHold;
    3: Flag := fRlsHold;
    4: Flag := fXoffHold;
    5: Flag := fXOffSent;
  else
    Flag := fCtlHold;
  end;
  Result := false;
  if Enabled then
  begin
    ClearCommError(FHandle, Errors, @ComStat);
    Result := Flag in ComStat.Flags;
  end;
end;


procedure TCustomComm.UpdateDataControlBlock;
var
  OptIndex: TCommOption;
begin
  if Enabled then
  begin
    FillChar(FDCB, Sizeof(FDCB), 0);

    FDCB.BaudRate := FBaudRate;
    FDCB.Parity := FParity;
    FDCB.Stopbits := FStopbits;
    FDCB.Bytesize := FDatabits;
    FDCB.XonChar := FEventChars.XonChar;
    FDCB.XoffChar := FEventChars.XOffChar;
    FDCB.ErrorChar := FEventChars.ErrorChar;
    FDCB.EofChar := FEventChars.EofChar;
    FDCB.EvtChar := FEventChars.EvtChar;
    FDCB.XonLim := FReadBufSize div 4;
    FDCB.XoffLim := FReadBufSize div 4;

    InitHandshaking(FDCB);

    for OptIndex := coParityCheck to coNullStrip do
      if OptIndex in FOptions then FDCB.Flags := FDCB.Flags or CommOptions[OptIndex]
      else FDCB.Flags := FDCB.Flags and not CommOptions[OptIndex];

    FDCB.Flags := 1;

    if not SetCommState(FHandle, FDCB) then
      RaiseLastOSError;
  end;
end;


procedure TCustomComm.EscapeComm(Flag: Integer);
begin
  if Enabled then
  begin
    if not EscapeCommFunction(FHandle, Flag) then
      RaiseLastOSError;
  end;
end;

procedure TCustomComm.SetDTRState(State: boolean);
const
  DTR: array[boolean] of Integer = (CLRDTR, SETDTR);
begin
  EscapeComm(DTR[State]);
end;

procedure TCustomComm.SetRTSState(State: boolean);
const
  RTS: array[boolean] of Integer = (CLRRTS, SETRTS);
begin
  EscapeComm(RTS[State]);
end;

procedure TCustomComm.SetBREAKState(State: Boolean);
const
  BREAK: array[boolean] of Integer = (CLRBREAK, SETBREAK);
begin
  EscapeComm(BREAK[State]);
  if Enabled then
    PurgeComm(FHandle, PurgeReadWrite);
end;

procedure TCustomComm.SetXONState(State: Boolean);
const
  XON: array[boolean] of Integer = (SETXOFF, SETXON);
begin
  EscapeComm(XON[State]);
end;

procedure TCustomComm.UpdateCommTimeouts;
var
  CommTimeouts: TCommTimeouts;
begin
  FillChar(CommTimeOuts, Sizeof(CommTimeOuts), 0);

  CommTimeOuts.ReadIntervalTimeout := MAXDWORD;
  CommTimeOuts.ReadTotalTimeoutMultiplier := MAXDWORD;
  CommTimeOuts.ReadTotalTimeoutConstant := 3000;
  CommTimeOuts.WriteTotalTimeoutMultiplier := 3000;
  CommTimeOuts.WriteTotalTimeoutConstant := 3000;
  if not SetCommTimeOuts(FHandle, CommTimeOuts) then
    RaiseLastOSError;
end;

procedure TCustomComm.InitHandshaking(var DCB: TDCB);
begin
  if FFlowControl <> fcDefault then
    DCB.Flags := fBinary;
  case FFlowControl of
    fcNone: ; //Clear all flags, fBinary only
    fcDefault: ; //do nothing;
    fcCTS:
      DCB.Flags := DCB.Flags or fOutxCtsFlow or fRtsControlHandshake;
    fcDTR:
      DCB.Flags := DCB.Flags or fOutxDsrFlow or fDtrControlHandshake;
    fcSoftware:
      DCB.Flags := DCB.Flags or fOutX or fInX;
  end;
end;

end.

