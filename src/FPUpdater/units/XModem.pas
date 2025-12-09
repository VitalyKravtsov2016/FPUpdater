unit XModem;

interface
uses
  // VCL
  SysUtils, Windows, Classes,
  // This
  AsyncSerialPort,
  //FileUtils,
  LogFile;

type
  TOnCancel = function: Boolean of object;

  TXModem = class
  private
    FDeviceReady: Boolean;
    FPort: TAsyncSerialPort;
    FBaudrate: Integer;
    FPortNumber: Integer;
    FTimeout: Integer;
    FFrameNumber: Byte;
    FOnCancel: TOnCancel;
    FLogOn: Boolean;
    FPercent: Integer;
    FOnPercent: TNotifyEvent;
    procedure OnRxChar(Sender: TObject; Count: Integer);
    procedure WaitForDeviceReady;
    procedure Transmit(const AFileName: string);
    function CRC16(const AData: AnsiString): AnsiString;
    procedure SendPacket(const AData: AnsiString);
    procedure Reboot;
    function ReadAckChar: AnsiChar;
    function Read(Count: Integer): AnsiString;
    function ReadChar: AnsiChar;
    procedure Write(const Data: AnsiString);
    procedure CheckCancelled;
    procedure LogDebug(const Data: string);
  public
    procedure SendFile(const AFileName: string);
    constructor Create;
    destructor Destroy; override;

    property LogOn: Boolean read FLogOn write FLogOn;
    property Port: TAsyncSerialPort read FPort write FPort;
    property PortNumber: Integer read FPortNumber write FPortNumber;
    property Baudrate: Integer read FBaudrate write FBaudrate;
    property Timeout: Integer read FTimeout write FTimeout;
    property OnCancel: TOnCancel read FOnCancel write FOnCancel;
    property OnPercent: TNotifyEvent read FOnPercent write FOnPercent;
    property Percent: Integer read FPercent write FPercent;
  end;

function CRCCITT16(const Buffer: AnsiString; Polynom, Initial: Word): Word;

implementation

const
  STX = #2;
  ENQ = #5;
  ACK = #6;
  NAK = #21;
  EOT = #04;
  CAN = #$18;

function StrToHex(const S: AnsiString; ASpace: Boolean = True): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    Result := Result + IntToHex(Ord(S[i]), 2);
    if ASpace then Result := Result + ' ';
  end;
end;

function ReadFileData(const FileName: string): AnsiString;
var
  Stream: TFileStream;
begin
  Result := '';
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    if Stream.Size > 0 then
    begin
      SetLength(Result, Stream.Size);
      Stream.Read(Result[1], Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

{$R-}
function CRCCITT16(const Buffer: AnsiString; Polynom, Initial: Word): Word;
var
  i,j: Integer;
begin
  Result:=Initial;
  for i:=1 to Length(Buffer) do
  begin
    Result:=Result xor (ord(buffer[i]) shl 8);
    for j:=0 to 7 do
    begin
      if (Result and $8000)<>0 then Result:=(Result shl 1) xor Polynom
      else Result:=Result shl 1;
    end;
  end;
  Result:=Result and $ffff;
end;
{$R+}

procedure TXModem.LogDebug(const Data: string);
begin
  if FLogOn then
    Logger.Debug(Data);
end;

procedure TXModem.CheckCancelled;
begin
  if Assigned(FOnCancel) then
    if FOncancel then
    begin
      raise Exception.Create('Operation aborted');
    end;
end;

function TXModem.CRC16(const AData: AnsiString): AnsiString;
var
  Crc: Word;
begin
  CRC := CRCCITT16(AData, $1021, 0);
  SetLength(Result, 2);
  Result[1] := AnsiChar(HiByte(Crc));
  Result[2] := AnsiChar(LoByte(Crc));
end;

constructor TXModem.Create;
begin
  inherited;
  FPort := TAsyncSerialPort.Create;
end;

destructor TXModem.Destroy;
begin
  FPort.Free;
  inherited;
end;

procedure TXModem.OnRxChar(Sender: TObject; Count: Integer);
var
  i: Integer;
  s: AnsiString;
begin
  s := FPort.Read(Count);
  for i := 1 to Length(S) do
  begin
    if Ord(S[i]) = $43 then
    begin
      LogDebug('Got XModem Sync char');
      FDeviceReady := True;
      Break;
    end
    else
      LogDebug('Try to read sync char: ' + String(StrToHex(S[i])));
  end;
end;

function TXModem.Read(Count: Integer): AnsiString;
begin
  try
    Result := Port.Read(Count);
    LogDebug('<- ' + StrToHex(Result));
  except
    on E: Exception do
    begin
      raise Exception.Create('Нет связи');
    end;
  end;
end;


function TXModem.ReadAckChar: AnsiChar;
var
  s: AnsiString;
  T: Cardinal;

begin
  T := GetTickCount;
 repeat
    s := Read(1);
    if length(S) = 0 then Break;
    Result := s[1];
    if (s[1] = ACK) or (s[1] = NAK) or (s[1] = ENQ) then
      Exit;
 until abs(GetTickCount - T) >= 25000;
 raise Exception.Create('Нет связи');
end;


function TXModem.ReadChar: AnsiChar;
var
  S: AnsiString;
begin
  S := Read(1);
  if Length(S) = 0 then
    raise Exception.Create('Нет связи');
  Result := S[1];
end;

procedure TXModem.Reboot;  {!!!}
var
  i: Integer;
begin
  Port.PurgeIn;
  Port.PurgeOut;
  for i := 1 to 5 do
  begin
    CheckCancelled;
   Write(#$05);
   try
     if ReadAckChar = NAK then
       break;
   except
   on E: Exception do
     if i = 5 then raise
   end;
   if i = 5 then
     raise Exception.Create('Нет связи');
  end;
  Write(#$02#$06#$FE#$F3#$00#$00#$00#$00#$0B);
  ReadChar;
  ReadChar;
  ReadChar;
  ReadChar;
  ReadChar;
  ReadChar;
  //Read(6);
  Write(#$06);
  Port.PurgeIn;
  Port.PurgeOut;
end;


procedure TXModem.SendFile(const AFileName: string);
begin
  FDeviceReady := False;
  Port.Close;
  Port.PortName := 'COM' + IntToStr(FPortNumber);
  Port.BaudRate := FBaudrate;
  Port.ReadTimeout := FTimeout;
  Port.Open;
  try
    Reboot;
    FPort.OnRxChar := OnRxChar;
    WaitForDeviceReady;
    FPort.OnRxChar := nil;
    Transmit(AFileName);
  finally
    Port.Close;
  end;
end;

procedure TXModem.SendPacket(const AData: AnsiString);
var
  i: Integer;
  C: AnsiChar;
  Packet: AnsiString;
begin
  Packet := #$01 + AnsiString(AnsiChar(FFrameNumber)) + AnsiString(AnsiChar($FF - FFrameNumber))
   + AData + CRC16(AData);
  if FFrameNumber = $FF then
    FFrameNumber := 0
  else
    Inc(FFrameNumber);
  for i := 1 to 5 do
  begin
    CheckCancelled;
    Write(Packet);
    C := ReadChar;
    if C = ACK then
      break
    else if C = NAK then
      Continue
    else
      raise Exception.Create('Нет связи');
    if i = 5 then
      raise Exception.Create('Нет связи');
  end;
end;

procedure TXModem.Transmit(const AFileName: string);
var
  Data: AnsiString;
  k: Integer;
  Number: Integer;
  Count: Integer;
begin
  LogDebug('Transmit file: ' + AFileName);
  FFrameNumber := 1;
  Data := ReadFileData(AFileName);
  FPercent := 0;
  Number := 0;
  Count := Length(Data) div 128;
  k := 1;
  repeat
    SendPacket(Copy(Data, k, 128));
    Inc(k, 128);
    Inc (Number);
    FPercent := 0;
    if Count <> 0 then
    begin
      FPercent := Trunc((Number/Count) * 100);
      LogDebug(IntToStr(Number) + '/' + IntToStr(Count));
      if Assigned(FOnPercent) then
        FOnPercent(Self);
    end;
    CheckCancelled;
  until k >= Length(Data);
  Write(EOT);
end;

procedure TXModem.WaitForDeviceReady;
var
  T: Cardinal;
begin
  T := GetTickCount;
  repeat
    CheckCancelled;
    if abs(GetTickCount - T) >= 30000 then
      raise Exception.Create('Не удается установить связь по XModem');
    Sleep(1);
  until FDeviceReady;
end;

procedure TXModem.Write(const Data: AnsiString);
begin
  LogDebug('-> ' + StrToHex(Data));
  Port.Write(Data);
end;

end.

