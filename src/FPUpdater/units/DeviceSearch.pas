unit DeviceSearch;

interface

uses
  // VCL
  Windows, SysUtils, Classes, ComObj, ActiveX,
  // This
  NotifyThread, SearchPort, DrvFRLib_TLB, untUtil, DriverError, LogFile,
  DriverTypes, ComportUtils;

type
  { TDeviceSearch }

  TDeviceSearch = class
  private
    FPorts: TSearchPorts;
    FDoTechReset: Boolean;
    FShortSearch: Boolean;
    procedure UpdatePorts;
    function GetCompleted: Boolean;
    function GetPortNames: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Wait;

    property Ports: TSearchPorts read FPorts;
    property Completed: Boolean read GetCompleted;
    property DoTechReset: Boolean read FDoTechReset write FDoTechReset;
    property ShortSearch: Boolean read FShortSearch write FShortSearch;
  end;

implementation

{ TDeviceSearch }

constructor TDeviceSearch.Create;
begin
  inherited Create;
  FPorts := TSearchPorts.Create;
end;

destructor TDeviceSearch.Destroy;
begin
  FPorts.Free;
  inherited Destroy;
end;

//(PortRec.vid <> $0E8D) and (PortRec.pid <> $2012) and (
// if (Port.vid <> $0E8D) and (Port.pid <> 2012) then

procedure TDeviceSearch.UpdatePorts;
var
  Port: TSearchPort;
  PortData: TSearchPortRec;
  PortRec: TSerialPortRec;
  PortList: TSerialPorts;
begin
  Ports.Lock;
  PortList := TSerialPorts.Create;
  try
    Ports.Clear;
    EnumPorts(PortList);
    for PortRec in PortList do
    begin
      // ????
      if PortRec.ComNumber >= 0 then
      begin
        PortData.PortName := PortRec.Name;
        PortData.PortNumber := PortRec.ComNumber;
        PortData.FriendlyName := PortRec.FriendlyName;
        Port := Ports.Add;
        Port.DoTechReset := DoTechReset;
        Port.ShortSearch := ShortSearch;
        Port.Data := PortData;
        Port.Text := PortRec.FriendlyName;
        Port.Selected := True;
      end;
    end;
  finally
    Ports.Unlock;
    PortList.Free;
  end;
end;

procedure TDeviceSearch.Stop;
var
  i: Integer;
begin
  for i := 0 to Ports.Count - 1 do
    Ports[i].Stop;
end;

procedure TDeviceSearch.Start;
var
  i: Integer;
begin
  Stop;
  UpdatePorts;
  for i := 0 to Ports.Count - 1 do
  begin
    Ports[i].Start;
  end;
end;

procedure TDeviceSearch.Wait;
var
  i: Integer;
begin
  for i := 0 to Ports.Count - 1 do
  begin
    Ports[i].Wait;
  end;
end;

function TDeviceSearch.GetCompleted: Boolean;
begin
  Result := Ports.Completed;
end;

function TDeviceSearch.GetPortNames: string;
var
  Strings: TStringList;
  Ports: TSerialPorts;
  Port: TSerialPortRec;
begin
  Strings := TStringList.Create;
  Ports := TSerialPorts.Create;
  try
    EnumPorts(Ports);
    for Port in Ports do
    begin
      Strings.Add(Port.Name);
    end;
    Result := Strings.Text;
  finally
    Ports.Free;
    Strings.Free;
  end;
end;

end.

