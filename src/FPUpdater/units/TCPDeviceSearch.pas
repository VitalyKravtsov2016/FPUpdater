unit TCPDeviceSearch;

interface

uses
  // VCL
  Windows, Forms, SysUtils, Classes, System.Generics.Collections,
  // Indy
  IdBaseComponent, IdUDPBase, IdUDPServer, IdGlobal, IdSocketHandle,
  // This
  MultiCastIPv6Receiver, TCPSearchRec, NetworkUtils, DebugUtils, LogFile;

type
  TOnFoundProc = procedure(AItem: TTCPSearchRec) of object;

  TTCPSearchItems = TList<TTCPSearchRec>;

  TTCPDeviceSearch = class
  private
    FClientIpv4: TIdUDPServer;
    FClientIpv6: TMulticastIPv6Receiver;
    FItems: TTCPSearchItems;
    FOnFound: TOnFoundProc;
    procedure OnIpv6Data(const AData: string);
    procedure OnUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
    function AddItem(const AItem: TTCPSearchRec): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure DeviceFound(const Data: string);

    property Items: TTCPSearchItems read FItems;
    property OnFound: TOnFoundProc read FOnFound write FOnFound;
  end;

function FindLocalTCPDevice(var Item: TTCPSearchRec; TimeoutInMs: Integer): Boolean;

implementation

function FindLocalTCPDevice(var Item: TTCPSearchRec; TimeoutInMs: Integer): Boolean;
var
  StopTime: Integer;
  Search: TTCPDeviceSearch;
begin
  Search := TTCPDeviceSearch.Create;
  try
    StopTime := GetTickCount + TimeoutInMs;
    Search.Start;
    while GetTickCount < StopTime do
    begin
      Sleep(100);
      Result := Search.Items.Count > 0;
      if Result then
      begin
        Item := Search.Items[0];
        Break;
      end;
      Sleep(10);
    end;
  finally
    Search.Free;
  end;
end;


const
  IPV6GROUP = 'FF02::2606:1';
  SEARCHPORT = 16327;

{ TTCPDeviceSearch }

constructor TTCPDeviceSearch.Create;
begin
  inherited;
  FItems := TTCPSearchItems.Create;
  FClientIpv4 := TIdUDPServer.Create(nil);
  FClientIpv4.DefaultPort := SEARCHPORT;
  FClientIpv4.OnUDPRead := OnUDPRead;
  FClientIpv4.ReuseSocket := rsTrue;
  FClientIpv4.ThreadedEvent := True;
  FClientIpv4.Active := False;

  FClientIpv6 := TMulticastIPv6Receiver.Create(IPV6GROUP, SEARCHPORT);
  FClientIpv6.OnData := OnIpv6Data;
end;

destructor TTCPDeviceSearch.Destroy;
begin
  Stop;
  FItems.Free;
  FClientIpv4.Free;
  FClientIpv6.Free;
  inherited Destroy;
end;

procedure TTCPDeviceSearch.OnIpv6Data(const AData: string);
begin
  DeviceFound(AData);
end;

procedure TTCPDeviceSearch.OnUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
begin
  DeviceFound(BytesToString(AData));
end;

function TTCPDeviceSearch.AddItem(const AItem: TTCPSearchRec): Boolean;
var
  Item: TTCPSearchRec;
begin
  Result := False;
  for Item in FItems do
    if Item.SN = AItem.SN then
      Exit;
  FItems.Add(AItem);
  Result := True;
end;

procedure TTCPDeviceSearch.DeviceFound(const Data: string);
var
  SR: TTCPSearchRec;
begin
  //Logger.Debug('TTCPDeviceSearch.DeviceFound: ' + Data);
  if SR.Parse(Data) then
  begin
    if IsRouteViaRNDIS(SR.IP) then
    begin
      if AddItem(SR) then
      begin
        if Assigned(FOnFound) then
          FOnFound(SR);
      end;
    end;
  end;
end;

procedure TTCPDeviceSearch.Start;
begin
  FItems.Clear;
  FClientIpv4.Active := True;
  //FClientIpv6.Active := True;
end;

procedure TTCPDeviceSearch.Stop;
begin
  FClientIpv4.Active := False;
  FClientIpv6.Active := False;
end;

end.

