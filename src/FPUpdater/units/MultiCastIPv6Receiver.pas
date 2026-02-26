unit MultiCastIPv6Receiver;

interface

uses
  IdGlobal, IdSocketHandle, IdBaseComponent, IdComponent, IdIPMCastClient,
  IdStackBSDBase, IdStackConsts, IdStack, IdStackWindows,
  System.Generics.Collections;

type
  TOnDataProc = procedure(const AData: string) of object;

  TMulticastIPv6Receiver = class
  private
    FClient: TIdIPMCastClient;
    FOnData: TOnDataProc;
    FGroupIP: string;
    FPort: Integer;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure AfterBind(Sender: TObject);
    procedure OnRead(Sender: TObject; const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure Getipv6InterfaceIndexes(List: TList<Integer>);
  public
    constructor Create(const AGroupIP: string; APort: Integer);
    destructor Destroy; override;
    property Active: Boolean read GetActive write SetActive;
    property OnData: TOnDataProc read FOnData write FOnData;
  end;

implementation

{ TMulticastIPv6Receiver }

procedure TMulticastIPv6Receiver.Getipv6InterfaceIndexes(List: TList<Integer>);
var
  Addresses: TIdStackLocalAddressList;
  Address: TIdStackLocalAddress;
  i: Integer;
begin
  List.Clear;
  Addresses := TIdStackLocalAddressList.Create;
  try
    TIdStackWindows(GStack).GetLocalAddressList(Addresses);
    for i := 0 to Addresses.Count - 1 do
    begin
      Address := Addresses[i];
      if Address.IPVersion = Id_IPv6 then
      begin
        if List.IndexOf(Address.InterfaceIndex) < 0 then
        begin
          //  Logger.Debug('Add interfact to multicast: ' + Address.InterfaceIndex.ToString + ', ' + Address.InterfaceName);
          List.Add(Address.InterfaceIndex);
        end;
      end;
    end;
  finally
    Addresses.Free;
  end;
end;

procedure TMulticastIPv6Receiver.AfterBind(Sender: TObject);
var
  LIP6: TIdIPv6Mreq;
  interfaceindex: Integer;
  GroupIP: string;
  InterfaceIndexes: TList<Integer>;
begin
  InterfaceIndexes := TList<Integer>.Create;
  try
    Getipv6InterfaceIndexes(InterfaceIndexes);
    for interfaceindex in InterfaceIndexes do
    begin
      FillChar(LIP6, SizeOf(LIP6), #0);
      TIdStackWindows(GStack).TranslateStringToTInAddr(FGroupIP, LIP6.ipv6mr_multiaddr, Id_IPv6);
      LIP6.ipv6mr_interface := interfaceindex;
      try
        // Нужно подключиться ко всем интерфейсам
        TIdStackWindows(GStack).SetSocketOption(FClient.Bindings[FClient.Bindings.Count - 1].Handle, Id_IPPROTO_IPv6, Id_IP_ADD_MEMBERSHIP, LIP6, SizeOf(LIP6));
        // Нужно ли потом делать Id_IPV6_DROP_MEMBERSHIP ?
      except
        //Logger.Debug('bind to ' + interfaceindex.ToString + ' error');
      end;
    end;
  finally
    InterfaceIndexes.Free;
  end;
end;

constructor TMulticastIPv6Receiver.Create(const AGroupIP: string; APort: Integer);
begin
  inherited Create;
  FGroupIP := AGroupIP;
  FPort := APort;
  FClient := TIdIPMCastClient.Create(nil);
  FClient.OnAfterBind := AfterBind;
  FClient.OnIPMCastRead := OnRead;
  FClient.IPVersion := Id_IPv6;
  FClient.DefaultPort := APort;
  FClient.ReuseSocket := rsTrue;
  FClient.MulticastGroup := FGroupIP;
end;

destructor TMulticastIPv6Receiver.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

procedure TMulticastIPv6Receiver.OnRead(Sender: TObject; const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  s: AnsiString;
  i: Integer;
begin
  if Assigned(FOnData) then
  begin
    s := '';
    for i := 0 to Length(AData) - 1 do
      s := s + AnsiChar(AData[i]);
    FOnData(s);
  end
end;

function TMulticastIPv6Receiver.GetActive: Boolean;
begin
  Result := FClient.Active;
end;

procedure TMulticastIPv6Receiver.SetActive(const Value: Boolean);
begin
  if not FClient.Active and Value then
  begin
    FClient.Bindings.Clear;
    FClient.Bindings.Add;
    FClient.Bindings[0].IP := '';
    FClient.Bindings[0].Port := FPort;
    FClient.Bindings[0].IPVersion := Id_IPv6;
    FClient.MulticastGroup := FGroupIP;
  end;
  FClient.Active := Value;
end;

end.


