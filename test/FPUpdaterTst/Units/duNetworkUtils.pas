unit duNetworkUtils;

interface

uses
  // VCL
  Windows, ActiveX, ComObj, SysUtils,
  // DUnit
  TestFramework,
  // This
  NetworkUtils, TCPDeviceSearch, TCPSearchRec;

type
  { TNetworkUtilsTest }

  TNetworkUtilsTest = class(TTestCase)
  published
    procedure TestIsLocalRNDISDevice;
    procedure TestFindLocalTCPDevice;
  end;

implementation

{ TNetworkUtilsTest }

procedure TNetworkUtilsTest.TestIsLocalRNDISDevice;
var
  Adapters: TArray<TNetworkAdapter>;
begin
  Adapters := GetAdaptersList;
  CheckEquals(3, Length(Adapters));
  CheckEquals('10.161.145.110', Adapters[0].IPAddress, 'Adapters[0].IPAddress');
  CheckEquals('192.168.56.1', Adapters[1].IPAddress, 'Adapters[1].IPAddress');
  CheckEquals('192.168.137.1', Adapters[2].IPAddress, 'Adapters[2].IPAddress');
  Check(IsLocalRNDISDevice('192.168.137.111'), '192.168.137.111');
  Check(not IsLocalRNDISDevice('10.161.145.110'), '10.161.145.110');
end;


procedure TNetworkUtilsTest.TestFindLocalTCPDevice;
var
  Item: TTCPSearchRec;
begin
  Check(FindLocalTCPDevice(Item, 3000), 'FindLocalTCPDevice');
  CheckEquals('192.168.137.111', Item.IP, 'Item.IP');
end;


initialization
  RegisterTest('', TNetworkUtilsTest.Suite);
end.
