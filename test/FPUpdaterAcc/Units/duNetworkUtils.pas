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
begin
  Check(IsRouteViaRNDIS('192.168.137.1'), '192.168.137.1');
  Check(IsRouteViaRNDIS('192.168.137.111'), '192.168.137.111');
  Check(not IsRouteViaRNDIS('10.161.145.110'), '10.161.145.110');
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
