unit TCPSearchRec;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

//const
//  data = 'KKT SN 0421670012001159; IP 192.168.137.111; port 7778';
//  data2 = 'KKT SN 0212731421234567; IPV6LL FE80::18:B3FF:FEFF:8B2F; IPV6A FC00::137:111; port 7778';

type
  TTCPSearchRec = record
  private
    procedure ParseSearchString(const AData: string; AList: TList<TPair<string, string>>);
  public
    SN: string;
    IP: string;
    IPV6LL: string;
    IPV6A: string;
    port: Integer;
    function Parse(const AData: string): boolean;
    function IsIPv6: Boolean;
  end;

implementation

{ TTCPSearchRec }

function TTCPSearchRec.IsIPv6: Boolean;
begin
  Result := IP = '';
end;

function TTCPSearchRec.Parse(const AData: string): boolean;
var
  List: TList<TPair<string, string>>;
  Item: TPair<string, string>;
begin
  Result := False;
  SN := '';
  IP := '';
  IPV6LL := '';
  IPV6A := '';
  port := 0;
  List := TList<TPair<string, string>>.Create;
  try
    ParseSearchString(AData, List);
    for Item in List do
    begin
      if Item.Key = 'SN' then
        SN := Item.Value;
      if Item.Key = 'IP' then
        IP := Item.Value;
      if Item.Key = 'IPV6LL' then
        IPV6LL := Item.Value;
      if Item.Key = 'IPV6A' then
        IPV6A := Item.Value;
      if Item.Key = 'port' then
        port := StrToIntDef(Item.Value, 0);
    end;
  finally
    List.Free;
  end;
  if SN.IsEmpty or (port = 0) then
    Exit;
  if IP.IsEmpty and IPV6LL.IsEmpty and IPV6A.IsEmpty then
    Exit;
  Result := True;
end;

procedure TTCPSearchRec.ParseSearchString(const AData: string; AList: TList<TPair<string, string>>);
var
  SL: TSTringList;
  S: string;
  k: Integer;
  d: string;
begin
  SL := TStringList.Create;
  try
    SL.StrictDelimiter := True;
    SL.Delimiter := ';';
    SL.DelimitedText := Copy(AData, 4, Length(AData));
    for S in SL do
    begin
      d := Copy(S, 2, Length(S));
      k := Pos(' ', d);
      AList.Add(TPair<string, string>.Create(Copy(d, 1, k - 1), Copy(d, k + 1, Length(S))));
    end;
  finally
    SL.Free;
  end;
end;

end.

