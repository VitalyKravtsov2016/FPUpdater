unit BinUtils;

interface

uses
  // VCL
  Windows, SysUtils, Forms, Classes, Controls, Registry, Consts, ComObj, Math;

function Round2(Value: Double): Int64;

function ExRound(const N: double; const APrecisionRound: double): double;

function Min(V1, V2: Integer): Integer;

function DecToStr(Value: AnsiString): AnsiString;

function StrToHex(const S: AnsiString): AnsiString;

function StrToHex2(const S: AnsiString): AnsiString;

function HexToStr(const Data: AnsiString): AnsiString;

procedure SetBit(var Value: Byte; Bit: Byte);

procedure SetBitInt(var Value: Integer; Bit: Byte);

function TestBit(Value, Bit: Integer): Boolean;

function StrToDec(const Value: AnsiString): AnsiString;

function Str2Date(const Data: AnsiString; Index: Integer): TDateTime;

function Str2DateRev(const Data: AnsiString; Index: Integer): TDateTime;

function Str2Time(const Data: AnsiString; Index: Integer): TDateTime;

function IntToBin(Value, Count: Int64): AnsiString;

function BinToInt(const S: AnsiString; Index, Count: Integer): Int64;

function DoEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;

function DoEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;

function SwapByte(Value: Byte): Byte;

function SwapBytes(const S: AnsiString): AnsiString;

function CompareVersions(V1Major: LongWord; V1Minor: LongWord; V1Release: LongWord; V1Build: LongWord; V2Major: LongWord; V2Minor: LongWord; V2Release: LongWord; V2Build: LongWord): Integer;

function CompareVersionsStr(const AVersion1: AnsiString; const AVersion2: AnsiString): Integer;

function DoubleToCurrency(D: Double): Currency;

function ByteBCDToInt(AValue: Byte): Integer;

function IntToByteBCD(AValue: Integer): Byte;

function ByteBCDToStr(AValue: Byte): AnsiString;

function Int64ToBCDStr(AValue: Int64): AnsiString;

function Int64ToBCDStr2(AValue: Int64): AnsiString;

function DoubleToBCDStr(AValue: Double; ADecimalPoint: Integer): AnsiString;

function GetDoubleSymLength(AValue: Double; ADecimalPoint: Integer): Integer;

function StrToBCDByte(const AValue: AnsiString): Byte;

function BCDStrToInt(const AStr: AnsiString): Int64;

function BCDStrToInt2(const AStr: AnsiString): Int64;

function Str2DateBCD(const Data: AnsiString; Index: Integer): TDateTime;

function Str2TimeBCD(const Data: AnsiString; Index: Integer): TDateTime;

function Int64ToStr(i64: int64): AnsiString;

function DateTimeToFN(const AValue: TDateTime): AnsiString;

function BCDStrToDouble(const AStr: AnsiString; ADecimalPoint: Integer): Double;

function BytesToString(const AData: TBytes): AnsiString;

function StringToBytes(const AData: AnsiString): TBytes;

function BytesToHex(const AData: TBytes): string;

function HexToBytes(const AHexStr: string): TBytes;

const
  //C_DOUBLE_PREC = 0.00001; //    9.5E-4;
  C_DOUBLE_PREC = 0.00000001; //

implementation

uses
  DateUtils;

function ExRound(const N: double; const APrecisionRound: double): double;
var
  i, f: double;
begin
  if N < 0 then
    Result := Int((N - C_DOUBLE_PREC) / APrecisionRound)
  else
    Result := Int((N + C_DOUBLE_PREC) / APrecisionRound);

  f := abs(N / APrecisionRound - Result); // при расчете дробной части использовать Frac нельз€, так как оно возвращает кривую дробную часть, например, дл€ числа 2, может вернуть 1, т.е. это типа дробна€ часть от 1.9999999999

  if f > 0.5 - C_DOUBLE_PREC then
    i := 1
  else
    i := 0;

  if N < 0 then
    Result := (Result - i) * APrecisionRound
  else
    Result := (Result + i) * APrecisionRound;
end;

function Round2(Value: Double): Int64;
begin
  Result := Trunc(ExRound(Value, 1))
end;

function Min(V1, V2: Integer): Integer;
begin
  if V1 < V2 then
    Result := V1
  else
    Result := V2;
end;

function StrToDec(const Value: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Value) do
  begin
    Result := Result + IntToStr(Ord(Value[i]));
    if i <> Length(Value) then
      Result := Result + ';';
  end;
end;

function Int64ToStr(i64: int64): AnsiString;
begin
  str(i64, result);
end;

function DecToStr(Value: AnsiString): AnsiString;
var
  P: Integer;
  S: AnsiString;
  V: Integer;
  Code: Integer;
begin
  repeat
    P := Pos(';', Value);
    if P <> 0 then
    begin
      S := Copy(Value, 1, P - 1);
      Val(S, V, Code);
      if Code = 0 then
        Result := Result + AnsiChar(V)
      else
        Exit;
      Value := Copy(Value, P + 1, Length(Value));
    end;
  until P = 0;
end;

procedure SetBitInt(var Value: Integer; Bit: Byte);
begin
  Value := Value or (1 shl Bit);
end;

procedure SetBit(var Value: Byte; Bit: Byte);
begin
  Value := Value or (1 shl Bit);
end;

function TestBit(Value, Bit: Integer): Boolean;
begin
  Result := (Value and (1 shl Bit)) <> 0;
end;

function Str2Date(const Data: AnsiString; Index: Integer): TDateTime;
var
  Day, Month, Year: Byte;
begin
  Day := Ord(Data[Index]);
  Month := Ord(Data[Index + 1]);
  Year := Ord(Data[Index + 2]);

  if not DoEncodeDate(2000 + Year, Month, Day, Result) then
    Result := 0;
end;

function Str2DateBCD(const Data: AnsiString; Index: Integer): TDateTime;
var
  Day, Month, Year: Byte;
begin
  Day := ByteBCDToInt(Ord(Data[Index]));
  Month := ByteBCDToInt(Ord(Data[Index + 1]));
  Year := ByteBCDToInt(Ord(Data[Index + 2]));

  if not DoEncodeDate(2000 + Year, Month, Day, Result) then
    Result := 0;
end;

function Str2DateRev(const Data: AnsiString; Index: Integer): TDateTime;
var
  Day, Month, Year: Byte;
begin
  Year := Ord(Data[Index]);
  Month := Ord(Data[Index + 1]);
  Day := Ord(Data[Index + 2]);

  if not DoEncodeDate(2000 + Year, Month, Day, Result) then
    Result := 0;
end;

function Str2Time(const Data: AnsiString; Index: Integer): TDateTime;
var
  Hour, Min, Sec: Byte;
begin
  Hour := Ord(Data[Index]);
  Min := Ord(Data[Index + 1]);
  Sec := Ord(Data[Index + 2]);
  Result := 0;
  DoEncodeTime(Hour, Min, Sec, 0, Result);
end;

function Str2TimeBCD(const Data: AnsiString; Index: Integer): TDateTime;
var
  Hour, Min, Sec: Byte;
begin
  Hour := ByteBCDToInt(Ord(Data[Index]));
  Min := ByteBCDToInt(Ord(Data[Index + 1]));
  Sec := ByteBCDToInt(Ord(Data[Index + 2]));
  Result := 0;
  DoEncodeTime(Hour, Min, Sec, 0, Result);
end;

function DoEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do
      Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

{ Time encoding and decoding }

function DoEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
begin
  Result := False;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then
  begin
    Time := (Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec) / MSecsPerDay;
    Result := True;
  end;
end;

function SwapByte(Value: Byte): Byte;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to 7 do
    if TestBit(Value, i) then
      SetBit(Result, 7 - i);
end;

function SwapBytes(const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + AnsiChar(SwapByte(Ord(S[i])));
end;

function StrToHex(const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    if i <> 1 then
      Result := Result + ' ';
    Result := Result + IntToHex(Ord(S[i]), 2);
  end;
end;

function StrToHex2(const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    Result := Result + IntToHex(Ord(S[i]), 2);
  end;
end;

function HexToStr(const Data: AnsiString): AnsiString;
var
  S: AnsiString;
  i: Integer;
  V, Code: Integer;
begin
  S := '';
  Result := '';
  for i := 1 to Length(Data) do
  begin
    S := Trim(S + Data[i]);
    if (Length(S) <> 0) and ((Length(S) = 2) or (Data[i] = ' ')) then
    begin
      Val('$' + S, V, Code);
      if Code <> 0 then
        Exit;
      Result := Result + AnsiChar(V);
      S := '';
    end;
  end;
  // последний символ
  if Length(S) <> 0 then
  begin
    Val('$' + S, V, Code);
    if Code <> 0 then
      Exit;
    Result := Result + AnsiChar(V);
  end;
end;

function BinToInt(const S: AnsiString; Index, Count: Integer): Int64;
var
  N: Integer;
begin
  Result := 0;
  if (Index > 0) and (Index <= Length(S)) then
  begin
    N := Min(Length(S) - Index + 1, 8);
    if Count <= N then
    begin
      Move(S[Index], Result, Count);
    end;
  end;
end;

function DoubleToCurrency(D: Double): Currency;
const
  MaxCurrency: Currency = 922337203685477.5807;
  MinCurrency: Currency = -922337203685477.5807;
begin
  Result := 0;
  if (D >= MinCurrency) and (D <= MaxCurrency) then
    Result := D;
end;

function IntToBin(Value, Count: Int64): AnsiString;
begin
  Result := '';
  if Count in [1..8] then
  begin
    SetLength(Result, Count);
    Move(Value, Result[1], Count);
  end;
end;

{ —равнение версий. 0: –авны, -1: 1-€ меньше, 1: 1-€ больше}
function CompareVersions(V1Major: LongWord; V1Minor: LongWord; V1Release: LongWord; V1Build: LongWord; V2Major: LongWord; V2Minor: LongWord; V2Release: LongWord; V2Build: LongWord): Integer;
begin
  if (V1Major = V2Major) and (V1Minor = V2Minor) and (V1Release = V2Release) and (V1Build = V2Build) then
  begin
    Result := 0;
    Exit;
  end;

  if (V2Major > V1Major) or ((V2Major = V1Major) and (V2Minor > V1Minor)) or ((V2Major = V1Major) and (V2Minor = V1Minor) and (V2Release > V1Release)) or ((V2Major = V1Major) and (V2Minor = V1Minor) and (V2Release = V1Release) and (V2Build > V1Build)) then
    Result := -1
  else
    Result := 1;
end;

function ParseVersionStr(const AVersion: AnsiString; var v1, v2, v3, v4: Integer): Boolean;
var
  S: TStringList;
  Code: Integer;
begin
  Result := False;
  S := TStringList.Create;
  try
    S.Delimiter := '.';
    S.DelimitedText := AVersion;
    ;
    if S.Count < 4 then
      Exit;
    Val(S[0], v1, Code);
    if Code <> 0 then
      Exit;
    Val(S[1], v2, Code);
    if Code <> 0 then
      Exit;
    Val(S[2], v3, Code);
    if Code <> 0 then
      Exit;
    Val(S[3], v4, Code);
    if Code <> 0 then
      Exit;
    Result := True;
  finally
    S.Free;
  end;
end;

function CompareVersionsStr(const AVersion1: AnsiString; const AVersion2: AnsiString): Integer;
var
  v11, v12, v13, v14, v21, v22, v23, v24: Integer;
begin
  Result := 2;
  if not ParseVersionStr(AVersion1, v11, v12, v13, v14) then
    Exit;
  if not ParseVersionStr(AVersion2, v21, v22, v23, v24) then
    Exit;
  Result := CompareVersions(v11, v12, v13, v14, v21, v22, v23, v24);
end;

function ByteBCDToInt(AValue: Byte): Integer;
var
  h, l: Byte;
begin
  h := AValue shr 4;
  l := AValue and $0F;
  Result := h * 10 + l;
end;

function ByteBCDToStr(AValue: Byte): AnsiString;
var
  h, l: Byte;
  hs: AnsiString;
  ls: AnsiString;
begin
  h := AValue shr 4;
  l := AValue and $0F;
  if h > 9 then
    hs := '?'
  else
    hs := IntToStr(h);
  if l > 9 then
    ls := '?'
  else
    ls := IntToStr(l);
  Result := hs + ls;
end;

function StrToBCDByte(const AValue: AnsiString): Byte;
var
  h, l: Byte;
begin
  h := StrToInt(AValue[1]);
  l := StrToInt(AValue[2]);
  Result := (h shl 4) or (l);
end;

function IntToByteBCD(AValue: Integer): Byte;
var
  h, l: Byte;
begin
  h := (AValue div 10) mod 10;
  l := AValue mod 10;
  Result := (h shl 4) or (l);
end;

function Int64ToBCDStr(AValue: Int64): AnsiString;
begin
  Result := '';
  while True do
  begin
    Result := Result + AnsiChar(IntToByteBCD(AValue mod 100));
    if AValue div 100 = 0 then
      Break;
    AValue := AValue div 100;
  end;
end;

function Int64ToBCDStr2(AValue: Int64): AnsiString;
var
  S: AnsiString;
  i: Integer;
begin
  S := Int64ToBCDStr(AValue);
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
    Result[Length(S) - i + 1] := S[i];
end;

function GetDoubleSymLength(AValue: Double; ADecimalPoint: Integer): Integer;
var
  Value: Int64;
begin
  Value := Round2(AValue * Power(10, ADecimalPoint));
  Result := Length(IntToStr(Value));
end;

function DoubleToBCDStr(AValue: Double; ADecimalPoint: Integer): AnsiString;
var
  Value: Int64;
begin
  Value := Round2(AValue * Power(10, ADecimalPoint));
  Result := Int64ToBCDStr(Value);
end;

function BCDStrToDecStr(const AStr: AnsiString; ADecimalPoint: Integer): AnsiString;
var
  i: Integer;
begin
  Result := '';
  if AStr = '' then
    Exit;
  for i := Length(AStr) downto 1 do
    Result := Result + ByteBCDToStr(Ord(AStr[i]));
  while (Pos('0', Result) = 1) and (Length(Result) > 0) do
    Delete(Result, 1, 1);
  if Result = '' then
    Result := '0';
  if ADecimalPoint = 0 then
    Exit;
  if ADecimalPoint >= Length(Result) then
    Result := StringOfChar('0', ADecimalPoint - Length(Result) + 1) + Result;
  Insert(FormatSettings.DecimalSeparator, Result, Length(Result) - ADecimalPoint + 1);
end;

//-- ѕреобразование строки типа "123.456" в BCD
function DecStrToBCDStr(const AStr: AnsiString; ADecimalPoint: Integer): AnsiString;
var
  i: Integer;
  S: AnsiString;
  int: AnsiString;
  frac: AnsiString;
  d: AnsiString;
  k: Integer;
begin
  Result := '';
  S := AStr;
  if S = '' then
    Exit;
  k := Pos(FormatSettings.DecimalSeparator, S);
  if k > 0 then
  begin
    //-- ÷ела€ часть
    int := Copy(S, 1, k - 1);
    //-- ƒробна€ часть
    frac := Copy(S, k + 1, ADecimalPoint);
    frac := frac + StringOfChar('0', ADecimalPoint - Length(frac));
  end
  else
  begin
    int := S;
    frac := StringOfChar('0', ADecimalPoint);
  end;

  S := int + frac;

  if Odd(Length(S)) then
    S := '0' + S;
  i := Length(S);
  while True do
  begin
    if i < 2 then
      Break;
    d := S[i - 1] + S[i];
    Result := Result + AnsiChar(StrToBCDByte(d));
    Dec(i, 2);
  end;
end;

function BCDStrToDouble(const AStr: AnsiString; ADecimalPoint: Integer): Double;
var
  s: AnsiString;
  Value: Int64;
  k: Integer;
  i: Integer;
begin
  s := AStr;
  if s = '' then
  begin
    Result := 0;
    Exit;
  end;
  Value := 0;
  while True do
  begin
    if Length(s) = 0 then
      Break;
    if s[Length(s)] = #$00 then
      Delete(s, Length(s), 1)
    else
      Break;
  end;
  k := 1;
  for i := 1 to Length(s) do
  begin
    Value := Value + ByteBCDToInt(Ord(s[i])) * k;
    k := k * 100;
  end;
  Result := Value / Power(10, ADecimalPoint);
end;

function BCDStrToInt(const AStr: AnsiString): Int64;
var
  k: Int64;
  i: Integer;
begin
  Result := 0;
  if AStr = '' then
    Exit;
  Result := 0;
  k := 1;
  for i := 1 to Length(AStr) do
  begin
    Result := Result + ByteBCDToInt(Ord(AStr[i])) * k;
    k := k * 100;
  end;
end;

function BCDStrToInt2(const AStr: AnsiString): Int64;
var
  k: Int64;
  i: Integer;
begin
  Result := 0;
  if AStr = '' then
    Exit;
  Result := 0;
  k := 1;
  for i := Length(AStr) downto 1 do
  begin
    Result := Result + ByteBCDToInt(Ord(AStr[i])) * k;
    k := k * 100;
  end;
end;

function DateTimeToFN(const AValue: TDateTime): AnsiString;
var
  d, m, y, h, min, s, ms: Word;
begin
  DecodeDateTime(AValue, y, m, d, h, min, s, ms);
  Result := AnsiChar(Abs(y - 2000)) + AnsiChar(m) + AnsiChar(d) + AnsiChar(h) + AnsiChar(min);
end;

function Int64DecToStr(AValue: Int64; ADecimalPoint: Integer): AnsiString;
var
  S: AnsiString;
  Int: AnsiString;
  Frac: AnsiString;
begin
  S := IntToStr(AValue);
  if ADecimalPoint = 0 then
  begin
    Result := S;
    Exit;
  end;
  Int := Copy(S, 1, Length(S) - ADecimalPoint);
  Frac := Copy(S, Length(S) - ADecimalPoint + 1, Length(S));
  if Int = '' then
    Int := '0';
  Frac := StringOfChar('0', ADecimalPoint - Length(Frac)) + Frac;
  Result := Int + FormatSettings.DecimalSeparator + Frac;
end;

function BytesToString(const AData: TBytes): AnsiString;
begin
  SetString(Result, PAnsiChar(AData), Length(AData));
end;

function StringToBytes(const AData: AnsiString): TBytes;
begin
  Result := BytesOf(AData);
end;

function BytesToHex(const AData: TBytes): string;
begin
  Result := StrToHex(BytesToString(AData));
end;

function HexToBytes(const AHexStr: string): TBytes;
begin
  Result := StringToBytes(HexToStr(AHexStr));
end;

end.

