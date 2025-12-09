unit untUtil;

interface

uses
  // VCL
  Windows, SysUtils, Forms, Classes, Controls, Registry, Consts, ComObj,
  // This
  DrvFRLib_TLB, DriverError, DriverTypes, BinUtils, LangUtils;

function GetAppName: string;
function GetCompName: string;
procedure NTDeleteRegKey(const KeyName: string; RootKey: HKEY);
function GetTableName(const Value: WideString): WideString;
function GetStr2(const S: AnsiString; Len: Integer): AnsiString;
procedure DrvCheck(Driver: IDrvFr48; ResultCode: Integer); overload;
procedure InvalidProp(const PropName: Widestring);

function ByteToTimeout(Value: Byte): DWORD;
function TimeoutToByte(Value: Integer): Byte;
function TimeToStr1C(Value: TDateTime): string;
function MetricsToModel(UModel, UMajorType, UMinorType,
  UMajorProtocolVersion, UMinorProtocolVersion: Integer): TDeviceModel;

function ModelToDataSize(Value: TDeviceModel): Integer;
function Str2EcrDate(const Data: string; Index: Integer): TEcrDate;
procedure ReadTableWindowParams(ATableNumber: Integer; AForm: TForm; RootKey: HKEY);
procedure SaveTableWindowParams(ATableNumber: Integer; AForm: TForm; RootKey: HKEY);
function DecodeTCPPort(ACode: Byte): Integer;
function EncodeTCPPort(ATCPPort: Word): Byte;
function ModelToMetrics(Model: TDeviceModel): Integer;
function Trim2(const s: string): string;

implementation

function Trim2(const s: string): string;
begin
  Result := Trim(S);
  if Pos(#0, Result) > 0 then
    Delete(Result, Pos(#0, S), Length(Result));
end;

function GetAppName: string;
var
  Wnd: HWND;
  ParentWnd: HWND;
  Text: array [0..MAX_PATH] of char;
begin
  Wnd := GetActiveWindow;
  if Wnd <> 0 then
  begin
    ParentWnd := GetParent(Wnd);
    while ParentWnd <> 0 do
    begin
      Wnd := ParentWnd;
      ParentWnd := GEtParent(Wnd);
    end;
    GetWindowText(Wnd, Text, MAX_PATH);
    Result := Text;
  end;
end;

function GetTableName(const Value: WideString): WideString;
var
  S: WideString;
begin
  Result := WideLowerCase(Value);
  if Length(Result) > 0 then
  begin
    S := WideUpperCase(Result[1]);
    if Length(S) > 0 then Result[1] := S[1];
  end;
end;

function GetCompName: string;
var
  Size: DWORD;
  LocalMachine: array [0..MAX_COMPUTERNAME_LENGTH] of char;
begin
  Size := Sizeof(LocalMachine);
  if GetComputerName(LocalMachine, Size) then
    Result := LocalMachine else Result := '';
end;

procedure DrvCheck(Driver: IDrvFr48; ResultCode: Integer); overload;
begin
  if ResultCode <> 0 then
    RaiseError(ResultCode, Driver.ResultCodeDescription);
end;

procedure NTDeleteRegKey(const KeyName: string; RootKey: HKEY);
var
  i: Integer;
  Reg: TRegistry;
  Strings: TStrings;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := RootKey;
  Strings := TStringList.Create;
  try
    if Reg.OpenKey(KeyName, False) then
    begin
      Reg.GetKeyNames(Strings);
      for i := 0 to Strings.Count-1 do
      begin
        NTDeleteRegKey(KeyName + '\' + Strings[i], RootKey);
      end;
      Reg.CloseKey;
      Reg.DeleteKey(KeyName);
    end;
  finally
    Reg.Free;
    Strings.Free;
  end;
end;

{*****************************************************************************}
{
{       Ïðåîáðàçîâàíèå âðåìåíè â ñòðîêó â ôîðìàòå hh:nn:ss
{
{*****************************************************************************}

function TimeToStr1C(Value: TDateTime): string;
begin
  Result := FormatDateTime('hh:nn:ss', Value);
end;

function ByteToTimeout(Value: Byte): DWORD;
begin
  case Value of
    0..150   : Result := Value;
    151..249 : Result := (Value-149)*150;
  else
    Result := (Value-248)*15000;
  end;
end;

function TimeoutToByte(Value: Integer): Byte;
begin
  case Value of
    0..150        : Result := Value;
    151..15000    : Result := Round(Value/150)+149;
    15001..105000 : Result := Round(Value/15000)+248;
  else
    Result := Value;
  end;
end;

function GetStr2(const S: AnsiString; Len: Integer): AnsiString;
var
  StrLen: Integer;
begin
  StrLen := Length(S);
  if StrLen < Len then
  begin
    Result := S;
    SetLength(Result, Len);
    FillChar(Result[StrLen+1], Len-StrLen, #0)
  end else
  begin
    Result := Copy(S, 1, Len);
  end;
end;

// Ïîëó÷åíèå íìîåðà ìîäåëè

function ModelToMetrics(Model: TDeviceModel): Integer;
begin
  case Model of
    dmUnknown: Result := 0; 					  // Íåèçâåñòíàÿ ìîäåëü
    dmShtrihFRF3: Result := 0;				  // ØÒÐÈÕ-ÔÐ-Ô (âåðñèÿ 3)
    dmShtrihFRF4: Result := 0;				  // ØÒÐÈÕ-ÔÐ-Ô (âåðñèÿ 4)
    dmShtrihFRFKaz: Result := 1;   	    // ØÒÐÈÕ-ÔÐ-Ô (Êàçàõñòàí)
    dmElvesMiniFRF: Result := 2;			  // ÝËÂÅÑ-ÌÈÍÈ-ÔÐ-Ô
    dmFelixRF: Result := 3; 					  // ÔÅËÈÊÑ-Ð Ô
    dmShtrihFRK: Result := 4;				    // ØÒÐÈÕ-ÔÐ-Ê
    dmShtrih950K: Result := 5;				  // ØÒÐÈÕ-950Ê âåðñèÿ 1
    dmShtrih950Kv2: Result := 11;			  // Øòðèõ950K âåðñèÿ 2
    dmElvesFRK: Result := 6; 				    // ÝËÂÅÑ-ÔÐ-Ê
    dmShtrihMiniFRK: Result := 7; 		  // ØÒÐÈÕ-ÌÈÍÈ-ÔÐ-Ê
    dmShtrihMiniFRK2: Result := 14; 	  // ØÒÐÈÕ-ÌÈÍÈ-ÔÐ-Ê 2
    dmShtrihFRFBel: Result := 8; 		    // ØÒÐÈÕ-ÔÐ-Ô (Áåëîðóññèÿ)
    dmShtrihComboFRKv1: Result := 9;    // ØÒÐÈÕ-ÊÎÌÁÎ-ÔÐ-Ê âåðñèè 1
    dmShtrihComboFRKv2: Result := 12;   // ØÒÐÈÕ-ÊÎÌÁÎ-ÔÐ-Ê âåðñèè 2
    dmShtrihPOSF: Result := 10;				  // Ôèñêàëüíûé áëîê Øòðèõ-POS-Ô
    dmShtrih500: Result := 0;					  // ØÒÐÈÕ-500
    dmShtrihMFRK: Result := 250;        // ØÒÐÈÕ-Ì-ÔÐ-Ê
    dmShtrihLightFRK: Result := 252;   // ØÒÐÈÕ-LIGHT-ÔÐ-Ê
    dmYARUS01K: Result := 249;          // ßÐÓÑ-01Ê
    dmYARUS02K: Result := 248;          // ßÐÓÑ-02Ê
    dmYARUSM2100K: Result := 20;        // ßÐÓÑ Ì2100Ê
  else
    Result := 0;
  end;
end;

{ Ïîëó÷åíèå ìîäåëè ïî ïàðàìåòðàì óñòðîéñòâà }

function MetricsToModel(UModel, UMajorType, UMinorType,
  UMajorProtocolVersion, UMinorProtocolVersion: Integer): TDeviceModel;
begin
  Result := dmUnknown;
  case UMajorType of
    0:
    begin
      if UMinorType = 0 then
      begin
        case UModel of
          0:
          begin
            if UMajorProtocolVersion = 1 then
            begin
              if UMinorProtocolVersion < 4 then
              begin
                Result := dmShtrihFRF3;
              end else
              begin
                Result := dmShtrihFRF4;
              end;
            end else
            begin
              Result := dmShtrihFRF3;
            end;
          end;
           1: Result := dmShtrihFRFKaz;
           2: Result := dmElvesMiniFRF;	     // ÝËÂÅÑ-ÌÈÍÈ-ÔÐ-Ô
           3: Result := dmFelixRF; 	         // ÔÅËÈÊÑ-Ð Ô
           4: Result := dmShtrihFRK;	       // ØÒÐÈÕ-ÔÐ-Ê
           5: Result := dmShtrih950K;	       // ØÒÐÈÕ-950Ê
           6: Result := dmElvesFRK; 	       // ÝËÂÅÑ-ÔÐ-Ê
           7: Result := dmShtrihMiniFRK;     // ØÒÐÈÕ-ÌÈÍÈ-ÔÐ-Ê
           8: Result := dmShtrihFRFBel;      // ØÒÐÈÕ-ÔÐ-Ô (Áåëîðóññèÿ)
           9: Result := dmShtrihComboFRKv1;  // ØÒÐÈÕ-ÊÎÌÁÎ-ÔÐ-Ê âåðñèè 1
          10: Result := dmShtrihPOSF;	       // Ôèñêàëüíûé áëîê Øòðèõ-POS-Ô
          11: Result := dmShtrih950Kv2;	     // Øòðèõ950K âåðñèÿ 2
          12: Result := dmShtrihComboFRKv2;  // ØÒÐÈÕ-ÊÎÌÁÎ-ÔÐ-Ê âåðñèè 2
          14: Result := dmShtrihMiniFRK2;    // ØÒÐÈÕ-ÌÈÍÈ-ÔÐ-Ê 2
          20: Result := dmYARUSM2100K;       // ßÐÓÑ Ì2100Ê
          250: Result := dmShtrihMFRK;       // ØÒÐÈÕ-Ì-ÔÐ-Ê
          252: Result := dmShtrihLightFRK;   // ØÒÐÈÕ-LIGHT-ÔÐ-Ê
          249: Result := dmYARUS01K;         // ßÐÓÑ-01Ê
          248: Result := dmYARUS02K;         // ßÐÓÑ-02Ê
          19: Result := dmShtrihMobilePTK; //"ØÒÐÈÕ-MOBILE-ÏÒÊ"
          21: Result := dmYarusTK; //- "YARUS-ÒÊ" | "ÀÑÏÄ YARUS C21"
          22: Result := dmRetail01K; //- "Retail-01Ê"
          23: Result := dmRR02K; //- "RR-02Ê"
          24: Result := dmRR01K; //- "RR-01Ê"
          25: Result := dmRR04K; //- "RR-04Ê"
          26: Result := dmRR03K; //- "RR-03Ê"
        end;
      end;
    end;
    5:
    begin
      if UMinorType = 0 then
      begin
        if UModel = 0 then Result := dmShtrih500;
      end;
    end;
  end;
end;

// Ïîëó÷åíèå øèðèíû ïå÷àòè

function ModelToDataSize(Value: TDeviceModel): Integer;
begin
  case Value of
    dmShtrihFRF3        : Result := 36;   // ØÒÐÈÕ-ÔÐ-Ô (âåðñèÿ 3)
    dmShtrihFRF4        : Result := 36;	  // ØÒÐÈÕ-ÔÐ-Ô (âåðñèÿ 4)
    dmShtrihFRFKaz      : Result := 36;	  // ØÒÐÈÕ-ÔÐ-Ô (Êàçàõñêàÿ âåðñèÿ)
    dmElvesMiniFRF      : Result := 24;	  // ÝËÂÅÑ-ÌÈÍÈ-ÔÐ-Ô
    dmFelixRF           : Result := 20;   // ÔÅËÈÊÑ-Ð Ô
    dmShtrihFRK         : Result := 36;	  // ØÒÐÈÕ-ÔÐ-Ê
    dmShtrih950K        : Result := 40;	  // ØÒÐÈÕ-950Ê
    dmElvesFRK          : Result := 32;   // ÝËÂÅÑ-ÔÐ-Ê
    dmShtrihMiniFRK     : Result := 50;   // ØÒÐÈÕ-ÌÈÍÈ-ÔÐ-Ê
    dmShtrihFRFBel      : Result := 36;   // ØÒÐÈÕ-ÔÐ-Ô (Áåëîðóññèÿ)
    dmShtrihComboFRKv1  : Result := 48;   // ØÒÐÈÕ-ÊÎÌÁÎ-ÔÐ-Ê âåðñèè 1
    dmShtrihPOSF        : Result := 40;	  // Ôèñêàëüíûé áëîê Øòðèõ-POS-Ô
    dmShtrih950Kv2      : Result := 40;	  // Øòðèõ950K âåðñèÿ 2
    dmShtrihComboFRKv2  : Result := 40;   // ØÒÐÈÕ-ÊÎÌÁÎ-ÔÐ-Ê âåðñèè 2
    dmShtrihMiniFRK2    : Result := 50;   // ØÒÐÈÕ-ÌÈÍÈ-ÔÐ-Ê 2
    dmShtrihMFRK        : Result := 48;   // ØÒÐÈÕ-Ì-ÔÐ-Ê
    dmShtrihLightFRK    : Result := 32;   // ØÒÐÈÕ-LIGHT-ÔÐ-Ê
    dmShtrihMobilePTK: Result := 19;  //"ØÒÐÈÕ-MOBILE-ÏÒÊ"
    dmYarusTK: Result := 21;  //- "YARUS-ÒÊ" | "ÀÑÏÄ YARUS C21"
    dmRetail01K: Result := 22;  //- "Retail-01Ê"
    dmRR02K: Result := 23;  //- "RR-02Ê"
    dmRR01K: Result := 24;  //- "RR-01Ê"
    dmRR04K: Result := 25;  //- "RR-04Ê"
    dmRR03K: Result := 26;  //- "RR-03Ê"

  else
    Result := 40;
  end;
end;

function Str2EcrDate(const Data: string; Index: Integer): TEcrDate;
begin
  Result.Day := Ord(Data[Index]);
  Result.Month := Ord(Data[Index + 1]);
  Result.Year := Ord(Data[Index + 2]);
end;

procedure InvalidProp(const PropName: WideString);
resourcestring
  SInvalidPropValue = 'Íåâåðíîå çíà÷åíèå ñâîéñòâà %s.';
begin
  RaiseError(E_INVALIDPARAM, Format(GetRes(@SInvalidPropValue), [PropName]));
end;

// ×òåíèå ðàçìåðîâ è ïîëîæåíèÿ ôîðìû òàáëèöû èç ðååñòðà
procedure ReadTableWindowParams(ATableNumber: Integer; AForm: TForm; RootKey: HKEY);
var
  Reg: TRegistry;
  L, T, H, W: Integer;
  isOK: Boolean;
  S: string;
begin
  if AForm = nil then Exit;
  L := AForm.Left;
  T := AForm.Top;
  W := AForm.Width;
  H := AForm.Height;
  isOK := True;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if not Reg.OpenKey(REGSTR_KEY_TABLEPARAMS, True) then Abort;

    S := 'Table' + IntToStr(ATableNumber) + '.' + REGSTR_VAL_TABLE_LEFT;
    if not Reg.ValueExists(S) then Abort;
    L := Reg.ReadInteger(S);

    S := 'Table' + IntToStr(ATableNumber) + '.' + REGSTR_VAL_TABLE_TOP;
    if not Reg.ValueExists(S) then Abort;
    T := Reg.ReadInteger(S);

    S := 'Table' + IntToStr(ATableNumber) + '.' + REGSTR_VAL_TABLE_WIDTH;
    if not Reg.ValueExists(S) then Abort;
    W := Reg.ReadInteger(S);

    S := 'Table' + IntToStr(ATableNumber) + '.' + REGSTR_VAL_TABLE_HEIGHT;
    if not Reg.ValueExists(S) then Abort;
    H := Reg.ReadInteger(S);
  except
    isOK := False;
  end;
  Reg.Free;
  if not isOK then Exit;
  AForm.Position := poDesigned;
  AForm.Left := L;
  AForm.Top := T;
  AForm.Width := W;
  AForm.Height := H;
end;

// Çàïèñü ðàçìåðîâ è ïîëîæåíèÿ ôîðìû òàáëèöû èç ðååñòðà
procedure SaveTableWindowParams(ATableNumber: Integer; AForm: TForm; RootKey: HKEY);
var
  Reg: TRegistry;
  S: string;
begin
  if AForm = nil then Exit;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if not Reg.OpenKey(REGSTR_KEY_TABLEPARAMS, True) then Abort;

    S := 'Table' + IntToStr(ATableNumber) + '.' + REGSTR_VAL_TABLE_LEFT;
    Reg.WriteInteger(S, AForm.Left);

    S := 'Table' + IntToStr(ATableNumber) + '.' + REGSTR_VAL_TABLE_TOP;
    Reg.WriteInteger(S, AForm.Top);

    S := 'Table' + IntToStr(ATableNumber) + '.' + REGSTR_VAL_TABLE_WIDTH;
    Reg.WriteInteger(S, AForm.Width);

    S := 'Table' + IntToStr(ATableNumber) + '.' + REGSTR_VAL_TABLE_HEIGHT;
    Reg.WriteInteger(S, AForm.Height);
  except
  end;
  Reg.Free;
end;

// Ïðîòîêîë Êß
function DecodeTCPPort(ACode: Byte): Integer;
var
  LoValue: Byte;
  HiValue: Byte;
begin
  LoValue := (ACode and $0F) or $30;
  HiValue := (ACode and $F0);
  Result := MakeWord(LoValue, HiValue);
end;

// Ïðîòîêîë Êß
function EncodeTCPPort(ATCPPort: Word): Byte;
begin
  Result := ((ATCPPort and $F000) shr 12) shl 4 or (ATCPPort and $000F);
end;


const
  c_CRC32I_Table: array [0 .. 255] of DWORD = (                                                                                                                                     //
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, //
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, //
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, //
    $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, //
    $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01, //
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, //
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9, //
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD, //
    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, //
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, //
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, //
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D, //
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, //
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, //
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9, //
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D  //
    );
{ CRC32 IEEE 802.3 Table }

function CRC_IEEE_802_3(Buffer: PByte; len: Integer): DWORD;
var
  I: Integer;
begin
  Result := $FFFFFFFF;
  for I  := 0 to len - 1 do
  begin
    Result := ((Result shr 8) and $00FFFFFF) xor c_CRC32I_Table[(Result xor Buffer^) and $FF];
    Inc(Buffer);
  end;
  Result := Result xor $FFFFFFFF;
end;

end.
