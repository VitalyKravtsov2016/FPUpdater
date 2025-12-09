unit StringUtils;

interface

uses
  // VCL
  Windows, SysUtils, Classes;

function IsDigit(Key: AnsiChar): Boolean;
function ConvertCharCodeString(const S: WideString): WideString;

type
  TSetOfChar = set of AnsiChar;

    TWideStringStream = class(TStream)
    private
      FDataP: PChar;
      FSize: Longint;
      FCapacity: Longint;
      FPosition: Longint;
      procedure InternalSetCapacity(NewCapacity: Longint);
      procedure InternalGrowCapacity(Needed: Longint);
      procedure InternalSetSize(NewSize: Longint);
      function GetData: PWideChar;
    protected
      // exposes internal data buffer - use with care
      property Data: PWideChar read GetData;
      procedure SetCapacity(NewCapacity: Longint); virtual;
      procedure SetSize(NewSize: Longint); override;
      procedure SetStringPosition(Value: Longint);
      function GetStringPosition: Longint;
      procedure SetStringLength(Value: Longint);
      function GetStringLength: Longint;
      function GetDataString: WideString;
    public
      constructor Create(const AString: WideString); overload;
      destructor Destroy; override;
      function Read(var Buffer; Count: Longint): Longint; override;
      function ReadString(Count: Longint): WideString;
      function Seek(Offset: Longint; Origin: Word): Longint; override;
      function Write(const Buffer; Count: Longint): Longint; override;
      function WriteNullTerminated(Buffer: PWideChar): Longint;
      procedure WriteChars(Chars: PWideChar; Count: Longint);
      procedure WriteString(const AString: WideString);
      property Capacity: Longint read FCapacity write SetCapacity;
      property StringLength: Longint read GetStringLength write SetStringLength;
      property StringPosition: Longint read GetStringPosition write SetStringPosition;
      property DataString: WideString read GetDataString;
    end;

function Inverse(const S: AnsiString): AnsiString;
function StrToHex(const S: AnsiString): string;
function StrToHexText(const S: AnsiString): string;
function HexToStr(const Data: string): AnsiString;
function CurrencyToStr(Value: Currency): string;
function StrToCurrency(const S: string): Currency;
function BoolToStr(Value: Boolean): AnsiString;

function Str1251To866(const S: AnsiString): AnsiString;
function Str866To1251(const S: AnsiString): AnsiString;

function GetString(const S: string; K: Integer; Delimiters: TSetOfChar): string;
function GetInteger(const Data: string; Index: Integer; Delimiters: TSetOfChar): Integer;

function WideStringToAnsiString(CodePage: Integer; const S: WideString): AnsiString;
function AnsiStringToWideString(CodePage: Integer; const S: AnsiString): WideString;

function AddLeadingZeros(const S: string; ACount: Integer): string;
function AddFinalSpaces(const S: string; ACount: Integer): string;
function DecimalToString(Value: Currency): WideString;
function AddLeadingZerobytes(const S: string; ACount: Integer): string;
function AddFinalZeroByts(const S: string; ACount: Integer): string;


implementation

resourcestring
  SCannotReadOddPos = 'Cannot read WideString from odd byte position';
  SCannotWriteOddPos = 'Cannot write WideString to odd byte position';
  SOddSizeInvalid = 'Odd size not valid for WideString';
  SNegativeSizeInvalid = 'Negative stream size invalid';
  SNegativeCapacityInvalid = 'Negative stream capacity invalid';
  SOddPosInvalid = 'Odd byte position not valid for WideString';
  SStringPositionInvalid = 'String position invalid';
  SCapacityLessSize = 'Capacity cannot be less than size';
  STargetNil = 'Target must not be nil';
  // STargetClosed = 'Target closed';
  SBufferSizeNotPositive = 'Buffer size must be positive';
  SReadBufferHasData = 'Read buffer has unprocessed data';
  SInvalidProcessInBuffer = 'Invalid call to ProcessInBuffer()';
  SInvalidProcessOutBuffer = 'Invalid call to ProcessOutBuffer()';
  SRecursiveRead = 'Recursive call to Read()';
  SRecursiveWrite = 'Recursive call to Write()';
  SStackCapacityError = 'Invalid Capacity for Stack: %d';
  SSwapDistanceError = 'Invalid Swap Distance: %d';
  SRotateDistanceError = 'Invalid Rotate Distance: %d';
  SInvalidBase64Encoding = 'Invalid Base64 encoding: %s';
  SBase64EndDetected = 'End of Base64 conversion detected';

  SWriterNotOpen = 'Writer not open';
  SReaderNotOpen = 'Reader not open';
  SReaderClosed = 'Reader closed';
  SFilterOpen = 'Filter open';
  SFilterNotOpen = 'Filter not open';
  SFilterClosed = 'Filter closed';
  SFilterClosing = 'Filter closing';
  SFilterNotClosed = 'Filter not closed';
  SFilterNil = 'Filter must not be nil';
  SFilterProcessorNil = 'Filter processor must not be nil';
  SFilterDriverNil = 'Filter driver must not be nil';
  SInvalidFilterInBuffer = 'Invalid call to ProcessInBuffer()';
  SInvalidFilterOutBuffer = 'Invalid call to ProcessOutBuffer()';
  SExtraDataFound = 'Extra data found';
  SUnexpectedFilterState = 'Unexpected filter state';
  SProcessorArgs = 'Invalid processor arguments';
  SStreamNil = 'Stream must not be nil';
  SWriterError = 'Writer error';
  SInvalidBuffer = 'Invalid buffer';
  SIOObjectNotReset = 'IO object not reset';
  SUnprocessedInput = 'Unprocessed input: %d bytes';

  SStringTableTooSmall = 'String table capacity too small'; 

function AddLeadingZeros(const S: string; ACount: Integer): string;
begin
  Result := Copy(S, 1, ACount);
  if ACount < Length(S) then Exit;
  Result := StringOfChar('0', ACount - Length(S)) + S;
end;

function AddLeadingZerobytes(const S: string; ACount: Integer): string;
begin
  Result := Copy(S, 1, ACount);
  if ACount < Length(S) then Exit;
  Result := StringOfChar(#0, ACount - Length(S)) + S;
end;

function AddFinalSpaces(const S: string; ACount: Integer): string;
begin
  Result := Copy(S, 1, ACount);
  if ACount < Length(S) then Exit;
  Result := S + StringOfChar(' ', ACount - Length(S));
end;

function AddFinalZeroByts(const S: string; ACount: Integer): string;
begin
  Result := Copy(S, 1, ACount);
  if ACount < Length(S) then Exit;
  Result := S + StringOfChar(#0, ACount - Length(S));
end;

function IsDigit(Key: AnsiChar): Boolean;
begin
  Result := Key in ['0'..'9', AnsiChar(VK_CLEAR), AnsiChar(VK_BACK)];
end;

// Преобразование строки для реализации возможности установки шрифтов.
// Для установки шрифта нужно использовать символы &font1;
// Строка &27;asd&font3;123 должна быть преобразована в 27#1'asd'#27#3'123'
// &font145;

function ConvertCharCodeString(const S: WideString): WideString;
{var
  P: Integer;
  R: TRegExpr;
  Line: WideString;
  OldPattern: WideString;
  NewPattern: WideString;}
begin
  Result := S;
{  Line := S;
  Result := '';
  repeat
    R := TRegExpr.Create;
    try
      R.Expression := '&[\$0-9]*[;]';
      if R.Exec(Line) then
      begin
        OldPattern := R.Match[0];
        NewPattern := Char(StrToInt(Copy(OldPattern, 3, Length(OldPattern)-3)));
        P := WideTextPos(OldPattern, Line);
        Result := Result + Copy(Line, 1, P-1) + NewPattern;
        Line := Copy(Line, P + Length(OldPattern), Length(Line));
      end else
      begin
        OldPattern := '';
        NewPattern := '';
        Result := Result + Line;
        Break;
      end;
    finally
      R.Free;
    end;
  until False;}
end;


function GetSubString(const S: string; var Value: string; K: Integer;
  Delimiters: TSetOfChar): Boolean;
var
  LastPos: Integer;
  CurPos: Integer;
  CurParam: Integer;
  Len: Integer;
begin
  Result := False;
  Value := '';
  Len := Length(S);
  CurParam := 1;
  CurPos := 1;
  while (CurPos <= Len) and (CurParam <= K) do
  begin
    LastPos := CurPos;

    while (CurPos <= Len) and not (CharInSet(S[CurPos], Delimiters)) do Inc(CurPos);
    if CurParam = K then
    begin
      Result := True;
      Value := Copy(S, LastPos, CurPos - LastPos);
      Exit;
    end;
    Inc(CurPos);
    Inc(CurParam);
  end;
end;

function GetString(const S: string; K: Integer; Delimiters: TSetOfChar): string;
begin
  Result := '';
  GetSubString(S, Result, K, Delimiters);
end;

function GetInteger(const Data: string; Index: Integer; Delimiters: TSetOfChar): Integer;
var
  S: string;
begin
  Result := 0;
  if GetSubString(Data, S, Index, Delimiters) then
    Result := StrToInt(S);
end;

function Inverse(const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + AnsiChar(Ord(S[i]) xor $FF);
end;

function CurrencyToStr(Value: Currency): string;
var
  SaveDecimalSeparator: Char;
begin
  SaveDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    Result := Format('%.2f', [Value]);
  finally
    FormatSettings.DecimalSeparator := SaveDecimalSeparator;
  end;
end;

function StrToCurrency(const S: string): Currency;
var
  SaveDecimalSeparator: Char;
begin
  SaveDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    Result := StrToFloat(S);
  finally
    FormatSettings.DecimalSeparator := SaveDecimalSeparator;
  end;
end;

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

function StrToHexText(const S: AnsiString): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    Result := Result + IntToHex(Ord(S[i]), 2);
  end;
end;

function HexToStr(const Data: string): AnsiString;
var
  S: string;
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
      if Code <> 0 then Exit;
      Result := Result + AnsiChar(V);
      S := '';
    end;
  end;
  // Last symbol
  if Length(S) <> 0 then
  begin
    Val('$' + S, V, Code);
    if Code <> 0 then Exit;
    Result := Result + AnsiChar(V);
  end;
end;

function BoolToStr(Value: Boolean): AnsiString;
begin
  if Value then Result := 'True'
  else Result := 'False';
end;

const
  Table866To1251: array [0..255] of Byte = (
  {00} $00,$01,$02,$03,$04,$05,$06,$07, $08,$09,$0A,$0B,$0C,$0D,$0E,$0F,
  {10} $10,$11,$12,$13,$14,$15,$16,$17, $18,$19,$1A,$1B,$1C,$1D,$1E,$1F,
  {20} $20,$21,$22,$23,$24,$25,$26,$27, $28,$29,$2A,$2B,$2C,$2D,$2E,$2F,
  {30} $30,$31,$32,$33,$34,$35,$36,$37, $38,$39,$3A,$3B,$3C,$3D,$3E,$3F,
  {40} $40,$41,$42,$43,$44,$45,$46,$47, $48,$49,$4A,$4B,$4C,$4D,$4E,$4F,
  {50} $50,$51,$52,$53,$54,$55,$56,$57, $58,$59,$5A,$5B,$5C,$5D,$5E,$5F,
  {60} $60,$61,$62,$63,$64,$65,$66,$67, $68,$69,$6A,$6B,$6C,$6D,$6E,$6F,
  {70} $70,$71,$72,$73,$74,$75,$76,$77, $78,$79,$7A,$7B,$7C,$7D,$7E,$7F,
  {80} $C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7, $C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF,
  {90} $D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7, $D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF,
  {A0} $E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7, $E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF,
  {B0} $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F, $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
  {C0} $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F, $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
  {D0} $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F, $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
  {E0} $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7, $F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF,
  {F0} $A8,$B8,$AA,$BA,$AF,$BF,$A1,$A2, $B0,$3F,$B7,$3F,$B9,$A4,$3F,$A0
  );

function Char866To1251(C: AnsiChar): AnsiChar;
begin
  Result := AnsiChar(Table866To1251[Ord(C)]);
end;

function Str866To1251(const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + Char866To1251(S[i]);
end;

const
  Table1251To866: array [0..255] of Byte = (
  {00} $00,$01,$02,$03,$04,$05,$06,$07, $08,$09,$0A,$0B,$0C,$0D,$0E,$0F,
  {10} $10,$11,$12,$13,$14,$15,$16,$17, $18,$19,$1A,$1B,$1C,$1D,$1E,$1F,
  {20} $20,$21,$22,$23,$24,$25,$26,$27, $28,$29,$2A,$2B,$2C,$2D,$2E,$2F,
  {30} $30,$31,$32,$33,$34,$35,$36,$37, $38,$39,$3A,$3B,$3C,$3D,$3E,$3F,
  {40} $40,$41,$42,$43,$44,$45,$46,$47, $48,$49,$4A,$4B,$4C,$4D,$4E,$4F,
  {50} $50,$51,$52,$53,$54,$55,$56,$57, $58,$59,$5A,$5B,$5C,$5D,$5E,$5F,
  {60} $60,$61,$62,$63,$64,$65,$66,$67, $68,$69,$6A,$6B,$6C,$6D,$6E,$6F,
  {70} $70,$71,$72,$73,$74,$75,$76,$77, $78,$79,$7A,$7B,$7C,$7D,$7E,$7F,

  {80} $3F,$3F,$2C,$3F,$3F,$3F,$3F,$3F, $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
  {90} $3F,$3F,$2C,$3F,$3F,$3F,$3F,$3F, $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,
  {A0} $FF,$F6,$F7,$3F,$FD,$3F,$3F,$3F, $F0,$3F,$F2,$3F,$3F,$3F,$3F,$F4,
  {B0} $F8,$3F,$3F,$3F,$3F,$3F,$3F,$FA, $F1,$FC,$F3,$3F,$3F,$6A,$73,$F5,
  {C0} $80,$81,$82,$83,$84,$85,$86,$87, $88,$89,$8A,$8B,$8C,$8D,$8E,$8F,
  {D0} $90,$91,$92,$93,$94,$95,$96,$97, $98,$99,$9A,$9B,$9C,$9D,$9E,$9F,
  {E0} $A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7, $A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF,
  {F0} $E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7, $E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF
  );

function Char1251To866(C: AnsiChar): AnsiChar;
begin
  Result := AnsiChar(Table1251To866[Ord(C)]);
end;

function Str1251To866(const S: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + Char1251To866(S[i]);
end;

function WideStringToAnsiString(CodePage: Integer; const S: WideString): AnsiString;
var
  P: PAnsiChar;
  Count: Integer;
  CharCount: Integer;
begin
  Result := '';
  Count := WideCharToMultiByte(CodePage, 0, PWideChar(S), Length(S), nil, 0,
    nil, nil);
  if Count > 0 then
  begin
    P := AllocMem(Count);
    CharCount := WideCharToMultiByte(CodePage, 0, PWideChar(S), Length(S),
      P, Count, nil, nil);
    Result := P;
    SetLength(Result, CharCount);

    FreeMem(P);
  end;
end;

function AnsiStringToWideString(CodePage: Integer; const S: AnsiString): WideString;
var
  P: PWideChar;
  Count: Integer;
  CharCount: Integer;
begin
  Result := '';
  Count := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), Length(S), nil, 0);
  if Count > 0 then
  begin
    P := AllocMem(Count*Sizeof(WideChar));
    CharCount := MultiByteToWideChar(CodePage, 0, PAnsiChar(S), Length(S), P, Count);
    Result := P;
    SetLength(Result, CharCount);
    FreeMem(P);
  end;
end;



{ TWideStringStream }

{ This is a stream interface for Widestrings.
  Purpose: avoid excessive memory re-allocations that occur with
    WideStrings because they are not reference counted.
  Note: when the DataString property is accessed, the complete
    WideString will be copied out                                }

constructor TWideStringStream.Create(const AString: WideString);
  begin
  inherited Create;
  //on creation, don't grow capacity beyond string
  FSize := Length(AString) shl 1;
  ReallocMem(FDataP, FSize);
  FCapacity := FSize;
  Move(Pointer(AString)^, FDataP^, FSize);
  end;

destructor TWideStringStream.Destroy;
  begin
  FreeMem(FDataP);
  inherited Destroy;
  end;

function TWideStringStream.GetData: PWideChar;
  begin
  Result := PWideChar(FDataP);
  end;

function TWideStringStream.GetDataString: WideString;
  begin
  SetLength(Result, StringLength);
  Move(FDataP^, Pointer(Result)^, FSize);
  end;

{ gets string length in terms of WideChars }
function TWideStringStream.GetStringLength: Longint;
  begin
  Result := FSize shr 1;
  end;

{ gets string position in terms of WideChars }
function TWideStringStream.GetStringPosition: Longint;
  begin
  if Odd(Position) then raise EStreamError.Create(SOddPosInvalid);
  Result := (Position shr 1) + 1;
  end;

procedure TWideStringStream.InternalGrowCapacity(Needed: Longint);
  var
    Delta: Longint;
  begin
  Delta := Needed shr 2;
  if Delta < 8 then Delta := 8;
  InternalSetCapacity(Needed + Delta);
  end;

{ sets stream capacity in bytes, rounds to Longword boundary }
procedure TWideStringStream.InternalSetCapacity(NewCapacity: Longint);
  begin
  NewCapacity := ((NewCapacity + 3) shr 2) shl 2; //Longword boundary
  ReallocMem(FDataP, NewCapacity);
  FCapacity := NewCapacity;
  end;

{ caller must make sure that NewSize is an even, positive number }
procedure TWideStringSTream.InternalSetSize(NewSize: Longint);
  begin
  if NewSize > Capacity then InternalGrowCapacity(NewSize);
  FSize := NewSize;
  end;

function TWideStringStream.Read(var Buffer; Count: Longint): Longint;
  begin
  Result := FSize - FPosition;
  if Result > Count then Result := Count;
  Move((FDataP + FPosition)^, Buffer, Result);
  FPosition := FPosition + Result;
  end;

{ reads Count WideChars from stream and returns them as WideString }
function TWideStringStream.ReadString(Count: Longint): WideString;
  var
    Len: Longint;
  begin
  if Odd(FPosition) then raise EReadError.Create(SCannotReadOddPos);
  Len := (FSize - FPosition) shr 1;
  if Len > Count then Len := Count;
  SetLength(Result, Len);
  Read(Pointer(Result)^, Len shl 1);
  end;

function TWideStringStream.Seek(Offset: Longint; Origin: Word): Longint;
  begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := FSize - Offset;
    end;
  if FPosition > FSize then FPosition := FSize
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
  end;

{ for user code - checks against Size }
procedure TWideStringStream.SetCapacity(NewCapacity: Longint);
  begin
  Assert(NewCapacity >= 0, SNegativeCapacityInvalid);
  if NewCapacity < FSize then raise EStreamError.Create(SCapacityLessSize);
  InternalSetCapacity(NewCapacity);
  end;

{ sets stream size in bytes }
procedure TWideStringStream.SetSize(NewSize: Longint);
  begin
  Assert(NewSize >= 0, SNegativeSizeInvalid);
  if Odd(NewSize) then raise EStreamError.Create(SOddSizeInvalid);
  InternalSetSize(NewSize);
  if FPosition > NewSize then FPosition := NewSize;
  end;

{ sets string length in terms of WideChars }
procedure TWideStringStream.SetStringLength(Value: Longint);
  begin
  SetSize(Value shl 1);
  end;

{ sets string position in terms of WideChars }
procedure TWideStringStream.SetStringPosition(Value: Longint);
  begin
  if Value < 1 then raise EStreamError.Create(SStringPositionInvalid);
  Position := (Value - 1) shl 1;
  end;

{ standard Write operation - operates in Bytes, not WideChars
  Note: does not grow capacity beyond what is immediately needed }
function TWideStringStream.Write(const Buffer; Count: Longint): Longint;
  var
    NewPos: Longint;
  begin
  Result := Count;
  NewPos := FPosition + Result;
  if NewPos > FSize then
    InternalSetSize(((NewPos + 1) shr 1) shl 1); //next larger even value
  Move(Buffer, (FDataP + FPosition)^, Result);
  FPosition := NewPos;           //next position to be written to
  end;

procedure TWideStringStream.WriteChars(Chars: PWideChar; Count: Longint);
  begin
  Write(Chars^, Count * SizeOf(WideChar));
  end;

function TWideStringStream.WriteNullTerminated(Buffer: PWideChar): Longint;
  const
    NullChar = WideChar(#0);
  var
    BufChar: WideChar;
    NewPosition: Longint;
  begin
  if Odd(FPosition) then raise EWriteError.Create(SCannotWriteOddPos);
  //Assert((not Odd(FSize)) and (FPosition <= (FSize - SizeOf(WideChar))));
  Result := 0;
  NewPosition := FPosition;
  BufChar := Buffer^;
  while BufChar <> NullChar do
    begin
    if NewPosition >= Capacity then
      InternalGrowCapacity(NewPosition + SizeOf(WideChar));
    PWideChar(FDataP + NewPosition)^ := BufChar;
    NewPosition := NewPosition + SizeOf(WideChar);
    Inc(Result);
    BufChar := Buffer[Result];
    end;
  FPosition := NewPosition;
  if NewPosition > FSize then FSize := NewPosition;
  end;

{ writes AString into stream, starting at StringPosition, overwriting
  existing characters and extending the stream if necessary           }
procedure TWideStringStream.WriteString(const AString: WideString);
  begin
  if Odd(FPosition) then raise EWriteError.Create(SCannotWriteOddPos);
  Write(Pointer(AString)^, Length(AString) shl 1);
  end;

function DecimalToString(Value: Currency): WideString;
var
  C: Char;
begin
  C := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    Result := CurrToStr(Value);
  finally
    FormatSettings.DecimalSeparator := C;
  end;
end;


end.
