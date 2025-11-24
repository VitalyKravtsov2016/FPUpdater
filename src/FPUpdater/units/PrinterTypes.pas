unit PrinterTypes;

interface

uses
  // VCL
  Windows, SysUtils,
  // This
  BinUtils, DriverTypes, TextEncoding, LangUtils;

const
  CRLF = #13#10;

  /////////////////////////////////////////////////////////////////////////////

  BAUD_RATE_CODE_2400   = 0;
  BAUD_RATE_CODE_4800   = 1;
  BAUD_RATE_CODE_9600   = 2;
  BAUD_RATE_CODE_19200  = 3;
  BAUD_RATE_CODE_38400  = 4;
  BAUD_RATE_CODE_57600  = 5;
  BAUD_RATE_CODE_115200 = 6;
  BAUD_RATE_CODE_230400 = 7;
  BAUD_RATE_CODE_460800 = 8;
  BAUD_RATE_CODE_921600 = 9;

  BAUD_RATE_CODE_MIN = BAUD_RATE_CODE_2400;
  BAUD_RATE_CODE_MAX = BAUD_RATE_CODE_921600;

  BAUD_RATE_CODE_SEARCH_ORDER: array[1..10] of Integer =
                                (BAUD_RATE_CODE_115200,
                                 BAUD_RATE_CODE_4800,
                                 BAUD_RATE_CODE_19200,
                                 BAUD_RATE_CODE_9600,
                                 BAUD_RATE_CODE_38400,
                                 BAUD_RATE_CODE_57600,
                                 BAUD_RATE_CODE_230400,
                                 BAUD_RATE_CODE_460800,
                                 BAUD_RATE_CODE_921600,
                                 BAUD_RATE_CODE_2400
                                );

  // 2D barcode alignment constants

  BARCODE_2D_ALIGNMENT_LEFT     = 0;
  BARCODE_2D_ALIGNMENT_CENTER   = 1;
  BARCODE_2D_ALIGNMENT_RIGHT    = 2;


  BARCODE_TYPE_QR_GRAPH = 100;

type

  { TPrinterDate }

  TPrinterDate = packed record
    Day: Byte;
    Month: Byte;
    Year: Byte;
  end;

  { TPrinterTime }

  TPrinterTime = packed record
    Hour: Byte;
    Min: Byte;
    Sec: Byte;
  end;

  { TPrinterDateTime }

  TPrinterDateTime = packed record
    Day: Byte;
    Month: Byte;
    Year: Byte;
    Hour: Byte;
    Min: Byte;
    Sec: Byte;
  end;


  { T2DBarcode }

  T2DBarcode = record
    Password: DWORD;
    BarcodeType: Byte;
    DataLength: Word;
    BlockNumber: Byte;
    Parameter1: Byte;
    Parameter2: Byte;
    Parameter3: Byte;
    Parameter4: Byte;
    Parameter5: Byte;
    Alignment: Byte;
  end;

  { TBlockData }

  TBlockData = record
    Password: DWORD;
    BlockType: Byte;
    BlockNumber: Byte;
    BlockData: AnsiString;
  end;

  { TPrinterError }

  TPrinterError = class
  public
    class function GetDescription(Code: Integer; ACapFN: Boolean = False): WideString;
  end;

  { TFontRec }

  TFontRec = record
    LineWidth: Integer;
    CharWidth: Integer;
    CharHeight: Integer;
  end;

  { TPrinterString }

  TPrinterString = class
  public
    class function Convert(const S: string): string;
  end;

  { T1CTax }

  T1CTax = array[1..4] of Single;
  TTaxNames = array[1..10] of string;


//  FDepartments = array[1..]

  { T1CPayNames }

  T1CPayNames = array[1..3] of string;

  TCashierRec = record
    CashierName: WideString;
    CashierPass: Integer;
  end;
  {TCashiers}
  TCashiers = array[1..30] of TCashierRec;

  { T1CDriverParams }

  T1CDriverParams = record
    Port: Integer;
    Speed: Integer;
    UserPassword: string;
    AdminPassword: string;
    Timeout: Integer;
    RegNumber: string;
    SerialNumber: string;
    Tax: T1CTax;
    CloseSession: Boolean;
    EnableLog: Boolean;
    PayNames: T1CPayNames;
    Cashiers: TCashiers;
    PrintLogo: Boolean;
    LogoSize: Integer;
    ConnectionType: Integer;
    ComputerName: string;
    IPAddress: string;
    TCPPort: Integer;
    ProtocolType: Integer;
    BufferStrings: Boolean;
    Codepage: Integer;
    BarcodeFirstLine: Integer;
    QRCodeHeight: Integer;
  end;

const

  // Шрифты Элвес-ФР-К без сжатия
  ElvesFRKFonts: array [1..7] of TFontRec = (
   (LineWidth: 384; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 45;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 384; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 25;));

  // Шрифты Элвес-ФР-К со сжатием
  ElvesFRKFontsCompressed: array [1..7] of TFontRec = (
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 22;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 384; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;));

  // Шрифты Штрих-ФР-Ф версии 3 (сжатия не бывает)
  FRF3Fonts: array [1..2] of TFontRec = (
   (LineWidth: 432; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 45;));

  // Шрифты Штрих-ФР-Ф версии 4 без сжатия
  FRF4Fonts: array [1..7] of TFontRec = (
   (LineWidth: 432; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 45;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 400; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 25;));

  // Шрифты Штрих-ФР-Ф версии 4 со сжатием
  FRF4FontsCompressed: array [1..7] of TFontRec = (
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 22;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 400; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;));

  // Шрифты Штрих-500 без сжатия
  Shtrih500Fonts: array [1..9] of TFontRec = (
   (LineWidth: 384; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 45;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 380; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 45;));

  // Шрифты Штрих-500 со сжатием
  Shtrih500FontsCompressed: array [1..9] of TFontRec = (
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 22;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 380; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 384; CharWidth: 24; CharHeight: 22;));

  // Шрифты Штрих-950
  Shtrih950Fonts: array [1..7] of TFontRec = (
   (LineWidth: 360; CharWidth:  9; CharHeight: 9;),
   (LineWidth: 360; CharWidth: 18; CharHeight: 9;),
   (LineWidth: 360; CharWidth:  9; CharHeight: 9;),
   (LineWidth: 360; CharWidth: 18; CharHeight: 9;),
   (LineWidth: 360; CharWidth: 11; CharHeight: 9;),
   (LineWidth: 360; CharWidth: 22; CharHeight: 9;),
   (LineWidth: 360; CharWidth:  7; CharHeight: 9;));

  // Шрифты Штрих-Комбо-ФР-К без сжатия
  ShtrihComboFonts: array [1..7] of TFontRec = (
   (LineWidth: 600; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 600; CharWidth: 24; CharHeight: 45;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 600; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 600; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 25;));

  // Шрифты Штрих-Комбо-ФР-К со сжатием
  ShtrihComboFontsCompressed: array [1..7] of TFontRec = (
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 600; CharWidth: 24; CharHeight: 22;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 600; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 600; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;));

  // Шрифты Штрих-Мини-ФР-Ф версии 1 (широкий) без сжатия
  ShtrihMiniFonts: array [1..7] of TFontRec = (
   (LineWidth: 600; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 600; CharWidth: 24; CharHeight: 45;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 600; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 600; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 25;));

  // Шрифты Штрих-Мини-ФР-Ф версии 1 (широкий) со сжатием
  ShtrihMiniFontsCompressed: array [1..7] of TFontRec = (
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 600; CharWidth: 24; CharHeight: 22;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 600; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 600; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 600; CharWidth: 12; CharHeight: 12;));

  // Шрифты Штрих-Мини-ФР-Ф версии 2 (узкий) без сжатия
  ShtrihMini2Fonts: array [1..7] of TFontRec = (
   (LineWidth: 432; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 45;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 432; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 25;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 25;));

  // Шрифты Штрих-Мини-ФР-Ф версии 2 (узкий) со сжатием
  ShtrihMini2FontsCompressed: array [1..7] of TFontRec = (
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 22;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 432; CharWidth: 24; CharHeight: 25;),
   (LineWidth: 432; CharWidth: 10; CharHeight: 19;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;),
   (LineWidth: 432; CharWidth: 12; CharHeight: 12;));

resourcestring
  SNoDescrpiption = 'Описание недоступно';

  /////////////////////////////////////////////////////////////////////////////
  // Cash registers

  SCashRegister00 = 'Накопление продаж в 1 отдел в чеке';
  SCashRegister01 = 'Накопление покупок в 1 отдел в чеке';
  SCashRegister02 = 'Накопление возврата продаж в 1 отдел в чеке';
  SCashRegister03 = 'Накопление возврата покупок в 1 отдел в чеке';
  SCashRegister04 = 'Накопление продаж в 2 отдел в чеке';
  SCashRegister05 = 'Накопление покупок в 2 отдел в чеке';
  SCashRegister06 = 'Накопление возврата продаж в 2 отдел в чеке';
  SCashRegister07 = 'Накопление возврата покупок в 2 отдел в чеке';
  SCashRegister08 = 'Накопление продаж в 3 отдел в чеке';
  SCashRegister09 = 'Накопление покупок в 3 отдел в чеке';
  SCashRegister0A = 'Накопление возврата продаж в 3 отдел в чеке';
  SCashRegister0B = 'Накопление возврата покупок в 3 отдел в чеке';
  SCashRegister0C = 'Накопление продаж в 4 отдел в чеке';
  SCashRegister0D = 'Накопление покупок в 4 отдел в чеке';
  SCashRegister0E = 'Накопление возврата продаж в 4 отдел в чеке';
  SCashRegister0F = 'Накопление возврата покупок в 4 отдел в чеке';
  SCashRegister10 = 'Накопление продаж в 5 отдел в чеке';
  SCashRegister11 = 'Накопление покупок в 5 отдел в чеке';
  SCashRegister12 = 'Накопление возврата продаж в 5 отдел в чеке';
  SCashRegister13 = 'Накопление возврата покупок в 5 отдел в чеке';
  SCashRegister14 = 'Накопление продаж в 6 отдел в чеке';
  SCashRegister15 = 'Накопление покупок в 6 отдел в чеке';
  SCashRegister16 = 'Накопление возврата продаж в 6 отдел в чеке';
  SCashRegister17 = 'Накопление возврата покупок в 6 отдел в чеке';
  SCashRegister18 = 'Накопление продаж в 7 отдел в чеке';
  SCashRegister19 = 'Накопление покупок в 7 отдел в чеке';
  SCashRegister1A = 'Накопление возврата продаж в 7 отдел в чеке';
  SCashRegister1B = 'Накопление возврата покупок в 7 отдел в чеке';
  SCashRegister1C = 'Накопление продаж в 8 отдел в чеке';
  SCashRegister1D = 'Накопление покупок в 8 отдел в чеке';
  SCashRegister1E = 'Накопление возврата продаж в 8 отдел в чеке';
  SCashRegister1F = 'Накопление возврата покупок в 8 отдел в чеке';
  SCashRegister20 = 'Накопление продаж в 9 отдел в чеке';
  SCashRegister21 = 'Накопление покупок в 9 отдел в чеке';
  SCashRegister22 = 'Накопление возврата продаж в 9 отдел в чеке';
  SCashRegister23 = 'Накопление возврата покупок в 9 отдел в чеке';
  SCashRegister24 = 'Накопление продаж в 10 отдел в чеке';
  SCashRegister25 = 'Накопление покупок в 10 отдел в чеке';
  SCashRegister26 = 'Накопление возврата продаж в 10 отдел в чеке';
  SCashRegister27 = 'Накопление возврата покупок в 10 отдел в чеке';
  SCashRegister28 = 'Накопление продаж в 11 отдел в чеке';
  SCashRegister29 = 'Накопление покупок в 11 отдел в чеке';
  SCashRegister2A = 'Накопление возврата продаж в 11 отдел в чеке';
  SCashRegister2B = 'Накопление возврата покупок в 11 отдел в чеке';
  SCashRegister2C = 'Накопление продаж в 12 отдел в чеке';
  SCashRegister2D = 'Накопление покупок в 12 отдел в чеке';
  SCashRegister2E = 'Накопление возврата продаж в 12 отдел в чеке';
  SCashRegister2F = 'Накопление возврата покупок в 12 отдел в чеке';
  SCashRegister30 = 'Накопление продаж в 13 отдел в чеке';
  SCashRegister31 = 'Накопление покупок в 13 отдел в чеке';
  SCashRegister32 = 'Накопление возврата продаж в 13 отдел в чеке';
  SCashRegister33 = 'Накопление возврата покупок в 13 отдел в чеке';
  SCashRegister34 = 'Накопление продаж в 14 отдел в чеке';
  SCashRegister35 = 'Накопление покупок в 14 отдел в чеке';
  SCashRegister36 = 'Накопление возврата продаж в 14 отдел в чеке';
  SCashRegister37 = 'Накопление возврата покупок в 14 отдел в чеке';
  SCashRegister38 = 'Накопление продаж в 15 отдел в чеке';
  SCashRegister39 = 'Накопление покупок в 15 отдел в чеке';
  SCashRegister3A = 'Накопление возврата продаж в 15 отдел в чеке';
  SCashRegister3B = 'Накопление возврата покупок в 15 отдел в чеке';
  SCashRegister3C = 'Накопление продаж в 16 отдел в чеке';
  SCashRegister3D = 'Накопление покупок в 16 отдел в чеке';
  SCashRegister3E = 'Накопление возврата продаж в 16 отдел в чеке';
  SCashRegister3F = 'Накопление возврата покупок в 16 отдел в чеке';
  SCashRegister40 = 'Накопление скидок с продаж в чеке';
  SCashRegister41 = 'Накопление скидок с покупок в чеке';
  SCashRegister42 = 'Накопление скидок с возврата продаж в чеке';
  SCashRegister43 = 'Накопление скидок с возврата покупок в чеке';
  SCashRegister44 = 'Накопление надбавок на продажи в чеке';
  SCashRegister45 = 'Накопление надбавок на покупок в чеке';
  SCashRegister46 = 'Накопление надбавок на возврата продаж в чеке';
  SCashRegister47 = 'Накопление надбавок на возврата покупок в чеке';
  SCashRegister48 = 'Накопление оплат продаж наличными в чеке';
  SCashRegister49 = 'Накопление оплат покупок наличными в чеке';
  SCashRegister4A = 'Накопление оплат возврата продаж наличными в чеке';
  SCashRegister4B = 'Накопление оплат возврата покупок наличными в чеке';
  SCashRegister4C = 'Накопление оплат продаж видом оплаты 2 в чеке';
  SCashRegister4D = 'Накопление оплат покупок видом оплаты 2 в чеке';
  SCashRegister4E = 'Накопление оплат возврата продаж видом оплаты 2 в чеке';
  SCashRegister4F = 'Накопление оплат возврата покупок видом оплаты 2 в чеке';
  SCashRegister50 = 'Накопление оплат продаж видом оплаты 3 в чеке';
  SCashRegister51 = 'Накопление оплат покупок видом оплаты 3 в чеке';
  SCashRegister52 = 'Накопление оплат возврата продаж видом оплаты 3 в чеке';
  SCashRegister53 = 'Накопление оплат возврата покупок видом оплаты 3 в чеке';
  SCashRegister54 = 'Накопление оплат продаж видом оплаты 4 в чеке';
  SCashRegister55 = 'Накопление оплат покупок видом оплаты 4 в чеке';
  SCashRegister56 = 'Накопление оплат возврата продаж видом оплаты 4 в чеке';
  SCashRegister57 = 'Накопление оплат возврата покупок видом оплаты 4 в чеке';
  SCashRegister58 = 'Оборот по налогу А с продаж в чеке';
  SCashRegister59 = 'Оборот по налогу А с покупок в чеке';
  SCashRegister5A = 'Оборот по налогу А с возврата продаж в чеке';
  SCashRegister5B = 'Оборот по налогу А с возврата покупок в чеке';
  SCashRegister5C = 'Оборот по налогу Б с продаж в чеке';
  SCashRegister5D = 'Оборот по налогу Б с покупок в чеке';
  SCashRegister5E = 'Оборот по налогу Б с возврата продаж в чеке';
  SCashRegister5F = 'Оборот по налогу Б с возврата покупок в чеке';
  SCashRegister60 = 'Оборот по налогу В с продаж в чеке';
  SCashRegister61 = 'Оборот по налогу В с покупок в чеке';
  SCashRegister62 = 'Оборот по налогу В с возврата продаж в чеке';
  SCashRegister63 = 'Оборот по налогу В с возврата покупок в чеке';
  SCashRegister64 = 'Оборот по налогу Г с продаж в чеке';
  SCashRegister65 = 'Оборот по налогу Г с покупок в чеке';
  SCashRegister66 = 'Оборот по налогу Г с возврата продаж в чеке';
  SCashRegister67 = 'Оборот по налогу Г с возврата покупок в чеке';
  SCashRegister68 = 'Накопления по налогу А с продаж в чеке';
  SCashRegister69 = 'Накопления по налогу А с покупок в чеке';
  SCashRegister6A = 'Накопления по налогу А с возврата продаж в чеке';
  SCashRegister6B = 'Накопления по налогу А с возврата покупок в чеке';
  SCashRegister6C = 'Накопления по налогу Б с продаж в чеке';
  SCashRegister6D = 'Накопления по налогу Б с покупок в чеке';
  SCashRegister6E = 'Накопления по налогу Б с возврата продаж в чеке';
  SCashRegister6F = 'Накопления по налогу Б с возврата покупок в чеке';
  SCashRegister70 = 'Накопления по налогу В с продаж в чеке';
  SCashRegister71 = 'Накопления по налогу В с покупок в чеке';
  SCashRegister72 = 'Накопления по налогу В с возврата продаж в чеке';
  SCashRegister73 = 'Накопления по налогу В с возврата покупок в чеке';
  SCashRegister74 = 'Накопления по налогу Г с продаж в чеке';
  SCashRegister75 = 'Накопления по налогу Г с покупок в чеке';
  SCashRegister76 = 'Накопления по налогу Г с возврата продаж в чеке';
  SCashRegister77 = 'Накопления по налогу Г с возврата покупок в чеке';
  SCashRegister78 = 'Наличность в кассе на момент закрытия чека';
  SCashRegister79 = 'Накопление продаж в 1 отдел в смене';
  SCashRegister7A = 'Накопление покупок в 1 отдел в смене';
  SCashRegister7B = 'Накопление возврата продаж в 1 отдел в смене';
  SCashRegister7C = 'Накопление возврата покупок в 1 отдел в смене';
  SCashRegister7D = 'Накопление продаж в 2 отдел в смене';
  SCashRegister7E = 'Накопление покупок в 2 отдел в смене';
  SCashRegister7F = 'Накопление возврата продаж в 2 отдел в смене';
  SCashRegister80 = 'Накопление возврата покупок в 2 отдел в смене';
  SCashRegister81 = 'Накопление продаж в 3 отдел в смене';
  SCashRegister82 = 'Накопление покупок в 3 отдел в смене';
  SCashRegister83 = 'Накопление возврата продаж в 3 отдел в смене';
  SCashRegister84 = 'Накопление возврата покупок в 3 отдел в смене';
  SCashRegister85 = 'Накопление продаж в 4 отдел в смене';
  SCashRegister86 = 'Накопление покупок в 4 отдел в смене';
  SCashRegister87 = 'Накопление возврата продаж в 4 отдел в смене';
  SCashRegister88 = 'Накопление возврата покупок в 4 отдел в смене';
  SCashRegister89 = 'Накопление продаж в 5 отдел в смене';
  SCashRegister8A = 'Накопление покупок в 5 отдел в смене';
  SCashRegister8B = 'Накопление возврата продаж в 5 отдел в смене';
  SCashRegister8C = 'Накопление возврата покупок в 5 отдел в смене';
  SCashRegister8D = 'Накопление продаж в 6 отдел в смене';
  SCashRegister8E = 'Накопление покупок в 6 отдел в смене';
  SCashRegister8F = 'Накопление возврата продаж в 6 отдел в смене';
  SCashRegister90 = 'Накопление возврата покупок в 6 отдел в смене';
  SCashRegister91 = 'Накопление продаж в 7 отдел в смене';
  SCashRegister92 = 'Накопление покупок в 7 отдел в смене';
  SCashRegister93 = 'Накопление возврата продаж в 7 отдел в смене';
  SCashRegister94 = 'Накопление возврата покупок в 7 отдел в смене';
  SCashRegister95 = 'Накопление продаж в 8 отдел в смене';
  SCashRegister96 = 'Накопление покупок в 8 отдел в смене';
  SCashRegister97 = 'Накопление возврата продаж в 8 отдел в смене';
  SCashRegister98 = 'Накопление возврата покупок в 8 отдел в смене';
  SCashRegister99 = 'Накопление продаж в 9 отдел в смене';
  SCashRegister9A = 'Накопление покупок в 9 отдел в смене';
  SCashRegister9B = 'Накопление возврата продаж в 9 отдел в смене';
  SCashRegister9C = 'Накопление возврата покупок в 9 отдел в смене';
  SCashRegister9D = 'Накопление продаж в 10 отдел в смене';
  SCashRegister9E = 'Накопление покупок в 10 отдел в смене';
  SCashRegister9F = 'Накопление возврата продаж в 10 отдел в смене';
  SCashRegisterA0 = 'Накопление возврата покупок в 10 отдел в смене';
  SCashRegisterA1 = 'Накопление продаж в 11 отдел в смене';
  SCashRegisterA2 = 'Накопление покупок в 11 отдел в смене';
  SCashRegisterA3 = 'Накопление возврата продаж в 11 отдел в смене';
  SCashRegisterA4 = 'Накопление возврата покупок в 11 отдел в смене';
  SCashRegisterA5 = 'Накопление продаж в 12 отдел в смене';
  SCashRegisterA6 = 'Накопление покупок в 12 отдел в смене';
  SCashRegisterA7 = 'Накопление возврата продаж в 12 отдел в смене';
  SCashRegisterA8 = 'Накопление возврата покупок в 12 отдел в смене';
  SCashRegisterA9 = 'Накопление продаж в 13 отдел в смене';
  SCashRegisterAA = 'Накопление покупок в 13 отдел в смене';
  SCashRegisterAB = 'Накопление возврата продаж в 13 отдел в смене';
  SCashRegisterAC = 'Накопление возврата покупок в 13 отдел в смене';
  SCashRegisterAD = 'Накопление продаж в 14 отдел в смене';
  SCashRegisterAE = 'Накопление покупок в 14 отдел в смене';
  SCashRegisterAF = 'Накопление возврата продаж в 14 отдел в смене';
  SCashRegisterB0 = 'Накопление возврата покупок в 14 отдел в смене';
  SCashRegisterB1 = 'Накопление продаж в 15 отдел в смене';
  SCashRegisterB2 = 'Накопление покупок в 15 отдел в смене';
  SCashRegisterB3 = 'Накопление возврата продаж в 15 отдел в смене';
  SCashRegisterB4 = 'Накопление возврата покупок в 15 отдел в смене';
  SCashRegisterB5 = 'Накопление продаж в 16 отдел в смене';
  SCashRegisterB6 = 'Накопление покупок в 16 отдел в смене';
  SCashRegisterB7 = 'Накопление возврата продаж в 16 отдел в смене';
  SCashRegisterB8 = 'Накопление возврата покупок в 16 отдел в смене';
  SCashRegisterB9 = 'Накопление скидок с продаж в смене';
  SCashRegisterBA = 'Накопление скидок с покупок в смене';
  SCashRegisterBB = 'Накопление скидок с возврата продаж в смене';
  SCashRegisterBC = 'Накопление скидок с возврата покупок в смене';
  SCashRegisterBD = 'Накопление надбавок на продажи в смене';
  SCashRegisterBE = 'Накопление надбавок на покупок в смене';
  SCashRegisterBF = 'Накопление надбавок на возврата продаж в смене';
  SCashRegisterC0 = 'Накопление надбавок на возврата покупок в смене';
  SCashRegisterC1 = 'Накопление оплат продаж наличными в смене';
  SCashRegisterC2 = 'Накопление оплат покупок наличными в смене';
  SCashRegisterC3 = 'Накопление оплат возврата продаж наличными в смене';
  SCashRegisterC4 = 'Накопление оплат возврата покупок наличными в смене';
  SCashRegisterC5 = 'Накопление оплат продаж видом оплаты 2 в смене';
  SCashRegisterC6 = 'Накопление оплат покупок видом оплаты 2 в смене';
  SCashRegisterC7 = 'Накопление оплат возврата продаж видом оплаты 2 в смене';
  SCashRegisterC8 = 'Накопление оплат возврата покупок видом оплаты 2 в смене';
  SCashRegisterC9 = 'Накопление оплат продаж видом оплаты 3 в смене';
  SCashRegisterCA = 'Накопление оплат покупок видом оплаты 3 в смене';
  SCashRegisterCB = 'Накопление оплат возврата продаж видом оплаты 3 в смене';
  SCashRegisterCC = 'Накопление оплат возврата покупок видом оплаты 3 в смене';
  SCashRegisterCD = 'Накопление оплат продаж видом оплаты 4 в смене';
  SCashRegisterCE = 'Накопление оплат покупок видом оплаты 4 в смене';
  SCashRegisterCF = 'Накопление оплат возврата продаж видом оплаты 4 в смене';
  SCashRegisterD0 = 'Накопление оплат возврата покупок видом оплаты 4 в смене';
  SCashRegisterD1 = 'Оборот по налогу А с продаж в смене';
  SCashRegisterD2 = 'Оборот по налогу А с покупок в смене';
  SCashRegisterD3 = 'Оборот по налогу А с возврата продаж в смене';
  SCashRegisterD4 = 'Оборот по налогу А с возврата покупок в смене';
  SCashRegisterD5 = 'Оборот по налогу Б с продаж в смене';
  SCashRegisterD6 = 'Оборот по налогу Б с покупок в смене';
  SCashRegisterD7 = 'Оборот по налогу Б с возврата продаж в смене';
  SCashRegisterD8 = 'Оборот по налогу Б с возврата покупок в смене';
  SCashRegisterD9 = 'Оборот по налогу В с продаж в смене';
  SCashRegisterDA = 'Оборот по налогу В с покупок в смене';
  SCashRegisterDB = 'Оборот по налогу В с возврата продаж в смене';
  SCashRegisterDC = 'Оборот по налогу В с возврата покупок в смене';
  SCashRegisterDD = 'Оборот по налогу Г с продаж в смене';
  SCashRegisterDE = 'Оборот по налогу Г с покупок в смене';
  SCashRegisterDF = 'Оборот по налогу Г с возврата продаж в смене';
  SCashRegisterE0 = 'Оборот по налогу Г с возврата покупок в смене';
  SCashRegisterE1 = 'Накопления по налогу А с продаж в смене';
  SCashRegisterE2 = 'Накопления по налогу А с покупок в смене';
  SCashRegisterE3 = 'Накопления по налогу А с возврата продаж в смене';
  SCashRegisterE4 = 'Накопления по налогу А с возврата покупок в смене';
  SCashRegisterE5 = 'Накопления по налогу Б с продаж в смене';
  SCashRegisterE6 = 'Накопления по налогу Б с покупок в смене';
  SCashRegisterE7 = 'Накопления по налогу Б с возврата продаж в смене';
  SCashRegisterE8 = 'Накопления по налогу Б с возврата покупок в смене';
  SCashRegisterE9 = 'Накопления по налогу В с продаж в смене';
  SCashRegisterEA = 'Накопления по налогу В с покупок в смене';
  SCashRegisterEB = 'Накопления по налогу В с возврата продаж в смене';
  SCashRegisterEC = 'Накопления по налогу В с возврата покупок в смене';
  SCashRegisterED = 'Накопления по налогу Г с продаж в смене';
  SCashRegisterEE = 'Накопления по налогу Г с покупок в смене';
  SCashRegisterEF = 'Накопления по налогу Г с возврата продаж в смене';
  SCashRegisterF0 = 'Накопления по налогу Г с возврата покупок в смене';
  SCashRegisterF1 = 'Накопление наличности в кассе';
  SCashRegisterF2 = 'Накопление внесений за смену';
  SCashRegisterF3 = 'Накопление выплат за смену';
  SCashRegisterF4 = 'Необнуляемая сумма до фискализации';
  SCashRegisterF5 = 'Сумма продаж в смене из ЭКЛЗ';
  SCashRegisterF6 = 'Сумма покупок в смене из ЭКЛЗ ';
  SCashRegisterF7 = 'Сумма возвратов продаж в смене из ЭКЛЗ';
  SCashRegisterF8 = 'Сумма возвратов покупок в смене из ЭКЛЗ';
  SCashRegisterF9 = 'Сумма аннулированных продаж в смене';
  SCashRegisterFA = 'Сумма аннулированных покупок в смене';
  SCashRegisterFB = 'Сумма аннулированных возвратов продаж в смене';
  SCashRegisterFC = 'Сумма аннулированных возвратов покупок в смене';
  SCashRegisterFF = 'Накопления по аннулированиям в смене';

  // Расширенные регистры

  SCashRegister4096 = 'Накопление оплат продаж видом оплаты 5 в чеке';
  SCashRegister4097 = 'Накопление оплат покупок видом оплаты 5 в чеке';
  SCashRegister4098 = 'Накопление оплат возврата продаж видом оплаты 5 в чеке';
  SCashRegister4099 = 'Накопление оплат возврата покупок видом оплаты 5 в чеке';
  SCashRegister4100 = 'Накопление оплат продаж видом оплаты 6 в чеке';
  SCashRegister4101 = 'Накопление оплат покупок видом оплаты 6 в чеке';
  SCashRegister4102 = 'Накопление оплат возврата продаж видом оплаты 6 в чеке';
  SCashRegister4103 = 'Накопление оплат возврата покупок видом оплаты 6 в чеке';
  SCashRegister4104 = 'Накопление оплат продаж видом оплаты 7 в чеке';
  SCashRegister4105 = 'Накопление оплат покупок видом оплаты 7 в чеке';
  SCashRegister4106 = 'Накопление оплат возврата продаж видом оплаты 7 в чеке';
  SCashRegister4107 = 'Накопление оплат возврата покупок видом оплаты 7 в чеке';
  SCashRegister4108 = 'Накопление оплат продаж видом оплаты 8 в чеке';
  SCashRegister4109 = 'Накопление оплат покупок видом оплаты 8 в чеке';
  SCashRegister4110 = 'Накопление оплат возврата продаж видом оплаты 8 в чеке';
  SCashRegister4111 = 'Накопление оплат возврата покупок видом оплаты 8 в чеке';
  SCashRegister4112 = 'Накопление оплат продаж видом оплаты 9 в чеке';
  SCashRegister4113 = 'Накопление оплат покупок видом оплаты 9 в чеке';
  SCashRegister4114 = 'Накопление оплат возврата продаж видом оплаты 9 в чеке';
  SCashRegister4115 = 'Накопление оплат возврата покупок видом оплаты 9 в чеке';
  SCashRegister4116 = 'Накопление оплат продаж видом оплаты 10 в чеке';
  SCashRegister4117 = 'Накопление оплат покупок видом оплаты 10 в чеке';
  SCashRegister4118 = 'Накопление оплат возврата продаж видом оплаты 10 в чеке';
  SCashRegister4119 = 'Накопление оплат возврата покупок видом оплаты 10 в чеке';
  SCashRegister4120 = 'Накопление оплат продаж видом оплаты 11 в чеке';
  SCashRegister4121 = 'Накопление оплат покупок видом оплаты 11 в чеке';
  SCashRegister4122 = 'Накопление оплат возврата продаж видом оплаты 11 в чеке';
  SCashRegister4123 = 'Накопление оплат возврата покупок видом оплаты 11 в чеке';
  SCashRegister4124 = 'Накопление оплат продаж видом оплаты 12 в чеке';
  SCashRegister4125 = 'Накопление оплат покупок видом оплаты 12 в чеке';
  SCashRegister4126 = 'Накопление оплат возврата продаж видом оплаты 12 в чеке';
  SCashRegister4127 = 'Накопление оплат возврата покупок видом оплаты 12 в чеке';
  SCashRegister4128 = 'Накопление оплат продаж видом оплаты 13 в чеке';
  SCashRegister4129 = 'Накопление оплат покупок видом оплаты 13 в чеке';
  SCashRegister4130 = 'Накопление оплат возврата продаж видом оплаты 13 в чеке';
  SCashRegister4131 = 'Накопление оплат возврата покупок видом оплаты 13 в чеке';
  SCashRegister4132 = 'Накопление оплат продаж видом оплаты 14 в чеке';
  SCashRegister4133 = 'Накопление оплат покупок видом оплаты 14 в чеке';
  SCashRegister4134 = 'Накопление оплат возврата продаж видом оплаты 14 в чеке';
  SCashRegister4135 = 'Накопление оплат возврата покупок видом оплаты 14 в чеке';
  SCashRegister4136 = 'Накопление оплат продаж видом оплаты 15 в чеке';
  SCashRegister4137 = 'Накопление оплат покупок видом оплаты 15 в чеке';
  SCashRegister4138 = 'Накопление оплат возврата продаж видом оплаты 15 в чеке';
  SCashRegister4139 = 'Накопление оплат возврата покупок видом оплаты 15 в чеке';
  SCashRegister4140 = 'Накопление оплат продаж видом оплаты 16 в чеке';
  SCashRegister4141 = 'Накопление оплат покупок видом оплаты 16 в чеке';
  SCashRegister4142 = 'Накопление оплат возврата продаж видом оплаты 16 в чеке';
  SCashRegister4143 = 'Накопление оплат возврата покупок видом оплаты 16 в чеке';

  SCashRegister4144 = 'Накопление оплат продаж видом оплаты 5 в смене';
  SCashRegister4145 = 'Накопление оплат покупок видом оплаты 5 в смене';
  SCashRegister4146 = 'Накопление оплат возврата продаж видом оплаты 5 в смене';
  SCashRegister4147 = 'Накопление оплат возврата покупок видом оплаты 5 в смене';
  SCashRegister4148 = 'Накопление оплат продаж видом оплаты 6 в смене';
  SCashRegister4149 = 'Накопление оплат покупок видом оплаты 6 в смене';
  SCashRegister4150 = 'Накопление оплат возврата продаж видом оплаты 6 в смене';
  SCashRegister4151 = 'Накопление оплат возврата покупок видом оплаты 6 в смене';
  SCashRegister4152 = 'Накопление оплат продаж видом оплаты 7 в смене';
  SCashRegister4153 = 'Накопление оплат покупок видом оплаты 7 в смене';
  SCashRegister4154 = 'Накопление оплат возврата продаж видом оплаты 7 в смене';
  SCashRegister4155 = 'Накопление оплат возврата покупок видом оплаты 7 в смене';
  SCashRegister4156 = 'Накопление оплат продаж видом оплаты 8 в смене';
  SCashRegister4157 = 'Накопление оплат покупок видом оплаты 8 в смене';
  SCashRegister4158 = 'Накопление оплат возврата продаж видом оплаты 8 в смене';
  SCashRegister4159 = 'Накопление оплат возврата покупок видом оплаты 8 в смене';
  SCashRegister4160 = 'Накопление оплат продаж видом оплаты 9 в смене';
  SCashRegister4161 = 'Накопление оплат покупок видом оплаты 9 в смене';
  SCashRegister4162 = 'Накопление оплат возврата продаж видом оплаты 9 в смене';
  SCashRegister4163 = 'Накопление оплат возврата покупок видом оплаты 9 в смене';
  SCashRegister4164 = 'Накопление оплат продаж видом оплаты 10 в смене';
  SCashRegister4165 = 'Накопление оплат покупок видом оплаты 10 в смене';
  SCashRegister4166 = 'Накопление оплат возврата продаж видом оплаты 10 в смене';
  SCashRegister4167 = 'Накопление оплат возврата покупок видом оплаты 10 в смене';
  SCashRegister4168 = 'Накопление оплат продаж видом оплаты 11 в смене';
  SCashRegister4169 = 'Накопление оплат покупок видом оплаты 11 в смене';
  SCashRegister4170 = 'Накопление оплат возврата продаж видом оплаты 11 в смене';
  SCashRegister4171 = 'Накопление оплат возврата покупок видом оплаты 11 в смене';
  SCashRegister4172 = 'Накопление оплат продаж видом оплаты 12 в смене';
  SCashRegister4173 = 'Накопление оплат покупок видом оплаты 12 в смене';
  SCashRegister4174 = 'Накопление оплат возврата продаж видом оплаты 12 в смене';
  SCashRegister4175 = 'Накопление оплат возврата покупок видом оплаты 12 в смене';
  SCashRegister4176 = 'Накопление оплат продаж видом оплаты 13 в смене';
  SCashRegister4177 = 'Накопление оплат покупок видом оплаты 13 в смене';
  SCashRegister4178 = 'Накопление оплат возврата продаж видом оплаты 13 в смене';
  SCashRegister4179 = 'Накопление оплат возврата покупок видом оплаты 13 в смене';
  SCashRegister4180 = 'Накопление оплат продаж видом оплаты 14 в смене';
  SCashRegister4181 = 'Накопление оплат покупок видом оплаты 14 в смене';
  SCashRegister4182 = 'Накопление оплат возврата продаж видом оплаты 14 в смене';
  SCashRegister4183 = 'Накопление оплат возврата покупок видом оплаты 14 в смене';
  SCashRegister4184 = 'Накопление оплат продаж видом оплаты 15 в смене';
  SCashRegister4185 = 'Накопление оплат покупок видом оплаты 15 в смене';
  SCashRegister4186 = 'Накопление оплат возврата продаж видом оплаты 15 в смене';
  SCashRegister4187 = 'Накопление оплат возврата покупок видом оплаты 15 в смене';
  SCashRegister4188 = 'Накопление оплат продаж видом оплаты 16 в смене';
  SCashRegister4189 = 'Накопление оплат покупок видом оплаты 16 в смене';
  SCashRegister4190 = 'Накопление оплат возврата продаж видом оплаты 16 в смене';
  SCashRegister4191 = 'Накопление оплат возврата покупок видом оплаты 16 в смене';

  SCashRegister4192 = 'Оборот по налогу 20/120 с продаж в чеке';
  SCashRegister4193 = 'Оборот по налогу 20/120 с покупок в чеке';
  SCashRegister4194 = 'Оборот по налогу 20/120 с возврата продаж в чеке';
  SCashRegister4195 = 'Оборот по налогу 20/120 с возврата покупок в чеке';

  SCashRegister4196 = 'Оборот по налогу 10/110 с продаж в чеке';
  SCashRegister4197 = 'Оборот по налогу 10/110 с покупок в чеке';
  SCashRegister4198 = 'Оборот по налогу 10/110 с возврата продаж в чеке';
  SCashRegister4199 = 'Оборот по налогу 10/110 с возврата покупок в чеке';

  SCashRegister4200 = 'Накопления по налогу 20/120 с продаж в чеке';
  SCashRegister4201 = 'Накопления по налогу 20/120 с покупок в чеке';
  SCashRegister4202 = 'Накопления по налогу 20/120 с возврата продаж в чеке';
  SCashRegister4203 = 'Накопления по налогу 20/120 с возврата покупок в чеке';

  SCashRegister4204 = 'Накопления по налогу 10/110 с продаж в чеке';
  SCashRegister4205 = 'Накопления по налогу 10/110 с покупок в чеке';
  SCashRegister4206 = 'Накопления по налогу 10/110 с возврата продаж в чеке';
  SCashRegister4207 = 'Накопления по налогу 10/110 с возврата покупок в чеке';

  SCashRegister4208 = 'Оборот по налогу 20/120 с продаж в смене';
  SCashRegister4209 = 'Оборот по налогу 20/120 с покупок в смене';
  SCashRegister4210 = 'Оборот по налогу 20/120 с возврата продаж в смене';
  SCashRegister4211 = 'Оборот по налогу 20/120 с возврата покупок в смене';

  SCashRegister4212 = 'Оборот по налогу 10/110 с продаж в смене';
  SCashRegister4213 = 'Оборот по налогу 10/110 с покупок в смене';
  SCashRegister4214 = 'Оборот по налогу 10/110 с возврата продаж в смене';
  SCashRegister4215 = 'Оборот по налогу 10/110 с возврата покупок в смене';

  SCashRegister4216 = 'Накопления по налогу 20/120 с продаж в смене';
  SCashRegister4217 = 'Накопления по налогу 20/120 с покупок в смене';
  SCashRegister4218 = 'Накопления по налогу 20/120 с возврата продаж в смене';
  SCashRegister4219 = 'Накопления по налогу 20/120 с возврата покупок в смене';

  SCashRegister4220 = 'Накопления по налогу 10/110 с продаж в смене';
  SCashRegister4221 = 'Накопления по налогу 10/110 с покупок в смене';
  SCashRegister4222 = 'Накопления по налогу 10/110 с возврата продаж в смене';
  SCashRegister4223 = 'Накопления по налогу 10/110 с возврата покупок в смене';

  SCashRegister4224 = 'Сумма чеков коррекции прихода';
  SCashRegister4225 = 'Сумма чеков коррекции расхода';
  SCashRegister4226 = 'Сумма чеков коррекции возврата прихода';
  SCashRegister4227 = 'Сумма чеков коррекции возврата расхода';

  // Счетчики ФН

  SCashRegister4228 = 'Итоги ФН: кол-во смен';
  SCashRegister4229 = 'Итоги ФН: Кол-во операций';
  SCashRegister4230 = 'Итоги ФН: Кол-во операций с признаком расчета «приход»';
  SCashRegister4231 = 'Итоги ФН: Итоговая сумма расчета, указанного в чеке (БСО) (приход)';
  SCashRegister4232 = 'Итоги ФН: Итоговая сумма по чекам (БСО) наличными (приход)';
  SCashRegister4233 = 'Итоги ФН: Итоговая сумма по чекам (БСО) электронными (приход)';
  SCashRegister4234 = 'Итоги ФН: Итоговая сумма по чеку (БСО) предоплатой (зачетом аванса) (приход)';
  SCashRegister4235 = 'Итоги ФН: Итоговая сумма по чеку (БСО) постоплатой (в кредит) (приход)';
  SCashRegister4236 = 'Итоги ФН: Итоговая сумма по чеку (БСО) встречным предоставлением (приход)';
  SCashRegister4237 = 'Итоги ФН: Итоговая сумма НДС чека по ставке 20% (приход)';
  SCashRegister4238 = 'Итоги ФН: Итоговая сумма НДС чека по ставке 10% (приход)';
  SCashRegister4239 = 'Итоги ФН: Итоговая сумма расчета по чеку с НДС по ставке 0% (приход)';
  SCashRegister4240 = 'Итоги ФН: Итоговая сумма расчета по чеку без НДС (приход)';
  SCashRegister4241 = 'Итоги ФН: Итоговая сумма НДС чека по расч. ставке 20/120 (приход)';
  SCashRegister4242 = 'Итоги ФН: Итоговая сумма НДС чека по расч. ставке 10/110 (приход)';
  SCashRegister4243 = 'Итоги ФН: Кол-во операций с признаком расчета «возврат прихода»';
  SCashRegister4244 = 'Итоги ФН: Итоговая сумма расчета, указанного в чеке (БСО) (возврат прихода)';
  SCashRegister4245 = 'Итоги ФН: Итоговая сумма по чекам (БСО) наличными (возврат прихода)';
  SCashRegister4246 = 'Итоги ФН: Итоговая сумма по чекам (БСО) электронными (возврат прихода)';
  SCashRegister4247 = 'Итоги ФН: Итоговая сумма по чеку (БСО) предоплатой (зачетом аванса) (возврат прихода)';
  SCashRegister4248 = 'Итоги ФН: Итоговая сумма по чеку (БСО) постоплатой (в кредит) (возврат прихода)';
  SCashRegister4249 = 'Итоги ФН: Итоговая сумма по чеку (БСО) встречным предоставлением (возврат прихода)';
  SCashRegister4250 = 'Итоги ФН: Итоговая сумма НДС чека по ставке 20% (возврат прихода)';
  SCashRegister4251 = 'Итоги ФН: Итоговая сумма НДС чека по ставке 10% (возврат прихода)';
  SCashRegister4252 = 'Итоги ФН: Итоговая сумма расчета по чеку с НДС по ставке 0% (возврат прихода)';
  SCashRegister4253 = 'Итоги ФН: Итоговая сумма расчета по чеку без НДС (возврат прихода)';
  SCashRegister4254 = 'Итоги ФН: Итоговая сумма НДС чека по расч. ставке 20/120 (возврат прихода)';
  SCashRegister4255 = 'Итоги ФН: Итоговая сумма НДС чека по расч. ставке 10/110 (возврат прихода)';
  SCashRegister4256 = 'Итоги ФН: Кол-во операций с признаком расчета «расход»';
  SCashRegister4257 = 'Итоги ФН: Итоговая сумма расчета, указанного в чеке (БСО) (расход)';
  SCashRegister4258 = 'Итоги ФН: Итоговая сумма по чекам (БСО) наличными (расход)';
  SCashRegister4259 = 'Итоги ФН: Итоговая сумма по чекам (БСО) электронными (расход)';
  SCashRegister4260 = 'Итоги ФН: Итоговая сумма по чеку (БСО) предоплатой (зачетом аванса) (расход)';
  SCashRegister4261 = 'Итоги ФН: Итоговая сумма по чеку (БСО) постоплатой (в кредит) (расход)';
  SCashRegister4262 = 'Итоги ФН: Итоговая сумма по чеку (БСО) встречным предоставлением (расход)';
  SCashRegister4263 = 'Итоги ФН: Итоговая сумма НДС чека по ставке 20% (расход)';
  SCashRegister4264 = 'Итоги ФН: Итоговая сумма НДС чека по ставке 10% (расход)';
  SCashRegister4265 = 'Итоги ФН: Итоговая сумма расчета по чеку с НДС по ставке 0% (расход)';
  SCashRegister4266 = 'Итоги ФН: Итоговая сумма расчета по чеку без НДС (расход)';
  SCashRegister4267 = 'Итоги ФН: Итоговая сумма НДС чека по расч. ставке 20/120 (расход)';
  SCashRegister4268 = 'Итоги ФН: Итоговая сумма НДС чека по расч. ставке 10/110 (расход)';
  SCashRegister4269 = 'Итоги ФН: Кол-во операций с признаком расчета «возврат расхода»';
  SCashRegister4270 = 'Итоги ФН: Итоговая сумма расчета, указанного в чеке (БСО) (возврат расхода)';
  SCashRegister4271 = 'Итоги ФН: Итоговая сумма по чекам (БСО) наличными (возврат расхода)';
  SCashRegister4272 = 'Итоги ФН: Итоговая сумма по чекам (БСО) электронными (возврат расхода)';
  SCashRegister4273 = 'Итоги ФН: Итоговая сумма по чеку (БСО) предоплатой (зачетом аванса) (возврат расхода)';
  SCashRegister4274 = 'Итоги ФН: Итоговая сумма по чеку (БСО) постоплатой (в кредит) (возврат расхода)';
  SCashRegister4275 = 'Итоги ФН: Итоговая сумма по чеку (БСО) встречным предоставлением (возврат расхода)';
  SCashRegister4276 = 'Итоги ФН: Итоговая сумма НДС чека по ставке 20% (возврат расхода)';
  SCashRegister4277 = 'Итоги ФН: Итоговая сумма НДС чека по ставке 10% (возврат расхода)';
  SCashRegister4278 = 'Итоги ФН: Итоговая сумма расчета по чеку с НДС по ставке 0% (возврат расхода)';
  SCashRegister4279 = 'Итоги ФН: Итоговая сумма расчета по чеку без НДС (возврат расхода)';
  SCashRegister4280 = 'Итоги ФН: Итоговая сумма НДС чека по расч. ставке 20/120 (возврат расхода)';
  SCashRegister4281 = 'Итоги ФН: Итоговая сумма НДС чека по расч. ставке 10/110 (возврат расхода)';
  SCashRegister4282 = 'Итоги ФН: Кол-во чеков коррекций со всеми признаками расчётов';
  SCashRegister4283 = 'Итоги ФН: Кол-во чеков коррекций с признаком расчета «приход»';
  SCashRegister4284 = 'Итоги ФН: Итоговая сумма по чекам коррекции с признаком расчета «приход»';
  SCashRegister4285 = 'Итоги ФН: Кол-во чеков коррекций с признаком расчета «возврат прихода»';
  SCashRegister4286 = 'Итоги ФН: Итоговая сумма по чекам коррекции с признаком расчета «возврат прихода»';
  SCashRegister4287 = 'Итоги ФН: Кол-во чеков коррекций с признаком расчета «расход»';
  SCashRegister4288 = 'Итоги ФН: Итоговая сумма по чекам коррекции с признаком расчета «расход»';
  SCashRegister4289 = 'Итоги ФН: Кол-во чеков коррекций с признаком расчета «возврат расхода»';
  SCashRegister4290 = 'Итоги ФН: Итоговая сумма по чекам коррекции с признаком расчета «возврат расхода»';
  SCashRegister4291 = 'Итоги смены ФН: номер смены';
  SCashRegister4292 = 'Итоги смены ФН: Кол-во операций';
  SCashRegister4293 = 'Итоги смены ФН: Кол-во операций с признаком расчета «приход»';
  SCashRegister4294 = 'Итоги смены ФН: Итоговая сумма расчета, указанного в чеке (БСО) (приход)';
  SCashRegister4295 = 'Итоги смены ФН: Итоговая сумма по чекам (БСО) наличными (приход)';
  SCashRegister4296 = 'Итоги смены ФН: Итоговая сумма по чекам (БСО) электронными (приход)';
  SCashRegister4297 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) предоплатой (зачетом аванса) (приход)';
  SCashRegister4298 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) постоплатой (в кредит) (приход)';
  SCashRegister4299 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) встречным предоставлением (приход)';
  SCashRegister4300 = 'Итоги смены ФН: Итоговая сумма НДС чека по ставке 20% (приход)';
  SCashRegister4301 = 'Итоги смены ФН: Итоговая сумма НДС чека по ставке 10% (приход)';
  SCashRegister4302 = 'Итоги смены ФН: Итоговая сумма расчета по чеку с НДС по ставке 0% (приход)';
  SCashRegister4303 = 'Итоги смены ФН: Итоговая сумма расчета по чеку без НДС (приход)';
  SCashRegister4304 = 'Итоги смены ФН: Итоговая сумма НДС чека по расч. ставке 20/120 (приход)';
  SCashRegister4305 = 'Итоги смены ФН: Итоговая сумма НДС чека по расч. ставке 10/110 (приход)';
  SCashRegister4306 = 'Итоги смены ФН: Кол-во операций с признаком расчета «возврат прихода»';
  SCashRegister4307 = 'Итоги смены ФН: Итоговая сумма расчета, указанного в чеке (БСО) (возврат прихода)';
  SCashRegister4308 = 'Итоги смены ФН: Итоговая сумма по чекам (БСО) наличными (возврат прихода)';
  SCashRegister4309 = 'Итоги смены ФН: Итоговая сумма по чекам (БСО) электронными (возврат прихода)';
  SCashRegister4310 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) предоплатой (зачетом аванса) (возврат прихода)';
  SCashRegister4311 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) постоплатой (в кредит) (возврат прихода)';
  SCashRegister4312 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) встречным предоставлением (возврат прихода)';
  SCashRegister4313 = 'Итоги смены ФН: Итоговая сумма НДС чека по ставке 20% (возврат прихода)';
  SCashRegister4314 = 'Итоги смены ФН: Итоговая сумма НДС чека по ставке 10% (возврат прихода)';
  SCashRegister4315 = 'Итоги смены ФН: Итоговая сумма расчета по чеку с НДС по ставке 0% (возврат прихода)';
  SCashRegister4316 = 'Итоги смены ФН: Итоговая сумма расчета по чеку без НДС (возврат прихода)';
  SCashRegister4317 = 'Итоги смены ФН: Итоговая сумма НДС чека по расч. ставке 20/120 (возврат прихода)';
  SCashRegister4318 = 'Итоги смены ФН: Итоговая сумма НДС чека по расч. ставке 10/110 (возврат прихода)';
  SCashRegister4319 = 'Итоги смены ФН: Кол-во операций с признаком расчета «расход»';
  SCashRegister4320 = 'Итоги смены ФН: Итоговая сумма расчета, указанного в чеке (БСО) (расход)';
  SCashRegister4321 = 'Итоги смены ФН: Итоговая сумма по чекам (БСО) наличными (расход)';
  SCashRegister4322 = 'Итоги смены ФН: Итоговая сумма по чекам (БСО) электронными (расход)';
  SCashRegister4323 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) предоплатой (зачетом аванса) (расход)';
  SCashRegister4324 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) постоплатой (в кредит) (расход)';
  SCashRegister4325 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) встречным предоставлением (расход)';
  SCashRegister4326 = 'Итоги смены ФН: Итоговая сумма НДС чека по ставке 20% (расход)';
  SCashRegister4327 = 'Итоги смены ФН: Итоговая сумма НДС чека по ставке 10% (расход)';
  SCashRegister4328 = 'Итоги смены ФН: Итоговая сумма расчета по чеку с НДС по ставке 0% (расход)';
  SCashRegister4329 = 'Итоги смены ФН: Итоговая сумма расчета по чеку без НДС (расход)';
  SCashRegister4330 = 'Итоги смены ФН: Итоговая сумма НДС чека по расч. ставке 20/120 (расход)';
  SCashRegister4331 = 'Итоги смены ФН: Итоговая сумма НДС чека по расч. ставке 10/110 (расход)';
  SCashRegister4332 = 'Итоги смены ФН: Кол-во операций признака расчета возврат расхода';
  SCashRegister4333 = 'Итоги смены ФН: Итоговая сумма расчета, указанного в чеке (БСО) (возврат расхода)';
  SCashRegister4334 = 'Итоги смены ФН: Итоговая сумма по чекам (БСО) наличными (возврат расхода)';
  SCashRegister4335 = 'Итоги смены ФН: Итоговая сумма по чекам (БСО) электронными (возврат расхода)';
  SCashRegister4336 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) предоплатой (зачетом аванса) (возврат расхода)';
  SCashRegister4337 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) постоплатой (в кредит) (возврат расхода)';
  SCashRegister4338 = 'Итоги смены ФН: Итоговая сумма по чеку (БСО) встречным предоставлением (возврат расхода)';
  SCashRegister4339 = 'Итоги смены ФН: Итоговая сумма НДС чека по ставке 20% (возврат расхода)';
  SCashRegister4340 = 'Итоги смены ФН: Итоговая сумма НДС чека по ставке 10% (возврат расхода)';
  SCashRegister4341 = 'Итоги смены ФН: Итоговая сумма расчета по чеку с НДС по ставке 0% (возврат расхода)';
  SCashRegister4342 = 'Итоги смены ФН: Итоговая сумма расчета по чеку без НДС (возврат расхода)';
  SCashRegister4343 = 'Итоги смены ФН: Итоговая сумма НДС чека по расч. ставке 20/120 (возврат расхода)';
  SCashRegister4344 = 'Итоги смены ФН: Итоговая сумма НДС чека по расч. ставке 10/110 (возврат расхода)';
  SCashRegister4345 = 'Итоги смены ФН: Кол-во чеков коррекций со всеми признаками расчётов';
  SCashRegister4346 = 'Итоги смены ФН: Кол-во чеков коррекций с признаком расчета «приход»';
  SCashRegister4347 = 'Итоги смены ФН: Итоговая сумма по чекам коррекции с признаком расчета «приход»';
  SCashRegister4348 = 'Итоги смены ФН: Кол-во чеков коррекций с признаком расчета «возврат прихода»';
  SCashRegister4349 = 'Итоги смены ФН: Итоговая сумма по чекам коррекции с признаком расчета «возврат прихода»';
  SCashRegister4350 = 'Итоги смены ФН: Кол-во чеков коррекций с признаком расчета «расход»';
  SCashRegister4351 = 'Итоги смены ФН: Итоговая сумма по чекам коррекции с признаком расчета «расход»';
  SCashRegister4352 = 'Итоги смены ФН: Кол-во чеков коррекций с признаком расчета «возврат расхода»';
  SCashRegister4353 = 'Итоги смены ФН: Итоговая сумма по чекам коррекции с признаком расчета «возврат расхода»';
  SCashRegister4354 = 'Непереданные чеки ФН: Кол-во непереданных чеков и чеков коррекции со всеми признаками расчётов';
  SCashRegister4355 = 'Непереданные чеки ФН: Кол-во чеков и чеков коррекции с признаком расчета «Приход»';
  SCashRegister4356 = 'Непереданные чеки ФН: Итоговая сумма по чекам и чекам коррекции с признаком расчета «Приход»';
  SCashRegister4357 = 'Непереданные чеки ФН: Кол-во чеков и чеков коррекции с признаком расчета «Возврат прихода»';
  SCashRegister4358 = 'Непереданные чеки ФН: Итоговая сумма по чекам и чекам коррекции с признаком расчета «Возврат прихода»';
  SCashRegister4359 = 'Непереданные чеки ФН: Кол-во чеков и чеков коррекции с признаком расчета «Расход»';
  SCashRegister4360 = 'Непереданные чеки ФН: Итоговая сумма по чекам и чекам коррекции с признаком расчета «Расход»';
  SCashRegister4361 = 'Непереданные чеки ФН: Кол-во чеков и чеков коррекции с признаком расчета «Возврат расхода»';
  SCashRegister4362 = 'Непереданные чеки ФН: Итоговая сумма по чекам и чекам коррекции с признаком расчета «Возврат расхода»';
  //////////////////////////////

  SCashRegister4363 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 1';
  SCashRegister4364 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 2';
  SCashRegister4365 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 3';
  SCashRegister4366 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 4';
  SCashRegister4367 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 5';
  SCashRegister4368 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 6';
  SCashRegister4369 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 7';
  SCashRegister4370 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 8';
  SCashRegister4371 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 9';
  SCashRegister4372 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 10';
  SCashRegister4373 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 11';
  SCashRegister4374 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 12';
  SCashRegister4375 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 13';
  SCashRegister4376 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 14';
  SCashRegister4377 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 15';
  SCashRegister4378 = 'Накопление по чекам коррекции прихода в смене по типу оплаты 16';

  SCashRegister4379 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 1';
  SCashRegister4380 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 2';
  SCashRegister4381 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 3';
  SCashRegister4382 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 4';
  SCashRegister4383 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 5';
  SCashRegister4384 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 6';
  SCashRegister4385 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 7';
  SCashRegister4386 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 8';
  SCashRegister4387 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 9';
  SCashRegister4388 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 10';
  SCashRegister4389 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 11';
  SCashRegister4390 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 12';
  SCashRegister4391 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 13';
  SCashRegister4392 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 14';
  SCashRegister4393 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 15';
  SCashRegister4394 = 'Накопление по чекам коррекции расхода в смене по типу оплаты 16';

  SCashRegister4395 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 1';
  SCashRegister4396 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 2';
  SCashRegister4397 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 3';
  SCashRegister4398 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 4';
  SCashRegister4399 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 5';
  SCashRegister4400 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 6';
  SCashRegister4401 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 7';
  SCashRegister4402 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 8';
  SCashRegister4403 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 9';
  SCashRegister4404 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 10';
  SCashRegister4405 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 11';
  SCashRegister4406 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 12';
  SCashRegister4407 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 13';
  SCashRegister4408 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 14';
  SCashRegister4409 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 15';
  SCashRegister4410 = 'Накопление по чекам коррекции возврата прихода в смене по типу оплаты 16';

  SCashRegister4411 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 1';
  SCashRegister4412 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 2';
  SCashRegister4413 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 3';
  SCashRegister4414 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 4';
  SCashRegister4415 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 5';
  SCashRegister4416 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 6';
  SCashRegister4417 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 7';
  SCashRegister4418 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 8';
  SCashRegister4419 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 9';
  SCashRegister4420 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 10';
  SCashRegister4421 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 11';
  SCashRegister4422 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 12';
  SCashRegister4423 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 13';
  SCashRegister4424 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 14';
  SCashRegister4425 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 15';
  SCashRegister4426 = 'Накопление по чекам коррекции возврата расхода в смене по типу оплаты 16';

  SCashRegister4427 = 'Оборот по налогу 5% с прихода в чеке';
  SCashRegister4428 = 'Оборот по налогу 5% с расхода в чеке';
  SCashRegister4429 = 'Оборот по налогу 5% с возврата прихода в чеке';
  SCashRegister4430 = 'Оборот по налогу 5% с возврата расхода в чеке';
  SCashRegister4431 = 'Оборот по налогу 7% с прихода в чеке';
  SCashRegister4432 = 'Оборот по налогу 7% с расхода в чеке';
  SCashRegister4433 = 'Оборот по налогу 7% с возврата прихода в чеке';
  SCashRegister4434 = 'Оборот по налогу 7% с возврата расхода в чеке';
  SCashRegister4435 = 'Оборот по налогу 5/105 с прихода в чеке';
  SCashRegister4436 = 'Оборот по налогу 5/105 с расхода в чеке';
  SCashRegister4437 = 'Оборот по налогу 5/105 с возврата прихода в чеке';
  SCashRegister4438 = 'Оборот по налогу 5/105 с возврата расхода в чеке';
  SCashRegister4439 = 'Оборот по налогу 7/107 с прихода в чеке';
  SCashRegister4440 = 'Оборот по налогу 7/107 с расхода в чеке';
  SCashRegister4441 = 'Оборот по налогу 7/107 с возврата прихода в чеке';
  SCashRegister4442 = 'Оборот по налогу 7/107 с возврата расхода в чеке';

  SCashRegister4443 = 'Накопления по налогу 5% с прихода в чеке';
  SCashRegister4444 = 'Накопления по налогу 5% с расхода в чеке';
  SCashRegister4445 = 'Накопления по налогу 5% с возврата прихода в чеке';
  SCashRegister4446 = 'Накопления по налогу 5% с возврата расхода в чеке';
  SCashRegister4447 = 'Накопления по налогу 7% с прихода в чеке';
  SCashRegister4448 = 'Накопления по налогу 7% с расхода в чеке';
  SCashRegister4449 = 'Накопления по налогу 7% с возврата прихода в чеке';
  SCashRegister4450 = 'Накопления по налогу 7% с возврата расхода в чеке';
  SCashRegister4451 = 'Накопления по налогу 5/105 с прихода в чеке';
  SCashRegister4452 = 'Накопления по налогу 5/105 с расхода в чеке';
  SCashRegister4453 = 'Накопления по налогу 5/105 с возврата прихода в чеке';
  SCashRegister4454 = 'Накопления по налогу 5/105 с возврата расхода в чеке';
  SCashRegister4455 = 'Накопления по налогу 7/107 с прихода в чеке';
  SCashRegister4456 = 'Накопления по налогу 7/107 с расхода в чеке';
  SCashRegister4457 = 'Накопления по налогу 7/107 с возврата прихода в чеке';
  SCashRegister4458 = 'Накопления по налогу 7/107 с возврата расхода в чеке';

  SCashRegister4459 = 'Оборот по налогу 5% с прихода в смене';
  SCashRegister4460 = 'Оборот по налогу 5% с расхода в смене';
  SCashRegister4461 = 'Оборот по налогу 5% с возврата прихода в смене';
  SCashRegister4462 = 'Оборот по налогу 5% с возврата расхода в смене';
  SCashRegister4463 = 'Оборот по налогу 7% с прихода в смене';
  SCashRegister4464 = 'Оборот по налогу 7% с расхода в смене';
  SCashRegister4465 = 'Оборот по налогу 7% с возврата прихода в смене';
  SCashRegister4466 = 'Оборот по налогу 7% с возврата расхода в смене';
  SCashRegister4467 = 'Оборот по налогу 5/105 с прихода в смене';
  SCashRegister4468 = 'Оборот по налогу 5/105 с расхода в смене';
  SCashRegister4469 = 'Оборот по налогу 5/105 с возврата прихода в смене';
  SCashRegister4470 = 'Оборот по налогу 5/105 с возврата расхода в смене';
  SCashRegister4471 = 'Оборот по налогу 7/107 с прихода в смене';
  SCashRegister4472 = 'Оборот по налогу 7/107 с расхода в смене';
  SCashRegister4473 = 'Оборот по налогу 7/107 с возврата прихода в смене';
  SCashRegister4474 = 'Оборот по налогу 7/107 с возврата расхода в смене';

  SCashRegister4475 = 'Накопления по налогу 5% с прихода в смене';
  SCashRegister4476 = 'Накопления по налогу 5% с расхода в смене';
  SCashRegister4477 = 'Накопления по налогу 5% с возврата прихода в смене';
  SCashRegister4478 = 'Накопления по налогу 5% с возврата расхода в смене';
  SCashRegister4479 = 'Накопления по налогу 7% с прихода в смене';
  SCashRegister4480 = 'Накопления по налогу 7% с расхода в смене';
  SCashRegister4481 = 'Накопления по налогу 7% с возврата прихода в смене';
  SCashRegister4482 = 'Накопления по налогу 7% с возврата расхода в смене';
  SCashRegister4483 = 'Накопления по налогу 5/105 с прихода в смене';
  SCashRegister4484 = 'Накопления по налогу 5/105 с расхода в смене';
  SCashRegister4485 = 'Накопления по налогу 5/105 с возврата прихода в смене';
  SCashRegister4486 = 'Накопления по налогу 5/105 с возврата расхода в смене';
  SCashRegister4487 = 'Накопления по налогу 7/107 с прихода в смене';
  SCashRegister4488 = 'Накопления по налогу 7/107 с расхода в смене';
  SCashRegister4489 = 'Накопления по налогу 7/107 с возврата прихода в смене';
  SCashRegister4490 = 'Накопления по налогу 7/107 с возврата расхода в смене';

  SCashRegister4491 = 'Оборот по налогу 22% с прихода в чеке';
  SCashRegister4492 = 'Оборот по налогу 22% с расхода в чеке';
  SCashRegister4493 = 'Оборот по налогу 22% с возврата прихода в чеке';
  SCashRegister4494 = 'Оборот по налогу 22% с возврата расхода в чеке';
  SCashRegister4495 = 'Оборот по налогу 22/122 с прихода в чеке';
  SCashRegister4496 = 'Оборот по налогу 22/122 с расхода в чеке';
  SCashRegister4497 = 'Оборот по налогу 22/122 с возврата прихода в чеке';
  SCashRegister4498 = 'Оборот по налогу 22/122 с возврата расхода в чеке';

  SCashRegister4499 = 'Накопления по налогу 22% с прихода в чеке';
  SCashRegister4500 = 'Накопления по налогу 22% с расхода в чеке';
  SCashRegister4501 = 'Накопления по налогу 22% с возврата прихода в чеке';
  SCashRegister4502 = 'Накопления по налогу 22% с возврата расхода в чеке';
  SCashRegister4503 = 'Накопления по налогу 22/122 с прихода в чеке';
  SCashRegister4504 = 'Накопления по налогу 22/122 с расхода в чеке';
  SCashRegister4505 = 'Накопления по налогу 22/122 с возврата прихода в чеке';
  SCashRegister4506 = 'Накопления по налогу 22/122 с возврата расхода в чеке';

  SCashRegister4507 = 'Оборот по налогу 22% с прихода в смене';
  SCashRegister4508 = 'Оборот по налогу 22% с расхода в смене';
  SCashRegister4509 = 'Оборот по налогу 22% с возврата прихода в смене';
  SCashRegister4510 = 'Оборот по налогу 22% с возврата расхода в смене';
  SCashRegister4511 = 'Оборот по налогу 22/122 с прихода в смене';
  SCashRegister4512 = 'Оборот по налогу 22/122 с расхода в смене';
  SCashRegister4513 = 'Оборот по налогу 22/122 с возврата прихода в смене';
  SCashRegister4514 = 'Оборот по налогу 22/122 с возврата расхода в смене';

  SCashRegister4515 = 'Накопления по налогу 22% с прихода в смене';
  SCashRegister4516 = 'Накопления по налогу 22% с расхода в смене';
  SCashRegister4517 = 'Накопления по налогу 22% с возврата прихода в смене';
  SCashRegister4518 = 'Накопления по налогу 22% с возврата расхода в смене';
  SCashRegister4519 = 'Накопления по налогу 22/122 с прихода в смене';
  SCashRegister4520 = 'Накопления по налогу 22/122 с расхода в смене';
  SCashRegister4521 = 'Накопления по налогу 22/122 с возврата прихода в смене';
  SCashRegister4522 = 'Накопления по налогу 22/122 с возврата расхода в смене';



  /////////////////////////////////////////////////////////////////////////////
  // Operating registers

  SOperatingRegister00 = 'Количество продаж в 1 отдел в чеке';
  SOperatingRegister01 = 'Количество покупок в 1 отдел в чеке';
  SOperatingRegister02 = 'Количество возврата продаж в 1 отдел в чеке';
  SOperatingRegister03 = 'Количество возврата покупок в 1 отдел в чеке';
  SOperatingRegister04 = 'Количество продаж в 2 отдел в чеке';
  SOperatingRegister05 = 'Количество покупок в 2 отдел в чеке';
  SOperatingRegister06 = 'Количество возврата продаж в 2 отдел в чеке';
  SOperatingRegister07 = 'Количество возврата покупок в 2 отдел в чеке';
  SOperatingRegister08 = 'Количество продаж в 3 отдел в чеке';
  SOperatingRegister09 = 'Количество покупок в 3 отдел в чеке';
  SOperatingRegister0A = 'Количество возврата продаж в 3 отдел в чеке';
  SOperatingRegister0B = 'Количество возврата покупок в 3 отдел в чеке';
  SOperatingRegister0C = 'Количество продаж в 4 отдел в чеке';
  SOperatingRegister0D = 'Количество покупок в 4 отдел в чеке';
  SOperatingRegister0E = 'Количество возврата продаж в 4 отдел в чеке';
  SOperatingRegister0F = 'Количество возврата покупок в 4 отдел в чеке';
  SOperatingRegister10 = 'Количество продаж в 5 отдел в чеке';
  SOperatingRegister11 = 'Количество покупок в 5 отдел в чеке';
  SOperatingRegister12 = 'Количество возврата продаж в 5 отдел в чеке';
  SOperatingRegister13 = 'Количество возврата покупок в 5 отдел в чеке';
  SOperatingRegister14 = 'Количество продаж в 6 отдел в чеке';
  SOperatingRegister15 = 'Количество покупок в 6 отдел в чеке';
  SOperatingRegister16 = 'Количество возврата продаж в 6 отдел в чеке';
  SOperatingRegister17 = 'Количество возврата покупок в 6 отдел в чеке';
  SOperatingRegister18 = 'Количество продаж в 7 отдел в чеке';
  SOperatingRegister19 = 'Количество покупок в 7 отдел в чеке';
  SOperatingRegister1A = 'Количество возврата продаж в 7 отдел в чеке';
  SOperatingRegister1B = 'Количество возврата покупок в 7 отдел в чеке';
  SOperatingRegister1C = 'Количество продаж в 8 отдел в чеке';
  SOperatingRegister1D = 'Количество покупок в 8 отдел в чеке';
  SOperatingRegister1E = 'Количество возврата продаж в 8 отдел в чеке';
  SOperatingRegister1F = 'Количество возврата покупок в 8 отдел в чеке';
  SOperatingRegister20 = 'Количество продаж в 9 отдел в чеке';
  SOperatingRegister21 = 'Количество покупок в 9 отдел в чеке';
  SOperatingRegister22 = 'Количество возврата продаж в 9 отдел в чеке';
  SOperatingRegister23 = 'Количество возврата покупок в 9 отдел в чеке';
  SOperatingRegister24 = 'Количество продаж в 10 отдел в чеке';
  SOperatingRegister25 = 'Количество покупок в 10 отдел в чеке';
  SOperatingRegister26 = 'Количество возврата продаж в 10 отдел в чеке';
  SOperatingRegister27 = 'Количество возврата покупок в 10 отдел в чеке';
  SOperatingRegister28 = 'Количество продаж в 11 отдел в чеке';
  SOperatingRegister29 = 'Количество покупок в 11 отдел в чеке';
  SOperatingRegister2A = 'Количество возврата продаж в 11 отдел в чеке';
  SOperatingRegister2B = 'Количество возврата покупок в 11 отдел в чеке';
  SOperatingRegister2C = 'Количество продаж в 12 отдел в чеке';
  SOperatingRegister2D = 'Количество покупок в 12 отдел в чеке';
  SOperatingRegister2E = 'Количество возврата продаж в 12 отдел в чеке';
  SOperatingRegister2F = 'Количество возврата покупок в 12 отдел в чеке';
  SOperatingRegister30 = 'Количество продаж в 13 отдел в чеке';
  SOperatingRegister31 = 'Количество покупок в 13 отдел в чеке';
  SOperatingRegister32 = 'Количество возврата продаж в 13 отдел в чеке';
  SOperatingRegister33 = 'Количество возврата покупок в 13 отдел в чеке';
  SOperatingRegister34 = 'Количество продаж в 14 отдел в чеке';
  SOperatingRegister35 = 'Количество покупок в 14 отдел в чеке';
  SOperatingRegister36 = 'Количество возврата продаж в 14 отдел в чеке';
  SOperatingRegister37 = 'Количество возврата покупок в 14 отдел в чеке';
  SOperatingRegister38 = 'Количество продаж в 15 отдел в чеке';
  SOperatingRegister39 = 'Количество покупок в 15 отдел в чеке';
  SOperatingRegister3A = 'Количество возврата продаж в 15 отдел в чеке';
  SOperatingRegister3B = 'Количество возврата покупок в 15 отдел в чеке';
  SOperatingRegister3C = 'Количество продаж в 16 отдел в чеке';
  SOperatingRegister3D = 'Количество покупок в 16 отдел в чеке';
  SOperatingRegister3E = 'Количество возврата продаж в 16 отдел в чеке';
  SOperatingRegister3F = 'Количество возврата покупок в 16 отдел в чеке';
  SOperatingRegister40 = 'Количество скидок с продаж в чеке';
  SOperatingRegister41 = 'Количество скидок с покупок в чеке';
  SOperatingRegister42 = 'Количество скидок с возврата продаж в чеке';
  SOperatingRegister43 = 'Количество скидок с возврата покупок в чеке';
  SOperatingRegister44 = 'Количество надбавок на продажи в чеке';
  SOperatingRegister45 = 'Количество надбавок на покупок в чеке';
  SOperatingRegister46 = 'Количество надбавок на возврата продаж в чеке';
  SOperatingRegister47 = 'Количество надбавок на возврата покупок в чеке';
  SOperatingRegister48 = 'Количество продаж в 1 отдел в смене';
  SOperatingRegister49 = 'Количество покупок в 1 отдел в смене';
  SOperatingRegister4A = 'Количество возврата продаж в 1 отдел в смене';
  SOperatingRegister4B = 'Количество возврата покупок в 1 отдел в смене';
  SOperatingRegister4C = 'Количество продаж в 2 отдел в смене';
  SOperatingRegister4D = 'Количество покупок в 2 отдел в смене';
  SOperatingRegister4E = 'Количество возврата продаж в 2 отдел в смене';
  SOperatingRegister4F = 'Количество возврата покупок в 2 отдел в смене';
  SOperatingRegister50 = 'Количество продаж в 3 отдел в смене';
  SOperatingRegister51 = 'Количество покупок в 3 отдел в смене';
  SOperatingRegister52 = 'Количество возврата продаж в 3 отдел в смене';
  SOperatingRegister53 = 'Количество возврата покупок в 3 отдел в смене';
  SOperatingRegister54 = 'Количество продаж в 4 отдел в смене';
  SOperatingRegister55 = 'Количество покупок в 4 отдел в смене';
  SOperatingRegister56 = 'Количество возврата продаж в 4 отдел в смене';
  SOperatingRegister57 = 'Количество возврата покупок в 4 отдел в смене';
  SOperatingRegister58 = 'Количество продаж в 5 отдел в смене';
  SOperatingRegister59 = 'Количество покупок в 5 отдел в смене';
  SOperatingRegister5A = 'Количество возврата продаж в 5 отдел в смене';
  SOperatingRegister5B = 'Количество возврата покупок в 5 отдел в смене';
  SOperatingRegister5C = 'Количество продаж в 6 отдел в смене';
  SOperatingRegister5D = 'Количество покупок в 6 отдел в смене';
  SOperatingRegister5E = 'Количество возврата продаж в 6 отдел в смене';
  SOperatingRegister5F = 'Количество возврата покупок в 6 отдел в смене';
  SOperatingRegister60 = 'Количество продаж в 7 отдел в смене';
  SOperatingRegister61 = 'Количество покупок в 7 отдел в смене';
  SOperatingRegister62 = 'Количество возврата продаж в 7 отдел в смене';
  SOperatingRegister63 = 'Количество возврата покупок в 7 отдел в смене';
  SOperatingRegister64 = 'Количество продаж в 8 отдел в смене';
  SOperatingRegister65 = 'Количество покупок в 8 отдел в смене';
  SOperatingRegister66 = 'Количество возврата продаж в 8 отдел в смене';
  SOperatingRegister67 = 'Количество возврата покупок в 8 отдел в смене';
  SOperatingRegister68 = 'Количество продаж в 9 отдел в смене';
  SOperatingRegister69 = 'Количество покупок в 9 отдел в смене';
  SOperatingRegister6A = 'Количество возврата продаж в 9 отдел в смене';
  SOperatingRegister6B = 'Количество возврата покупок в 9 отдел в смене';
  SOperatingRegister6C = 'Количество продаж в 10 отдел в смене';
  SOperatingRegister6D = 'Количество покупок в 10 отдел в смене';
  SOperatingRegister6E = 'Количество возврата продаж в 10 отдел в смене';
  SOperatingRegister6F = 'Количество возврата покупок в 10 отдел в смене';
  SOperatingRegister70 = 'Количество продаж в 11 отдел в смене';
  SOperatingRegister71 = 'Количество покупок в 11 отдел в смене';
  SOperatingRegister72 = 'Количество возврата продаж в 11 отдел в смене';
  SOperatingRegister73 = 'Количество возврата покупок в 11 отдел в смене';
  SOperatingRegister74 = 'Количество продаж в 12 отдел в смене';
  SOperatingRegister75 = 'Количество покупок в 12 отдел в смене';
  SOperatingRegister76 = 'Количество возврата продаж в 12 отдел в смене';
  SOperatingRegister77 = 'Количество возврата покупок в 12 отдел в смене';
  SOperatingRegister78 = 'Количество продаж в 13 отдел в смене';
  SOperatingRegister79 = 'Количество покупок в 13 отдел в смене';
  SOperatingRegister7A = 'Количество возврата продаж в 13 отдел в смене';
  SOperatingRegister7B = 'Количество возврата покупок в 13 отдел в смене';
  SOperatingRegister7C = 'Количество продаж в 14 отдел в смене';
  SOperatingRegister7D = 'Количество покупок в 14 отдел в смене';
  SOperatingRegister7E = 'Количество возврата продаж в 14 отдел в смене';
  SOperatingRegister7F = 'Количество возврата покупок в 14 отдел в смене';
  SOperatingRegister80 = 'Количество продаж в 15 отдел в смене';
  SOperatingRegister81 = 'Количество покупок в 15 отдел в смене';
  SOperatingRegister82 = 'Количество возврата продаж в 15 отдел в смене';
  SOperatingRegister83 = 'Количество возврата покупок в 15 отдел в смене';
  SOperatingRegister84 = 'Количество продаж в 16 отдел в смене';
  SOperatingRegister85 = 'Количество покупок в 16 отдел в смене';
  SOperatingRegister86 = 'Количество возврата продаж в 16 отдел в смене';
  SOperatingRegister87 = 'Количество возврата покупок в 16 отдел в смене';
  SOperatingRegister88 = 'Количество скидок с продаж в смене';
  SOperatingRegister89 = 'Количество скидок с покупок в смене';
  SOperatingRegister8A = 'Количество скидок с возврата продаж в смене';
  SOperatingRegister8B = 'Количество скидок с возврата покупок в смене';
  SOperatingRegister8C = 'Количество надбавок на продажи в смене';
  SOperatingRegister8D = 'Количество надбавок на покупки в смене';
  SOperatingRegister8E = 'Количество надбавок на возвраты продаж в смене';
  SOperatingRegister8F = 'Количество надбавок на возвраты покупок в смене';
  SOperatingRegister90 = 'Количество чеков продаж в смене';
  SOperatingRegister91 = 'Количество чеков покупок в смене';
  SOperatingRegister92 = 'Количество чеков возврата продаж в смене';
  SOperatingRegister93 = 'Количество чеков возврата покупок в смене';
  SOperatingRegister94 = 'Номер чека продаж';
  SOperatingRegister95 = 'Номер чека покупок';
  SOperatingRegister96 = 'Номер чека возврата продаж';
  SOperatingRegister97 = 'Номер чека возврата покупок';
  SOperatingRegister98 = 'Сквозной номер документа';
  SOperatingRegister99 = 'Количество внесений денежных сумм за смену';
  SOperatingRegister9A = 'Количество выплат денежных сумм за смену';
  SOperatingRegister9B = 'Номер внесения денежных сумм';
  SOperatingRegister9C = 'Номер выплаты денежных сумм';
  SOperatingRegister9D = 'Количество отмененных документов за смену';
  SOperatingRegister9E = 'Номер сменного отчета без гашения';
  SOperatingRegister9F = 'Номер сменного отчета с гашением до фискализации';
  SOperatingRegisterA0 = 'Номер общего гашения';
  SOperatingRegisterA1 = 'Номер полного фискального отчета';
  SOperatingRegisterA2 = 'Номер сокращенного фискального отчета';
  SOperatingRegisterA3 = 'Номер тестового прогона';
  SOperatingRegisterA4 = 'Номер снятия показаний операционных регистров';
  SOperatingRegisterA5 = 'Номер отчетов по секциям';
  SOperatingRegisterA6 = 'Количество аннулирований';
  SOperatingRegisterA7 = 'Количество запусков теста самодиагностики';
  SOperatingRegisterA8 = 'Количество активизаций ЭКЛЗ';
  SOperatingRegisterA9 = 'Количество отчетов по итогам активизации ЭКЛЗ';
  SOperatingRegisterAA = 'Количество отчетов по  номеру КПК  из ЭКЛЗ';
  SOperatingRegisterAB = 'Количество отчетов по контрольной ленте из ЭКЛЗ';
  SOperatingRegisterAC = 'Количество отчетов по  датам из ЭКЛЗ';
  SOperatingRegisterAD = 'Количество отчетов по  сменам из ЭКЛЗ';
  SOperatingRegisterAE = 'Количество отчетов по  итогам смен из ЭКЛЗ';
  SOperatingRegisterAF = 'Количество отчетов по  датам в отделе из ЭКЛЗ';
  SOperatingRegisterB0 = 'Количество отчетов по  сменам в отделе из ЭКЛЗ';
  SOperatingRegisterB1 = 'Количество закрытий архивов ЭКЛЗ';
  SOperatingRegisterB2 = 'Номер отчета по налогам';
  SOperatingRegisterB3 = 'Количество аннулированных чеков продаж';
  SOperatingRegisterB4 = 'Количество аннулированных чеков покупок';
  SOperatingRegisterB5 = 'Количество аннулированных чеков возврата продаж';
  SOperatingRegisterB6 = 'Количество аннулированных чеков возврата покупок';
  SOperatingRegisterB7 = 'Количество нефискальных документов в день';
  SOperatingRegisterB8 = 'Количество нефискальных документов';
  SOperatingRegisterB9 = 'Сквозной номер документа';
  SOperatingRegisterBA = 'Сквозной номер документа (старшее слово)';
  SOperatingRegisterBB = 'Количество стационарных проверок ПО ФП';
  SOperatingRegisterBC = 'Номер отчетов по кассирам';
  SOperatingRegisterBD = 'Номер отчетов почасовых';
  SOperatingRegisterBE = 'Номер отчетов по товарам';
  SOperatingRegisterC1 = 'Количество нефискальных документов за смену';
  SOperatingRegisterC2 = 'Количество нефискальных документов';
  SOperatingRegisterC3 = 'Номер первого платежного документа в суточном (сменном) отчёте';
  SOperatingRegisterC4 = 'Номер первого платежного документа в суточном (сменном) отчёте (старшее слово)';
  SOperatingRegisterC5 = 'Количество аннулирований за смену';
  SOperatingRegisterC6 = 'Количество аннулирований за смену (старшее слово)';

  SOperatingRegisterB8FN = 'Количество отчетов в буфере отчетов';
  SOperatingRegisterB9FN = 'Сквозной номер документа (младшее слово)';
  SOperatingRegisterBAFN = 'Сквозной номер документа (старшее слово)';
  SUnusedRegister        = 'Не используется';
  SOperatingRegisterC4FN = 'Количество шагов мотора (младшее слово)';
  SOperatingRegisterC5FN = 'Количество шагов мотора (старшее слово)';
  SOperatingRegisterC6FN = 'Количество отрезов (младшее слово)';
  SOperatingRegisterC7FN = 'Количество отрезов (старшее слово)';

  SOperatingRegisterC8FN = 'Общее количество чеков коррекции прихода';
  SOperatingRegisterC9FN = 'Общее количество чеков коррекции расхода';
  SOperatingRegisterCAFN = 'Количество чеков коррекции прихода за смену';
  SOperatingRegisterCBFN = 'Количество чеков коррекции расхода за смену';


  {
  B8 - количество отчетов в буфере отчетов
  сквозной номер документа B9-BA
сквозной номер отчетов по кассирам BB
оBC-C3 не использ
оC4-C5 количество шагов мотора
оC6-C7 количество отрезов
оC8 - общее количество чеков коррекции прихода
оC9 - общее количество чеков коррекции расхода
оCA -  количество за смену чеков  коррекции прихода
оCB -  количество за смену чеков коррекции расхода
оFF не используется}


  SOperatingRegisterFF = 'Накопления по аннулированиям в смене';

  /////////////////////////////////////////////////////////////////////////////
  // Error description

  SErrorDescription00 = 'Ошибок нет';
  SErrorDescription01 = 'Неисправен накопитель ФП 1, ФП 2 или часы';
  SErrorDescription02 = 'Отсутствует ФП 1';
  SErrorDescription03 = 'Отсутствует ФП 2';
  SErrorDescription04 = 'Некорректные параметры в команде обращения к ФП';
  SErrorDescription05 = 'Нет запрошенных данных';
  SErrorDescription06 = 'ФП в режиме вывода данных';
  SErrorDescription07 = 'Некорректные параметры в команде для данной реализации ФП';
  SErrorDescription08 = 'Команда не поддерживается в данной реализации ФП';
  SErrorDescription09 = 'Некорректная длина команды';
  SErrorDescription0A = 'Формат данных не BCD';
  SErrorDescription0B = 'Неисправна ячейка памяти ФП при записи итога';
  SErrorDescription11 = 'Не введена лицензия';
  SErrorDescription12 = 'Заводской номер уже введен';
  SErrorDescription13 = 'Текущая дата меньше даты последней записи в ФП';
  SErrorDescription14 = 'Область сменных итогов ФП переполнена';
  SErrorDescription15 = 'Смена уже открыта';
  SErrorDescription16 = 'Смена не открыта';
  SErrorDescription17 = 'Номер первой смены больше номера последней смены';
  SErrorDescription18 = 'Дата первой смены больше даты последней смены';
  SErrorDescription19 = 'Нет данных в ФП';
  SErrorDescription1A = 'Область перерегистраций в ФП переполнена';
  SErrorDescription1B = 'Заводской номер не введен';
  SErrorDescription1C = 'В заданном диапазоне есть поврежденная запись';
  SErrorDescription1D = 'Повреждена последняя запись сменных итогов';
  SErrorDescription1E = 'Область перерегистраций ФП переполнена';
  SErrorDescription1F = 'Отсутствует память регистров';
  SErrorDescription20 = 'Переполнение денежного регистра при добавлении';
  SErrorDescription21 = 'Вычитаемая сумма больше содержимого денежного регистра';
  SErrorDescription22 = 'Неверная дата';
  SErrorDescription23 = 'Нет записи активизации';
  SErrorDescription24 = 'Область активизаций переполнена';
  SErrorDescription25 = 'Нет активизации с запрашиваемым номером';
  SErrorDescription26 = 'В ФП больше 3 поврежденных записей';
  SErrorDescription27 = 'Повреждение контрольных сумм ФП';
  SErrorDescription28 = 'Переполнение ФП по количество перезапусков ФР';
  SErrorDescription29 = 'Несанкционированная замена ФП';
  SErrorDescription2F = 'ЭКЛЗ не отвечает';
  SErrorDescription30 = 'ЭКЛЗ ответила NAK';
  SErrorDescription31 = 'ЭКЛЗ: ошибка формата';
  SErrorDescription32 = 'ЭКЛЗ: ошибка контрольной суммы';
  SErrorDescription33 = 'Некорректные параметры в команде';
  SErrorDescription34 = 'Нет данных';
  SErrorDescription35 = 'Некорректный параметр при данных настройках';
  SErrorDescription36 = 'Некорректные параметры в команде для данной реализации';
  SErrorDescription37 = 'Команда не поддерживается в данной реализации';
  SErrorDescription38 = 'Ошибка в ПЗУ';
  SErrorDescription39 = 'Внутренняя ошибка ПО';
  SErrorDescription3A = 'Переполнение накопления по надбавкам в смене';
  SErrorDescription3B = 'Переполнение накопления в смене';
  SErrorDescription3C = 'ЭКЛЗ: Неверный регистрационный номер';
  SErrorDescription3D = 'Смена не открыта - операция невозможна';
  SErrorDescription3E = 'Переполнение накопления по секциям в смене';
  SErrorDescription3F = 'Переполнение накопления по скидкам в смене';
  SErrorDescription40 = 'Переполнение диапазона скидок';
  SErrorDescription41 = 'Переполнение диапазона оплаты наличными';
  SErrorDescription42 = 'Переполнение диапазона оплаты типом 2';
  SErrorDescription43 = 'Переполнение диапазона оплаты типом 3';
  SErrorDescription44 = 'Переполнение диапазона оплаты типом 4';
  SErrorDescription45 = 'Cумма всех типов оплаты меньше итога чека';
  SErrorDescription46 = 'Не хватает наличности в кассе';
  SErrorDescription47 = 'Переполнение накопления по налогам в смене';
  SErrorDescription48 = 'Переполнение итога чека';
  SErrorDescription49 = 'Операция невозможна в открытом чеке данного типа';
  SErrorDescription4A = 'Открыт чек - операция невозможна';
  SErrorDescription4B = 'Буфер чека переполнен';
  SErrorDescription4C = 'Переполнение накопления по обороту налогов в смене';
  SErrorDescription4D = 'Вносимая безналичной оплатой сумма больше суммы чека';
  SErrorDescription4E = 'Смена превысила 24 часа';
  SErrorDescription4F = 'Неверный пароль';
  SErrorDescription50 = 'Идет печать предыдущей команды';
  SErrorDescription51 = 'переполнение накоплений наличными в смене';
  SErrorDescription52 = 'переполнение накоплений по типу оплаты 2 в смене';
  SErrorDescription53 = 'переполнение накоплений по типу оплаты 3 в смене';
  SErrorDescription54 = 'переполнение накоплений по типу оплаты 4 в смене';
  SErrorDescription55 = 'Чек закрыт - операция невозможна';
  SErrorDescription56 = 'Нет документа для повтора';
  SErrorDescription57 = 'ЭКЛЗ: Количество закрытых смен не совпадает с ФП';
  SErrorDescription58 = 'Ожидание команды продолжения печати';
  SErrorDescription59 = 'Документ открыт другим оператором';
  SErrorDescription5A = 'Скидка превышает накопления в чеке';
  SErrorDescription5B = 'Переполнение диапазона надбавок';
  SErrorDescription5C = 'Понижено напряжение 24В';
  SErrorDescription5D = 'Таблица не определена';
  SErrorDescription5E = 'Некорректная операция';
  SErrorDescription5F = 'Отрицательный итог чека';
  SErrorDescription60 = 'Переполнение при умножении';
  SErrorDescription61 = 'Переполнение диапазона цены';
  SErrorDescription62 = 'Переполнение диапазона количества';
  SErrorDescription63 = 'Переполнение диапазона отдела';
  SErrorDescription64 = 'ФП отсутствует';
  SErrorDescription65 = 'Не хватает денег в секции';
  SErrorDescription66 = 'Переполнение денег в секции';
  SErrorDescription67 = 'Ошибка связи с ФП';
  SErrorDescription68 = 'Не хватает денег по обороту налогов';
  SErrorDescription69 = 'Переполнение денег по обороту налогов';
  SErrorDescription6A = 'Ошибка питания в момент ответа по I2C';
  SErrorDescription6B = 'Нет чековой ленты';
  SErrorDescription6C = 'Нет контрольной ленты';
  SErrorDescription6D = 'Не хватает денег по налогу';
  SErrorDescription6E = 'Переполнение денег по налогу';
  SErrorDescription6F = 'Переполнение по выплате в смене';
  SErrorDescription70 = 'Переполнение ФП';
  SErrorDescription71 = 'Ошибка отрезчика';
  SErrorDescription72 = 'Команда не поддерживается в данном подрежиме';
  SErrorDescription73 = 'Команда не поддерживается в данном режиме';
  SErrorDescription74 = 'Ошибка ОЗУ';
  SErrorDescription75 = 'Ошибка питания';
  SErrorDescription76 = 'Ошибка принтера: нет импульсов с тахогенератора';
  SErrorDescription77 = 'Ошибка принтера: нет сигнала с датчиков';
  SErrorDescription78 = 'Замена ПО';
  SErrorDescription79 = 'Замена ФП';
  SErrorDescription7A = 'Поле не редактируется';
  SErrorDescription7B = 'Ошибка оборудования';
  SErrorDescription7C = 'Не совпадает дата';
  SErrorDescription7D = 'Неверный формат даты';
  SErrorDescription7E = 'Неверное значение в поле длины';
  SErrorDescription7F = 'Переполнение диапазона итога';
  SErrorDescription80 = 'Ошибка связи с ФП';
  SErrorDescription81 = 'Ошибка связи с ФП';
  SErrorDescription82 = 'Ошибка связи с ФП';
  SErrorDescription83 = 'Ошибка связи с ФП';
  SErrorDescription84 = 'Переполнение наличности';
  SErrorDescription85 = 'Переполнение по продажам в смене';
  SErrorDescription86 = 'Переполнение по покупкам в смене';
  SErrorDescription87 = 'Переполнение по возвратам продаж в смене';
  SErrorDescription88 = 'Переполнение по возвратам покупок в смене';
  SErrorDescription89 = 'Переполнение по внесению в смене';
  SErrorDescription8A = 'Переполнение по надбавкам в чеке';
  SErrorDescription8B = 'Переполнение по скидкам в чеке';
  SErrorDescription8C = 'Отрицательный итог надбавки в чеке';
  SErrorDescription8D = 'Отрицательный итог скидки в чеке';
  SErrorDescription8E = 'Нулевой итог чека';
  SErrorDescription8F = 'Касса не фискализирована';
  SErrorDescription90 = 'Поле превышает размер установленный в настройках';
  SErrorDescription91 = 'Выход за границу поля печати при данных настройках шрифта';
  SErrorDescription92 = 'Наложение полей';
  SErrorDescription93 = 'Восстановление ОЗУ прошло успешно';
  SErrorDescription94 = 'Исчерпан лимит операций в чеке';
  SErrorDescription95 = 'Неизвестная ошибка ЭКЛЗ';
  SErrorDescriptionA0 = 'Ошибка связи с ЭКЛЗ';
  SErrorDescriptionA1 = 'ЭКЛЗ отсутствует';
  SErrorDescriptionA2 = 'ЭКЛЗ: Некорректный формат или параметр команды';
  SErrorDescriptionA3 = 'Некорректное состояние ЭКЛЗ';
  SErrorDescriptionA4 = 'Авария ЭКЛЗ';
  SErrorDescriptionA5 = 'Авария КС в составе ЭКЛЗ';
  SErrorDescriptionA6 = 'Исчерпан временной ресурс ЭКЛЗ';
  SErrorDescriptionA7 = 'ЭКЛЗ переполнена';
  SErrorDescriptionA8 = 'ЭКЛЗ: Неверные дата или время';
  SErrorDescriptionA9 = 'ЭКЛЗ: Нет запрошенных данных';
  SErrorDescriptionAA = 'Переполнение ЭКЛЗ (отрицательный итог документа)';
  SErrorDescriptionAB = 'Превышено количество попыток выполнения подготовки активизации';
  SErrorDescriptionAC = 'Неверный код разрешения активизации';
  SErrorDescriptionAD = 'Некорректно указан заводской номер ККМ';
  SErrorDescriptionAE = 'Некорректно указан ИНН';
  SErrorDescriptionAF = 'Некорректно указан номер последней смены';
  SErrorDescriptionB0 = 'ЭКЛЗ: Переполнение в параметре количество';
  SErrorDescriptionB1 = 'ЭКЛЗ: Переполнение в параметре сумма';
  SErrorDescriptionB2 = 'ЭКЛЗ: Уже активизирована';
  SErrorDescriptionB3 = 'Некорректно указаны дата и время';
  SErrorDescriptionC0 = 'Контроль даты и времени (подтвердите дату и время)';
  SErrorDescriptionC1 = 'ЭКЛЗ: суточный отчет с гашением прервать нельзя';
  SErrorDescriptionC2 = 'Превышение напряжения блока питания';
  SErrorDescriptionC3 = 'Несовпадение итогов чека с ЭКЛЗ';
  SErrorDescriptionC4 = 'Несовпадение номеров смен';
  SErrorDescriptionC5 = 'Буфер подкладного документа пуст';
  SErrorDescriptionC6 = 'Подкладной документ отсутствует';
  SErrorDescriptionC7 = 'Поле не редактируется в данном режиме';
  SErrorDescriptionC8 = 'Ошибка связи с принтером';
  SErrorDescriptionC9 = 'Перегрев печатающей головки';
  SerrorDescriptionCA = 'Температура вне условий эксплуатации';
  SerrorDescriptionCB = 'Переполнение длинного сквозного номера';
  SErrorDescriptionD0 = 'Не распечатана контрольная лента по смене из ЭКЛЗ';
  SErrorDescriptionD1 = 'Нет данных в буфере';
  SErrorDescriptionD2 = 'Неверная денежная сумма при данных настройках округления';
  SErrorDescriptionD3 = 'Код товара не распознан';
  SErrorDescriptionD4 = 'Код маркировки фальсифицирован';
  SErrorDescriptionD5 = 'Ошибка авторизации';
  SErrorDescriptionE0 = 'Ошибка связи с купюроприемником';
  SErrorDescriptionE1 = 'Купюроприемник занят';
  SErrorDescriptionE2 = 'Итог чека не соответствует итогу купюроприемника';
  SErrorDescriptionE3 = 'Ошибка купюроприемника';
  SErrorDescriptionE4 = 'Итог купюроприемника не нулевой';

  SErrorDescriptionF0 = 'Ошибка передачи в ФП';
  SErrorDescriptionF1 = 'Ошибка приема от ФП';
  SErrorDescriptionF2 = 'Истек таймаут приема';
  SErrorDescriptionF3 = 'Переполнение буфера';
  SErrorDescriptionF4 = 'Нет запрошенных строк';
  SErrorDescriptionF5 = 'Переполнение кадра ответа';


  { =====================================================
    Ошибки ФН
  02h	Другое состояние ФН|	Данная команда требует другого состояния ФН или должны быть выполнены условия выполнения команды
  03h	Отказ ФН|	Запросить расширенные сведения о причинах отказа. Перезапустить ФН.
  04h	Отказ КС|	Запросить расширенные сведения о причинах отказа. Перезапустить ФН.
  05h	Параметры команды не соответствуют сроку жизни ФН|	Необходимо поменять параметры команды
  07h	Некорректная дата и/или время|	Дата и время операции не соответствуют установленным требованиям
  08h	Нет запрошенных данных|	Запрошенные данные отсутствуют в Архиве ФН
  09h	Некорректное значение параметров команды|	Параметры команды имеют правильный формат, но их значение не верно
  0Ah	Некорректная команда. Код ответа формируется только в случае, если ФН активизирован в режиме поддержки ФФД-1.1)|
    	В данном режиме функционирования ФН команда не разрешена
  0Bh	Неразрешенные реквизиты. (Код ответа формируется только в случае, если ФН активизирован в режиме поддержки ФФД-1.1)|
      Во входящем сообщении ККТ с кодом команды 07h ККТ передает в ФН данные, которые должен формировать ФН. Номер некорректного реквизита, переданного ККТ в ФН, передается ФН в ККТ в данных ответа (Uint16, LE)
  0Ch	Дублирование данных	|ККТ передает данные, которые уже были переданы в составе данного документа. Номер дублируемого реквизита передаётся в данных ответа (Uint16, LE)
  0Dh	Отсутствуют данные, необходимые для корректного учета в ФН |	Для корректного учета и хранения фискальных данных, требуется передача недостающих данных в ФН
      Для команд завершения чеков может содержать доп. байт ответа, указывающий на недостающие суммы по разным видам оплаты
  0Eh	Количество позиций в документе, превысило допустимый предел|	ФН передает в ККТ этот код ответа, если максимальное число позиций, превышает допустимые пределы
  10h	Превышение размеров TLV данных|	Размер передаваемых данных, имеющих TLV структуру, превысил допустимый
  11h	Нет транспортного соединения|	Транспортное соединение (ТС) отсутствует. Необходимо установить ТС с ОФД и передать в ФН команду "Транспортное соединение с ОФД"
  12h	Исчерпан ресурс ФН|	Требуется закрытие ФН
  14h	Ограничение ресурса ФН|	Может быть выдан в нескольких случаях, в сочетании с флагами предупреждений.
      Если стоит флаг "Превышено время ожидания ответа ОФД", то это значит, что время нахождения в очереди самого старого сообщения на выдачу более 30 календарных дней. Только ККТ в режиме передачи данных. Необходимо передать сообщения в ОФД.
      Если стоит флаг "Архив ФН заполнен на 90 %", то это означат, что Архив ФН полностью заполнен - необходимо закрыть ФН.
      Если флаги предупреждений отсутствуют, то это означает, что ресурс 30 дневного хранения для документов для ОФД исчерпан.
  16h	Продолжительность смены превышена|	Продолжительность смены превысила 24 часа. Требуется закрыть смену.
  17h	Некорректные данные о промежутке времени между фискальными документами|	Данные о промежутке времени между формируемым и предыдущим фискальным документом, указанные ККТ, более чем на 5 минут превышают данные об этом промежутке времени, определенными по таймеру ФН
  18h Некорректный реквизит, переданный ККТ в ФН |	Реквизит, переданный ККТ в ФН, не соответствует установленным требованиям.
      Номер некорректного реквизита передаётся в данных ответа (Uint16, LE)
  19h Некорректный реквизит с признаком продажи подакцизного товара|	Фискальный документ, переданный в ФН, содержит признак "продажа подакцизного товара", отчет о регистрации или текущий отчет об изменении параметров регистрации, хранящийся в ФН, не содержит признак "продажа подакцизного товара"
  20h	Сообщение ОФД не может быть принято|	Сообщение ОФД не может быть принято, расширенные данные ответа указывают причину отказа в принятии сообщения
  }




  {
 02h	Другое состояние ФН
03h	Отказ ФН
04h	Отказ КС
05h	Параметры команды не соответствуют сроку жизни ФН
07h	Некорректная дата и/или время
08h	Нет запрошенных данных
09h	Некорректное значение параметров команды
0Ah	Некорректная команда.
0Bh	Неразрешенные реквизиты.
0Ch	Дублирование данных
0Dh	Отсутствуют данные, необходимые для корректного учета в ФН
0Eh	Количество позиций в документе, превысило допустимый предел
10h	Превышение размеров TLV данных
11h	Нет транспортного соединения
12h	Исчерпан ресурс ФН
14h	Ограничение ресурса ФН
16h	Продолжительность смены превышена
17h	Некорректные данные о промежутке времени между фискальными документами
18h Некорректный реквизит, переданный ККТ в ФН
19h Реквизит не соответствует установкам при регистрации
20h	Ошибка при обработке ответа в ФН
23h	Ошибка сервиса обновления ключей проверки кодов маркировки
24h	Неизвестный ответ сервиса обновления ключей проверки

32h + 6E = A0	Запрещена работа с маркированным товарами
33h	A1 Неверная последовательность команд группы Bxh
34h	A2 Работа с маркированными товарами временно заблокирована
35h	A3 Переполнена таблица хранения КМ
3Ch	AA В блоке TLV отсутствуют необходимые реквизиты
3Eh	AC В реквизите 2007 содержится КМ, который ранее не проверялся в ФН

}





  SErrorDescription01FN = 'Неизвестная команда, неверный формат посылки или неизвестные параметры';
  SErrorDescription02FN = 'Другое состояние ФН';
  SErrorDescription03FN = 'Отказ ФН';
  SErrorDescription04FN = 'Отказ КС';
  SErrorDescription05FN = 'Параметры команды не соответствуют сроку жизни ФН';
  SErrorDescription06FN = 'Архив ФН переполнен';
  SErrorDescription07FN = 'Неверные дата и/или время';
  SErrorDescription08FN = 'Нет запрошенных данных';
  SErrorDescription09FN = 'Некорректное значение параметров команды';
  SErrorDescription0AFN = 'Некорректная команда';
  SErrorDescription0BFN = 'Неразрешенные реквизиты';
  SErrorDescription0CFN = 'Дублирование данных';
  SErrorDescription0DFN = 'Отсутствуют данные, необходимые для корректного учета в ФН';
  SErrorDescription0EFN = 'Количество позиций в документе превысило допустимый предел';
  SErrorDescription10FN = 'Превышение размеров TLV данных';
  SErrorDescription11FN = 'Нет транспортного соединения';
  SErrorDescription12FN = 'Исчерпан ресурс ФН';
  SErrorDescription14FN = 'Ограничение ресурса ФН';
  SErrorDescription15FN = 'Исчерпан ресурс Ожидания передачи сообщения';
  SErrorDescription16FN = 'Продолжительность смены более 24 часов';
  SErrorDescription17FN = 'Некорректные данные о промежутке времени между фискальными документами';
  SErrorDescription18FN = 'Некорректный реквизит, переданный ККТ в ФН';
  SErrorDescription19FN = 'Реквизит не соответствует установкам при регистрации';
  SErrorDescription20FN = 'Ошибка при обработке ответа в ФН';
  SErrorDescription21FN = 'Нет связи с ФН. Фатальная ошибка !!!';
  SErrorDescription30FN = 'ФН не отвечает';
  SErrorDescription77FN = 'Ошибка лицензии';
  SErrorDescription79FN = 'Ошибка часов';

  SErrorDescription23FN =	'Ошибка сервиса обновления ключей проверки кодов маркировки';
  SErrorDescription24FN =	'Неизвестный ответ сервиса обновления ключей проверки';
  SErrorDescriptionA0FN = 'Запрещена работа с маркированным товарами';
  SErrorDescriptionA1FN = 'Неверная последовательность команд группы Bxh';
  SErrorDescriptionA2FN = 'Работа с маркированными товарами временно заблокирована';
  SErrorDescriptionA3FN = 'Переполнена таблица хранения КМ';
  SErrorDescriptionAAFN = 'В блоке TLV отсутствуют необходимые реквизиты';
  SErrorDescriptionACFN = 'В реквизите 2007 содержится КМ, который ранее не проверялся в ФН';


  SErrorDescriptionUnknown = 'Неизвестный код ошибки';

  /////////////////////////////////////////////////////////////////////////////
  // Advanced mode

  SAdvancedMode0 = 'бумага есть';
  SAdvancedMode1 = 'Пассивное отсутствие бумаги';
  SAdvancedMode2 = 'Активное отсутствие бумаги';
  SAdvancedMode3 = 'После активного отсутствия бумаги';
  SAdvancedMode4 = 'Фаза печати операции длинных отчетов';
  SAdvancedMode5 = 'Фаза печати операции';
  SAdvancedModeUnknown = 'Неизвестный подрежим устройства (%d)';

  /////////////////////////////////////////////////////////////////////////////
  // Device code

  SDeviceCode1 = 'Накопитель ФП1';
  SDeviceCode2 = 'Накопитель ФП2';
  SDeviceCode3 = 'Часы';
  SDeviceCode4 = 'Энергонезависимая память';
  SDeviceCode5 = 'Процессор ФП';
  SDeviceCode6 = 'Память программ';
  SDeviceCode7 = 'Оперативная память';
  SDeviceCodeUnknown = 'Неизвестный код устройства (%d)';

  /////////////////////////////////////////////////////////////////////////////
  // Device mode

  SDeviceMode01 = 'Выдача данных';
  SDeviceMode02 = 'Открытая смена; 24 часа не кончились';
  SDeviceMode03 = 'Открытая смена; 24 часа кончились';
  SDeviceMode04 = 'Закрытая смена';
  SDeviceMode05 = 'Блокировка по неправильному паролю налогового инспектора';
  SDeviceMode06 = 'Ожидание подтверждения ввода даты';
  SDeviceMode07 = 'Разрешение изменения положения десятичной точки';
  SDeviceMode08 = 'Открытый документ: продажа';
  SDeviceMode09 = 'Режим разрешения тех. обнуления';
  SDeviceMode0A = 'Тестовый прогон';
  SDeviceMode0B = 'Печать полного отчета';
  SDeviceMode0C = 'Печать отчета ЭКЛЗ';
  SDeviceMode0D = 'Открыт ПД продажи';
  SDeviceMode0E = 'Ожидание загрузки ПД';
  SDeviceMode0F = 'ПД сформирован';
  SDeviceMode18 = 'Открытый документ: покупка';
  SDeviceMode1D = 'Открыт ПД покупки';
  SDeviceMode1E = 'Загрузка и позиционирование ПД';
  SDeviceMode28 = 'Открытый документ: возврат продажи';
  SDeviceMode2D = 'Открыт ПД возврата продажи';
  SDeviceMode2E = 'Позиционирование ПД';
  SDeviceMode38 = 'Открытый документ: возврат покупки';
  SDeviceMode48 = 'Открытый документ: нефискальный';
  SDeviceMode3D = 'Открыт ПД возврата покупки';
  SDeviceMode3E = 'Печать ПД';
  SDeviceMode4C = 'Печать ПД закончена';
  SDeviceMode4E = 'Печать закончена';
  SDeviceMode5E = 'Выброс ПД';
  SDeviceMode6E = 'Ожидание извлечения ПД';
  SDeviceModeUnknown = 'Неизвестный режим: %d';
  SCorrectionReceipt = '. (Чек коррекции)';

  /////////////////////////////////////////////////////////////////////////////
  // Command name

  SCommandName01 = 'Запрос дампа';
  SCommandName02 = 'Запрос данных';
  SCommandName03 = 'Прерывание выдачи данных';
  SCommandName0D = 'Фискализация с длинным РНМ';
  SCommandName0E = 'Ввод длинного заводского номера';
  SCommandName0F = 'Запрос длинного заводского номера';
  SCommandName10 = 'Короткий запрос состояния';
  SCommandName11 = 'Запрос состояния';
  SCommandName12 = 'Печать жирной строки';
  SCommandName13 = 'Гудок';
  SCommandName14 = 'Установка параметров обмена';
  SCommandName15 = 'Чтение параметров обмена';
  SCommandName16 = 'Технологическое обнуление';
  SCommandName17 = 'Печать строки';
  SCommandName18 = 'Печать заголовка документа';
  SCommandName19 = 'Тестовый прогон';
  SCommandName1A = 'Запрос денежного регистра';
  SCommandName1B = 'Запрос операционного регистра';
  SCommandName1C = 'Запись лицензии';
  SCommandName1D = 'Чтение лицензии';
  SCommandName1E = 'Запись таблицы';
  SCommandName1F = 'Чтение таблицы';
  SCommandName20 = 'Запись положения десятичной точки';
  SCommandName21 = 'Программирование времени';
  SCommandName22 = 'Программирование даты';
  SCommandName23 = 'Подтверждение программирования даты';
  SCommandName24 = 'Инициализация таблиц';
  SCommandName25 = 'Отрезка чека';
  SCommandName26 = 'Прочитать параметры шрифта';
  SCommandName27 = 'Общее гашение';
  SCommandName28 = 'Открыть денежный ящик';
  SCommandName29 = 'Протяжка';
  SCommandName2A = 'Выброс подкладного документа';
  SCommandName2B = 'Прерывание тестового прогона';
  SCommandName2C = 'Снятие показаний операционных регистров';
  SCommandName2D = 'Запрос структуры таблицы';
  SCommandName2E = 'Запрос структуры поля';
  SCommandName2F = 'Печать строки данным шрифтом';
  SCommandName40 = 'Суточный отчет без гашения';
  SCommandName41 = 'Суточный отчет с гашением';
  SCommandName42 = 'Отчёт по секциям';
  SCommandName43 = 'Отчёт по налогам';
  SCommandName50 = 'Внесение';
  SCommandName51 = 'Выплата';
  SCommandName57 = 'Закрытие чека (Беларусь)';
  SCommandName60 = 'Ввод заводского номера';
  SCommandName61 = 'Инициализация ФП';
  SCommandName62 = 'Запрос суммы записей в ФП';
  SCommandName63 = 'Запрос даты последней записи в ФП';
  SCommandName64 = 'Запрос диапазона дат и смен';
  SCommandName65 = 'Фискализация';
  SCommandName66 = 'Фискальный отчет по диапазону дат';
  SCommandName67 = 'Фискальный отчет по диапазону смен';
  SCommandName68 = 'Прерывание полного отчета';
  SCommandName69 = 'Чтение параметров фискализации';
  SCommandName6B = 'Запрос описания ошибки';
  SCommandName70 = 'Открыть фискальный подкладной документ';
  SCommandName71 = 'Открыть стандартный фискальный подкладной документ';
  SCommandName72 = 'Формирование операции на подкладном документе';
  SCommandName73 = 'Формирование стандартной операции на подкладном документе';
  SCommandName74 = 'Формирование скидки на подкладном документе';
  SCommandName75 = 'Формирование стандартной скидки на подкладном документе';
  SCommandName76 = 'Формирование закрытия чека на подкладном документе';
  SCommandName77 = 'Формирование стандартного закрытия чека на подкладном документе';
  SCommandName78 = 'Конфигурация подкладного документа';
  SCommandName79 = 'Установка стандартной конфигурации подкладного документа';
  SCommandName7A = 'Заполнение буфера подкладного документа нефискальной информацией';
  SCommandName7B = 'Очистка строки буфера подкладного документа от нефискальной информации';
  SCommandName7C = 'Очистка всего буфера подкладного документа от нефискальной информации';
  SCommandName7D = 'Печать подкладного документа';
  SCommandName7E = 'Общая конфигурация подкладного документа';
  SCommandName80 = 'Продажа';
  SCommandName81 = 'Покупка';
  SCommandName82 = 'Возврат продажи';
  SCommandName83 = 'Возврат покупки';
  SCommandName84 = 'Сторно';
  SCommandName85 = 'Закрытие чека';
  SCommandName86 = 'Скидка';
  SCommandName87 = 'Надбавка';
  SCommandName88 = 'Аннулирование чека';
  SCommandName89 = 'Подытог чека';
  SCommandName8A = 'Сторно скидки';
  SCommandName8B = 'Сторно надбавки';
  SCommandName8C = 'Повтор документа';
  SCommandName8D = 'Открыть чек';
  SCommandName90 = 'Формирование чека отпуска нефтепродуктов в режиме предоплаты заданной дозы';
  SCommandName91 = 'Формирование чека отпуска нефтепродуктов в режиме предоплаты на заданную сумму';
  SCommandName92 = 'Формирование чека коррекции при неполном отпуске нефтепродуктов';
  SCommandName93 = 'Задание дозы РК в миллилитрах';
  SCommandName94 = 'Задание дозы РК в денежных единицах';
  SCommandName95 = 'Продажа нефтепродуктов';
  SCommandName96 = 'Останов РК';
  SCommandName97 = 'Пуск РК';
  SCommandName98 = 'Сброс РК';
  SCommandName99 = 'Сброс всех ТРК';
  SCommandName9A = 'Задание параметров РК';
  SCommandName9B = 'Считать литровый суммарный счетчик';
  SCommandName9E = 'Запрос текущей дозы РК';
  SCommandName9F = 'Запрос состояния РК';
  SCommandNameA0 = 'Отчет ЭКЛЗ по отделам в заданном диапазоне дат';
  SCommandNameA1 = 'Отчет ЭКЛЗ по отделам в заданном диапазоне номеров смен';
  SCommandNameA2 = 'Отчет ЭКЛЗ по закрытиям смен в заданном диапазоне дат';
  SCommandNameA3 = 'Отчет ЭКЛЗ по закрытиям смен в заданном диапазоне номеров смен';
  SCommandNameA4 = 'Итоги смены по номеру смены ЭКЛЗ';
  SCommandNameA5 = 'Платежный документ из ЭКЛЗ по номеру КПК';
  SCommandNameA6 = 'Контрольная лента из ЭКЛЗ по номеру смены';
  SCommandNameA7 = 'Прерывание полного отчета ЭКЛЗ';
  SCommandNameA8 = 'Итог активизации ЭКЛЗ';
  SCommandNameA9 = 'Активизация ЭКЛЗ';
  SCommandNameAA = 'Закрытие архива ЭКЛЗ';
  SCommandNameAB = 'Запрос регистрационного номера ЭКЛЗ';
  SCommandNameAC = 'Прекращение ЭКЛЗ';
  SCommandNameAD = 'Запрос состояния ЭКЛЗ по коду 1';
  SCommandNameAE = 'Запрос состояния ЭКЛЗ по коду 2';
  SCommandNameAF = 'Тест целостности архива ЭКЛЗ';
  SCommandNameB0 = 'Продолжение печати';
  SCommandNameB1 = 'Запрос версии ЭКЛЗ';
  SCommandNameB2 = 'Инициализация архива ЭКЛЗ';
  SCommandNameB3 = 'Запрос данных отчёта ЭКЛЗ';
  SCommandNameB4 = 'Запрос контрольной ленты ЭКЛЗ';
  SCommandNameB5 = 'Запрос документа ЭКЛЗ';
  SCommandNameB6 = 'Запрос отчёта ЭКЛЗ по отделам в заданном диапазоне дат';
  SCommandNameB7 = 'Запрос отчёта ЭКЛЗ по отделам в заданном диапазоне номеров смен';
  SCommandNameB8 = 'Запрос отчёта ЭКЛЗ по закрытиям смен в заданном диапазоне дат';
  SCommandNameB9 = 'Запрос отчёта ЭКЛЗ по закрытиям смен в заданном диапазоне номеров смен';
  SCommandNameBA = 'Запрос в ЭКЛЗ итогов смены по номеру смены';
  SCommandNameBB = 'Запрос итога активизации ЭКЛЗ';
  SCommandNameBC = 'Вернуть ошибку ЭКЛЗ';
  SCommandNameC0 = 'Загрузка графики';
  SCommandNameC1 = 'Печать графики';
  SCommandNameC2 = 'Печать штрихкода';
  SCommandNameC3 = 'Печать расширенной графики';
  SCommandNameC4 = 'Загрузка расширенной графики';
  SCommandNameD0 = 'Запрос состояния принтера IBM';
  SCommandNameD1 = 'Краткий запрос состояния принтера IBM';
  SCommandNameE0 = 'Открытие смены';
  SCommandNameF0 = 'Управление заслонкой';
  SCommandNameF1 = 'Выдать чек';
  SCommandNameFC = 'Получить тип устройства';
  SCommandNameFD = 'Управление портом';

  SCommandNameF7 = 'Запрос параметров модели';

  SCommandNameFE = 'Сервисная команда';

  SCommandName44 = 'Отчёт по кассирам'; { !!! }
  SCommandName45 = 'Отчёт почасовой'; { !!! }
  SCommandName46 = 'Отчёт по товарам'; { !!! }
  SCommandName4E = 'Загрузка графики 512'; { !!! }
  SCommandName4F = 'Печать графики с масштабированием'; { !!! }

  SCommandName52 = 'Печать клише'; { !!! }
  SCommandName53 = 'Завершение документа и печать клише'; { !!! }
  SCommandName54 = 'Печать рекламного текста'; { !!! }

  SCommandNameFE03 = 'Загрузка символа шрифта'; { !!! }
  SCommandNameC5 = 'Печать графической линии'; { !!! }

  SCommandNameFF01 = 'Запрос статуса ФН';
  SCommandNameFF02 = 'Запрос номера ФН'; // FF02H	3
  SCommandNameFF03 = 'Запрос срока действия ФН'; // FF03H	3
  SCommandNameFF04 = 'Запрос версии ФН'; // FF04H	4
  SCommandNameFF05 = 'Начать отчет о регистрации ККТ'; // FF05H	4
  SCommandNameFF06 = 'Сформировать отчёт о регистрации ККТ'; // FF06H	4
  SCommandNameFF07 = 'Сброс состояния ФН'; // FF07H	4
  SCommandNameFF08 = 'Отменить документ в ФН'; // FF08H	5
  SCommandNameFF09 = 'Запрос итогов последней фискализации (перерегистрации)'; // FF09H	5
  SCommandNameFF0A = 'Найти фискальный документ по номеру'; // FF0AH	6
  SCommandNameFF0B = 'Открыть смену в ФН'; // FF0BH	6
  SCommandNameFF0C = 'Передать произвольную TLV структуру'; // FF0CH	6
  SCommandNameFF0D = 'Операция со скидками и надбавками'; // FF0DH	6
  SCommandNameFF30 = 'Запросить о наличие данных в буфере'; // FF30H	7
  SCommandNameFF31 = 'Прочитать блок данных из буфера'; // FF31H	7
  SCommandNameFF32 = 'Начать запись данных в буфер'; // FF32H	8
  SCommandNameFF33 = 'Записать блок данных в буфер'; // FF33H	8
  SCommandNameFF34 = 'Сформировать отчёт о перерегистрации ККТ'; // FF34H	8
  SCommandNameFF35 = 'Начать формирование чека коррекции'; // FF35H	8
  SCommandNameFF36 = 'Сформировать чек коррекции'; // FF36H	8
  SCommandNameFF37 = 'Начать формирование отчёта о состоянии расчётов'; // FF37H	9
  SCommandNameFF38 = 'Сформировать отчёт о состоянии расчётов'; // FF38H	9
  SCommandNameFF39 = 'Получить статус информационного  обмена'; // FF39H	9
  SCommandNameFF3A = 'Запросить фискальный документ в TLV формате'; // FF3AH	10
  SCommandNameFF3B = 'Чтение TLV фискального документа'; // FF3BH	10
  SCommandNameFF3C = 'Запрос квитанции о получении данных в ОФД по номеру  документа'; // FF3CH	10
  SCommandNameFF3D = 'Начать закрытие фискального режима'; // FF3DH	10
  SCommandNameFF3E = 'Закрыть фискальный режим'; // FF3EH	10
  SCommandNameFF3F = 'Запрос количества ФД на которые нет квитанции'; // FF3FH	11
  SCommandNameFF40 = 'Запрос параметров текущей смены'; // FF40H	11
  SCommandNameFF41 = 'Начать открытие смены'; // FF41H	11
  SCommandNameFF42 = 'Начать закрытие смены'; // FF42H	11
  SCommandNameFF43 = 'Закрыть смену в ФН'; // FF43H	11
  SCommandNameFF45 = 'Закрытие чека расширенное вариант V2'; // FF45H	12
  SCommandNameFF46 = 'Операция V2'; // FF46H	13
  SCommandNameFF47 = 'Дополнительный реквизит ФНС'; // FF47H	14
  SCommandNameFF48 = 'Скидки и надбавки в операции'; // FF48H	14
  SCommandNameFF49 = 'Передача кода товарной номенклатуры'; // FF49H	14
  SCommandNameFF4A = 'Сформировать чек коррекции V2'; // FF4AH	14
  SCommandNameFF4B = 'Скидка, надбавка на чек для Роснефти'; // FF4BH	15
  SCommandNameFF4C = 'Запрос итогов фискализации (перерегистрации) V2'; // FF4CH	15
  SCommandNameFF4D = 'Передать произвольную TLV структуру привязанную к операции'; // FF4DH.	16
  SCommandNameFF4E = 'Запись блока данных прошивки  ФР на SD карту'; // FF4EH	16
  SCommandNameFF50 = 'Онлайн платёж'; // FF50H	16
  SCommandNameFF51 = 'Статус онлайн платёжа'; // FF51H	17
  SCommandNameFF52 = 'Получить реквизит последнего онлайн платёжа'; // FF52H	17
  SCommandNameFF60 = 'Запрос параметра фискализации '; // FF60H	17
  SCommandNameFF61 = 'Проверка  маркированного товара'; // FF61H	18
  SCommandNameFF62 = 'Синхронизировать регистры со счётчиком ФН'; // FF62H	18
  SCommandNameFF63 = 'Запрос ресурса свободной памяти  в ФН'; // FF63H	18
  SCommandNameFF64 = 'Передача в ФН  TLV из буфера'; // FF64H	19
  SCommandNameFF65 = 'Получить случайную последовательность'; // FF65H
  SCommandNameFF66 = 'Авторизация'; // FF66H
  SCommandNameFF67 = 'Привязка  маркированного товара к позиции'; // FF67H
  SCommandNameFF68 = 'Получить состояние по передачи уведомлений'; // FF68H
  SCommandNameFF69 = 'Принять/отклонить КМ'; // FF69H
  SCommandNameFF70 = 'Запрос статуса по работе с кодами маркировки'; // FF70H
  SCommandNameFF71 = 'Начать выгрузку уведомлений  о реализации маркированных товаров'; // FF71H
  SCommandNameFF72 = 'Прочитать блок уведомления'; // FF72H
  SCommandNameFF73 = 'Подтвердить выгрузку уведомления'; // FF73H
  SCommandNameFF74 = 'Запрос исполнения ФН'; // FF74H
  SCommandNameFF75 = 'Запрос размера данных документа в ФН'; // FF75H

  SNoCommandName = '';

function WordToStr(Value: Word): AnsiString;
function GetCommandName(Command: Word): string;
function BaudRateToStr(Value: Integer): WideString;
function GetECRModeDescription(Value: Integer): WideString;
function GetDeviceCodeDescription(Value: Integer): WideString;
function GetAdvancedModeDescription(Value: Integer): WideString;
function GetCashRegisterName(Number: Integer; ACapFN: Boolean): WideString;
function GetOperRegisterName(Number: Integer; ACapFN: Boolean): WideString;
function PollToDescription(Poll1: Integer; Poll2: Integer): WideString;
function GetCommandTimeout(Command: Word): Integer;

implementation

{ TPrinterError }

class function TPrinterError.GetDescription(Code: Integer; ACapFN: Boolean = False): WideString;
var
  Res: PResStringRec;
begin
  case Code of
    $00: Res := @SErrorDescription00;
    $01: Res := @SErrorDescription01;
    $02: Res := @SErrorDescription02;
    $03: Res := @SErrorDescription03;
    $04: Res := @SErrorDescription04;
    $05: Res := @SErrorDescription05;
    $06: Res := @SErrorDescription06;
    $07: Res := @SErrorDescription07;
    $08: Res := @SErrorDescription08;
    $09: Res := @SErrorDescription09;
    $0A: Res := @SErrorDescription0A;
    $0B: Res := @SErrorDescription0B;

    $11: Res := @SErrorDescription11;
    $12: Res := @SErrorDescription12;
    $13: Res := @SErrorDescription13;
    $14: Res := @SErrorDescription14;
    $15: Res := @SErrorDescription15;
    $16: Res := @SErrorDescription16;
    $17: Res := @SErrorDescription17;
    $18: Res := @SErrorDescription18;
    $19: Res := @SErrorDescription19;
    $1A: Res := @SErrorDescription1A;
    $1B: Res := @SErrorDescription1B;
    $1C: Res := @SErrorDescription1C;
    $1D: Res := @SErrorDescription1D;
    $1E: Res := @SErrorDescription1E;
    $1F: Res := @SErrorDescription1F;

    $20: Res := @SErrorDescription20;
    $21: Res := @SErrorDescription21;
    $22: Res := @SErrorDescription22;
    $23: Res := @SErrorDescription23;
    $24: Res := @SErrorDescription24;
    $25: Res := @SErrorDescription25;
    $26: Res := @SErrorDescription26;
    $27: Res := @SErrorDescription27;
    $28: Res := @SErrorDescription28;
    $29: Res := @SErrorDescription29;
    $2F: Res := @SErrorDescription2F;

    $30: Res := @SErrorDescription30;
    $31: Res := @SErrorDescription31;
    $32: Res := @SErrorDescription32;
    $33: Res := @SErrorDescription33;
    $34: Res := @SErrorDescription34;
    $35: Res := @SErrorDescription35;
    $36: Res := @SErrorDescription36;
    $37: Res := @SErrorDescription37;
    $38: Res := @SErrorDescription38;
    $39: Res := @SErrorDescription39;
    $3A: Res := @SErrorDescription3A;
    $3B: Res := @SErrorDescription3B;
    $3C: Res := @SErrorDescription3C;
    $3D: Res := @SErrorDescription3D;
    $3E: Res := @SErrorDescription3E;
    $3F: Res := @SErrorDescription3F;

    $40: Res := @SErrorDescription40;
    $41: Res := @SErrorDescription41;
    $42: Res := @SErrorDescription42;
    $43: Res := @SErrorDescription43;
    $44: Res := @SErrorDescription44;
    $45: Res := @SErrorDescription45;
    $46: Res := @SErrorDescription46;
    $47: Res := @SErrorDescription47;
    $48: Res := @SErrorDescription48;
    $49: Res := @SErrorDescription49;
    $4A: Res := @SErrorDescription4A;
    $4B: Res := @SErrorDescription4B;
    $4C: Res := @SErrorDescription4C;
    $4D: Res := @SErrorDescription4D;
    $4E: Res := @SErrorDescription4E;
    $4F: Res := @SErrorDescription4F;

    $50: Res := @SErrorDescription50;
    $51: Res := @SErrorDescription51;
    $52: Res := @SErrorDescription52;
    $53: Res := @SErrorDescription53;
    $54: Res := @SErrorDescription54;
    $55: Res := @SErrorDescription55;
    $56: Res := @SErrorDescription56;
    $57: Res := @SErrorDescription57;
    $58: Res := @SErrorDescription58;
    $59: Res := @SErrorDescription59;
    $5A: Res := @SErrorDescription5A;
    $5B: Res := @SErrorDescription5B;
    $5C: Res := @SErrorDescription5C;
    $5D: Res := @SErrorDescription5D;
    $5E: Res := @SErrorDescription5E;
    $5F: Res := @SErrorDescription5F;

    $60: Res := @SErrorDescription60;
    $61: Res := @SErrorDescription61;
    $62: Res := @SErrorDescription62;
    $63: Res := @SErrorDescription63;
    $64: Res := @SErrorDescription64;
    $65: Res := @SErrorDescription65;
    $66: Res := @SErrorDescription66;
    $67: Res := @SErrorDescription67;
    $68: Res := @SErrorDescription68;
    $69: Res := @SErrorDescription69;
    $6A: Res := @SErrorDescription6A;
    $6B: Res := @SErrorDescription6B;
    $6C: Res := @SErrorDescription6C;
    $6D: Res := @SErrorDescription6D;
    $6E: Res := @SErrorDescription6E;
    $6F: Res := @SErrorDescription6F;

    $70: Res := @SErrorDescription70;
    $71: Res := @SErrorDescription71;
    $72: Res := @SErrorDescription72;
    $73: Res := @SErrorDescription73;
    $74: Res := @SErrorDescription74;
    $75: Res := @SErrorDescription75;
    $76: Res := @SErrorDescription76;
    $77: Res := @SErrorDescription77;
    $78: Res := @SErrorDescription78;
    $79: Res := @SErrorDescription79;
    $7A: Res := @SErrorDescription7A;
    $7B: Res := @SErrorDescription7B;
    $7C: Res := @SErrorDescription7C;
    $7D: Res := @SErrorDescription7D;
    $7E: Res := @SErrorDescription7E;
    $7F: Res := @SErrorDescription7F;

    $80: Res := @SErrorDescription80;
    $81: Res := @SErrorDescription81;
    $82: Res := @SErrorDescription82;
    $83: Res := @SErrorDescription83;
    $84: Res := @SErrorDescription84;
    $85: Res := @SErrorDescription85;
    $86: Res := @SErrorDescription86;
    $87: Res := @SErrorDescription87;
    $88: Res := @SErrorDescription88;
    $89: Res := @SErrorDescription89;
    $8A: Res := @SErrorDescription8A;
    $8B: Res := @SErrorDescription8B;
    $8C: Res := @SErrorDescription8C;
    $8D: Res := @SErrorDescription8D;
    $8E: Res := @SErrorDescription8E;
    $8F: Res := @SErrorDescription8F;

    $90: Res := @SErrorDescription90;
    $91: Res := @SErrorDescription91;
    $92: Res := @SErrorDescription92;
    $93: Res := @SErrorDescription93;
    $94: Res := @SErrorDescription94;
    $95: Res := @SErrorDescription95;

    $A0: Res := @SErrorDescriptionA0;
    $A1: Res := @SErrorDescriptionA1;
    $A2: Res := @SErrorDescriptionA2;
    $A3: Res := @SErrorDescriptionA3;
    $A4: Res := @SErrorDescriptionA4;
    $A5: Res := @SErrorDescriptionA5;
    $A6: Res := @SErrorDescriptionA6;
    $A7: Res := @SErrorDescriptionA7;
    $A8: Res := @SErrorDescriptionA8;
    $A9: Res := @SErrorDescriptionA9;
    $AA: Res := @SErrorDescriptionAA;
    $AB: Res := @SErrorDescriptionAB;
    $AC: Res := @SErrorDescriptionAC;
    $AD: Res := @SErrorDescriptionAD;
    $AE: Res := @SErrorDescriptionAE;
    $AF: Res := @SErrorDescriptionAF;

    $B0: Res := @SErrorDescriptionB0;
    $B1: Res := @SErrorDescriptionB1;
    $B2: Res := @SErrorDescriptionB2;
    $B3: Res := @SErrorDescriptionB3;

    $C0: Res := @SErrorDescriptionC0;
    $C1: Res := @SErrorDescriptionC1;
    $C2: Res := @SErrorDescriptionC2;
    $C3: Res := @SErrorDescriptionC3;
    $C4: Res := @SErrorDescriptionC4;
    $C5: Res := @SErrorDescriptionC5;
    $C6: Res := @SErrorDescriptionC6;
    $C7: Res := @SErrorDescriptionC7;
    $C8: Res := @SErrorDescriptionC8;
    $C9: Res := @SErrorDescriptionC9;
    $CA: Res := @SerrorDescriptionCA;
    $CB: Res := @SerrorDescriptionCB;

    $D0: Res := @SErrorDescriptionD0;
    $D1: Res := @SErrorDescriptionD1;
    $D2: Res := @SErrorDescriptionD2;
    $D3: Res := @SErrorDescriptionD3;
    $D4: Res := @SErrorDescriptionD4;
    $D5: Res := @SErrorDescriptionD5;

    $E0: Res := @SErrorDescriptionE0;
    $E1: Res := @SErrorDescriptionE1;
    $E2: Res := @SErrorDescriptionE2;
    $E3: Res := @SErrorDescriptionE3;
    $E4: Res := @SErrorDescriptionE4;

    $F0: Res := @SErrorDescriptionF0;
    $F1: Res := @SErrorDescriptionF1;
    $F2: Res := @SErrorDescriptionF2;
    $F3: Res := @SErrorDescriptionF3;
    $F4: Res := @SErrorDescriptionF4;
    $F5: Res := @SErrorDescriptionF5;
  else
    Res := @SErrorDescriptionUnknown;
  end;

  if ACapFN then
  begin
    case Code of
      $01: Res := @SErrorDescription01FN;
      $02: Res := @SErrorDescription02FN;
      $03: Res := @SErrorDescription03FN;
      $04: Res := @SErrorDescription04FN;
      $05: Res := @SErrorDescription05FN;
      $06: Res := @SErrorDescription06FN;
      $07: Res := @SErrorDescription07FN;
      $08: Res := @SErrorDescription08FN;
      $09: Res := @SErrorDescription09FN;
      $0A: Res := @SErrorDescription0AFN;
      $0B: Res := @SErrorDescription0BFN;
      $0C: Res := @SErrorDescription0CFN;
      $0D: Res := @SErrorDescription0DFN;
      $0E: Res := @SErrorDescription0EFN;
      $10: Res := @SErrorDescription10FN;
      $11: Res := @SErrorDescription11FN;
      $12: Res := @SErrorDescription12FN;
      $14: Res := @SErrorDescription14FN;
      $15: Res := @SErrorDescription15FN;
      $16: Res := @SErrorDescription16FN;
      $17: Res := @SErrorDescription17FN;
      $18: Res := @SErrorDescription18FN;
      $19: Res := @SErrorDescription19FN;
      $20: Res := @SErrorDescription20FN;
      $21: Res := @SErrorDescription21FN;
      $30: Res := @SErrorDescription30FN;
      $77: Res := @SErrorDescription77FN;
      $79: Res := @SErrorDescription79FN;



      $23: Res := @SErrorDescription23FN;
      $24: Res := @SErrorDescription24FN;
      $A0: Res := @SErrorDescriptionA0FN;
      $A1: Res := @SErrorDescriptionA1FN;
      $A2: Res := @SErrorDescriptionA2FN;
      $A3: Res := @SErrorDescriptionA3FN;
      $AA: Res := @SErrorDescriptionAAFN;
      $AC: Res := @SErrorDescriptionACFN;


    end;
  end;
  Result := GetRes(Res);
end;





function GetAdvancedModeDescription(Value: Integer): WideString;
var
  Res: PResStringRec;
begin
  Result := '';
  case Value of
    0: Res := @SAdvancedMode0;
    1: Res := @SAdvancedMode1;
    2: Res := @SAdvancedMode2;
    3: Res := @SAdvancedMode3;
    4: Res := @SAdvancedMode4;
    5: Res := @SAdvancedMode5;
  else
    Result := Format(GetRes(@SAdvancedModeUnknown), [Value]);
  end;
  if Result = '' then
    Result := GetRes(Res);
end;

function GetDeviceCodeDescription(Value: Integer): WideString;
var
  Res: PResStringRec;
begin
  Result := '';
  case Value of
    1: Res := @SDeviceCode1;
    2: Res := @SDeviceCode2;
    3: Res := @SDeviceCode3;
    4: Res := @SDeviceCode4;
    5: Res := @SDeviceCode5;
    6: Res := @SDeviceCode6;
    7: Res := @SDeviceCode7;
  else
    Result := Format(GetRes(@SDeviceCodeUnknown), [Value]);
  end;
  if Result = '' then
    Result := GetRes(Res);
end;

function WordToStr(Value: Word): AnsiString;
begin
  SetLength(Result, 2);
  Move(Value, Result[1], 2);
end;


function GetECRModeDescription(Value: Integer): WideString;
var
  Status: Integer;
  Res: PResStringRec;
begin
  Result := '';
  // Старший бит означает, что есть документ для допечатывания
  Status := Value and $7F;
  case Status of
    $01: Res := @SDeviceMode01;
    $02: Res := @SDeviceMode02;
    $03: Res := @SDeviceMode03;
    $04: Res := @SDeviceMode04;
    $05: Res := @SDeviceMode05;
    $06: Res := @SDeviceMode06;
    $07: Res := @SDeviceMode07;
    $08: Res := @SDeviceMode08;
    $09: Res := @SDeviceMode09;
    $0A: Res := @SDeviceMode0A;
    $0B: Res := @SDeviceMode0B;
    $0C: Res := @SDeviceMode0C;
    $0D: Res := @SDeviceMode0D;
    $0E: Res := @SDeviceMode0E;
    $0F: Res := @SDeviceMode0F;
    $18: Res := @SDeviceMode18;
    $1D: Res := @SDeviceMode1D;
    $1E: Res := @SDeviceMode1E;
    $28: Res := @SDeviceMode28;
    $2D: Res := @SDeviceMode2D;
    $2E: Res := @SDeviceMode2E;
    $38: Res := @SDeviceMode38;
    $48: Res := @SDeviceMode48;
    $3D: Res := @SDeviceMode3D;
    $3E: Res := @SDeviceMode3E;
    $4C: Res := @SDeviceMode4C;
  	$4E: Res := @SDeviceMode4E;
    $5E: Res := @SDeviceMode5E;
    $6E: Res := @SDeviceMode6E;
  else
    Result := Format(GetRes(@SDeviceModeUnknown), [Value]);
  end;
  if Result = '' then
    Result := GetRes(Res);
  if TestBit(Value, 7) then
    Result := Result + GetRes(@SCorrectionReceipt);
end;

function BaudRateToStr(Value: Integer): WideString;
begin
  case Value of
    0: Result := '2400';
    1: Result := '4800';
    2: Result := '9600';
    3: Result := '19200';
    4: Result := '38400';
    5: Result := '57600';
    6: Result := '115200';
    7: Result := '230400';
    8: Result := '460800';
    9: Result := '921600';
  else
    Result := '';
  end;
end;

function GetCashRegisterName(Number: Integer; ACapFN: Boolean): WideString;
var
  Res: PResStringRec;
begin
  Result := '';
  case Number of
    $00: Res := @SCashRegister00;
    $01: Res := @SCashRegister01;
    $02: Res := @SCashRegister02;
    $03: Res := @SCashRegister03;
    $04: Res := @SCashRegister04;
    $05: Res := @SCashRegister05;
    $06: Res := @SCashRegister06;
    $07: Res := @SCashRegister07;
    $08: Res := @SCashRegister08;
    $09: Res := @SCashRegister09;
    $0A: Res := @SCashRegister0A;
    $0B: Res := @SCashRegister0B;
    $0C: Res := @SCashRegister0C;
    $0D: Res := @SCashRegister0D;
    $0E: Res := @SCashRegister0E;
    $0F: Res := @SCashRegister0F;

    $10: Res := @SCashRegister10;
    $11: Res := @SCashRegister11;
    $12: Res := @SCashRegister12;
    $13: Res := @SCashRegister13;
    $14: Res := @SCashRegister14;
    $15: Res := @SCashRegister15;
    $16: Res := @SCashRegister16;
    $17: Res := @SCashRegister17;
    $18: Res := @SCashRegister18;
    $19: Res := @SCashRegister19;
    $1A: Res := @SCashRegister1A;
    $1B: Res := @SCashRegister1B;
    $1C: Res := @SCashRegister1C;
    $1D: Res := @SCashRegister1D;
    $1E: Res := @SCashRegister1E;
    $1F: Res := @SCashRegister1F;

    $20: Res := @SCashRegister20;
    $21: Res := @SCashRegister21;
    $22: Res := @SCashRegister22;
    $23: Res := @SCashRegister23;
    $24: Res := @SCashRegister24;
    $25: Res := @SCashRegister25;
    $26: Res := @SCashRegister26;
    $27: Res := @SCashRegister27;
    $28: Res := @SCashRegister28;
    $29: Res := @SCashRegister29;
    $2A: Res := @SCashRegister2A;
    $2B: Res := @SCashRegister2B;
    $2C: Res := @SCashRegister2C;
    $2D: Res := @SCashRegister2D;
    $2E: Res := @SCashRegister2E;
    $2F: Res := @SCashRegister2F;

    $30: Res := @SCashRegister30;
    $31: Res := @SCashRegister31;
    $32: Res := @SCashRegister32;
    $33: Res := @SCashRegister33;
    $34: Res := @SCashRegister34;
    $35: Res := @SCashRegister35;
    $36: Res := @SCashRegister36;
    $37: Res := @SCashRegister37;
    $38: Res := @SCashRegister38;
    $39: Res := @SCashRegister39;
    $3A: Res := @SCashRegister3A;
    $3B: Res := @SCashRegister3B;
    $3C: Res := @SCashRegister3C;
    $3D: Res := @SCashRegister3D;
    $3E: Res := @SCashRegister3E;
    $3F: Res := @SCashRegister3F;

    $40: Res := @SCashRegister40;
    $41: Res := @SCashRegister41;
    $42: Res := @SCashRegister42;
    $43: Res := @SCashRegister43;
    $44: Res := @SCashRegister44;
    $45: Res := @SCashRegister45;
    $46: Res := @SCashRegister46;
    $47: Res := @SCashRegister47;
    $48: Res := @SCashRegister48;
    $49: Res := @SCashRegister49;
    $4A: Res := @SCashRegister4A;
    $4B: Res := @SCashRegister4B;
    $4C: Res := @SCashRegister4C;
    $4D: Res := @SCashRegister4D;
    $4E: Res := @SCashRegister4E;
    $4F: Res := @SCashRegister4F;

    $50: Res := @SCashRegister50;
    $51: Res := @SCashRegister51;
    $52: Res := @SCashRegister52;
    $53: Res := @SCashRegister53;
    $54: Res := @SCashRegister54;
    $55: Res := @SCashRegister55;
    $56: Res := @SCashRegister56;
    $57: Res := @SCashRegister57;
    $58: Res := @SCashRegister58;
    $59: Res := @SCashRegister59;
    $5A: Res := @SCashRegister5A;
    $5B: Res := @SCashRegister5B;
    $5C: Res := @SCashRegister5C;
    $5D: Res := @SCashRegister5D;
    $5E: Res := @SCashRegister5E;
    $5F: Res := @SCashRegister5F;

    $60: Res := @SCashRegister60;
    $61: Res := @SCashRegister61;
    $62: Res := @SCashRegister62;
    $63: Res := @SCashRegister63;
    $64: Res := @SCashRegister64;
    $65: Res := @SCashRegister65;
    $66: Res := @SCashRegister66;
    $67: Res := @SCashRegister67;
    $68: Res := @SCashRegister68;
    $69: Res := @SCashRegister69;
    $6A: Res := @SCashRegister6A;
    $6B: Res := @SCashRegister6B;
    $6C: Res := @SCashRegister6C;
    $6D: Res := @SCashRegister6D;
    $6E: Res := @SCashRegister6E;
    $6F: Res := @SCashRegister6F;

    $70: Res := @SCashRegister70;
    $71: Res := @SCashRegister71;
    $72: Res := @SCashRegister72;
    $73: Res := @SCashRegister73;
    $74: Res := @SCashRegister74;
    $75: Res := @SCashRegister75;
    $76: Res := @SCashRegister76;
    $77: Res := @SCashRegister77;
    $78: Res := @SCashRegister78;
    $79: Res := @SCashRegister79;
    $7A: Res := @SCashRegister7A;
    $7B: Res := @SCashRegister7B;
    $7C: Res := @SCashRegister7C;
    $7D: Res := @SCashRegister7D;
    $7E: Res := @SCashRegister7E;
    $7F: Res := @SCashRegister7F;

    $80: Res := @SCashRegister80;
    $81: Res := @SCashRegister81;
    $82: Res := @SCashRegister82;
    $83: Res := @SCashRegister83;
    $84: Res := @SCashRegister84;
    $85: Res := @SCashRegister85;
    $86: Res := @SCashRegister86;
    $87: Res := @SCashRegister87;
    $88: Res := @SCashRegister88;
    $89: Res := @SCashRegister89;
    $8A: Res := @SCashRegister8A;
    $8B: Res := @SCashRegister8B;
    $8C: Res := @SCashRegister8C;
    $8D: Res := @SCashRegister8D;
    $8E: Res := @SCashRegister8E;
    $8F: Res := @SCashRegister8F;

    $90: Res := @SCashRegister90;
    $91: Res := @SCashRegister91;
    $92: Res := @SCashRegister92;
    $93: Res := @SCashRegister93;
    $94: Res := @SCashRegister94;
    $95: Res := @SCashRegister95;
    $96: Res := @SCashRegister96;
    $97: Res := @SCashRegister97;
    $98: Res := @SCashRegister98;
    $99: Res := @SCashRegister99;
    $9A: Res := @SCashRegister9A;
    $9B: Res := @SCashRegister9B;
    $9C: Res := @SCashRegister9C;
    $9D: Res := @SCashRegister9D;
    $9E: Res := @SCashRegister9E;
    $9F: Res := @SCashRegister9F;

    $A0: Res := @SCashRegisterA0;
    $A1: Res := @SCashRegisterA1;
    $A2: Res := @SCashRegisterA2;
    $A3: Res := @SCashRegisterA3;
    $A4: Res := @SCashRegisterA4;
    $A5: Res := @SCashRegisterA5;
    $A6: Res := @SCashRegisterA6;
    $A7: Res := @SCashRegisterA7;
    $A8: Res := @SCashRegisterA8;
    $A9: Res := @SCashRegisterA9;
    $AA: Res := @SCashRegisterAA;
    $AB: Res := @SCashRegisterAB;
    $AC: Res := @SCashRegisterAC;
    $AD: Res := @SCashRegisterAD;
    $AE: Res := @SCashRegisterAE;
    $AF: Res := @SCashRegisterAF;

    $B0: Res := @SCashRegisterB0;
    $B1: Res := @SCashRegisterB1;
    $B2: Res := @SCashRegisterB2;
    $B3: Res := @SCashRegisterB3;
    $B4: Res := @SCashRegisterB4;
    $B5: Res := @SCashRegisterB5;
    $B6: Res := @SCashRegisterB6;
    $B7: Res := @SCashRegisterB7;
    $B8: Res := @SCashRegisterB8;
    $B9: Res := @SCashRegisterB9;
    $BA: Res := @SCashRegisterBA;
    $BB: Res := @SCashRegisterBB;
    $BC: Res := @SCashRegisterBC;
    $BD: Res := @SCashRegisterBD;
    $BE: Res := @SCashRegisterBE;
    $BF: Res := @SCashRegisterBF;

    $C0: Res := @SCashRegisterC0;
    $C1: Res := @SCashRegisterC1;
    $C2: Res := @SCashRegisterC2;
    $C3: Res := @SCashRegisterC3;
    $C4: Res := @SCashRegisterC4;
    $C5: Res := @SCashRegisterC5;
    $C6: Res := @SCashRegisterC6;
    $C7: Res := @SCashRegisterC7;
    $C8: Res := @SCashRegisterC8;
    $C9: Res := @SCashRegisterC9;
    $CA: Res := @SCashRegisterCA;
    $CB: Res := @SCashRegisterCB;
    $CC: Res := @SCashRegisterCC;
    $CD: Res := @SCashRegisterCD;
    $CE: Res := @SCashRegisterCE;
    $CF: Res := @SCashRegisterCF;

    $D0: Res := @SCashRegisterD0;
    $D1: Res := @SCashRegisterD1;
    $D2: Res := @SCashRegisterD2;
    $D3: Res := @SCashRegisterD3;
    $D4: Res := @SCashRegisterD4;
    $D5: Res := @SCashRegisterD5;
    $D6: Res := @SCashRegisterD6;
    $D7: Res := @SCashRegisterD7;
    $D8: Res := @SCashRegisterD8;
    $D9: Res := @SCashRegisterD9;
    $DA: Res := @SCashRegisterDA;
    $DB: Res := @SCashRegisterDB;
    $DC: Res := @SCashRegisterDC;
    $DD: Res := @SCashRegisterDD;
    $DE: Res := @SCashRegisterDE;
    $DF: Res := @SCashRegisterDF;

    $E0: Res := @SCashRegisterE0;
    $E1: Res := @SCashRegisterE1;
    $E2: Res := @SCashRegisterE2;
    $E3: Res := @SCashRegisterE3;
    $E4: Res := @SCashRegisterE4;
    $E5: Res := @SCashRegisterE5;
    $E6: Res := @SCashRegisterE6;
    $E7: Res := @SCashRegisterE7;
    $E8: Res := @SCashRegisterE8;
    $E9: Res := @SCashRegisterE9;
    $EA: Res := @SCashRegisterEA;
    $EB: Res := @SCashRegisterEB;
    $EC: Res := @SCashRegisterEC;
    $ED: Res := @SCashRegisterED;
    $EE: Res := @SCashRegisterEE;
    $EF: Res := @SCashRegisterEF;

    $F0: Res := @SCashRegisterF0;
    $F1: Res := @SCashRegisterF1;
    $F2: Res := @SCashRegisterF2;
    $F3: Res := @SCashRegisterF3;
    $F4: Res := @SCashRegisterF4;
    $F5: Res := @SCashRegisterF5;
    $F6: Res := @SCashRegisterF6;
    $F7: Res := @SCashRegisterF7;
    $F8: Res := @SCashRegisterF8;
    $F9: Res := @SCashRegisterF9;
    $FA: Res := @SCashRegisterFA;
    $FB: Res := @SCashRegisterFB;
    $FC: Res := @SCashRegisterFC;
    $FF: Res := @SCashRegisterFF;

    // Дополнительные регистры
    4096: Res := @SCashRegister4096;
    4097: Res := @SCashRegister4097;
    4098: Res := @SCashRegister4098;
    4099: Res := @SCashRegister4099;
    4100: Res := @SCashRegister4100;
    4101: Res := @SCashRegister4101;
    4102: Res := @SCashRegister4102;
    4103: Res := @SCashRegister4103;
    4104: Res := @SCashRegister4104;
    4105: Res := @SCashRegister4105;
    4106: Res := @SCashRegister4106;
    4107: Res := @SCashRegister4107;
    4108: Res := @SCashRegister4108;
    4109: Res := @SCashRegister4109;
    4110: Res := @SCashRegister4110;
    4111: Res := @SCashRegister4111;
    4112: Res := @SCashRegister4112;
    4113: Res := @SCashRegister4113;
    4114: Res := @SCashRegister4114;
    4115: Res := @SCashRegister4115;
    4116: Res := @SCashRegister4116;
    4117: Res := @SCashRegister4117;
    4118: Res := @SCashRegister4118;
    4119: Res := @SCashRegister4119;
    4120: Res := @SCashRegister4120;
    4121: Res := @SCashRegister4121;
    4122: Res := @SCashRegister4122;
    4123: Res := @SCashRegister4123;
    4124: Res := @SCashRegister4124;
    4125: Res := @SCashRegister4125;
    4126: Res := @SCashRegister4126;
    4127: Res := @SCashRegister4127;
    4128: Res := @SCashRegister4128;
    4129: Res := @SCashRegister4129;
    4130: Res := @SCashRegister4130;
    4131: Res := @SCashRegister4131;
    4132: Res := @SCashRegister4132;
    4133: Res := @SCashRegister4133;
    4134: Res := @SCashRegister4134;
    4135: Res := @SCashRegister4135;
    4136: Res := @SCashRegister4136;
    4137: Res := @SCashRegister4137;
    4138: Res := @SCashRegister4138;
    4139: Res := @SCashRegister4139;
    4140: Res := @SCashRegister4140;
    4141: Res := @SCashRegister4141;
    4142: Res := @SCashRegister4142;
    4143: Res := @SCashRegister4143;
    4144: Res := @SCashRegister4144;
    4145: Res := @SCashRegister4145;
    4146: Res := @SCashRegister4146;
    4147: Res := @SCashRegister4147;
    4148: Res := @SCashRegister4148;
    4149: Res := @SCashRegister4149;
    4150: Res := @SCashRegister4150;
    4151: Res := @SCashRegister4151;
    4152: Res := @SCashRegister4152;
    4153: Res := @SCashRegister4153;
    4154: Res := @SCashRegister4154;
    4155: Res := @SCashRegister4155;
    4156: Res := @SCashRegister4156;
    4157: Res := @SCashRegister4157;
    4158: Res := @SCashRegister4158;
    4159: Res := @SCashRegister4159;
    4160: Res := @SCashRegister4160;
    4161: Res := @SCashRegister4161;
    4162: Res := @SCashRegister4162;
    4163: Res := @SCashRegister4163;
    4164: Res := @SCashRegister4164;
    4165: Res := @SCashRegister4165;
    4166: Res := @SCashRegister4166;
    4167: Res := @SCashRegister4167;
    4168: Res := @SCashRegister4168;
    4169: Res := @SCashRegister4169;
    4170: Res := @SCashRegister4170;
    4171: Res := @SCashRegister4171;
    4172: Res := @SCashRegister4172;
    4173: Res := @SCashRegister4173;
    4174: Res := @SCashRegister4174;
    4175: Res := @SCashRegister4175;
    4176: Res := @SCashRegister4176;
    4177: Res := @SCashRegister4177;
    4178: Res := @SCashRegister4178;
    4179: Res := @SCashRegister4179;
    4180: Res := @SCashRegister4180;
    4181: Res := @SCashRegister4181;
    4182: Res := @SCashRegister4182;
    4183: Res := @SCashRegister4183;
    4184: Res := @SCashRegister4184;
    4185: Res := @SCashRegister4185;
    4186: Res := @SCashRegister4186;
    4187: Res := @SCashRegister4187;
    4188: Res := @SCashRegister4188;
    4189: Res := @SCashRegister4189;
    4190: Res := @SCashRegister4190;
    4191: Res := @SCashRegister4191;

    4192: Res := @SCashRegister4192;
    4193: Res := @SCashRegister4193;
    4194: Res := @SCashRegister4194;
    4195: Res := @SCashRegister4195;
    4196: Res := @SCashRegister4196;
    4197: Res := @SCashRegister4197;
    4198: Res := @SCashRegister4198;
    4199: Res := @SCashRegister4199;
    4200: Res := @SCashRegister4200;
    4201: Res := @SCashRegister4201;
    4202: Res := @SCashRegister4202;
    4203: Res := @SCashRegister4203;
    4204: Res := @SCashRegister4204;
    4205: Res := @SCashRegister4205;
    4206: Res := @SCashRegister4206;
    4207: Res := @SCashRegister4207;
    4208: Res := @SCashRegister4208;
    4209: Res := @SCashRegister4209;
    4210: Res := @SCashRegister4210;
    4211: Res := @SCashRegister4211;
    4212: Res := @SCashRegister4212;
    4213: Res := @SCashRegister4213;
    4214: Res := @SCashRegister4214;
    4215: Res := @SCashRegister4215;
    4216: Res := @SCashRegister4216;
    4217: Res := @SCashRegister4217;
    4218: Res := @SCashRegister4218;
    4219: Res := @SCashRegister4219;
    4220: Res := @SCashRegister4220;
    4221: Res := @SCashRegister4221;
    4222: Res := @SCashRegister4222;
    4223: Res := @SCashRegister4223;
    4224: Res := @SCashRegister4224;
    4225: Res := @SCashRegister4225;
    4226: Res := @SCashRegister4226;
    4227: Res := @SCashRegister4227;
    4228: Res := @SCashRegister4228;
    4229: Res := @SCashRegister4229;
    4230: Res := @SCashRegister4230;
    4231: Res := @SCashRegister4231;
    4232: Res := @SCashRegister4232;
    4233: Res := @SCashRegister4233;
    4234: Res := @SCashRegister4234;
    4235: Res := @SCashRegister4235;
    4236: Res := @SCashRegister4236;
    4237: Res := @SCashRegister4237;
    4238: Res := @SCashRegister4238;
    4239: Res := @SCashRegister4239;
    4240: Res := @SCashRegister4240;
    4241: Res := @SCashRegister4241;
    4242: Res := @SCashRegister4242;
    4243: Res := @SCashRegister4243;
    4244: Res := @SCashRegister4244;
    4245: Res := @SCashRegister4245;
    4246: Res := @SCashRegister4246;
    4247: Res := @SCashRegister4247;
    4248: Res := @SCashRegister4248;
    4249: Res := @SCashRegister4249;
    4250: Res := @SCashRegister4250;
    4251: Res := @SCashRegister4251;
    4252: Res := @SCashRegister4252;
    4253: Res := @SCashRegister4253;
    4254: Res := @SCashRegister4254;
    4255: Res := @SCashRegister4255;
    4256: Res := @SCashRegister4256;
    4257: Res := @SCashRegister4257;
    4258: Res := @SCashRegister4258;
    4259: Res := @SCashRegister4259;
    4260: Res := @SCashRegister4260;
    4261: Res := @SCashRegister4261;
    4262: Res := @SCashRegister4262;
    4263: Res := @SCashRegister4263;
    4264: Res := @SCashRegister4264;
    4265: Res := @SCashRegister4265;
    4266: Res := @SCashRegister4266;
    4267: Res := @SCashRegister4267;
    4268: Res := @SCashRegister4268;
    4269: Res := @SCashRegister4269;
    4270: Res := @SCashRegister4270;
    4271: Res := @SCashRegister4271;
    4272: Res := @SCashRegister4272;
    4273: Res := @SCashRegister4273;
    4274: Res := @SCashRegister4274;
    4275: Res := @SCashRegister4275;
    4276: Res := @SCashRegister4276;
    4277: Res := @SCashRegister4277;
    4278: Res := @SCashRegister4278;
    4279: Res := @SCashRegister4279;
    4280: Res := @SCashRegister4280;
    4281: Res := @SCashRegister4281;
    4282: Res := @SCashRegister4282;
    4283: Res := @SCashRegister4283;
    4284: Res := @SCashRegister4284;
    4285: Res := @SCashRegister4285;
    4286: Res := @SCashRegister4286;
    4287: Res := @SCashRegister4287;
    4288: Res := @SCashRegister4288;
    4289: Res := @SCashRegister4289;
    4290: Res := @SCashRegister4290;
    4291: Res := @SCashRegister4291;
    4292: Res := @SCashRegister4292;
    4293: Res := @SCashRegister4293;
    4294: Res := @SCashRegister4294;
    4295: Res := @SCashRegister4295;
    4296: Res := @SCashRegister4296;
    4297: Res := @SCashRegister4297;
    4298: Res := @SCashRegister4298;
    4299: Res := @SCashRegister4299;
    4300: Res := @SCashRegister4300;
    4301: Res := @SCashRegister4301;
    4302: Res := @SCashRegister4302;
    4303: Res := @SCashRegister4303;
    4304: Res := @SCashRegister4304;
    4305: Res := @SCashRegister4305;
    4306: Res := @SCashRegister4306;
    4307: Res := @SCashRegister4307;
    4308: Res := @SCashRegister4308;
    4309: Res := @SCashRegister4309;
    4310: Res := @SCashRegister4310;
    4311: Res := @SCashRegister4311;
    4312: Res := @SCashRegister4312;
    4313: Res := @SCashRegister4313;
    4314: Res := @SCashRegister4314;
    4315: Res := @SCashRegister4315;
    4316: Res := @SCashRegister4316;
    4317: Res := @SCashRegister4317;
    4318: Res := @SCashRegister4318;
    4319: Res := @SCashRegister4319;
    4320: Res := @SCashRegister4320;
    4321: Res := @SCashRegister4321;
    4322: Res := @SCashRegister4322;
    4323: Res := @SCashRegister4323;
    4324: Res := @SCashRegister4324;
    4325: Res := @SCashRegister4325;
    4326: Res := @SCashRegister4326;
    4327: Res := @SCashRegister4327;
    4328: Res := @SCashRegister4328;
    4329: Res := @SCashRegister4329;
    4330: Res := @SCashRegister4330;
    4331: Res := @SCashRegister4331;
    4332: Res := @SCashRegister4332;
    4333: Res := @SCashRegister4333;
    4334: Res := @SCashRegister4334;
    4335: Res := @SCashRegister4335;
    4336: Res := @SCashRegister4336;
    4337: Res := @SCashRegister4337;
    4338: Res := @SCashRegister4338;
    4339: Res := @SCashRegister4339;
    4340: Res := @SCashRegister4340;
    4341: Res := @SCashRegister4341;
    4342: Res := @SCashRegister4342;
    4343: Res := @SCashRegister4343;
    4344: Res := @SCashRegister4344;
    4345: Res := @SCashRegister4345;
    4346: Res := @SCashRegister4346;
    4347: Res := @SCashRegister4347;
    4348: Res := @SCashRegister4348;
    4349: Res := @SCashRegister4349;
    4350: Res := @SCashRegister4350;
    4351: Res := @SCashRegister4351;
    4352: Res := @SCashRegister4352;
    4353: Res := @SCashRegister4353;
    4354: Res := @SCashRegister4354;
    4355: Res := @SCashRegister4355;
    4356: Res := @SCashRegister4356;
    4357: Res := @SCashRegister4357;
    4358: Res := @SCashRegister4358;
    4359: Res := @SCashRegister4359;
    4360: Res := @SCashRegister4360;
    4361: Res := @SCashRegister4361;
    4362: Res := @SCashRegister4362;
    4363: Res := @SCashRegister4363;
    4364: Res := @SCashRegister4364;
    4365: Res := @SCashRegister4365;
    4366: Res := @SCashRegister4366;
    4367: Res := @SCashRegister4367;
    4368: Res := @SCashRegister4368;
    4369: Res := @SCashRegister4369;
    4370: Res := @SCashRegister4370;
    4371: Res := @SCashRegister4371;
    4372: Res := @SCashRegister4372;
    4373: Res := @SCashRegister4373;
    4374: Res := @SCashRegister4374;
    4375: Res := @SCashRegister4375;
    4376: Res := @SCashRegister4376;
    4377: Res := @SCashRegister4377;
    4378: Res := @SCashRegister4378;
    4379: Res := @SCashRegister4379;
    4380: Res := @SCashRegister4380;
    4381: Res := @SCashRegister4381;
    4382: Res := @SCashRegister4382;
    4383: Res := @SCashRegister4383;
    4384: Res := @SCashRegister4384;
    4385: Res := @SCashRegister4385;
    4386: Res := @SCashRegister4386;
    4387: Res := @SCashRegister4387;
    4388: Res := @SCashRegister4388;
    4389: Res := @SCashRegister4389;
    4390: Res := @SCashRegister4390;
    4391: Res := @SCashRegister4391;
    4392: Res := @SCashRegister4392;
    4393: Res := @SCashRegister4393;
    4394: Res := @SCashRegister4394;
    4395: Res := @SCashRegister4395;
    4396: Res := @SCashRegister4396;
    4397: Res := @SCashRegister4397;
    4398: Res := @SCashRegister4398;
    4399: Res := @SCashRegister4399;
    4400: Res := @SCashRegister4400;
    4401: Res := @SCashRegister4401;
    4402: Res := @SCashRegister4402;
    4403: Res := @SCashRegister4403;
    4404: Res := @SCashRegister4404;
    4405: Res := @SCashRegister4405;
    4406: Res := @SCashRegister4406;
    4407: Res := @SCashRegister4407;
    4408: Res := @SCashRegister4408;
    4409: Res := @SCashRegister4409;
    4410: Res := @SCashRegister4410;
    4411: Res := @SCashRegister4411;
    4412: Res := @SCashRegister4412;
    4413: Res := @SCashRegister4413;
    4414: Res := @SCashRegister4414;
    4415: Res := @SCashRegister4415;
    4416: Res := @SCashRegister4416;
    4417: Res := @SCashRegister4417;
    4418: Res := @SCashRegister4418;
    4419: Res := @SCashRegister4419;
    4420: Res := @SCashRegister4420;
    4421: Res := @SCashRegister4421;
    4422: Res := @SCashRegister4422;
    4423: Res := @SCashRegister4423;
    4424: Res := @SCashRegister4424;
    4425: Res := @SCashRegister4425;
    4426: Res := @SCashRegister4426;

    4427: Res := @SCashRegister4427;
    4428: Res := @SCashRegister4428;
    4429: Res := @SCashRegister4429;
    4430: Res := @SCashRegister4430;
    4431: Res := @SCashRegister4431;
    4432: Res := @SCashRegister4432;
    4433: Res := @SCashRegister4433;
    4434: Res := @SCashRegister4434;
    4435: Res := @SCashRegister4435;
    4436: Res := @SCashRegister4436;

    4437: Res := @SCashRegister4437;
    4438: Res := @SCashRegister4438;
    4439: Res := @SCashRegister4439;
    4440: Res := @SCashRegister4440;
    4441: Res := @SCashRegister4441;
    4442: Res := @SCashRegister4442;
    4443: Res := @SCashRegister4443;
    4444: Res := @SCashRegister4444;
    4445: Res := @SCashRegister4445;
    4446: Res := @SCashRegister4446;

    4447: Res := @SCashRegister4447;
    4448: Res := @SCashRegister4448;
    4449: Res := @SCashRegister4449;
    4450: Res := @SCashRegister4450;
    4451: Res := @SCashRegister4451;
    4452: Res := @SCashRegister4452;
    4453: Res := @SCashRegister4453;
    4454: Res := @SCashRegister4454;
    4455: Res := @SCashRegister4455;
    4456: Res := @SCashRegister4456;

    4457: Res := @SCashRegister4457;
    4458: Res := @SCashRegister4458;
    4459: Res := @SCashRegister4459;
    4460: Res := @SCashRegister4460;
    4461: Res := @SCashRegister4461;
    4462: Res := @SCashRegister4462;
    4463: Res := @SCashRegister4463;
    4464: Res := @SCashRegister4464;
    4465: Res := @SCashRegister4465;
    4466: Res := @SCashRegister4466;

    4467: Res := @SCashRegister4467;
    4468: Res := @SCashRegister4468;
    4469: Res := @SCashRegister4469;
    4470: Res := @SCashRegister4470;
    4471: Res := @SCashRegister4471;
    4472: Res := @SCashRegister4472;
    4473: Res := @SCashRegister4473;
    4474: Res := @SCashRegister4474;
    4475: Res := @SCashRegister4475;
    4476: Res := @SCashRegister4476;

    4477: Res := @SCashRegister4477;
    4478: Res := @SCashRegister4478;
    4479: Res := @SCashRegister4479;
    4480: Res := @SCashRegister4480;
    4481: Res := @SCashRegister4481;
    4482: Res := @SCashRegister4482;
    4483: Res := @SCashRegister4483;
    4484: Res := @SCashRegister4484;
    4485: Res := @SCashRegister4485;
    4486: Res := @SCashRegister4486;

    4487: Res := @SCashRegister4487;
    4488: Res := @SCashRegister4488;
    4489: Res := @SCashRegister4489;
    4490: Res := @SCashRegister4490;
    4491: Res := @SCashRegister4491;
    4492: Res := @SCashRegister4492;
    4493: Res := @SCashRegister4493;
    4494: Res := @SCashRegister4494;
    4495: Res := @SCashRegister4495;
    4496: Res := @SCashRegister4496;

    4497: Res := @SCashRegister4497;
    4498: Res := @SCashRegister4498;
    4499: Res := @SCashRegister4499;
    4500: Res := @SCashRegister4500;
    4501: Res := @SCashRegister4501;
    4502: Res := @SCashRegister4502;
    4503: Res := @SCashRegister4503;
    4504: Res := @SCashRegister4504;
    4505: Res := @SCashRegister4505;
    4506: Res := @SCashRegister4506;

    4507: Res := @SCashRegister4507;
    4508: Res := @SCashRegister4508;
    4509: Res := @SCashRegister4509;
    4510: Res := @SCashRegister4510;
    4511: Res := @SCashRegister4511;
    4512: Res := @SCashRegister4512;
    4513: Res := @SCashRegister4513;
    4514: Res := @SCashRegister4514;
    4515: Res := @SCashRegister4515;
    4516: Res := @SCashRegister4516;

    4517: Res := @SCashRegister4517;
    4518: Res := @SCashRegister4518;
    4519: Res := @SCashRegister4519;
    4520: Res := @SCashRegister4520;
    4521: Res := @SCashRegister4521;
    4522: Res := @SCashRegister4522;

  else
    Res := @SNoDescrpiption;
  end;

  if ACapFN then
  begin
    case Number of
      244, 245..248, 253..255:  Res := @SUnusedRegister;
    end;
  end;

  Result := GetRes(Res);
end;

function GetOperRegisterName(Number: Integer; ACapFN: Boolean): WideString;
var
  Res: PResStringRec;
begin
  case Number of
    $00: Res := @SOperatingRegister00;
    $01: Res := @SOperatingRegister01;
    $02: Res := @SOperatingRegister02;
    $03: Res := @SOperatingRegister03;
    $04: Res := @SOperatingRegister04;
    $05: Res := @SOperatingRegister05;
    $06: Res := @SOperatingRegister06;
    $07: Res := @SOperatingRegister07;
    $08: Res := @SOperatingRegister08;
    $09: Res := @SOperatingRegister09;
    $0A: Res := @SOperatingRegister0A;
    $0B: Res := @SOperatingRegister0B;
    $0C: Res := @SOperatingRegister0C;
    $0D: Res := @SOperatingRegister0D;
    $0E: Res := @SOperatingRegister0E;
    $0F: Res := @SOperatingRegister0F;

    $10: Res := @SOperatingRegister10;
    $11: Res := @SOperatingRegister11;
    $12: Res := @SOperatingRegister12;
    $13: Res := @SOperatingRegister13;
    $14: Res := @SOperatingRegister14;
    $15: Res := @SOperatingRegister15;
    $16: Res := @SOperatingRegister16;
    $17: Res := @SOperatingRegister17;
    $18: Res := @SOperatingRegister18;
    $19: Res := @SOperatingRegister19;
    $1A: Res := @SOperatingRegister1A;
    $1B: Res := @SOperatingRegister1B;
    $1C: Res := @SOperatingRegister1C;
    $1D: Res := @SOperatingRegister1D;
    $1E: Res := @SOperatingRegister1E;
    $1F: Res := @SOperatingRegister1F;

    $20: Res := @SOperatingRegister20;
    $21: Res := @SOperatingRegister21;
    $22: Res := @SOperatingRegister22;
    $23: Res := @SOperatingRegister23;
    $24: Res := @SOperatingRegister24;
    $25: Res := @SOperatingRegister25;
    $26: Res := @SOperatingRegister26;
    $27: Res := @SOperatingRegister27;
    $28: Res := @SOperatingRegister28;
    $29: Res := @SOperatingRegister29;
    $2A: Res := @SOperatingRegister2A;
    $2B: Res := @SOperatingRegister2B;
    $2C: Res := @SOperatingRegister2C;
    $2D: Res := @SOperatingRegister2D;
    $2E: Res := @SOperatingRegister2E;
    $2F: Res := @SOperatingRegister2F;

    $30: Res := @SOperatingRegister30;
    $31: Res := @SOperatingRegister31;
    $32: Res := @SOperatingRegister32;
    $33: Res := @SOperatingRegister33;
    $34: Res := @SOperatingRegister34;
    $35: Res := @SOperatingRegister35;
    $36: Res := @SOperatingRegister36;
    $37: Res := @SOperatingRegister37;
    $38: Res := @SOperatingRegister38;
    $39: Res := @SOperatingRegister39;
    $3A: Res := @SOperatingRegister3A;
    $3B: Res := @SOperatingRegister3B;
    $3C: Res := @SOperatingRegister3C;
    $3D: Res := @SOperatingRegister3D;
    $3E: Res := @SOperatingRegister3E;
    $3F: Res := @SOperatingRegister3F;

    $40: Res := @SOperatingRegister40;
    $41: Res := @SOperatingRegister41;
    $42: Res := @SOperatingRegister42;
    $43: Res := @SOperatingRegister43;
    $44: Res := @SOperatingRegister44;
    $45: Res := @SOperatingRegister45;
    $46: Res := @SOperatingRegister46;
    $47: Res := @SOperatingRegister47;
    $48: Res := @SOperatingRegister48;
    $49: Res := @SOperatingRegister49;
    $4A: Res := @SOperatingRegister4A;
    $4B: Res := @SOperatingRegister4B;
    $4C: Res := @SOperatingRegister4C;
    $4D: Res := @SOperatingRegister4D;
    $4E: Res := @SOperatingRegister4E;
    $4F: Res := @SOperatingRegister4F;

    $50: Res := @SOperatingRegister50;
    $51: Res := @SOperatingRegister51;
    $52: Res := @SOperatingRegister52;
    $53: Res := @SOperatingRegister53;
    $54: Res := @SOperatingRegister54;
    $55: Res := @SOperatingRegister55;
    $56: Res := @SOperatingRegister56;
    $57: Res := @SOperatingRegister57;
    $58: Res := @SOperatingRegister58;
    $59: Res := @SOperatingRegister59;
    $5A: Res := @SOperatingRegister5A;
    $5B: Res := @SOperatingRegister5B;
    $5C: Res := @SOperatingRegister5C;
    $5D: Res := @SOperatingRegister5D;
    $5E: Res := @SOperatingRegister5E;
    $5F: Res := @SOperatingRegister5F;

    $60: Res := @SOperatingRegister60;
    $61: Res := @SOperatingRegister61;
    $62: Res := @SOperatingRegister62;
    $63: Res := @SOperatingRegister63;
    $64: Res := @SOperatingRegister64;
    $65: Res := @SOperatingRegister65;
    $66: Res := @SOperatingRegister66;
    $67: Res := @SOperatingRegister67;
    $68: Res := @SOperatingRegister68;
    $69: Res := @SOperatingRegister69;
    $6A: Res := @SOperatingRegister6A;
    $6B: Res := @SOperatingRegister6B;
    $6C: Res := @SOperatingRegister6C;
    $6D: Res := @SOperatingRegister6D;
    $6E: Res := @SOperatingRegister6E;
    $6F: Res := @SOperatingRegister6F;

    $70: Res := @SOperatingRegister70;
    $71: Res := @SOperatingRegister71;
    $72: Res := @SOperatingRegister72;
    $73: Res := @SOperatingRegister73;
    $74: Res := @SOperatingRegister74;
    $75: Res := @SOperatingRegister75;
    $76: Res := @SOperatingRegister76;
    $77: Res := @SOperatingRegister77;
    $78: Res := @SOperatingRegister78;
    $79: Res := @SOperatingRegister79;
    $7A: Res := @SOperatingRegister7A;
    $7B: Res := @SOperatingRegister7B;
    $7C: Res := @SOperatingRegister7C;
    $7D: Res := @SOperatingRegister7D;
    $7E: Res := @SOperatingRegister7E;
    $7F: Res := @SOperatingRegister7F;

    $80: Res := @SOperatingRegister80;
    $81: Res := @SOperatingRegister81;
    $82: Res := @SOperatingRegister82;
    $83: Res := @SOperatingRegister83;
    $84: Res := @SOperatingRegister84;
    $85: Res := @SOperatingRegister85;
    $86: Res := @SOperatingRegister86;
    $87: Res := @SOperatingRegister87;
    $88: Res := @SOperatingRegister88;
    $89: Res := @SOperatingRegister89;
    $8A: Res := @SOperatingRegister8A;
    $8B: Res := @SOperatingRegister8B;
    $8C: Res := @SOperatingRegister8C;
    $8D: Res := @SOperatingRegister8D;
    $8E: Res := @SOperatingRegister8E;
    $8F: Res := @SOperatingRegister8F;

    $90: Res := @SOperatingRegister90;
    $91: Res := @SOperatingRegister91;
    $92: Res := @SOperatingRegister92;
    $93: Res := @SOperatingRegister93;
    $94: Res := @SOperatingRegister94;
    $95: Res := @SOperatingRegister95;
    $96: Res := @SOperatingRegister96;
    $97: Res := @SOperatingRegister97;
    $98: Res := @SOperatingRegister98;
    $99: Res := @SOperatingRegister99;
    $9A: Res := @SOperatingRegister9A;
    $9B: Res := @SOperatingRegister9B;
    $9C: Res := @SOperatingRegister9C;
    $9D: Res := @SOperatingRegister9D;
    $9E: Res := @SOperatingRegister9E;
    $9F: Res := @SOperatingRegister9F;

    $A0: Res := @SOperatingRegisterA0;
    $A1: Res := @SOperatingRegisterA1;
    $A2: Res := @SOperatingRegisterA2;
    $A3: Res := @SOperatingRegisterA3;
    $A4: Res := @SOperatingRegisterA4;
    $A5: Res := @SOperatingRegisterA5;
    $A6: Res := @SOperatingRegisterA6;
    $A7: Res := @SOperatingRegisterA7;
    $A8: Res := @SOperatingRegisterA8;
    $A9: Res := @SOperatingRegisterA9;
    $AA: Res := @SOperatingRegisterAA;
    $AB: Res := @SOperatingRegisterAB;
    $AC: Res := @SOperatingRegisterAC;
    $AD: Res := @SOperatingRegisterAD;
    $AE: Res := @SOperatingRegisterAE;
    $AF: Res := @SOperatingRegisterAF;

    $B0: Res := @SOperatingRegisterB0;
    $B1: Res := @SOperatingRegisterB1;
    $B2: Res := @SOperatingRegisterB2;
    $B3: Res := @SOperatingRegisterB3;
    $B4: Res := @SOperatingRegisterB4;
    $B5: Res := @SOperatingRegisterB5;
    $B6: Res := @SOperatingRegisterB6;
    $B7: Res := @SOperatingRegisterB7;
    $B8: Res := @SOperatingRegisterB8;
    $B9: Res := @SOperatingRegisterB9;
    $BA: Res := @SOperatingRegisterBA;
    $BB: Res := @SOperatingRegisterBB;
    $BC: Res := @SOperatingRegisterBC;
    $BD: Res := @SOperatingRegisterBD;
    $BE: Res := @SOperatingRegisterBE;
    $C1: Res := @SOperatingRegisterC1;
    $C2: Res := @SOperatingRegisterC2;
    $C3: Res := @SOperatingRegisterC3;
    $C4: Res := @SOperatingRegisterC4;
    $C5: Res := @SOperatingRegisterC5;
    $C6: Res := @SOperatingRegisterC6;
    $FF: Res := @SOperatingRegisterFF;
  else
    Res := @SNoDescrpiption;
  end;

  if ACapFN then
  begin
    case Number of
      $B8: Res := @SOperatingRegisterB8FN;
      $B9: Res := @SOperatingRegisterB9FN;
      $BA: Res := @SOperatingRegisterBAFN;
      $BC..$C3, $FF: Res := @SUnusedRegister;
      $C4: Res := @SOperatingRegisterC4FN;
      $C5: Res := @SOperatingRegisterC5FN;
      $C6: Res := @SOperatingRegisterC6FN;
      $C7: Res := @SOperatingRegisterC7FN;

      $C8: Res := @SOperatingRegisterC8FN;
      $C9: Res := @SOperatingRegisterC9FN;
      $CA: Res := @SOperatingRegisterCAFN;
      $CB: Res := @SOperatingRegisterCBFN;
    end;
  end;
  Result := GetRes(Res);
end;


{ Получение названия команды }

function GetCommandName(Command: Word): string;
var
  Res: PResStringRec;
begin
  case Command of
    $01: Res := @SCommandName01;
    $02: Res := @SCommandName02;
    $03: Res := @SCommandName03;
    $0D: Res := @SCommandName0D;
    $0E: Res := @SCommandName0E;
    $0F: Res := @SCommandName0F;
    $10: Res := @SCommandName10;
    $11: Res := @SCommandName11;
    $12: Res := @SCommandName12;
    $13: Res := @SCommandName13;
    $14: Res := @SCommandName14;
    $15: Res := @SCommandName15;
    $16: Res := @SCommandName16;
    $17: Res := @SCommandName17;
    $18: Res := @SCommandName18;
    $19: Res := @SCommandName19;
    $1A: Res := @SCommandName1A;
    $1B: Res := @SCommandName1B;
    $1C: Res := @SCommandName1C;
    $1D: Res := @SCommandName1D;
    $1E: Res := @SCommandName1E;
    $1F: Res := @SCommandName1F;
    $20: Res := @SCommandName20;
    $21: Res := @SCommandName21;
    $22: Res := @SCommandName22;
    $23: Res := @SCommandName23;
    $24: Res := @SCommandName24;
    $25: Res := @SCommandName25;
    $26: Res := @SCommandName26;
    $27: Res := @SCommandName27;
    $28: Res := @SCommandName28;
    $29: Res := @SCommandName29;
    $2A: Res := @SCommandName2A;
    $2B: Res := @SCommandName2B;
    $2C: Res := @SCommandName2C;
    $2D: Res := @SCommandName2D;
    $2E: Res := @SCommandName2E;
    $2F: Res := @SCommandName2F;
    $40: Res := @SCommandName40;
    $41: Res := @SCommandName41;
    $42: Res := @SCommandName42;
    $43: Res := @SCommandName43;
    $44: Res := @SCommandName44;
    $45: Res := @SCommandName45;
    $46: Res := @SCommandName46;
    $4E: Res := @SCommandName4E;
    $4F: Res := @SCommandName4F;
    $50: Res := @SCommandName50;
    $51: Res := @SCommandName51;
    $52: Res := @SCommandName52;
    $53: Res := @SCommandName53;
    $54: Res := @SCommandName54;
    $57: Res := @SCommandname57;
    $60: Res := @SCommandName60;
    $61: Res := @SCommandName61;
    $62: Res := @SCommandName62;
    $63: Res := @SCommandName63;
    $64: Res := @SCommandName64;
    $65: Res := @SCommandName65;
    $66: Res := @SCommandName66;
    $67: Res := @SCommandName67;
    $68: Res := @SCommandName68;
    $69: Res := @SCommandName69;
    $6B: Res := @SCommandName6B;
    $70: Res := @SCommandName70;
    $71: Res := @SCommandName71;
    $72: Res := @SCommandName72;
    $73: Res := @SCommandName73;
    $74: Res := @SCommandName74;
    $75: Res := @SCommandName75;
    $76: Res := @SCommandName76;
    $77: Res := @SCommandName77;
    $78: Res := @SCommandName78;
    $79: Res := @SCommandName79;
    $7A: Res := @SCommandName7A;
    $7B: Res := @SCommandName7B;
    $7C: Res := @SCommandName7C;
    $7D: Res := @SCommandName7D;
    $7E: Res := @SCommandName7E;
    $80: Res := @SCommandName80;
    $81: Res := @SCommandName81;
    $82: Res := @SCommandName82;
    $83: Res := @SCommandName83;
    $84: Res := @SCommandName84;
    $85: Res := @SCommandName85;
    $86: Res := @SCommandName86;
    $87: Res := @SCommandName87;
    $88: Res := @SCommandName88;
    $89: Res := @SCommandName89;
    $8A: Res := @SCommandName8A;
    $8B: Res := @SCommandName8B;
    $8C: Res := @SCommandName8C;
    $8D: Res := @SCommandName8D;
    $90: Res := @SCommandName90;
    $91: Res := @SCommandName91;
    $92: Res := @SCommandName92;
    $93: Res := @SCommandName93;
    $94: Res := @SCommandName94;
    $95: Res := @SCommandName95;
    $96: Res := @SCommandName96;
    $97: Res := @SCommandName97;
    $98: Res := @SCommandName98;
    $99: Res := @SCommandName99;
    $9A: Res := @SCommandName9A;
    $9B: Res := @SCommandName9B;
    $9E: Res := @SCommandName9E;
    $9F: Res := @SCommandName9F;
    $A0: Res := @SCommandNameA0;
    $A1: Res := @SCommandNameA1;
    $A2: Res := @SCommandNameA2;
    $A3: Res := @SCommandNameA3;
    $A4: Res := @SCommandNameA4;
    $A5: Res := @SCommandNameA5;
    $A6: Res := @SCommandNameA6;
    $A7: Res := @SCommandNameA7;
    $A8: Res := @SCommandNameA8;
    $A9: Res := @SCommandNameA9;
    $AA: Res := @SCommandNameAA;
    $AB: Res := @SCommandNameAB;
    $AC: Res := @SCommandNameAC;
    $AD: Res := @SCommandNameAD;
    $AE: Res := @SCommandNameAE;
    $AF: Res := @SCommandNameAF;
    $B0: Res := @SCommandNameB0;
    $B1: Res := @SCommandNameB1;
    $B2: Res := @SCommandNameB2;
    $B3: Res := @SCommandNameB3;
    $B4: Res := @SCommandNameB4;
    $B5: Res := @SCommandNameB5;
    $B6: Res := @SCommandNameB6;
    $B7: Res := @SCommandNameB7;
    $B8: Res := @SCommandNameB8;
    $B9: Res := @SCommandNameB9;
    $BA: Res := @SCommandNameBA;
    $BB: Res := @SCommandNameBB;
    $BC: Res := @SCommandNameBC;
    $C0: Res := @SCommandNameC0;
    $C1: Res := @SCommandNameC1;
    $C2: Res := @SCommandNameC2;
    $C3: Res := @SCommandNameC3;
    $C4: Res := @SCommandNameC4;
    $C5: Res := @SCommandNameC5;
    $D0: Res := @SCommandNameD0;
    $D1: Res := @SCommandNameD1;
    $E0: Res := @SCommandNameE0;
    $F0: Res := @SCommandNameF0;
    $F1: Res := @SCommandNameF1;
    $F7: Res := @SCommandNameF7;
    $FC: Res := @SCommandNameFC;
    $FD: Res := @SCommandNameFD;
    $FE: Res := @SCommandNameFE;
//    $FE03: Res := @SCommandNameFE03;


    $FF01: Res := @SCommandNameFF01;
    $FF02: Res := @SCommandNameFF02;
    $FF03: Res := @SCommandNameFF03;
    $FF04: Res := @SCommandNameFF04;
    $FF05: Res := @SCommandNameFF05;
    $FF06: Res := @SCommandNameFF06;
    $FF07: Res := @SCommandNameFF07;
    $FF08: Res := @SCommandNameFF08;
    $FF09: Res := @SCommandNameFF09;
    $FF0A: Res := @SCommandNameFF0A;
    $FF0B: Res := @SCommandNameFF0B;
    $FF0C: Res := @SCommandNameFF0C;
    $FF0D: Res := @SCommandNameFF0D;
    $FF30: Res := @SCommandNameFF30;
    $FF31: Res := @SCommandNameFF31;
    $FF32: Res := @SCommandNameFF32;
    $FF33: Res := @SCommandNameFF33;
    $FF34: Res := @SCommandNameFF34;
    $FF35: Res := @SCommandNameFF35;
    $FF36: Res := @SCommandNameFF36;
    $FF37: Res := @SCommandNameFF37;
    $FF38: Res := @SCommandNameFF38;
    $FF39: Res := @SCommandNameFF39;
    $FF3A: Res := @SCommandNameFF3A;
    $FF3B: Res := @SCommandNameFF3B;
    $FF3C: Res := @SCommandNameFF3C;
    $FF3D: Res := @SCommandNameFF3D;
    $FF3E: Res := @SCommandNameFF3E;
    $FF3F: Res := @SCommandNameFF3F;
    $FF40: Res := @SCommandNameFF40;
    $FF41: Res := @SCommandNameFF41;
    $FF42: Res := @SCommandNameFF42;
    $FF43: Res := @SCommandNameFF43;
    $FF45: Res := @SCommandNameFF45;
    $FF46: Res := @SCommandNameFF46;
    $FF47: Res := @SCommandNameFF47;
    $FF48: Res := @SCommandNameFF48;
    $FF49: Res := @SCommandNameFF49;
    $FF4A: Res := @SCommandNameFF4A;
    $FF4B: Res := @SCommandNameFF4B;
    $FF4C: Res := @SCommandNameFF4C;
    $FF4D: Res := @SCommandNameFF4D;
    $FF4E: Res := @SCommandNameFF4E;
    $FF50: Res := @SCommandNameFF50;
    $FF51: Res := @SCommandNameFF51;
    $FF52: Res := @SCommandNameFF52;
    $FF60: Res := @SCommandNameFF60;
    $FF61: Res := @SCommandNameFF61;
    $FF62: Res := @SCommandNameFF62;
    $FF63: Res := @SCommandNameFF63;
    $FF64: Res := @SCommandNameFF64;
    $FF65: Res := @SCommandNameFF65;
    $FF66: Res := @SCommandNameFF66;
    $FF67: Res := @SCommandNameFF67;
    $FF68: Res := @SCommandNameFF68;
    $FF69: Res := @SCommandNameFF69;
    $FF70: Res := @SCommandNameFF70;
    $FF71: Res := @SCommandNameFF71;
    $FF72: Res := @SCommandNameFF72;
    $FF73: Res := @SCommandNameFF73;
    $FF74: Res := @SCommandNameFF74;
    $FF75: Res := @SCommandNameFF75;
  else
    Res := @SNoCommandName;
  end;
  Result := GetRes(Res);
end;

{ TPrinterString }

class function TPrinterString.Convert(const S: string): string;
begin
  { !!! }
end;

resourcestring
  /////////////////////////////////////////////////////////////////////////////
  // Poll 1 description

  SPoll1Description10 = 'После включения';
  SPoll1Description11 = 'После включения с купюрой в валидаторе';
  SPoll1Description12 = 'После включения с купюрой в накопителе';
  SPoll1Description13 = 'Инициализация';
  SPoll1Description14 = 'Ожидание купюры';
  SPoll1Description15 = 'Распознавание купюры';
  SPoll1Description17 = 'Перемещение купюры в накопитель';
  SPoll1Description18 = 'Возврат купюры';
  SPoll1Description19 = 'Модуль отключен';
  SPoll1Description1A = 'Купюра удерживается после команды HOLD';
  SPoll1Description1B = 'Устройство занято. Освобождение через: %d мс';
  SPoll1Description1C = 'Купюра не принята. Причина: ';

  SPoll1Description41 = 'Накопительная кассета заполнена';
  SPoll1Description42 = 'Накопительной кассеты открыта или отсутствует';
  SPoll1Description43 = 'Купюра застряла в приемном пути';
  SPoll1Description44 = 'Купюра застряла в накопительной кассете';
  SPoll1Description45 = 'Попытка мошенничества';
  SPoll1Description46 = 'Пауза. Купюра вставлена прежде, чем обработана предыдущая';

  SPoll1Description80 = 'Купюра во временном хранилище. Тип купюры: %d';
  SPoll1Description81 = 'Купюра принята в хранилище. Тип купюры: %d';
  SPoll1Description82 = 'Купюра возвращена. Тип купюры: %d';
  SPoll1DescriptionUnknown = 'Неизвестное значение Poll1';

  /////////////////////////////////////////////////////////////////////////////
  // Poll 2 description

  // Poll1 = 1C
  SPoll2Description1C60 = 'Ошибка вставки';
  SPoll2Description1C61 = 'Ошибка ферромагнетика';
  SPoll2Description1C62 = 'Купюра осталась в головке';
  SPoll2Description1C63 = 'Ошибка компенсации/множителя'; // ???
  SPoll2Description1C64 = 'Ошибка транспортировки купюры';
  SPoll2Description1C65 = 'Ошибка идентификации купюры';
  SPoll2Description1C66 = 'Ошибка верификации купюры';
  SPoll2Description1C67 = 'Ошибка оптического датчика';
  SPoll2Description1C68 = 'Запрещенный номинал купюры';
  SPoll2Description1C69 = 'Ошибка ёмкости';
  SPoll2Description1C6A = 'Ошибка операции';
  SPoll2Description1C6C = 'Ошибка длины';
  SPoll2Description1CUnknown = 'Неизвестная причина';

  // Poll1 = 47
  SPoll2Description4750 = 'Ошибка двигателя накопительной кассеты';
  SPoll2Description4751 = 'Ошибка скорости транспортного двигателя';
  SPoll2Description4752 = 'Ошибка транспортного двигателя';
  SPoll2Description4753 = 'Ошибка выравнивающего двигателя';
  SPoll2Description4754 = 'Ошибочное начальное состояние накопительной кассеты';
  SPoll2Description4755 = 'Ошибка одного из оптических датчиков';
  SPoll2Description4756 = 'Ошибка индукционного датчика';
  SPoll2Description475F = 'Ошибка датчика ёмкости';
  SPoll2Description47Unknown = 'Неизвестное значение Poll2';

                                                   function PollToDescription(Poll1: Integer; Poll2: Integer): WideString;
begin
  case Poll1 of
    $10: Result := GetRes(@SPoll1Description10);
    $11: Result := GetRes(@SPoll1Description11);
    $12: Result := GetRes(@SPoll1Description12);
    $13: Result := GetRes(@SPoll1Description13);
    $14: Result := GetRes(@SPoll1Description14);
    $15: Result := GetRes(@SPoll1Description15);
    $17: Result := GetRes(@SPoll1Description17);
    $18: Result := GetRes(@SPoll1Description18);
    $19: Result := GetRes(@SPoll1Description19);
    $1A: Result := GetRes(@SPoll1Description1A);
    $1B: Result := Format(GetRes(@SPoll1Description1B), [Poll2 * 100]);
    $1C: begin
           Result := SPoll1Description1C;
           case Poll2 of
             $60: Result := Result + GetRes(@SPoll2Description1C60);
             $61: Result := Result + GetRes(@SPoll2Description1C61);
             $62: Result := Result + GetRes(@SPoll2Description1C62);
             $63: Result := Result + GetRes(@SPoll2Description1C63);
             $64: Result := Result + GetRes(@SPoll2Description1C64);
             $65: Result := Result + GetRes(@SPoll2Description1C65);
             $66: Result := Result + GetRes(@SPoll2Description1C66);
             $67: Result := Result + GetRes(@SPoll2Description1C67);
             $68: Result := Result + GetRes(@SPoll2Description1C68);
             $69: Result := Result + GetRes(@SPoll2Description1C69);
             $6A: Result := Result + GetRes(@SPoll2Description1C6A);
             $6C: Result := Result + GetRes(@SPoll2Description1C6C);
           else
             Result := Result + GetRes(@SPoll2Description1CUnknown);
           end;
         end;
    $41: Result := GetRes(@SPoll1Description41);
    $42: Result := GetRes(@SPoll1Description42);
    $43: Result := GetRes(@SPoll1Description43);
    $44: Result := GetRes(@SPoll1Description44);
    $45: Result := GetRes(@SPoll1Description45);
    $46: Result := GetRes(@SPoll1Description46);
    $47: case Poll2 of
           $50: Result := GetRes(@SPoll2Description4750);
           $51: Result := GetRes(@SPoll2Description4751);
           $52: Result := GetRes(@SPoll2Description4752);
           $53: Result := GetRes(@SPoll2Description4753);
           $54: Result := GetRes(@SPoll2Description4754);
           $55: Result := GetRes(@SPoll2Description4755);
           $56: Result := GetRes(@SPoll2Description4756);
           $5F: Result := GetRes(@SPoll2Description475F);
         else
           Result := GetRes(@SPoll2Description47Unknown);
         end;
    $80: Result := Format(GetRes(@SPoll1Description80), [Poll2]);
    $81: Result := Format(GetRes(@SPoll1Description81), [Poll2]);
    $82: Result := Format(GetRes(@SPoll1Description82), [Poll2]);
  else
    Result := GetRes(@SPoll1DescriptionUnknown);
  end;
end;

{ Получение таймаута выполнения команды }
function GetCommandTimeout(Command: Word): Integer;
begin
  case Command of
    $16: Result := 60000; 	// Технологическое обнуление
    $B2: Result := 20000; 	// Инициализация архива ЭКЛЗ
    $B4: Result := 40000;   // Запрос контрольной ленты ЭКЛЗ
    $B5: Result := 40000;   // Запрос документа ЭКЛЗ
    $B6: Result := 150000;	// Запрос отчёта ЭКЛЗ по отделам в заданном диапазоне дат
    $B7: Result := 150000;	// Запрос отчёта ЭКЛЗ по отделам в заданном диапазоне номеров смен
    $B8: Result := 100000;	// Запрос отчёта ЭКЛЗ по закрытиям смен в заданном диапазоне дат
    $B9: Result := 100000;	// Запрос отчёта ЭКЛЗ по закрытиям смен в заданном диапазоне номеров смен
    $BA: Result := 40000; 	// Запрос в ЭКЛЗ итогов смены по номеру смены
    $40: Result := 30000;   // Суточный отчет без гашения
    $41: Result := 30000;   // Суточный отчет с гашением
    $61: Result := 20000;   // Инициализация ФП
    $62: Result := 30000;   // Запрос суммы записей в ФП
    $66: Result := 35000;   // Фискальный отчет по диапазону дат
    $67: Result := 20000;   // Фискальный отчет по диапазону смен
    $FE: Result := 30000;
    $11: Result := 5000;
    $10: Result := 5000;
    $26: Result := 5000;
    $E0: Result := 40000;
    $FF61: Result := 40000;
    $FF67: Result := 40000;
  else
    Result := 30000;	        // По умолчанию
  end;
end;

end.
