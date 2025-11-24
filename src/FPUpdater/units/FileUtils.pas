unit FileUtils;

interface

uses
  // VCL
  Windows, Classes, SysUtils, ShlObj, ShFolder;

function GetModulePath: string;
function GetModuleFileName: string;
function ReadFileData(const FileName: string): AnsiString;
procedure WriteFileData(const FileName, Data: AnsiString);
function GetLongFileName(const FileName: string): string;
function GetSystemPath: string;
function GetSpecialFolderPath(Folder : integer) : string;
function GetLocalAppDataPath: string;
function GetCommonAppDataPath: string;
procedure WriteAppendFileData(const FileName, Data: AnsiString);
function FileOpenAsReadOnly(const sFileName: String): THandle;

implementation

function GetSpecialFolderPath(Folder : integer) : string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  Path: array [0..MAX_PATH] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0, Folder, 0, SHGFP_TYPE_CURRENT, @Path[0])) then
    Result := Path
  else
    Result := '';
end;

function GetLocalAppDataPath: string;
begin
  Result := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA);
end;

function GetCommonAppDataPath: string;
begin
  Result := GetSpecialFolderPath(CSIDL_COMMON_APPDATA);
end;

function GetModulePath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(
    GetLongFileName(GetModuleFileName)));
end;

function GetModuleFileName: string;
var
  Buffer: array[0..261] of Char;
begin
  SetString(Result, Buffer, Windows.GetModuleFileName(HInstance,
    Buffer, SizeOf(Buffer)));
end;

function ReadFileData(const FileName: string): AnsiString;
var
  Stream: TFileStream;
begin
  Result := '';
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    if Stream.Size > 0 then
    begin
      SetLength(Result, Stream.Size);
      Stream.Read(Result[1], Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

procedure WriteFileData(const FileName, Data: Ansistring);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Data) > 0 then
      Stream.Write(Data[1], Length(Data));
  finally
    Stream.Free;
  end;
end;


procedure WriteAppendFileData(const FileName, Data: AnsiString);
var
  Stream: TFileStream;
begin
  if not FileExists(FileName) then Exit;
  Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    Stream.Seek(0, soFromEnd);
    if Length(Data) > 0 then
      Stream.Write(Data[1], Length(Data));
  finally
    Stream.Free;
  end;
end;

function GetLongFileName(const FileName: string): string;
var
  L: Integer;
  Handle: Integer;
  Buffer: array[0..MAX_PATH] of Char;
  GetLongPathName: function (ShortPathName: PChar; LongPathName: PChar;
    cchBuffer: Integer): Integer stdcall;
const
  kernel = 'kernel32.dll';
begin
  Result := FileName;
  Handle := GetModuleHandle(kernel);
  if Handle <> 0 then
  begin
    @GetLongPathName := GetProcAddress(Handle, 'GetLongPathNameA');
    if Assigned(GetLongPathName) then
    begin
      L := GetLongPathName(PChar(FileName), Buffer, SizeOf(Buffer));
      SetString(Result, Buffer, L);
    end;
  end;
end;

function GetSystemPath: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  Result := '';
  SHGetSpecialFolderPath(0, Buffer, CSIDL_SYSTEM, False);
  Result := IncludeTrailingPathDelimiter(Buffer);
end;

function FileOpenAsReadOnly(const sFileName: String): THandle;
begin
  Result := CreateFile(PChar(sFileName),
                       GENERIC_READ,
                       FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                       nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Result = INVALID_HANDLE_VALUE then
    raise EFOpenError.CreateFmt('Ошибка открытия файла: %s, %s',
                                   [ExpandFileName(sFileName),
                                   SysErrorMessage(GetLastError)])
end;

end.
