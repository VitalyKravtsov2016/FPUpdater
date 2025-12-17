unit LangUtils;

interface

uses
  // VCL
  Windows, SysUtils, Classes, Registry, IniFiles,
  // gnugettext
  gnugettext,
  // This
  DriverTypes;


function GetLanguage: string;
procedure SetLanguage(const Language: string);
function GetRes(Value: PResStringRec): WideString;
function GetLanguageParamsFileName: string;


implementation
var
  GLanguage: string = '';

const
  LangParamsFileName = 'locale.ini';


function GetLanguageParamsFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserShtrihPath) + LangParamsFileName;
end;

function GetRes(Value: PResStringRec): WideString;
begin
  Result := LoadResStringW(Value);
end;

function GetModuleFileName: string;
var
  Buffer: array[0..261] of Char;
begin
  SetString(Result, Buffer, Windows.GetModuleFileName(HInstance,
    Buffer, SizeOf(Buffer)));
end;

function GetLanguage: string;
var
  F: TIniFile;
begin
  if GLanguage <> '' then
  begin
    Result := GLanguage;
    Exit;
  end;
  if FileExists(GetLanguageParamsFileName) then
  begin
    F := TIniFile.Create(GetLanguageParamsFileName);
    try
      Result := F.ReadString('Locale', 'Lang', 'RU');
    finally
      F.Free;
    end;
    if (Result <> 'RU') and (Result <> 'EN') then
      Result := 'RU';
  end
  else
    Result := 'RU';
  GLanguage := Result;
end;

procedure SetLanguage(const Language: string);
var
  F: TIniFile;
begin
  F := TIniFile.Create(GetLanguageParamsFileName);
  try
  finally
    F.WriteString('Locale', 'Lang', Language);
  end;
end;

end.
