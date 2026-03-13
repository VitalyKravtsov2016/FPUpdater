unit SystemUtils;

interface

uses
  // VCL
  System.SysUtils, WinAPI.Windows, AnsiStrings,
  // This
  LogFile;

function ExecuteProcess(const ExeName: string; const Parameters: string;
  var Output: string): Cardinal;

implementation

// Вспомогательная функция для чтения доступных данных
procedure ReadPipeData(Pipe: THandle; var Output: string;
  WaitForData: Boolean = False; TimeoutMs: DWORD = 5000);
var
  BytesRead: Cardinal;
  Buffer: array[0..4095] of AnsiChar;
  TotalOutput: AnsiString;
  BytesAvailable: DWORD;
  StartTime: DWORD;
begin
  TotalOutput := '';
  StartTime := GetTickCount;

  repeat
    // Проверяем таймаут
    if GetTickCount - StartTime > TimeoutMs then
      Break;

    // Проверяем наличие данных
    if not WaitForData then
    begin
      if not PeekNamedPipe(Pipe, nil, 0, nil, @BytesAvailable, nil) then
        Break;
      if BytesAvailable = 0 then
      begin
        Sleep(10); // Не грузим процессор
        Continue;
      end;
    end;

    // Читаем данные
    if ReadFile(Pipe, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) then
    begin
      if BytesRead > 0 then
      begin
        Buffer[BytesRead] := #0;
        OemToAnsi(@Buffer[0], @Buffer[0]);
        TotalOutput := TotalOutput + AnsiStrings.StrPas(Buffer);
      end;
    end else
    begin
      Break;
    end;

    // Если не ждем данные, выходим после первого чтения
    if not WaitForData then
      Break;

  until BytesRead = 0;

  Output := Output + String(TotalOutput);
end;

function ExecuteProcess(const ExeName: string; const Parameters: string;
  var Output: string): Cardinal;
var
  CommandLine: string;
  WorkDir: string;
  Handle: Boolean;
  SI: TStartupInfo;
  SA: TSecurityAttributes;
  PI: TProcessInformation;
  StdOutPipeRead: THandle;
  StdOutPipeWrite: THandle;
begin
  Output := '';

  // Настройка безопасности для наследования дескрипторов
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  // Создаем пайп для чтения вывода
  if not CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0) then
  begin
    //Logger.Debug('Failed CreatePipe');
    RaiseLastOSError;
  end;

  try
    // Настраиваем запуск процесса
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    WorkDir := ExtractFilePath(ExeName);
    CommandLine := '"' + ExeName + '"';
    if Parameters <> '' then
      CommandLine := CommandLine + ' ' + Parameters;

    // Запускаем процесс
    Handle := CreateProcess(PChar(ExeName), PChar(CommandLine), nil, nil, True,
      CREATE_NO_WINDOW, nil, PChar(WorkDir), SI, PI);

    // Закрываем нашу копию трубы для записи
    CloseHandle(StdOutPipeWrite);

    if not Handle then
    begin
      //Logger.Debug('Failed CreateProcess');
      raise Exception.Create(SysErrorMessage(GetLastError));
    end;

    try
      // Читаем вывод пока процесс работает
      while WaitForSingleObject(PI.hProcess, 100) = WAIT_TIMEOUT do
      begin
        ReadPipeData(StdOutPipeRead, Output);
      end;

      // Финальное чтение после завершения процесса
      ReadPipeData(StdOutPipeRead, Output, True);

      // Получаем код возврата
      GetExitCodeProcess(PI.hProcess, Result);

    finally
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;

  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

end.
