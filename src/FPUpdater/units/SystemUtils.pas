unit SystemUtils;

interface

uses
  // VCL
  System.SysUtils, WinAPI.Windows;


function ExecuteProcess(const ExeName: string; const Parameters: string;
  var Output: string): Cardinal;

implementation

function ExecuteProcess(const ExeName: string; const Parameters: string;
  var Output: string): Cardinal;
var
  BytesRead: Cardinal;
  CommandLine: string;
  WorkDir: string;
  Handle: Boolean;
  SI: TStartupInfo;
  SA: TSecurityAttributes;
  PI: TProcessInformation;
  StdOutPipeRead: THandle;
  StdOutPipeWrite: THandle;
  Buffer: array[0..255] of AnsiChar;
begin
  Output := '';
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    WorkDir := ExtractFilePath(ExeName);
    CommandLine := ExeName + ' ' + Parameters;
    Handle := CreateProcess(nil, PChar(CommandLine), nil, nil, True, 0, nil,
      PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if not Handle then
      raise Exception.Create(SysErrorMessage(GetLastError));

    try
      while True do
      begin
        if not ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil) then Break;
        if BytesRead = 0 then Break;
        Buffer[BytesRead] := #0;
        OemToAnsi(@Buffer[0], @Buffer[0]);
        Output := Output + StrPas(Buffer);
      end;
      WaitForSingleObject(PI.hProcess, INFINITE);
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
