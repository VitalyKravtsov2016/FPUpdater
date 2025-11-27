unit NotifyThread;

interface

uses
  // VCL
  Classes;

type
  { TNotifyThread }

  TNotifyThread = class(TThread)
  private
    FOnExecute: TNotifyEvent;
  protected
    procedure Execute; override;
  published
    constructor CreateThread(AOnExecute: TNotifyEvent);
    property Terminated;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

function AsyncAwait(AOnExecuteMethod: TNotifyEvent;
                    AOnTerminateMethod: TNotifyEvent): TThread;

function AsyncAwait2(AOnExecuteMethod: TNotifyEvent;
                    AOnTerminateMethod: TNotifyEvent): TThread;

implementation

{ TNotifyThread }

constructor TNotifyThread.CreateThread(AOnExecute: TNotifyEvent);
begin
  FOnExecute := AOnExecute;
  inherited Create(False);
end;

procedure TNotifyThread.Execute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;


function AsyncAwait(AOnExecuteMethod: TNotifyEvent;
                    AOnTerminateMethod: TNotifyEvent): TThread;
var
  Thread: TNotifyThread;
begin
  Thread := TNotifyThread.CreateThread(AOnExecuteMethod);
  Thread.OnTerminate := AOnTerminateMethod;
  Thread.FreeOnTerminate := False;
  Thread.Resume;
  Result := Thread;
end;

function AsyncAwait2(AOnExecuteMethod: TNotifyEvent;
                    AOnTerminateMethod: TNotifyEvent): TThread;
var
  Thread: TNotifyThread;
begin
  Thread := TNotifyThread.CreateThread(AOnExecuteMethod);
  Thread.OnTerminate := AOnTerminateMethod;
  Thread.FreeOnTerminate := True;
  Thread.Resume;
  Result := Thread;
end;


end.
