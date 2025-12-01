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

implementation

{ TNotifyThread }

constructor TNotifyThread.CreateThread(AOnExecute: TNotifyEvent);
begin
  inherited Create(True);
  FOnExecute := AOnExecute;
  Resume;
end;

procedure TNotifyThread.Execute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;


end.
