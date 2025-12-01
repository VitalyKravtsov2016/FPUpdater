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
  public
    constructor CreateThread(AOnExecute: TNotifyEvent);
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  published
    property Terminated;
  end;

implementation

{ TNotifyThread }

constructor TNotifyThread.CreateThread(AOnExecute: TNotifyEvent);
begin
  inherited Create(True);
  FOnExecute := AOnExecute;
end;

procedure TNotifyThread.Execute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

end.
