unit threadtimer;

// YTuner: thread timer unit.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOnTimer = procedure(Sender: TObject) of object;

  TThreadTimer = class(TThread)
  private
    FInterval: Cardinal;
    FOnTimer: TOnTimer;
    FEvent: PRTLEvent;
    FEnabled: Boolean;
    procedure DoOnTimer;
  protected
    procedure Execute; override;
  public
    property OnTimer: TOnTimer read FOnTimer write FOnTimer;
    property Interval: Cardinal read FInterval write FInterval;
    property Enabled: Boolean read FEnabled write FEnabled;
    procedure StopTimer;
    procedure StartTimer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TThreadTimer.Create;
begin
  inherited Create(True);  // Suspended = True
  FEvent:=RTLEventCreate;
  FInterval:=1000;
  FreeOnTerminate:=True;
  FEnabled:=False;
end;

destructor TThreadTimer.Destroy;
begin
  RTLeventdestroy(FEvent);
  inherited Destroy;
end;

procedure TThreadTimer.DoOnTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TThreadTimer.Execute;
begin
  while not Terminated do
    begin
      RTLeventWaitFor(FEvent,FInterval);
      if Terminated then
        Break;
      if FEnabled then
        DoOnTimer;
      RTLeventResetEvent(FEvent);
    end;
end;

procedure TThreadTimer.StopTimer;
begin
  FEnabled:=False;
end;

procedure TThreadTimer.StartTimer;
begin
  FEnabled:=True;
  if Self.Suspended then Start;
end;

end.

