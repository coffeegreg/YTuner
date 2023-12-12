unit threadtimer;

// YTuner: thread timer unit.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type
  TOnTimer = procedure(Sender: TObject);

  TThreadTimer = class(TThread)
  private
    FInterval: Cardinal;
    FOnTimer: TOnTimer;
    FEvent: TEventObject;
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
    procedure TerminateTimer;
    constructor Create(AName: string);
  end;

const
  RB_THREAD = 'RBThread';
  MS_THREAD = 'MSThread';

var
  RBThread, MSThread : TThreadTimer;

implementation

constructor TThreadTimer.Create(AName: string);
begin
  inherited Create(True);  // Suspended = True
  FEvent:=TEventObject.Create(nil,True,False,AName);
  FInterval:=1000;
  FreeOnTerminate:=True;
  FEnabled:=False;
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
      FEvent.WaitFor(FInterval);
      if Terminated then
        Break;
      if FEnabled then
        DoOnTimer;
      FEvent.ResetEvent;
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

procedure TThreadTimer.TerminateTimer;
begin
  StopTimer;
  if Assigned(FEvent) then
    begin
      FEvent.SetEvent;
      FEvent.Free;
    end;
  Terminate;
//  WaitFor;
end;

end.

