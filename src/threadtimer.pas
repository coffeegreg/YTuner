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
    FName: string;
    FInterval: Cardinal;
    FOnTimer: TOnTimer;
    FEvent: TEventObject;
    FEnabled: Boolean;
    FProcessing: Boolean;
    FNeeded: Boolean;
    procedure DoOnTimer;
  protected
    procedure Execute; override;
  public
    property OnTimer: TOnTimer read FOnTimer write FOnTimer;
    property Interval: Cardinal read FInterval write FInterval;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Processing: Boolean read FProcessing write FProcessing;
    property Needed: Boolean read FNeeded write FNeeded;
    procedure StopTimer;
    procedure StartTimer;
    procedure TerminateTimer;
    constructor Create(AName: string);
  end;

const
  RB_THREAD = 'RBThread';
  MS_THREAD = 'MSThread';
  GT_THREAD = 'GTThread';

var
  RBThread, MSThread : TThreadTimer;
  GTThread : TThreadTimer = nil;

implementation

constructor TThreadTimer.Create(AName: string);
begin
  inherited Create(True);  // Suspended = True
  FName:=AName;
  FEvent:=TEventObject.Create(nil,True,False,AName);
  FInterval:=1000;
  FreeOnTerminate:=True;
  FEnabled:=False;
  FProcessing:=False;
  FNeeded:=False;
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
      if Assigned(FEvent) then
        FEvent.WaitFor(FInterval);
      if Terminated then
        begin
          if Assigned(FEvent) then
            FEvent.Free;
          Break;
        end;
      if FEnabled and (not FProcessing) then
        DoOnTimer;
      if Assigned(FEvent) then
        FEvent.ResetEvent;
    end;
end;

procedure TThreadTimer.StopTimer;
begin
  FEnabled:=False;
//  Self.Suspended:=True;       // Not suitable for *nix.
end;

procedure TThreadTimer.StartTimer;
begin
  FEnabled:=True;
  if Self.Suspended then Start;
end;

procedure TThreadTimer.TerminateTimer;
begin
  FEnabled:=False;
  if Assigned(FEvent) then
    begin
      FEvent.SetEvent;
      FEvent.Free;
      FEvent:=nil;
    end;
  OnTerminate:=nil;
  Terminate;
//  WaitFor;
end;

end.

