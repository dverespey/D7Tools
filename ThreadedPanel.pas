//
//     Name:   ThreadedPanel.pas
//     Desc:   ThreadedPanel Component
//             A base class which can derived off. A panel with an associated thread
//             attached. If the Thread is enabled, the Timer function is called, at a
//             given interval. This componenet replaces any need for use of the TTimer
//             component.
//
//     Author:  David Verespey
//
//     Revision History:
//
//     09/01/96    Start Program
//
//
unit ThreadedPanel;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, EmptyPanel;

type
  TThreadedPanel = class;

  TTimerThread = class(TThread)
    OwnerTimer: TThreadedPanel;
    procedure Execute; override;
    procedure DoTimer;
  end;

  TThreadedPanel = class(TEmptyPanel)
  private
  protected
   // Keep everything protected so that any derived object can also see these
    FEnabled: Boolean;
    FInterval: Word;
    FTimerThread: TTimerThread;
    FThreadPriority: TThreadPriority;
    procedure UpdateTimer; virtual;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetInterval(Value: Word);
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure Timer; dynamic; // This is the function to override to get the panel to do somthing
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean
      read FEnabled
      write SetEnabled;

    property Interval: Word
      read FInterval
      write SetInterval;

    property ThreadPriority: TThreadPriority
      read FThreadPriority
      write SetThreadPriority;
  end;


implementation

// Actual thread
procedure TTimerThread.Execute;
begin
  Priority := OwnerTimer.FThreadPriority;
  FreeOnTerminate:=True;
  repeat
    if not terminated then
      Sleep(OwnerTimer.FInterval);
    if not terminated then
      Synchronize(DoTimer);
  until Terminated;
end;

// call back to panel
procedure TTimerThread.DoTimer;
begin
  OwnerTimer.Timer;
end;

// Create the thread here
constructor TThreadedPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;
  FThreadPriority := tpNormal;
  FTimerThread := TTimerThread.Create(True);
  FTimerThread.FreeOnTerminate:=True;
  FTimerThread.OwnerTimer := Self;
end;

// Clean up and delete the thread
destructor TThreadedPanel.Destroy;
begin
  fTimerThread.Terminate;
  inherited Destroy;
end;

// Start of stop the thread
procedure TThreadedPanel.UpdateTimer;
begin
  if (FInterval <> 0) and FEnabled {and Assigned(FOnTimer)} then
  begin
    FTimerThread.Resume;
  end
  else
  begin
    FTimerThread.Suspend;
  end;
end;

// Set enabled or not
procedure TThreadedPanel.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  UpdateTimer;
end;

// How often the thread should execute
procedure TThreadedPanel.SetInterval(Value: Word);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

// threads priority
procedure TThreadedPanel.SetThreadPriority(Value: TThreadPriority);
begin
  if Value <> FThreadPriority then
  begin
    FThreadPriority := Value;
    UpdateTimer;
  end;
end;

// The function to override this bas class does nothing
procedure TThreadedPanel.Timer;
begin
end;

end.
