//
//     Name:   ThreadedEventPanel.pas
//     Desc:   ThreadedEventPanel Component
//             This component is used as a base class and could be used on its
//             own. It has an imbeded thread which is used to call an event if it
//             set by the user. One use is to fill in the caption of the panel at
//             at a specified interval, using whatever code is in the OnTimer Event.
//
//     Author: David Verespey
//
//     Revision History:
//
//     09/01/96    Start Program
//
//
unit ThreadedEventPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, emptypanel, ThreadedPanel;

type
  TThreadedEventPanel = class(TThreadedPanel)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FOnTimer: TNotifyEvent;
    procedure UpdateTimer; override;
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure Timer; override;
  public
    { Public declarations }
  published
    { Published declarations }
    property OnTimer: TNotifyEvent
      read FOnTimer
      write SetOnTimer;

  end;


implementation

// Set The on timer event function call address
procedure TThreadedEventPanel.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

// Overide the base class update timer procedure to handle the if the OnTimer
// event is assigned
procedure TThreadedEventPanel.UpdateTimer;
begin
	if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
  	begin
      FTimerThread.Resume;
      FTimerThread.Resume;
  	end
  	else
  	begin
  		FTimerThread.Suspend;
  	end;
end;

// Override the Timer function to call the event if enabled
procedure TThreadedEventPanel.Timer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;
end.
