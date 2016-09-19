unit Panelclk;

{
  This is just a simple Panel Clock.  It is descended from type TTimerPanel,
  and on each timer tick, if enabled, updates it's caption to reflect the
  current time, displayed according to the value of the DisplayMask.  Caption
  is not published (it is protected in the base class) so that it can't be
  fiddled with by external means.

  Credit where credit is due:  The idea for this component came from a training
  course on Delphi given to me by an outside company that I can't recall the
  name of (I'll gladly credit them if I can remember their names).  The basic
  functionality existed in an object that we created during the class.  I
  recoded it however, to descend from my TTimerPanel base class instead, and
  recoded and supplemented the functionality.  The idea and original
  implementation is theirs.  The actual implementation and specific code is
  mine.  Anyone that wants to challenge the legal rights of this is more than
  welcome to try and get 50% of the royalties I receive from this object.  Since
  it's being released by me free of charge, be aware that 50% of nothing means
  a lot of legal bills. :)

  Released into the public domain, November 1995.

  Author: Gary D. Foster
  Date: 11/9/95

  Change Log:
    * 11/10/95 -- Gary D. Foster
         Moved DisplayTime and SetDisplayMask from private to protected status
            and changed them to virtual.
    
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Timerpnl;

type
  TPanelClock = class(TTimerPanel)
  private
    FDisplayMask: String;
  protected
    procedure DisplayTime(Sender: TObject); virtual;
    procedure SetDisplayMask(Value: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DisplayMask: String read FDisplayMask write SetDisplayMask;
  end;

implementation

procedure TPanelClock.DisplayTime(Sender: TObject);
begin
  if Enabled then Caption := FormatDateTime(FDisplayMask, Now);
end;

procedure TPanelClock.SetDisplayMask(Value: String);
begin
   FDisplayMask := Value;
end;

constructor TPanelClock.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   SetDisplayMask('hh:mm:ss');
   ClockDriver.OnTimer := DisplayTime;
end;

end.
 