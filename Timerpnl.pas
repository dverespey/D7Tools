unit Timerpnl;

{ 
  This unit is a generic base class designed to have various things descended
  from it.  Therefore, it really is useless as is.  All it attempts to be is
  a generic 3D panel with a timer attached to it.  This timer can be used to
  do various things.  Descendants include a flashing alarm indicator and a
  panel clock.

  Author: Gary D. Foster
  Date: 11/6/95

  Change Log:
    * 11/10/95 -- Gary D. Foster
        Published ParentShowHint, TabOrder, TabStop, and OnEnter properties.
        Changed 'Interval' property to 'ClockInterval'
    * 11/9/95 -- Gary D. Foster
        Moved SetEnabled and SetClockInterval procedures from private to
           protected status and added 'virtual'.
    * 11/16/95 -- Gary D. Foster
        The TimerPanel now dynamically creates and destroys the underlying
           ClockDriver (TTimer) object during the SetEnabled procedure.  It
           takes care of storing a pointer to the original OnTimer event if
           you set one, so the children don't need to worry about this.  This
           should make it considerably more resource friendly.
    * 12/4/95 -- Gary D. Foster
         Changed FClockInterval from an Integer to a Word.
}


interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Menus;

type
  TTimerPanel = class(TCustomPanel)
  private
    FClockInterval: Word;
    FClockEnabled: Boolean;
    FTimerEvent: TNotifyEvent;
  protected
    ClockDriver: TTimer;
    procedure SetClockEnabled(Value: Boolean); virtual;
    procedure SetClockInterval(Value: Word); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Color;
    property Ctl3D;
    property Font;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ClockInterval: Word read FClockInterval
                                    write SetClockInterval;
    property Enabled: Boolean read FClockEnabled
                                    write SetClockEnabled default True;
  end;

implementation

procedure TTimerPanel.SetClockInterval(Value: Word);
begin
  FClockInterval := Value;
  if Enabled then
     ClockDriver.Interval := FClockInterval;
end;

procedure TTimerPanel.SetClockEnabled(Value: Boolean);
begin
  If Value = FClockEnabled then exit;
  if Value then
    begin
      ClockDriver := TTimer.Create(Self);
      ClockDriver.Interval := FClockInterval;
      FClockEnabled := True;
      ClockDriver.Enabled := True;
      ClockDriver.OnTimer := FTimerEvent;
    end
  else
    begin
      FClockEnabled := False;
      FTimerEvent := ClockDriver.OnTimer;
      ClockDriver.Free;
    end;
end;

constructor TTimerPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimerEvent := nil;
  SetClockInterval(1000);
  SetClockEnabled(true);
end;

destructor TTimerPanel.Destroy;
begin
  SetClockEnabled(False);
  inherited Destroy;
end;

end.
 