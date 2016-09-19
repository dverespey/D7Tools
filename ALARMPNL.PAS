unit Alarmpnl;

{
  This component is a direct descendant of a TTimerPanel object.  It displays
  a 3D panel that flashes (reverses color and font.color) when enabled, and
  resets to it's normal state on deactivation.  In addition, it has two events:
  OnAlarmActivate is triggered when Enabled is set to true and OnAlarmDeactivate
  is triggered when Enabled is set to false.  These do *not* trigger while in
  design state.

  Copyright (c) November, 1995 Gary D. Foster
  Released under the provisions of the Gnu Public License.

  Author:  Gary D. Foster
  Date: 11/9/95

  Change Log:
     * 11/10/95 -- Gary D. Foster
          Moved FlashLabel, Reset, and SetEnabled from private to protected
             status and made them virtual as appropriate.
     * 11/13/95 -- Gary D. Foster
          Fixed bug in the Reset routine that prevented the original colors
             from being restored.  Basically, if the alarm is saved in the
             'disabled' state (not flashing), the FOriginalColor and
             FOriginalFontColor values are used to restore the colors when
             'reset' is called during object creation.  This means that they
             are both black (0) and therefore the colors are set to black on
             black.  Instead, we bail out of reset if the colors are equal
             (no harm done, since if they're equal a reset is pointless anyway).
     * 11/13/95 -- Gary D. Foster
          Added the 'StringTag' property... designed to hold various information
             that the OnAlarmActivate and Deactivate procedures can reference.
             Specifically, I store DDE Address pointers into Allen Bradley PLC's
             in this property.  That way, with a bank of alarm panels, I only
             have to have one SetAlarm and one ClearAlarm procedure.
     * 11/13/95 -- Gary D. Foster
          Restructured the OnAlarmActivate and Deactivate calls so that they
          are called BEFORE enabling and AFTER disabling the flashing panel.
          Makes it handy for changing colors and such.
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Timerpnl;

type
  TAlarmPanel = class(TTimerPanel)
  private
    FDoneCreating: Boolean;
    FOriginalColor: TColor;
    FOriginalFontColor: TColor;
    FOnAlarmActivate: TNotifyEvent;
    FOnAlarmDeactivate: TNotifyEvent;
    FStringTag: String;
  protected
    procedure FlashLabel(Sender: TObject); virtual;
    procedure Reset; virtual;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetStringTag(Value: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property StringTag: String read FStringTag write SetStringTag;
    property OnAlarmActivate: TNotifyEvent read FOnAlarmActivate
                                           write FOnAlarmActivate;
    property OnAlarmDeactivate: TNotifyEvent read FOnAlarmDeactivate
                                             write FOnAlarmDeactivate;
end;

implementation

procedure TAlarmPanel.SetEnabled(Value: Boolean);
begin
  { Call the inherited... }
  if Enabled = Value then exit;
  inherited SetEnabled(Value);
  { and then save the original colors so we can restore them }
  if value = false then
    begin
      reset;
      if not (csDesigning in ComponentState) and assigned(FOnAlarmDeActivate)then
        FOnAlarmDeActivate(Self);
    end
  else
    begin
      if FDoneCreating then
        begin
          if not (csDesigning in ComponentState) and assigned(FOnAlarmActivate) then
            FOnAlarmActivate(Self);
          FOriginalColor := Color;
          FOriginalFontColor := Font.Color;
        end;
    end;
end;

procedure TAlarmPanel.FlashLabel(Sender: TObject);
var
  foo: TColor;
begin
  if not Enabled then exit;
  foo := Color;
  Color := Font.Color;
  Font.Color := Foo;
end;

constructor TAlarmPanel.Create(AOwner: TComponent);
begin
  FDoneCreating := False;
  inherited Create(AOwner);
  ClockDriver.OnTimer := FlashLabel;
  FDoneCreating := True;
end;

procedure TAlarmPanel.Reset;
begin
  if FOriginalColor = FOriginalFontColor then exit;
  Color := FOriginalColor;
  Font.Color := FOriginalFontColor;
end;

procedure TAlarmPanel.SetStringTag(Value: String);
begin
  FStringTag := Value;
end;

end.
 