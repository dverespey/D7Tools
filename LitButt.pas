unit Litbutt;

{
  This component is a pushbutton with a twist... it has a light panel imbedded
  into it which toggles between on and off every time the button is pressed.
  It has a slew of properties to control the appearance and behavior of the
  light panel.  It also adds several new events... OnIsLit and OnIsUnlit fire
  whenever the light changes to the appropriate state (the light can be
  controlled seperately from the click event) and the button also fires events
  corresponding to which side of the button was depressed.  A neat trick is to
  set the light position to centered, make it large enough to nearly fill the
  button, and then attach events to the various click events.  This gives you
  a universal control to do all sorts of nifty things with such as controlling
  an audio playback device.

  Copyright (c) November, 1995 Gary D. Foster.
  This code is released under the provisions of the Gnu Public license and
  is freely redistributable.

  Author: Gary D. Foster

  Class Name: TIndicatorButton

  Descends From: TButton

  Exported properties/methods:

   Protected
   ---------
    procedure MouseDown -- overridden
    procedure MouseUp -- overridden
    procedure MouseMove -- overridden
    procedure SetColorLit(Value: TColor); virtual; -- Sets the "lit" color of
            the imbedded light.
    procedure SetColorUnlit(Value: TColor); virtual; -- Sets the "unlit" color
            of the imbedded light.
    procedure SetIsLit(Value: Boolean); virtual; -- Toggles the light status.
    procedure SetLightWidth(Value: Integer); virtual -- Sets the width of the
            imbedded light panel.  It calculates the maximum width based on the
            OffsetX value and trims the value to the maximum if you try to
            exceed it.
    procedure SetLightHeight(Value: Integer); virtual; -- Same as above, except
            affects the Height.
    procedure SetOffsetX(Value: Integer); virtual; -- used to set the OffsetX
            property.
    procedure SetOffsetY(Value: Integer); virtual; -- used to set the OffsetY
            property.
    procedure SetCorner(Value: TCorner); virtual; -- positions the light panel
            in one of five places... allowable values are in the TCorner type.
    procedure SetFixed(Value: Boolean); virtual; -- used to set the
            'LightRidesPanel' property which is kind of hard to explain...
            if this value is set to false, when you depress the button it
            pushes down around the imbedded light, so it looks like the light
            is fixed into place.  Setting this value to true makes it appear
            that the light is attached to the button instead.
    procedure Click -- overridden
    procedure SetCaption(NewCaption: String); virtual;
    function GetCaption: String; virtual; -- these two caption methods are used
            to circumvent a PITA bug that occurred whenever the caption changed.
            Normally, a refresh had to be forced after a caption change because
            the light panel would disappear.  However, if you override the
            caption PROPERTY and call a refresh after it's changed you no
            longer have that problem.  MUCH thanks to Jon Shemitz,
            <jon@armory.com> for the thump in the head that it took to realize
            this.

   Public
   ------
    constructor Create -- overridden;

   Published
   ---------
    property Caption: String; -- Republished to allow us to intercept the
            the caption changing and force a refresh.
    property ColorLit: TColor -- controls the 'lit' color of the light panel.
    property ColorUnlit: TColor -- controls the 'unlit' color of the light.
    property IsLit: Boolean -- Turns the light on and off.  Also toggled in the
            click procedure.
    property LightWidth: Integer -- the width of the embedded light.  Max value
            is (Button.Width - (2*OffsetX)) and is enforced in the SetWidth
            procedure.
    property LightHeight: Integer -- the height of the embedded light.  Max is
            (Button.Height - (2*OffsetY)) and is enforced in the SetHeight
            procedure.
    property LightOffsetX: Integer -- the amount of pixels to offset the light
            along the X axis.  Indents the light from either the left or the
            right side, depending on the corner position so you don't need to
            recalculate if you change the light position.  Ignored if
            LightPosition = lpCenter.
    property LightOffsetY: Integer -- same as the X Offset, only different.
    property LightPosition: TCorner -- determines the sides that the light is
            offset from.  Type TCorner = (lpTopLeft, lpTopRight, lpBottomRight,
                                          lpBottomLeft, lpCenter);
    property LightRidesPanel: Boolean -- determines the runtime behavior and
            appearance of the light.  This is the action controlled by the
            SetFixed procedure.
    property OnLit: TNotifyEvent -- triggered when the light turns on.
    property OnUnlit: TNotifyEvent -- triggered when the light turns off.
    property OnClickTop: TNotifyEvent -- an EXTRA click procedure.
    property OnClickBottom: TNotifyEvent -- another EXTRA click procedure.
    property OnClickRight: TNotifyEvent -- yet another EXTRA click procedure.
    property OnClickLeft: TNotifyEvent -- the last EXTRA click procedure.

    The extra OnClick procedures are triggered depending on which side of the
    button the user clicks on and are fired after the standard OnClick event
    finishes.

}


interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TCorner = (lpTopLeft, lpTopRight, lpBottomRight, lpBottomLeft, lpCenter);
  TIndicatorButton = class(TButton)
  private
     FColorLit: TColor;
     FColorUnlit: TColor;
     FIsLit: Boolean;
     FLightHeight: Integer;
     FLightWidth: Integer;
     FOffsetX: Integer;
     FOffsetY: Integer;
     FCorner: TCorner;
     FOnLit: TNotifyEvent;
     FOnUnlit: TNotifyEvent;
     FFixed: Boolean;
     FOnTopClick: TNotifyEvent;
     FOnBottomClick: TNotifyEvent;
     FOnRightClick: TNotifyEvent;
     FOnLeftClick: TNotifyEvent;
     FMouseX: Integer;
     FMouseY: Integer;
     MBDown: Boolean;
  protected
     Light: TPanel;
     procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
     procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
     procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
     procedure SetColorLit(Value: TColor); virtual;
     procedure SetColorUnlit(Value: TColor); virtual;
     procedure SetIsLit(Value: Boolean); virtual;
     procedure SetLightWidth(Value: Integer); virtual;
     procedure SetLightHeight(Value: Integer); virtual;
     procedure SetOffsetX(Value: Integer); virtual;
     procedure SetOffsetY(Value: Integer); virtual;
     procedure SetCorner(Value: TCorner); virtual;
     procedure SetFixed(Value: Boolean); virtual;
     procedure SetCaption(NewCaption: String); virtual;
     function GetCaption: String; virtual;
  public
     constructor Create(AOwner: TComponent); override;
     procedure Click; override;
  published
     property ColorLit: TColor read FColorLit write SetColorLit default clLime;
     property ColorUnlit: TColor read FColorUnlit write SetColorUnlit default clBtnFace;
     property IsLit: Boolean read FIsLit write SetIsLit default true;
     property LightWidth: Integer read FLightWidth write SetLightWidth;
     property LightHeight: Integer read FLightHeight write SetLightHeight;
     property LightOffsetX: Integer read FOffsetX write SetOffsetX;
     property LightOffsetY: Integer read FOffsetY write SetOffsetY;
     property LightPosition: TCorner read FCorner write SetCorner default lpTopLeft;
     property LightRidesPanel: Boolean read FFixed write SetFixed default false;
     property OnLit: TNotifyEvent read FOnLit write FOnLit;
     property OnUnlit: TNotifyEvent read FOnUnlit write FOnUnlit;
     property OnClickTop: TNotifyEvent read FOnTopClick write FOnTopClick;
     property OnClickBottom: TNotifyEvent read FOnBottomClick write FOnBottomClick;
     property OnClickRight: TNotifyEvent read FOnRightClick write FOnRightClick;
     property OnClickLeft: TNotifyEvent read FOnLeftClick write FOnLeftClick;
     property Caption: String read GetCaption write SetCaption;
  end;

implementation

procedure TIndicatorButton.SetCaption(NewCaption: String);
begin
   inherited Caption := NewCaption;
   refresh;
end;

function TIndicatorButton.GetCaption: String;
begin
   result := inherited Caption;
end;

procedure TIndicatorButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MBDown := True;
   FMouseX := X;
   FMouseY := Y;
   if not FFixed then Light.BevelOuter := BvRaised;
   Light.Refresh;
   inherited MouseDown(Button, Shift, X, Y);
end;

procedure TIndicatorButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MBDown := False;
   Light.BevelOuter := BvLowered;
   Light.Refresh;
   inherited MouseUp(Button, Shift, X, Y);
end;

procedure TIndicatorButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
   If MBDown then
     begin
       FMouseX := X;
       FMouseY := Y;
     end;
   If MBDown and ((X < 0) or (Y < 0) or (X > Width) or (Y > Height)) then
      Light.BevelOuter := bvLowered;
   If MBDown and (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) and not FFixed then
      Light.BevelOuter := bvRaised;
   Light.Refresh;
   inherited MouseMove(Shift, X, Y);
end;

procedure TIndicatorButton.SetOffsetX(Value: Integer);
var
  Max: Integer;
begin
   FOffsetX := Value;
   Max := Width - Light.Width;
   if Value > Max then Value := Max;
   case FCorner of
      lpTopLeft: Light.Left := Value;
      lpBottomLeft: Light.Left := Value;
      lpTopRight: Light.Left := Width - (Light.Width + Value);
      lpBottomRight: Light.Left := Width - (Light.Width + Value);
      lpCenter: Light.Left := (Width div 2) - (Light.Width div 2);
   end;
end;

procedure TIndicatorButton.SetOffsetY(Value: Integer);
var
   Max: Integer;
begin
   FOffsetY := Value;
   Max := Height - Light.Height;
   If Value > Max then Value := Max;
   case FCorner of
      lpTopLeft: Light.Top := Value;
      lpBottomLeft: Light.Top := Height - (Light.Height + Value);
      lpTopRight: Light.Top := Value;
      lpBottomRight: Light.Top := Height - (Light.Height + Value);
      lpCenter: Light.Top := (Height div 2) - (Light.Height div 2);
   end;
end;

procedure TIndicatorButton.SetCorner(Value: TCorner);
begin
   if Value = FCorner then exit;
   FCorner := Value;
   SetOffsetX(FOffsetX);
   SetOffsetY(FOffsetY);
end;

procedure TIndicatorButton.SetLightHeight(Value: Integer);
var
   Max: Integer;
begin
   If FLightHeight = Value then exit;
   Max := Height - (2 * LightOffsetY);
   if Value > Max then Value := Max;
   FLightHeight := Value;
   Light.Height := Value;
   SetOffsetY(FOffsetY);
end;

procedure TIndicatorButton.SetLightWidth(Value: Integer);
var
   Max: Integer;
begin
   If FLightWidth = Value then exit;
   Max := Width - (2 * LightOffsetX);
   if Value > Max then Value := Max;
   FLightWidth := Value;
   Light.Width := Value;
   SetOffsetX(FOffsetX);
end;

procedure TIndicatorButton.SetFixed(Value: Boolean);
begin
   if FFixed = value then exit;
   FFixed := Value;

   if MBDown then
      if FFixed then
         Light.BevelOuter := bvLowered 
      else
         Light.BevelOuter := bvRaised;
end;

constructor TIndicatorButton.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   SetFixed(False);
   Light := TPanel.Create(Self);
   Light.Parent := Self;
   SetCorner(lpTopLeft);
   SetOffsetX(8);
   SetOffsetY(8);
   SetLightHeight(Height div 3);
   SetLightWidth(Width div 4);
   Light.BevelOuter := bvLowered;
   Light.BevelWidth := 1;
   SetColorLit(clLime);
   SetColorUnlit(clBtnFace);
   SetIsLit(True);
end;

procedure TIndicatorButton.SetColorLit(Value: TColor);
begin
   If FColorLit = Value then exit;
   FColorLit := Value;
   if FIsLit then Light.Color := Value;
end;

procedure TIndicatorButton.SetColorUnlit(Value: TColor);
begin
   If FColorUnlit = Value then exit;
   FColorUnlit := Value;
   If not FIsLit then Light.Color := Value;
end;

procedure TIndicatorButton.Click;
var
  Pos: Integer;
begin
  Pos := 0; { initialize the click position }
  Light.BevelOuter := bvLowered;
  SetIsLit(Not FIsLit);
  inherited Click;
  { Now figure out where they were when they clicked }

  { Top: }
  if (FMouseX >= 0) and (FMouseX <= Width) and (FMouseY >= 0) and (FMouseY < Light.Top) then Pos := 1;
  { Bottom: }
  If (FMouseX >= 0) and (FMouseX <= Width) and (FMouseY > Light.Height) and (FMouseY <= Height) then Pos := 2;
  { Left Side: }
  If (FMouseX >= 0) and (FMouseX < FOffsetX) and (FMouseY >= 0) and (FMouseY <= Height) then Pos := 3;
  { Right Side: }
  If (FMouseX > (Width - FOffsetX)) and (FMouseX <= Width) and (FMouseY >= 0) and (FMouseY <= Height) then Pos := 4;

  case Pos of
     1: If assigned(FOnTopClick) then FOnTopClick(Self);
     2: If assigned(FOnBottomClick) then FOnBottomClick(Self);
     3: If assigned(FOnLeftClick) then FOnLeftClick(Self);
     4: If assigned(FOnRightClick) then FOnRightClick(Self);
  end;
end;

procedure TIndicatorButton.SetIsLit(Value: Boolean);
begin
  If FIsLit = Value then exit;
  FIsLit := Value;
  If FIsLit then
     Light.Color := ColorLit
  else
     Light.Color := ColorUnlit;
  if not (csDesigning in ComponentState) then
  case FIsLit of
     True: if Assigned(FOnLit) then FOnLit(Self);
     False: if Assigned(FOnUnlit) then FOnUnlit(Self);
  end;
end;

end.

