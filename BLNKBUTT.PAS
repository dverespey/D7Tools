unit Blnkbutt;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Litbutt;

type
  TBlinkingIndicatorButton = class(TIndicatorButton)
  private
    FBlinking: Boolean;
    FInterval: word;
    FOriginalCaption: String;
    ClockDriver: TTimer;
  protected
    procedure SetBlinking(Value: Boolean); virtual;
    procedure BlinkCaption(Sender: TObject); virtual;
    procedure SetInterval(Value: word); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Interval: word read FInterval
                            write SetInterval
                            default 1000;
    property Blinking: Boolean read FBlinking
                               write SetBlinking
                               default False;
  end;

implementation

procedure TBlinkingIndicatorButton.SetBlinking(Value: Boolean);
begin
   if (value = FBlinking) then exit;
   FBlinking := Value;
   if Value then
      begin
         FOriginalCaption := Caption;
         ClockDriver := TTimer.Create(Self);
         SetInterval(FInterval);
         ClockDriver.OnTimer := BlinkCaption;
       end
   else
      begin
         ClockDriver.Free;
         SetCaption(FOriginalCaption);
      end;
end;

procedure TBlinkingIndicatorButton.SetInterval(Value: word);
begin
   if (Value = FInterval) then exit;
   FInterval := Value;
   if FBlinking then ClockDriver.Interval := FInterval;
end;

procedure TBlinkingIndicatorButton.BlinkCaption(Sender: TObject);
begin
   If Caption = FOriginalCaption then
      Caption := ''
   else
      Caption := FOriginalCaption;
end;

constructor TBlinkingIndicatorButton.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   SetInterval(1000);
   SetBlinking(False);
end;

end.
