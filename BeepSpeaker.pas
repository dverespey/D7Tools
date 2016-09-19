unit BeepSpeaker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls;

type
  TBeepSpeaker = class(TComponent)
  private
    { Private declarations }
    fInterval: integer;
    fEnabled: boolean;
    fTimer: TTimer;
  protected
    { Protected declarations }
    procedure SetInterval(interval: integer); virtual;
    procedure SetEnabled(enabled: boolean); virtual;
    procedure BeepSpeaker(sender: TObject); virtual;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Interval: integer
    read fInterval
    write SetInterval;

    property  Enabled: boolean
    read fEnabled
    write SetEnabled;
  end;

implementation

constructor TBeepSpeaker.Create(Owner: TComponent);
begin
	inherited Create(Owner);
   fTimer:=TTimer.Create(self);
   fTimer.Enabled:=False;
   SetInterval(fTimer.Interval);
   fTimer.OnTimer:=BeepSpeaker;
end;

destructor TBeepSpeaker.Destroy;
begin
	fTimer.Enabled:=False;
   fTimer.Free;
	inherited Destroy;
end;

procedure TBeepSpeaker.SetInterval(interval: integer);
begin
	fInterval:=interval;
   fTimer.Interval:=interval;
end;

procedure TBeepSpeaker.SetEnabled(enabled: boolean);
begin
	fEnabled:=enabled;
   fTimer.Enabled:=enabled;
end;

procedure TBeepSpeaker.BeepSpeaker(sender: TObject);
begin
	Beep;
end;

end.
