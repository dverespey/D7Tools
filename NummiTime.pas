unit NummiTime;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  EMyTimeException = class(Exception);
  TNummiTime = class(TComponent)
  private
    { Private declarations }
    fTime: TDateTime;
    fTimeValue: Extended;
    fNummiTime: String;
    fYear,fMonth,FDay,fHour,fMin,fSec,fMsec: word;
    fDayCount: longint;
    procedure FormatNummiTime;
    procedure FormatTime;
    procedure Decode;
    procedure Encode;
  protected
    { Protected declarations }
    procedure SetTime( value: TDateTime);
    function GetTime: TDateTime;
    procedure SetTimeValue( value: Extended);
    function GetTimeValue: Extended;
    procedure SetNummiTime(value: String);
    function GetNummiTime: String;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    procedure ChangeDay( change: integer);
    procedure ChangeHour( change: integer);
    procedure ChangeMinute( change: integer);
    procedure ChangeSecond( change: integer);
  published
    { Published declarations }
    property Time: TDateTime
    read GetTime
    write SetTime;

    property NummiTime: string
    read GetNummiTime
    write SetNummiTime;

    property TimeValue: extended
    read GetTimeValue
    write SetTimeValue;
  end;


implementation

constructor TNummiTime.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  SetTime(now);
end;

procedure TNummiTime.SetTimeValue( value: Extended);
begin
  fTime:=value;
  fTimeValue:=value;
  Decode;
  FormatNummiTime;
end;

function TNummiTime.GetTimeValue: Extended;
begin
  result:=fTimeValue;
end;


procedure TNummiTime.SetTime( value: TDateTime);
begin
  fTime:=value;
  fTimeValue:=value;
  Decode;
  FormatNummiTime;
end;

function TNummiTime.GetTime: TDateTime;
begin
  result:=fTime;
end;

procedure TNummiTime.SetNummiTime(value: String);
begin
  fNummiTime:=value;
  FormatTime;
end;

function TNummiTime.GetNummiTime: String;
begin
  result:=fNummiTime;
end;

procedure TNummiTime.FormatNummiTime;
begin
  fNummiTime:=FormatDateTime('yyyymmddhhmmss00',ftime);
end;

procedure TNummiTime.FormatTime;
var
  newDT: TDateTime;
begin
  try
    newDT:=StrToDateTime(copy(fNummiTime,5,2)+'/'+copy(fNummiTime,7,2)+'/'+copy(fNummiTime,1,4)+' '+copy(fNummiTime,9,2)+':'+copy(fNummiTime,11,2)+':'+copy(fNummiTime,13,2));
    if newDT <> fTime then
    begin
      fTime:=newDT;
      fTimeValue:=fTime;
      Decode;
    end;
  except
    on e:exception do
    begin
      fTime:=Now;
      fTimeValue:=fTime;
      FormatNummiTime;
      FormatTime;
      raise;
    end;
  end;
end;

procedure TNummiTime.Decode;
begin
  DecodeTime(fTime,fHour,fMin,fSec,fMsec);
  DecodeDate(fTime,fYear,FMonth,FDay);
  fDayCount:=trunc(fTime);
end;

procedure TNummiTime.Encode;
var
  newTime,newDate: TDateTime;
begin
  newTime:=EncodeTime(fHour,fMin,fSec,fMsec);
  newDate:=EncodeDate(fYear,fMonth,fDay);
  fTime:=newDate+newTime;
  FormatNummiTime;
end;

procedure TNummiTime.ChangeDay( change: integer);
var
  newTime: tDateTime;
begin
  fDayCount:=fDayCount+change;
  newTime:=EncodeTime(fHour,fMin,fSec,fMsec);
  fTime:=fDaycount+newTime;
  FormatNummiTime;
end;

procedure TNummiTime.ChangeHour( change: integer);
begin
  if fHour+change >= 24 then
  begin
    if change > 23 then
    begin
      fDayCount:=fDayCount+(change div 24);
      fTime:=fDayCount;
      fHour:=fHour+(change mod 24);
      DecodeDate(fTime,fYear,FMonth,FDay);
      Encode;
    end
    else
    begin
      fDayCount:=fDayCount+1;
      fTime:=fDayCount;
      fHour:=(fHour+change)-24;
      DecodeDate(fTime,fYear,FMonth,FDay);
      Encode;
    end;
  end
  else if fHour+change < 0 then
  begin
    if change < -23 then
    begin
      fDayCount:=fDayCount+(change div 24);
      fTime:=fDayCount;
      DecodeDate(fTime,fYear,FMonth,FDay);
      fHour:=fHour+(change mod 24);
      Encode;
    end
    else
    begin
      fDayCount:=fDayCount-1;
      fTime:=fDayCount;
      fHour:=24+(fHour+change);
      DecodeDate(fTime,fYear,FMonth,FDay);
      Encode;
    end;
  end
  else
  begin
    fHour:=fHour+change;
    Encode;
  end;
end;

procedure TNummiTime.ChangeMinute( change: integer);
begin
end;

procedure TNummiTime.ChangeSecond( change: integer);
begin
end;


end.
