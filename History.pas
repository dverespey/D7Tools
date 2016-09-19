unit History;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  THistory = class(TMemo)
  private
    { Private declarations }
    fShowDate: boolean;
    fShowTime: boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
		procedure Append (msg	:	string);

  published
    { Published declarations }
    property ShowDate: boolean
    read fShowDate
    write fShowDate;
    property ShowTime: boolean
    read fShowtime
    write fShowTime;
  end;


implementation


//----------------------------------------------------------
//	Append messages to a memo box so the user can see a
//	history as the program executes.
//----------------------------------------------------------
procedure THistory.Append (msg	:	string);
var
  stamp1		:	string [32];
  stamp2		:	string [ 3];
  curtime		:	TDateTime;
	curhh			:	word;
	curmm			:	word;
	curss			:	word;
  curms			:	word;
  pc				:	PChar;
begin
	if (length (msg) = 0) then
		exit;

	while (Lines.Count >= 128) do
		Lines.Delete (0);

	curTime := Now;
	DecodeTime (curtime, curhh, curmm, curss, curms);

  stamp1:='';
  if fShowDate then
  begin
	  stamp1 := formatDateTime ('mm/dd/yyyy ', curTime);
  end;
  if FShowtime then
  begin
    if stamp1 <> '' then
	    stamp1 := stamp1+formatDateTime (' hh:nn:ss', curTime)
    else
	    stamp1 := formatDateTime ('hh:nn:ss', curTime);
  end;
	//stamp1 := formatDateTime ('mm/dd/yyyy hh:nn:ss', curTime);

	str (curms:3, stamp2);
	if (stamp2 [1] = ' ') then stamp2 [1] := '0';
	if (stamp2 [2] = ' ') then stamp2 [2] := '0';
	if (stamp2 [3] = ' ') then stamp2 [3] := '0';

  pc := PChar (msg);
	while (pc^ <> #0) do
	begin
		if (pc^ < ' ') then
			pc^ := ' ';
		INC (pc);
	end;

	Lines.Append (format ('%s.%s, %s', [stamp1, stamp2, msg]));
end;



end.
