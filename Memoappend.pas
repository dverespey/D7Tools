unit Memoappend;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMemoUpdate = class(TMemo)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
		procedure Append (msg	:	string);

  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NUMMI Tools', [TMemoUpdate]);
end;

//----------------------------------------------------------
//	Append messages to a memo box so the user can see a
//	history as the program executes.
//----------------------------------------------------------
procedure TMemoUpdate.Append (msg	:	string);
var
    pc				:	PChar;
begin
	if (length (msg) = 0) then
		exit;

	while (Lines.Count >= 128) do
		Lines.Delete (0);

	

  pc := PChar (msg);
	while (pc^ <> #0) do
	begin
		if (pc^ < ' ') then
			pc^ := ' ';
		INC (pc);
	end;

	Lines.Append (format ('%s', [msg]));
end;


end.
