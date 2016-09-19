unit Seqnum;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

type
  TSequenceNumber = class(TComponent)
  private
    { Private declarations }
    fSequenceValue: longint;
    fRollOver: boolean;
    fWarningBeforeOffset: integer;
    fWarningAfterOffset: integer;
  protected
    { Protected declarations }
    function  GetSequence: string;
    procedure SetSequence(value: longint);
    function  GetValue: longint;
    procedure SetValue(sequence: string);
    function AddZeros:string;
    function GetRollover: boolean;
    procedure SetRollover(RollOver: boolean);
    function GetWarningBeforeOffset: integer;
    procedure SetWarningBeforeOffset(WarningBeforeOffset: integer);
    function GetWarningAfterOffset: integer;
    procedure SetWarningAfterOffset(WarningAfterOffset: integer);
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    procedure Increment; virtual;
    procedure Decrement; virtual;
    function CountWillRoll(count: integer): boolean;
  published
    { Published declarations }
    property SequenceValue: longint
    read GetValue
    write SetSequence;

    property SequenceNumber: string
    read GetSequence
    write SetValue;

    property RolloverWarning: boolean
    read GetRollover
    write SetRollover;

    property WarningBeforeOffset: integer
    read GetWarningBeforeOffset
    write SetWarningBeforeOffset;

    property WarningAfterOffset: integer
    read GetWarningAfterOffset
    write SetWarningAfterOffset;

  end;


implementation

constructor TSequenceNumber.Create(Owner: TComponent);
begin
     inherited Create(Owner);
     fSequenceValue:=1;
end;

procedure TSequenceNumber.Increment;
begin
     Inc(fSequenceValue);
     if fSequenceValue > 9999 then
        fSequenceValue:=1;
end;

procedure TSequenceNumber.Decrement;
begin
     Dec(fSequenceValue);
     if fSequenceValue < 1 then
        fSequenceValue:=9999;
end;

function TSequenceNumber.CountWillRoll(count: integer): boolean;
begin
	if count+fSequenceValue>9999 then
   	result:=true
   else
   	result:=false;
end;

function TSequenceNumber.GetRollover: boolean;
begin
	result:=fRollover;
end;

procedure TSequenceNumber.SetRollover(RollOver: boolean);
begin
	fRollover:=Rollover;
end;

function TSequenceNumber.GetWarningBeforeOffset: integer;
begin
	Result:=fWarningBeforeOffset;
end;

procedure TSequenceNumber.SetWarningBeforeOffset(WarningBeforeOffset: integer);
begin
	fWarningBeforeOffset:=WarningBeforeOffset;
end;

function TSequenceNumber.GetWarningAfterOffset: integer;
begin
	Result:=fWarningAfterOffset;
end;

procedure TSequenceNumber.SetWarningAfterOffset(WarningAfterOffset: integer);
begin
	fWarningAfterOffset:=WarningAfterOffset;
end;

function TSequenceNumber.GetValue: longint;
begin
     result:=fSequenceValue;
end;

procedure TSequenceNumber.SetSequence(value: longint);
begin
     if (value>9999) or (value<1) then
        value:=1;
     fSequenceValue:=value;
end;

function TSequenceNumber.GetSequence: string;
begin
     result:=AddZeros;
end;

procedure TSequenceNumber.SetValue(sequence: string);
begin
	if length(sequence) > 4 then
   begin
   	fSequenceValue:=1;
   end
   else
   begin
   	try
   		if (StrToInt(sequence)<1) or (StrToInt(sequence)>9999) then
       	begin
       		fSequenceValue:=1;
       	end
       	else
       	begin
       		fSequenceValue:=StrToInt(sequence);
       	end;
       except
       	fSequenceValue:=1;
       end;
   end;
end;

function TSequenceNumber.AddZeros:string;
var
   s, data: string;
   i,j: integer;
begin
     data:=IntToStr(fSequenceValue);
     result := data;
     j:=1;
     s:='';
     if length(data) < 4 then
     begin
          for i := 1 to 4 - length(data) do
          begin
               insert('0', s, j);
               INC(j);
          end;
          insert(data,s,j);
          result := s;
     end;
end;

end.
