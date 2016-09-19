unit ThreadedQueryDef;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls;

type
  // qryCommand - What TQuery method is used when the thread wakes up
  // qryOpen - TQuery method will be "Open"   , data is expected
  // qryExec - TQuery method will be "ExecSQL", NO data is expected
	TqryCommand      = (qryOpen, qryExec);

  // runMode - execute behaviour for the thread
  // runOneShot		- run once and then set FActive to false
  // runContinuous - run repeatedly while FActive is true
  TrunMode        	=	(runContinuous, runOneShot);

	// forward declared

  TThreadedQuery	= class(TComponent)
	  protected
      //	non-property fields
      //	property fields
			FActive						:	boolean;        // FALSE=suspend the thread, TRUE=run again
	    FSQL							:	TStringList;    // the SQL statement when the thread executes
  	  FTimerInitial			:	longint;        // initial delay for CONTINUOUS queries
    	FTimerRestart			:	longint;        // sleep time in between repeats of CONTINUOUS mode
			FIsRunning				:	boolean;		   	// thread is busy with the query *** READ-ONLY property
			FQryCommand				:	TqryCommand;      // OPEN vs. EXECSQL
			FRunMode					:	TrunMode;         // CONTINUOUS vs. ONESHOT
	    FKeepConnection		:	boolean;          // keep database conections once established


			procedure SetSQL    (value :	TStringList);
      function  GetSQL : TStringList;
			procedure SetActive (value :	boolean); virtual;
      function  GetActive : boolean;

			{procedure	SetQryCommand			(value :	TqryCommand); virtual; abstract;
      function GetQryCommand		:	TqryCommand; virtual; abstract;
			procedure	SetRunMode				(value :	TrunMode); virtual; abstract;
      function GetRunMode				:	TrunMode; virtual; abstract;
			procedure	SetTimerInitial 	(value :	longint); virtual; abstract;
      function GetTimerInitial 	: longint; virtual; abstract;
			procedure	SetTimerRestart 	(value :	longint); virtual; abstract;
      function GetTimerRestart 	: longint; virtual; abstract;
			procedure	SetKeepConnection	(value :	boolean); virtual; abstract;
      function GetKeepConnection: boolean; virtual; abstract;
	    		//read-only, no Setxxx
      function GetIsRunning    	: boolean; virtual; abstract;}


			procedure	SetQryCommand			(value :	TqryCommand); virtual;
      function GetQryCommand		:	TqryCommand; virtual;
			procedure	SetRunMode				(value :	TrunMode); virtual;
      function GetRunMode				:	TrunMode; virtual;
			procedure	SetTimerInitial 	(value :	longint); virtual;
      function GetTimerInitial 	: longint; virtual;
			procedure	SetTimerRestart 	(value :	longint); virtual;
      function GetTimerRestart 	: longint; virtual;
			procedure	SetKeepConnection	(value :	boolean); virtual;
      function GetKeepConnection: boolean; virtual;
	    		{read-only, no Setxxx}
      function GetIsRunning    	: boolean; virtual;

    public
	    {procedure   Open; virtual; abstract;
	    procedure   Exec; virtual; abstract;
      procedure		Hold; virtual; abstract;
      procedure		UnHold; virtual; abstract;
      procedure 	Disconnect; virtual; abstract;}

	    procedure   Open; virtual;
	    procedure   Exec; virtual;
      procedure		Hold; virtual;
      procedure		UnHold; virtual;
      procedure 	Disconnect; virtual;

	  published
	    { Published declarations }
    	property SQL								:	TStringList 	  read GetSQL         	 write SetSQL;
			property Active							:	boolean     	  read GetActive      	 write SetActive;
			property QryCommand					:	TqryCommand		  read GetQryCommand		 write SetQryCommand;
			property RunMode						:	TrunMode     	  read GetRunMode				 write SetRunMode;
	    property TimerInitial				:	longint 	 		  read GetTimerInitial	 write SetTimerInitial;
  	  property TimerRestart				:	longint     	  read GetTimerRestart	 write SetTimerRestart;
    	property KeepConnection			:	boolean    		  read GetKeepConnection write SetKeepConnection;
	    property IsRunning					:	boolean				  read GetIsRunning;

  end;


implementation

procedure   TThreadedQuery.Open;
begin
end;
procedure   TThreadedQuery.Exec;
begin
end;
procedure		TThreadedQuery.Hold;
begin
end;
procedure		TThreadedQuery.UnHold;
begin
end;
procedure 	TThreadedQuery.Disconnect;
begin
end;

procedure	TThreadedQuery.SetQryCommand			(value :	TqryCommand);
begin
end;
function TThreadedQuery.GetQryCommand		:	TqryCommand;
begin
  result:=qryOpen;
end;
procedure	TThreadedQuery.SetRunMode				(value :	TrunMode);
begin
end;
function TThreadedQuery.GetRunMode				:	TrunMode;
begin
  result:=runContinuous
end;
procedure	TThreadedQuery.SetTimerInitial 	(value :	longint);
begin
end;
function TThreadedQuery.GetTimerInitial 	: longint;
begin
  result:=0;
end;
procedure	TThreadedQuery.SetTimerRestart 	(value :	longint);
begin
end;
function TThreadedQuery.GetTimerRestart 	: longint;
begin
  result:=0;
end;
procedure	TThreadedQuery.SetKeepConnection	(value :	boolean);
begin
end;
function TThreadedQuery.GetKeepConnection: boolean;
begin
  result:=False;
end;
    {read-only, no Setxxx}
function TThreadedQuery.GetIsRunning    	: boolean;
begin
  result:=False;
end;


procedure	TThreadedQuery.SetSQL (value :	TStringList);
begin
	FSQL.BeginUpdate;
	FSQL.Assign (value);
	FSQL.EndUpdate;
end;

function  TThreadedQuery.GetSQL  : TStringList;
begin
	result := FSQL;
end;

function  TThreadedQuery.GetActive        : boolean;
begin
	result := FActive;
end;

procedure TThreadedQuery.SetActive (value :	boolean);
begin
  fActive:=value;
end;

end.
