//
//
//    TThreadedQuery
//    Created By Gregg Teehan
//    Modified By David Verespey
//    Copyright 1997-2000 Gregg Teehan,David Verespey
//
//    This component is intended to encapsulate all the code nessasary to run a database query in a
//    thread.
//
//    Releases
//      Version 1.0 David Verespey  Initial release to the world
//
//    Please send any comments, suggestions, accusations to dverespey@nummi.com
//    My goal is to make this a useful and used component. Any changes that work well will ba added
//    and appropriate credit will be added to the source file.
//
//
//    Note: This component was developed for use in Delphi 2.0. I have since upgraded to Delphi 5.0
//          I assume that this will work ok with all versions above Delphi 2.0.
//
//
unit ThreadedBDEQuery;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  DB,
  DBTables,
  ThreadedQueryDef;

type
	TErrorEvent			= procedure (Sender : TObject; ErrorMsg : string)   	of object;
  TCompleteEvent	= procedure (Sender : TObject; Dataset	: TDataset)		of object;
  TRunningEvent		= procedure (Sender : TObject; Query		: TQuery)   	of object;
  TConnectEvent		= procedure (Sender : TObject; Database	: TDatabase)	of object;


	// forward declared
  TThreadedBDEQuery	= class;


  // The thread that runs inside the ThreadedQuery.
  //		1.  sleep until we have a command to execute
  //		2.  execute the requested command
  //		3.  if "OneShot" then set FActive to FALSE'
  //		4.  else sleep one "Restart" cycle and back to step 2
  TQThread		= class (TThread)
    private
    // Private declarations
			OwnerComp		:	TThreadedBDEQuery;

      SleepThread	:	integer;
      SleepUser		:	integer;

      Holding			:	boolean;

      //-------------------------------------------------------------------
      // the following procedures are called via "synchronize" so that the
      //	owning form need not worry about VCL re-entrancy problems.
      //-------------------------------------------------------------------
      procedure syncActivate;
      procedure syncDeActivate;
      procedure syncBeforeConnect;
      procedure syncAfterConnect;
      procedure syncBeforeRun;
      procedure syncComplete;
      procedure syncAfterRun;
      procedure syncBeforeDisConnect;
      procedure syncAfterDisConnect;

      // the three steps of each execute cycle
      //	1) connect to the specified database if not already done.
      //	2) process the query and any data received as a result
      //	3) disconnect if "KeepConnection" is FALSE;
      function  doConnect:boolean;
      procedure doDisConnect;
      procedure doForceDisConnect;

      // the main procedure called from the thread's "execute" method
      procedure QueryProcess (cmnd	:	TqryCommand);

      procedure syncOn (fSync		:	TThreadMethod);

  	protected
    // Protected declarations
      procedure doProcess    (cmnd	:	TqryCommand);
    	procedure Execute;	override;

		public
    // Public declarations

		published
    // Published declarations
	end;



  TThreadedBDEQuery = class(TThreadedQuery)
	  private
  	  // Private declarations
      //	non-property fields
	    FSession					:	TSession;				// do NOT use the default thread, the main thread needs it
  	  FDatabase					:	TDatabase;      // a separate database component for logon and alias selection
	    FQuery						:	TQuery;         // the query that actually execute the SQL commands
	    FThread						:	TQThread;   		// the thread that controls everything

      //	property fields
			FDriverName				:	string;         // BDE driver, e.g. ODBC_xxxxxxxx
      fAliasName				:	string;					// BDE Alias Name
			FLoginParams			:	TStringList;    // login parameters for database connect

      //	event fields                         triggered when
      FActivate					:	TNotifyEvent;		  // Active is set to TRUE
      FDeActivate				:	TNotifyEvent;		  // Active is set to FALSE (after a ONESHOT)
			FBeforeConnect		:	TConnectEvent;		// before the database connect is attempted
			FAfterConnect			:	TConnectEvent;		// after the database connect succeeds
			FBeforeRun				:	TRunningEvent;		// before the TQuery is sent to the server
  	  FComplete     		:	TCompleteEvent;	  // when the TQuery completes successfully
    	FAfterRun					:	TRunningEvent;		// after return from "FComplete" above
			FBeforeDisConnect	:	TConnectEvent;		// before database disconnect if KeepConnection=FALSE
			FAfterDisConnect	:	TConnectEvent;		// after database has been disconnected
	    FError						:	TErrorEvent;			// when an error occurs in the thread.


			procedure	SetDriverName   	(value :	string);			function GetDriverName   	: string;
			procedure	SetAliasName    	(value :	string);			function GetAliasName     : string;
			procedure	SetLoginParams  	(value :	TStringList);	function GetLoginParams  	: TStringList;


      procedure SetActivate					(value :	TNotifyEvent );
      procedure SetDeActivate				(value :	TNotifyEvent );
			procedure SetBeforeConnect		(value :  TConnectEvent );
			procedure SetAfterConnect			(value :  TConnectEvent );
			procedure SetBeforeRun				(value :  TRunningEvent );
  	  procedure SetComplete     		(value :  TCompleteEvent);
    	procedure SetAfterRun					(value :  TRunningEvent );
 			procedure SetBeforeDisConnect	(value :  TConnectEvent );
			procedure SetAfterDisConnect	(value :  TConnectEvent );
	    procedure SetError						(value :  TErrorEvent   );

  	  procedure SendError	(msg : string);

	  protected
  	  // Protected declarations
			procedure SetActive (value :	boolean); override;
			procedure	SetQryCommand			(value :	TqryCommand); override;
      function GetQryCommand		:	TqryCommand; override;
			procedure	SetRunMode				(value :	TrunMode); override;
      function GetRunMode				:	TrunMode; override;
			procedure	SetTimerInitial 	(value :	longint); override;
      function GetTimerInitial 	: longint; override;
			procedure	SetTimerRestart 	(value :	longint); override;
      function GetTimerRestart 	: longint; override;
			procedure	SetKeepConnection	(value :	boolean); override;
      function GetKeepConnection: boolean; override;
	    		{read-only, no Setxxx}
      function GetIsRunning    	: boolean; override;

	  public
  	  // Public declarations
			constructor Create (AOwner : TComponent); override;
	    destructor  Destroy; override;
	    procedure   Open; override;
	    procedure   Exec; override;
      procedure		Hold; override;
      procedure		UnHold; override;
      procedure 	Disconnect; override;

	  published
	    // Published declarations
	    property DriverName					:	string      	  read GetDriverName   	 write SetDriverName;
      property AliasName					: string				  read GetAliasName			 write SetaliasName;
  	  property LoginParams				:	TStringList 	  read GetLoginParams 	 write SetLoginParams;
      property OnActivate					:	TNotifyEvent    read FActivate	       write SetActivate;
      property OnDeActivate				:	TNotifyEvent    read FDeActivate       write SetDeActivate;
			property OnBeforeConnect		:	TConnectEvent   read FBeforeConnect	 	 write SetBeforeConnect;
			property OnAfterConnect			:	TConnectEvent   read FAfterConnect		 write SetAfterConnect;
			property OnBeforeRun				:	TRunningEvent   read FBeforeRun			 	 write SetBeforeRun;
  	  property OnComplete     		:	TCompleteEvent  read FComplete     	   write SetComplete;
    	property OnAfterRun					:	TRunningEvent   read FAfterRun				 write SetAfterRun;
			property OnBeforeDisConnect	:	TConnectEvent	  read FBeforeDisConnect write SetBeforeDisConnect;
			property OnAfterDisConnect	:	TConnectEvent   read FAfterDisConnect  write SetAfterDisConnect;
	    property OnError						:	TErrorEvent     read FError					   write SetError;
	  end;


implementation

const
	    msgQueryInProcess	=	'%s: Query is %srunning';


//---------------------------------------------------
// TQThread                                     .
//---------------------------------------------------
procedure TQThread.syncActivate;        begin OwnerComp.FActivate        (OwnerComp); end;
procedure TQThread.syncDeActivate;			begin OwnerComp.FDeActivate			 (OwnerComp); end;
procedure TQThread.syncBeforeConnect;   begin OwnerComp.FBeforeConnect   (OwnerComp, OwnerComp.FDatabase); end;
procedure TQThread.syncAfterConnect;    begin OwnerComp.FAfterConnect    (OwnerComp, OwnerComp.FDatabase); end;
procedure TQThread.syncBeforeRun;       begin OwnerComp.FBeforeRun       (OwnerComp, OwnerComp.FQuery   ); end;
procedure TQThread.syncComplete;        begin OwnerComp.FComplete        (OwnerComp, OwnerComp.FQuery   ); end;
procedure TQThread.syncAfterRun;        begin OwnerComp.FAfterRun        (OwnerComp, OwnerComp.FQuery   ); end;
procedure TQThread.syncBeforeDisConnect;begin OwnerComp.FBeforeDisConnect(OwnerComp, OwnerComp.FDatabase); end;
procedure TQThread.syncAfterDisConnect; begin OwnerComp.FAfterDisConnect (OwnerComp, OwnerComp.FDatabase); end;


procedure TQThread.syncOn (
	fSync		:	TThreadMethod);
begin
	while (Holding and (NOT terminated)) do
  	sleep (0);

	if (NOT terminated) then
		synchronize (fSync);
end;


//----------------------------------------------------
//                                                   .
//                                                   .
//    Connect to DB either thru the Alias or Driver  .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//----------------------------------------------------
function TQThread.doConnect: boolean;
begin
  result:=true;
	with OwnerComp do
	begin
		if (NOT FDatabase.Connected) then
 	  begin
			if (Assigned (FBeforeConnect)) then
				syncOn (syncBeforeConnect);

     	try
        if fAliasName <> '' then
        begin
					FDatabase.AliasName   := FAliasName;
	  	  	FDatabase.Params.Assign (FLoginParams);

	  	  	FQuery.DatabaseName := FDatabase.DatabaseName;
					FDatabase.Connected := TRUE;
        end
      	else if fDriverName <> '' then
        begin
					FDatabase.DriverName   := FDriverName;
	  	  	FDatabase.Params.Assign (FLoginParams);

	  	  	FQuery.DatabaseName := FDatabase.DatabaseName;
					FDatabase.Connected := TRUE;
        end
        else
        begin
        	SendError('No database specified');
					FDatabase.Connected := FALSE;
          result:=False;
          exit;
        end;

 		  except
   	  	on E:Exception do
        begin
     	  	SendError ('Connect:: '+E.Message);
					FDatabase.Connected := FALSE;
          result:=False;
          exit;
        end;
    	end;

			if (Assigned (FAfterConnect)) then
				syncOn (syncAfterConnect);
		end;
  end;
end;

//----------------------------------------------------
//                                                   .
//                                                   .
// 		Open or exec the query                         .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//----------------------------------------------------
procedure TQThread.doProcess (cmnd	:	TqryCommand);
begin
	with OwnerComp do
	begin
		if (NOT FDatabase.Connected) then
      exit
    	//SendError ('thread: NOT CONNECTED')
    else
    begin
			try
	      case cmnd of
 	      	qryOpen	:
          		FQuery.Open;
   	      qryExec	:
          		FQuery.ExecSQL;
     	  end;

      	try

        	if (Assigned (FComplete)) then
        		syncOn (syncComplete);

	        case cmnd of
 	      	  qryOpen	:
          	  fQuery.Close;
          end;

        except
        	on E:Exception do
          begin
          	SendError ('Close Failed: '+E.Message);
          	doForceDisconnect;
          end;
        end;

   	  except
     		on E:Exception do
        begin
       		SendError ('Exec/Open failed: '+E.Message);
          doForceDisconnect;
        end;
			end;
		end;
	end;
end;

//----------------------------------------------------
//                                                   .
//                                                   .
// 		Disconnect if required                         .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//----------------------------------------------------
procedure TQThread.doDisConnect;
begin
	with OwnerComp do
	begin
		if (FDatabase.Connected) and (NOT FKeepConnection) then
		begin
			if (Assigned (FBeforeDisConnect)) then
				syncOn (syncBeforeDisConnect);
			try
				FDatabase.Connected := FALSE;
      except
     		on E:Exception do
        begin
       		SendError ('Disconnect:: '+E.Message);
        end;
      end;

			if (Assigned (FAfterDisConnect)) then
				syncOn (syncAfterDisConnect);
 	  end;
	end;
end;


procedure TQThread.doForceDisConnect;
begin
	with OwnerComp do
	begin
		if (FDatabase.Connected) then
		begin
			if (Assigned (FBeforeDisConnect)) then
				syncOn (syncBeforeDisConnect);
			try
				FDatabase.Connected := FALSE;
      except
     		on E:Exception do
        begin
       		SendError ('Disconnect:: '+E.Message);
        end;
      end;

			if (Assigned (FAfterDisConnect)) then
				syncOn (syncAfterDisConnect);
 	  end;
	end;
end;



//----------------------------------------------------
//                                                   .
//                                                   .
// 		Process Loop                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//----------------------------------------------------
procedure TQThread.QueryProcess (cmnd	:	TqryCommand);

function slLength (strList	:	TStringList) : integer;
var
	ix	:	integer;
begin
	result := 0;
  for ix := 0 to strList.Count-1 do
		INC (result, length (TRIM (strList.Strings[ix])));
end;

begin
	with OwnerComp do
	begin
  	try
			if (slLength (TStringList (FSQL)) > 0) then
				FQuery.SQL.Assign (FSQL);

			if (Assigned (FBeforeRun)) then
				//syncOn (syncBeforeRun);
        //
        //  This allows the Main program to fill in the SQL
        //  while in this context
        //
				syncBeforeRun;

			if (slLength (TStringList (FQuery.SQL)) > 0) then
    	begin
				if doConnect then
        begin
				  doProcess (cmnd);
      	  doDisConnect;
        end
        else
          exit;
			end;
    except
    	on E:Exception do
      begin
      	SendError ('Main:: '+E.Message);
      	doForceDisConnect;
      end;
    end;

		if (Assigned (FAfterRun)) then
			syncOn (syncAfterRun);
  end;
end;

//----------------------------------------------------
//                                                   .
//                                                   .
// 		Main Thread Loop                               .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//----------------------------------------------------
procedure TQThread.Execute;
var
	tmrSleep		:	longint;
begin
	with OwnerComp do
  begin
	  FSession.SessionName   := format ('Session%d' , [ThreadID]);

	  FDatabase.SessionName  := FSession.SessionName;
		FDatabase.LoginPrompt  := FALSE;

    FQuery.SessionName  	 := FSession.SessionName;

    Holding     := FALSE;
    SleepThread := 1;
    SleepUser   := 1;

		if (FRunMode = runContinuous) and (FTimerInitial > 0) then
			sleep (FTimerInitial);

		while NOT terminated do
  	begin
    	if (NOT FActive) then
      	Suspend
      else
      begin
        if (NOT Terminated) then
        begin
          SleepThread := SleepUser;
          FIsRunning  := TRUE;

          case FQryCommand of
            qryOpen	:	QueryProcess (qryOpen);
            qryExec	:	QueryProcess (qryExec);
          end;

          FIsRunning := FALSE;
        end;
			end;

      if (NOT Terminated) then
				case FRunMode of
      		runOneShot		:
          							begin
													if (SleepThread = SleepUser) then
                            Suspend;
							          end;

        	runContinuous	:
          							begin
							          	if (FTimerRestart > 0) then
													begin
										      	tmrSleep := FTimerRestart;

      			  							while (FActive and
                            		  (NOT Terminated) and
                                  (tmrSleep > 0) and
                                  (SleepThread = SleepUser)) do
										        begin
      			  								sleep (10);
															DEC (tmrSleep, 10);
			        							end;
													end;
												end;
        end;
    end;

  end;
  OwnerComp.FThread := nil;
end;




//----------------------------------------------------
// TThreadedQuery Component                          .
//----------------------------------------------------
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//----------------------------------------------------
constructor TThreadedBDEQuery.Create (AOwner : TComponent);
begin
	inherited Create (AOwner);

	FSession      	:= TSession.Create (self);
  FDatabase    		:= TDatabase.Create (self);
  FDatabase.DatabaseName := 'DB_' + inttostr (integer (fDatabase));
  FQuery        	:= TQuery.Create (self);

	FActive					:= FALSE;
	FDriverName			:= '';
	FLoginParams	  := TStringList.Create;
	FSQL					  := TStringList.Create;
	FQryCommand			:= qryOpen;
	FRunMode				:= runOneShot;
	FTimerInitial	  := 0;
	FTimerRestart	  := 0;
  FKeepConnection := FALSE;
	FIsRunning			:= FALSE;

  if  UpperCase(ExtractFileName(Application.ExeName)) <> 'DELPHI32.EXE' then
  begin
		FThread := TQThread.Create (FALSE);
		FThread.OwnerComp := self;
    fThread.FreeOnTerminate:=True;
  end
  else
    fThread := nil;
end;

destructor  TThreadedBDEQuery.Destroy;
begin
	if (Assigned (FThread)) then
  begin
  	FThread.Terminate;
    //
    //  Change just send a message and then continue
    //  Let the thread handle it's own demise
    //
    while (Assigned (FThread)) do
    begin
      case FRunMode of
      		runOneShot:
      	    FThread.Resume;
        	runContinuous	:
            begin
			        INC (FThread.SleepUser);
      	      FThread.Resume;
            end;
      end;
    	Application.ProcessMessages;
    	sleep (0);
    end;
  end;

  fLoginParams.Free;
  fSQL.Free;
  FQuery.Free;
  FDatabase.Free;
  FSession.Free;

	inherited Destroy;
end;


procedure TThreadedBDEQuery.SendError (msg : string);
begin
	if (Assigned (FError)) then
  	FError (self, msg);
end;


procedure TThreadedBDEQuery.Open;
begin
	INC (FThread.SleepUser);
  FQryCommand := qryOpen;
  Active := TRUE;
end;

procedure TThreadedBDEQuery.Exec;
begin
	INC (FThread.SleepUser);
  FQryCommand := qryExec;
  Active := TRUE;
end;

procedure TThreadedBDEQuery.Hold;
begin
	if (Assigned (FThread)) then
  	FThread.Holding := TRUE;
end;

procedure TThreadedBDEQuery.UnHold;
begin
	if (Assigned (FThread)) then
  	FThread.Holding := FALSE;
end;

procedure TThreadedBDEQuery.Disconnect;
begin
	if fDatabase.Connected then
  	fDatabase.Connected:=False;
end;



//----------------------------------------------------
//                                                   .
//                                                   .
// 		Set and Get Properties                         .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//----------------------------------------------------
procedure TThreadedBDEQuery.SetActive       (value :	boolean);
begin
	if (Assigned (FThread)) then
  begin
	  if (NOT FActive and     value) then
    begin
    	if (Assigned (FActivate)) then
	    	FThread.syncOn (FThread.syncActivate)
		end
		else
  	if (    FActive and NOT value) then
    begin
    	if (Assigned (FDeactivate)) then
  	  	FThread.syncOn (FThread.syncDeActivate);
		end;
	end;

  inherited SetActive (value);
//	FActive := value;

	if (FActive and Assigned (FThread)) then
  begin
		while (FThread.Suspended) do
	  	FThread.Resume;
  end;
end;

{function  TThreadedBDEQuery.GetActive        : boolean;
begin
	result := FActive;
end;}

procedure	TThreadedBDEQuery.SetDriverName    (value :	string);
begin
	if value <> '' then
  begin
  	if fAliasName <> '' then
  		SetAliasName('');
  end;
	FDriverName := value;
end;
function  TThreadedBDEQuery.GetDriverName     : string;
begin
	result := FDriverName;
end;

procedure	TThreadedBDEQuery.SetAliasName    (value :	string);
begin
	if value <> '' then
  begin
  	if fDriverName <> '' then
  		SetDriverName('');
  end;
	FAliasName := value;
end;
function  TThreadedBDEQuery.GetAliasName     : string;
begin
	result := FAliasName;
end;

procedure	TThreadedBDEQuery.SetLoginParams (value :	TStringList);
begin
	FLoginParams.BeginUpdate;
	FLoginParams.Assign (value);
	FLoginParams.EndUpdate;
end;
function  TThreadedBDEQuery.GetLoginParams : TStringList;
begin
	result := FLoginParams;
end;



{procedure	TThreadedBDEQuery.SetSQL (value :	TStringList);
begin
	FSQL.BeginUpdate;
	FSQL.Assign (value);
	FSQL.EndUpdate;
end;

function  TThreadedBDEQuery.GetSQL  : TStringList;
begin
	result := FSQL;
end;}


procedure	TThreadedBDEQuery.SetQryCommand (value :	TqryCommand);
begin
  FQryCommand := value;
end;
function  TThreadedBDEQuery.GetQryCommand  : TqryCommand;
begin
	result := FQryCommand;
end;


procedure	TThreadedBDEQuery.SetRunMode (value :	TrunMode);
begin
  FRunMode := value;
end;
function  TThreadedBDEQuery.GetRunMode  : TrunMode;
begin
	result := FRunMode;
end;


procedure	TThreadedBDEQuery.SetTimerInitial (value :	longint);
begin
  FTimerInitial := value;
end;
function  TThreadedBDEQuery.GetTimerInitial  : longint;
begin
	result := FTimerInitial;
end;


procedure	TThreadedBDEQuery.SetTimerRestart (value :	longint);
begin
	FTimerRestart := value;
end;
function  TThreadedBDEQuery.GetTimerRestart  : longint;
begin
	result := FTimerRestart;
end;

procedure	TThreadedBDEQuery.SetKeepConnection (value :	boolean);
begin
	FKeepConnection := value;
end;
function  TThreadedBDEQuery.GetKeepConnection  : boolean;
begin
	result := FKeepConnection;
end;

function  TThreadedBDEQuery.GetIsRunning	:	boolean;
begin
	if (Assigned (FThread)) then
  	result := FIsRunning
  else
  	result := FALSE;
end;



//----------------------------------------------------
//                                                   .
//                                                   .
// 		Events								                         .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//                                                   .
//----------------------------------------------------
procedure TThreadedBDEQuery.SetActivate		 		 (value :  TNotifyEvent );  begin FActivate         := value; end;
procedure TThreadedBDEQuery.SetDeactivate		 	 (value :  TNotifyEvent );  begin FDeactivate       := value; end;
procedure TThreadedBDEQuery.SetBeforeConnect		 (value :  TConnectEvent ); begin FBeforeConnect	  := value; end;
procedure TThreadedBDEQuery.SetAfterConnect		 (value :  TConnectEvent ); begin FAfterConnect		  := value; end;
procedure TThreadedBDEQuery.SetBeforeRun				 (value :  TRunningEvent ); begin FBeforeRun				:= value; end;
procedure TThreadedBDEQuery.SetComplete     		 (value :  TCompleteEvent); begin FComplete     		:= value; end;
procedure TThreadedBDEQuery.SetAfterRun				 (value :  TRunningEvent ); begin FAfterRun				  := value; end;
procedure TThreadedBDEQuery.SetBeforeDisConnect (value :  TConnectEvent ); begin FBeforeDisConnect := value; end;
procedure TThreadedBDEQuery.SetAfterDisConnect	 (value :  TConnectEvent ); begin FAfterDisConnect	:= value; end;
procedure TThreadedBDEQuery.SetError						 (value :  TErrorEvent   ); begin FError						:= value; end;



end.
