//
//
//    TThreadedADOQuery
//    Based on TTHreadedQuery
//    Created By Gregg Teehan
//    Modified By David Verespey
//    Copyright 1997-2001 Gregg Teehan,David Verespey
//
//    This component is intended to encapsulate all the code nessasary to run a database ado query in a
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
//
//
unit ThreadedADOQuery;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ActiveX,
  ADOdb,
  ThreadedQueryDef;

type
	TErrorEvent			= procedure (Sender : TObject; ErrorMsg   : string)   	    of object;
  TCompleteEvent	= procedure (Sender : TObject; Dataset    : TADOQuery)      of object;
  TRunningEvent		= procedure (Sender : TObject; Dataset    : TADOQuery)      of object;
  TConnectEvent		= procedure (Sender : TObject; Connection	: TADOConnection) of object;

	// forward declared
  TThreadedADOQuery	= class;


  // The thread that runs inside the ThreadedQuery.
  //		1.	sleep until we have a command to execute
  //		2.	execute the requested command
  //		3.	if "OneShot" then set FActive to FALSE'
  //		4.	else sleep one "Restart" cycle and back to step 2
  TQThread		= class (TThread)
    private
    { Private declarations }
			OwnerComp		:	TThreadedADOQuery;

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
      procedure doProcess    (cmnd	:	TqryCommand);
      function  doConnect:boolean;
      procedure doDisConnect;
      procedure doForceDisConnect;

      // the main procedure called from the thread's "execute" method
      procedure QueryProcess (cmnd	:	TqryCommand);

      // forces called to be synchronized
      procedure syncOn (fSync		:	TThreadMethod);

  	protected
    { Protected declarations }
      // main loop
    	procedure Execute;	override;

		public
    { Public declarations }

		published
    { Published declarations }
	end;



  TThreadedADOQuery = class(TThreadedQuery)
	  private
  	  { Private declarations }

      //	non-property fields
      fQuery            : TADOQuery;
      fConnection       : TADOConnection;
	    FThread						:	TQThread;   		// the thread that controls everything

      //	property fields
      fConnectionString : widestring;     // Info required to connect

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
  	  { Protected declarations }
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
      procedure	SetConnectionStr  (value :	widestring);
      function GetConnectionStr	: widestring;

	  public
  	  { Public declarations }
			constructor Create (AOwner : TComponent); override;
	    destructor  Destroy; override;
	    procedure   Open; override;
	    procedure   Exec; override;
      procedure		Hold; override;
      procedure		UnHold; override;
      procedure 	Disconnect; override;

	  published
	    { Published declarations }
      property ConnectionString   : widestring      read GetConnectionStr  write SetConnectionStr;
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
procedure TQThread.syncBeforeConnect;   begin OwnerComp.FBeforeConnect   (OwnerComp, OwnerComp.FConnection); end;
procedure TQThread.syncAfterConnect;    begin OwnerComp.FAfterConnect    (OwnerComp, OwnerComp.FConnection); end;
procedure TQThread.syncBeforeRun;       begin OwnerComp.FBeforeRun       (OwnerComp, OwnerComp.FQuery   ); end;
procedure TQThread.syncComplete;        begin OwnerComp.FComplete        (OwnerComp, OwnerComp.FQuery   ); end;
procedure TQThread.syncAfterRun;        begin OwnerComp.FAfterRun        (OwnerComp, OwnerComp.FQuery   ); end;
procedure TQThread.syncBeforeDisConnect;begin OwnerComp.FBeforeDisConnect(OwnerComp, OwnerComp.FConnection); end;
procedure TQThread.syncAfterDisConnect; begin OwnerComp.FAfterDisConnect (OwnerComp, OwnerComp.FConnection); end;


procedure TQThread.syncOn (
	fSync		:	TThreadMethod);
begin
	while (Holding and (NOT terminated)) do
  	sleep (0);

	if (NOT terminated) then
		synchronize (fSync);
end;


//----------------------------------------------------
//                                                    .
//                                                    .
//    Connect to DB                                   .
//                                                    .
//                                                    .
//                                                    .
//                                                    .
//                                                    .
//                                                    .
//----------------------------------------------------
function TQThread.doConnect: boolean;
begin
  result:=true;
	with OwnerComp do
	begin
		if (NOT fConnection.Connected) then
 	  begin
			if (Assigned (FBeforeConnect)) then
				syncOn (syncBeforeConnect);

     	try
        if fConnectionString <> '' then
        begin
          fConnection.ConnectionString:=fConnectionString;
          fConnection.Connected:=True;
        end
        else
        begin
        	SendError('No connection string');
					FConnection.Connected := FALSE;
          result:=False;
          exit;
        end;

 		  except
   	  	on E:Exception do
        begin
     	  	SendError ('Connect:: '+E.Message);
					FConnection.Connected := FALSE;
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
		if (NOT fConnection.Connected) then
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
            //syncComplete;

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
		if (fConnection.Connected) and (NOT FKeepConnection) then
		begin
			if (Assigned (FBeforeDisConnect)) then
				syncOn (syncBeforeDisConnect);
			try
				fConnection.Connected := FALSE;
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
		if (fConnection.Connected) then
		begin
			if (Assigned (FBeforeDisConnect)) then
				syncOn (syncBeforeDisConnect);
			try
				fConnection.Connected := FALSE;
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
			if (slLength (TStringList (SQL)) > 0) then
				FQuery.SQL.Assign (FSQL);

			if (Assigned (FBeforeRun)) then
      begin
				//syncOn (syncBeforeRun);
        //
        //  This allows the Main program to fill in the SQL
        //  while in this context
        //
        syncBeforeRun;
      end;

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
  firstrun    : boolean;
begin
  firstrun:=true;
  ReturnValue:=1;
	with OwnerComp do
  begin
    CoInitializeEx(nil,COINIT_MULTITHREADED or COINIT_SPEED_OVER_MEMORY);
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
							          	if (FTimerRestart > 0) and not(firstrun) then
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
                          firstrun:=False;
												end;
        end;
    end;

    FQuery.Free;
    fConnection.Free;

    coUnInitialize;

    //FThread := nil;
  end;
  ReturnValue:=0;
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
constructor TThreadedADOQuery.Create (AOwner : TComponent);
begin
	inherited Create (AOwner);

  fConnection                   := TADOConnection.Create (self);
  fConnection.LoginPrompt       := FALSE;
  fConnection.ConnectionTimeout := 5; //This should be a property
  FQuery        	        := TADOQuery.Create (self);
  fQuery.Connection       := fConnection;
  fQuery.ParamCheck       := False;

	FActive					:= FALSE;
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
    fThread.FreeOnTerminate:=False;
  end
  else
    fThread := nil;
end;

destructor  TThreadedADOQuery.Destroy;
begin
	if (Assigned (FThread)) then
  begin
  	FThread.Terminate;
    case FRunMode of
      runOneShot:
        begin
          FThread.Resume;
          sleep(0);
        end;
      runContinuous	:
        begin
          FThread.Resume;
          sleep(0);
        end;
    end;
    while FThread.ReturnValue>0 do
    begin
    	Application.ProcessMessages;
    	sleep (0);
    end;
  end;

  fSQL.Free;
    fThread.Free;

	inherited Destroy;
end;


procedure TThreadedADOQuery.SendError (msg : string);
begin
	if (Assigned (FError)) then
  	FError (self, msg);
end;

procedure TThreadedADOQuery.Open;
begin
	INC (FThread.SleepUser);
  FQryCommand := qryOpen;
  Active := TRUE;
end;

procedure TThreadedADOQuery.Exec;
begin
	INC (FThread.SleepUser);
  FQryCommand := qryExec;
  Active := TRUE;
end;

procedure TThreadedADOQuery.Hold;
begin
	if (Assigned (FThread)) then
  	FThread.Holding := TRUE;
end;

procedure TThreadedADOQuery.UnHold;
begin
	if (Assigned (FThread)) then
  	FThread.Holding := FALSE;
end;

procedure TThreadedADOQuery.Disconnect;
begin
	if fConnection.Connected then
  	fConnection.Connected:=False;
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
procedure TThreadedADOQuery.SetActive       (value :	boolean);
begin

	if (Assigned (FThread)) then
  begin
	  if (NOT FActive and value) then
    begin
    	if (Assigned (FActivate)) then
	    	FThread.syncOn (FThread.syncActivate)
		end
		else
  	if (FActive and NOT value) then
    begin
    	if (Assigned (FDeactivate)) then
  	  	FThread.syncOn (FThread.syncDeActivate);
		end;
	end;

  inherited SetActive (value);

	if (FActive and Assigned (FThread)) then
  begin
		while (FThread.Suspended) do
	  	FThread.Resume;
  end;
end;

procedure	TThreadedADOQuery.SetConnectionStr  (value :	widestring);
begin
  fConnectionString:=value;
end;

function TThreadedADOQuery.GetConnectionStr	: widestring;
begin
  result := fConnectionString;
end;

procedure	TThreadedADOQuery.SetQryCommand (value :	TqryCommand);
begin
  FQryCommand := value;
end;

function  TThreadedADOQuery.GetQryCommand  : TqryCommand;
begin
	result := FQryCommand;
end;

procedure	TThreadedADOQuery.SetRunMode (value :	TrunMode);
begin
  FRunMode := value;
end;

function  TThreadedADOQuery.GetRunMode  : TrunMode;
begin
	result := FRunMode;
end;

procedure	TThreadedADOQuery.SetTimerInitial (value :	longint);
begin
  FTimerInitial := value;
end;

function  TThreadedADOQuery.GetTimerInitial  : longint;
begin
	result := FTimerInitial;
end;

procedure	TThreadedADOQuery.SetTimerRestart (value :	longint);
begin
	FTimerRestart := value;
end;

function  TThreadedADOQuery.GetTimerRestart  : longint;
begin
	result := FTimerRestart;
end;

procedure	TThreadedADOQuery.SetKeepConnection (value :	boolean);
begin
	FKeepConnection := value;
end;

function  TThreadedADOQuery.GetKeepConnection  : boolean;
begin
	result := FKeepConnection;
end;

function  TThreadedADOQuery.GetIsRunning	:	boolean;
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
procedure TThreadedADOQuery.SetActivate         (value :  TNotifyEvent );   begin FActivate		      := value; end;
procedure TThreadedADOQuery.SetDeactivate       (value :  TNotifyEvent );   begin FDeactivate	      := value; end;
procedure TThreadedADOQuery.SetBeforeConnect    (value :  TConnectEvent );  begin FBeforeConnect	  := value; end;
procedure TThreadedADOQuery.SetAfterConnect     (value :  TConnectEvent );  begin FAfterConnect		  := value; end;
procedure TThreadedADOQuery.SetBeforeRun				(value :  TRunningEvent );  begin FBeforeRun				:= value; end;
procedure TThreadedADOQuery.SetComplete     		(value :  TCompleteEvent);  begin FComplete     		:= value; end;
procedure TThreadedADOQuery.SetAfterRun				  (value :  TRunningEvent );  begin FAfterRun         := value; end;
procedure TThreadedADOQuery.SetBeforeDisConnect (value :  TConnectEvent );  begin FBeforeDisConnect := value; end;
procedure TThreadedADOQuery.SetAfterDisConnect	(value :  TConnectEvent );  begin FAfterDisConnect	:= value; end;
procedure TThreadedADOQuery.SetError						(value :  TErrorEvent   );  begin FError						:= value; end;



end.


