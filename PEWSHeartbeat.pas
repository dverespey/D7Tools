unit PEWSHeartbeat;

interface

uses
  SysUtils, Classes, ADOdb, windows;

type
	TErrorProc = procedure (Sender : TObject; ErrorMsg : string)   	of object;

  TAppState = (asOK, asTHREAD, asPLC, asSTORAGE, asHARDWARE);

  EPEWSHBException = class(Exception);
const
  SAppState : array[TAppState] of string
                  = ('OK', 'THREAD', 'PLC', 'STORAGE', 'HARDWARE');
type
  TPEWSHeartbeatThread = class(TThread)
  private
    { Private declarations }
    fAppName:string;
    fConnectionString:string;
    fAppState:TAppState;
    fsleeptime:integer;
    fADOConnection: TADOConnection;
    fADOCommand: TADOCommand;
    fEvent:THandle;
  protected
    procedure Wakeup;
    procedure Execute; override;
  published
    property ConnectionString:string
    read fConnectionstring
    write fConnectionString;

    property SleepTime:integer
    read fsleeptime
    write fsleeptime;

    property AppName:string
    read fAppName
    write fAppName;

    property AppState:TAppState
    read fAppState
    write fAppState;
  end;

  TPEWSHeartbeat = class(TComponent)
  private
    { Private declarations }
    fActive:boolean;
    fAppName:string;
    fConnectionstring:string;
    fthread:TPEWSHeartbeatThread;
    FError:	TErrorProc;
    fHBInterval:integer;
    fAppState:TAppState;
  protected
    { Protected declarations }
    procedure SetAppName(appname:string);
    procedure SetConnectionString(connectionstring:string);
    procedure SetAppState(appstate:TAppState);
    procedure SetActive(Active:boolean);
    procedure SetHBInterval(HBInterval:integer);
  	procedure SetError(value :  TErrorProc);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Active: boolean
    read fActive
    write SetActive;

    property AppName: string
    read fAppName
    write SetAppName;

    property OnError :	TErrorProc
    read FError
    write SetError;

    property ConnectionString: string
    read fconnectionstring
    write SetConnectionString;

    property HBInterval:integer
    read fHBInterval
    write SetHBInterval;

    property AppState:TAppState
    read fAppState
    write SetAppState;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NUMMI Tools', [TPEWSHeartbeat]);
end;

constructor TPEWSHeartbeat.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

  fHBInterval:=15000;

  if (not (csDesigning in ComponentState)) then
  begin
    fThread:=TPEWSHeartbeatThread.Create(true);
    fthread.FreeOnTerminate:=FALSE;
  end;
end;

destructor TPEWSHeartbeat.Destroy;
begin
  if assigned(fThread) then
  begin
    fThread.Terminate;
    fThread.Wakeup;
    fthread.WaitFor;
    fThread.Free;
  end;
	inherited destroy;
end;

procedure TPEWSHeartbeat.SetAppName(appname:string);
begin
  fAppName:=AppName;

  if assigned(fThread) then
    fThread.AppName:=appname;
end;

procedure TPEWSHeartbeat.SetConnectionString(connectionstring:string);
begin
  fconnectionstring:=connectionstring;

  if assigned(fThread) then
    fThread.ConnectionString:=connectionstring;
end;

procedure TPEWSHeartbeat.SetAppState(appstate:TAppState);
begin
  fAppState:=AppState;

  if assigned(fThread) then
    fThread.AppState:=appstate;
end;

procedure TPEWSHeartbeat.SetActive(Active:boolean);
begin
  if assigned(fThread) then
    if active then
      if fThread.Suspended then
        fThread.Resume
    else
      if not fThread.Suspended then
        fThread.Suspend;

  fActive:=active;
end;

procedure TPEWSHeartbeat.SetHBInterval(HBInterval:integer);
begin
  if assigned(fThread) then
    fThread.SleepTime:=HBInterval;

  fHBInterval:=HBInterval;
end;

procedure TPEWSHeartbeat.SetError(value :  TErrorProc   );
begin
	FError:= value;
end;

////////////////////////////////////////////////////////////////////////////////
//
//    Hearbeat Thread
//
//
//
//
//
////////////////////////////////////////////////////////////////////////////////
procedure TPEWSHeartbeatThread.Execute;
const
  UpdatePEWSSQL = 'UPDATE PEWS SET AppHeartBeat = AppHeartBeat+1, AppState = ''%s'' WHERE App_From = ''%s'' ';
begin
  // create event object for thread wake
  FEvent := CreateEvent(
              Nil,    // use default security
              true,   // event will be manually reset
              false,  // event starts out not signaled
              nil );  // event has no name

  If FEvent = 0 Then
  begin
     raise EPEWSHBException.Create('Unable to create Event, PEWS HB disabled');
  end
  else
  begin
    { Place thread code here }
    fADOConnection:=TADOConnection.Create(nil);
    fADOConnection.LoginPrompt:=False;
    fADOConnection.ConnectionString:=fConnectionString;

    fADOCommand:=TADOCommand.Create(nil);
    fADOCommand.Connection:=fADOConnection;

    while not terminated do
    begin
      try
        if not fADOConnection.Connected then
          fADOConnection.Connected:=TRUE;

        fADOCommand.CommandText:=format(UpdatePEWSSQL,[sAppState[fAppState],fAppName]);
        fADOCommand.Execute;

      except
        on e:exception do
        begin
          fADOConnection.Connected:=FALSE;
        end;
      end;

      //sleep(fSleepTime);
      WaitForSingleObject( FEvent, fsleeptime );
      ResetEvent( FEvent );
    end;

    fADOConnection.Connected:=FALSE;
    fADOCommand.Free;
    fADOConnection.Free;
  end;
end;

procedure TPEWSHeartbeatThread.Wakeup;
begin
  // executes in callers thread context
  SetEvent( FEvent );
end;


end.
