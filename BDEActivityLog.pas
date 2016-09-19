unit BDEActivityLog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ThreadedBDEQuery,
  seqnum, DB, DBTables,ThreadedQueryDef;

type
	TErrorProc = procedure (Sender : TObject; ErrorMsg : string)   	of object;

  TBDEActivityLog = class(TComponent)
  private
    FError:	TErrorProc;			// when an error occurs in the thread.
    FOnComplete: TNotifyEvent;

    { Private declarations }
    fActive:boolean;
    fThreaded:boolean;
    fAppName: string;
    fIPAddress: string;
    fSequence: string;
    fSequenceNumber: TSequenceNumber;
    fNTUser: string;
    fComputerName:string;
    fDTSender:string;
    fDBTime:extended;
    fTrans: string;
    fDescription: string;
    fSendError:boolean;
    fBDEQuery:TQuery;
    fDatabase: TDatabase;
    fThreadedQuery: TThreadedBDEQuery;
    //fQueryList:TStringList;
    fQueryCount:integer;
    fAlias:string;
    fUsername:string;
    fPassword:string;
    msglist: array [0..1000] of string;
    ftop:integer;
    fbottom:integer;
    function GetSequence: string;
    procedure SetSequence( Sequence: string );


		procedure SetOnComplete(Value: TNotifyEvent);
  	procedure SetError(value :  TErrorProc);

    procedure SendError (msg : string);

    procedure BeforeRun(Sender: TObject; Dataset: TQuery);
    procedure BDEQueryComplete(Sender: TObject; Dataset: TDataset);
    procedure BDEQueryError(Sender: TObject; ErrorMsg: String);
    procedure INIT;

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Send;
  published
    { Published declarations }
    property Active: boolean
    	read fActive
      write fActive;

    property Threaded: boolean
      read fThreaded
      write fThreaded;

    property AppName: string
    	read fAppName
      write fAppName;

    property IPAddress: string
    	read fIPAddress
      write fIPAddress;

    property Sequence: string
    	read GetSequence
      write SetSequence;

    property NTUser: string
    	read fNTUser
      write fNTUser;

    property ComputerName: string
    	read fComputerName
      write fComputerName;

    property DTSender: string
    	read fDTSender
      write fDTSender;

    property DBTime: extended
    	read fDBTime
      write fDBTime;

    property Trans: string
    	read fTrans
      Write fTrans;

    property Description: string
    	read fDescription
      write fDescription;

    property OnError :	TErrorProc
    	read FError
      write SetError;

    property OnComplete: TNotifyEvent
    	read fOnComplete
      write SetOnComplete;

    property QueryCount: integer
      read fquerycount
      write fQueryCount;

    property Alias:string
      read fAlias
      write fAlias;

    property UserName:string
      read fUsername
      write fUsername;

    property Password:string
      read fPassword
      write fPassword;
  end;

const
  ProcedureName='EXEC InsertDetailedAct_Log ''%s'',''%s'',''%s'',''%s'',''%s'',''%s'',''%s'',%f';
  msglistsize = 1000;

implementation

//Get current NT User name to fill in login box
function CurrentUserName:String;
var
  u: array[0..127] of Char;
  sz:DWord;
begin
  sz:=SizeOf(u);
  GetUserName(u,sz);
  Result:=u;
end;

//Get current NT User name to fill in login box
function CurrentComputerName:String;
var
  u: array[0..127] of Char;
  sz:DWord;
begin
  sz:=SizeOf(u);
  GetComputerName(u,sz);
  Result:=u;
end;

constructor TBDEActivityLog.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  fSequenceNumber:=TSequenceNumber.Create(self);
  fSequenceNumber.SequenceValue:=1;
  fComputerName:=CurrentComputerName;
  fNTUser:=CurrentUserName;
  fSequence:=fSequenceNumber.SequenceNumber;
  //fQueryList:=TstringList.Create;
  ftop:=0;
  fbottom:=0;
  fQueryCount:=0;
  fSendError:=False;
end;

destructor TBDEActivityLog.Destroy;
begin
  fSequenceNumber.Free;
  fBDEQuery.Free;
  fDatabase.Free;
  fThreadedQuery.Free;
  //fQueryList.Free;
	inherited destroy;
end;

procedure TBDEActivityLog.INIT;
begin
  //Non Threaded
  fDatabase:=TDatabase.Create(self);
  fDatabase.LoginPrompt:=False;
  fDatabase.AliasName:=fAlias;
  fDatabase.DatabaseName := 'DB_' + inttostr (integer (fDatabase));
  fDatabase.Params.Clear;
  fDatabase.Params.Add('USER NAME='+fUsername);
  fDatabase.Params.Add('PASSWORD='+fPassword);
  fBDEQuery:=TQuery.Create(self);
  fBDEQuery.DatabaseName:=fDatabase.DatabaseName;

  //Threaded
  fThreadedQuery:=TThreadedBDEQuery.Create(self);
  fThreadedQuery.AliasName:=fAlias;
  fThreadedQuery.LoginParams.Add('USER NAME='+fUsername);
  fThreadedQuery.LoginParams.Add('PASSWORD='+fPassword);
  fThreadedQuery.RunMode:=runContinuous;
  fThreadedQuery.QryCommand:=qryExec;
  fThreadedQuery.KeepConnection:=False;
  fThreadedQuery.TimerInitial:=50;
  fThreadedQuery.TimerRestart:=200;
  fThreadedQuery.OnBeforeRun:=BeforeRun;
  fThreadedQuery.OnComplete:=BDEQueryComplete;
  fThreadedQuery.OnError:=BDEQueryError;
end;

procedure TBDEActivityLog.Send;
var
  sdesc,desc:string;
begin
  desc:=fDescription;
  while length(desc) > 0 do
  begin
    if length(desc) > 100 then
    begin
      sdesc:=copy(desc,1,100);
      desc:=copy(desc,101,length(desc)-100);
    end
    else
    begin
      sdesc:=desc;
      desc:='';
    end;
    if fThreaded then
    begin
      if not assigned(fThreadedQuery) then
        INIT;
      try
        //fQueryList.Add('EXEC InsertDetailedAct_Log '''+fAppName+''','''+fIPAddress+''','''+fTrans+''','''+fDTSender+''','''+fComputerName+''','''+sdesc+''','''+fNTUser+''','+FloatToStr(fDBTime));
        msglist[fbottom]:='EXEC InsertDetailedAct_Log '''+fAppName+''','''+fIPAddress+''','''+fTrans+''','''+fDTSender+''','''+fComputerName+''','''+sdesc+''','''+fNTUser+''','+FloatToStr(fDBTime);
        INC(fbottom);
        if fbottom>msglistsize then
          fbottom:=0;
        INC(fQueryCount);
        if (not fThreadedQuery.Active) and fActive then
        begin
          fThreadedQuery.Active:=True;
        end;
      except
        on e:exception do
        begin
          SendError('Send ERROR:: '+e.message);
        end;
      end;
    end
    else
    begin
      if not assigned(fDatabase) then
        INIT;
      try
        if fActive then
        begin
          fDatabase.Connected:=True;
          fBDEquery.SQL.Clear;
          if DTSender = '' then
            DTSender:=formatdatetime('yyyymmddhhmmss00',now);
          FBDEQuery.SQL.Add(format(ProcedureName,[fAppName,IPAddress,fTrans,fDTSender,fComputerName,sdesc,fNTUser,fDBTime]));
          fBDEQuery.ExecSQL;
          fDatabase.Connected:=False;

        if (Assigned (fOnComplete)) then
          fOnComplete(self);
        end;
      except
        on e:exception do
        begin
          fDatabase.Connected:=False;
          SendError('Send ERROR:: '+e.message);
        end;
      end;
    end;
  end;
end;


procedure TBDEActivityLog.BeforeRun(Sender: TObject; Dataset: TQuery);
begin
  Dataset.SQL.Clear;
  //Dataset.SQL.Add(fQueryList.Strings[0]);
  Dataset.SQL.Add(msglist[ftop]);
end;

procedure TBDEActivityLog.BDEQueryComplete(Sender: TObject; Dataset: TDataset);
begin
  // delete after run, save if error
  //fQueryList.Delete(0);
  INC(fTop);
  if fTop>msglistsize then
    fTop:=0;
  DEC(fQueryCount);

  // stop query if no more, if not pull next and send
  if fQueryCount <= 0 then
    fThreadedQuery.Active:=False;
  //else
  //begin
    //fThreadedQuery.SQL.Clear;
    //fThreadedQuery.SQL.Add(fQueryList.Strings[0]);
    //fThreadedQuery.SQL.Add(msglist[ftop]);
  //end;
  if (Assigned (fOnComplete)) then
    fOnComplete(self);
end;

procedure TBDEActivityLog.BDEQueryError(Sender: TObject; ErrorMsg: String);
begin
  // shutdown query if error
  fThreadedQuery.Active:=False;
  SendError(ErrorMsg);
end;

function TBDEActivityLog.GetSequence: string;
begin
	result:=fSequence;
end;

procedure TBDEActivityLog.SetSequence( Sequence: string );
begin
	if Sequence <> '' then
  begin
  	try
			fSequenceNumber.SequenceNumber:=Sequence;
			fSequence:=fSequenceNumber.SequenceNumber;
    except
  		fSequence:='0001';
      SendError('Invalid sequence number');
    end;
  end
  else
  begin
  	fSequence:='0001';
  end
end;

procedure TBDEActivityLog.SetError(value :  TErrorProc   );
begin
	FError:= value;
end;

procedure TBDEActivityLog.SetOnComplete(Value: TNotifyEvent);
begin
  FOnComplete := Value;
end;

procedure TBDEActivityLog.SendError (msg : string);
begin
	if (Assigned (FError)) then
  	FError (self, msg);
end;


end.
