unit ADOActivityLog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ADOdb, seqnum, ThreadedADOQuery,ThreadedQueryDef;

type
	TErrorProc = procedure (Sender : TObject; ErrorMsg : string)   	of object;

  TADOActivityLog = class(TComponent)
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
    fVIN:string;
    fSendError:boolean;
    fADOStoredProc:TADOStoredProc;
    fADOConnection: TADOConnection;
    fThreadedQuery: TThreadedADOQuery;
    fQueryList:TStringList;
    fQueryCount:integer;
    msglist: array [0..1000] of string;
    ftop:integer;
    fbottom:integer;
    fconnectionstring:string;
    function GetSequence: string;
    procedure SetSequence( Sequence: string );
    procedure SetConnectionString(connectionstring:string);


		procedure SetOnComplete(Value: TNotifyEvent);
  	procedure SetError(value :  TErrorProc);

    procedure SendError (msg : string);

    procedure BeforeRun(Sender: TObject; Dataset: TADOQuery);
    procedure ADOQueryComplete(Sender: TObject; Dataset: TADOQuery);
    procedure ADOQueryError(Sender: TObject; ErrorMsg: String);

    function  GetConnectionStringItem (index : string) : string;
    procedure SetConnectionStringItem (index : string; value : string);

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Send;

		property ConnectionStringItem [index : string] : string
    	read  GetConnectionStringItem
      write SetConnectionStringItem;

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
      write fNTUser
      stored False;

    property ComputerName: string
    	read fComputerName
      write fComputerName
      stored False;

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

    property VIN: string
      read fVIN
      write fVIN;

    property OnError :	TErrorProc
    	read FError
      write SetError;

    property OnComplete: TNotifyEvent
    	read fOnComplete
      write SetOnComplete;

    property QueryCount: integer
      read fquerycount
      write fQueryCount;

    property ConnectionString: string
      read fconnectionstring
      write SetConnectionString;
  end;

const
  ConnectionStringconst='Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=Activity;Data Source=T22MAD02';
  //ConnectionString='Provider=SQLOLEDB.1;Persist Security Info=False;User ID=sa;Initial Catalog=Activity;Data Source=ISID6052';
  ProcedureName='InsertDetailedAct_Log;1';
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

//Get current Computer name to fill in login box
function CurrentComputerName:String;
var
  u: array[0..127] of Char;
  sz:DWord;
begin
  sz:=SizeOf(u);
  GetComputerName(u,sz);
  Result:=u;
end;

constructor TADOActivityLog.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  fSequenceNumber:=TSequenceNumber.Create(self);
  fSequenceNumber.SequenceValue:=1;
  fComputerName:=CurrentComputerName;
  fNTUser:=CurrentUserName;
  fDTsender:='';
  fSequence:=fSequenceNumber.SequenceNumber;
  //Non Threaded
  fADOConnection:=TADOConnection.Create(self);
  fADOConnection.LoginPrompt:=False;
  fADOConnection.ConnectionString:=ConnectionStringconst;
  fADOStoredProc:=TADOStoredProc.Create(self);
  fADOStoredProc.Connection:=fADOConnection;
  fADOStoredProc.ProcedureName:=ProcedureName;
  //Threaded
  if fThreaded then
  begin
    fThreadedQuery:=TThreadedADOQuery.Create(self);
    fThreadedQuery.ConnectionString:=ConnectionStringconst;
    fThreadedQuery.RunMode:=runContinuous;
    fThreadedQuery.QryCommand:=qryExec;
    fThreadedQuery.KeepConnection:=False;
    fThreadedQuery.TimerInitial:=50;
    fThreadedQuery.TimerRestart:=200;
    fThreadedQuery.OnBeforeRun:=BeforeRun;
    fThreadedQuery.OnComplete:=ADOQueryComplete;
    fThreadedQuery.OnError:=ADOQueryError;
  end;
  fQueryList:=TstringList.Create;
  fQueryCount:=0;
  fSendError:=False;
end;

destructor TADOActivityLog.Destroy;
begin
  fSequenceNumber.Free;
  fADOStoredProc.Free;
  if fThreaded then
  begin
    fThreadedQuery.Free;
  end;
  fQueryList.Free;
	inherited destroy;
end;

procedure TADOActivityLog.SetConnectionString(connectionstring:string);
begin
  if assigned(fADOConnection) then
    fADOConnection.Connectionstring:=connectionstring;
  if assigned(fThreadedQuery) then
    fThreadedQuery.ConnectionString:=connectionstring;

  fconnectionstring:=connectionstring;
end;

procedure TADOActivityLog.Send;
var
  sdesc,desc:string;
begin

  if (fDTSender = '') then
  begin
    fDTSender:=formatdatetime('yyyymmddhhmmsszz',now);
  end;

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
      try
        msglist[fbottom]:='EXEC '+Copy(ProcedureName,1,pos(';',ProcedureName)-1)
                              +' '''+fAppName
                              +''','''+fIPAddress
                              +''','''+fTrans
                              +''','''+fDTSender
                              +''','''+fComputerName
                              +''','''+sdesc
                              +''','''+fNTUser
                              +''','+FloatToStr(fDBTime)
                              +'  ,'''+fVIN
                              +''','+fSequenceNumber.SequenceNumber;
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
      try
        fADOConnection.Connected:=True;
        fADOStoredProc.Parameters.refresh;
        fADOStoredProc.Parameters.ParamByName('@App_From').Value:=fAppName;
        fADOStoredProc.Parameters.ParamByName('@IP_Address').Value:=fIPAddress;
        fADOStoredProc.Parameters.ParamByName('@Trans').Value:=fTrans;
        fADOStoredProc.Parameters.ParamByName('@DT_Sender').Value:=fDTSender;
        fADOStoredProc.Parameters.ParamByName('@ComputerName').Value:=fComputerName;
        fADOStoredProc.Parameters.ParamByName('@Description').Value:=sdesc;
        fADOStoredProc.Parameters.ParamByName('@NTUserName').Value:=fNTUser;
        fADOStoredProc.Parameters.ParamByName('@DB_Time').Value:=fDBTime;
        fADOStoredProc.Parameters.ParamByName('@VIN').Value:=fVIN;
        fADOStoredProc.Parameters.ParamByName('@Sequence_Number').Value:=fSequenceNumber.SequenceNumber;
        fADOStoredProc.ExecProc;
        fADOConnection.Connected:=False;

        if (Assigned (fOnComplete)) then
          fOnComplete(self);
      except
        on e:exception do
        begin
          fADOConnection.Connected:=False;
          SendError('Send ERROR:: '+e.message);
        end;
      end;
    end;
  end;
end;

procedure TADOActivityLog.BeforeRun(Sender: TObject; Dataset: TADOQuery);
begin
  Dataset.SQL.Clear;
  Dataset.SQL.Add(msglist[ftop]);
end;

procedure TADOActivityLog.ADOQueryComplete(Sender: TObject; Dataset: TADOQuery);
begin

  INC(fTop);
  if fTop>msglistsize then
    fTop:=0;
  DEC(fQueryCount);

  // stop query if no more
  if fQueryCount <= 0 then
    fThreadedQuery.Active:=False;

  if (Assigned (fOnComplete)) then
    fOnComplete(self);
end;

procedure TADOActivityLog.ADOQueryError(Sender: TObject; ErrorMsg: String);
begin
  // shutdown query if error
  fThreadedQuery.Active:=False;
  SendError(ErrorMsg);
end;

function TADOActivityLog.GetSequence: string;
begin
	result:=fSequence;
end;

procedure TADOActivityLog.SetSequence( Sequence: string );
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

procedure TADOActivityLog.SetError(value :  TErrorProc   );
begin
	FError:= value;
end;

procedure TADOActivityLog.SetOnComplete(Value: TNotifyEvent);
begin
  FOnComplete := Value;
end;

procedure TADOActivityLog.SendError (msg : string);
begin
	if (Assigned (FError)) then
  	FError (self, msg);
end;




//-------------------------------------------------------
//
//	get and set components of the ConnectionString
//
//	set it this way
//		ActLog.ConnectionItem ['Data Source'] := 'T22MADBU';
//
//
//	get it this way
//
//		var
//    	s : string;
//		begin
//		  s := ActLog.ConnectionItem ['Provider'];
//


type
	TSplitType	=	(spltBefore, spltKey, spltSeparator, spltValue, spltAfter);
	TSplitList	=	array [TSplitType] of string;
function split (src : string; key : string) : TSplitList;
var
	bgn		:	integer;
begin
	result [spltBefore]    := '';
	result [spltKey]       := '';
	result [spltSeparator] := '';
	result [spltValue]     := '';
	result [spltAfter]     := '';

	bgn := pos (UpperCase (key), UpperCase (src));
  if (bgn > 0) then
	begin
		result [spltBefore] := copy (src,   1, bgn-1);
		result [spltKey]    := copy (src, bgn, length (key));

  	inc (bgn, length (key));
    while (bgn <= length (src)) and ((src [bgn] <= ' ') or (src [bgn] = '=')) do
    begin
			result [spltSeparator] := result [spltSeparator] + src [bgn];
			inc (bgn);
		end;

    while (bgn <= length (src)) and (src [bgn] <> ';') do
    begin
			result [spltValue] := result [spltValue] + src [bgn];
			inc (bgn);
		end;

		result [spltAfter] := copy (src, bgn, 9999);
	end;
end;


function  TADOActivityLog.GetConnectionStringItem (index : string) : string;
var
	splitter	:	TSplitList;
begin
	splitter := split (fADOConnection.ConnectionString, index);
	result   := splitter [spltValue];
end;

procedure TADOActivityLog.SetConnectionStringItem (index : string; value : string);
var
	splitter	:	TSplitList;
  wasActive :	boolean;
begin
	splitter := split (fADOConnection.ConnectionString, index);
  if (splitter [spltValue] <> '') then
  begin
  	with fADOConnection do
    begin
			wasActive := Connected;
	    Connected := false;
			ConnectionString := splitter [spltBefore] +  splitter [spltKey] +
			 									  splitter [spltSeparator] + value +
													splitter [spltAfter];
			Connected := wasActive;
		end;

    if fThreaded then
    begin
		  fThreadedQuery.ConnectionString := fADOConnection.ConnectionString;
    end;
  end;
end;



end.

