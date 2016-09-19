//
//    Name:	ThreadedActivityLog.pas
//    Desc: This component will write to the activity log using the threaded query componenet
//          Set the login params property and then set active to connect to the SQL log. To send
//					a log message set the trans and desciption properties and then call send.
//
//    Author:  David Verespey
//
//    Revision History:
//
//    06/25/97	Start Program
//
//
unit ThreadedActivityLog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ThreadedBDEQuery,
  seqnum, DB, DBTables,ThreadedQueryDef;

type

	TErrorProc = procedure (Sender : TObject; ErrorMsg : string)   	of object;

  TThreadedActivityLog = class(TComponent)
  private
    { Private declarations }
    fSendList: TStringList; //list of current messages to send
    fQuery:TThreadedBDEQuery;
    fAppName: string;
    fIPAddress: string;
    fSequence: string;
    fSequenceNumber: TSequenceNumber;
    fVIN: string;
    fTrans: string;
    fDescription: string;
    fAlias: string;
    //fTable: string;
    fParams: TStringList;
    fSendError:boolean;
    fActive:boolean;
  protected
    { Protected declarations }

    FError:	TErrorProc;			// when an error occurs in the thread.
    FOnComplete: TNotifyEvent;

    // property get and set
    function GetActive: boolean;
    procedure SetActive(Active: boolean);
    function GetAppName: string;
    procedure SetAppName(AppName: string);
    function GetSequence: string;
    procedure SetSequence( Sequence: string );
    function GetVIN: string;
    procedure SetVIN(VIN: string);
    function GetTrans: string;
    procedure SetTrans(Trans: string);
    function GetDescription: string;
    procedure SetDescription(Description: string);
    function GetAlias: string;
    procedure SetAlias(alias: string);
    function GetLoginParams: TStringList;
    procedure SetLoginParams(params: TstringList);
		procedure SetOnComplete(Value: TNotifyEvent);
    function  GetSendCount: integer;

    // notify events
  	procedure SetError(value :  TErrorProc);

    // handle callbacks from threaded query
		procedure QueryComplete(Sender: TObject; Dataset: TDataSet);
		procedure QueryError(Sender: TObject; ErrorMsg: string);
    procedure AfterRun(Sender:TObject; Query:TQuery);

    // internal
		procedure SendError (msg : string);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Send;
  published

    property Active: boolean
    	read GetActive
      write SetActive;

    property AppName: string
    	read GetAppName
      write SetAppName;

    property IPAddress: string
    	read fIPAddress
      write fIPAddress;

    property Sequence: string
    	read GetSequence
      write SetSequence;

    property VIN: string
    	read GetVIN
      write SetVIN;

    property TransCode: string
    	read GetTrans
      Write SetTrans;

    property Description: string
    	read GetDescription
      write Setdescription;

    property AliasName: string
    	read GetAlias
      write SetAlias;

  	property LoginParams : TStringList
    	read GetLoginParams
    	write SetLoginParams;

    property OnError :	TErrorProc
    	read FError
      write SetError;

    property OnComplete: TNotifyEvent
    	read fOnComplete
      write SetOnComplete;

    property SendCount : integer
      read GetSendCount;
  end;


implementation

constructor TThreadedActivityLog.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  fParams:=TStringList.Create;
  fSendList:=TStringList.Create;
  fSequenceNumber:=TSequenceNumber.Create(self);
  fSendError:=False;
end;

destructor TThreadedActivityLog.Destroy;
begin
	fParams.Free;
	fSendList.Free;
	fsequenceNumber.Free;
	inherited destroy;
end;

procedure TThreadedActivityLog.Send;
const
	sqlCommand	=	'Insert into dbo.Act_Log ' +
  							'(App_From,IP_Address,Trans,DT_Sender,VIN,Description,Sequence_Number) ' +
  							'VALUES ( ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s)';
var
	ssQuery			:	string;
  ssDesc      : string;
begin
	// format the message
  try
    repeat
      if length(fDescription)>100 then
      begin
        ssDesc:=copy(fDescription,1,100);
        fdescription:=copy(fDescription,101,length(fDescription)-100);
      end
      else
      begin
        ssDesc:=fDescription;
        fDescription:='';
      end;
      ssQuery := format (sqlCommand,[fAppName, fIPAddress,
      fTrans, formatdatetime('yyyymmddhhmmss00',now),fVIN,ssDesc,fsequence]);
      if assigned(fQuery) and (fSendList.Count = 0) and (not fSendError) and (fActive) then
      begin
		    fQuery.SQL.Clear;
        fQuery.SQL.Add (ssQuery);
        fQuery.Exec;
      end
      else
        fSendList.Add(ssQuery);
    until Length(fDescription) >= 0;
  except
  	on e:exception do
    begin
    	SendError('Act Send:: '+e.message);
    end;
  end;
end;

procedure TThreadedActivityLog.AfterRun(Sender:TObject; Query:TQuery);
var
  I:integer;
begin
	try
		// reset timer if there are any other messages in out cue then send next
  	// and start timer again. If not then leave it off.
    fSendError:=False;
  	if (fSendList.Count > 0) and (not fSendError) and (fActive) then
  	begin
		  fQuery.SQL.Clear;
      if fSendList.Count > 1 then
      begin
        for i:=0 to fSendList.Count-1 do
        begin
  		    fQuery.SQL.Add(fSendList[0]);
    	    fSendList.Delete(0);
        end;
      end
      else
      begin
  		  fQuery.SQL.Add(fSendList[0]);
    	  fSendList.Delete(0);
      end;
    	fQuery.Exec;
  	end;
  	// complete event
		if (Assigned (fOnComplete)) then
  		fOnComplete(self);
  except
  	on e:exception do
    begin
    	SendError('Act Comp:: '+e.message);
    end;
  end;
end;

procedure TThreadedActivityLog.QueryComplete(Sender: TObject; Dataset: TDataSet);
begin
	{try
		// reset timer if there are any other messages in out cue then send next
  	// and start timer again. If not then leave it off.
    fSendError:=False;
		fQuery.SQL.Clear;
  	if (fSendList.Count > 0) and (not fSendError) and (fActive) then
  	begin
  		fQuery.SQL.Add(fSendList[0]);
    	fSendList.Delete(0);
    	fQuery.Exec;
  	end;
  	// complete event
		if (Assigned (fOnComplete)) then
  		fOnComplete(self);
  except
  	on e:exception do
    begin
    	SendError('Act Comp:: '+e.message);
    end;
  end;}
end;

procedure TThreadedActivityLog.QueryError(Sender: TObject; ErrorMsg: string);
begin
  fSendError:=True;
	SendError(ErrorMsg);
end;

function TThreadedActivityLog.GetSendCount: integer;
begin
  result:=fSendlist.Count;
end;

function TThreadedActivityLog.GetActive: boolean;
begin
	result:=fActive;
end;

procedure TThreadedActivityLog.SetActive(Active: boolean);
begin
	fActive:=Active;
  if not Active then
  begin
    if assigned(fQuery) then
    begin
      while (fSendList.Count > 0) or (fQuery.IsRunning) do
      begin
        Application.ProcessMessages;
        sleep(0);
      end;
      fQuery.Free;
      fSendError:=False;
    end;
  end
  else
  begin
    fQuery:=TThreadedBDEQuery.Create(self);
    //Set login and other info
    fQuery.KeepConnection:=false;
    fQuery.qryCommand:=qryExec;
    fQuery.RunMode:=runOneShot;
    fQuery.DriverName:='';
    fQuery.AliasName:=fAlias;
    fQuery.LoginParams.Assign(fParams);

    //point to the callbacks
    fQuery.OnComplete:=QueryComplete;
    fQuery.OnError:=QueryError;
    fQuery.OnAfterRun:=AfterRun;

    if fSendList.Count > 0 then
    begin
  		fQuery.SQL.Add(fSendList[0]);
    	fSendList.Delete(0);
      fQuery.Exec;
    end;
  end;
end;

function TThreadedActivityLog.GetAppName: string;
begin
	result:=fAppName;
end;

procedure TThreadedActivityLog.SetAppName(AppName: string);
begin
	fAppName:=AppName;
end;

function TThreadedActivityLog.GetSequence: string;
begin
	result:=fSequence;
end;

procedure TThreadedActivityLog.SetSequence( Sequence: string );
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

function TThreadedActivityLog.GetVIN: string;
begin
	result:=fVIN;
end;

procedure TThreadedActivityLog.SetVIN(VIN: string);
begin
	fVIN:=VIN;
end;

function TThreadedActivityLog.GetTrans: string;
begin
	result:=fTrans;
end;

procedure TThreadedActivityLog.SetTrans(Trans: string);
begin
	fTrans:=Trans;
end;

function TThreadedActivityLog.GetDescription: string;
begin
	result:=fdescription;
end;

procedure TThreadedActivityLog.SetDescription(Description: string);
begin
	if length(description) < 100 then
		fDescription:=description
  else
  begin
  	fDescription:=copy(description,1,100);
  end;
end;

function TThreadedActivityLog.GetAlias: string;
begin
	result:=fAlias;
end;

procedure TThreadedActivityLog.SetAlias(alias: string);
begin
	fAlias:=alias;
end;

function TThreadedActivityLog.GetLoginParams: TStringList;
begin
	result:=fParams;
end;

procedure TThreadedActivityLog.SetLoginParams(params: TstringList);
begin
	fParams.Assign (params);
end;

procedure TThreadedActivityLog.SetError(value :  TErrorProc   );
begin
	FError:= value;
end;

procedure TThreadedActivityLog.SendError (msg : string);
begin
	if (Assigned (FError)) then
  	FError (self, msg);
end;

procedure TThreadedActivityLog.SetOnComplete(Value: TNotifyEvent);
begin
  FOnComplete := Value;
end;

end.
