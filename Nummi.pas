//
//    Name:	Nummi.pas
//    Desc: Register all nummi controls, and property or component editors
//
//    Author:  David Verespey
//
//
//
unit Nummi;


interface

uses
  Classes, Windows, Dialogs, DB, DBTables,Designeditors,designintf,{DsgnIntf, }seqnum, panelmes,AlarmPnl,
  PanelClk, LitButt, BlnkButt,
  CIniFld, Ini, FlashPanel, ThreadedEventPanel,
  TitlePanel, PCPanel, History, Memoappend, Cregfld, BeepSpeaker, ThreadedBDEQuery,
  ThreadedActivityLog,NummiTime, ThreadedADOQuery, ADOActivityLog, ADOConed, ADOdb,
  ADOappStatus,BDEAppStatus,ADOVersion,BDEActivityLog,ADU_200_Comp;

type
	TDriverNameProperty = class (TStringProperty)
   	function  GetAttributes : TPropertyAttributes; override;
    	procedure GetValues (proc : TGetStrProc); override;
	end;
	TAliasNameProperty = class (TStringProperty)
   	function  GetAttributes : TPropertyAttributes; override;
    	procedure GetValues (proc : TGetStrProc); override;
   end;
	TTableNameProperty = class (TStringProperty)
   	function  GetAttributes : TPropertyAttributes; override;
    	procedure GetValues (proc : TGetStrProc); override;
   end;

  TConnectionStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

{ TConnectionStringProperty }

function TConnectionStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TConnectionStringProperty.Edit;
begin
  if EditConnectionString(GetComponent(0) as TComponent) then
    Modified;
end;

function  TDriverNameProperty.GetAttributes : TPropertyAttributes;
begin
	result := [paValueList];
end;

procedure TDriverNameProperty.GetValues (proc : TGetStrProc);
var
	AliasList:TStringList;
	ix:integer;
begin
	AliasList := TStringList.Create;
  	Session.GetDriverNames (AliasList);
  	for ix := 0 to AliasList.Count - 1 do
  		proc (AliasList [ix]);
end;

function  TAliasNameProperty.GetAttributes : TPropertyAttributes;
begin
	result := [paValueList];
end;

procedure TAliasNameProperty.GetValues (proc : TGetStrProc);
var
	AliasList:TStringList;
	ix:integer;
begin
	AliasList := TStringList.Create;
  	Session.GetAliasNames (AliasList);
  	for ix := 0 to AliasList.Count - 1 do
  		proc (AliasList [ix]);
end;

function  TTableNameProperty.GetAttributes : TPropertyAttributes;
begin
	result := [paValueList];
end;

procedure TTableNameProperty.GetValues (proc : TGetStrProc);
var
	TableList:TStringList;
   Log: TThreadedActivityLog;
	ix:integer;
begin
	TableList := TStringList.Create;
	Log := GetComponent(0) as TThreadedActivityLog;
  	Session.GetTableNames (Log.AliasName,'*.*',false,false,TableList);
  	for ix := 0 to TableList.Count - 1 do
  		proc (TableList [ix]);
end;

procedure Register;
begin
  	RegisterComponents('NUMMI Tools', [ TSequenceNumber,
   									                    TPanelMessage,
                                        TAlarmPanel,
                                        TPanelClock,
                                        TIndicatorButton,
                                        TBlinkingIndicatorButton,
                                        TADU200,
                                        //TMarquee,
                                        THistory,
                                        TMemoUpdate,
                                        TCIniField,
                                        TCRegField,
                                        TIni,
                                        TFlashPanel,
                                        TTitlePanel,
                                        //TMrPLC,
                                        TBeepSpeaker,
                                        TThreadedBDEQuery,
                                        TThreadedADOQuery,
                                        TADOActivityLog,
                                        TBDEActivityLog,
                                        TThreadedActivityLog,
                                        TNummiTime,
                                        TBDEAppStatus,
                                        TADOAppStatus,
                                        TADOVersion ]);
  RegisterPropertyEditor (typeinfo (string), TThreadedBDEQuery, 'DriverName', TDriverNameProperty);
  RegisterPropertyEditor (typeinfo (string), TThreadedBDEQuery, 'AliasName', TAliasNameProperty);
  RegisterPropertyEditor (typeinfo (string), TThreadedActivityLog, 'AliasName', TAliasNameProperty);
  RegisterPropertyEditor (typeinfo (string), TThreadedActivityLog, 'TableName', TTableNameProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TThreadedADOQuery, 'ConnectionString', TConnectionStringProperty);
end;

end.
