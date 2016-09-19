unit ADOAppStatus;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, extctrls,threadedADOquery,threadedquerydef;

type
  TADOAppStatus = class(TThreadedADOQuery)
  private
    { Private declarations }
    fAppName: string;
    fAppCount: integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AppName: string
    read fAppName
    write fAppName;
    property AppCount: integer
    read fAppCount
    write fAppCount;
  end;


implementation


constructor TADOAppStatus.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FQryCommand:=qryExec;
  FRunMode:=runContinuous;
  FKeepConnection:=True;
  fAppCount:=0;
  TimerRestart:=15000;
end;

destructor TADOAppStatus.Destroy;
begin
  Inherited destroy;
end;

end.
