unit Panelmes;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Timerpnl;

type
  TPanelMessage = class(TTimerPanel)
  private
    { Private declarations }
    fDisplayString: String;
  protected
    { Protected declarations }
    procedure DisplayString(Sender: TObject); virtual;
    procedure SetDisplayString(Value: String); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Display: String read fDisplayString write SetDisplayString;
  end;


implementation

procedure TPanelMessage.DisplayString(Sender: TObject);
var
   TempString: string;
begin
   if Enabled then
   begin
       if length(fDisplayString) > 0 then
       begin
           TempString:=Copy(fDisplayString,2,Length(fDisplayString))+fDisplayString[1];
           fDisplayString:=TempString;
           Caption:=TempString;
       end
       else
       begin
           Caption := '';
       end;
   end;
end;

procedure TPanelMessage.SetDisplayString(Value: String);
begin
   fDisplayString := Value;
end;

constructor TPanelMessage.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   fDisplayString:=Name;
   ClockDriver.OnTimer := DisplayString;
end;

end.
