unit NUMMICheckBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls;


Type
  TNUMMICheckBox = Class(TCheckBox)
  Private
    { Private declarations }
    FReadOnly: Boolean;
  Protected
    { Protected declarations }
    procedure WndProc(var Message: TMessage); override;
  Public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    function CanFocus:boolean; Override;
  Published
    { Published declarations }
    Property ReadOnly: Boolean Read FReadOnly Write FReadOnly;
  End;

procedure Register;

Implementation


function TNUMMICheckBox.CanFocus: boolean;
begin
  if readonly then
    result:=false
  else
    result:=Inherited CanFocus;
end;

procedure TNUMMICheckBox.WndProc(var Message: TMessage);
begin

  case Message.Msg of
    WM_MOUSEFIRST..WM_MOUSELAST:
         if ReadOnly and (not (csDesigning in ComponentState)) then Exit;
    WM_KEYFIRST..WM_KEYLAST:
         if ReadOnly and (not (csDesigning in ComponentState)) then Exit;
  end;

  Inherited WndProc(Message);

end;

Constructor TNUMMICheckBox.Create(AOwner: TComponent);
Begin
  FReadOnly:= True;
  Inherited Create(AOwner);
End;

procedure Register;
begin
  RegisterComponents('NUMMI Tools', [TNUMMICheckBox]);
end;

end.
