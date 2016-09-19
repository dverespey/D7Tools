unit NUMMIComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls;

type
  TNUMMIComboBox = class(TComboBox)
  private
    { Private declarations }
    FReadOnly: Boolean;
  protected
    { Protected declarations }
    procedure WndProc(var Message: TMessage); override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    function CanFocus:boolean; Override;
  published
    { Published declarations }
    Property ReadOnly: Boolean Read FReadOnly Write FReadOnly;
  end;

procedure Register;

implementation

function TNUMMIComboBox.CanFocus: boolean;
begin
  if readonly then
    result:=false
  else
    result:=Inherited CanFocus;
end;

procedure TNUMMIComboBox.WndProc(var Message: TMessage);
begin

  case Message.Msg of
    WM_MOUSEFIRST..WM_MOUSELAST:
         if ReadOnly and (not (csDesigning in ComponentState)) then Exit;
    WM_KEYFIRST..WM_KEYLAST:
         if ReadOnly and (not (csDesigning in ComponentState)) then Exit;
  end;

  Inherited WndProc(Message);

end;

Constructor TNUMMIComboBox.Create(AOwner: TComponent);
Begin
  FReadOnly:= True;
  Inherited Create(AOwner);
End;

procedure Register;
begin
  RegisterComponents('NUMMI Tools', [TNUMMIComboBox]);
end;

end.
