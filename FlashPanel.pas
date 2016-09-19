//
//     Name:   FlashPanel.pas
//     Desc:   FlashPanel Component
//             A panel that flashes between 2 colors
//
//     Author: David Verespey
//
//     Revision History:
//
//     09/01/96    Start Program
//
//
unit FlashPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, emptypanel, ThreadedPanel;

type
  TFlashPanel = class(TThreadedPanel)
  private
    { Private declarations }
    fFlashColor: TColor;
    fColor: TColor;
  protected
    { Protected declarations }
    procedure SetFlashColor(value: TColor);
    procedure UpdateTimer; override;
    procedure Timer; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }

    property FlashColor: TColor
    	read fFlashColor
       write SetFlashColor;
  end;


implementation


constructor TFlashPanel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  FlashColor:=clRed;
  fColor:=Color;
end;

procedure TFlashPanel.SetFlashColor(value: TColor);
begin
	fFlashColor:=value;
end;

procedure TFlashPanel.UpdateTimer;
begin
  if fEnabled then
  begin
    fColor:=Color;
  end;

  Inherited UpdateTimer;

  if not fEnabled then
  begin
       // Reset back to original
    Color:=fColor;
  end;
end;

procedure TFlashPanel.Timer;
begin
  if Color = fColor then
  begin
    Color:=fFlashColor;
  end
  else
  begin
    Color:=fColor;
  end;
end;

end.
