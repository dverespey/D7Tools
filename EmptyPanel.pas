unit emptypanel;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  	ExtCtrls, StdCtrls;

type
	TEmptyPanel = class(TPanel)
	public
		constructor Create(AOwner: TComponent); override;
	end;

implementation

//
//
//	Empty Panel base class
//
//
constructor TEmptyPanel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   Caption := ' ';
end;

end.
