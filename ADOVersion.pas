unit ADOVersion;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,COMObj,Variants;

type
  TADOVersion = class(TEdit)
  private
    { Private declarations }
    function GetADOVersion: Double;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
  published
    { Published declarations }
    property text stored false;
  end;


implementation


function TADOVersion.GetADOVersion: Double;
var
  ADO: Variant;
begin
  try
    ADO := CreateOLEObject('adodb.connection');
    Result := StrToFloat(ADO.Version);
    ADO := Unassigned;
  except
    Result := 0.0;
  end;
end;

constructor TADOVersion.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  width:=30;
  readonly:=true;
  Text:=FloatToStrF(GetADOVersion,ffFixed,1,1);
end;

end.
