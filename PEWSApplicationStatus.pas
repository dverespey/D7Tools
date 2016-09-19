unit PEWSApplicationStatus;

interface

uses
  SysUtils, Classes;

type
  TPEWSApplicationStatus = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NUMMI Tools', [TPEWSApplicationStatus]);
end;

end.
 