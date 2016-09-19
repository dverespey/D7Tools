unit Cinifld;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, IniLocal;

type
  TCIniField = class(TComponent)
  private
     fSection      : string;
     fKey          : string;
     fDefault      : string;
     fRewrite      : Boolean;
  protected
    { Protected declarations }
    function GetAsString : string ; virtual;
    function GetAsInteger : LongInt; virtual;
    function GetAsBoolean : Boolean; virtual;
    function GetAsDateTime : TDateTime; virtual;

    Procedure SetAsString( s : string ); virtual;
    Procedure SetAsInteger( i : LongInt); virtual;
    Procedure SetAsBoolean( b : Boolean); virtual;
    Procedure SetAsDateTime( d : TDateTime ); virtual;

  public
    { Public declarations }
    Property AsString : string
             read GetAsString
             write SetAsString
             ;
    Property AsInteger : LongInt
             read GetAsInteger
             write SetAsInteger
             ;
    Property AsBoolean : Boolean
             read GetAsBoolean
             write SetAsBoolean
             ;
    Property AsDateTime : TDateTime
             read GetAsDateTime
             write SetAsDateTime
             ;

  published
    Property Section : string
             read fSection
             write fSection;
    Property Key : string
             read fKey
             write fKey;
    Property Default : string
             read fDefault
             write fDefault;
    Property Rewrite : Boolean
             read fRewrite
             write fRewrite
             ;
  end;


implementation

function TCIniField.GetAsString : string;
var
  err:boolean;
Begin
  err:=False;
  With LocalIni Do Try
    Result := ReadString(fSection,fKey,'ERR!ERR');

    if Result='ERR!ERR' then
    begin
      Result:=fDefault;
      err:=true;
    end;

    If fRewrite and err Then
      WriteString(fSection,fKey,fDefault);

  Finally
    Free
  End;
End;

Function TCIniField.GetAsBoolean : Boolean;
var
  err:boolean;
  val:string;
Begin
  err:=False;
  With LocalIni Do
  begin
    Try
      val := ReadString(fSection,fKey,'ERR!ERR');

      if val='ERR!ERR' then
      begin
        val:=fDefault;
        err:=true;
      end;

      Result := (UpperCase(val) = 'TRUE');
      if Rewrite and err then
        WriteString(fSection,fKey,fDefault);
    Finally
      Free
    End;
  end;
End;

Function TCIniField.GetAsInteger : LongInt;
var
  err:boolean;
  val:longint;
Begin
  err:=False;
  With LocalIni Do
  begin
    Try
      val:=ReadInteger(fSection,fKey,-1);

      if val=-1 then
      begin
        val:=strtoint(fdefault);
        err:=true;
      end;

      result:=val;

      If fRewrite and err Then
        WriteInteger(fSection,fKey,strtoint(fDefault));

    Finally
      Free
    End;
  End;
End;

Function TCIniField.GetAsDateTime : TDateTime;
var
  err:boolean;
  val:string;
Begin
  err:=False;
  With LocalIni Do
  begin
    Try
      val := ReadString(fSection,fKey,'ERR!ERR');

      if val='ERR!ERR' then
      begin
        val:=fDefault;
        err:=true;
      end;

      Result := StrToDateTime(val);

      If Rewrite and err Then
        WriteString(fSection,fKey,fDefault);
    Finally
      Free
    End;
  end;
End;

Procedure TCIniField.SetAsString( s : string );
Begin
   With LocalIni Do Try
      WriteString(fSection,fKey,s);
   Finally
      Free
   End;
End;

Procedure TCIniField.SetAsInteger( i : LongInt );
Begin
   With LocalIni Do Try
      WriteInteger(fSection,fKey,i);
   Finally
      Free
   End;
End;

Procedure TCIniField.SetAsBoolean( b : Boolean );
Begin
   With LocalIni Do Try
      If b
      Then AsString := 'True'
      Else AsString := 'False'
   Finally
      Free
   End
End;

Procedure TCIniField.SetAsDateTime( d : TDateTime );
Var
   s : String;
Begin
   With LocalIni Do Try
      s := DateTimeToStr(d);
      AsString := s
   Finally
      Free
   End
End;

end.
