//
//
//		Author:	David Verespey
//		Date:		10/06/97
//
//		Simple registry component
//		Adds an entry into the HKEY_CURRENT_USER tree
//		under the heading software/NUMMI. It then uses the Application and
//		section properties to set the next two levels.
//
//		All data is entered in string format and converted if possible
//		when read.
//
//
//
unit Cregfld;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Registry;

type
  TCregfield = class(TComponent)
  private
    { Private declarations }
    Reg:TRegistry;
    fSection:string;
    fApplication:string;
    fKey:string;
    fDefault:string;
    fRewrite:Boolean;
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
    Procedure SetSection( s : string ); virtual;
    Procedure SetApplication( s : string ); virtual;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

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
    { Published declarations }
    Property ApplicationName : string
             read fApplication
             write SetApplication;
    Property Section : string
             read fSection
             write SetSection;
    Property Key : string
             read fKey
             write fKey;
    Property Default : string
             read fDefault
             write fDefault;
    Property Rewrite : Boolean
             read fRewrite
             write fRewrite;
  end;

implementation

constructor TCRegfield.Create(aOwner: TComponent);
begin
	inherited Create(aOwner);
  Reg:=TRegistry.Create;
end;

destructor TCRegfield.Destroy;
begin
	if not (csDesigning in ComponentState) then
  begin
   	Reg.CloseKey;
  end;
  Reg.Free;
	inherited destroy;
end;

function TCRegfield.GetAsString : string;
Begin
	try
  	result:=Reg.ReadString(fKey);
    if (result='') and (fDefault<> '') then
    begin
   		result:=fDefault;
      if fRewrite then
    	  Reg.WriteString(fKey,fDefault);
    end;
  except
    result:=fDefault;
    raise;
  end;
End;

Function TCRegfield.GetAsBoolean : Boolean;
Begin
	try
  	if Reg.ReadString(fKey) = 'TRUE' then
    begin
    	result:=True;
    end
    else
    begin
    	if Reg.ReadString(fKey) = '' then
      begin
      	if UpperCase(fDefault) = 'TRUE' then
        begin
        	result:=True;
          if fRewrite then
            Reg.WriteString(fKey,'TRUE');
        end
        else
        begin
        	result:=False;
          if fRewrite then
            Reg.WriteString(fKey,'FALSE');
        end;
      end
      else
      begin
      	result:=false;
      end;
    end;
  except
    raise;
  end;
End;

Function TCRegfield.GetAsInteger : LongInt;
Begin
	try
  	result:=StrToInt(Reg.ReadString(fKey));
  except
    raise;
  end;
End;

Function TCRegfield.GetAsDateTime : TDateTime;
Begin
	try
  	result:=StrToDateTime(Reg.ReadString(fKey));
  except
		if Reg.ReadString(fKey)='' then
    begin
    	try
      	result:=StrTodateTime(fDefault);
        Reg.WriteString(fKey,fDefault);
      except
      	result:=now;
        Reg.WriteString(fKey,DateTimeToStr(now));
      end;
    end
    else
    	raise;
  end;
End;

Procedure TCRegfield.SetAsString( s : string );
Begin
	Reg.WriteString(fKey,s);
End;

Procedure TCRegfield.SetAsInteger( i : LongInt );
Begin
	Reg.WriteString(fKey,IntToStr(i));
End;

Procedure TCRegfield.SetAsBoolean( b : Boolean );
Begin
	if b then
  	Reg.WriteString(fKey,'TRUE')
  else
  	Reg.WriteString(fKey,'FALSE');
End;

Procedure TCRegfield.SetAsDateTime( d : TDateTime );
Begin
	Reg.WriteString(fKey,DateTimeToStr(d));
End;

Procedure TCRegfield.SetApplication( s : string );
begin
	fApplication:=s;
  if not (csDesigning in ComponentState) then
  begin
  	Reg.CloseKey;
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
  	if s <> '' then
    begin
      if not Reg.OpenKey('\Software\NUMMI\'+fapplication+'\',true) then
        raise exception.create('Fail on registry key open '+'\Software\NUMMI\'+fapplication+'\');
    end;
  end;
end;

Procedure TCRegfield.SetSection( s : string );
begin
	fSection:=s;
  if not (csDesigning in ComponentState) then
  begin
  	Reg.CloseKey;
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
  	if fApplication <> '' then
    begin
  		if s <> '' then
        if not Reg.OpenKey('\Software\NUMMI\'+fapplication+'\'+fsection,true) then
          raise exception.create('Fail on registry key open '+'\Software\NUMMI\'+fapplication+'\'+fsection);
    end;
  end;
end;

end.
