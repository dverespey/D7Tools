(*
   Example of usage

       Ini1: TIni;

       Ini1.FileName:='d:\path\foo.ini';
       ini1.enabled:=true;
       ini1.find('section','item','default');
       result:=ini1.AsString;
       ini1.enabled:=false;
*)

unit Ini;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, IniFiles;

type
  EIniConversion = class(Exception);
  TIni = class(TComponent)
  private
    { Private declarations }
    fIniFile: TIniFile;
    fFileName: string;
    fSection: string;
    fKey: string;
    fValue: string;
    fEnabled: Boolean;
    fYes    : TStringList;
    fNo     : TStringList;
    fOnCreateFailure: TNotifyEvent;
    fConvertedFalse : string;
    fConvertedTrue  : string;
  protected
    { Protected declarations }
    procedure SetBoolean(aValue: Boolean);Virtual;
    procedure SetInteger(aValue: LongInt);Virtual;
    procedure SetString(aValue: string);Virtual;
    procedure SetKey(aValue: string);Virtual;
    procedure SetEnabled(aValue: Boolean);Virtual;
    procedure SetDateTime(aValue: TDateTime);Virtual;
    function  GetBoolean: Boolean;Virtual;
    function  GetInteger: LongInt;Virtual;
    function  GetString: string;Virtual;
    function  GetDateTime: TDateTime;Virtual;
    procedure SetFilename(aFileName: string);Virtual;
    function FindIniFile(Ini: string):string;Virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Free;
    procedure Load;
    procedure Post;
    function Find(aSection, aKey, aDefault: string): String;
    procedure Store(aSection, aKey, aValue: string);
    {------------------------ Public properties ----------------------------}
    {---
     --- ONLY AVAILABLE AT RUN TIME
     ---
    }
    property AsBoolean :  Boolean
             read GetBoolean
             write SetBoolean;
    property AsInteger : LongInt
             read GetInteger
             write SetInteger;
    property AsDateTime : TDateTime
             read GetDateTime
             write SetDateTime;
    property AsString : string
             read GetString
             write SetString;
published
    { Published declarations }
    property Name;
    property Tag;
    property Section : string
             read fSection
             write fSection;
    property Key : string
             read fKey
             write SetKey;
    property Enabled : Boolean
             read fEnabled
             write SetEnabled;
    property FileName : string
             read fFilename
             write SetFilename;
    property TokensTrue  : TStringList
             read fYes
             write fYes;
    property TokensFalse : TStringList
             read fNo
             write fNo;
    property ConvertedFalse : string
             read fConvertedFalse
             write fConvertedFalse;
    property ConvertedTrue : string
             read fConvertedTrue
             write fConvertedTrue;
    { Events }
    property OnCreateFailure: TNotifyEvent
             read fOnCreateFailure
             write fOnCreateFailure;
end;

{ Behaves like TIni, but does NOT reference the
  win.ini file. A blank file name will force the
  object to be disabled.
}
TIniNoWin = class(TIni)
protected
   procedure SetFileName( aFileName : string); override;
public
   property AsBoolean;
   property AsInteger;
   property AsDateTime;
   property AsString;
published
   property Name;
   property Tag;
   property Section;
   property Key;
   property Enabled;
   property FileName;
end;


procedure Register;

implementation

procedure Register;
begin
   RegisterComponents('System', [TIni,TIniNoWin]);
end;

destructor TIni.Destroy;
begin
  if Assigned(fYes) then
    fYes.Free;
  if Assigned(fNo) then
    fNo.Free;
  //if Assigned(fIniFile) then
    //fIniFile.Free;
  inherited Destroy;
end;

constructor Tini.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   fEnabled := False;

   if fFileName = '' then
   begin
      FileName:='WIN.INI';
   end
   else
   begin
      FileName:=fFileName;
   end;

   fConvertedFalse := 'False';
   fConvertedTrue := 'True';
   if not Assigned(fYes) then
   begin
      fYes := TStringList.Create;
      fYes.Clear;
      fYes.Add('True');
      fYes.Add('true');
      fYes.Add('Yes');
      fYes.Add('yes');
      fYes.Add('1');
      fYes.Add('Y');
      fYes.Add('y');
   end;

   if not Assigned(fNo) then
   begin
      fNo := TStringList.Create;
      fNo.Clear;
      fNo.Add('False');
      fNo.Add('false');
      fNo.Add('No');
      fNo.Add('no');
      fNo.Add('0');
      fNo.Add('N');
      fNo.Add('n');
   end;
end;

function TIni.Find(aSection, aKey, aDefault: string): String;
begin
   Section := aSection;
   Key := aKey;
   AsString := aDefault;
   Load;
   result:=fValue;
end;

procedure TIni.Store(aSection, aKey, aValue: string);
begin
   Section := aSection;
   Key := aKey;
   AsString := aValue;
   Post;
end;

procedure TIni.Free;
begin
   if Enabled then
   begin
      Post;
      fIniFile.Free;
   end;
   //fYes.Free;
   //fNo.Free;
   inherited free;
end;

procedure TIni.SetBoolean(aValue: Boolean);
begin
   case aValue of
     True: fValue := fConvertedTrue;
     False: fValue := fConvertedFalse;
   end;
end;

procedure TIni.SetInteger(aValue: LongInt);
begin
   fValue := IntToStr(aValue)
end;

procedure TIni.SetString(aValue: string);
begin
   fValue := aValue;
end;

function TIni.GetBoolean: Boolean;
var
   Index : Word;
begin
   for Index := 0 to fYes.Count - 1 do
   begin
      if fYes.Strings[Index] = fValue then
      begin
         Result := True;
         Exit;
      end;
   end;
   for Index := 0 to fNo.Count - 1 do
   begin
      if fNo.Strings[Index] = fValue then
      begin
         Result := False;
         Exit;
      end
   end;
//   Raise EIniConversion.Create('Can not convert ''' + fValue + ''' to boolean');
   Result := False;
end;

function TIni.GetInteger: LongInt;
begin
   Result := StrToInt(fValue)
end;

function TIni.GetString: string;
begin
   Result := fValue
end;

procedure TIni.SetFileName(aFilename: string);
begin
   if fEnabled then
   begin
      ShowMessage('You can not change the file name of'+chr(13)+chr(10)
                 +'an INI component while it is active');
      exit;
   end;

   if aFileName = '' then aFileName := 'WIN.INI';

   { Expand the file with its path info }
   fFileName:=FindIniFile(aFileName);
   if length(fFileName)=0 then
   begin
     if Assigned(fOnCreateFailure) then fOnCreateFailure(self);
   end;

   fIniFile.Free;
   fIniFile := TIniFile.Create(fFileName);
end;

procedure TIni.Post;
begin
  if fEnabled and (fKey <> '') then
    try
      fIniFile.WriteString(fSection, fKey, fValue);
    except
    end;
end;

function TIni.GetDateTime: TDateTime;
begin
   Result := StrToDateTime(fValue)
end;

procedure TIni.SetDateTime(aValue: TDateTime);
begin
   fValue := DateTimeToStr(aValue);
end;

procedure TIni.Load;
begin
   if fKey = '' then exit;
   if fSection = '' then exit;
   if fFileName = '' then exit;
   if fEnabled then
      fValue := fIniFile.ReadString(fSection, fKey, fValue)
end;

procedure TIni.SetKey(aValue: string);
begin
   fKey := aValue;
end;

procedure TIni.SetEnabled(aValue: Boolean);
begin
   if aValue = fEnabled then exit;

   if fEnabled = True then { disable the ini file }
   begin
      fIniFile.Free;
      fEnabled := False;
   end
   else
   begin
      if fFileName = '' then fFileName := 'WIN.INI';

      fFileName:=FindIniFile(fFileName);

      fEnabled := True; { Enable the ini file }
      fIniFile := TIniFile.Create(fFileName);
   end
end;

{ *************************************************************** }
  function TIni.FindIniFile(Ini: string):string;
  var
     WinDirP, SysDirP: array [0..255] of char;
     WinDir, SysDir: string;
     Size: Integer;
     CurPath: String;
  begin
    Size:= 254;

    { First we should see if the INI file already has an explicit path
      prepended to it. If so then we should just assign the return to
      the input and exit }
    if not (pos('\', ini)=0) then
    begin
        Result:=Ini;
        exit;
    end
    else
        Result:='';



    CurPath:=ExtractFilePath(Application.ExeName);
    if  NOT FileExists(CurPath + Ini) then
    begin
        GetWindowsDirectory(WinDirP, Size);
        WinDir:=string(WinDirP);
        if  NOT FileExists(WinDir + '\'+ Ini) then
        begin
            GetSystemDirectory(SysDirP, Size);
            SysDir:=string(SysDirP);
            if  NOT FileExists(SysDir + '\' + Ini) then
            begin
               Result:='';
            end
            else
            begin { Found it in the Windows System directory }
                result:= SysDir + '\' + ini;
            end;
        end
        else
        begin { Found it in the Windows Root }
           result:= WinDir + '\' + ini;
        end;
    end
    else
    begin { Found it in the CurPath }
        result:= CurPath + ini;
    end;
  end;
{================================================================
 This alters the behaviour of TIni so that it does not use
 win.ini. Instead, it will disable the TIni instance if the
 file name is zero length
 ================================================================ }
procedure TIniNoWin.SetFileName(aFilename: string);
begin
   if aFileName = '' then begin
      SetEnabled( False );
      Exit;
   end;
   if fEnabled then begin
      ShowMessage('You can not change the file name of'+chr(13)+chr(10)
                 +'an INI component while it is active');
      exit;
   end;
   fFileName := aFileName;
   fIniFile.Free;
   fIniFile := TIniFile.Create(fFileName);
end;


end.
