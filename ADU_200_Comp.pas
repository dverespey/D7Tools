unit ADU_200_Comp;

interface

uses
  Windows, SysUtils, Classes, Dialogs;

const
  ADU_LIB_C         = 'aduhid.dll';
  ADU_TIMEOUT_C     = 1000; {Milliseconds}
  ADU_NULL_HANDLE_C = 0;
  PROC_CLOSE_C      = 'CloseAduDevice';
  PROC_OPEN_C       = 'OpenAduDevice';
  PROC_READ_C       = 'ReadAduDevice';
  PROC_WRITE_C      = 'WriteAduDevice';

type
  TRelayState = 0..1;

  TIntPtr = ^LongInt;

  TCloseAduDevice = function(aduHandle: integer) : integer; stdcall;

  TOpenAduDevice  = function(iTimeout: integer) : integer; stdcall;

  TReadAduDevice  = function(aduHandle: integer;
                             buffer: PChar;
                             NumberOfBytesToRead: integer;
                             BytesRead: TIntPtr;
                             Timeout: integer) : integer; stdcall;

  TWriteAduDevice = function(aduHandle: integer;
                             Command: PChar;
                             NumberOfBytesToWrite: integer;
                             BytesWritten: TIntPtr;
                             Timeout: integer) : integer; stdcall;

  TADU200 = class(TComponent)
  private
    Proc_Close: TCloseAduDevice;
    Proc_Open:  TOpenAduDevice;
    Proc_Read:  TReadAduDevice;
    Proc_Write: TWriteAduDevice;

    FByteCount: Integer;
    FCmdPtr:    array [0..7] of char;
    FErrorCmd:  String;
    FErrorNum:  Integer;
    FHndl:      THandle;
    FLibHndl:   THandle;
    FRelays:    array [0..7] of char;
  protected
    function  Get_Bank : String;
    function  Get_ErrorText : String;
    function  Get_Relay01 : TRelayState;
    function  Get_Relay02 : TRelayState;
    function  Get_Relay03 : TRelayState;
    function  Get_Relay04 : TRelayState;
    function  Get_Relay05 : TRelayState;
    function  Get_Relay06 : TRelayState;
    function  Get_Relay07 : TRelayState;
    function  Get_Relay08 : TRelayState;
    function  Read : boolean;
    function  Read_Relays : boolean;
    function  Get_Input0 : integer;
    function  Get_Input1 : integer;
    function  Get_Input2 : integer;
    function  Get_Input3 : integer;
    function  Get_StatusInput0 : boolean;
    function  Get_StatusInput1 : boolean;
    function  Get_StatusInput2 : boolean;
    function  Get_StatusInput3 : boolean;
    procedure Set_Bank(Value: String);
    procedure Set_Relay01(Value: TRelayState);
    procedure Set_Relay02(Value: TRelayState);
    procedure Set_Relay03(Value: TRelayState);
    procedure Set_Relay04(Value: TRelayState);
    procedure Set_Relay05(Value: TRelayState);
    procedure Set_Relay06(Value: TRelayState);
    procedure Set_Relay07(Value: TRelayState);
    procedure Set_Relay08(Value: TRelayState);
    function  Write(cmd: string) : boolean;
  public
    property Bank : String read Get_Bank write Set_Bank;
    property ByteCount: Integer read FByteCount;
    property ErrorCmd: String read FErrorCmd;
    property ErrorNum: Integer read FErrorNum;
    property ErrorText: String read Get_ErrorText;
    property Relay01 : TRelayState read Get_Relay01 write Set_Relay01;
    property Relay02 : TRelayState read Get_Relay02 write Set_Relay02;
    property Relay03 : TRelayState read Get_Relay03 write Set_Relay03;
    property Relay04 : TRelayState read Get_Relay04 write Set_Relay04;
    property Relay05 : TRelayState read Get_Relay05 write Set_Relay05;
    property Relay06 : TRelayState read Get_Relay06 write Set_Relay06;
    property Relay07 : TRelayState read Get_Relay07 write Set_Relay07;
    property Relay08 : TRelayState read Get_Relay08 write Set_Relay08;
    property Input0 : integer read Get_Input0;
    property Input1 : integer read Get_Input1;
    property Input2 : integer read Get_Input2;
    property Input3 : integer read Get_Input3;
    property StatusInput0 : boolean read Get_StatusInput0;
    property StatusInput1 : boolean read Get_StatusInput1;
    property StatusInput2 : boolean read Get_StatusInput2;
    property StatusInput3 : boolean read Get_StatusInput3;
    procedure   Close;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    Open(time_out: integer = 1000) : boolean;
    procedure Toggle_Relay(Value: TRelayState; Relay_No: Integer);
  end;

implementation

function TADU200.Read : boolean;
var
  status:  integer;
begin

  Try
    if FHndl = ADU_NULL_HANDLE_C then
      Open;

    status := Proc_Read(FHndl, FRelays, length(FRelays), @FByteCount, ADU_TIMEOUT_C);
  Except
    on E:Exception Do
    begin
      MessageDlg('Read Error: ' + E.Message, mtWarning, [mbOk], 0);
      status := 0;
    end;
  End;

  result := (status = 1);
  if not result then
  begin
    FErrorCmd := 'Read';
    FErrorNum := GetLastError;
    MessageDlg(ErrorText, mtError, [mbOk], 0);
    Exit;
  end;

end;


function TADU200.Write(cmd: string) : boolean;
var
  status:  integer;
begin

  Try
    if FHndl = ADU_NULL_HANDLE_C then
      Open;

    StrPCopy(FCmdPtr, cmd);
    status := Proc_Write(FHndl, FCmdPtr, length(FCmdPtr), @FByteCount, ADU_TIMEOUT_C);
  Except
    status := 0;
  End;

  result := (status = 1);
  if not result then
  begin
    FErrorCmd := 'Write: "' + cmd + '"';
    FErrorNum := GetLastError;
  end;

end;


procedure TADU200.Toggle_Relay(Value: TRelayState; Relay_No: Integer);
var
  Pre: String;
begin
  if Value = 0 then
    Pre := 'RK'
  else
    Pre := 'SK';

  Write(Pre + IntToStr(Relay_No));
end;


procedure TADU200.Close;
begin
  if FHndl <> ADU_NULL_HANDLE_C then
  begin
    Proc_Close(FHndl);
    FHndl := 0;
  end;
end;


function TADU200.open(time_out: integer = 1000) : boolean;
begin

  result := False;

  @Proc_Close := GetProcAddress(FLibHndl, PROC_CLOSE_C);
  if @Proc_Close = nil then
  begin
    FErrorCmd := 'Close: ' + PROC_CLOSE_C;
    FErrorNum := GetLastError;
    Exit;
  end;

  @Proc_Read := GetProcAddress(FLibHndl, PROC_READ_C);
  if @Proc_Read = nil then
  begin
    FErrorCmd := 'Read: ' + PROC_READ_C;
    FErrorNum := GetLastError;
    Exit;
  end;

  @Proc_Write := GetProcAddress(FLibHndl, PROC_WRITE_C);
  if @Proc_Write = nil then
  begin
    FErrorCmd := 'Write: ' + PROC_WRITE_C;
    FErrorNum := GetLastError;
    Exit;
  end;

  @Proc_Open := GetProcAddress(FLibHndl, PROC_OPEN_C);
  if @Proc_Open = nil then
  begin
    FErrorCmd := 'Open: ' + PROC_OPEN_C;
    FErrorNum := GetLastError;
    Exit;
  end;

  FErrorNum := 0;
  FHndl     := Proc_Open(time_out);
  if (FHndl = 0) then
  begin
    FErrorCmd := 'Open';
    FErrorNum := GetLastError;
    Exit;
  end;

  Relay01 := 0;
  Relay02 := 0;
  Relay03 := 0;
  Relay04 := 0;
  Relay05 := 0;
  Relay06 := 0;
  Relay07 := 0;
  Relay08 := 0;

  result  := true;

end;


procedure TADU200.Set_Bank(Value: String);
begin

  Try
    Value := format('%8.8d', [StrToInt(Value)]);
  Except
    On E:Exception Do
    begin
      MessageDlg('Invalid Bank Value: "' + Value + '"' + #13#13 + E.Message, mtError, [mbOk], 0);
      Exit;
    End;
  End;

  Try
    Relay08 := StrToInt(Copy(Value, 1, 1));
  Except
  End;

  Try
    Relay07 := StrToInt(Copy(Value, 2, 1));
  Except
  End;

  Try
    Relay06 := StrToInt(Copy(Value, 3, 1));
  Except
  End;

  Try
    Relay05 := StrToInt(Copy(Value, 4, 1));
  Except
  End;

  Try
    Relay04 := StrToInt(Copy(Value, 5, 1));
  Except
  End;

  Try
    Relay03 := StrToInt(Copy(Value, 6, 1));
  Except
  End;

  Try
    Relay02 := StrToInt(Copy(Value, 7, 1));
  Except
  End;

  Try
    Relay01 := StrToInt(Copy(Value, 8, 1));
  Except
  End;
end;


function TADU200.Get_Bank : String;
begin
  Read_Relays;
  result := FRelays;
end;


procedure TADU200.Set_Relay01(Value: TRelayState);
begin
  Toggle_Relay(Value, 0);
end;


procedure TADU200.Set_Relay02(Value: TRelayState);
begin
  Toggle_Relay(Value, 1);
end;


procedure TADU200.Set_Relay03(Value: TRelayState);
begin
  Toggle_Relay(Value, 2);
end;


procedure TADU200.Set_Relay04(Value: TRelayState);
begin
  Toggle_Relay(Value, 3);
end;


procedure TADU200.Set_Relay05(Value: TRelayState);
begin
  Toggle_Relay(Value, 4);
end;


procedure TADU200.Set_Relay06(Value: TRelayState);
begin
  Toggle_Relay(Value, 5);
end;


procedure TADU200.Set_Relay07(Value: TRelayState);
begin
  Toggle_Relay(Value, 6);
end;


procedure TADU200.Set_Relay08(Value: TRelayState);
begin
  Toggle_Relay(Value, 7);
end;


function TADU200.Get_Relay01 : TRelayState;
begin
  Read_Relays;
  result := StrToInt(FRelays[0]);
end;


function TADU200.Get_Relay02 : TRelayState;
begin
  Read_Relays;
  result := StrToInt(FRelays[1]);
end;


function TADU200.Get_Relay03 : TRelayState;
begin
  Read_Relays;
  result := StrToInt(FRelays[2]);
end;


function TADU200.Get_Relay04 : TRelayState;
begin
  Read_Relays;
  result := StrToInt(FRelays[3]);
end;


function TADU200.Get_Relay05: TRelayState;
begin
  Read_Relays;
  result := StrToInt(FRelays[4]);
end;


function TADU200.Get_Relay06 : TRelayState;
begin
  Read_Relays;
  result := StrToInt(FRelays[5]);
end;


function TADU200.Get_Relay07 : TRelayState;
begin
  Read_Relays;
  result := StrToInt(FRelays[6]);
end;


function TADU200.Get_Relay08 : TRelayState;
begin
  Read_Relays;
  result := StrToInt(FRelays[7]);
end;

function  TADU200.Get_StatusInput0 : boolean;
begin
  Write('RPA0');
  Read;

  if fRelays[0] = '1' then
    result:=TRUE
  else
    result:=FALSE;
end;

function  TADU200.Get_StatusInput1 : boolean;
begin
  Write('RPA1');
  Read;

  if fRelays[0] = '1' then
    result:=TRUE
  else
    result:=FALSE;
end;

function  TADU200.Get_StatusInput2 : boolean;
begin
  Write('RPA2');
  Read;

  if fRelays[0] = '1' then
    result:=TRUE
  else
    result:=FALSE;
end;

function  TADU200.Get_StatusInput3 : boolean;
begin
  Write('RPA3');
  Read;

  if fRelays[0] = '1' then
    result:=TRUE
  else
    result:=FALSE;
end;

function  TADU200.Get_Input0 : integer;
begin
  Write('RE0');
  Read;
  if not tryStrToInt(fRelays,result) then
    result:=-1;
end;

function  TADU200.Get_Input1 : integer;
begin
  Write('RE01');
  Read;
  if not tryStrToInt(fRelays,result) then
    result:=-1;
end;

function  TADU200.Get_Input2 : integer;
begin
  Write('RE2');
  Read;
  if not tryStrToInt(fRelays,result) then
    result:=-1;
end;

function  TADU200.Get_Input3 : integer;
begin
  Write('RE3');
  Read;
  if not tryStrToInt(fRelays,result) then
    result:=-1;
end;


function TADU200.Read_Relays : boolean;
begin
  result := False;

  fillchar(FRelays, Length(FRelays), -1);
  if not Write('rpk') then
    exit;

  result := Read;
end;


destructor TADU200.Destroy;
begin
  Close;
  FreeLibrary(FLibHndl);
  inherited Destroy;
end;


constructor TADU200.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHndl    := 0;
  FLibHndl := LoadLibrary(ADU_LIB_C);
end;


function TADU200.Get_ErrorText : String;
begin
  result := format('%s, error=%d, msg=%s', [ErrorCmd, ErrorNum, SysErrorMessage(ErrorNum)]);
end;


end.
