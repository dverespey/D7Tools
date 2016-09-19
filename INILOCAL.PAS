unit Inilocal;

interface

USES Forms, SysUtils, WinProcs, IniFiles;

function LocalIni : TIniFile;
function LocateIni( FileName : string ) : TIniFile;

implementation

function LocateIni( FileName : string ) : TIniFile;
var
   WinDirP, SysDirP : array[0..255] of char;
   WinDir, SysDir : string;
   CurPath, CurName, CurExt : string;

   function FindIniFile : string;
      function FileInDir( dirnam : string ) : string;
      begin
         Result := dirnam  + FileName + '.INI'
      end;
   begin
      { Get the current path to the executable }
      CurPath := Application.ExeName;

      { Extract the name, extension, and path }
      CurName := ExtractFileName(CurPath);
      CurName := Copy(CurName,1,Pos('.',CurName)-1);
      CurExt := ExtractFileExt(CurPath);
      CurPath := ExtractFilePath(Application.ExeName);

      { Locate the windows and system directories }
      GetWindowsDirectory(WinDirP, High(WinDirP) - Low(WinDirP) - 1);
      GetSystemDirectory(SysDirP, High(SysDirP) - Low(SysDirP) - 1);
      WinDir := StrPas(WinDirP);
      SysDir := StrPas(SysDirP);

      { Check out the current, windows, and system directories }
      Result := FileInDir(CurPath);
      if FileExists( Result ) then Exit;

      Result := FileInDir(WinDir);
      if FileExists( Result ) then Exit;

      Result := FileInDir(SysDir);
      if FileExists( Result ) then Exit;

      { Assume it is in the current folder }
      Result := FileInDir(CurPath);
   end;
Begin
   Result := TIniFile.Create(FindIniFile);
End;

function LocalIni : TIniFile;
   {
      Answer the name of a file exename.INI, where exename is the
      name of the executable. If it is not in the current directory
      check in the windows directory. If not in the windows directory,
      check in the sytem directory. If not there either is doesn't
      exist, so return the exename.INI file with the current path
      attached.
   }

   function FindIniFile : string;
   var
      WinDirP, SysDirP : array[0..255] of char;
      WinDir, SysDir : string;
      CurPath, CurName, CurExt: string;

      function FileInDir( dirnam : string ) : string;
      begin
         Result := dirnam  + CurName + '.INI'
      end;

   begin
      { Get the current path to the executable }
      CurPath := Application.ExeName;

      { Extract the name, extension, and path }
      CurName := ExtractFileName(CurPath);
      CurName := Copy(CurName,1,Pos('.',CurName)-1);
      CurExt := ExtractFileExt(CurPath);
      CurPath := ExtractFilePath(Application.ExeName);

      { Locate the windows and system directories }
      GetWindowsDirectory(WinDirP, High(WinDirP) - Low(WinDirP) - 1);
      GetSystemDirectory(SysDirP, High(SysDirP) - Low(SysDirP) - 1);
      WinDir := StrPas(WinDirP);
      SysDir := StrPas(SysDirP);

      { Check out the current, windows, and system directories }
      Result := FileInDir(CurPath);
      if FileExists( Result ) then Exit;

      Result := FileInDir(WinDir);
      if FileExists( Result ) then Exit;

      Result := FileInDir(SysDir);
      if FileExists( Result ) then Exit;

      { Assume it is in the current folder }
      Result := FileInDir(CurPath);
   end;
begin
   Result := TIniFile.Create(FindIniFile);
end;

end.
