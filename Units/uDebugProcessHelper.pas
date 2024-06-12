{******************************************************************************}
{                                                                              }
{            __________.__                                                     }
{            \______   \  |_________  ____________ ____   ____                 }
{             |     ___/  |  \_  __ \/  _ \___   // __ \ /    \                }
{             |    |   |   Y  \  | \(  <_> )    /\  ___/|   |  \               }
{             |____|   |___|  /__|   \____/_____ \\___  >___|  /               }
{             \/                  \/    \/     \/                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/darkcodersc                        }
{                   https://www.phrozen.io/                                    }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{******************************************************************************}

unit uDebugProcessHelper;

interface

uses System.Classes, Winapi.Windows, Winapi.PsAPI, Winapi.TlHelp32;

type
  TDebugProcessHelper = class
  private
    FProcessHandle : THandle;
  public
    {@C}
    constructor CreateFromPID(const AProcessId : Cardinal);
    constructor CreateFromHandle(const AProcessHandle : THandle);

    {@M}
    function ReadRemoteString(const pOffset : Pointer; const ALength : Cardinal; const AUnicode : Boolean) : String; overload;
    function ReadRemoteString(const pBaseOffset : Pointer; const AUnicode : Boolean) : String; overload;
    function IsMemoryNull(const pOffset : Pointer; const ARegionSize : Cardinal) : Boolean;
    function ReadRemoteString_PTRPTR(const pBaseOffset : Pointer; const AUnicode : Boolean) : String;
    function GetProcessImageFileName() : String; overload;

    {@ST}
    class function PhysicalToVirtualPath(APath : String) : String; static;
    class function GetParentProcessIdByProcessId(const AProcessId : Cardinal) : Cardinal; static;
    class function GetProcessImageFileName_FromPID(const AProcessId : Cardinal) : String;
    class function GetProcessImageFileName_FromHandle(const AProcessHandle : THandle) : String;

  end;

  const PROCESS_QUERY_LIMITED_INFORMATION = $1000;

  function GetProcessImageFileNameW(
    hProcess         : THandle;
    lpImageFileName  : LPWSTR;
    nSize            : DWORD
  ): DWORD; stdcall; external 'PSAPI.DLL';

implementation

uses System.SysUtils, uExceptions, uFunctions;

(* Static *)

{ TDebugProcessHelper.GetProcessImageFileName_FromPID }
class function TDebugProcessHelper.GetProcessImageFileName_FromPID(const AProcessId : Cardinal) : String;
var hProcess : THandle;
begin
  hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    result := GetProcessImageFileName_FromHandle(hProcess);
  finally
    CloseHandle(hProcess);
  end;
end;

{ TDebugProcessHelper.GetProcessImageFileName_FromHandle }
class function TDebugProcessHelper.GetProcessImageFileName_FromHandle(const AProcessHandle : THandle) : String;
var ALength         : Cardinal;
    AReturnedLength : Cardinal;
    AImagePath      : String;
begin
  result := 'Unknown';
  ///

  ALength := MAX_PATH * 2;

  AReturnedLength := 0;

  SetLength(AImagePath, ALength);
  try
    AReturnedLength := GetProcessImageFileNameW(AProcessHandle, PWideChar(AImagePath), ALength);
    if AReturnedLength = 0 then
      Exit();
  finally
    SetLength(AImagePath, AReturnedLength);
  end;

  ///
  result := CleanFileName(PhysicalToVirtualPath(AImagePath));
end;

{ TDebugProcessHelper.GetParentProcessIdByProcessId }
class function TDebugProcessHelper.GetParentProcessIdByProcessId(const AProcessId : Cardinal) : Cardinal;
var AProcHandle   : THandle;
    AProcessEntry : TProcessEntry32;
begin
  result := 0;
  ///

  ZeroMemory(@AProcessEntry, sizeOf(TProcessEntry32));
  AProcessEntry.dwSize := SizeOf(AProcessEntry);
  AProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    if NOT Process32First(AProcHandle, AProcessEntry) then
      Exit();

    if (AProcessEntry.th32ProcessID = AProcessId) then begin
      result := AProcessEntry.th32ParentProcessID;

      Exit();
    end;

    while Process32Next(AProcHandle, AProcessEntry) do begin
      if (AProcessEntry.th32ProcessID = AProcessId) then begin
        result := AProcessEntry.th32ParentProcessID;

        Break;
      end;
    end;
  finally
    CloseHandle(AProcHandle);
  end;
end;

{ TDebugProcessHelper.PhysicalToVirtualPath }
class function TDebugProcessHelper.PhysicalToVirtualPath(APath : String) : String;
var i          : integer;
    ADrive     : String;
    ABuffer    : array[0..MAX_PATH-1] of Char;
    ACandidate : String;
begin
  result := '';
  {$I-}
  try
    for I := 0 to 25 do begin
      ADrive := Format('%s:', [Chr(Ord('A') + i)]);
      ///

      if (QueryDosDevice(PWideChar(ADrive), ABuffer, MAX_PATH) = 0) then
        continue;

      ACandidate := String(ABuffer).ToLower();

      if String(Copy(APath, 1, Length(ACandidate))).ToLower() = ACandidate then begin
        Delete(APath, 1, Length(ACandidate));

        result := Format('%s%s', [ADrive, APath]);
      end;
    end;
  except
    result := '';
  end;
  {$I+}
end;

(* Instanciated *)

{ TDebugProcessHelper.CreateFromHandle }
constructor TDebugProcessHelper.CreateFromHandle(const AProcessHandle : THandle);
begin
  inherited Create();
  ///

  FProcessHandle := AProcessHandle;
end;

{ TDebugProcessHelper.CreateFromPID }
constructor TDebugProcessHelper.CreateFromPID(const AProcessId : Cardinal);
begin
  inherited Create();
  ///

  FProcessHandle := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if FProcessHandle = 0 then
    raise EWindowsException.Create('OpenProcess');
end;

{ TDebugProcessHelper.ReadRemoteString }
function TDebugProcessHelper.ReadRemoteString(const pOffset : Pointer; const ALength : Cardinal; const AUnicode : Boolean) : String;
var ABytesRead : SIZE_T;
    pBuffer    : Pointer;
begin
  try
    GetMem(pBuffer, ALength);
    try
      if not ReadProcessMemory(FProcessHandle, pOffset, pBuffer, ALength, ABytesRead) then
        raise EWindowsException.Create('ReadProcessMemory');

      if not AUnicode then
        SetString(result, PAnsiChar(pBuffer), ALength)
      else
        SetString(result, PWideChar(pBuffer), ALength);
    finally
      FreeMem(pBuffer, ALength);
    end;
  except
    result := 'n/a';
  end;
end;

{ TDebugProcessHelper.IsMemoryNull }
function TDebugProcessHelper.IsMemoryNull(const pOffset : Pointer; const ARegionSize : Cardinal) : Boolean;
var I : Cardinal;
begin
  if (pOffset = nil) or (ARegionSize = 0) then
    raise Exception.Create('Memory offset must not be null and size above zero.');

  result := True;

  for I := 0 to ARegionSize -1 do begin
    if PByte(NativeUInt(pOffset) + I)^ <> 0 then begin
      result := False;

      break;
    end;
  end;
end;

{ TDebugProcessHelper.ReadRemoteString }
function TDebugProcessHelper.ReadRemoteString(const pBaseOffset : Pointer; const AUnicode : Boolean) : String;
var pBuffer    : Pointer;
    ALength    : Cardinal;
    I          : Cardinal;
    pOffset    : Pointer;
    ABytesRead : SIZE_T;
    ACharacter : array of byte;
begin
  result := 'n/a';
  if pBaseOffset = nil then
    Exit();
  try
    ALength := MAX_PATH * 2;
    ///

    if AUnicode then
      SetLength(ACharacter, SizeOf(WideChar))
    else
      SetLength(ACharacter, SizeOf(AnsiChar));

    GetMem(pBuffer, ALength); // Might be sufficient
    try
      ZeroMemory(pBuffer, ALength);
      ///

      I := 0;
      while True do begin
        pOffset := Pointer(NativeUInt(pBaseOffset) + I);
        ///

        if not ReadProcessMemory(FProcessHandle, pOffset, PByte(ACharacter), Length(ACharacter), ABytesRead) then
          raise EWindowsException.Create('ReadProcessMemory');

        // Null-Byte Exit
        if IsMemoryNull(ACharacter, Length(ACharacter)) then
          break;

        CopyMemory(Pointer(NativeUInt(pBuffer) + I), PByte(ACharacter), Length(ACharacter));

        Inc(I, Length(ACharacter));

        // Emergency Exit
        if I >= ALength -1 then
          break;
      end;

      ///
      if not AUnicode then
        SetString(result, PAnsiChar(pBuffer), I)
      else
        SetString(result, PWideChar(pBuffer), (I div SizeOf(WideChar)));
    finally
      FreeMem(pBuffer, ALength);
    end;
  except
  end;
end;

{ TDebugProcessHelper.ReadRemoteString_PTRPTR }
function TDebugProcessHelper.ReadRemoteString_PTRPTR(const pBaseOffset : Pointer; const AUnicode : Boolean) : String;
var AStringOffset : NativeUInt;
    ABytesRead    : SIZE_T;
begin
  result := 'n/a';
  if pBaseOffset = nil then
    Exit();
  try
    // Get String Pointer
    if not ReadProcessMemory(FProcessHandle, pBaseOffset, @AStringOffset, SizeOf(Pointer), ABytesRead) then
      raise EWindowsException.Create('ReadProcessMemory');
    ///

    // Read String Content
    if AStringOffset > 0 then
      result := ReadRemoteString(Pointer(AStringOffset), AUnicode);
  except
  end;
end;

{ TDebugProcessHelper.ReadRemoteString_PTRPTR }
function TDebugProcessHelper.GetProcessImageFileName() : String;
begin
  result := self.GetProcessImageFileName_FromHandle(FProcessHandle);
end;

end.
