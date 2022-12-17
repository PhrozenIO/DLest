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
{                   https://www.twitter.com/                                   }
{                   https://www.phrozen.io/                                    }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{******************************************************************************}

unit uFunctions;

interface

uses System.Classes,
     System.SysUtils,
     Winapi.Windows,
     VCL.Controls,
     Winapi.PsAPI,
     Winapi.ShellAPI,
     uWorkerThread;

type
  TArchitecture = (
      archUnknown,
      arch32,
      arch64
  );

function GetWindowsDirectory() : string;
procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; ALargeIcon : Boolean = False);
function SystemFileIcon(const AFileName : string; AExtensionMode : Boolean = False) : Integer;
function IsProcessRunningSameArchitecture(const AProcessId : Cardinal) : Boolean;
function IsProcessElevated(const hProcess : THandle) : Boolean;
function IsProcessElevatedById(const AProcessId : Cardinal) : Boolean;
function IsCurrentProcessElevated() : Boolean;
function GetImagePathFromProcessId(const AProcessID : Cardinal) : String;
function FormatSize(const ASize : Int64) : string;
function DELF_GetModuleFileNameEx(const AProcessId : Cardinal; const hModule : THandle) : String; overload;
function DELF_GetModuleFileNameEx(const hProcess, hModule : THandle) : String; overload;
procedure EnumFilesInDirectory(var AFiles : TStringList; ADirectory : String; const AWildCard : String = '*.*'; const ARecursive : Boolean = False; const AValidatePE : Boolean = False; const AThread : TWorkerThread = nil);
function FastPECheck(const AFilePath : String) : Boolean;

const PROCESS_QUERY_LIMITED_INFORMATION = $1000;

implementation

uses System.Math, System.IOUtils, uExceptions, System.Masks;

{ _.GetWindowsDirectory }
function GetWindowsDirectory() : string;
var ALen  : Cardinal;
begin
  SetLength(result, MAX_PATH);

  ALen := WinAPI.Windows.GetWindowsDirectory(@result[1], MAX_PATH);

  SetLength(result, ALen);
  if ALen > MAX_PATH then
    WinAPI.Windows.GetWindowsDirectory(@result[1], ALen);

  ///
  result := IncludeTrailingPathDelimiter(result);
end;

{ _.InitializeSystemIcons }
procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; ALargeIcon : Boolean = False);
var AFlags : Integer;
begin
  ZeroMemory(@AFileInfo, SizeOf(TSHFileInfo));
  ///

  if ALargeIcon then
    AFlags := SHGFI_LARGEICON
  else
    AFlags := SHGFI_SMALLICON;

  AImages.Handle := SHGetFileInfo(
                                    PChar(TPath.GetPathRoot(GetWindowsDirectory())),
                                    0,
                                    AFileInfo,
                                    SizeOf(AFileInfo),
                                    AFlags or (SHGFI_SYSICONINDEX)
  );
end;

{ _.SystemFileIcon }
function SystemFileIcon(const AFileName : string; AExtensionMode : Boolean = False) : Integer;
var AFileInfo : TSHFileInfo;
    AFlags    : Integer;
begin
  ZeroMemory(@AFileInfo, sizeof(AFileInfo));
  ///

  AExtensionMode := AFileName.IsEmpty or (not FileExists(AFileName));

  AFlags := SHGFI_SMALLICON or SHGFI_SYSICONINDEX;
  if AExtensionMode then
    AFlags := AFlags or SHGFI_USEFILEATTRIBUTES;

  SHGetFileInfo(PWideChar(AFileName), 0, AFileInfo, SizeOf(AFileInfo), AFlags);

  Result := AFileInfo.iIcon;
end;

{ _.GetProcessArchitecture }
function GetProcessArchitecture(const AProcessId : Cardinal) : TArchitecture;
var hProcess : THandle;
    AWow64Process : bool;
begin
  result := archUnknown;
  ///

  if (TOSVersion.Architecture = arIntelX86) then
    Exit(arch32);
  ///

  hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    IsWow64Process(hProcess, AWow64Process);
    ///

    if AWow64Process then
      result := arch32
    else
      result := arch64;
  finally
    CloseHandle(hProcess);
  end;
end;

{ _.IsProcessRunningSameArchitecture }
function IsProcessRunningSameArchitecture(const AProcessId : Cardinal) : Boolean;
var ACurrent64    : Boolean;
    AProcess64    : Boolean;
    AArchitecture : TArchitecture;
begin
  {$IFDEF WIN64}
    ACurrent64 := True;
  {$ELSE IF WIN32}
    ACurrent64 := False;
  {$ENDIF}

  AArchitecture := GetProcessArchitecture(AProcessId);
  if AArchitecture = archUnknown then
    raise Exception.Create('Could not resolve process architecture');

  AProcess64 := (AArchitecture = arch64);

  ///
  result := (ACurrent64 = AProcess64);
end;

{ _.IsProcessElevated }
function IsProcessElevated(const hProcess : THandle) : Boolean;
var AToken     : THandle;
    AElevation : DWORD;
    ALength    : Cardinal;

const ATokenForElevation = 20;
begin
  result := True;
  ///

  if NOT OpenProcessToken(hProcess, TOKEN_QUERY, AToken) then
    raise EWindowsException.Create('OpenProcessToken');
  try
    if NOT GetTokenInformation(AToken, TTokenInformationClass(ATokenForElevation), @AElevation, SizeOf(DWORD), ALength) then
      raise EWindowsException.Create('GetTokenInformation');

    result := (AElevation <> 0);
  finally
    CloseHandle(AToken);
  end;
end;

{ _.IsProcessElevatedById }
function IsProcessElevatedById(const AProcessId : Cardinal) : Boolean;
var hProcess : THandle;
begin
  result := True;
  ///

  if (Win32MajorVersion < 6) then
    Exit();

  hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    result := IsProcessElevated(hProcess);
  finally
    CloseHandle(hProcess);
  end;
end;

{ _.IsCurrentProcessElevated }
function IsCurrentProcessElevated() : Boolean;
begin
  result := IsProcessElevated(GetCurrentProcess());
end;

{ _.GetImagePathFromProcessId }
function GetImagePathFromProcessId(const AProcessID : Cardinal) : String;
var hProcess    : THandle;
    ACBNeeded   : Cardinal;
    hMainModule : THandle;
begin
  result := '';

  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create(Format('OpenProcess[pid: %d]', [AProcessId]));
  try
    if not EnumProcessModules(hProcess, @hMainModule, SizeOf(HMODULE), ACBNeeded) then
      raise EWindowsException.Create(Format('EnumProcessModules[pid: %d]', [AProcessId]));
    ///

    result := DELF_GetModuleFileNameEx(hProcess, hMainModule);
  finally
    CloseHandle(hProcess);
  end;
end;

{ _.DELF_GetModuleFileNameEx }
function DELF_GetModuleFileNameEx(const hProcess, hModule : THandle) : String;
var ALength         : Cardinal;
    AReturnedLength : Cardinal;
begin
  result := '';
  ///

  ALength := MAX_PATH * 2;

  SetLength(result, ALength);
  try
    AReturnedLength := GetModuleFileNameExW(hProcess, hModule, PWideChar(result), ALength);
    if AReturnedLength = 0 then
      raise EWindowsException.Create('GetModuleFileNameExW');
  finally
    SetLength(result, AReturnedLength);
  end;
end;

{ _.DELF_GetModuleFileNameEx }
function DELF_GetModuleFileNameEx(const AProcessId : Cardinal; const hModule : THandle) : String;
var hProcess : THandle;
begin
  result := '';
  ///

  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create(Format('OpenProcess[pid: %d]', [AProcessId]));
  try
    result := DELF_GetModuleFileNameEx(hProcess, hModule);
  finally
    if hProcess <> INVALID_HANDLE_VALUE then
      CloseHandle(hProcess);
  end;
end;

{ _.FormatSize }
function FormatSize(const ASize : Int64) : string;
const AByteDescription : Array[0..8] of string = ('Bytes', 'KiB', 'MB', 'GiB', 'TB', 'PB', 'EB', 'ZB', 'YB');

var ACount : Integer;
begin
  ACount := 0;

  while ASize > Power(1024, ACount +1) do
    Inc(ACount);

  result := Format('%s %s', [FormatFloat('###0.00', ASize / Power(1024, ACount)), AByteDescription[ACount]]);
end;

{ _.EnumFilesInDirectory }
procedure EnumFilesInDirectory(var AFiles : TStringList; ADirectory : String; const AWildCard : String = '*.*'; const ARecursive : Boolean = False; const AValidatePE : Boolean = False; const AThread : TWorkerThread = nil);
var ASearch      : TSearchRec;
    AFilePath    : String;
    AIsDirectory : Boolean;
begin
  if not Assigned(AFiles) then
    Exit();
  ///

  ADirectory := IncludeTrailingPathDelimiter(ADirectory);
  ///

  if System.SysUtils.FindFirst(Format('%s*.*', [ADirectory, AWildCard]), faAnyFile, ASearch) <> 0 then
    Exit();
  try
    repeat
      if Assigned(AThread) then begin
        if AThread.IsTerminated then
          break;
      end;

      if (ASearch.Name = '.') or (ASearch.Name = '..') then
        continue;
      ///

      AFilePath := Format('%s%s', [ADirectory, ASearch.Name]);
      ///

      AIsDirectory := (ASearch.Attr and faDirectory) <> 0;

      if (not AIsDirectory) and (MatchesMask(ASearch.Name, AWildCard)) then
        AFiles.Add(AFilePath);

      if AIsDirectory and ARecursive then
        EnumFilesInDirectory(AFiles, AFilePath, AWildCard, ARecursive);
    until System.SysUtils.FindNext(ASearch) <> 0;
  finally
    System.SysUtils.FindClose(ASearch);
  end;
end;

{ _.FastPECheck }
function FastPECheck(const AFilePath : String) : Boolean;
var hFile             : THandle;

    AImageDosHeader   : TImageDosHeader;
    AImageNtSignature : DWORD;
    AImageFileHeader  : TImageFileHeader;

    ABytesRead        : Cardinal;
begin
  result := False;
  ///

  hFile := CreateFileW(
      PWideChar(AFilePath),
      GENERIC_READ,
      FILE_SHARE_READ,
      nil,
      OPEN_EXISTING,
      0,
      0
  );
  if hFile = INVALID_HANDLE_VALUE then
    Exit();
  try
    if not ReadFile(hFile, AImageDosHeader, SizeOf(TImageDosHeader), ABytesRead, nil) then
      Exit();

    if AImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
      Exit();

    SetFilePointerEx(hFile, AImageDosHeader._lfanew, nil, FILE_BEGIN);

    if not ReadFile(hFile, AImageNtSignature, SizeOf(DWORD), ABytesRead, nil) then
      Exit();

    if AImageNtSignature <> IMAGE_NT_SIGNATURE then
      Exit();

    if not ReadFile(hFile, AImageFileHeader, SizeOf(TImageFileHeader), ABytesRead, nil) then
      Exit();

    {$IFDEF WIN64}
      if AImageFileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then
    {$ELSE}
      if AImageFileHeader.Machine <> IMAGE_FILE_MACHINE_I386 then
    {$ENDIF}
      Exit();

    ///
    result := True;
  finally
    CloseHandle(hFile);
  end;
end;

end.
