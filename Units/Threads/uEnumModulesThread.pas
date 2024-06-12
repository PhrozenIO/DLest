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

unit uEnumModulesThread;

interface

uses System.Classes,
     System.SysUtils,
     Winapi.Windows;

type
  TEnumModulesThread = class(TThread)
  private
    FProcessId : Cardinal;
  protected
    {@M}
    procedure Execute(); override;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal); overload;
  end;

implementation

uses VirtualTrees, uFormProcessList, Winapi.TlHelp32, uExceptions, uFunctions,
     uConstants, VirtualTrees.Types;

{ TEnumModulesThread.Execute }
procedure TEnumModulesThread.Execute();
var hSnap             : THandle;
    AModuleEntry      : TModuleEntry32;
    ATotalModulesSize : UInt64;

  procedure AddItem();
  var pNode      : PVirtualNode;
      pData      : PModuleTreeData;
  begin
    Synchronize(procedure begin
      pNode := FormProcessList.VSTModules.AddChild(nil);
      pData := pNode.GetData;
    end);

    pData^.ImagePath  := CleanFileName(AModuleEntry.szExePath);
    pData^.ProcessId  := AModuleEntry.th32ProcessID;
    pData^.ModuleBase := AModuleEntry.modBaseAddr;
    pData^.ModuleSize := AModuleEntry.modBaseSize;
    pData^.ImageIndex := SystemFileIcon(pData^.ImagePath);

    ///
    Inc(ATotalModulesSize, AModuleEntry.modBaseSize);
  end;

begin
  ATotalModulesSize := 0;
  try
    PostMessage(FormProcessList.Handle, WM_MESSAGE_BEGIN_UPDATE, 0, LPARAM(ulkModules));
    ///

    hSnap := CreateToolHelp32Snapshot(TH32CS_SNAPMODULE, FProcessId);
    if hSnap = INVALID_HANDLE_VALUE then
      raise EWindowsException.Create('CreateToolHelp32Snapshot');

    ZeroMemory(@AModuleEntry, SizeOf(TModuleEntry32));
    AModuleEntry.dwSize := SizeOf(TModuleEntry32);

    if not Module32First(hSnap, AModuleEntry) then
      raise EWindowsException.Create('Process32First');

    repeat
      AddItem();

      ///
      ZeroMemory(@AModuleEntry, SizeOf(TModuleEntry32));
      AModuleEntry.dwSize := SizeOf(TModuleEntry32);
    until (not Module32Next(hSnap, AModuleEntry));
  finally
    // WParam usually takes an handle or an integer
    // LParam usually takes a pointer
    PostMessage(FormProcessList.Handle, WM_MESSAGE_END_UPDATE, ATotalModulesSize, LPARAM(ulkModules));

    ///
    ExitThread(0);
  end;
end;

{ TEnumModulesThread.Create }
constructor TEnumModulesThread.Create(const AProcessId : Cardinal);
begin
  inherited Create(False);

  FProcessId := AProcessId;
end;

end.
