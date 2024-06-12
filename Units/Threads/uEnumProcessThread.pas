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

unit uEnumProcessThread;

interface

uses System.Classes,
     System.SysUtils,
     Winapi.Windows;

type
  TEnumProcessThread = class(TThread)
  private

  protected
    {@M}
    procedure Execute(); override;
  public
  end;

implementation

uses VirtualTrees, uFormProcessList, Winapi.TlHelp32, uExceptions, uFunctions,
     uConstants, VirtualTrees.Types;

{ TEnumProcessThread.Execute }
procedure TEnumProcessThread.Execute();
var hSnap         : THandle;
    AProcessEntry : TProcessEntry32;

  procedure AddItem();
  var AImagePath : String;
      pNode      : PVirtualNode;
      pData      : PProcessTreeData;
  begin
    try
      // Filter different process arch
      if not IsProcessRunningSameArchitecture(AProcessEntry.th32ProcessID) then
        Exit();
      ///

      if (not IsCurrentProcessElevated()) then begin
        if IsProcessElevatedById(AProcessEntry.th32ProcessID) then
          Exit();
      end;

      AImagePath := GetImagePathFromProcessId(AProcessEntry.th32ProcessID);
    except
      Exit();
    end;
    ///

    Synchronize(procedure begin
      pNode := FormProcessList.VSTProcess.AddChild(nil);
      pData := pNode.GetData;
    end);

    pData^.ImagePath  := CleanFileName(AImagePath);
    pData^.ProcessId  := AProcessEntry.th32ProcessID;
    pData^.ImageIndex := SystemFileIcon(AImagePath);
  end;

begin
  try
    PostMessage(FormProcessList.Handle, WM_MESSAGE_BEGIN_UPDATE, 0, LPARAM(ulkProcess));
    ///

    hSnap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if hSnap = INVALID_HANDLE_VALUE then
      raise EWindowsException.Create('CreateToolHelp32Snapshot');

    ZeroMemory(@AProcessEntry, SizeOf(TProcessEntry32));
    AProcessEntry.dwSize := SizeOf(TProcessEntry32);

    if not Process32First(hSnap, AProcessEntry) then
      raise EWindowsException.Create('Process32First');

    repeat
      AddItem();

      ///
      ZeroMemory(@AProcessEntry, SizeOf(TProcessEntry32));
      AProcessEntry.dwSize := SizeOf(TProcessEntry32);
    until (not Process32Next(hSnap, AProcessEntry));
  finally
    PostMessage(FormProcessList.Handle, WM_MESSAGE_END_UPDATE, 0, LPARAM(ulkProcess));

    ///
    ExitThread(0);
  end;
end;

end.
