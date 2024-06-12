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

unit uDebuggerThread;

interface

uses WinApi.Windows,
     WinApi.PsAPI,
     System.SysUtils,
     System.Classes,
     System.SyncObjs,
     uWorkerThread,
     uFormProcessMonitor;


type
  TDebuggerThread = class;

  TDebuggerThread = class(TWorkerThread)
  private
    FEventTimeout  : Integer;
    FForm          : TFormProcessMonitor;

    // Control Events
    FStopEvent     : TEvent;
    FPlayEvent     : TEvent;
    FContinueEvent : TEvent;
  protected
    FProcessId : Cardinal;

    {@M}
    procedure MonitorEvents();

    procedure ThreadExecute(); override;
    procedure Proc(); virtual; abstract;
  public
    {@C}
    constructor Create(); override;
    destructor Destroy(); override;

    {@G}
    property ProcessId : Cardinal read FProcessId;
  end;

  TCreateProcessAndDebugThread = class(TDebuggerThread)
  private
    FFileName         : String;
    FProcessArguments : String;
    FDebugChild       : Boolean;
    FShowProcess      : Boolean;
  protected
    {@M}
    procedure Proc(); override;
  public
    {@C}
    constructor Create(const AFileName, AProcessArguments : String; const ADebugChild, AShowProcess : Boolean); overload;
  end;

  TAttachProcessAndDebugThread = class(TDebuggerThread)
  protected
    {@M}
    procedure Proc(); override;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal); overload;
  end;

  function WaitForDebugEventEx(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD) : BOOL; stdcall; external 'Kernel32.Dll';

implementation

uses uExceptions, uFormMain, uFunctions;

(* TDebuggerThread *)

{ TDebuggerThread.MonitorEvents }
procedure TDebuggerThread.MonitorEvents();
var ADebugEvent      : TDebugEvent;
    AResult          : Bool;
    AHasSignaledStop : Boolean;
    AHasSignaledPlay : Boolean;
begin
  Synchronize(procedure begin
    FForm.DebugStarted(
      FProcessId,
      FStopEvent,
      FPlayEvent,
      FContinueEvent
    );
  end);
  try
    AHasSignaledStop := False;
    AHasSignaledPlay := False;
    ///

    while (not Terminated) and (not AHasSignaledStop) do begin
      SetLastError(0);
      ///

      AHasSignaledStop := FStopEvent.WaitFor(0) = wrSignaled;
      if AHasSignaledStop then
        break;

      if TOSVersion.Major >= 10 then
        AResult := WaitForDebugEventEx(ADebugEvent, FEventTimeout)
      else
        AResult := WaitForDebugEvent(ADebugEvent, FEventTimeout);

      // Timeout Check
      if GetLastError() = 121 then
        continue;

      if not AResult then
        break;

      var AContinue : Boolean := True;

      //
      // Create Process Handler
      //
      if (ADebugEvent.dwDebugEventCode = EXIT_PROCESS_DEBUG_EVENT) then begin
        Synchronize(procedure begin
          FForm.NotifyEvent(ADebugEvent, False);
        end);
      end;

      //
      // Load / Unload DLL Handler
      //
      if (ADebugEvent.dwDebugEventCode = LOAD_DLL_DEBUG_EVENT) or
         (ADebugEvent.dwDebugEventCode = UNLOAD_DLL_DEBUG_EVENT) then begin

          Synchronize(procedure begin
            FForm.NotifyEvent(ADebugEvent, True);
          end);

          if not AHasSignaledPlay then begin
            // Wait for User-GUI interaction
            FContinueEvent.WaitFor(INFINITE);

            FContinueEvent.ResetEvent();

            ///
            AHasSignaledPlay := FPlayEvent.WaitFor(0) = wrSignaled;
          end;
       end;

      var AFlag : DWORD := DBG_CONTINUE;
      if (not AContinue) or (AHasSignaledStop) then
        AFlag := DBG_TERMINATE_PROCESS;

      if not ContinueDebugEvent(ADebugEvent.dwProcessId, ADebugEvent.dwThreadId, AFlag) then
        break;

      // Detect debugged process exit to interrupt debugger.
      if FProcessId = ADebugEvent.dwProcessId then begin
        case ADebugEvent.dwDebugEventCode of
          EXIT_PROCESS_DEBUG_EVENT: begin
            DebugActiveProcessStop(FProcessId);

            break;
          end;
        end;
      end;
    end;
  finally
    DebugActiveProcessStop(FProcessId);
    ///

    Synchronize(procedure begin
      FForm.DebugStopped();
    end);
  end;
end;

{ TDebuggerThread.Execute }
procedure TDebuggerThread.ThreadExecute();
begin
  Queue(procedure begin
    FForm.Show();
  end);
  try
    try
      Proc();
    except
      on E : Exception do
        Queue(procedure begin
          FormMain.OnException(self, E);
        end);
    end;
  finally
    Queue(procedure begin
      FForm.Close();
    end);
    ///

    ///
    ExitThread(0); // !important
  end;
end;

{ TDebuggerThread.Create }
constructor TDebuggerThread.Create();
begin
  inherited Create();
  ///

  self.FreeOnTerminate := False;

  FEventTimeout := 100;
  FProcessId := 0;

  // Create Control Events
  FStopEvent     := TEvent.Create(nil, True, False, TGUID.NewGuid.ToString());
  FPlayEvent     := TEvent.Create(nil, True, False, TGUID.NewGuid.ToString());
  FContinueEvent := TEvent.Create(nil, True, False, TGUID.NewGuid.ToString());

  FForm := TFormProcessMonitor.Create(FormMain);
end;

{ TDebuggerThread.Destroy }
destructor TDebuggerThread.Destroy();
begin
  if Assigned(FStopEvent) then
    FreeAndNil(FStopEvent);

  if Assigned(FPlayEvent) then
    FreeAndNil(FPlayEvent);

  if Assigned(FContinueEvent) then
    FreeAndNil(FContinueEvent);

  Queue(procedure begin
    FForm.Release();
  end);

  ///
  inherited Destroy();
end;

(* TCreateProcessAndDebugThread *)

{ TCreateProcessAndDebugThread.Proc }
procedure TCreateProcessAndDebugThread.Proc();
var AStartupInfo   : TStartupInfo;
    AProcessInfo   : TProcessInformation;
    ACreationFlags : Cardinal;
begin
  ZeroMemory(@AProcessInfo, SizeOf(TProcessInformation));
  ZeroMemory(@AStartupInfo, Sizeof(TStartupInfo));

  AStartupInfo.cb          := SizeOf(TStartupInfo);
  AStartupInfo.wShowWindow := Ternary(self.FShowProcess, SW_SHOW, SW_HIDE);
  AStartupInfo.dwFlags     := (STARTF_USESHOWWINDOW);

  UniqueString(FFileName);
  UniqueString(FProcessArguments);

  ACreationFlags := CREATE_NEW_CONSOLE;
  if FDebugChild then
    ACreationFlags := ACreationFlags or DEBUG_PROCESS
  else
    ACreationFlags := ACreationFlags or DEBUG_ONLY_THIS_PROCESS;

  if not CreateProcessW(
                  nil,
                  PWideChar(FFileName),
                  nil,
                  nil,
                  False,
                  ACreationFlags,
                  nil,
                  nil,
                  AStartupInfo,
                  AProcessInfo
  ) then
    raise EWindowsException.Create('CreateProcessW', True);
  try
    FProcessId := AProcessInfo.dwProcessId;

    self.MonitorEvents();

    ///
    DebugActiveProcessStop(AProcessInfo.dwProcessId);
  finally
    TerminateProcess(AProcessInfo.hProcess, 0);
  end;
end;

{ TCreateProcessAndDebugThread.Create }
constructor TCreateProcessAndDebugThread.Create(const AFileName, AProcessArguments : String; const ADebugChild, AShowProcess : Boolean);
begin
  inherited Create();
  ///

  FFileName         := AFileName;
  FProcessArguments := AProcessArguments;
  FDebugChild       := ADebugChild;
  FShowProcess      := AShowProcess;
end;

(* TAttachProcessAndDebugThread *)

{ TAttachProcessAndDebugThread.Proc }
procedure TAttachProcessAndDebugThread.Proc();
begin
  DebugActiveProcess(FProcessId);
  try
    self.MonitorEvents();
  finally
    DebugActiveProcessStop(FProcessId);
  end;
end;

{ TAttachProcessAndDebugThread.Create }
constructor TAttachProcessAndDebugThread.Create(const AProcessId : Cardinal);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;
end;

end.
