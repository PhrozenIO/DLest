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

unit uWorkerThread;

interface

uses System.Classes,
     System.SyncObjs;

type
  TWorkerThread = class(TThread)
  private
  strict private
    {@G}
    function GetIsTerminated() : Boolean;
  protected
    {@M}
    procedure TerminatedSet(); override;
    procedure ThreadExecute(); virtual; abstract;
    procedure Execute(); override;
  public
    {@C}
    constructor Create(); overload; virtual;
    destructor Destroy(); override;

    {@G}
    property IsTerminated : Boolean read GetIsTerminated;
  end;

implementation

uses Winapi.Windows, uFormMain, System.SysUtils;

{ TWorkerThread.Execute }
procedure TWorkerThread.Execute();
begin
  try
    try
      ThreadExecute();
    except
      on E : Exception do begin
        Synchronize(procedure begin
          FormMain.OnException(self, E);
        end);
      end;
    end;
  finally
    ExitThread(0); // !important
  end;
end;

{ TWorkerThread.Create}
constructor TWorkerThread.Create();
begin
  inherited Create(True);
  ///

  self.FreeOnTerminate := False;
end;

{ TWorkerThread.Destroy }
destructor TWorkerThread.Destroy();
begin
  self.Terminate();

  self.WaitFor();

  ///
  inherited Destroy();
end;

{ TWorkerThread.TerminatedSet }
procedure TWorkerThread.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

end;

{ TWorkerThread.GetIsTerminated }
function TWorkerThread.GetIsTerminated() : Boolean;
var AExitCode : Cardinal;
begin
  result := False;
  ///

  if GetExitCodeThread(self.Handle, AExitCode) then
    result := (AExitCode = 0);
end;


end.
