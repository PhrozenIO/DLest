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

unit uScanFilesThread;

interface

uses System.Classes,
     System.SysUtils,
     VCL.Forms,
     Winapi.Windows,
     uWorkerThread;

type
  TScanFilesThread = class(TWorkerThread)
  private
    FDirectory : String;
    FRecursive : Boolean;
    FDeepScan  : Boolean;

    FForm      : TForm;
  protected
    {@M}
    procedure ThreadExecute(); override;

  public
    {@C}
    constructor Create(const ADirectory : String; const ADeepScan : Boolean = False; const ARecursive : Boolean = False); overload;
  end;

implementation

uses uFormMain, uFormTask, uFunctions, uFormThreadManager, uEnumExportsThread;

{ TScanFilesThread.ThreadExecute }
procedure TScanFilesThread.ThreadExecute();
var AFiles    : TStringList;
    AWildCard : String;
    ACaption  : String;
begin
  Queue(procedure begin
    FForm.Show();
  end);
  try
    AFiles := TStringList.Create();
    try
      if FDeepScan then
        AWildCard := '*.*'
      else
        AWildCard := '*.dll';
      ///

      ACaption := FDirectory;
      if FRecursive then
        ACaption := '+' + ACaption;

      EnumFilesInDirectory(AFiles, FDirectory, AWildCard, FRecursive, FDeepScan, self);

      Synchronize(procedure begin
        FormThreadManager.AddWorkerAndStart(TEnumExportsThread.Create(AFiles, ACaption));
      end);
    finally
      if Assigned(AFiles) then
        FreeAndNil(AFiles);
    end;
  finally
    Queue(procedure begin
      FForm.Close();
    end);
  end;
end;

{ TScanFilesThread.Create }
constructor TScanFilesThread.Create(const ADirectory : String; const ADeepScan : Boolean = False; const ARecursive : Boolean = False);
begin
  inherited Create();
  ///

  FDirectory := IncludeTrailingPathDelimiter(ADirectory);
  FRecursive := ARecursive;
  FDeepScan  := ADeepScan;

  FForm := TFormTask.Create(FormMain, self);
end;

end.
