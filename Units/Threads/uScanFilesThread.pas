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
    FDirectory   : String;
    FRecursive   : Boolean;
    FDeepScan    : Boolean;
    FFilterRegex : String;

    FForm        : TForm;
  protected
    {@M}
    procedure ThreadExecute(); override;

  public
    {@C}
    constructor Create(const ADirectory : String; const ADeepScan : Boolean = False; const ARecursive : Boolean = False; const AFilterRegex : String = ''); overload;
  end;

implementation

uses uFormMain, uFormTask, uFunctions, uFormThreadManager, uEnumExportsThread,
     uPortableExecutable, System.RegularExpressions;

{ TScanFilesThread.ThreadExecute }
procedure TScanFilesThread.ThreadExecute();
var AFiles         : TStringList;
    AFilteredFiles : TStringList;
    AWildCard      : String;
    ACaption       : String;
    AFile          : String;
    AValidated     : Boolean;
    AParser        : TPortableExecutable;
    AExport        : TExportEntry;
begin
  Queue(procedure begin
    FForm.Show();
  end);
  try
    AFiles := TStringList.Create();
    AFilteredFiles := TStringList.Create();
    try
      if FDeepScan then
        AWildCard := '*.*'
      else
        AWildCard := '*.dll';
      ///

      ACaption := FDirectory;
      if FRecursive then
        ACaption := '+' + ACaption;

      EnumFilesInDirectory(AFiles, FDirectory, AWildCard, FRecursive, self);

      if FDeepScan or (not FFilterRegex.IsEmpty) then begin
        for AFile in AFiles do begin
          // Deep Scan
          if FDeepScan then begin
            if not FastPECheck(AFile) then
              continue;
          end;

          // Regex Filter Search
          if not FFilterRegex.IsEmpty then begin
            try
              AValidated := False;
              ///

              AParser := TPortableExecutable.CreateFromFile(AFile);
              try
                for AExport in AParser.ExportList do begin
                  if TRegEx.IsMatch(AExport.Name, FFilterRegex) then begin
                    AValidated := True;

                    break;
                  end;
                end;
              finally
                if Assigned(AParser) then
                  FreeAndNil(AParser);
              end;
            except
              AValidated := False;
            end;

            if not AValidated then
              continue;
          end;

          ///
          AFilteredFiles.Add(AFile);
        end;
      end else
        AFilteredFiles.Assign(AFiles);

      Synchronize(procedure begin
        FormThreadManager.AddWorkerAndStart(TEnumExportsThread.Create(AFilteredFiles, ACaption));
      end);
    finally
      FreeAndNil(AFilteredFiles);
      FreeAndNil(AFiles);
    end;
  finally
    Queue(procedure begin
      FForm.Close();
    end);
  end;
end;

{ TScanFilesThread.Create }
constructor TScanFilesThread.Create(const ADirectory : String; const ADeepScan : Boolean = False; const ARecursive : Boolean = False; const AFilterRegex : String = '');
begin
  inherited Create();
  ///

  FDirectory   := IncludeTrailingPathDelimiter(ADirectory);
  FRecursive   := ARecursive;
  FDeepScan    := ADeepScan;
  FFilterRegex := AFilterRegex.Trim();

  FForm := TFormTask.Create(FormMain, self);
end;

end.
