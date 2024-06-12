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
    FFileFilter  : String;
    FFilterRegex : String;

    FForm        : TForm;
  protected
    {@M}
    procedure ThreadExecute(); override;

  public
    {@C}
    constructor Create(const ADirectory : String; const ARecursive : Boolean = False; const AFileFilter : String = '*.*'; const AFilterRegex : String = ''); overload;
  end;

implementation

uses uFormMain, uFormTask, uFunctions, uFormThreadManager, uEnumExportsThread,
     uPortableExecutable, System.RegularExpressions;

{ TScanFilesThread.ThreadExecute }
procedure TScanFilesThread.ThreadExecute();
var AFiles         : TStringList;
    AFilteredFiles : TStringList;
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
      ACaption := FDirectory;
      if FRecursive then
        ACaption := '+' + ACaption;

      EnumFilesInDirectory(AFiles, FDirectory, FFileFilter, FRecursive, self);

      for AFile in AFiles do begin
        if Terminated then
          break;
        ///

        if not FastPECheck(AFile) then
          continue;

        // Regex Filter Search
        if not FFilterRegex.IsEmpty then begin
          try
            AValidated := False;
            ///

            AParser := TPortableExecutable.CreateFromFile(AFile, []);
            try
              for AExport in AParser.ExportList do begin
                if Terminated then
                  break;
                ///

                if TRegEx.IsMatch(AExport.DisplayName, FFilterRegex) then begin
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
        AFilteredFiles.Add(CleanFileName(AFile));
      end;


      if not Terminated then
        Synchronize(procedure begin
          FormThreadManager.AddWorkerAndStart(
            TEnumExportsThread.Create(
              AFilteredFiles,
              ACaption,
              SystemFolderIcon()
            )
          );
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
constructor TScanFilesThread.Create(const ADirectory : String; const ARecursive : Boolean = False; const AFileFilter : String = '*.*'; const AFilterRegex : String = '');
begin
  inherited Create();
  ///

  FDirectory   := IncludeTrailingPathDelimiter(ADirectory);
  FRecursive   := ARecursive;
  FFileFilter  := AFileFilter;
  FFilterRegex := AFilterRegex.Trim();

  FForm := TFormTask.Create(FormMain, self);
end;

end.
