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

unit uExportExportsToJsonThread;

interface

uses System.Classes,
     System.SysUtils,
     Winapi.Windows,
     VCL.Forms,
     uWorkerThread,
     uFrameList,
     uTypes;

type
  TExportExportsToJsonThread = class(TWorkerThread)
  private
    FForm            : TForm;
    FFrame           : TFrameList;
    FOutputDirectory : String;
    FMode            : TJSONExportMode;
  protected
    {@M}
    procedure ThreadExecute(); override;

  public
    {@C}
    constructor Create(const AFrame : TFrameList; const AOutputDirectory : String; const AExportMode : TJSONExportMode); overload;
  end;

implementation

uses uFormMain, uFormTask, uFunctions, XSuperObject, System.Hash,
     VirtualTrees, Generics.Collections, uPortableExecutable,
     VirtualTrees.Types, VirtualTrees.BaseTree;

{ TExportExportsToJsonThread.ThreadExecute }
procedure TExportExportsToJsonThread.ThreadExecute();
var AJson              : ISuperObject;
    AJsonImage         : ISuperObject;
    AJsonImages        : ISuperArray;
    AJsonExports       : ISuperArray;
    AItems             : TObjectDictionary<String, TObjectList<TExportEntry>>;
    AImagePath         : String;
    AExports           : TObjectList<TExportEntry>;
    AExport            : TExportEntry;
    ATotalExportsCount : UInt64;
    ADestination       : String;
begin
  Queue(procedure begin
    FForm.Show();
  end);
  try
    if not Assigned(FFrame) then
      Exit();
    ///

    AItems := TObjectDictionary<String, TObjectList<TExportEntry>>.Create([doOwnsValues]);
    try
      Synchronize(procedure begin
        var pNode    : PVirtualNode;
        var pData    : PTreeData;
        var AExports : TObjectList<TExportEntry>;

        for pNode in FFrame.VST.Nodes do begin
          pData := pNode.GetData;
          if not Assigned(pData) then
            continue;
          ///

          /// Could mean parent or in error
          if not Assigned(pData^.ExportEntry) then
            continue;

          case FMode of
            jemAll: ;

            jemVisible: begin
              if not (vsVisible in pNode.States) then
                continue;
            end;

            jemSelected: begin
              if not (vsSelected in pNode.States) then
                continue;
            end;
          end;

          if not AItems.TryGetValue(pData^.ImagePath, AExports) then begin
            AExports := TObjectList<TExportEntry>.Create(True);

            AItems.Add(pData^.ImagePath, AExports);
          end;

          if pData^.ExportEntry is TPEExportEntry then
            AExports.Add(TPEExportEntry.Create(TPEExportEntry(pData^.ExportEntry)));  // TODO
        end;
      end);

      AJson := SO();
      ///

      AJsonImages := SA();

      ATotalExportsCount := 0;

      for AImagePath in AItems.Keys do begin
        if Terminated then
          break;
        ///

        if not AItems.TryGetValue(AImagePath, AExports) then
          continue;
        ///

        AJsonImage := SO();

        AJsonImage.S['file'] := AImagePath;

        try
          AJsonImage.S['md5']  := System.Hash.THashMD5.GetHashStringFromFile(AImagePath);
        except
        end;

        try
          AJsonImage.S['sha1']  := System.Hash.THashSHA1.GetHashStringFromFile(AImagePath);
        except
        end;

        try
          AJsonImage.S['sha2']  := System.Hash.THashSHA2.GetHashStringFromFile(AImagePath);
        except
        end;

        AJsonExports := SA();

        for AExport in AExports do begin
          if Terminated then
            break;
          ///

          if AExport is TPEExportEntry then
            AJsonExports.Add(TPEExportEntry(AExport).ToJson());
        end;

        AJsonImage.A['exports'] := AJsonExports;

        ///
        AJsonImages.Add(AJsonImage);

        ///
        Inc(ATotalExportsCount, AExports.Count);
      end;

      AJson.I['files_count']  := AItems.Count;
      AJson.I['export_count'] := ATotalExportsCount;
      AJson.A['files']        := AJsonImages;

      if Terminated then
        Exit();
      ///

      ADestination := Format('%s%s.json', [
        IncludeTrailingPathDelimiter(FOutputDirectory),
        FormatDateTime('yyyy-mm-dd hh-nn-ss', Now())
      ]);

      AJson.SaveTo(ADestination, True);

      ///
      Queue(procedure begin
        FormMain.ShowInformation(Format('Export list successfully saved to "%s"', [ADestination]));
      end);
    finally
      if Assigned(AItems) then
        FreeAndNil(AItems);
    end;
  finally
    Queue(procedure begin
      FForm.Close();
    end);
  end;
end;

{ TExportExportsToJsonThread.Create }
constructor TExportExportsToJsonThread.Create(const AFrame : TFrameList; const AOutputDirectory : String; const AExportMode : TJSONExportMode);
begin
  inherited Create();
  ///

  FForm            := TFormTask.Create(FormMain, self);
  FFrame           := AFrame;
  FOutputDirectory := AOutputDirectory;
  FMode            := AExportMode;
end;

end.
