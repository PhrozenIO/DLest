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

unit uEnumExportsThread;

interface

uses System.Classes,
     System.SysUtils,
     Generics.Collections,
     VCL.ComCtrls,
     Winapi.Windows,
     Winapi.PsAPI,
     uPortableExecutable,
     uFrameList,
     uWorkerThread,
     uFormExtendedLibrariesInformation;

type
  TEnumExportsMode = (
    eemFromFile,
    eemFromMemory
  );

  TBeginUpdateMessageParam = record
    Task    : TThread;
    Grouped : Boolean;
    Max     : UInt64;
  end;
  PBeginUpdateMessageParam = ^TBeginUpdateMessageParam;

  TEndUpdateMessageParam = record
    TotalExports : UInt64;
    Canceled     : Boolean;
  end;
  PEndUpdateMessageParam = ^TEndUpdateMessageParam;

  TEnumExportsThread = class(TWorkerThread)
  private
    FImageFiles  : TStringList;
    FTab         : TTabSheet;
    FFrame       : TFrameList;
    FMode        : TEnumExportsMode;
    FModules     : TList<Pointer>;
    FProcessId   : THandle;
    FGroup       : Boolean;
    FScanOptions : TScanOptions;

    {@C}
    function ProcessImage(const AImageFile : String; const APEParser : TPortableExecutable) : UInt64;
  protected
    {@M}
    procedure ThreadExecute(); override;
  public
    {@C}
    constructor Create(); override;
    constructor Create(const AImageFile : String); overload;
    constructor Create(const AImageFiles : TStringList; const ACaption : String; const AIconIndex : Integer); overload;

    constructor Create(const AProcessId : Cardinal; const AModules : TList<Pointer> = nil); overload;

    destructor Destroy(); override;
  end;

implementation

uses VirtualTrees, uFormMain, uFunctions, uConstants, uExceptions, VirtualTrees.Types;

{ TEnumExportsThread.ProcessImage }
function TEnumExportsThread.ProcessImage(const AImageFile : String; const APEParser : TPortableExecutable) : UInt64;
var AExport     : TExportEntry;
    pNode       : PVirtualNode;
    pData       : uFrameList.PTreeData;
    pParentNode : PVirtualNode;
begin
  result := 0;
  ///

  if not Assigned(APEParser) then
    Exit();
  ///

  try
    if FGroup and (APEParser.ExportList.Count > 0) then begin
      Synchronize(procedure begin
        pParentNode := FFrame.VST.AddChild(nil);
        pData := pParentNode.GetData;
      end);

      pData^.ImagePath   := AImageFile;
      pData^.ExportEntry := nil;
      pData^.StateIndex  := SystemFileIcon(AImageFile);
      pData^.ExportCount := APEParser.ExportList.Count;
    end else
      pParentNode := nil;
    ///

    if APEParser.ExportList.Count = 0 then
      Queue(procedure begin
        FormMain.Warn(Format('No exports so far for file "%s".', [AImageFile]), self);
      end)
    else begin
      for AExport in APEParser.ExportList do begin
        if Terminated then
          break;
        ///

        Synchronize(procedure begin
          pNode := FFrame.VST.AddChild(pParentNode);
          pData := pNode.GetData;
        end);

        pData^.ImagePath   := AImageFile;

        if AExport is TPEExportEntry then
          pData^.ExportEntry := TPEExportEntry.Create(TPEExportEntry(AExport))
        else if AExport is TCOMExportEntry then
          pData^.ExportEntry := TCOMExportEntry.Create(TCOMExportEntry(AExport));

        pData^.StateIndex  := -1;
        pData^.ExportCount := 0;
      end;

      ///
      result := APEParser.ExportList.Count;
    end;
  finally
    PostMessage(FFrame.Handle, WM_MESSAGE_INCREMENT_PB, 0, 0);
  end;
end;

{ TEnumExportsThread.ThreadExecute }
procedure TEnumExportsThread.ThreadExecute();
var APEParser     : TPortableExecutable;
    AImageFile    : String;
    ATotalExports : UInt64;
    pModule       : Pointer;
    hProcess      : THandle;

    procedure RaiseException(const AImageFile : String; const E : Exception);
    begin
      E.Message := Format('Could not parse:"%s", error:"%s"', [AImageFile, E.Message]);
      ///

      // I sometimes prefer to use Queue over the pure Windows PostMessage method.
      // This is mainly due to personal code design preferences.
      // I rarely like mixing main-thread destination code within thread code
      // using Queue. In some cases, it doesn't pose any problems if the mixed
      // code is very small and straightforward.
      Queue(procedure begin
        FormMain.OnException(self, E);
      end);
    end;

begin
  try
    FGroup := (FImageFiles.Count > 1) or (FModules.Count > 1);
    ///

    // Notify Frame for Begin Update
    var pParam : PBeginUpdateMessageParam;
    New(pParam);

    pParam^.Grouped := FGroup;
    pParam^.Max     := (FImageFiles.Count + FModules.Count);
    pParam^.Task    := self;

    if not PostMessage(FFrame.Handle, WM_MESSAGE_BEGIN_UPDATE, 0, LPARAM(pParam)) then
      Dispose(pParam);

    ATotalExports := 0;

    case FMode of
      eemFromFile: begin
        for AImageFile in FImageFiles do begin
          if Terminated then
            break;
          ///

          // Resolve Shortcut if it is needed.
          var AResolvedImageFile := CleanFileName(GetShortcutTarget(AImageFile));

          if not FileExists(AResolvedImageFile) then
            continue;

          try
            APEParser := TPortableExecutable.CreateFromFile(AResolvedImageFile, FScanOptions);
            try
              Inc(ATotalExports, ProcessImage(AResolvedImageFile, APEParser));
            finally
              if Assigned(APEParser) then
                FreeAndNil(APEParser);
            end;
          except
            on E : Exception do
              RaiseException(AResolvedImageFile, E);
          end;
        end;
      end;

      eemFromMemory: begin
        hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, FProcessId);
        if hProcess = INVALID_HANDLE_VALUE then
          raise EWindowsException.Create('OpenProcess');
        try
          for pModule in FModules do begin
            if Terminated then
              break;
            ///
            try
              try
                AImageFile := CleanFileName(GetModuleFileNameEx_FromHandle(hProcess, NativeUInt(pModule)));
              except
                AImageFile := Format('Unknown Module (0x%p)', [pModule]);
              end;

              APEParser := TPortableExecutable.CreateFromMemory_HANDLE(hProcess, pModule, FScanOptions);
              try
                Inc(ATotalExports, ProcessImage(AImageFile, APEParser));
              finally
                if Assigned(APEParser) then
                  FreeAndNil(APEParser);
              end;
            except
              on E : Exception do
                RaiseException(AImageFile, E);
            end;
          end;
        finally
          if hProcess <> INVALID_HANDLE_VALUE then
            CloseHandle(hProcess);
        end;
      end;
    end;
  finally
    // Notify Frame for End Update
    var pParam : PEndUpdateMessageParam;
    New(pParam);

    pParam^.TotalExports := ATotalExports;
    pParam^.Canceled     := Terminated;

    if not PostMessage(FFrame.Handle, WM_MESSAGE_END_UPDATE, 0, LPARAM(pParam)) then
      Dispose(pParam);
  end;
end;

{ TEnumExportsThread.Create }

constructor TEnumExportsThread.Create();
begin
  inherited Create();
  ///

  FImageFiles := TStringList.Create();

  FTab := TTabSheet.Create(FormMain.Pages);
  FTab.PageControl := FormMain.Pages;

  FFrame := TFrameList.Create(FTab);
  FFrame.Parent := FTab;

  FormMain.Pages.ActivePage := FTab;

  FModules := TList<Pointer>.Create();

  FScanOptions := FormMain.ScanOptions;

  FMode := eemFromFile;
end;

constructor TEnumExportsThread.Create(const AImageFile : String);
begin
  Create();
  ///

  FImageFiles.Add(AImageFile);

  FTab.Caption := ExtractFileName(AImageFile);
  FTab.ImageIndex := SystemFileIcon(AImageFile);
end;

constructor TEnumExportsThread.Create(const AImageFiles : TStringList; const ACaption : String; const AIconIndex : Integer);
begin
  Create();
  ///

  if not Assigned(AImageFiles) then
    raise Exception.Create('You must specify a valid instance of "TStringList".');

  FImageFiles.Assign(AImageFiles);

  // Rather than setting "TStringList.Duplicates" to dupIgnore for each instance where "TStringList" is used to store images,
  // I prefer to remove all duplicates at this point in the assigned list to ensure it is deduplicated.
  RemoveDuplicates(FImageFiles);

  FTab.Caption := ACaption;
  FTab.ImageIndex := AIconIndex;
end;

constructor TEnumExportsThread.Create(const AProcessId : Cardinal; const AModules : TList<Pointer> = nil);
var AModulesCount  : Cardinal;
    ACBNeeded      : Cardinal;
    AModuleHandles : array of HMODULE;
    I              : Cardinal;
    hProcess       : THandle;
    AModuleName    : String;
    AProcessName   : String;
    AImagePath     : String;
begin
  Create();
  ///

  FMode := eemFromMemory;

  FProcessId := AProcessId;

  FModules.Clear();

  if not Assigned(AModules) then begin
    hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
    if hProcess = INVALID_HANDLE_VALUE then
      raise EWindowsException.Create('OpenProcess');
    try
      if not EnumProcessModules(hProcess, nil, 0, ACBNeeded) then
        raise EWindowsException.Create(Format('EnumProcessModules(call_1)[pid: %d]', [AProcessId]));

      AModulesCount := ACBNeeded div SizeOf(HMODULE);

      SetLength(AModuleHandles, AModulesCount);
      try
        if not EnumProcessModules(hProcess, @AModuleHandles[0], ACBNeeded, ACBNeeded) then
          raise EWindowsException.Create(Format('EnumProcessModules(call_2)[pid: %d]', [AProcessId]));

        for I := 0 to AModulesCount -1 do
          FModules.Add(Pointer(AModuleHandles[I]));
      finally
        SetLength(AModuleHandles, 0);
      end;
    finally
      if hProcess <> INVALID_HANDLE_VALUE then
        CloseHandle(hProcess);
    end;
  end else begin
    FModules.AddRange(AModules.ToArray);
  end;

  try
    AImagePath := GetImagePathFromProcessId(AProcessId);
    AProcessName := ExtractFileName(AImagePath);
  except
    AProcessName := 'Unknown';
  end;

  if FModules.Count = 1 then begin
    try
      AModuleName := ExtractFileName(GetModuleFileNameEx_FromPID(AProcessId, THandle(FModules.Items[0])));
    except
      AModuleName := Format('0x%p', [FModules.Items[0]]);
    end;

    FTab.Caption := Format('%s_%s (%d)', [
      AProcessName,
      AModuleName,
      AProcessId
    ]);
  end else if FModules.Count > 1 then begin
    FTab.Caption := Format('%s (%d)', [
      AProcessName,
      AProcessId
    ]);
  end else
    raise Exception.Create(Format('No module so far for process "%d".', [AProcessId]));

  ///
  FTab.ImageIndex := SystemFileIcon(AImagePath);
end;

destructor TEnumExportsThread.Destroy();
begin
  if Assigned(FImageFiles) then
    FreeAndNil(FImageFiles);

  if Assigned(FModules) then
    FreeAndNil(FModules);

  ///
  inherited Destroy();
end;

end.
