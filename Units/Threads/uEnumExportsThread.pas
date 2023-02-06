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

  TEnumExportsThread = class(TWorkerThread)
  private
    FImageFiles : TStringList;
    FTab        : TTabSheet;
    FFrame      : TFrameList;
    FExtForm    : TFormExtendedLibrariesInformation;
    FMode       : TEnumExportsMode;
    FModules    : TList<Pointer>;
    FProcessId  : THandle;
    FGroup      : Boolean;

    {@C}
    constructor Create(); overload;
    function ProcessImage(const AImageFile : String; const APEParser : TPortableExecutable) : UInt64;
    procedure ProcessImageExtendedInformation(const AImageFile : String; const AExportsCount : UInt64);
  protected
    {@M}
    procedure ThreadExecute(); override;
  public
    {@C}
    constructor Create(const AImageFile : String); overload;
    constructor Create(const AImageFiles : TStringList; const ACaption : String); overload;

    constructor Create(const AProcessId : Cardinal; const AModules : TList<Pointer> = nil); overload;

    destructor Destroy(); override;
  end;

implementation

uses VirtualTrees, uFormMain, uFunctions, uConstants, uExceptions, System.Hash;

{ TEnumExportsThread.ProcessImageExtendedInformation }
procedure TEnumExportsThread.ProcessImageExtendedInformation(const AImageFile : String; const AExportsCount : UInt64);
var pData     : uFormExtendedLibrariesInformation.PTreeData;
    pNode     : PVirtualNode;
    AFileSize : UInt64;
begin
  if AExportsCount = 0 then
    Exit();
  ///

  AFileSize := GetFileSize(AImageFile);

  Synchronize(procedure begin
    FFrame.ExtForm.IncTotalFilesSize(AFileSize); // TODO Dirty
    FFrame.ExtForm.IncTotalExports(AExportsCount); // TODO Dirty
    pNode := FFrame.ExtForm.VST.AddChild(nil); // TODO Dirty
    pData := pNode.GetData;
  end);

  pData^.ImagePath    := AImageFile;
  pData^.ExportsCount := AExportsCount;
  pData^.FileSize     := AFileSize;

  try
    pData^.CompanyName  := GetApplicationCompany(AImageFile);
    pData^.FileVersion  := GetApplicationVersion(AImageFile);
    pData^.MD5 := System.Hash.THashMD5.GetHashStringFromFile(AImageFile);
    pData^.SHA1 := System.Hash.THashSHA1.GetHashStringFromFile(AImageFile);
    pData^.SHA2 := System.Hash.THashSHA2.GetHashStringFromFile(AImageFile);
  except
  end;

  pData^.ImageIndex  := SystemFileIcon(AImageFile);
end;

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
    ProcessImageExtendedInformation(AImageFile, APEParser.ExportList.Count); // TODO Dirty
    ///

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
    Synchronize(procedure begin
      FFrame.ProgressBar.Position := FFrame.ProgressBar.Position +1;
    end);
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

      Synchronize(procedure begin
        FormMain.OnException(self, E);
      end);
    end;

begin
  try
    Synchronize(procedure begin
      FFrame.BeginUpdate();

      FFrame.ProgressBar.Max := FImageFiles.Count + FModules.Count;
      FFrame.ProgressBar.Visible :=  FFrame.ProgressBar.Max > 1;
    end);

    FGroup := (FImageFiles.Count > 1) or (FModules.Count > 1);
    Synchronize(procedure begin
      FFrame.Grouped := FGroup;
    end);

    ATotalExports := 0;

    case FMode of
      eemFromFile: begin
        for AImageFile in FImageFiles do begin
          if Terminated then
            break;
          try
            APEParser := TPortableExecutable.CreateFromFile(AImageFile, True); // TODO
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
      end;

      eemFromMemory: begin
        hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, FProcessId);
        if hProcess = INVALID_HANDLE_VALUE then
          raise EWindowsException.Create('OpenProcess');
        try
          for pModule in FModules do begin
            if Terminated then
              break;
            try
              try
                AImageFile := DELF_GetModuleFileNameEx(hProcess, NativeUInt(pModule));
              except
                AImageFile := Format('Unknown Module (0x%p)', [pModule]);
              end;

              APEParser := TPortableExecutable.CreateFromMemory(hProcess, pModule);
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
    Synchronize(procedure begin
      FFrame.TotalExports := ATotalExports;

      FFrame.VST.FullExpand();

      FFrame.ProgressBar.Visible := False;

      FFrame.EndUpdate();
    end);
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

  FExtForm := FFrame.ExtForm;

  FormMain.Pages.ActivePage := FTab;

  FModules := TList<Pointer>.Create();

  FMode := eemFromFile;
end;

constructor TEnumExportsThread.Create(const AImageFile : String);
begin
  Create();
  ///

  // TODO check in PE Header if it is library
  // If it is a valid PE but not a library, then list imported libraries

  FImageFiles.Add(AImageFile);

  FTab.Caption := ExtractFileName(AImageFile);
  FTab.ImageIndex := _ICON_PAGES_DLL;
end;

constructor TEnumExportsThread.Create(const AImageFiles : TStringList; const ACaption : String);
begin
  Create();
  ///

  if not Assigned(AImageFiles) then
    raise Exception.Create('You must specify a valid instance of "TStringList".');

  FImageFiles.Assign(AImageFiles);

  FTab.Caption    := ACaption;
  FTab.ImageIndex := _ICON_PAGES_DLL_GROUP;
end;

constructor TEnumExportsThread.Create(const AProcessId : Cardinal; const AModules : TList<Pointer> = nil);
var AModulesCount  : Cardinal;
    ACBNeeded      : Cardinal;
    AModuleHandles : array of HMODULE;
    I              : Cardinal;
    hProcess       : THandle;
    AModuleName    : String;
    AProcessName   : String;
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
    AProcessName := ExtractFileName(GetImagePathFromProcessId(AProcessId));
  except
    AProcessName := 'Unknown';
  end;

  if FModules.Count = 1 then begin
    try
      AModuleName := ExtractFileName(DELF_GetModuleFileNameEx(AProcessId, THandle(FModules.Items[0])));
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

  FTab.ImageIndex := _ICON_PAGES_PROCESS;
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
