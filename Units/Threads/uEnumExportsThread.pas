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
     uFrameList;

type
  TEnumExportsMode = (
    eemFromFile,
    eemFromMemory
  );

  TEnumExportsThread = class(TThread)
  private
    FImageFiles    : TStringList;
    FTab           : TTabSheet;
    FFrame         : TFrameList;
    FMode          : TEnumExportsMode;
    FModules       : TList<Pointer>;
    FProcessId     : THandle;

    {@C}
    constructor Create(); overload;
  protected
    {@M}
    procedure Execute(); override;
  public
    {@C}
    constructor Create(const AImageFile : String); overload;
    constructor Create(const AImageFiles : TStringList; const ACaption : String); overload;

    constructor Create(const AProcessId : Cardinal); overload;

    destructor Destroy(); override;
  end;

implementation

uses VirtualTrees, uFormMain, uFunctions, uConstants, uExceptions;

{ TEnumExportsThread.Execute }
procedure TEnumExportsThread.Execute();
var APEParser     : TPortableExecutable;
    AImageFile    : String;
    pNode         : PVirtualNode;
    pData         : PTreeData;
    AExport       : TExport;
    AGroup        : Boolean;
    pParentNode   : PVirtualNode;
    ATotalExports : UInt64;
    pModule       : Pointer;
    hProcess      : THandle;

    procedure AddParent();
    begin
      if AGroup then begin
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
    end;

    procedure AddExport();
    begin
      Synchronize(procedure begin
        pNode := FFrame.VST.AddChild(pParentNode);
        pData := pNode.GetData;
      end);

      pData^.ImagePath   := AImageFile;
      pData^.ExportEntry := TExport.Create(AExport);
      pData^.StateIndex  := -1;
      pData^.ExportCount := 0;
    end;

begin
  try
    Synchronize(procedure begin
      FFrame.VST.BeginUpdate();
    end);

    AGroup := (FImageFiles.Count > 1) or (FModules.Count > 1);
    Synchronize(procedure begin
      FFrame.Grouped := AGroup;
    end);

    ATotalExports := 0;

    case FMode of
      eemFromFile: begin
        for AImageFile in FImageFiles do begin
          try
            APEParser := TPortableExecutable.CreateFromFile(AImageFile);
            try
              AddParent();

              for AExport in APEParser.ExportList do
                AddExport();

              ///
              Inc(ATotalExports, APEParser.ExportList.Count);
            finally
              if Assigned(APEParser) then
                FreeAndNil(APEParser);
            end;
          except
            // TODO
          end;
        end;
      end;

      eemFromMemory: begin
        hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, FProcessId);
        if hProcess = INVALID_HANDLE_VALUE then
          raise EWindowsException.Create('OpenProcess');
        try
          for pModule in FModules do begin
            try
              try
                AImageFile := DELF_GetModuleFileNameEx(hProcess, NativeUInt(pModule));
              except
                AImageFile := Format('Unknown Module (0x%p)', [pModule]);
              end;

              APEParser := TPortableExecutable.CreateFromMemory(hProcess, pModule);
              try
                AddParent();

                for AExport in APEParser.ExportList do
                  AddExport();

                ///
                Inc(ATotalExports, APEParser.ExportList.Count);
              finally
                if Assigned(APEParser) then
                  FreeAndNil(APEParser);
              end;
            except
              on E : Exception do begin

              end;
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

      FFrame.VST.EndUpdate();
    end);

    ///
    ExitThread(0);
  end;
end;

{ TEnumExportsThread.Create }

constructor TEnumExportsThread.Create();
begin
  inherited Create(False);
  ///

  FImageFiles := TStringList.Create();

  FTab := TTabSheet.Create(FormMain.Pages);
  FTab.PageControl := FormMain.Pages;

  FFrame := TFrameList.Create(FTab);
  FFrame.Parent := FTab;

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

  if AImageFiles.Count = 1 then
    Create(AImageFiles.Strings[0])
  else begin
    FImageFiles.Assign(AImageFiles);

    FTab.Caption    := ACaption;
    FTab.ImageIndex := _ICON_PAGES_DLL_GROUP;
  end;
end;

constructor TEnumExportsThread.Create(const AProcessId : Cardinal);
var AModulesCount : Cardinal;
    ACBNeeded     : Cardinal;
    AModules      : array of HMODULE;
    I             : Cardinal;
    hProcess      : THandle;
begin
  Create();
  ///

  FMode := eemFromMemory;

  FProcessId := AProcessId;

  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if hProcess = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('OpenProcess');
  try
    FModules.Clear();

    if not EnumProcessModules(hProcess, nil, 0, ACBNeeded) then
      raise EWindowsException.Create(Format('EnumProcessModules(call_1)[pid: %d]', [AProcessId]));

    AModulesCount := ACBNeeded div SizeOf(HMODULE);

    SetLength(AModules, AModulesCount);
    try
      if not EnumProcessModules(hProcess, @AModules[0], ACBNeeded, ACBNeeded) then
        raise EWindowsException.Create(Format('EnumProcessModules(call_2)[pid: %d]', [AProcessId]));

      for I := 0 to AModulesCount -1 do
        FModules.Add(Pointer(AModules[I]));
    finally
      SetLength(AModules, 0);
    end;
  finally
    if hProcess <> INVALID_HANDLE_VALUE then
      CloseHandle(hProcess);
  end;

  FTab.Caption := Format('%s (%d)', [
    ExtractFileName(GetImagePathFromProcessId(AProcessId)),
    AProcessId
  ]);

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
