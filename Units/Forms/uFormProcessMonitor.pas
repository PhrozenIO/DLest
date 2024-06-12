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

unit uFormProcessMonitor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.ExtCtrls, System.SyncObjs,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees, Vcl.Menus;

type
  TRowKind = (
    rkProcess,
    rkLibrary
  );

  TTreeData = record
    ImageFile  : String;
    ProcessId  : Cardinal;
    Base       : Pointer;

    Kind       : TRowKind;
    Dead       : Boolean;

    ImageIndex : Integer;
  end;
  PTreeData = ^TTreeData;

  TFormProcessMonitor = class(TForm)
    PanelBottom: TPanel;
    ButtonContinue: TSpeedButton;
    ButtonPlay: TSpeedButton;
    ButtonStop: TSpeedButton;
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    CalculateSelectedLibraryHashes1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonContinueClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVstTextType; var CellText: string);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure PopupMenuPopup(Sender: TObject);
    procedure CalculateSelectedLibraryHashes1Click(Sender: TObject);
  private
    FStopEvent     : TEvent;
    FPlayEvent     : TEvent;
    FContinueEvent : TEvent;
    FDebuggedImage : String;
    FDebuggedPID   : Cardinal;

    {@M}
    procedure BrowseListedLibrariesExports();
    function GetProcessNodeById(const AProcessId : Cardinal) : PVirtualNode;
    function RegisterOrGetProcess(const AProcessId : Cardinal) : PVirtualNode;
    procedure RegisterLibrary(AImageFile : String; const pImageBase : Pointer; const AProcessId : Cardinal);
    procedure SignalEvent(const AEvent : TEvent);
    procedure DoResize();
    procedure Reset();
  protected
    {@M}
    procedure CreateParams(var Params: TCreateParams); override;
  public
    {@M}
    procedure DebugStarted(const ADebuggedPID : Cardinal; const AStopEvent, APlayEvent, AContinueEvent : TEvent);
    procedure NotifyEvent(const ADebugEvent : TDebugEvent; const AResetButtons : Boolean);
    procedure DebugStopped();
  end;

var
  FormProcessMonitor: TFormProcessMonitor;

implementation

uses uFormMain, uExceptions, uFunctions, uFormLogs, uDebugProcessHelper, System.IOUtils,
     uFormThreadManager, uEnumExportsThread, VirtualTrees.Types, uFormHashMe;

{$R *.dfm}

procedure TFormProcessMonitor.CalculateSelectedLibraryHashes1Click(
  Sender: TObject);
var AFiles : TStringList;
begin
  AFiles := TStringList.Create();
  try
    for var pNode in VST.Nodes do begin
      if (vsSelected in pNode.States) then begin
        var pData : PTreeData := pNode.GetData;
        ///

        AFiles.Add(pData^.ImageFile);
      end;
    end;

    ///
    FormHashMe.AddFiles(AFiles);
  finally
    if Assigned(AFiles) then
      FreeAndNil(AFiles);
  end;
end;

procedure TFormProcessMonitor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle and NOT WS_EX_APPWINDOW;

  Params.WndParent := 0;
end;


procedure TFormProcessMonitor.BrowseListedLibrariesExports();
var AImageList : TStringList;
begin
  AImageList := TStringList.Create();
  try
    for var pNode in VST.Nodes do begin
      var pData : PTreeData := pNode.GetData;
      if not Assigned(pData) then
        Exit();
      ///

      if not pNode.CheckState.IsChecked then
        continue;

      if pData^.Kind <> rkLibrary then
        continue;

      ///
      AImageList.Add(pData^.ImageFile);
    end;
  finally
    FormThreadManager.AddWorkerAndStart(TEnumExportsThread.Create(
      AImageList,
      Format('%s (Debug)', [
        ExtractFileName(FDebuggedImage)
      ]),
      SystemFileIcon(FDebuggedImage)
    ));
  end;
end;

procedure TFormProcessMonitor.Reset();
begin
  FStopEvent     := nil;
  FPlayEvent     := nil;
  FContinueEvent := nil;
  ///

  ButtonStop.Enabled     := False;
  ButtonPlay.Enabled     := False;
  ButtonContinue.Enabled := False;
end;

procedure TFormProcessMonitor.SignalEvent(const AEvent : TEvent);
begin
  if Assigned(AEvent) then
    AEvent.SetEvent();

  ///
  ButtonContinue.Enabled := False;
  ButtonPlay.Enabled     := False;
end;

procedure TFormProcessMonitor.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormProcessMonitor.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormProcessMonitor.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PTreeData;
begin
  if Kind <> TVTImageKind.ikState then
    Exit();
  ///

  if Column <> 0 then
    Exit();

  pData := Node.GetData;

  ///
  ImageIndex := pData^.ImageIndex;
end;

procedure TFormProcessMonitor.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormProcessMonitor.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVstTextType;
  var CellText: string);
var pData : PTreeData;
begin
  CellText := '';
  ///

  pData := Node.GetData;

  case Column of
    0 : CellText := ExtractFileName(pData^.ImageFile);

    1 : begin
      if pData^.Kind = rkProcess then
        CellText := IntToStr(pData^.ProcessId)
      else
        CellText := Format('0x%p', [pData.Base]);
    end;

    2 : CellText := pData^.ImageFile;
  end;
end;

procedure TFormProcessMonitor.ButtonContinueClick(Sender: TObject);
begin
  SignalEvent(FContinueEvent);
end;

procedure TFormProcessMonitor.ButtonPlayClick(Sender: TObject);
begin
  SignalEvent(FPlayEvent);

  ///
  SignalEvent(FContinueEvent);

  ///
  FPlayEvent     := nil;
  FContinueEvent := nil;
end;

procedure TFormProcessMonitor.ButtonStopClick(Sender: TObject);
begin
  if Assigned(FStopEvent) then
    FStopEvent.SetEvent();

  ///
  SignalEvent(FContinueEvent);

  ///
  FStopEvent := nil;

  BrowseListedLibrariesExports();

  ///
  self.Close();
end;

procedure TFormProcessMonitor.DebugStarted(const ADebuggedPID : Cardinal; const AStopEvent, APlayEvent, AContinueEvent : TEvent);
begin
  FStopEvent     := AStopEvent;
  FPlayEvent     := APlayEvent;
  FContinueEvent := AContinueEvent;
  ///

  FDebuggedPID   := ADebuggedPID;

  try
    FDebuggedImage := TDebugProcessHelper.GetProcessImageFileName_FromPID(FDebuggedPID);
  except
    FDebuggedImage := 'Unnamed';
  end;

  ///
  ButtonStop.Enabled     := True;
  ButtonPlay.Enabled     := True;
  ButtonContinue.Enabled := False;
end;

procedure TFormProcessMonitor.DebugStopped();
begin
  self.Reset();
end;

function TFormProcessMonitor.GetProcessNodeById(const AProcessId : Cardinal) : PVirtualNode;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData : PTreeData := pNode.GetData;
    if not Assigned(pData) then
      continue;
    ///

    if pData^.Kind <> rkProcess then
      continue;

    if pData^.ProcessId = AProcessId then begin
      // These two lines could be replaced by `Exit(pNode)` but I don't like !
      result := pNode;

      break;
    end;
  end;
end;

function TFormProcessMonitor.RegisterOrGetProcess(const AProcessId : Cardinal) : PVirtualNode;
var ADebugHelper     : TDebugProcessHelper;
    AParentProcessId : Cardinal;
    AImageFile       : String;
begin
  ADebugHelper := TDebugProcessHelper.CreateFromPID(AProcessId);
  try
    AParentProcessId := ADebugHelper.GetParentProcessIdByProcessId(AProcessId);
    AImageFile := ADebugHelper.GetProcessImageFileName();
    ///

    result := GetProcessNodeById(AProcessId);
    if not Assigned(result) then begin
      // Create

      {TODO -oDarkCoderSc -cGeneral : Track process parent alteration / migration}
      VST.BeginUpdate();
      try
        var pParent := GetProcessNodeById(AParentProcessId);

        result := VST.AddChild(pParent);
        result.CheckType := ctTriStateCheckBox;

        var pData : PTreeData := result.GetData;

        pData^.ImageFile  := CleanFileName(AImageFile);
        pData^.ProcessId  := AProcessId;
        pData^.Kind       := rkProcess;
        pData^.Dead       := False;
        pData^.Base       := nil;
        pData^.ImageIndex := SystemFileIcon(AImageFile);
      finally
        VST.FullExpand(nil);

        ///
        VST.EndUpdate();
      end;
    end;
  finally
    if Assigned(ADebugHelper) then
      FreeAndNil(ADebugHelper);
  end;
end;

procedure TFormProcessMonitor.RegisterLibrary(AImageFile : String; const pImageBase : Pointer; const AProcessId : Cardinal);
var pParent : PVirtualNode;
    pNode   : PVirtualNode;
    pData   : PTreeData;
begin
  pParent := RegisterOrGetProcess(AProcessId);
  ///

  {TODO -oDarkCoderSc -cGeneral : GetWorkingDirectory via NtQueryInformationProcess for Priority Checking}
  if not FileExists(AImageFile) then begin
    // Level 1
    var ATemp := SearchPath_DELF(AImageFile);
    if FileExists(ATemp) then
      AImageFile := ATemp
  end;

  VST.BeginUpdate();
  try
    pNode := VST.AddChild(pParent);
    pNode.CheckType := ctCheckBox;
    //pNode.CheckState := csCheckedNormal;

    // pNode.CheckState simply toggle, we want to update parent
    VST.CheckState[pNode] := csCheckedNormal;

    pData := pNode.GetData;

    pData^.ImageFile  := AImageFile;
    pData^.ProcessId  := AProcessId;
    pData^.Base       := pImageBase;
    pData^.Kind       := rkLibrary;
    pData^.Dead       := False;
    pData^.ImageIndex := SystemFileIcon(AImageFile);
  finally
    VST.FullExpand();

    ///
    VST.EndUpdate();
  end;
end;

procedure TFormProcessMonitor.NotifyEvent(const ADebugEvent : TDebugEvent; const AResetButtons : Boolean);
var ADebugHelper : TDebugProcessHelper;
begin
  try
    ADebugHelper := TDebugProcessHelper.CreateFromPID(ADebugEvent.dwProcessId);
    try
      case ADebugEvent.dwDebugEventCode of
        //
        // Process Exit
        //
        EXIT_PROCESS_DEBUG_EVENT : begin
          {TODO -oDarkCoderSc -cGeneral : Signal Process Exit on List}
        end;

        //
        //  DLL Load
        //
        LOAD_DLL_DEBUG_EVENT : begin
          var AImageFile := ADebugHelper.ReadRemoteString_PTRPTR(
            ADebugEvent.LoadDll.lpImageName,
            ADebugEvent.LoadDll.fUnicode <> 0
          );

          var pImageBase := ADebugEvent.LoadDll.lpBaseOfDll;

          // @Add
          RegisterLibrary(AImageFile, pImageBase, ADebugEvent.dwProcessId);
        end;

        //
        // DLL Unload
        //
        UNLOAD_DLL_DEBUG_EVENT : begin
          // var pImageBase := ADebugEvent.UnloadDll.lpBaseOfDll;

          // @Signal(Unload)
          {TODO -oDarkCoderSc -cGeneral : Signal DLL Unload on List}
        end;
      end;
    finally
      if Assigned(ADebugHelper) then
        FreeAndNil(ADebugHelper);
    end;
  except
    on E : Exception do begin
      FormLogs.Log(E.Message, self, TLogLevel.llException);
    end;
  end;

  ///
  if AResetButtons then begin
    ButtonStop.Enabled     := Assigned(FStopEvent);
    ButtonPlay.Enabled     := Assigned(FPlayEvent);
    ButtonContinue.Enabled := Assigned(FContinueEvent);
  end;
end;

procedure TFormProcessMonitor.PopupMenuPopup(Sender: TObject);
begin
  CalculateSelectedLibraryHashes1.Enabled := VST.SelectedCount > 0;
end;

procedure TFormProcessMonitor.DoResize();
begin
  ButtonStop.Top     := (PanelBottom.Height div 2) - (ButtonStop.Height div 2);
  ButtonPlay.Top     := ButtonStop.Top;
  ButtonContinue.Top := ButtonStop.Top;
  ///

  ButtonPlay.Left     := (PanelBottom.Width div 2) - (ButtonPlay.Width div 2);
  ButtonStop.Left     := ButtonPlay.Left - ButtonStop.Width - 4;
  ButtonContinue.Left := ButtonPlay.Left + ButtonPlay.Width + 4;
end;

procedure TFormProcessMonitor.FormCreate(Sender: TObject);
begin
  self.Reset();
  ///

  FDebuggedPID   := 0;
  FDebuggedImage := '';

  DoResize();
end;

procedure TFormProcessMonitor.FormShow(Sender: TObject);
begin
  DoResize();
end;

end.
