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

unit uFormProcessList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, OMultiPanel, VirtualTrees,
  Vcl.VirtualImage, Vcl.StdCtrls, Vcl.Menus, Vcl.ComCtrls, Vcl.ToolWin;

type
  TProcessTreeData = record
    ImagePath  : String;
    ProcessId  : Cardinal;
    ImageIndex : Integer;
  end;
  PProcessTreeData = ^TProcessTreeData;

  TModuleTreeData = record
    ProcessId  : Cardinal;
    ModuleBase : Pointer;
    ModuleSize : DWORD;
    ImagePath  : String;
    ImageIndex : Integer;
  end;
  PModuleTreeData = ^TModuleTreeData;

  TFormProcessList = class(TForm)
    MultiPanel: TOMultiPanel;
    PanelProcess: TPanel;
    PanelModules: TPanel;
    VSTProcess: TVirtualStringTree;
    VSTModules: TVirtualStringTree;
    PanelMessage: TPanel;
    VirtualImage1: TVirtualImage;
    LabelMessage: TLabel;
    PopupProcess: TPopupMenu;
    OpenProcess1: TMenuItem;
    PopupModules: TPopupMenu;
    OpenSelectedModules1: TMenuItem;
    N1: TMenuItem;
    OpenSelectedModulesFromFiles1: TMenuItem;
    N2: TMenuItem;
    DumpReconstructedImageofSelectedModules1: TMenuItem;
    SelectAll1: TMenuItem;
    DeselectAll1: TMenuItem;
    N3: TMenuItem;
    ToolBar: TToolBar;
    ToolRefresh: TToolButton;
    procedure VSTProcessChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTProcessFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTProcessGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTProcessGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormShow(Sender: TObject);
    procedure VSTProcessGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTProcessNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure FormResize(Sender: TObject);
    procedure OpenProcess1Click(Sender: TObject);
    procedure PopupProcessPopup(Sender: TObject);
    procedure VSTModulesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTModulesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTModulesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTModulesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTProcessNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure VSTModulesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure VSTProcessCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTModulesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTModulesBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure OpenSelectedModules1Click(Sender: TObject);
    procedure PopupModulesPopup(Sender: TObject);
    procedure VSTModulesNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure OpenSelectedModulesFromFiles1Click(Sender: TObject);
    procedure DumpReconstructedImageofSelectedModules1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure DeselectAll1Click(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure ToolRefreshClick(Sender: TObject);
  private
    FTotalModulesSize : UInt64;

    {@M}
    procedure DoResize();
    procedure OpenProcess(const pNode : PVirtualNode);
    procedure OpenSelectedModulesFromMemory();
    procedure OpenSelectedModulesFromFiles();
    procedure CreateParams(var Params: TCreateParams); override;
  public
    {@M}
    procedure Reset();
    procedure Refresh();

    {@C}
    constructor Create(AOwner : TComponent); override;

    {@G/S}
    property TotalModulesSize : UInt64 read FTotalModulesSize write FTotalModulesSize;
  end;

var
  FormProcessList: TFormProcessList;

implementation

uses uEnumProcessThread, uEnumExportsThread, uFormMain, uEnumModulesThread,
     uFunctions, System.Math, uConstants, uGraphicUtils, Generics.Collections,
     uFormThreadManager, uPortableExecutable, VCL.FileCtrl;

{$R *.dfm}

procedure TFormProcessList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle and NOT WS_EX_APPWINDOW;

  Params.WndParent := 0;
end;

constructor TFormProcessList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Reset();
end;

procedure TFormProcessList.DeselectAll1Click(Sender: TObject);
begin
  VSTModules.ClearSelection();
end;

procedure TFormProcessList.DoResize();
var ARect       : TRect;
    ACaption    : String;
    AClientRect : TRect;
begin
  LabelMessage.Canvas.Font := LabelMessage.Font;

  ACaption := LabelMessage.Caption;

  ARect := LabelMessage.ClientRect;

  LabelMessage.Canvas.TextRect(ARect, ACaption, [tfCalcRect, tfWordBreak]);

  PanelMessage.ClientHeight := ARect.Height + LabelMessage.Margins.Top + LabelMessage.Margins.Bottom;
end;

procedure TFormProcessList.DumpReconstructedImageofSelectedModules1Click(
  Sender: TObject);
var APortableExecutable : TPortableExecutable;
    pNode               : PVirtualNode;
    pData               : PModuleTreeData;
    ADestFile           : String;
    ADirectory          : String;
begin
  if not SelectDirectory('Select output directory', '', ADirectory) then
    Exit();
  ///

  for pNode in VSTModules.Nodes do begin
    if not (vsSelected in pNode.States) then
      continue;
    ///

    pData := pNode.GetData;
    if not Assigned(pData) then
      Exit();
    ///

    APortableExecutable := TPortableExecutable.CreateFromMemory(
      pData^.ProcessId,
      pData^.ModuleBase
    );
    try
      ADestFile := Format('%s%s_%s', [
        IncludeTrailingPathDelimiter(ADirectory),
        Ternary(APortableExecutable.Is64, 'x64', 'x32'),
        ExtractFileName(pData^.ImagePath)
      ]);

      ///
      APortableExecutable.SaveToFile(ADestFile);
    finally
      if Assigned(APortableExecutable) then
        FreeAndNil(APortableExecutable);
    end;
  end;
end;

procedure TFormProcessList.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormProcessList.FormShow(Sender: TObject);
begin
  self.Refresh();

  self.DoResize();
end;

procedure TFormProcessList.Refresh();
begin
  TEnumProcessThread.Create();
end;

procedure TFormProcessList.Refresh1Click(Sender: TObject);
begin
  self.Refresh();
end;

procedure TFormProcessList.Reset();
begin
  VSTProcess.Clear();
  VSTModules.Clear();

  ///
  FTotalModulesSize := 0;
end;

procedure TFormProcessList.SelectAll1Click(Sender: TObject);
begin
  VSTModules.SelectAll(True);
end;

procedure TFormProcessList.ToolRefreshClick(Sender: TObject);
begin
  self.Refresh();
end;

procedure TFormProcessList.VSTModulesBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var pData     : PModuleTreeData;
    AColorA   : TColor;
    AColorB   : TColor;
    AProgress : TRect;
begin
  if Column <> 3 then
    Exit();
  ///

  pData := Node.GetData;

  if pData^.ModuleSize = 0 then
    Exit();

  AColorA := _COLOR_GRAD1_BEG;
  AColorB := _COLOR_GRAD1_END;

  AProgress := CellRect;

  AProgress.Width := (AProgress.Width * pData^.ModuleSize) div FTotalModulesSize;

  // Grandient
  DrawGradient(
    TargetCanvas,
    AColorA,
    AColorB,
    AProgress,
    False
  );

  CellPaintMode := cpmPaint;
end;

procedure TFormProcessList.VSTModulesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormProcessList.VSTModulesCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1 : PModuleTreeData;
    pData2 : PModuleTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Exit();

  case Column of
    0 : result := CompareText(
      ExtractFileName(pData1^.ImagePath),
      ExtractFileName(pData2^.ImagePath)
    );

    1 : result := CompareValue(pData1^.ProcessId, pData2^.ProcessId);
    2 : result := CompareValue(NativeUInt(pData1^.ModuleBase), NativeUInt(pData2^.ModuleBase));
    3 : result := CompareValue(pData1^.ModuleSize, pData2^.ModuleSize);
    4 : result := CompareText(pData1^.ImagePath, pData2^.ImagePath);
  end;
end;

procedure TFormProcessList.VSTModulesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormProcessList.VSTModulesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PModuleTreeData;
begin
  pData := Node.GetData;

  if Column <> 0 then
    Exit();

  if Kind <> ikState then
    Exit();

  ///
  ImageIndex := pData^.ImageIndex;
end;

procedure TFormProcessList.VSTModulesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TModuleTreeData);
end;

procedure TFormProcessList.VSTModulesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PModuleTreeData;
begin
  pData := Node.GetData;

  CellText := '';

  case Column of
    0 : CellText := ExtractFileName(pData^.ImagePath);
    1 : CellText := Format('0x%p (%d)', [
      Pointer(pData^.ProcessId),
      pData^.ProcessId
    ]);
    2 : CellText := Format('0x%p', [pData^.ModuleBase]);
    3 : CellText := FormatSize(pData^.ModuleSize);
    4 : CellText := pData^.ImagePath;
  end;
end;

procedure TFormProcessList.VSTModulesNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  self.OpenSelectedModulesFromMemory();
end;

procedure TFormProcessList.VSTProcessChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormProcessList.VSTProcessCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1 : PProcessTreeData;
    pData2 : PProcessTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Exit();

  case Column of
    0 : result := CompareText(
      ExtractFileName(pData1^.ImagePath),
      ExtractFileName(pData2^.ImagePath)
    );

    1 : result := CompareValue(pData1^.ProcessId, pData2^.ProcessId);
    2 : result := CompareText(pData1^.ImagePath, pData2^.ImagePath);
  end;
end;

procedure TFormProcessList.VSTProcessFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormProcessList.VSTProcessGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PProcessTreeData;
begin
  pData := Node.GetData;

  if Column <> 0 then
    Exit();

  if Kind <> ikState then
    Exit();

  ///
  ImageIndex := pData^.ImageIndex;
end;

procedure TFormProcessList.VSTProcessGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TProcessTreeData);
end;

procedure TFormProcessList.VSTProcessGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PProcessTreeData;
begin
  pData := Node.GetData;
  ///

  case Column of
    0 : CellText := ExtractFileName(pData^.ImagePath);
    1 : CellText := Format('0x%p (%d)', [
      Pointer(pData^.ProcessId),
      pData^.ProcessId
    ]);
    2 : CellText := pData^.ImagePath;
  end;
end;

procedure TFormProcessList.VSTProcessNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var pData : PProcessTreeData;
begin
  pData := VSTProcess.FocusedNode.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  TEnumModulesThread.Create(pData^.ProcessId);
end;

procedure TFormProcessList.VSTProcessNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  OpenProcess(HitInfo.HitNode);
end;

procedure TFormProcessList.OpenProcess(const pNode : PVirtualNode);
var pData: PProcessTreeData;
begin
  if not Assigned(pNode) then
    Exit();
  ///

  pData := pNode.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  FormThreadManager.AddWorkerAndStart(
    TEnumExportsThread.Create(
      pData^.ProcessId
    )
  );

  ///
  self.Close();
end;

procedure TFormProcessList.OpenProcess1Click(
  Sender: TObject);
begin
  OpenProcess(VSTProcess.FocusedNode);
end;

procedure TFormProcessList.PopupModulesPopup(Sender: TObject);
begin
  OpenSelectedModules1.Enabled          := VSTModules.FocusedNode <> nil;
  OpenSelectedModulesFromFiles1.Enabled := VSTModules.FocusedNode <> nil;
end;

procedure TFormProcessList.PopupProcessPopup(Sender: TObject);
begin
  self.OpenProcess1.Enabled := VSTProcess.FocusedNode <> nil;
end;

procedure TFormProcessList.OpenSelectedModulesFromMemory();
var pNode      : PVirtualNode;
    pData      : PModuleTreeData;
    AModules   : TList<Pointer>;
    AProcessId : Cardinal;
begin
  AModules := TList<Pointer>.Create();
  try
    AProcessId := 0;
    ///

    for pNode in VSTModules.Nodes(True) do begin
      if not (vsSelected in pNode.States) then
        continue;
      ///

      pData := pNode.GetData;
      if not Assigned(pData) then
        continue;
      ///

      AModules.Add(pData^.ModuleBase);

      if AProcessId = 0 then
        AProcessId := pData^.ProcessId;
    end;

    if AModules.Count > 0 then
      FormThreadManager.AddWorkerAndStart(
        TEnumExportsThread.Create(AProcessId, AModules)
      );
  finally
    if Assigned(AModules) then
      FreeAndNil(AModules);

    ///
    self.Close();
  end;
end;

procedure TFormProcessList.OpenSelectedModulesFromFiles();
var pNode        : PVirtualNode;
    pData        : PModuleTreeData;
    AFiles       : TStringList;
    ACaption     : String;
    AProcessName : String;
begin
  AFiles := TStringList.Create();
  try
    ACaption := '';
    ///

    for pNode in VSTModules.Nodes(True) do begin
      if not (vsSelected in pNode.States) then
        continue;
      ///

      pData := pNode.GetData;
      if not Assigned(pData) then
        continue;
      ///

      if FastPECheck(pData^.ImagePath) then
        AFiles.Add(pData^.ImagePath);

      if ACaption.IsEmpty then begin
        try
          AProcessName := ExtractFileName(GetImagePathFromProcessId(pData^.ProcessId));
        except
          AProcessName := 'Unknown';
        end;

        ///
        ACaption := Format('%s (%d)', [
                      AProcessName,
                      pData^.ProcessId
                    ]);
      end;
    end;

    if AFiles.Count > 0 then begin
      FormThreadManager.AddWorkerAndStart(
        TEnumExportsThread.Create(AFiles, ACaption)
      );
    end;
  finally
    if Assigned(AFiles) then
      FreeAndNil(AFiles);

    ///
    self.Close();
  end;
end;

procedure TFormProcessList.OpenSelectedModules1Click(Sender: TObject);
begin
  self.OpenSelectedModulesFromMemory();
end;

procedure TFormProcessList.OpenSelectedModulesFromFiles1Click(Sender: TObject);
begin
  self.OpenSelectedModulesFromFiles();
end;

end.
