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

unit uFrameList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees,
  Vcl.ExtCtrls, OMultiPanel, uPortableExecutable, Vcl.Menus, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Buttons, uTypes, uFormExtendedLibrariesInformation,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees.Types, uConstants;

type
  TTreeData = record
    ImagePath   : String;
    ExportEntry : TExportEntry;
    StateIndex  : Integer;
    ExportCount : UInt64;
  end;
  PTreeData = ^TTreeData;

  TExportStatistics = record
    Libraries            : UInt64;
    FilteredLibraries    : UInt64;
    Functions            : UInt64;
    FwdFunctions         : UInt64;
    FilteredFwdFunctions : UInt64;
    FilteredFunctions    : UInt64;
    COM                  : UInt64;
    FilteredCOM          : UInt64;

    ///
    function Total() : UInt64;
    function TotalFiltered() : UInt64;
  end;

  TFilterExportKindOptions = set of TExportKind;

  (* TFrameList *)

  TFrameList = class(TFrame)
    PopupMenu: TPopupMenu;
    ExpandAll1: TMenuItem;
    CollapseAll1: TMenuItem;
    N1: TMenuItem;
    CloseTab1: TMenuItem;
    PanelSearch: TPanel;
    EditRegex: TButtonedEdit;
    VST: TVirtualStringTree;
    RenameTab1: TMenuItem;
    N2: TMenuItem;
    ShowSelectedFileProperties1: TMenuItem;
    ShowSelectedFileOnExplorer1: TMenuItem;
    LoadSelectedFileinNewTab1: TMenuItem;
    N3: TMenuItem;
    ExportEntireListToJson1: TMenuItem;
    ExportVisibleFilteredItemsToJson1: TMenuItem;
    ExportSelectedItemsToJson1: TMenuItem;
    SelectAll1: TMenuItem;
    ClearSelection1: TMenuItem;
    N4: TMenuItem;
    Google1: TMenuItem;
    N5: TMenuItem;
    SearchLibraryName1: TMenuItem;
    SearchAPIName1: TMenuItem;
    N6: TMenuItem;
    SearchBoth1: TMenuItem;
    Copy1: TMenuItem;
    N7: TMenuItem;
    LibrariesViewExtendedInfo1: TMenuItem;
    ButtonSearch: TSpeedButton;
    UnprotectSearch1: TMenuItem;
    SearchUnprotectSelectedLibraryName1: TMenuItem;
    SearchUnprotectSelectedAPIName1: TMenuItem;
    PanelProgressTask: TPanel;
    ProgressBarTask: TProgressBar;
    ButtonCancelTask: TSpeedButton;
    Filter1: TMenuItem;
    N8: TMenuItem;
    ExportFunctions1: TMenuItem;
    ExportFunctions2: TMenuItem;
    ForwardedFunctions1: TMenuItem;
    COMMethod1: TMenuItem;
    COMProperties1: TMenuItem;
    COMUnknown1: TMenuItem;
    N10: TMenuItem;
    ResetClear1: TMenuItem;
    N9: TMenuItem;
    CalculateSelectedLibrariesHashes1: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ExpandAll1Click(Sender: TObject);
    procedure CollapseAll1Click(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure EditRegexChange(Sender: TObject);
    procedure EditRegexRightButtonClick(Sender: TObject);
    procedure CloseTab1Click(Sender: TObject);
    procedure RenameTab1Click(Sender: TObject);
    procedure ShowSelectedFileProperties1Click(Sender: TObject);
    procedure ShowSelectedFileOnExplorer1Click(Sender: TObject);
    procedure LoadSelectedFileinNewTab1Click(Sender: TObject);
    procedure ExportEntireListToJson1Click(Sender: TObject);
    procedure ExportVisibleFilteredItemsToJson1Click(Sender: TObject);
    procedure ExportSelectedItemsToJson1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure ClearSelection1Click(Sender: TObject);
    procedure SearchLibraryName1Click(Sender: TObject);
    procedure SearchAPIName1Click(Sender: TObject);
    procedure SearchBoth1Click(Sender: TObject);
    procedure LibrariesViewExtendedInfo1Click(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure EditRegexKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SearchUnprotectSelectedLibraryName1Click(Sender: TObject);
    procedure SearchUnprotectSelectedAPIName1Click(Sender: TObject);
    procedure ButtonCancelTaskClick(Sender: TObject);
    procedure ResetClear1Click(Sender: TObject);
    procedure FilterMenuItemClick(Sender: TObject);
    procedure CalculateSelectedLibrariesHashes1Click(Sender: TObject);
  private
    FGrouped          : Boolean;
    FTotalExports     : UInt64;
    FExtForm          : TFormExtendedLibrariesInformation;
    FExtInfoLoaded    : Boolean;
    FDestroyRequested : Boolean;
    FEnumTask         : TThread;

    {@M}
    procedure FilterList();
    procedure ExportToJson(const AMode : TJSONExportMode);
    procedure CopyToClipboard(ASender : TObject);
    function HaveExportInSelection() : Boolean;
    function HiddenNodeCount() : Cardinal;
    function VisibleNodeCount() : Cardinal;
    procedure OnBeginUpdateMessage(var AMessage : TMessage); message WM_MESSAGE_BEGIN_UPDATE;
    procedure OnIncrementProgressBarMessage(var AMessage: TMessage); message WM_MESSAGE_INCREMENT_PB;
    procedure OnEndUpdateMessage(var AMessage : TMessage); message WM_MESSAGE_END_UPDATE;
    function GetUpdating() : Boolean;
  private
    {@M}
    procedure DoSearch();
    procedure UpdateStatusBar();
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure GetStatistics(var AStatistics : TExportStatistics);
    procedure SetupOrRefreshExtendedLibrariesInformation(const AForce : Boolean = False);

    {@G}
    property Updating : Boolean read GetUpdating;

    {@G/S}
    property ExtForm : TFormExtendedLibrariesInformation read FExtForm write FExtForm;
  end;

  ///

  const AUTO_SEARCH_DELTA = 1000;

implementation

uses uFormMain, System.Math, uGraphicUtils, System.RegularExpressions,
     uFunctions, uFormThreadManager, uEnumExportsThread, uExportExportsToJsonThread,
     VCL.FileCtrl, uVirtualStringTreeUtils, VCL.Clipbrd, Generics.Collections,
  uFormHashMe;

{$R *.dfm}

(* TExportStatistics *)

function TExportStatistics.Total() : UInt64;
begin
  result := self.Functions + self.FwdFunctions + self.COM;
end;

function TExportStatistics.TotalFiltered() : UInt64;
begin
  result := self.FilteredFunctions + self.FilteredFwdFunctions +
            self.FilteredCOM;
end;

(* TFrameList *)

procedure TFrameList.SetupOrRefreshExtendedLibrariesInformation(const AForce : Boolean = False);
var AImageList : TDictionary<String (* Image Path *), UInt64 (* Export Count *)>;
begin
  if (AForce = False) and FExtInfoLoaded then
    Exit();
  ///

  AImageList := TDictionary<String, UInt64>.Create();
  try
    // Retrieve Images
    if not FGrouped then begin
      // Single Malt
      var pNode := VST.GetFirst(True);
      if Assigned(pNode) then begin
        if Assigned(pNode.GetData()) then
          AImageList.Add(PTreeData(pNode.GetData)^.ImagePath, FTotalExports);
      end;
    end else begin
      // Grouped Mode
      for var pNode in VST.Nodes do begin
        if VST.GetNodeLevel(pNode) <> 0 then
          continue;

        var pData : PTreeData := pNode.GetData;
        if not Assigned(pData) then
          continue;
        ///

        AImageList.Add(pData^.ImagePath, pData^.ExportCount);
      end;
    end;
  finally
    FExtForm.RegisterImages(AImageList);

    ///
    FExtInfoLoaded := True;
  end;
end;

function TFrameList.GetUpdating() : Boolean;
begin
  result := (FEnumTask <> nil);
end;

procedure TFrameList.UpdateStatusBar();
begin
  if self.Parent = FormMain.Pages.ActivePage then
    FormMain.UpdateStatusBar();
end;

procedure TFrameList.GetStatistics(var AStatistics : TExportStatistics);
var pNode    : PVirtualNode;
    pData    : PTreeData;
    AVisible : Boolean;
begin
  ZeroMemory(@AStatistics, SizeOf(TExportStatistics));
  ///

  if Assigned(FEnumTask) or FDestroyRequested then
    Exit();

  for pNode in VST.Nodes do begin
    pData := pNode.GetData;
    if not Assigned(pData) then
      continue;
    ///

    AVisible := vsVisible in pNode.States;

    // Libraries
    if FGrouped then begin
      if not Assigned(pData^.ExportEntry) then begin
        if AVisible then
          Inc(AStatistics.Libraries)
        else
          Inc(AStatistics.FilteredLibraries);
      end;
    end else
      AStatistics.Libraries := 1;

    // Exports
    if Assigned(pData^.ExportEntry) then begin
      case pData^.ExportEntry.Kind of
        ekExportFunction : begin
          if AVisible then
            Inc(AStatistics.Functions)
          else
            Inc(AStatistics.FilteredFunctions);
        end;

        ekForwardedFunction : begin
          if AVisible then
            Inc(AStatistics.FwdFunctions)
          else
            Inc(AStatistics.FilteredFwdFunctions);
        end;

        ekCOMMethod, ekCOMProperty, ekCOMUnknown : begin
          if AVisible then
            Inc(AStatistics.COM)
          else
            Inc(AStatistics.FilteredCOM);
        end;
      end;
    end;
  end;
end;

procedure TFrameList.OnIncrementProgressBarMessage(var AMessage: TMessage);
begin
  ProgressBarTask.Position := ProgressBarTask.Position + 1;
end;

procedure TFrameList.OnBeginUpdateMessage(var AMessage : TMessage);
begin
  FGrouped := PBeginUpdateMessageParam(AMessage.LParam)^.Grouped;
  FEnumTask := PBeginUpdateMessageParam(AMessage.LParam)^.Task;
  ProgressBarTask.Max := PBeginUpdateMessageParam(AMessage.LParam)^.Max;

  Dispose(PBeginUpdateMessageParam(AMessage.LParam));
  ///

  PanelProgressTask.Visible := True;

  EditRegex.Enabled := False;

  VST.Enabled := False;

  FormMain.Pages.PopupMenu := nil;

  ///
  VST.BeginUpdate();
  FExtForm.BeginUpdate();

  UpdateStatusBar();
end;

procedure TFrameList.OnEndUpdateMessage(var AMessage : TMessage);
var ACanceled : Boolean;
begin
  FTotalExports := PEndUpdateMessageParam(AMessage.LParam)^.TotalExports;
  ACanceled := PEndUpdateMessageParam(AMessage.LParam)^.Canceled;

  Dispose(PEndUpdateMessageParam(AMessage.LParam));
  ///

  if ACanceled then
    FormMain.CloseTab(TTabSheet(Parent) (* TTabSheet *))
  else begin
    ButtonSearch.Visible := FTotalExports > AUTO_SEARCH_DELTA;

    PanelProgressTask.Visible := False;

    FormMain.Pages.PopupMenu := FormMain.PopupTabs;

    EditRegex.Enabled := True;

    VST.EndUpdate();
    FExtForm.EndUpdate();

    VST.Enabled := True;

    VST.FullExpand();

    FEnumTask := nil;

    UpdateStatusBar();

    // Tiny hack to fix a glitch in VirtualStringTree header height
    VST.Header.Height := self.ScaleValue(19);
  end;
end;

procedure TFrameList.DoSearch();
begin
  FilterList();
end;

procedure TFrameList.CopyToClipboard(ASender : TObject);
var pData    : PTreeData;
    pNode    : PVirtualNode;
    AStrings : TStringList;

    function ToPointerString(const AValue : UInt64) : String;
    begin
      result := Format('0x%p', [Pointer(AValue)]);
    end;

begin
  if not (ASender is TMenuItem) then
    Exit();
  ///

  AStrings := TStringList.Create();
  try
    for pNode in VST.Nodes do begin
      if not (vsSelected in pNode.States) then
        continue;
      ///

      pData := pNode.GetData;
      if not Assigned(pData) then
        continue;
      ///

      if (not Assigned(pData^.ExportEntry)) then
        continue;
      ///


      case TMenuItem(ASender).Tag of
        0 : AStrings.Add(pData^.ExportEntry.DisplayName);

        1 : begin
          if pData^.ExportEntry is TPEExportEntry then
            AStrings.Add(ToPointerString(TPEExportEntry(pData^.ExportEntry).Address));
        end;

        2 : begin
          if pData^.ExportEntry is TPEExportEntry then
            AStrings.Add(ToPointerString(TPEExportEntry(pData^.ExportEntry).RelativeAddress));
        end;

        3 : AStrings.Add(IntToStr(pData^.ExportEntry.Ordinal));

        4 : AStrings.Add(pData^.ExportEntry.KindAsString);

        5 : AStrings.Add(pData^.ImagePath);
        else
          // TODO
          if pData^.ExportEntry is TPEExportEntry then
            AStrings.Add(Format('name:%s, address:0x%p, rel_address:0x%p, ordinal:%d, kind:%s, path:"%s"', [
              pData^.ExportEntry.DisplayName,
              Pointer(TPEExportEntry(pData^.ExportEntry).Address),
              Pointer(TPEExportEntry(pData^.ExportEntry).RelativeAddress),
              pData^.ExportEntry.Ordinal,
              pData^.ExportEntry.KindAsString,
              pData^.ImagePath
            ]))
          else if pData^.ExportEntry is TCOMExportEntry then
            AStrings.Add(Format('name:%s, ordinal:%d, kind:%s, path:"%s"', [
              pData^.ExportEntry.DisplayName,
              pData^.ExportEntry.Ordinal,
              pData^.ExportEntry.KindAsString,
              pData^.ImagePath
            ]))
      end;
    end;
  finally
    if AStrings.Count > 0 then
      Clipboard.AsText := AStrings.Text.Trim();
    ///

    if Assigned(AStrings) then
      FreeAndNil(AStrings);
  end;
end;

procedure TFrameList.ExportEntireListToJson1Click(Sender: TObject);
begin
  self.ExportToJson(jemAll);
end;

procedure TFrameList.FilterMenuItemClick(Sender: TObject);
begin
  FilterList();
end;

procedure TFrameList.ExportSelectedItemsToJson1Click(Sender: TObject);
begin
  self.ExportToJson(jemSelected);
end;

procedure TFrameList.ExportToJson(const AMode : TJSONExportMode);
var ADirectory : String;
begin
  if not SelectDirectory('Select destination', '', ADirectory, [sdShowShares]) then
    Exit();
  ///

  FormThreadManager.AddWorkerAndStart(
    TExportExportsToJsonThread.Create(self, ADirectory, AMode)
  );
end;

procedure TFrameList.ExportVisibleFilteredItemsToJson1Click(Sender: TObject);
begin
  self.ExportToJson(jemVisible);
end;

procedure TFrameList.FilterList();
var AFilterKindOptions : TFilterExportKindOptions;
    pNode              : PVirtualNode;
    pData              : PTreeData;

    // Required cause VST.VisibleCount does not seems to work well
    function GetVisibleNodesCount() : UInt64;
    var pNode : PVirtualNode;
    begin
      result := 0;
      ///

      for pNode in VST.Nodes do begin
        if vsVisible in pNode.States then
          Inc(result);
      end;
    end;

    procedure UpdateParentVisibility();
    var pNode      : PVirtualNode;
        pChildNode : PVirtualNode;
        AVisible   : Boolean;
    begin
      if not FGrouped then
        Exit();
      ///

      for pNode in VST.Nodes(False) do begin
        if VST.GetNodeLevel(pNode) <> 0 then
          continue;
        ///

        pChildNode := pNode.FirstChild;
        if not Assigned(pChildNode) then
          Exit();

        repeat
          AVisible := vsVisible in pChildNode.States;

          if AVisible then
            break;

          ///
          pChildNode := pChildNode.NextSibling;
        until pChildNode = nil;

        ///
        if not AVisible then
          Exclude(pNode.States, vsVisible)
        else
          Include(pNode.States, vsVisible);
      end;
    end;

begin
  AFilterKindOptions := [];
  ///

  if ExportFunctions1.Checked then
    Include(AFilterKindOptions, ekExportFunction);

  if ForwardedFunctions1.Checked then
    Include(AFilterKindOptions, ekForwardedFunction);

  if COMMethod1.Checked then
    Include(AFilterKindOptions, ekCOMMethod);

  if COMProperties1.Checked then
    Include(AFilterKindOptions, ekCOMProperty);

  if COMUnknown1.Checked then
    Include(AFilterKindOptions, ekCOMUnknown);
  ///

  VST.BeginUpdate();
  try
    VST.RootNode.TotalHeight := VST.DefaultNodeHeight;
    ///

    for pNode in VST.Nodes do begin
      pData := pNode.GetData;
      if not Assigned(pData) then
        continue;
      ///

      if not Assigned(pData^.ExportEntry) then
        continue;

      // Text Filter
      var AFilteredByRegex : Boolean := False;
      if Length(EditRegex.Text) > 0 then
        AFilteredByRegex := not TRegEx.IsMatch(pData^.ExportEntry.DisplayName, EditRegex.Text);

      // Type Filter
      var AFilteredByType : Boolean := False;
      if AFilterKindOptions <> [] then
        AFilteredByType := not (pData^.ExportEntry.Kind in AFilterKindOptions);

      var ADoFilter := AFilteredByType or AFilteredByRegex;

      if ADoFilter then
        Exclude(pNode.States, vsVisible)
      else
        Include(pNode.States, vsVisible);
    end;
  finally
    VST.EndUpdate();

    UpdateParentVisibility();

    VST.RootNode.TotalHeight := GetVisibleNodesCount() * VST.DefaultNodeHeight;

    VST.UpdateScrollBars(True);

    ///
    UpdateStatusBar();
  end;
end;

procedure TFrameList.LibrariesViewExtendedInfo1Click(Sender: TObject);
begin
  SetupOrRefreshExtendedLibrariesInformation();

  ///
  ShowForm(FExtForm);
end;

procedure TFrameList.LoadSelectedFileinNewTab1Click(Sender: TObject);
var pData : PTreeData;
begin
  if not Assigned(VST.FocusedNode) then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  FormThreadManager.AddWorkerAndStart(TEnumExportsThread.Create(
    pData^.ImagePath
  ));
end;

constructor TFrameList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  FGrouped                           := False;
  FTotalExports                      := 0;
  self.EditRegex.RightButton.Visible := False;
  FDestroyRequested                  := False;
  FExtInfoLoaded                     := False;

  FEnumTask := nil;

  FExtForm := TFormExtendedLibrariesInformation.Create(FormMain, self);

  ButtonSearch.Visible := False;

  ///
  InitializeCopyPopupMenu(VST, self.Copy1, self.CopyToClipboard);
end;

destructor TFrameList.Destroy();
begin
  FDestroyRequested := True;

  UpdateStatusBar();
  ///

  if Assigned(FExtForm) then
    FreeAndNil(FExtForm);

  ///
  inherited Destroy();
end;

procedure TFrameList.ButtonCancelTaskClick(Sender: TObject);
begin
  if Assigned(FEnumTask) then
    FEnumTask.Terminate;
end;

procedure TFrameList.ButtonSearchClick(Sender: TObject);
begin
  DoSearch();
end;

procedure TFrameList.CalculateSelectedLibrariesHashes1Click(Sender: TObject);
var AFiles : TStringList;
begin
  AFiles := TStringList.Create();
  try
    for var pNode in VST.Nodes do begin
      if (vsVisible in pNode.States) and (vsSelected in pNode.States) then begin
        var pData : PTreeData := pNode.GetData;
        ///

        AFiles.Add(pData^.ImagePath);
      end;
    end;

    ///
    FormHashMe.AddFiles(AFiles);
  finally
    if Assigned(AFiles) then
      FreeAndNil(AFiles);
  end;
end;

procedure TFrameList.ClearSelection1Click(Sender: TObject);
begin
  VST.ClearSelection();
end;

procedure TFrameList.CloseTab1Click(Sender: TObject);
begin
  FormMain.CloseActiveTab();
end;

procedure TFrameList.CollapseAll1Click(Sender: TObject);
begin
  VST.FullCollapse(nil);
end;

procedure TFrameList.EditRegexChange(Sender: TObject);
begin
  if FTotalExports <= AUTO_SEARCH_DELTA then
    DoSearch();

  ///
  TButtonedEdit(Sender).RightButton.Visible := Length(TButtonedEdit(Sender).Text) > 0;
end;

procedure TFrameList.EditRegexKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FTotalExports <= AUTO_SEARCH_DELTA then
    Exit();
  ///

  case Key of
    13 : DoSearch();
  end;
end;

procedure TFrameList.EditRegexRightButtonClick(Sender: TObject);
begin
  TButtonedEdit(Sender).Clear();
  ///

  FilterList();
end;

procedure TFrameList.ExpandAll1Click(Sender: TObject);
begin
  VST.FullExpand(nil);
end;

function TFrameList.HaveExportInSelection() : Boolean;
var pNode : PVirtualNode;
    pData : PTreeData;
begin
  result := False;
  ///

  for pNode in VST.Nodes do begin
    if not (vsSelected in pNode.States) then
      continue;
    ///

    pData := pNode.GetData;
    if not Assigned(pData) then
      continue;
    ///

    if Assigned(pData^.ExportEntry) then begin
      result := True;

      // Or Exit(True)

      ///
      break;
    end;
  end;
end;

function TFrameList.HiddenNodeCount() : Cardinal;
var pNode : PVirtualNode;
begin
  result := 0;
  ///

  for pNode in VST.Nodes do begin
    if not (vsVisible in pNode.States) then
      Inc(result);
  end;
end;

function TFrameList.VisibleNodeCount() : Cardinal;
var pNode : PVirtualNode;
begin
  result := 0;
  ///

  for pNode in VST.Nodes do begin
    if vsVisible in pNode.States then
      Inc(result);
  end;
end;

procedure TFrameList.PopupMenuPopup(Sender: TObject);
var ASelectedCount         : Cardinal;
    AHaveExportInSelection : Boolean;
    AHiddenNodeCount       : Cardinal;
    AVisibleNodeCount      : Cardinal;
begin
  ASelectedCount         := VST.SelectedCount;
  AHaveExportInSelection := HaveExportInSelection();
  AHiddenNodeCount       := HiddenNodeCount();
  AVisibleNodeCount      := VisibleNodeCount(); // Could be done differently
  ///

  self.ExpandAll1.Visible                        := FGrouped;
  self.CollapseAll1.Visible                      := FGrouped;
  self.ShowSelectedFileProperties1.Enabled       := ASelectedCount = 1;
  self.ShowSelectedFileOnExplorer1.Enabled       := self.ShowSelectedFileProperties1.Enabled;
  self.LoadSelectedFileinNewTab1.Enabled         := self.ShowSelectedFileProperties1.Enabled;
  self.ClearSelection1.Enabled                   := ASelectedCount >= 1;

  self.Google1.Enabled                           := ASelectedCount = 1;

  self.SearchAPIName1.Enabled                    := (ASelectedCount = 1) and AHaveExportInSelection;
  self.SearchBoth1.Enabled                       := self.SearchAPIName1.Enabled;

  self.Copy1.Enabled                             := AHaveExportInSelection;

  self.ExportSelectedItemsToJson1.Enabled        := ASelectedCount = 1;
  self.ExportVisibleFilteredItemsToJson1.Enabled := (AHiddenNodeCount > 0) and (AVisibleNodeCount > 0);
  self.ExportEntireListToJson1.Enabled           := VST.RootNodeCount > 0;
  self.CalculateSelectedLibrariesHashes1.Enabled := ASelectedCount >= 1;
end;

procedure TFrameList.RenameTab1Click(Sender: TObject);
begin
  FormMain.RenameActiveTab();
end;

procedure TFrameList.ResetClear1Click(Sender: TObject);
begin
  ExportFunctions1.Checked := False;
  ForwardedFunctions1.Checked := False;
  COMMethod1.Checked := False;
  COMProperties1.Checked := False;
  COMUnknown1.Checked := False;

  ///
  self.FilterList();
end;

procedure TFrameList.SearchAPIName1Click(Sender: TObject);
var pData : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  if not Assigned(pData^.ExportEntry) then
    Exit();

  ///
  GoogleSearch(Format('"%s"', [pData^.ExportEntry.DisplayName]));
end;

procedure TFrameList.SearchBoth1Click(Sender: TObject);
var pData : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  if not Assigned(pData^.ExportEntry) then
    Exit();

  ///
  GoogleSearch(Format('"%s"+"%s"', [
    ExtractFileName(pData^.ImagePath),
    ExtractFileName(pData^.ExportEntry.DisplayName)
  ]));
end;

procedure TFrameList.SearchLibraryName1Click(Sender: TObject);
var pData : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  ///
  GoogleSearch(Format('"%s"', [ExtractFileName(pData^.ImagePath)]));
end;

procedure TFrameList.SearchUnprotectSelectedAPIName1Click(Sender: TObject);
var pData : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  if not Assigned(pData^.ExportEntry) then
    Exit();

  ///
  UnprotectSearch(pData^.ExportEntry.DisplayName);
end;

procedure TFrameList.SearchUnprotectSelectedLibraryName1Click(Sender: TObject);
var pData : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  ///
  UnprotectSearch(ExtractFileName(pData^.ImagePath));
end;

procedure TFrameList.SelectAll1Click(Sender: TObject);
begin
  VST.SelectAll(True);
end;

procedure TFrameList.ShowSelectedFileOnExplorer1Click(Sender: TObject);
var pData : PTreeData;
begin
  if not Assigned(VST.FocusedNode) then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  ShowFileOnExplorer(pData^.ImagePath);
end;

procedure TFrameList.ShowSelectedFileProperties1Click(Sender: TObject);
var pData : PTreeData;
begin
  if not Assigned(VST.FocusedNode) then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  FileProperties(pData^.ImagePath);
end;

procedure TFrameList.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var pData     : PTreeData;
    AColorA   : TColor;
    AColorB   : TColor;
    AProgress : TRect;
begin
  pData := Node.GetData;

  AColorA := clNone;
  AColorB := clNone;

  if Assigned(pData^.ExportEntry) then begin
    if pData^.ExportEntry.Kind = ekForwardedFunction then
      AColorA := _COLOR_LIST_BG_ALT
    else
      AColorA := _COLOR_LIST_BG_GRAY;
  end else if FGrouped and (VST.GetNodeLevel(Node) = 0) then begin
    AProgress := CellRect;

    AColorA := _COLOR_GRAD1_BEG;
    AColorB := _COLOR_GRAD1_END;

    if (pData^.ExportCount = 0) or (FTotalExports = 0) then
      AProgress.Width := 0
    else
      AProgress.Width := (AProgress.Width * pData^.ExportCount) div FTotalExports;
  end;

  if (AColorA <> clNone) and (AColorB = clNone) then begin
    // Solid
    TargetCanvas.Brush.Color := AColorA;

    TargetCanvas.FillRect(CellRect);
  end else if (AColorA <> clNone) and (AColorB <> clNone) then begin
    // Grandient
    DrawGradient(
      TargetCanvas,
      AColorA,
      AColorB,
      AProgress,
      False
    );
  end;
end;

procedure TFrameList.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameList.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1            : PTreeData;
    pData2            : PTreeData;
    AAddress1         : UInt64;
    ARelativeAddress1 : UInt64;
    AAddress2         : UInt64;
    ARelativeAddress2 : UInt64;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;

  if Assigned(pData1^.ExportEntry) and
     Assigned(pData2^.ExportEntry) then begin


    AAddress1         := 0;
    ARelativeAddress1 := 0;
    AAddress2         := 0;
    ARelativeAddress2 := 0;

    if pData1^.ExportEntry is TPEExportEntry then begin
      AAddress1         := TPEExportEntry(pData1^.ExportEntry).Address;
      ARelativeAddress1 := TPEExportEntry(pData1^.ExportEntry).RelativeAddress;
    end;

    if pData2^.ExportEntry is TPEExportEntry then begin
      AAddress2         := TPEExportEntry(pData2^.ExportEntry).Address;
      ARelativeAddress2 := TPEExportEntry(pData2^.ExportEntry).RelativeAddress;
    end;

    case Column of
      0 : result := CompareText(pData1^.ExportEntry.DisplayName, pData2^.ExportEntry.DisplayName);

      1 : begin
        result := CompareValue(AAddress1, AAddress2);
      end;

      2 : begin
        result := CompareValue(ARelativeAddress1, ARelativeAddress2);
      end;

      3 : result := CompareValue(pData1^.ExportEntry.Ordinal, pData2^.ExportEntry.Ordinal);
      4 : result := CompareValue(Integer(pData1^.ExportEntry.Kind), Integer(pData2^.ExportEntry.Kind));
      5 : result := CompareText(pData1^.ImagePath, pData2^.ImagePath);
    end;
  end else if
    (not Assigned(pData1^.ExportEntry)) and
    (not ASsigned(pData2^.ExportEntry)) then
      result := CompareText(pData1^.ImagePath, pData2^.ImagePath);

end;

procedure TFrameList.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFrameList.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if Assigned(pData) then begin
    if Assigned(pData^.ExportEntry) then
      FreeAndNil(pData^.ExportEntry);
  end;
end;

procedure TFrameList.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PTreeData;
begin
  pData := Node.GetData;

  ImageIndex := -1;

  if Assigned(pData^.ExportEntry) then begin
    case Kind of
      ikNormal, ikSelected: begin
        case Column of
          0 : begin
            case pData^.ExportEntry.Kind of
              ekExportFunction    : ImageIndex := _ICON_EXPORT;
              ekForwardedFunction : ImageIndex := _ICON_EXPORT_FORWARDED;
              ekCOMMethod         : ImageIndex := _ICON_COM_METHOD;
              ekCOMProperty       : ImageIndex := _ICON_COM_PROPERTY;
              ekCOMUnknown        : ImageIndex := _ICON_COM_UNKNOWN;
              else
                ImageIndex := _ICON_UNKNOWN_EXPORT;
            end;
          end;
        end;
      end;
    end;
  end else begin
    case Kind of
      ikState: begin
        case Column of
          0 : ImageIndex := pData^.StateIndex;
        end;
      end;
    end;
  end;
end;

procedure TFrameList.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFrameList.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;

  CellText := '';

  if Assigned(pData.ExportEntry) then begin
    case Column of
      0 : begin
        if pData^.ExportEntry.Anonymous then
          CellText := Format('Ordinal%d', [pData^.ExportEntry.Ordinal])
        else
          CellText := pData^.ExportEntry.DisplayName;
      end;

      1 : begin
        if pData^.ExportEntry is TPEExportEntry then
          if TPEExportEntry(pData^.ExportEntry).Forwarded then
            CellText := TPEExportEntry(pData^.ExportEntry).ForwardName
          else
            CellText := Format('0x%p', [
              Pointer(TPEExportEntry(pData^.ExportEntry).Address)
            ]);
      end;

      2 : begin
        if pData^.ExportEntry is TPEExportEntry then
          CellText := Format('0x%p', [
            Pointer(TPEExportEntry(pData^.ExportEntry).RelativeAddress)

          ]);
      end;

      3 : CellText := IntToStr(pData^.ExportEntry.Ordinal);
      4 : CellText := pData^.ExportEntry.KindAsString;
      5 : CellText := pData^.ImagePath;
    end;
  end else begin
    case Column of
      0 : CellText := Format('(%d) %s', [
        pData^.ExportCount,
        pData^.ImagePath
      ]);
    end;
  end;
end;

end.
