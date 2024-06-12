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

unit uFormExtendedLibrariesInformation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.Menus, VCL.ImgList,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree, VirtualTrees.AncestorVCL,
  VirtualTrees.Types, Generics.Collections;

type
  TTreeData = record
    Directory     : String;
    DirectoryPath : String;
    DirectoryNode : Boolean;
    ImagePath     : String;
    ExportsCount  : UInt64;
    FileSize      : UInt64;
    CompanyName   : String;
    FileVersion   : String;
    ImageIndex    : Integer;
  end;
  PTreeData = ^TTreeData;

  (* TFormExtendedLibrariesInformation *)

  TFormExtendedLibrariesInformation = class(TForm)
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    Copy1: TMenuItem;
    SelectAll1: TMenuItem;
    ClearSelection1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    SelectedFileProperties1: TMenuItem;
    ShowSelectedFileOnExplorer1: TMenuItem;
    N3: TMenuItem;
    OpenSelectedFilesonanewTab1: TMenuItem;
    N4: TMenuItem;
    ShowListasTree1: TMenuItem;
    CalculateSelectedImageFileHashes1: TMenuItem;
    N5: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure SelectAll1Click(Sender: TObject);
    procedure ClearSelection1Click(Sender: TObject);
    procedure SelectedFileProperties1Click(Sender: TObject);
    procedure ShowSelectedFileOnExplorer1Click(Sender: TObject);
    procedure OpenSelectedFilesonanewTab1Click(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowListasTree1Click(Sender: TObject);
    procedure CalculateSelectedImageFileHashes1Click(Sender: TObject);
  private
    FTotalFilesSize : UInt64;
    FTotalExports   : UInt64;
    FOwnerFrame     : TFrame;

    {@M}
    procedure CopyToClipboard(ASender : TObject);
    procedure IncTotalFilesSize(const AValue : UInt64);
    procedure IncTotalExports(const AValue : UInt64);
  protected
    {@M}
    procedure CreateParams(var Params: TCreateParams); override;
  public
    {@M}
    procedure BeginUpdate();
    procedure EndUpdate();
    procedure RegisterImages(var AImageList : TDictionary<String, UInt64>);

    {@C}
    constructor Create(AOwner : TComponent; const AOwnerFrame : TFrame); overload;
  end;

var
  FormExtendedLibrariesInformation: TFormExtendedLibrariesInformation;

implementation

uses uFormMain, uFunctions, uConstants, uGraphicUtils, System.Math,
     uVirtualStringTreeUtils, VCL.Clipbrd, uFormThreadManager, uEnumExportsThread,
     System.IOUtils, uFrameList, uFormHashMe;

{$R *.dfm}

procedure TFormExtendedLibrariesInformation.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle and NOT WS_EX_APPWINDOW;

  Params.WndParent := 0;
end;

procedure TFormExtendedLibrariesInformation.RegisterImages(var AImageList : TDictionary<String, UInt64>);

  function GetOrCreateDirectoryNode(const ADirectory : String; const pParent : PVirtualNode) : PVirtualNode;
  begin
    result := nil;
    ///

    for var pNode in VST.Nodes do begin
      if (pNode.Parent <> pParent) and (pParent <> nil) then
        continue;
      ///

      var pData : PTreeData := pNode.GetData;

      if String.Compare(pData^.Directory, ADirectory, True) = 0 then
        Exit(pNode);
    end;

    ///
    result := VST.AddChild(pParent);
    var pData : PTreeData := result.GetData;

    pData^.Directory := ADirectory;

    if Assigned(pParent) then begin
      var pParentData : PTreeData := pParent.GetData();

      pData^.DirectoryPath := TPath.Combine(
        pParentData^.DirectoryPath,
        pData^.Directory
      );
    end else
      pData^.DirectoryPath := IncludeTrailingPathDelimiter(pData^.Directory);

    pData^.DirectoryNode := True;
    pData^.ImageIndex := SystemFolderIcon(pData^.DirectoryPath);
  end;

  function SetupPathTree(const APath : String) : PVirtualNode;
  var APathList : TStringList;
  begin
    APathList := GetDirectoryList(APath);

    var pParent : PVirtualNode := nil;

    for var ADirectory in APathList do
      pParent := GetOrCreateDirectoryNode(ADirectory, pParent);

    ///
    result := pParent;
  end;

begin
  VST.Clear();
  ///

  VST.BeginUpdate();
  try
    for var AImage in AImageList.Keys do begin
      var AExportCount : UInt64;
      if not AImageList.TryGetValue(AImage, AExportCount) then
        continue;
      ///

      var pParent : PVirtualNode := nil;

      if self.ShowListasTree1.Checked then
        pParent := SetupPathTree(ExtractFilePath(AImage));

      // Add Image File to it's parent tree folder
      var pNode := VST.AddChild(pParent);
      var pData : PTreeData := pNode.GetData;

      pData^.DirectoryNode := False;
      pData^.Directory     := '';
      pData^.DirectoryPath := '';
      pData^.ImagePath     := AImage;
      pData^.ExportsCount  := AExportCount;
      pData^.FileSize      := GetFileSize(AImage);
      pData^.CompanyName   := GetApplicationCompany(AImage);
      pData^.FileVersion   := GetApplicationVersion(AImage);
      pData^.ImageIndex    := SystemFileIcon(AImage);

      // Update Metrics
      IncTotalFilesSize(pData^.FileSize);
      IncTotalExports(AExportCount);
    end;
  finally
    if Assigned(AImageList) then
      FreeAndNil(AImageList);

    VST.FullExpand();

    ///
    VST.EndUpdate();
  end;
end;

procedure TFormExtendedLibrariesInformation.IncTotalFilesSize(const AValue : UInt64);
begin
  Inc(FTotalFilesSize, AValue);
end;

procedure TFormExtendedLibrariesInformation.OpenSelectedFilesonanewTab1Click(
  Sender: TObject);
var pData  : PTreeData;
    pNode  : PVirtualNode;
    AFiles : TStringList;
begin
  if VST.SelectedCount = 0 then
    Exit();
  ///

  AFiles := TStringList.Create();
  try
    for pNode in VST.Nodes do begin
      if not (vsSelected in pNode.States) then
        continue;
      ///

      pData := pNode.GetData;
      if not Assigned(pData) then
        continue;
      ///

      AFiles.Add(pData^.ImagePath);
    end;

    ///
    if AFiles.Count > 1 then
      FormThreadManager.AddWorkerAndStart(TEnumExportsThread.Create(
        AFiles,
        'Group',
        SystemFolderIcon()
      ))
    else if AFiles.Count = 1 then
      FormThreadManager.AddWorkerAndStart(TEnumExportsThread.Create(
        AFiles.Strings[0]
      ));
  finally
    if Assigned(AFiles) then
      FreeAndNil(AFiles);
  end;
end;

procedure TFormExtendedLibrariesInformation.PopupMenuPopup(Sender: TObject);
var ASelectedCount : UInt64;
begin
  ASelectedCount := VST.SelectedCount;
  ///

  SelectedFileProperties1.Enabled           := ASelectedCount = 1;
  ShowSelectedFileOnExplorer1.Enabled       := ASelectedCount = 1;
  OpenSelectedFilesonanewTab1.Enabled       := ASelectedCount > 0;
  ClearSelection1.Enabled                   := ASelectedCount > 0;
  Copy1.Visible                             := ASelectedCount > 0;
  CalculateSelectedImageFileHashes1.Enabled := ASelectedCount > 0;
end;

procedure TFormExtendedLibrariesInformation.SelectAll1Click(Sender: TObject);
begin
  VST.SelectAll(True);
end;

procedure TFormExtendedLibrariesInformation.SelectedFileProperties1Click(
  Sender: TObject);
var pData : PTreeData;
begin
  if not Assigned(VST.FocusedNode) then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  FileProperties(pData^.ImagePath);
end;

procedure TFormExtendedLibrariesInformation.ShowListasTree1Click(
  Sender: TObject);
begin
  if Assigned(FOwnerFrame) then
    TFrameList(FOwnerFrame).SetupOrRefreshExtendedLibrariesInformation(True);
end;

procedure TFormExtendedLibrariesInformation.ShowSelectedFileOnExplorer1Click(
  Sender: TObject);
var pData : PTreeData;
begin
  if not Assigned(VST.FocusedNode) then
    Exit();
  ///

  pData := VST.FocusedNode.GetData;

  ShowFileOnExplorer(pData^.ImagePath);
end;

procedure TFormExtendedLibrariesInformation.IncTotalExports(const AValue : UInt64);
begin
  Inc(FTotalExports, AValue);
end;

procedure TFormExtendedLibrariesInformation.CalculateSelectedImageFileHashes1Click(
  Sender: TObject);
var AFiles : TStringList;
begin
  AFiles := TStringList.Create();
  try
    for var pNode in VST.Nodes do begin
      if (vsSelected in pNode.States) then begin
        var pData : PTreeData := pNode.GetData;
        ///

        if pData^.DirectoryNode then
          continue;

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

procedure TFormExtendedLibrariesInformation.ClearSelection1Click(
  Sender: TObject);
begin
  VST.ClearSelection;
end;

procedure TFormExtendedLibrariesInformation.CopyToClipboard(ASender : TObject);
var pData    : PTreeData;
    pNode    : PVirtualNode;
    AStrings : TStringList;
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

      if pData^.DirectoryNode then
        continue;

      case TMenuItem(ASender).Tag of
        0 : AStrings.Add(ExtractFileName(pData^.ImagePath));
        1 : AStrings.Add(IntToStr(pData^.ExportsCount));
        2 : AStrings.Add(FormatSize(pData^.FileSize));
        3 : AStrings.Add(pData^.CompanyName);
        4 : AStrings.Add(pData^.FileVersion);
        5 : AStrings.Add(pData^.ImagePath);
        else
          AStrings.Add(Format('filename:%s, exports_count:%d, file_size:%s, company:%s, version:%s, file:"%s"', [
            ExtractFileName(pData^.ImagePath),
            pData^.ExportsCount,
            FormatSize(pData^.FileSize),
            pData^.CompanyName,
            pData^.FileVersion,
            pData^.ImagePath
          ]));
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

constructor TFormExtendedLibrariesInformation.Create(AOwner : TComponent; const AOwnerFrame : TFrame);
begin
  inherited Create(AOwner);
  ///

  FOwnerFrame := AOwnerFrame;

  FTotalFilesSize := 0;
  FTotalExports   := 0;

  ///
  InitializeCopyPopupMenu(VST, Copy1, CopyToClipboard);
end;

procedure TFormExtendedLibrariesInformation.BeginUpdate();
begin
  VST.BeginUpdate();
end;

procedure TFormExtendedLibrariesInformation.EndUpdate();
begin
  VST.EndUpdate();
end;

procedure TFormExtendedLibrariesInformation.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    27 : self.Close();
  end;
end;

procedure TFormExtendedLibrariesInformation.VSTBeforeCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var pData     : PTreeData;
    AProgress : TRect;
    AColorA   : TColor;
    AColorB   : TColor;
begin
  pData := Node.GetData;
  ///

  if ((column <> 1) and (column <> 2)) or (pData^.DirectoryNode)  then
    Exit();
  ///

  AColorA := clNone;
  AColorB := clNone;

  AProgress := CellRect;

  case column of
    1 : begin
      if (FTotalExports = 0) or (pData^.ExportsCount = 0) then
        Exit();
      ///

      AColorA := _COLOR_GRAD2_BEG;
      AColorB := _COLOR_GRAD2_END;

      ///
      AProgress.Width := (AProgress.Width * pData^.ExportsCount) div FTotalExports;
    end;

    2 : begin
      if (FTotalFilesSize = 0) or (pData^.FileSize = 0) then
        Exit();
      ///

      AColorA := _COLOR_GRAD1_BEG;
      AColorB := _COLOR_GRAD1_END;

      ///
      AProgress.Width := (AProgress.Width * pData^.FileSize) div FTotalFilesSize;
    end;
  end;


  if (AColorA <> clNone) and (AColorB <> clNone) then
    DrawGradient(
      TargetCanvas,
      AColorA,
      AColorB,
      AProgress,
      False
    );
end;

procedure TFormExtendedLibrariesInformation.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFormExtendedLibrariesInformation.VSTCompareNodes(
  Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var pData1 : PTreeData;
    pData2 : PTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Exit();

  if (pData1^.DirectoryNode) and (pData2^.DirectoryNode) then begin
    // TODO
  end else if (not pData1^.DirectoryNode) and (not pData2^.DirectoryNode) then begin
    case column of
      0 : result := CompareText(
        ExtractFileName(pData1^.ImagePath),
        ExtractFileName(pData2^.ImagePath)
      );
      1 : result := CompareValue(pData1^.ExportsCount, pData2^.ExportsCount);
      2 : result := CompareValue(pData1^.FileSize, pData2^.FileSize);
      3 : result := CompareText(pData1^.CompanyName, pData2^.CompanyName);
      4 : result := CompareText(pData1^.FileVersion, pData2^.FileVersion);
      5 : result := CompareText(pData1^.ImagePath, pData2^.ImagePath);
    end;
  end;
end;

procedure TFormExtendedLibrariesInformation.VSTFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFormExtendedLibrariesInformation.VSTGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PTreeData;
begin
  if column <> 0 then
    Exit();
  ///

  pData := Node.GetData;

  // TODO

  case Kind of
    ikState : ImageIndex := pData^.ImageIndex;
  end;
end;

procedure TFormExtendedLibrariesInformation.VSTGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormExtendedLibrariesInformation.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PTreeData;
begin
  CellText := '';
  ///

  pData := Node.GetData;

  if (pData^.DirectoryNode) and (Column = 0) then begin
    CellText := pData^.Directory;
  end else if (not pData^.DirectoryNode) then begin
    CellText := '-';
    ///

    case column of
      0 : CellText := ExtractFileName(pData^.ImagePath);
      1 : CellText := IntToStr(pData^.ExportsCount);
      2 : CellText := FormatSize(pData^.FileSize);
      3 : CellText := pData^.CompanyName;
      4 : CellText := pData^.FileVersion;
      5 : CellText := pData^.ImagePath;
    end;
  end;
end;

end.
