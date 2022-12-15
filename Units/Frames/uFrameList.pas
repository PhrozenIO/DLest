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

unit uFrameList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees,
  Vcl.ExtCtrls, OMultiPanel, uPortableExecutable, Vcl.Menus;

type
  TTreeData = record
    ImagePath   : String;
    ExportEntry : TExport;
    StateIndex  : Integer;
    ExportCount : UInt64; // I see things in big
  end;
  PTreeData = ^TTreeData;

  TFrameList = class(TFrame)
    MultiPanel: TOMultiPanel;
    VST: TVirtualStringTree;
    PanelDetail: TPanel;
    PopupMenu: TPopupMenu;
    ExpandAll1: TMenuItem;
    CollapseAll1: TMenuItem;
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
  private
    FGrouped      : Boolean;
    FTotalExports : UInt64;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;

    {@G/S}
    property Grouped      : Boolean read FGrouped      write FGrouped;
    property TotalExports : UInt64  read FTotalExports write FTotalExports;
  end;

implementation

uses uFormMain, uConstants, System.Math, uGraphicUtils;

{$R *.dfm}

constructor TFrameList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  FGrouped      := False;
  FTotalExports := 0;
end;

procedure TFrameList.CollapseAll1Click(Sender: TObject);
begin
  VST.FullCollapse(nil);
end;

procedure TFrameList.ExpandAll1Click(Sender: TObject);
begin
  VST.FullExpand(nil);
end;

procedure TFrameList.PopupMenuPopup(Sender: TObject);
begin
  self.ExpandAll1.Visible   := FGrouped;
  self.CollapseAll1.Visible := FGrouped;
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
    if pData^.ExportEntry.Forwarded then
      AColorA := _COLOR_LIST_BG_ALT;
  end else if FGrouped and (VST.GetNodeLevel(Node) = 0) then begin
    AProgress := CellRect;

    AColorA := _COLOR_GRAD1_BEG;
    AColorB := _COLOR_GRAD1_END;

    if pData^.ExportCount = 0 then
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

  CellPaintMode := cpmPaint;
end;

procedure TFrameList.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameList.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1 : PTreeData;
    pData2 : PTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;

  if Assigned(pData1^.ExportEntry) and
     Assigned(pData2^.ExportEntry) then begin
    case Column of
      0 : result := CompareText(pData1^.ExportEntry.Name, pData2^.ExportEntry.Name);
      1 : result := CompareValue(pData1^.ExportEntry.Address, pData2^.ExportEntry.Address);
      2 : result := CompareValue(pData1^.ExportEntry.RelativeAddress, pData2^.ExportEntry.RelativeAddress);
      3 : result := CompareValue(pData1^.ExportEntry.Ordinal, pData2^.ExportEntry.Ordinal);
      4 : result := CompareText(pData1^.ImagePath, pData2^.ImagePath);
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

  if Assigned(pData^.ExportEntry) then begin
    case Kind of
      ikNormal, ikSelected: begin
        case Column of
          0 : begin
            if pData^.ExportEntry.Forwarded then
              ImageIndex := _ICON_EXPORT_FORWARDED
            else
              ImageIndex := _ICON_EXPORT;
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
      0 : CellText := pData^.ExportEntry.Name;

      1 : begin
        if pData^.ExportEntry.Forwarded then
          CellText := pData^.ExportEntry.ForwardName
        else
          CellText := Format('0x%p', [Pointer(pData^.ExportEntry.Address)]);
      end;

      2 : CellText := Format('0x%p', [Pointer(pData^.ExportEntry.RelativeAddress)]);
      3 : CellText := IntToStr(pData^.ExportEntry.Ordinal);
      4 : CellText := pData^.ImagePath;
    end;
  end else begin
    case Column of
      0 : CellText := pData^.ImagePath;
    end;
  end;
end;

end.
