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

unit uFormLogs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.Menus;

type
  TLogLevel = (
    llException,
    llWarning
  );

  TTreeData = record
    DateTime : TDateTime;
    Owner    : String;
    Msg      : String;
    Level    : TLogLevel;
  end;
  PTreeData = ^TTreeData;

  TFormLogs = class(TForm)
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    Clear1: TMenuItem;

    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure Clear1Click(Sender: TObject);
  private
    {@M}
    procedure CreateParams(var Params: TCreateParams); override;
  public
    {@M}
    procedure Log(const AMessage : String; const AOwner : TObject; const ALevel : TLogLevel);
  end;

var
  FormLogs: TFormLogs;

implementation

{$R *.dfm}

uses uFormMain, uConstants;

{$R *.dfm}

procedure TFormLogs.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle and NOT WS_EX_APPWINDOW;

  Params.WndParent := 0;
end;

procedure TFormLogs.Clear1Click(Sender: TObject);
begin
  VST.Clear();
end;

procedure TFormLogs.Log(const AMessage : String; const AOwner : TObject; const ALevel : TLogLevel);
var pNode : PVirtualNode;
    pData : PTreeData;
begin
  VST.BeginUpdate();
  try
    pNode := VST.AddChild(nil);
    pData := pNode.GetData;
    ///

    pData^.DateTime := Now();
    pData^.Msg      := AMessage;
    pData^.Owner    := AOwner.ToString;
    pData^.Level    := ALevel;
  finally
    VST.EndUpdate();
  end;

  VST.TopNode := VST.GetLast();
end;

procedure TFormLogs.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var pData  : PTreeData;
    AColor : TColor;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  AColor := clNone;
  case pData^.Level of
    llException : AColor := _COLOR_LIGHT_RED;
  end;

  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormLogs.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFormLogs.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFormLogs.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PTreeData;
begin
  if Column <> 0 then
    Exit();
  ///

  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();

  case Kind of
    ikSelected, ikNormal: begin
      case pData^.Level of
        llException : ImageIndex := _STATE_IMAGE_EXCEPTION;
        llWarning   : ImageIndex := _STATE_IMAGE_WARNING;
      end;
    end;
  end;
end;

procedure TFormLogs.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormLogs.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  case Column of
    0 : CellText := DateTimeToStr(pData^.DateTime);
    1 : CellText := pData^.Owner;
    2 : CellText := pData^.Msg;
  end;
end;

end.
