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

{TODO -oDarkCoderSc -cGeneral : This feature is not yet very optimized, re-think the whole thing and make it more smooth }

unit uFormHashMe;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree, VirtualTrees.AncestorVCL, VirtualTrees, Vcl.Menus,
  Vcl.ExtCtrls, Generics.Collections, System.Threading, uWorkerThread,
  System.SyncObjs, Vcl.ComCtrls, uConstants;

type
  TProcessedKind = (
    pkCalculated,
    pkDuplicated
  );

  TTreeData = record
    FilePath        : String;
    MD5             : String;
    SHA1            : String;
    SHA224          : String;
    SHA256          : String;
    SHA384          : String;
    SHA512          : String;

    Color           : TColor;

    Calculated      : Boolean;

    StateImageIndex : Cardinal;
  end;
  PTreeData = ^TTreeData;

  (* THashThread *)
  THashThread = class(TWorkerThread)
  private
    FNodesData     : TThreadList<PTreeData>;
    FIntervalEvent : TEvent;
  protected
    {@M}
    procedure ThreadExecute(); override;

    procedure TerminatedSet(); override;
  public
    {@C}
    constructor Create(); override;
    destructor Destroy(); override;

    {@M}
    procedure AddNodeData(const pData : PTreeData);
  end;

  (* TFormHashMe *)
  TFormHashMe = class(TForm)
    VST: TVirtualStringTree;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    AddFiles1: TMenuItem;
    OpenDialog: TOpenDialog;
    Clear1: TMenuItem;
    N1: TMenuItem;
    StatusBar: TStatusBar;
    PopupMenu: TPopupMenu;
    Copy1: TMenuItem;
    procedure VSTFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVstTextType; var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure FormCreate(Sender: TObject);
    procedure AddFiles1Click(Sender: TObject);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure Clear1Click(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  private
    FHashThread : THashThread;
    FFirstFile  : Boolean;

    FCalculated : UInt64;
    FDuplicated : UInt64;

    {@M}
    procedure WMDropFiles(var AMessage: TMessage); message WM_DROPFILES;
    function FileListed(const AFilePath : String) : Boolean;
    //function GetDuplicates(const ASignature : String) : TList<PVirtualNode>;
    procedure Reset(const AStartNew : Boolean = False);
    procedure OnItemProcessedMessage(var AMessage : TMessage); message WM_MESSAGE_ITEM_PROCESSED;
    procedure UpdateStatusBar();
    procedure CopyToClipboard(ASender : TObject);
  protected
    {@M}
    procedure CreateParams(var Params: TCreateParams); override;
  public
    {@M}
    procedure AddFile(AFilePath : String);
    procedure AddFiles(AFiles : TStringList);

    {@C}
    destructor Destroy(); override;
  end;

var
  FormHashMe: TFormHashMe;

implementation

uses uFormMain, System.Hash, Winapi.ShellAPI, uFunctions, uFormThreadManager,
     System.Math, VirtualTrees.Types, VCL.Clipbrd, uVirtualStringTreeUtils;

{$R *.dfm}

(* THashThread *)

procedure THashThread.ThreadExecute();
var AList       : TList<PTreeData>;
    ACopiedList : TList<PTreeData>;

    function GenerateRandomColor() : TColor;
    begin
      var r : Byte := 210 + Random(46);
      var g : Byte := 210 + Random(46);
      var b : Byte := 210 + Random(46);

      ///
      result := RGB(
        r, g, b
      );
    end;

begin
  ACopiedList := TList<PTreeData>.Create();
  try
    while not Terminated do begin
      try
        // Quick copy while locked
        AList := FNodesData.LockList();
        try
          for var pData in AList do begin
            if Terminated then
              break;

            ///
            ACopiedList.Add(pData);
          end;
        finally
          FNodesData.UnlockList();
        end;

        // Calculate hashes (node data)
        for var pData in ACopiedList do begin
          if Terminated then
            break;
          ///

          if pData^.Calculated then
            continue;

          var AFilePath := pData^.FilePath;
          ///

          pData^.MD5    := System.Hash.THashMD5.GetHashStringFromFile(AFilePath);
          pData^.SHA1   := System.Hash.THashSHA1.GetHashStringFromFile(AFilePath);
          pData^.SHA224 := System.Hash.THashSHA2.GetHashStringFromFile(AFilePath, SHA224);
          pData^.SHA256 := System.Hash.THashSHA2.GetHashStringFromFile(AFilePath, SHA256);
          pData^.SHA384 := System.Hash.THashSHA2.GetHashStringFromFile(AFilePath, SHA384);
          pData^.SHA512 := System.Hash.THashSHA2.GetHashStringFromFile(AFilePath, SHA512);

          ///
          pData^.Calculated := True;

          ///
          PostMessage(FormHashMe.Handle, WM_MESSAGE_ITEM_PROCESSED, Integer(pkCalculated), 0);

          ///
          Sleep(10);
        end;

        // Check for duplicates
        for var pData in ACopiedList do begin
          for var pData2 in ACopiedList do begin
            if String.Compare(pData^.FilePath, pData2^.FilePath, True) = 0 then
              continue;
            ///

            if (String.Compare(pData^.SHA512, pData2^.SHA512) = 0) then begin
              var AColor : TColor := clNone;
              ///

              if (pData^.Color = clNone) and (pData2^.Color = clNone) then
                AColor := GenerateRandomColor()
              else if (pData^.Color = pData2^.Color) then
                continue // Already done !
              else if pData^.Color <> clNone then
                AColor := pData^.Color
              else if pData2^.Color <> clNone then
                AColor := pData^.Color;

              ///
              pData^.Color := AColor;
              pData2.Color := AColor;

              ///
              PostMessage(FormHashMe.Handle, WM_MESSAGE_ITEM_PROCESSED, Integer(pkDuplicated), 0);

              ///
              Sleep(10);
            end;
          end;
        end;
      finally
        ACopiedList.Clear();

        ///
        FIntervalEvent.WaitFor(1000);
      end;
    end;
  finally
    if Assigned(ACopiedList) then
      FreeAndNil(ACopiedList);
  end;
end;

procedure THashThread.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if Assigned(FIntervalEvent) then
    FIntervalEvent.SetEvent();
end;

constructor THashThread.Create();
begin
  inherited Create();
  ///

  FNodesData := TThreadList<PTreeData>.Create();
  FIntervalEvent := TEvent.Create(nil, True, False, TGUID.NewGuid.ToString());
end;

procedure THashThread.AddNodeData(const pData : PTreeData);
begin
  if not Assigned(pData) then
    Exit();
  ///

  if Assigned(FNodesData) then
    FNodesData.Add(pData);
end;

destructor THashThread.Destroy();
begin
  if Assigned(FIntervalEvent) then
    FreeAndNil(FIntervalEvent);

  if Assigned(FNodesData) then
    FreeAndNil(FNodesData);

  ///
  inherited Destroy();
end;

(* TFormHashMe *)

procedure TFormHashMe.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle and NOT WS_EX_APPWINDOW;

  Params.WndParent := 0;
end;

procedure TFormHashMe.UpdateStatusBar();
var ATotal : UInt64;
begin
  ATotal := VST.RootNodeCount;
  ///

  StatusBar.Panels[0].Text := Format('Total: %d', [ATotal]);
  StatusBar.Panels[1].Text := Format('Processing: %d', [ATotal - FCalculated]);
  StatusBar.Panels[2].Text := Format('Done: %d', [FCalculated]);
  StatusBar.Panels[3].Text := Format('Duplicates: %d', [FDuplicated]);
  StatusBar.Panels[4].Text := Format('', []);
end;

procedure TFormHashMe.OnItemProcessedMessage(var AMessage : TMessage);
begin
  VST.Refresh();

  ///
  case TProcessedKind(AMessage.WParam) of
    pkCalculated : Inc(FCalculated);
    pkDuplicated : Inc(FDuplicated);
  end;

  ///
  self.UpdateStatusBar();
end;

destructor TFormHashMe.Destroy();
begin
  if Assigned(FHashThread) then begin
    FHashThread.Terminate();

    FHashThread.WaitFor();

    FHashThread := nil;
  end;

  ///
  inherited Destroy();
end;

procedure TFormHashMe.Reset(const AStartNew : Boolean = False);
begin
  if Assigned(FHashThread) then begin
    FHashThread.Terminate;

    FHashThread.WaitFor();
  end;

  VST.Clear();

  FCalculated := 0;
  FDuplicated := 0;

  FHashThread := nil;

  if AStartNew then begin
    FHashThread := THashThread.Create();

    FormThreadManager.AddWorkerAndStart(FHashThread);
  end;

  ///
  UpdateStatusBar();
end;

(*
function TFormHashMe.GetDuplicates(const ASignature : String) : TList<PVirtualNode>;
begin
  result := nil;
  ///

  for var pNode in VST.Nodes do begin
    var pData : PTreeData := pNode.GetData;
    if not Assigned(pData) then
      continue;
    ///

    if String.Compare(pData^.SHA512, ASignature) = 0 then begin
      if result = nil then
        result := TList<PVirtualNode>.Create();
      ///

      result.Add(pNode);
    end;
  end;
end;
*)

procedure TFormHashMe.WMDropFiles(var AMessage: TMessage);
var i      : Integer;
    ACount : Integer;
    ALen   : Integer;
    AFile  : String;
    AFiles : TStringList;
begin
  try
    ACount := DragQueryFile(AMessage.WParam, $FFFFFFFF, nil, 0);
    ///

    AFiles := TStringList.Create();
    try
      for i := 0 to ACount -1 do begin
        ALen := DragQueryFile(AMessage.WParam, I, nil, 0) +1;

        SetLength(AFile, ALen -1);

        DragQueryFile(AMessage.WParam, I, PWideChar(AFile), ALen);

        ///
        AFiles.Add(AFile);
      end;

      if AFiles.Count > 0 then
        AddFiles(AFiles);
    finally
      FreeAndNil(AFiles);
    end;
  finally
    DragFinish(AMessage.WParam);
  end;
end;

procedure TFormHashMe.AddFiles(AFiles : TStringList);
begin
  if not Assigned(AFiles) then
    Exit();
  ///

  RemoveDuplicates(AFiles);

  for var AFile in AFiles do
    AddFile(AFile);
end;

procedure TFormHashMe.AddFiles1Click(Sender: TObject);
begin
  if not self.OpenDialog.Execute() then
    Exit();
  ///

  var AFiles := TStringList.Create();
  try
    AFiles.Assign(OpenDialog.Files);

    ///
    AddFiles(AFiles);
  finally
    if Assigned(AFiles) then
    FreeAndNil(AFiles);
  end;
end;

procedure TFormHashMe.Clear1Click(Sender: TObject);
begin
  Reset(True);
end;

procedure TFormHashMe.CopyToClipboard(ASender : TObject);
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

      case TMenuItem(ASender).Tag of
        0 : AStrings.Add(ExtractFileName(pData^.FilePath));
        1 : AStrings.Add(pData^.MD5);
        2 : AStrings.Add(pData^.SHA1);
        3 : AStrings.Add(pData^.SHA224);
        4 : AStrings.Add(pData^.SHA256);
        5 : AStrings.Add(pData^.SHA384);
        6 : AStrings.Add(pData^.SHA512);
        7 : AStrings.Add(Ternary(pData^.Color <> clNone, 'Yes', 'No'));
        8 : AStrings.Add(pData^.FilePath);
        else
          AStrings.Add(Format('name:%s, md5:%s, sha1:%s, sha224:%s, sha256:%s, sha384:%s, sha512:%s, duplicate:%s, path:"%s"', [
            ExtractFileName(pData^.FilePath),
            pData^.MD5,
            pData^.SHA1,
            pData^.SHA224,
            pData^.SHA256,
            pData^.SHA384,
            pData^.SHA512,
            Ternary(pData^.Color <> clNone, 'Yes', 'No'),
            pData^.FilePath
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

procedure TFormHashMe.FormCreate(Sender: TObject);
begin
  if Assigned(ChangeWindowMessageFilterEx) then
    ChangeWindowMessageFilterEx(self.Handle, WM_DROPFILES, MSGFLT_ALLOW, nil);

  ///
  DragAcceptFiles(self.Handle, true);

  UpdateStatusBar();

  InitializeCopyPopupMenu(VST, self.Copy1, self.CopyToClipboard);

  ///
  FFirstFile := True;
end;

function TFormHashMe.FileListed(const AFilePath : String) : Boolean;
begin
  result := False;
  ///

  for var pNode in VST.Nodes do begin
    var pData : PTreeData := pNode.GetData;
    if not Assigned(pData) then
      continue;
    ///

    if String.Compare(pData^.FilePath, AFilePath, True) = 0 then begin
      result := True;

      ///
      break;
    end;
  end;
end;

procedure TFormHashMe.AddFile(AFilePath : String);
var pNode : PVirtualNode;
    pData : PTreeData;

const PLACEHOLDER = '... Processing ...';
begin
  if FFirstFile then begin
    FFirstFile := False;

    Reset(True);
  end;
  ///

  AFilePath := GetShortcutTarget(AFilePath);
  ///

  if not FileExists(AFilePath) then
    Exit();
  ///

  if FileListed(AFilePath) then
    Exit();
  ///

  pNode := VST.AddChild(nil);
  pData := pNode.GetData;
  ///

  pData^.FilePath        := AFilePath;
  pData^.MD5             := PLACEHOLDER;
  pData^.SHA1            := PLACEHOLDER;
  pData^.SHA224          := PLACEHOLDER;
  pData^.SHA256          := PLACEHOLDER;
  pData^.SHA384          := PLACEHOLDER;
  pData^.SHA512          := PLACEHOLDER;
  pData^.Color           := clNone;
  pData^.StateImageIndex := SystemFileIcon(AFilePath);
  pData^.Calculated      := False;

  ///
  if Assigned(FHashThread) then
    FHashThread.AddNodeData(pData);

  ///
  UpdateStatusBar();

  ///
  if not Visible then
    Show();
end;

procedure TFormHashMe.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if pData^.Color = clNone then
    Exit();
  ///

  TargetCanvas.Brush.Color := pData^.Color;
  TargetCanvas.FillRect(CellRect);
end;

procedure TFormHashMe.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormHashMe.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1, pData2 : PTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  case Column of
    0 : Result := CompareText(ExtractFileName(pData1^.FilePath), ExtractFileName(pData2^.FilePath));
    1 : Result := CompareText(pData1^.MD5, pData2^.MD5);
    2 : Result := CompareText(pData1^.SHA1, pData2^.SHA1);
    3 : Result := CompareText(pData1^.SHA224, pData2^.SHA224);
    4 : Result := CompareText(pData1^.SHA256, pData2^.SHA256);
    5 : Result := CompareText(pData1^.SHA384, pData2^.SHA384);
    6 : Result := CompareText(pData1^.SHA512, pData2^.SHA512);
    7 : Result := CompareValue(Integer(pData1^.Color <> clNone), Integer(pData2^.Color <> clNone));
    8 : Result := CompareText(pData1^.FilePath, pData2^.FilePath);
  end;
end;

procedure TFormHashMe.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  TVirtualStringTree(Sender).Refresh();
end;

procedure TFormHashMe.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
  var pData : PTreeData;
begin
  pData := Node.GetData;

  case Kind of
    TVTImageKind.ikState : begin
      if Column = 0 then
        ImageIndex := pData^.StateImageIndex;
    end;
  end;
end;

procedure TFormHashMe.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormHashMe.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVstTextType;
  var CellText: string);
var pData : PTreeData;
begin
  CellText := '';
  ///

  pData := Node.GetData;

  case Column of
    0 : CellText := ExtractFileName(pData^.FilePath);
    1 : CellText := pData^.MD5;
    2 : CellText := pData^.SHA1;
    3 : CellText := pData^.SHA224;
    4 : CellText := pData^.SHA256;
    5 : CellText := pData^.SHA384;
    6 : CellText := pData^.SHA512;
    7 : CellText := Ternary(pData^.Color <> clNone, 'Yes', 'No');
    8 : CellText := pData^.FilePath;
  end;
end;

end.
