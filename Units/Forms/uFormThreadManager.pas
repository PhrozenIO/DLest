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

unit uFormThreadManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, uWorkerThread,
  System.SyncObjs, System.Generics.Collections;

type
  TTreeData = record
    StartDateTime : TDateTime;
    ThreadId      : Cardinal;
    ThreadHandle  : THandle;
    ClassName     : String;
    Thread        : TWorkerThread;
  end;
  PTreeData = ^TTreeData;

  TFormThreadManager = class;

  TThreadWatcher = class(TThread)
  strict private
    FIntervalEvent : TEvent;
    FWorkers       : TThreadList<TWorkerThread>;
    FView          : TFormThreadManager;
  protected
    {@M}
    procedure Execute(); override;
    procedure TerminatedSet(); override;
  public
    {@C}
    constructor Create(const AWorkers : TThreadList<TWorkerThread>; const AView : TFormThreadManager); overload;
    destructor Destroy(); override;
  end;

  TFormThreadManager = class(TForm)
    VST: TVirtualStringTree;
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
  private
    FThreadWatcher : TThreadWatcher;
    FWorkers       : TThreadList<TWorkerThread>;
  public
    {@M}
    procedure AddWorker(const AThread : TWorkerThread);
    procedure AddWorkerAndStart(const AThread : TWorkerThread);
    procedure RemoveNode(const AWorker : TWorkerThread);

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

var
  FormThreadManager: TFormThreadManager;

implementation

uses uFormMain, uConstants, System.Math, System.DateUtils;

{$R *.dfm}

(* TThreadWatcher *)

constructor TThreadWatcher.Create(const AWorkers : TThreadList<TWorkerThread>; const AView : TFormThreadManager);
begin
  inherited Create(False);
  ///

  FWorkers       := AWorkers;
  FView          := AView;
  FIntervalEvent := TEvent.Create(nil, True, False, TGUID.NewGuid.ToString());

  self.FreeOnTerminate := False;
  self.Priority        := tpLowest;
end;

procedure TThreadWatcher.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if Assigned(FIntervalEvent) then
    FIntervalEvent.SetEvent();
end;


destructor TThreadWatcher.Destroy();
begin
  self.Terminate;

  self.WaitFor();

  if Assigned(FIntervalEvent) then
    FreeAndNil(FIntervalEvent);

  ///
  inherited Destroy();
end;

procedure TThreadWatcher.Execute();
var AList     : TList<TWorkerThread>;
    AWorker   : TWorkerThread;
    AToRemove : TList<TWorkerThread>;
begin
  try
    if not Assigned(FWorkers) or not Assigned(FView) then
      Exit();
    ///

    AToRemove := TList<TWorkerThread>.Create();
    try
      while not Terminated do begin
        AToRemove.Clear();
        ///

        AList := FWorkers.LockList;
        try
          // Enumerate Dead Threads / Finalize if Dead
          for AWorker in AList do begin
            if AWorker.IsTerminated then begin
              AWorker.Free;

              ///
              AToRemove.Add(AWorker);
            end;
          end;

          // Remove Dead Threads
          for AWorker in AToRemove do begin
            Synchronize(procedure begin
              FView.RemoveNode(AWorker);
            end);

            ///
            AList.Remove(AWorker);
          end;
        finally
          FWorkers.UnlockList;
        end;

        ///
        FIntervalEvent.WaitFor(1000);
      end;
    finally
      if Assigned(AToRemove) then
        FreeAndNil(AToRemove);
    end;
  finally
    ExitThread(0);
  end;
end;

(* TFormThreadManager *)

procedure TFormThreadManager.RemoveNode(const AWorker : TWorkerThread);
var pNode : PVirtualNode;
    pData : PTreeData;
begin
  for pNode in VST.Nodes do begin
    pData := pNode.GetData;
    if not Assigned(pData) then
      continue;
    ///

    if pData^.Thread = AWorker then begin
      VST.DeleteNode(pNode);

      break;
    end;
  end;
end;

constructor TFormThreadManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  FWorkers := TThreadList<TWorkerThread>.Create();

  FThreadWatcher := TThreadWatcher.Create(FWorkers, self);
end;

destructor TFormThreadManager.Destroy();
var AWorker : TWorkerThread;
    AList   : TList<TWorkerThread>;
begin
  if Assigned(FThreadWatcher) then
    FreeAndNil(FThreadWatcher);
  ///

  if Assigned(FWorkers) then begin
    AList := FWorkers.LockList;
    try
      for AWorker in AList do
        if Assigned(AWorker) then
          FreeAndNil(AWorker);
    finally
      FWorkers.UnlockList;
    end;

    ///
    FreeAndNil(FWorkers);
  end;

  ///
  inherited Destroy();
end;

procedure TFormThreadManager.AddWorker(const AThread : TWorkerThread);
var pNode : PVirtualNode;
    pData : PTreeData;
    AList : TList<TWorkerThread>;
begin
  VST.BeginUpdate();
  try
    pNode := VST.AddChild(nil);
    pData := pNode.GetData;
    ///

    pData^.StartDateTime := Now();
    pData^.ThreadId      := AThread.ThreadID;
    pData^.ThreadHandle  := AThread.Handle;
    pData^.Thread        := AThread;
    pData^.ClassName     := AThread.ClassName;

    ///
    AList := FWorkers.LockList;
    try
      AList.Add(AThread);
    finally
      FWorkers.UnlockList;
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormThreadManager.AddWorkerAndStart(const AThread : TWorkerThread);
begin
  AddWorker(AThread);

  ///
  AThread.Start();
end;

procedure TFormThreadManager.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if odd(Node.Index) then begin
    TargetCanvas.Brush.Color := _ODD_LIST_BG_COLOR;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormThreadManager.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VST.Update();
end;

procedure TFormThreadManager.VSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1 : PTreeData;
    pData2 : PTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Exit();

  case Column of
    0 : Result := CompareValue(pData1^.ThreadId, pData2^.ThreadId);
    1 : Result := CompareValue(pData1^.ThreadHandle, pData2^.ThreadHandle);
    2 : Result := CompareText(pData1^.ClassName, pData2^.ClassName);
    3 : Result := CompareDateTime(pData1^.StartDateTime, pData2^.StartDateTime);
  end;
end;

procedure TFormThreadManager.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFormThreadManager.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  ///
end;

procedure TFormThreadManager.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  if (Column = 0) and ((Kind = ikNormal) or (Kind = ikSelected)) then
    ImageIndex := _STATE_IMAGE_GEAR;
end;

procedure TFormThreadManager.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormThreadManager.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  case Column of
    0 : CellText := IntToStr(pData^.ThreadID);
    1 : CellText := IntToStr(pData^.ThreadHandle);
    2 : CellText := pData^.ClassName;
    3 : CellText := DateTimeToStr(pData^.StartDateTime);
  end;
end;

end.
