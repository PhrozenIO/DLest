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

unit uFormTask;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.VirtualImage,
  Vcl.StdCtrls, Vcl.ComCtrls, uWorkerThread;

type
  TFormTask = class(TForm)
    PanelIcon: TPanel;
    PanelBody: TPanel;
    VirtualImage1: TVirtualImage;
    Label1: TLabel;
    ProgressBar: TProgressBar;
    PanelBottom: TPanel;
    ButtonCancel: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FThread : TWorkerThread;
  public
    {@C}
    constructor Create(AOwner : TComponent; const AThread : TWorkerThread); overload;
  end;

var
  FormTask: TFormTask;

implementation

uses uFormMain;

{$R *.dfm}

procedure TFormTask.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FThread) then
    FThread.Terminate;

  ///
  TButton(Sender).Caption := 'Cancelling...';
  TButton(Sender).Enabled := False;
end;

constructor TFormTask.Create(AOwner : TComponent; const AThread : TWorkerThread);
begin
  inherited Create(AOwner);
  ///

  FThread := AThread;
end;

procedure TFormTask.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;

  ///
  FormMain.Enabled := True;
end;

procedure TFormTask.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27 : ButtonCancel.Click;
  end;
end;

procedure TFormTask.FormShow(Sender: TObject);
begin
  FormMain.Enabled := False; // Fake Modal
end;

end.
