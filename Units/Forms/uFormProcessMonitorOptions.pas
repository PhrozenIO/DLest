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

unit uFormProcessMonitorOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.VirtualImage, Vcl.Buttons, Winapi.ShellAPI;

type
  TFormProcessMonitorOptions = class(TForm)
    Label1: TLabel;
    EditApplication: TButtonedEdit;
    GroupBoxExtraOptions: TGroupBox;
    CheckBoxScanSubProcess: TCheckBox;
    Panel1: TPanel;
    Label3: TLabel;
    VirtualImage1: TVirtualImage;
    PanelBottom: TPanel;
    ButtonValidate: TSpeedButton;
    ButtonCancel: TSpeedButton;
    CheckShowProcess: TCheckBox;
    Label2: TLabel;
    EditArguments: TButtonedEdit;
    OpenDialog: TOpenDialog;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonValidateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditApplicationRightButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    {@M}
    procedure WMDropFiles(var AMessage: TMessage); message WM_DROPFILES;
  public
    { Public declarations }
  end;

var
  FormProcessMonitorOptions: TFormProcessMonitorOptions;

implementation

uses uFormThreadManager, uDebuggerThread, uFunctions, uExceptions;

{$R *.dfm}

procedure TFormProcessMonitorOptions.WMDropFiles(var AMessage: TMessage);
var ALen  : Integer;
    AFile : String;
begin
  try
    ALen := DragQueryFile(AMessage.WParam, 0, nil, 0) +1;

    SetLength(AFile, ALen -1);

    DragQueryFile(AMessage.WParam, 0, PWideChar(AFile), ALen);

    ///
    EditApplication.Text := GetShortcutTarget(AFile);
  finally
    DragFinish(AMessage.WParam);
  end;
end;

procedure TFormProcessMonitorOptions.ButtonCancelClick(Sender: TObject);
begin
  self.Close();
end;

procedure TFormProcessMonitorOptions.ButtonValidateClick(Sender: TObject);
begin
  if not FileExists(EditApplication.Text) then begin
    var ATemp := SearchPath_DELF(EditApplication.Text);
    if FileExists(ATemp) then
      EditApplication.Text := ATemp;
  end;

  if not FileExists(EditApplication.Text) then begin
    Application.MessageBox(
      'Please select an existing and valid Windows Application File (PE Format)',
      'Error',
       MB_ICONHAND
    );

    EditApplication.SetFocus;

    Exit();
  end;

  var ACurrent64 : Boolean;
  {$IFDEF WIN64}
    ACurrent64 := True;
  {$ELSE IF WIN32}
    ACurrent64 := False;
  {$ENDIF}

  var ABasicInformation : TPEBasicInformation;
  try
    ABasicInformation := GetBasicPEInformation(EditApplication.Text);
  except
    on E : EPortableExecutableException do begin
      Application.MessageBox(PWideChar(
        Format('"%s" is not a valid Microsoft Windows application. Please select ' +
        'a valid application file that matches the current DLest architecture.',
        [
          EditApplication.Text
        ])),
        'PE Format Error',
        MB_ICONHAND
      );

      ///
      Exit();
    end;

    else begin
      Application.MessageBox(
        'The file could not be opened or accessed. Please ensure that the file ' +
        'has at least read-only permissions.', 'File Error', MB_ICONHAND
      );

      ///
      Exit();
    end;
  end;

  if not ABasicInformation.IsRunnable then begin
    Application.MessageBox(
      'Despite being a valid Portable Executable file, the target file is not intended ' +
      'to be run as an independent Windows Executable process. Additionally, opening ' +
      'libraries for debugging is not yet supported.',
      'Runtime Error',
      MB_ICONHAND
    );

    ///
    Exit();
  end;


  if (ABasicInformation.Is64Image and ACurrent64) or
     (not ABasicInformation.Is64Image and not ACurrent64) then begin
    FormThreadManager.AddWorkerAndStart(
      TCreateProcessAndDebugThread.Create(
        EditApplication.Text,
        EditArguments.Text,
        CheckBoxScanSubProcess.Checked,
        CheckShowProcess.Checked
      )
    );

    ///
    self.Close();
  end else
    Application.MessageBox(PWideChar(
      Format('To debug the target application, its architecture must match the current DLest ' +
      'architecture. Since the current DLest architecture is %s-bit and the target application ' +
      'is %s-bit, they are incompatible.',
      [
        Ternary(ACurrent64, '64', '32'),
        Ternary(ABasicInformation.Is64Image, '64', '32')
      ])),
      'Architecture Mismatch',
      MB_ICONHAND
    );
end;

procedure TFormProcessMonitorOptions.EditApplicationRightButtonClick(
  Sender: TObject);
begin
  if not self.OpenDialog.Execute() then
    Exit();
  ///

  EditApplication.Text := OpenDialog.FileName;
end;

procedure TFormProcessMonitorOptions.FormCreate(Sender: TObject);
begin
  if Assigned(ChangeWindowMessageFilterEx) then
    ChangeWindowMessageFilterEx(self.Handle, WM_DROPFILES, MSGFLT_ALLOW, nil);

  ///
  DragAcceptFiles(self.Handle, true);
end;

procedure TFormProcessMonitorOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13 : ButtonValidate.Click();
    27 : ButtonCancel.Click();
  end;
end;

procedure TFormProcessMonitorOptions.FormShow(Sender: TObject);
begin
  EditApplication.SetFocus;
end;

end.
