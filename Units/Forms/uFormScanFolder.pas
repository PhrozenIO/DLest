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

unit uFormScanFolder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.VirtualImage,
  Vcl.ExtCtrls, Vcl.Buttons;

type
  TFormScanFolder = class(TForm)
    PanelBottom: TPanel;
    ButtonValidate: TSpeedButton;
    ButtonCancel: TSpeedButton;
    PanelBackground: TPanel;
    PanelCore: TPanel;
    Shape1: TShape;
    PanelForm: TPanel;
    Label1: TLabel;
    EditDirectory: TButtonedEdit;
    PanelWarning: TPanel;
    Label4: TLabel;
    IconInfo: TVirtualImage;
    PanelIcon: TPanel;
    ImageIcon: TVirtualImage;
    CheckBoxRecursive: TCheckBox;
    CheckBoxDeepScan: TCheckBox;
    Label2: TLabel;
    ButtonedEdit1: TButtonedEdit;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonValidateClick(Sender: TObject);
    procedure EditDirectoryRightButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormScanFolder: TFormScanFolder;

implementation

uses uFormThreadManager, uScanFilesThread, VCL.FileCtrl;

{$R *.dfm}

procedure TFormScanFolder.ButtonCancelClick(Sender: TObject);
begin
  self.Close();
end;

procedure TFormScanFolder.ButtonValidateClick(Sender: TObject);
begin
  if Length(Trim(EditDirectory.Text)) = 0 then begin
    Application.MessageBox('Please specify a valid directory', 'Error', MB_ICONHAND);

    EditDirectory.SetFocus;

    Exit();
  end;

  if not DirectoryExists(EditDirectory.Text) then
    Application.MessageBox(
      PWideChar(Format('Directory "%s" does not exists.', [EditDirectory.Text])),
      'Error',
      MB_ICONHAND
    )
  else begin
    FormThreadManager.AddWorkerAndStart(
      TScanFilesThread.Create(
        EditDirectory.Text,
        CheckBoxDeepScan.Checked,
        CheckBoxRecursive.Checked
      )
    );

    ///
    self.Close();
  end;
end;

procedure TFormScanFolder.EditDirectoryRightButtonClick(Sender: TObject);
var ADirectory : String;
begin
  if not SelectDirectory('Select target directory', '', ADirectory) then
    Exit();
  ///

  EditDirectory.Text := ADirectory;
end;

procedure TFormScanFolder.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13 : ButtonValidate.Click();
    27 : ButtonCancel.Click();
  end;
end;

end.
