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

unit uFormAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.VirtualImage, Vcl.StdCtrls;

type
  TFormAbout = class(TForm)
    Logo: TVirtualImage;
    LabelName: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label1: TLabel;
    ImageFlag: TVirtualImage;
    ImageRepo: TVirtualImage;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure ImageRepoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    {@M}
    procedure DoResize();
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

uses uFormMain, uFunctions, uApplication;

{$R *.dfm}

procedure TFormAbout.DoResize();
begin
  ImageFlag.Top  := Logo.Top + Logo.Height - ImageFlag.Height;
  ImageFlag.Left := (Logo.Width div 2) - (ImageFlag.Width div 2) + ScaleValue(48);

  ClientHeight   := ImageRepo.Top + ImageRepo.Height + ScaleValue(8);
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  LabelName.Caption := Format('%s v%s', [
    APPLICATION_NAME,
    APPLICATION_VERSION
  ]);
end;

procedure TFormAbout.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13, 27: Close();
  end;
end;

procedure TFormAbout.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  DoResize();
end;

procedure TFormAbout.ImageRepoClick(Sender: TObject);
begin
  Open('https://github.com/PhrozenIO/DLest');
end;

procedure TFormAbout.Label1Click(Sender: TObject);
begin
  Open('https://unprotect.it');
end;

procedure TFormAbout.Label3Click(Sender: TObject);
begin
  Open('https://www.twitter.com/darkcodersc');
end;

procedure TFormAbout.Label4Click(Sender: TObject);
begin
  Open('https://www.phrozen.io');
end;

procedure TFormAbout.Label5Click(Sender: TObject);
begin
  Open('https://www.github.com/PhrozenIO');
end;

end.
