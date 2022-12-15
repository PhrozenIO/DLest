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

unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.Menus, Vcl.VirtualImageList, System.ImageList, Vcl.ImgList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Winapi.ShellAPI;

type
  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Pages: TPageControl;
    Open1: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenFolder1: TMenuItem;
    OpenProcess1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    About1: TMenuItem;
    Config1: TMenuItem;
    GPTAPIKey1: TMenuItem;
    APIKey1: TMenuItem;
    ImageCollection: TImageCollection;
    ImageSystem: TImageList;
    VirtualImageList: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure OpenProcess1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    FFileInfo : TSHFileInfo;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses uEnumExportsThread, uFunctions, uFormProcessList;

{$R *.dfm}

procedure TFormMain.Exit1Click(Sender: TObject);
begin
  self.Close();
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitializeSystemIcons(ImageSystem, FFileInfo);
  ///
end;

procedure TFormMain.Open1Click(Sender: TObject);
begin
  if not OpenDialog.Execute() then
    Exit();

  if OpenDialog.Files.Count = 1 then
    TEnumExportsThread.Create(OpenDialog.FileName)
  else if OpenDialog.Files.Count > 1 then begin
    TEnumExportsThread.Create(
      TStringList(OpenDialog.Files),
      ExtractFilePath(OpenDialog.Files.Strings[0])
    );
  end;
end;

procedure TFormMain.OpenProcess1Click(Sender: TObject);
begin
  FormProcessList.ShowModal();
end;

end.
