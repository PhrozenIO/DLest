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

// - Export to JSON
// - Exception handler and logs
// - Export Scanner Regex (in Scan Folder)

// Export Json:
  // - DLL Name, DLL Path, MD5, SHA1, SHA256, Export List


unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.Menus, Vcl.VirtualImageList, System.ImageList, Vcl.ImgList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Winapi.ShellAPI,
  Vcl.ExtCtrls, Generics.Collections;

type
  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenFolder1: TMenuItem;
    OpenProcess1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    About1: TMenuItem;
    ImageCollection: TImageCollection;
    ImageSystem: TImageList;
    VirtualImageList: TVirtualImageList;
    ScanFolder1: TMenuItem;
    N2: TMenuItem;
    System1: TMenuItem;
    hreadManager1: TMenuItem;
    Pages: TPageControl;
    PopupTabs: TPopupMenu;
    CloseActiveTab1: TMenuItem;
    CloseAllTabs1: TMenuItem;
    N3: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure OpenProcess1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure OpenFolder1Click(Sender: TObject);
    procedure hreadManager1Click(Sender: TObject);
    procedure ScanFolder1Click(Sender: TObject);
    procedure CloseActiveTab1Click(Sender: TObject);
    procedure CloseAllTabs1Click(Sender: TObject);
  private
    FFileInfo : TSHFileInfo;

    {@M}
    procedure CloseTab(const ATab : TTabSheet);
  public
    {@M}
    procedure CloseTabs();
    procedure CloseActiveTab();
  end;

var
  FormMain: TFormMain;

implementation

uses uEnumExportsThread, uFunctions, uFormProcessList, VCL.FileCtrl,
  uFormThreadManager, uScanFilesThread, uFormScanFolder, uFrameList;

{$R *.dfm}

procedure TFormMain.CloseTabs();
var ATab  : TTabSheet;
    ATabs : TList<TTabSheet>;
    I     : Cardinal;
begin
  ATabs := TList<TTabSheet>.Create();
  try
    for I := 0 to Pages.PageCount -1 do
      ATabs.Add(Pages.Pages[I]);

    for ATab in ATabs do
      CloseTab(ATab);
  finally
    FreeAndNil(ATabs);
  end;
end;

procedure TFormMain.CloseActiveTab();
begin
  CloseTab(Pages.ActivePage);
end;

procedure TFormMain.CloseActiveTab1Click(Sender: TObject);
begin
  self.CloseActiveTab();
end;

procedure TFormMain.CloseAllTabs1Click(Sender: TObject);
begin
  self.CloseTabs();
end;

procedure TFormMain.CloseTab(const ATab : TTabSheet);
begin
  if not Assigned(ATab) then
    Exit();
  ///

  if not (ATab.Controls[0] is TFrameList) then
    Exit();

  if Assigned(ATab.Controls[0]) then
    FreeAndNil(ATab.Controls[0]);

  ///
  ATab.Free;
end;

procedure TFormMain.Exit1Click(Sender: TObject);
begin
  self.Close();
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitializeSystemIcons(ImageSystem, FFileInfo);
  ///

end;

procedure TFormMain.hreadManager1Click(Sender: TObject);
begin
  FormThreadManager.Show();
end;

procedure TFormMain.Open1Click(Sender: TObject);
begin
  if not OpenDialog.Execute() then
    Exit();

  if OpenDialog.Files.Count = 1 then
    FormThreadManager.AddWorkerAndStart(
      TEnumExportsThread.Create(OpenDialog.FileName)
    )
  else if OpenDialog.Files.Count > 1 then begin
    FormThreadManager.AddWorkerAndStart(TEnumExportsThread.Create(
      TStringList(OpenDialog.Files),
      ExtractFilePath(OpenDialog.Files.Strings[0])
    ));
  end;
end;

procedure TFormMain.OpenFolder1Click(Sender: TObject);
var ADirectory : String;
begin
  if not SelectDirectory('Select directory', '', ADirectory, [sdShowShares]) then
    Exit();
  ///

  FormThreadManager.AddWorkerAndStart(TScanFilesThread.Create(ADirectory));
end;

procedure TFormMain.OpenProcess1Click(Sender: TObject);
begin
  FormProcessList.ShowModal();
end;

procedure TFormMain.ScanFolder1Click(Sender: TObject);
begin
  FormScanFolder.ShowModal();
end;

end.
