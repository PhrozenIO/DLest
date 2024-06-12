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

(*
 For Next Releases:
  - Integrate OpenAI for Library / export's descriptions.
  - Offer an option to disable Wow64 Redirection (Is it useful???).
  - Upload to VirusTotal.
  - Registry Settings Component
  - Add a feature to search for PE files that use a specific export (Import Table)
  - Add a feature to allow user to param a list of application / argument to open a library (PE tools etc..)
  - Add Import Enumeration (+Color Tabs)
  - For Process Spy, add an option to support DLL's like some debuggers do
  - Internationalization
  - Improve TPopupMenu logic
  - Resolve TODO's

  ---

  Changelog:
    # June 2024
      - Compiled with Delphi 12 Version 29.0.51961.7529
      - Virtual TreeView Component updated to Version 8.0.3
      - Improved design, icons, and structure
      - Unprotect Search integrated (Module / API Name)
      - Cancel folder scan now works as expected
      - It is now possible to cancel the export list enumeration task
      - To considerably improve speed, library hashing (MD5, SHA1, SHA2) has been
        removed from the export enumeration task
      - Better thread synchronization/queue practices implemented to limit overhead
        and increase speed
      - A new live filter mechanism has been added to enable filtering of exports by their type.
        This filter works in conjunction with the export search input, allowing for seamless
        filtering without requiring a refresh.
      - Export statistics displayed to new status bar
      - Folder Search "Deep Scan" was replaced by user-defined wildcard file filter.
      - Extended Library Information feature now offer by default to display libraries as a tree
      - Anonymous exported function / forwarded function are now enumerated (Lone ordinals)
      - Process Spy Feature : Debug a process and monitor for DLL Load signals for export enumeration.
      - File Hash Calculation Tool Feature added.
      - Other code quality improvements


    # June 2023
      - Enumerate COM Object (Method & Properties) - File only (not in-memory yet)
      - Possibility to select which items user want to enumerate (exported function, com properties or methods)
      - Few application icons updated for more confort.
      - Virtual TreeView component updated to version 7.6.4.
      - Compiled with Delphi 11.3.
*)

unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.Menus, Vcl.VirtualImageList, System.ImageList, Vcl.ImgList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Winapi.ShellAPI,
  Vcl.ExtCtrls, Generics.Collections, uFormLogs, uPortableExecutable,
  Vcl.StdCtrls, Vcl.VirtualImage;

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
    N4: TMenuItem;
    RenameActiveTab1: TMenuItem;
    Logs1: TMenuItem;
    ToolBar: TToolBar;
    ToolOpen: TToolButton;
    ToolOpenFolder: TToolButton;
    ToolScan: TToolButton;
    ToolButton3: TToolButton;
    ToolOpenProcess: TToolButton;
    ToolButton4: TToolButton;
    ToolThreadManager: TToolButton;
    ToolLogs: TToolButton;
    ToolAbout: TToolButton;
    ReloadasAdministrator1: TMenuItem;
    N5: TMenuItem;
    ToolButtonAdmin: TToolButton;
    SeparatorAdmin: TToolButton;
    ToolSepAdmin: TToolButton;
    ToolScanVT: TToolButton;
    ToolChat: TToolButton;
    ToolFileHash: TToolButton;
    StatusBar: TStatusBar;
    ButtonProcessMon: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure OpenProcess1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure OpenFolder1Click(Sender: TObject);
    procedure hreadManager1Click(Sender: TObject);
    procedure ScanFolder1Click(Sender: TObject);
    procedure CloseActiveTab1Click(Sender: TObject);
    procedure CloseAllTabs1Click(Sender: TObject);
    procedure RenameActiveTab1Click(Sender: TObject);
    procedure Logs1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure ToolOpenClick(Sender: TObject);
    procedure ToolOpenFolderClick(Sender: TObject);
    procedure ToolScanClick(Sender: TObject);
    procedure ToolOpenProcessClick(Sender: TObject);
    procedure ToolThreadManagerClick(Sender: TObject);
    procedure ToolLogsClick(Sender: TObject);
    procedure ToolAboutClick(Sender: TObject);
    procedure ReloadasAdministrator1Click(Sender: TObject);
    procedure ToolButtonAdminClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure ButtonProcessMonClick(Sender: TObject);
    procedure ToolFileHashClick(Sender: TObject);
  private
    FFileInfo : TSHFileInfo;

    {@M}
    procedure WMDropFiles(var AMessage: TMessage); message WM_DROPFILES;

    function GetScanOptions() : TScanOptions;
  public
    {@M}
    procedure CloseTab(const ATab : TTabSheet);
    procedure CloseTabs();
    procedure CloseActiveTab();
    procedure RenameActiveTab();

    procedure OnException(Sender : TObject; E : Exception);
    procedure Log(const AMessage : String; const Sender: TObject; const ALevel : TLogLevel);
    procedure Warn(const AMessage : String; const Sender : TObject);

    procedure ShowInformation(const AMessage : String);
    procedure UpdateStatusBar();

    {@G}
    property ScanOptions : TScanOptions read GetScanOptions;
  end;

var
  FormMain: TFormMain;

implementation

uses uEnumExportsThread, uFunctions, uFormProcessList, VCL.FileCtrl,
  uFormThreadManager, uScanFilesThread, uFormScanFolder, uFrameList, uFormAbout,
  uApplication, Winapi.ShlObj, uGraphicUtils, uFormProcessMonitor,
  uFormProcessMonitorOptions, uFormHashMe, System.UITypes;

{$R *.dfm}

procedure TFormMain.UpdateStatusBar();
var AStatistics : TExportStatistics;

  function GetValue(const ACount : UInt64; AFilteredCount : UInt64 = 0) : String;
  begin
    if (ACount = 0) and (AFilteredCount = 0) then
      result := '-'
    else begin
      if AFilteredCount = 0 then
        result := IntToStr(ACount)
      else
        result := Format('(V:%d, H:%d)', [
          ACount,
          AFilteredCount
        ]);
    end;
  end;

begin
  ZeroMemory(@AStatistics, SizeOf(TExportStatistics));
  ///

  if Pages.ActivePageIndex > -1 then begin
    var ATab : TTabSheet := Pages.ActivePage;

    if ATab.Controls[0] is TFrameList then begin
      var AFrameList : TFrameList := TFrameList(ATab.Controls[0]);
      ///

      AFrameList.GetStatistics(AStatistics);
    end;
  end;

  ///
  StatusBar.Panels[0].Text := Format('Libraries: %s', [
    GetValue(AStatistics.Libraries, AStatistics.FilteredLibraries)
  ]);

  StatusBar.Panels[1].Text := Format('Total: %s', [
    GetValue(
      AStatistics.Total,
      AStatistics.TotalFiltered
    )
  ]);

  StatusBar.Panels[2].Text := Format('Functions: %s', [
    GetValue(AStatistics.Functions, AStatistics.FilteredFunctions)
  ]);

  StatusBar.Panels[3].Text := Format('Fwd Functions: %s', [
    GetValue(AStatistics.FwdFunctions, AStatistics.FilteredFwdFunctions)
  ]);

  StatusBar.Panels[4].Text := Format('COM: %s', [
    GetValue(AStatistics.COM, AStatistics.FilteredCOM)
  ]);
end;

function TFormMain.GetScanOptions(): TScanOptions;
begin
  result := [];
end;

procedure TFormMain.ShowInformation(const AMessage : String);
begin
  MessageDlg(AMessage, mtInformation, [mbOk], 0);
end;

procedure TFormMain.Log(const AMessage : String; const Sender: TObject; const ALevel : TLogLevel);
begin
  FormLogs.Log(AMessage, Sender, ALevel);
end;

procedure TFormMain.Warn(const AMessage : String; const Sender : TObject);
begin
  Log(AMessage, Sender, llWarning);
end;

procedure TFormMain.OnException(Sender : TObject; E : Exception);
begin
  Log(E.Message, Sender, llException);

  ///
  ShowForm(FormLogs);
end;

procedure TFormMain.ReloadasAdministrator1Click(Sender: TObject);
begin
  if MessageDlg('You are about to close and reload application as administrator. Are you sure?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = ID_YES then begin
    if RunAs(GetModuleName(0)) then
      self.Close()
    else
      MessageDlg('Could not reload application as administrator.', mtError, [mbOk], 0);
  end;
end;

procedure TFormMain.RenameActiveTab();
var ATab     : TTabSheet;
    ACaption : String;
begin
  ATab := Pages.ActivePage;
  ///

  if not Assigned(ATab) then
    Exit();

  if not InputQuery('Update Tab', 'Provide a new name', ACaption) then
    Exit();

  ///
  ATab.Caption := ACaption;
end;

procedure TFormMain.WMDropFiles(var AMessage: TMessage);
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
        FormThreadManager.AddWorkerAndStart(TEnumExportsThread.Create(
          AFiles,
          ExtractFilePath(AFiles.Strings[0]),
          SystemFolderIcon()
        ));

    finally
      FreeAndNil(AFiles);
    end;
  finally
    DragFinish(AMessage.WParam);
  end;
end;

procedure TFormMain.CloseTabs();
var ATab  : TTabSheet;
    ATabs : TList<TTabSheet>;
    I     : Cardinal;
begin
  if MessageDlg('You are about to definitively close all tabs. All data will be lost. Are you sure?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> ID_YES then
    Exit();
  ///

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

procedure TFormMain.About1Click(Sender: TObject);
begin
  FormAbout.ShowModal();
end;

procedure TFormMain.ButtonProcessMonClick(Sender: TObject);
begin
  FormProcessMonitorOptions.ShowModal();
end;

procedure TFormMain.CloseActiveTab();
begin
  if MessageDlg('You are about to definitively close current active tab. All data will be lost. Are you sure?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = ID_YES then
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
  if MessageDlg('You are about to terminate current application. All data will be lost. Are you sure?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = ID_YES then
    self.Close();
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitializeSystemIcons(ImageSystem, FFileInfo);
  ///

  if Assigned(ChangeWindowMessageFilterEx) then
    ChangeWindowMessageFilterEx(self.Handle, WM_DROPFILES, MSGFLT_ALLOW, nil);

  ///
  DragAcceptFiles(self.Handle, true);

  ///
  Application.OnException := OnException;

  ///
  self.Caption := Format('%s v%s (%s) - %s', [
    APPLICATION_NAME,
    APPLICATION_VERSION,
    APPLICATION_ARCHITECTURE,
    GetElevationLabel()
  ]);

  self.ReloadasAdministrator1.Visible := (Win32MajorVersion >= 6);

  if self.ReloadasAdministrator1.Visible then
    self.ReloadasAdministrator1.Visible := not IsUserAnAdmin();

  self.ToolSepAdmin.Visible    := self.ReloadasAdministrator1.Visible;
  self.ToolButtonAdmin.Visible := self.ReloadasAdministrator1.Visible;
  self.SeparatorAdmin.Visible  := self.ReloadasAdministrator1.Visible;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  UpdateStatusBar();
end;

procedure TFormMain.hreadManager1Click(Sender: TObject);
begin
  FormThreadManager.Show();
end;

procedure TFormMain.Logs1Click(Sender: TObject);
begin
  FormLogs.Show();
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
      ExtractFilePath(OpenDialog.Files.Strings[0]),
      SystemFileIcon(OpenDialog.Files.Strings[0])
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
  ShowForm(FormProcessList);
end;

procedure TFormMain.PagesChange(Sender: TObject);
begin
  self.UpdateStatusBar();
end;

procedure TFormMain.RenameActiveTab1Click(Sender: TObject);
begin
  self.RenameActiveTab();
end;

procedure TFormMain.ScanFolder1Click(Sender: TObject);
begin
  FormScanFolder.ShowModal();
end;

procedure TFormMain.ToolAboutClick(Sender: TObject);
begin
  self.About1.Click();
end;

procedure TFormMain.ToolButtonAdminClick(Sender: TObject);
begin
  self.ReloadasAdministrator1.Click();
end;

procedure TFormMain.ToolFileHashClick(Sender: TObject);
begin
  FormHashMe.Show();
end;

procedure TFormMain.ToolLogsClick(Sender: TObject);
begin
  self.Logs1.Click();
end;

procedure TFormMain.ToolOpenClick(Sender: TObject);
begin
  self.Open1.Click();
end;

procedure TFormMain.ToolOpenFolderClick(Sender: TObject);
begin
  self.OpenFolder1.Click();
end;

procedure TFormMain.ToolOpenProcessClick(Sender: TObject);
begin
  self.OpenProcess1.Click();
end;

procedure TFormMain.ToolScanClick(Sender: TObject);
begin
  self.ScanFolder1.Click();
end;

procedure TFormMain.ToolThreadManagerClick(Sender: TObject);
begin
  self.hreadManager1.Click();
end;

end.
