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

program dlest;

uses
  Vcl.Forms,
  uPortableExecutable in 'Units\Objects\uPortableExecutable.pas',
  uExceptions in 'Units\Objects\uExceptions.pas',
  uGraphicUtils in 'Units\uGraphicUtils.pas',
  XSuperObject in 'Libs\XSuperObject\XSuperObject.pas',
  XSuperJSON in 'Libs\XSuperObject\XSuperJSON.pas',
  uWorkerThread in 'Units\Threads\Components\uWorkerThread.pas',
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  uFrameList in 'Units\Frames\uFrameList.pas' {FrameList: TFrame},
  uFunctions in 'Units\uFunctions.pas',
  uEnumExportsThread in 'Units\Threads\uEnumExportsThread.pas',
  uConstants in 'Units\uConstants.pas',
  uFormProcessList in 'Units\Forms\uFormProcessList.pas' {FormProcessList},
  uFormThreadManager in 'Units\Forms\uFormThreadManager.pas' {FormThreadManager},
  uEnumProcessThread in 'Units\Threads\uEnumProcessThread.pas',
  uEnumModulesThread in 'Units\Threads\uEnumModulesThread.pas',
  uScanFilesThread in 'Units\Threads\uScanFilesThread.pas',
  uFormTask in 'Units\Forms\uFormTask.pas' {FormTask},
  uFormScanFolder in 'Units\Forms\uFormScanFolder.pas' {FormScanFolder},
  uFormLogs in 'Units\Forms\uFormLogs.pas' {FormLogs},
  uFormAbout in 'Units\Forms\uFormAbout.pas' {FormAbout},
  uExportExportsToJsonThread in 'Units\Threads\uExportExportsToJsonThread.pas',
  uApplication in 'Units\uApplication.pas',
  uTypes in 'Units\uTypes.pas',
  uVirtualStringTreeUtils in 'Units\uVirtualStringTreeUtils.pas',
  uFormExtendedLibrariesInformation in 'Units\Forms\uFormExtendedLibrariesInformation.pas' {FormExtendedLibrariesInformation},
  Vcl.Themes,
  Vcl.Styles,
  uFormProcessMonitor in 'Units\Forms\uFormProcessMonitor.pas' {FormProcessMonitor},
  uFormProcessMonitorOptions in 'Units\Forms\uFormProcessMonitorOptions.pas' {FormProcessMonitorOptions},
  uDebuggerThread in 'Units\Threads\uDebuggerThread.pas',
  uDebugProcessHelper in 'Units\uDebugProcessHelper.pas',
  uFormHashMe in 'Units\Forms\uFormHashMe.pas' {FormHashMe};

{$R *.res}

begin
  isMultiThread := True;

  NTSetPrivilege('SeDebugPrivilege', True);
  ///

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormProcessList, FormProcessList);
  Application.CreateForm(TFormThreadManager, FormThreadManager);
  Application.CreateForm(TFormScanFolder, FormScanFolder);
  Application.CreateForm(TFormLogs, FormLogs);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormProcessMonitor, FormProcessMonitor);
  Application.CreateForm(TFormProcessMonitorOptions, FormProcessMonitorOptions);
  Application.CreateForm(TFormHashMe, FormHashMe);
  Application.Run;
end.
