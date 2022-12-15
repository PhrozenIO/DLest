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

program dlest;

uses
  Vcl.Forms,
  uPortableExecutable in 'Units\Objects\uPortableExecutable.pas',
  uExceptions in 'Units\Objects\uExceptions.pas',
  uGraphicUtils in 'Units\uGraphicUtils.pas',
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  uFrameList in 'Units\Frames\uFrameList.pas' {FrameList: TFrame},
  uFunctions in 'Units\uFunctions.pas',
  uEnumExportsThread in 'Units\Threads\uEnumExportsThread.pas',
  uConstants in 'Units\uConstants.pas',
  uFormProcessList in 'Units\Forms\uFormProcessList.pas' {FormProcessList},
  uEnumProcessThread in 'Units\Threads\uEnumProcessThread.pas',
  uEnumModulesThread in 'Units\Threads\uEnumModulesThread.pas';

{$R *.res}

begin
  isMultiThread := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormProcessList, FormProcessList);
  Application.Run;
end.
