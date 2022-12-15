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

unit uConstants;

interface

uses VCL.Graphics,
     Winapi.Windows;

const
  _ICON_EXPORT           = 0;
  _ICON_EXPORT_FORWARDED = 1;
  _ICON_PAGES_DLL        = 2;
  _ICON_PAGES_DLL_GROUP  = 3;
  _ICON_PAGES_PROCESS    = 4;

var
  _COLOR_GRAD1_BEG   : TColor;
  _COLOR_GRAD1_END   : TColor;
  _COLOR_LIST_BG_ALT : TColor;

implementation

initialization
  _COLOR_GRAD1_BEG   := $00CAF3FF;
  _COLOR_GRAD1_END   := $008CEBFF;

  _COLOR_LIST_BG_ALT := RGB(230, 250, 255);

end.
