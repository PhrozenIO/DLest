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

unit uConstants;

interface

uses VCL.Graphics,
     Winapi.Windows,
     Winapi.Messages;

const
  // Image Indexes
  _ICON_EXPORT           = 8;
  _ICON_EXPORT_FORWARDED = 9;
  _ICON_COM_PROPERTY     = 11;
  _ICON_COM_METHOD       = 10;
  _ICON_COM_UNKNOWN      = 23;
  _ICON_UNKNOWN_EXPORT   = 12;

  _STATE_IMAGE_GEAR      = 18;
  _STATE_IMAGE_EXCEPTION = 16;
  _STATE_IMAGE_WARNING   = 17;

  // Custom Windows Messages
  WM_MESSAGE_BASE           = WM_USER + 1403;
  WM_MESSAGE_INCREMENT_PB   = WM_MESSAGE_BASE + 1;
  WM_MESSAGE_BEGIN_UPDATE   = WM_MESSAGE_BASE + 2;
  WM_MESSAGE_END_UPDATE     = WM_MESSAGE_BASE + 3;
  WM_MESSAGE_ITEM_PROCESSED = WM_MESSAGE_BASE + 4;

var
  // Colors
  _COLOR_GRAD1_BEG            : TColor;
  _COLOR_GRAD1_END            : TColor;
  _COLOR_GRAD2_BEG            : TColor;
  _COLOR_GRAD2_END            : TColor;
  _COLOR_LIST_BG_ALT          : TColor;
  _COLOR_LIST_BG_GRAY         : TColor;
  _ODD_LIST_BG_COLOR          : TColor;
  _COLOR_LIGHT_RED            : TColor;

implementation

initialization
  // Colors
  _COLOR_GRAD1_BEG            := $00CAF3FF;
  _COLOR_GRAD1_END            := $008CEBFF;
  _COLOR_LIST_BG_ALT          := rgb(230, 250, 255);
  _ODD_LIST_BG_COLOR          := rgb(250, 250, 250);
  _COLOR_LIGHT_RED            := rgb(254, 236, 231);
  _COLOR_GRAD2_BEG            := rgb(202, 224, 255);
  _COLOR_GRAD2_END            := rgb(140, 207, 255);
  _COLOR_LIST_BG_GRAY         := rgb(250, 250, 250);

end.
