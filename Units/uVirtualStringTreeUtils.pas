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

unit uVirtualStringTreeUtils;

interface

uses VirtualTrees,
     System.Classes,
     System.SysUtils,
     VCL.Menus;

procedure InitializeCopyPopupMenu(const AVST : TVirtualStringTree; const AMenuItem : TMenuItem; const AEvent : TNotifyEvent);

implementation

{ _.InitializeCopyPopupMenu }
procedure InitializeCopyPopupMenu(const AVST : TVirtualStringTree; const AMenuItem : TMenuItem; const AEvent : TNotifyEvent);
var AColumn  : TVirtualTreeColumn;
    I        : Cardinal;

    procedure AddSubMenu(const ACaption : String; const ATag : Integer);
    var ASubMenu : TMenuItem;
    begin
      ASubMenu := TMenuItem.Create(AMenuItem);
      ///

      ASubMenu.Caption := ACaption;
      ASubMenu.Tag     := ATag;

      if ATag >= -1 then
        ASubMenu.OnClick := AEvent;

      AMenuItem.Add(ASubMenu);
    end;

begin
  if not Assigned(AVST) or
     not Assigned(AMenuItem) or
     not Assigned(AEvent) then
    Exit();
  ///

  AMenuItem.Clear();
  ///

  for I := 0 to AVST.Header.Columns.Count -1 do begin
    AColumn := AVST.Header.Columns.Items[i];
    if not Assigned(AColumn) then
      continue;
    ///

    AddSubMenu(Format('Copy selected %s(s)', [
      AColumn.Text
    ]), I);
  end;

  ///
  AddSubMenu('-', -2);

  AddSubMenu('Copy selected line(s)', -1);
end;

end.
