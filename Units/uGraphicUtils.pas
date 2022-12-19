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

unit uGraphicUtils;

interface

uses Winapi.Windows,
     VCL.Graphics,
     VCL.Forms;

procedure DrawGradient(const ACanvas: TCanvas; const AColorA : TColor; const AColorB: TColor; const ARect: TRect; const AVertical: Boolean);
procedure ShowForm(const AHandle : THandle); overload;
procedure ShowForm(const AForm : TForm); overload;

implementation

uses Winapi.Messages;

{ _.ShowForm }
procedure ShowForm(const AHandle : THandle);
begin
  if AHandle <= 0 then
    exit();

  if NOT IsWindowVisible(AHandle) then
    ShowWindow(AHandle, SW_SHOW);
  ///

  if IsIconic(AHandle) then begin
    if Application.MainForm.Handle = AHandle then
      SendMessage(AHandle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      ShowWindow(AHandle, SW_RESTORE);
  end;

  ///
  SetForeGroundWindow(AHandle);
end;

{ _.ShowForm }
procedure ShowForm(const AForm : TForm);
begin
  if not Assigned(AForm) then
    exit();

  AForm.Show();

  ///
  ShowForm(AForm.Handle);
end;

{ _.DrawGradient }
procedure DrawGradient(const ACanvas: TCanvas; const AColorA : TColor; const AColorB: TColor; const ARect: TRect; const AVertical: Boolean);
var AGradientRect : TGradientRect;
    ATriVertexes  : array [0..2-1] of TTriVertex;

    function ColorToTriVertex(const AColor : TColor; const X : Integer; const Y : Integer) : TTriVertex;
    var ARgbColor : Longint;
    begin
      ARgbColor := ColorToRGB(AColor);
      ///

      result.x     := X;
      result.y     := Y;
      result.Red   := GetRValue(ARgbColor) shl 8;
      result.Blue  := GetBValue(ARgbColor) shl 8;
      result.Green := GetGValue(ARgbColor) shl 8;
    end;

begin
  ATriVertexes[0] := ColorToTriVertex(AColorA, ARect.Left, ARect.Top);
  ATriVertexes[1] := ColorToTriVertex(AColorB, ARect.Right, ARect.Bottom);

  AGradientRect.UpperLeft  := 0;
  AGradientRect.LowerRight := 1;

  ///
  GradientFill(ACanvas.Handle, @ATriVertexes[0], 2, @AGradientRect, 1, integer(AVertical));
end;

end.
