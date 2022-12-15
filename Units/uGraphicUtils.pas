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
     VCL.Graphics;

procedure DrawGradient(const ACanvas: TCanvas; const AColorA : TColor; const AColorB: TColor; const ARect: TRect; const AVertical: Boolean);

implementation

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
