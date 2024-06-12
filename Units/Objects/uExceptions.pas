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

unit uExceptions;

interface

uses System.SysUtils;

type
  EPhrozenException = class(Exception)
  private
    FDisplay : Boolean;
  public
    {@C}
    constructor Create(const AMessage : String; const ADisplay : Boolean = False); overload;

    {@G}
    property Display : Boolean read FDisplay;
  end;

  EWindowsException = class(EPhrozenException)
  private
    FLastError : Integer;
  public
    {@C}
    constructor Create(const WinAPI : String; const ADisplay : Boolean = False); overload;

    {@G}
    property LastError : Integer read FLastError;
  end;

  TPEExceptionKind = (
    peekInvalidDosHeader,
    peekInvalidSignature,
    peekIncompatibleArchitecture,
    peekInvalidArchitecture
  );

  EPortableExecutableException = class(EPhrozenException)
  public
    {@C}
    constructor Create(const AExceptionKind : TPEExceptionKind; const ADisplay : Boolean = False); overload;
  end;

implementation

uses Winapi.Windows;

(* EPhrozenException *)

{ EPhrozenException.Create }
constructor EPhrozenException.Create(const AMessage : String; const ADisplay : Boolean = False);
begin
  inherited Create(AMessage);
  ///

  FDisplay := ADisplay;
end;

(* EWindowsException *)

{ EWindowsException.Create }
constructor EWindowsException.Create(const WinAPI : String; const ADisplay : Boolean = False);
var AFormatedMessage : String;
begin
  FLastError := GetLastError();

  AFormatedMessage := Format('%s: last_err=%d, last_err_msg="%s".', [
      WinAPI,
      FLastError,
      SysErrorMessage(FLastError)
  ]);

  ///
  inherited Create(AFormatedMessage, ADisplay);
end;

(* EPortableExecutableException *)

{ EPortableExecutableException.Create }
constructor EPortableExecutableException.Create(const AExceptionKind : TPEExceptionKind; const ADisplay : Boolean = False);
var AFormatedMessage : String;
    AReason          : String;
begin
  case AExceptionKind of
    peekInvalidDosHeader         : AReason := 'Invalid Image Dos Header';
    peekInvalidSignature         : AReason := 'Invalid NT Header Signature';
    peekInvalidArchitecture      : AReason := 'Invalid Architecture';
    peekIncompatibleArchitecture : AReason := 'PE CPU Architecture does not match current process CPU Architecture.';

    else
      AReason := 'Unknown';
  end;

  AFormatedMessage := Format('Invalid PE File: "%s"', [
    AReason
  ]);

  ///
  inherited Create(AFormatedMessage, ADisplay);
end;

end.
