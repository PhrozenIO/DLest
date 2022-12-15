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

unit uPortableExecutable;

interface

uses Winapi.Windows,
     System.Classes,
     Generics.Collections;

type
  TPEHeaderSectionNature = (
    pesnNone,
    pesnDosHeader,
    pesnDosStub,
    pesnNtSignature,
    pesnFileHeader,
    pesnOptionalHeader,
    pesnSectionHeader,
    pesnSectionData
  );

  TImageOptionalHeader =
    {$IFDEF WIN64}
      TImageOptionalHeader64
    {$ELSE}
      TImageOptionalHeader32
    {$ENDIF};
  PImageOptionalHeader = ^TImageOptionalHeader;

  TSection = class
  private
    FName               : String;
    FOffset             : UInt64;
    FImageSectionHeader : TImageSectionHeader;
  public
    {@C}
    constructor Create(const AImageSectionHeader : TImageSectionHeader; const AOffset : UInt64);

    {@G}
    property Name               : String              read FName;
    property Offset             : UInt64              read FOffset;
    property ImageSectionHeader : TImageSectionHeader read FImageSectionHeader;
  end;

  TParseFrom = (
    pfNone,
    pfFile,
    pfMemory
  );

  TExport = class(TPersistent)
  private
    FOrdinal         : Word;
    FRelativeAddress : UInt64;
    FAddress         : UInt64;
    FName            : AnsiString;
    FForwarded       : Boolean;
    FForwardName     : AnsiString;
  public
    {@M}
    constructor Create(); overload;
    constructor Create(const AExport : TExport); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Ordinal         : Word       read FOrdinal         write FOrdinal;
    property RelativeAddress : UInt64     read FRelativeAddress write FRelativeAddress;
    property Address         : UInt64     read FAddress         write FAddress;
    property Name            : AnsiString read FName            write FName;
    property Forwarded       : Boolean    read FForwarded       write FForwarded;
    property ForwardName     : AnsiString read FForwardName     write FForwardName;
  end;

  TPortableExecutable = class
  private
    FSuccess                   : Boolean;
    FParseFrom                 : TParseFrom;

    FHandle                    : THandle;
    FCloseHandle               : Boolean;
    FBaseOffset                : UInt64;

    FImageDosHeader            : TImageDosHeader;

    FDosStubOffset             : UInt64;
    FDosStub                   : array of byte;

    FImageNtSignatureOffset    : UInt64;
    FImageNtSignature          : DWORD;

    FImageFileHeaderOffset     : UInt64;
    FImageFileHeader           : TImageFileHeader;

    FImageOptionalHeaderOffset : UInt64;
    FImageOptionalHeader       : TImageOptionalHeader;

    FSections                  : TObjectList<TSection>;
    FExports                   : TObjectList<TExport>;

    FExportDataDirectory       : TImageDataDirectory;
    FImageExportDirectory      : TImageExportDirectory;

    FImportDataDirectory       : TImageDataDirectory;

    {@C}
    constructor Create();

    {@M}
    function GetImageDosHeader() : TImageDosHeader;
    function GetImageNtSignature() : DWORD;
    function GetImageFileHeader() : TImageFileHeader;
    function GetImageOptionalHeader() : TImageOptionalHeader;
    function GetSections() : TObjectList<TSection>;
    function GetExports() : TObjectList<TExport>;

    procedure RaiseUnparsedHeader();

    procedure Parse();
  public
    {@C}
    constructor CreateFromFile(const AFileName : String);
    constructor CreateFromMemory(const AProcessId : Cardinal; const ABaseAddress : Pointer); overload;
    constructor CreateFromMemory(const AProcessHandle : THandle; const ABaseAddress : Pointer); overload;

    destructor Destroy(); override;

    {@M}
    function GetHeaderSectionNature(const AOffset : UInt64; var AExtraInfo : String) : TPEHeaderSectionNature; overload;
    function GetHeaderSectionNature(const AOffset : UInt64) : TPEHeaderSectionNature; overload;

    function GetHeaderSectionName(const AOffset : UInt64; const AShortName : Boolean; var APEHeaderSectionNature : TPEHeaderSectionNature) : String; overload;
    function GetHeaderSectionName(const AOffset : UInt64; const AShortName : Boolean) : String; overload;

    {@G}
    property ImageDosHeader      : TImageDosHeader       read GetImageDosHeader;
    property ImageNtSignature    : DWORD                 read GetImageNtSignature;
    property ImageFileHeader     : TImageFileHeader      read GetImageFileHeader;
    property ImageOptionalHeader : TImageOptionalHeader  read GetImageOptionalHeader;
    property Sections            : TObjectList<TSection> read GetSections;
    property ExportList          : TObjectList<TExport>  read GetExports;
  end;

  var
    Wow64RevertWow64FsRedirection : function(AOlValue : LongBool) : LongBool; stdcall = nil;
    Wow64DisableWow64FsRedirection : function(var AOldValue : LongBool) : LongBool; stdcall = nil;

implementation

uses uExceptions, System.SysUtils;

(* TPortableExecutable Class *)

{ TPortableExecutable.Create }
constructor TPortableExecutable.Create();
begin
  inherited Create();
  ///

  FSuccess     := False;
  FParseFrom   := pfNone;
  FHandle      := INVALID_HANDLE_VALUE;
  FCloseHandle := True;
  FBaseOffset  := 0;

  FDosStubOffset             := 0;
  FImageNtSignatureOffset    := 0;
  FImageOptionalHeaderOffset := 0;
  FImageFileHeaderOffset     := 0;

  ZeroMemory(@FImageDosHeader, SizeOf(TImageDosHeader));
  ZeroMemory(@FImageFileHeader, SizeOf(TImageFileHeader));
  ZeroMemory(@FImageOptionalHeader, SizeOf(TImageOptionalHeader));

  SetLength(FDosStub, 0);

  FImageNtSignature := 0;

  FSections := TObjectList<TSection>.Create(True);
  FExports  := TObjectList<TExport>.Create(True);;
end;

{ TPortableExecutable.Parse }
procedure TPortableExecutable.Parse();
var AOffset              : UInt64;
    I                    : UInt64;
    ASectionHeader       : TImageSectionHeader;
    ASectionHeaderOffset : UInt64;
    AExport              : TExport;
    AOrdinal             : Word;

  procedure Read(pBuffer : Pointer; const ABufferSize : UInt64; var ASavedOffset : UInt64; const AForwardOffset : Boolean = False); overload;
  var ABytesRead  : Cardinal;
      stBytesRead : SIZE_T;
  begin
    case FParseFrom of
      pfFile: begin
        if not SetFilePointerEx(FHandle, AOffset, nil, FILE_BEGIN) then
          raise EWindowsException.Create('SetFilePointerEx');
        ///

        if not ReadFile(FHandle, PByte(pBuffer)^, ABufferSize, ABytesRead, nil) then
          raise EWindowsException.Create('ReadFile');
      end;

      pfMemory: begin
        if not ReadProcessMemory(FHandle, Pointer(AOffset), pBuffer, ABufferSize, stBytesRead) then
          raise EWindowsException.Create('ReadProcessMemory');
      end;
    end;

    ASavedOffset := AOffset;

    ///
    if AForwardOffset then
      Inc(AOffset, ABufferSize);
  end;

  procedure Read(pBuffer : Pointer; const ABufferSize : UInt64; const AForwardOffset : Boolean = False); overload;
  var ADummy : UInt64;
  begin
    Read(pBuffer, ABufferSize, ADummy, AForwardOffset);
  end;

  function GetStringLength() : Cardinal;
  var ASavedOffset : UInt64;
      AByte        : Byte;
  begin
    result := 0;
    ///

    ASavedOffset := AOffset;
    try
      while True do begin
        Read(@AByte, SizeOf(Byte), True);

        // Increment with printable characters
        if (AByte >= Ord(Low(AnsiChar))) and (AByte <= Ord(High(AnsiChar))) and (AByte <> 0) then
          Inc(result)
        else
          break;
      end;
    finally
      AOffset := ASavedOffset;
    end;
  end;

  function ReadString() : AnsiString;
  var ALength : Cardinal;
      pBuffer : Pointer;
  begin
    result := '';
    ///

    ALength := GetStringLength();
    ///

    if ALength > 0 then begin
      GetMem(pBuffer, ALength);

      Read(pBuffer, ALength, False);

      SetString(result, PAnsiChar(pBuffer), ALength);

      FreeMem(pBuffer, ALength);
    end;
  end;

  function SectionRVAToFileOffset(const ASectionRVA : UInt64) : UInt64;
  var ASection : TSection;
  begin
    result := 0;

    case FParseFrom of
      pfFile: begin
        for ASection in FSections do begin
          if (ASectionRVA >= ASection.ImageSectionHeader.VirtualAddress) and
             (ASectionRVA < (ASection.ImageSectionHeader.VirtualAddress + ASection.ImageSectionHeader.SizeOfRawData)) then begin
            result := ASectionRVA - ASection.ImageSectionHeader.VirtualAddress + ASection.ImageSectionHeader.PointerToRawData;

            break;
           end;
        end;
      end;

      pfMemory: result := FBaseOffset + ASectionRVA;
    end;
  end;

begin
  FSuccess := False;
  ///

  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Invalid handle, can''t parse "PE Header".');
  ///

  AOffset := FBaseOffset;

  (*
      IMAGE_DOS_HEADER
  *)
  Read(@FImageDosHeader, SizeOf(TImageDosHeader), True);

  if FImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
    raise EPortableExecutableException.Create(peekInvalidDosHeader);

  (*
      DOS_STUB
  *)
  SetLength(FDosStub, FImageDosHeader._lfanew - SizeOf(TImageDosHeader));

  Read(@FDosStub[0], Length(FDosStub), True);

  Read(@FImageNtSignature, SizeOf(DWORD), FImageNtSignatureOffset, True);

  (*
      IMAGE_NT_SIGNATURE
  *)
  if FImageNtSignature <> IMAGE_NT_SIGNATURE then
    raise EPortableExecutableException.Create(peekInvalidSignature);

  (*
      IMAGE_FILE_HEADER
  *)
  Read(@FImageFileHeader, SizeOf(TImageFileHeader), FImageFileHeaderOffset, True);

  case FImageFileHeader.Machine of
    IMAGE_FILE_MACHINE_AMD64 : ;
    IMAGE_FILE_MACHINE_I386  : ;
    else
      raise EPortableExecutableException.Create(peekInvalidArchitecture);
  end;

  {$IFDEF WIN64}
    if FImageFileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then
  {$ELSE}
    if FImageFileHeader.Machine <> IMAGE_FILE_MACHINE_I386 then
  {$ENDIF}
    raise EPortableExecutableException.Create(peekIncompatibleArchitecture);

  (*
      IMAGE_OPTIONAL_HEADER
  *)
  Read(@FImageOptionalHeader, SizeOf(TImageOptionalHeader), FImageOptionalHeaderOffset, True);

  FSections.Clear();

  (*
      SECTIONS
  *)
  for I := 1 to FImageFileHeader.NumberOfSections do begin
    Read(@ASectionHeader, SizeOf(TImageSectionHeader), ASectionHeaderOffset, True);

    FSections.Add(TSection.Create(ASectionHeader, ASectionHeaderOffset));
  end;

  (*
      IMPORTS DIRECTORY
  *)
  FImportDataDirectory := FImageOptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  // TODO Read Imports

  (*
      EXPORTS DIRECTORY
  *)
  FExportDataDirectory := FImageOptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];

  AOffset := SectionRVAToFileOffset(FExportDataDirectory.VirtualAddress);

  Read(@FImageExportDirectory, SizeOf(TImageExportDirectory));

  if (FImageExportDirectory.NumberOfFunctions > 0) and
      (FImageExportDirectory.NumberOfNames > 0) then begin
    FExports.Clear();

    for I := 0 to FImageExportDirectory.NumberOfNames {??? NumberOfFunctions ???} -1 do begin
      AExport := TExport.Create();

      // Read Function Ordinal
      AOffset := SectionRVAToFileOffset(FImageExportDirectory.AddressOfNameOrdinals + (I * SizeOf(Word)));

      Read(@AOrdinal, SizeOf(Word));

      AExport.Ordinal := AOrdinal + FImageExportDirectory.Base;

      // Read Function REL Address
      AOffset := SectionRVAToFileOffset(
                    FImageExportDirectory.AddressOfFunctions +
                    (AOrdinal * SizeOf(Cardinal))
      );

      Read(@AExport.RelativeAddress, SizeOf(Cardinal));

      // Set Function Address
      AExport.Address := FImageOptionalHeader.ImageBase + AExport.RelativeAddress;

      // Read Function Name (If applicable)
      AOffset := SectionRVAToFileOffset(
        FImageExportDirectory.AddressOfNames + (I * SizeOf(Cardinal))
      );

      Read(@AOffset, SizeOf(Cardinal), False);

      AOffset := SectionRVAToFileOffset(AOffset);

      if AOffset > 0 then
        AExport.Name := ReadString();

      // Check if Function is forwarded
      AExport.Forwarded := (AExport.RelativeAddress >= FExportDataDirectory.VirtualAddress) and
                           (AExport.RelativeAddress < FExportDataDirectory.VirtualAddress + FExportDataDirectory.Size);


      if AExport.Forwarded then begin
        AOffset := SectionRVAToFileOffset(AExport.RelativeAddress);

        AExport.ForwardName := ReadString();
      end;

      ///
      FExports.Add(AExport);
    end;
  end;

  ///
  FSuccess := True;
end;

{ TPortableExecutable.CreateFromFile }
constructor TPortableExecutable.CreateFromFile(const AFileName : String);
var AOldWow64RedirectionValue : LongBool;
begin
  Create();
  ///

  FParseFrom := pfFile;

  if Assigned(Wow64DisableWow64FsRedirection) then
    Wow64DisableWow64FsRedirection(AOldWow64RedirectionValue);
  try
    FHandle := CreateFileW(
        PWideChar(AFileName),
        GENERIC_READ,
        FILE_SHARE_READ,
        nil,
        OPEN_EXISTING,
        0,
        0
    );
    if FHandle = INVALID_HANDLE_VALUE then
      raise EWindowsException.Create('CreateFileW');
  finally
    if Assigned(Wow64RevertWow64FsRedirection) then
      Wow64RevertWow64FsRedirection(AOldWow64RedirectionValue);
  end;

  ///
  self.Parse();
end;

{ TPortableExecutable.CreateFromMemory }
constructor TPortableExecutable.CreateFromMemory(const AProcessId : Cardinal; const ABaseAddress : Pointer);
var AProcessHandle : THandle;
begin
  AProcessHandle := OpenProcess(
      PROCESS_VM_READ,
      False,
      AProcessId
  );
  if AProcessHandle = 0 then
    raise EWindowsException.Create('OpenProcess');
  ///

  self.CreateFromMemory(AProcessHandle, ABaseAddress);
end;

constructor TPortableExecutable.CreateFromMemory(const AProcessHandle : THandle; const ABaseAddress : Pointer);
begin
  Create();
  ///

  FParseFrom   := pfMemory;

  FHandle      := AProcessHandle;
  FCloseHandle := False;

  FBaseOffset  := UInt64(ABaseAddress);

  ///
  self.Parse();
end;

{ TPortableExecutable.Destroy }
destructor TPortableExecutable.Destroy();
begin
  SetLength(FDosStub, 0);
  ///

  if Assigned(FSections) then
    FreeAndNil(FSections);

  if Assigned(FExports) then
    FreeAndNil(FExports);

  if (FHandle <> INVALID_HANDLE_VALUE) and FCloseHandle then
    CloseHandle(FHandle);

  ///
  inherited Destroy();
end;

{ TPortableExecutable.GetHeaderSectionNature }
function TPortableExecutable.GetHeaderSectionNature(const AOffset : UInt64; var AExtraInfo : String) : TPEHeaderSectionNature;
var ASection       : TSection;
    ANextSection   : TSection;
    ASectionSize   : UInt64;
    ASectionOffset : UInt64;
begin
  result := pesnNone;
  ///

  AExtraInfo := '';

  self.RaiseUnparsedHeader();

  try
    if (AOffset >= FBaseOffset) and (AOffset < FDosStubOffset) then
      result := pesnDosHeader
    else if (AOffset >= FDosStubOffset) and (AOffset < FImageDosHeader._lfanew) then
      result := pesnDosStub
    else if (AOffset >= FImageDosHeader._lfanew) and (AOffset < FImageNtSignatureOffset) then
      result := pesnNtSignature
    else if (AOffset >= FImageNtSignatureOffset) and (AOffset < FImageFileHeaderOffset) then
      result := pesnFileHeader
    else if (AOffset >= FImageFileHeaderOffset) and (AOffset < FImageOptionalHeaderOffset) then
      result := pesnOptionalHeader
    else if (AOffset >= FImageOptionalHeaderOffset) and (
      AOffset < (FImageOptionalHeaderOffset + (FSections.Count * SizeOf(TImageSectionHeader)))
    ) then
      result := pesnSectionHeader
    else begin
      // Section Headers
      for ASection in FSections do begin
        case FParseFrom of
          pfFile : begin
            ASectionSize   := ASection.ImageSectionHeader.SizeOfRawData;
            ASectionOffset := FBaseOffset + ASection.ImageSectionHeader.PointerToRawData;
          end;

          pfMemory : begin
            ASectionSize   := ASection.ImageSectionHeader.Misc.VirtualSize;
            ASectionOffset := FBaseOffset + ASection.ImageSectionHeader.VirtualAddress;
          end;
        end;

        if (AOffset >= ASectionOffset) and (AOffset < (ASectionOffset + ASectionSize)) then begin
          result := pesnSectionData;

          AExtraInfo := ASection.Name;

          break;
        end;
      end;
    end;
  except
    // Ignore (Possible Integer Overflow)
  end;
end;

function TPortableExecutable.GetHeaderSectionNature(const AOffset : UInt64) : TPEHeaderSectionNature;
var ADummy : String;
begin
  GetHeaderSectionNature(AOffset, ADummy);
end;

{ TPortableExecutable.GetHeaderSectionName }
function TPortableExecutable.GetHeaderSectionName(const AOffset : UInt64; const AShortName : Boolean; var APEHeaderSectionNature : TPEHeaderSectionNature) : String;
var AExtraInfo : String;
begin
  result := '';
  ///

  APEHeaderSectionNature := GetHeaderSectionNature(AOffset, AExtraInfo);

  if AShortName then begin
    case APEHeaderSectionNature of
      pesnDosHeader,
      pesnDosStub,
      pesnNtSignature,
      pesnFileHeader,
      pesnOptionalHeader,
      pesnSectionHeader:
        result := 'PE Header';
    end;
  end else begin
    case APEHeaderSectionNature of
      pesnDosHeader      : result := 'Dos Header';
      pesnDosStub        : result := 'Dos Stub';
      pesnNtSignature    : result := 'NT Signature';
      pesnFileHeader     : result := 'File Header';
      pesnOptionalHeader : result := 'Optional Header';
      pesnSectionHeader  : result := 'Section Header';
    end;
  end;

  ///
  if (APEHeaderSectionNature = pesnSectionData) then begin
    if AShortName then
      result := AExtraInfo
    else
      result := Format('%s (Section Data)', [AExtraInfo]);
  end;
end;

function TPortableExecutable.GetHeaderSectionName(const AOffset : UInt64; const AShortName : Boolean) : String;
var ANature : TPEHeaderSectionNature;
begin
  self.GetHeaderSectionName(AOffset, AShortName, ANature);
end;

{ TPortableExecutable.RaiseUnparsedHeader }
procedure TPortableExecutable.RaiseUnparsedHeader();
begin
  if not FSuccess then
    raise Exception.Create('PE Header not parsed.');
end;

{ TPortableExecutable.GetImageDosHeader }
function TPortableExecutable.GetImageDosHeader() : TImageDosHeader;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageDosHeader;
end;

{ TPortableExecutable.GetImageNtSignature }
function TPortableExecutable.GetImageNtSignature() : DWORD;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageNtSignature;
end;

{ TPortableExecutable.GetImageFileHeader }
function TPortableExecutable.GetImageFileHeader() : TImageFileHeader;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageFileHeader;
end;

{ TPortableExecutable.GetImageOptionalHeader }
function TPortableExecutable.GetImageOptionalHeader() : TImageOptionalHeader;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageOptionalHeader;
end;

{ TPortableExecutable.GetSections }
function TPortableExecutable.GetSections() : TObjectList<TSection>;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FSections;
end;

{ TPortableExecutable.GetExports }
function TPortableExecutable.GetExports() : TObjectList<TExport>;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FExports;
end;

(* TSection Class *)

{ TSection.Create }
constructor TSection.Create(const AImageSectionHeader : TImageSectionHeader; const AOffset : UInt64);
begin
  inherited Create();
  ///

  FOffset             := AOffset;
  FImageSectionHeader := AImageSectionHeader;

  SetString(FName, PAnsiChar(@AImageSectionHeader.Name), Length(AImageSectionHeader.Name));

  FName := FName.Trim();
end;

(* TExport Class *)

{ TExport.Create }
constructor TExport.Create();
begin
  inherited Create();
  ///

  FOrdinal         := 0;
  FName            := '';
  FAddress         := 0;
  FRelativeAddress := 0;
  FForwarded       := False;
  FForwardName     := '';
end;

constructor TExport.Create(const AExport : TExport);
begin
  inherited Create();
  ///

  self.Assign(AExport);
end;

{ TExport.Assign }
procedure TExport.Assign(ASource : TPersistent);
begin
  if ASource is TExport then begin
    FOrdinal         := TExport(ASource).Ordinal;
    FRelativeAddress := TExport(ASource).RelativeAddress;
    FAddress         := TExport(ASource).Address;
    FName            := TExport(ASource).Name;
    FForwarded       := TExport(ASource).Forwarded;
    FForwardName     := TExport(ASource).ForwardName;
  end else
    inherited Assign(ASource);
end;

initialization
  Wow64RevertWow64FsRedirection  := GetProcAddress(GetModuleHandle('Kernel32.dll'), 'Wow64RevertWow64FsRedirection');
  Wow64DisableWow64FsRedirection := GetProcAddress(GetModuleHandle('Kernel32.dll'), 'Wow64DisableWow64FsRedirection');

finalization
  Wow64RevertWow64FsRedirection  := nil;
  Wow64DisableWow64FsRedirection := nil;

end.

