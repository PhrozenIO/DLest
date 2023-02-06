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

// TODO: Improve the code quality and efficiency

unit uPortableExecutable;

interface

uses Winapi.Windows,
     System.Classes,
     Generics.Collections,
     XSuperObject,
     Winapi.ActiveX;

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

  TParseFrom = (
    pfFile,
    pfMemory
  );

  TPortableExecutable = class;

  TSection = class
  private
    FOwner              : TPortableExecutable;
    FName               : String;
    FHeaderOffset       : UInt64;
    FImageSectionHeader : TImageSectionHeader;

    {@M}
    function GetDataOffset() : UInt64;
    function GetDataSize() : Cardinal;
  public
    {@C}
    constructor Create(const AOwner : TPortableExecutable; const AImageSectionHeader : TImageSectionHeader; const AHeaderOffset : UInt64);

    {@G}
    property Name               : String              read FName;
    property HeaderOffset       : UInt64              read FHeaderOffset;
    property DataOffset         : UInt64              read GetDataOffset;
    property DataSize           : Cardinal            read GetDataSize;
    property ImageSectionHeader : TImageSectionHeader read FImageSectionHeader;
  end;

  TExportEntry = class(TPersistent)
  private
    FName    : String;
    FOrdinal : Longint;
  public
    {@C}
    constructor Create();

    {@M}
    procedure Assign(ASource : TPersistent); override;
    function ToJson() : ISuperObject; virtual;

    {@G/S}
    property Name    : String  read FName    write FName;
    property Ordinal : Longint read FOrdinal write FOrdinal;
  end;

  TCOMKind = (
    comUnknown,
    comkMethod,
    comkProperty
  );

  TCOMExportEntry = class(TExportEntry)
  private
    FTypeName : String;
    FCOMKind  : TCOMKind;
  public
    {@C}
    constructor Create(const ATypeName : String = ''); overload;
    constructor Create(const AExport : TCOMExportEntry); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;
    function ToJson() : ISuperObject; override;

    {@/S}
    property COMKind  : TCOMKind read FCOMKind  write FCOMKind  default comUnknown;
    property TypeName : String   read FTypeName write FTypeName;
  end;

  TPEExportEntry = class(TExportEntry)
  private
    FRelativeAddress : UInt64;
    FAddress         : UInt64;
    FForwarded       : Boolean;
    FForwardName     : AnsiString;
  public
    {@C}
    constructor Create(); overload;
    constructor Create(const AExport : TPEExportEntry); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;
    function ToJson() : ISuperObject; override;

    {@G/S}
    property RelativeAddress : UInt64     read FRelativeAddress write FRelativeAddress;
    property Address         : UInt64     read FAddress         write FAddress;

    property Forwarded       : Boolean    read FForwarded       write FForwarded;
    property ForwardName     : AnsiString read FForwardName     write FForwardName;
  end;

  TPortableExecutable = class
  private
    FSuccess                   : Boolean;
    FParseFrom                 : TParseFrom;
    FFileName                  : String;

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
    FExports                   : TObjectList<TExportEntry>;

    FExportDataDirectory       : TImageDataDirectory;
    FImageExportDirectory      : TImageExportDirectory;

    FImportDataDirectory       : TImageDataDirectory;

    FScanCOMTypesLibraries     : Boolean;

    {@C}
    constructor Create();

    {@M}
    function GetImageDosHeader() : TImageDosHeader;
    function GetImageNtSignature() : DWORD;
    function GetImageFileHeader() : TImageFileHeader;
    function GetImageOptionalHeader() : TImageOptionalHeader;
    function GetSections() : TObjectList<TSection>;
    function GetExports() : TObjectList<TExportEntry>;
    function GetIs64() : Boolean;

    procedure ScanCOMTypesLibraries();

    procedure RaiseUnparsedHeader();

    procedure Read(const AOffset : UInt64; pBuffer : Pointer; const ABufferSize : UInt64);
    procedure Parse();
  public
    {@C}
    constructor CreateFromFile(const AFileName : String; const AScanCOMTypesLibraries : Boolean = False);
    constructor CreateFromMemory(const AProcessId : Cardinal; const ABaseAddress : Pointer); overload;
    constructor CreateFromMemory(const AProcessHandle : THandle; const ABaseAddress : Pointer); overload;

    destructor Destroy(); override;

    {@M}
    function GetHeaderSectionNature(const AOffset : UInt64; var AExtraInfo : String) : TPEHeaderSectionNature; overload;
    function GetHeaderSectionNature(const AOffset : UInt64) : TPEHeaderSectionNature; overload;

    function GetHeaderSectionName(const AOffset : UInt64; const AShortName : Boolean; var APEHeaderSectionNature : TPEHeaderSectionNature) : String; overload;
    function GetHeaderSectionName(const AOffset : UInt64; const AShortName : Boolean) : String; overload;

    function SaveToFile(const ADestinationFile : String) : Boolean;

    {@G}
    property ImageDosHeader      : TImageDosHeader           read GetImageDosHeader;
    property ImageNtSignature    : DWORD                     read GetImageNtSignature;
    property ImageFileHeader     : TImageFileHeader          read GetImageFileHeader;
    property ImageOptionalHeader : TImageOptionalHeader      read GetImageOptionalHeader;
    property Sections            : TObjectList<TSection>     read GetSections;
    property ExportList          : TObjectList<TExportEntry> read GetExports;
    property BaseOffset          : UInt64                    read FBaseOffset;
    property ParseFrom           : TParseFrom                read FParseFrom;
    property Is64                : Boolean                   read GetIs64;
  end;

  var
    Wow64RevertWow64FsRedirection : function(AOlValue : LongBool) : LongBool; stdcall = nil;
    Wow64DisableWow64FsRedirection : function(var AOldValue : LongBool) : LongBool; stdcall = nil;

implementation

uses uExceptions, System.SysUtils, Winapi.ImageHlp;

(* Globals *)

procedure ComputePECheckSum(const APEFile : String);
var hFile        : THandle;
    hFileMap     : THandle;
    pFileMapView : Pointer;
    AFileSize    : Int64;
    AHeaderSum   : DWORD;
    ACheckSum    : DWORD;
    AOffset      : DWORD;
begin
  hFile := CreateFileW(
      PWideChar(APEFile),
      GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ,
      nil,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
  );
  if hFile = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create(Format('CreateFileW[%s]', [APEFile]));
  try
    if not GetFileSizeEx(hFile, AFileSize) then
      raise EWindowsException.Create(Format('GetFileSizeEx[%s]', [APEFile]));
    ///

    hFileMap := CreateFileMapping(hFile, nil, PAGE_READWRITE, 0, AFileSize, nil);
    if hFileMap = 0 then
      raise EWindowsException.Create(Format('CreateFileMapping[%s]', [APEFile]));
    try
      pFileMapView := MapViewOfFile(hFileMap, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, AFileSize);
      if pFileMapView = nil then
        raise EWindowsException.Create(Format('MapViewOfFile[%s]', [APEFile]));
      try
        if CheckSumMappedFile(pFileMapView, AFileSize, @AHeaderSum, @ACheckSum) = nil then
          raise EWindowsException.Create(Format('CheckSumMappedFile[%s]', [APEFile]));
        ///

        if (PImageDosHeader(pFileMapView)^.e_magic <> IMAGE_DOS_SIGNATURE) or
           (PDWORD(NativeUInt(pFileMapView) + PImageDosHeader(pFileMapView)^._lfanew)^ <> IMAGE_NT_SIGNATURE) then
          raise Exception.Create(Format('"%s" is not a valid PE file.', [APEFile]));
        ///

        // Hot-Patch Checksum in Optional Header
        AOffset := PImageDosHeader(pFileMapView)^._lfanew;
        Inc(AOffset, SizeOf(DWORD)); // NT Signature
        Inc(AOffset, SizeOf(TImageFileHeader));

        PImageOptionalHeader(Pointer(NativeUInt(pFileMapView) + AOffset))^.CheckSum := ACheckSum;
      finally
        FlushViewOfFile(pFileMapView, AFileSize);
        UnmapViewOfFile(pFileMapView);
      end;
    finally
      CloseHandle(hFileMap);
    end;
  finally
    CloseHandle(hFile);
  end;
end;

(* TPortableExecutable Class *)

{ TPortableExecutable.Create }
constructor TPortableExecutable.Create();
begin
  inherited Create();
  ///

  FSuccess               := False;
  FParseFrom             := pfFile;
  FHandle                := INVALID_HANDLE_VALUE;
  FCloseHandle           := True;
  FBaseOffset            := 0;
  FScanCOMTypesLibraries := False;
  FFileName              := '';

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
  FExports  := TObjectList<TExportEntry>.Create(True);
end;

{ TPortableExecutable.Read }
procedure TPortableExecutable.Read(const AOffset : UInt64; pBuffer : Pointer; const ABufferSize : UInt64);
var ABytesRead  : Cardinal;
    stBytesRead : SIZE_T;
begin
  if not Assigned(pBuffer) then
    raise Exception.Create('Invalid Pointer');
  ///

  if ABufferSize = 0 then
    Exit();
  
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
end;

{ TPortableExecutable.Parse }
procedure TPortableExecutable.Parse();
var AOffset              : UInt64;
    I                    : UInt64;
    ASectionHeader       : TImageSectionHeader;
    ASectionHeaderOffset : UInt64;
    AExport              : TPEExportEntry;
    AOrdinal             : Word;
    ANameOffset          : UInt64;
    ALength              : UInt64;

  procedure Read(pBuffer : Pointer; const ABufferSize : UInt64; var ASavedOffset : UInt64; const AForwardOffset : Boolean = False); overload;
  var ABytesRead  : Cardinal;
      stBytesRead : SIZE_T;
  begin
    self.Read(AOffset, pBuffer, ABufferSize);
    ///

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
  ALength := FImageDosHeader._lfanew - SizeOf(TImageDosHeader);

  if ALength > 0 then begin
    SetLength(FDosStub, ALength);

    Read(@FDosStub[0], Length(FDosStub), True);
  end;

  AOffset := FBaseOffset + FImageDosHeader._lfanew;

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

    FSections.Add(TSection.Create(self, ASectionHeader, ASectionHeaderOffset));
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
      AExport := TPEExportEntry.Create();

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

      ZeroMemory(@ANameOffset, SizeOf(Cardinal));

      Read(@ANameOffset, SizeOf(Cardinal), False);

      AOffset := SectionRVAToFileOffset(ANameOffset);

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

  (*
    Read COM Objects Methods + Properties (Only possible through physical path reading)
    TODO: Parse from memory.
  *)
  if (FParseFrom = pfFile) and (FScanCOMTypesLibraries) then
    self.ScanCOMTypesLibraries();

  ///
  FSuccess := True;
end;

{ TPortableExecutable.ScanCOMTypesLibraries }
procedure TPortableExecutable.ScanCOMTypesLibraries();
var ATypeLib    : ITypeLib;
    AResult     : Integer;
    I, N        : Cardinal;
    ATypeInfo   : ITypeInfo;
    ptrTypeAttr : PTypeAttr;
    ptrFuncDesc : PFuncDesc;
    ATypeName   : String;
    pTypeName   : PWideChar;
    AItemName   : String;
    pFuncName   : PWideChar;
    AExport     : TCOMExportEntry;
begin
  try
    CoInitialize(nil);
    try
      // Investigate how we could load COM Type Libraries info directly from memory.
      // CreatePointerMoniker ?
      AResult := LoadTypeLib(PWideChar(FFileName), ATypeLib);
      if AResult <> S_OK then
        Exit();

      for I := 0 to ATypeLib.GetTypeInfoCount - 1 do begin
        AResult := ATypeLib.GetTypeInfo(I, ATypeInfo);
        if AResult <> S_OK then
          continue;

        AResult := ATypeInfo.GetTypeAttr(ptrTypeAttr);
        if AResult <> S_OK then
          continue;
        try
          case ptrTypeAttr^.typekind of
            TKIND_INTERFACE,
            TKIND_DISPATCH : ;

            else
              continue;
          end;
          ///

          ATypeInfo.GetDocumentation(-1, @pTypeName, nil, nil, nil);
          try
            ATypeName := WideString(pTypeName);
          finally
            SysFreeString(pTypeName);
          end;


          if ptrTypeAttr^.cFuncs > 0 then begin
            for N := 0 to ptrTypeAttr^.cFuncs -1 do begin
              AResult := ATypeInfo.GetFuncDesc(N, ptrFuncDesc);
              if AResult <> S_OK then
                continue;
              try
                AResult := ATypeinfo.GetDocumentation(ptrFuncDesc^.memid, @pFuncName, nil, nil, nil);
                if AResult <> S_OK then
                  continue;
                try
                  //
                  AItemName := WideString(pFuncName);
                finally
                  SysFreeString(PWideChar(pFuncName));
                end;

                if ptrFuncDesc.wFuncFlags = FUNCFLAG_FRESTRICTED then
                  continue;

                AExport := TCOMExportEntry.Create(ATypeName);
                AExport.Name := AItemName;
                AExport.Ordinal := ptrFuncDesc^.memid;

                case ptrFuncDesc.invkind of
                  INVOKE_FUNC: AExport.COMKind := comkMethod;

                  INVOKE_PROPERTYGET(*,
                  INVOKE_PROPERTYPUT,
                  INVOKE_PROPERTYPUTREF*): AExport.COMKind := comkProperty;
                end;

                ///
                FExports.Add(AExport);
              finally
                ATypeInfo.ReleaseFuncDesc(ptrFuncDesc);
              end;
            end;
          end;
        finally
          ATypeInfo.ReleaseTypeAttr(ptrTypeAttr);
        end;
      end;
    finally
      CoUninitialize();
    end;
  except
  end;
end;

{ TPortableExecutable.CreateFromFile }
constructor TPortableExecutable.CreateFromFile(const AFileName : String; const AScanCOMTypesLibraries : Boolean = False);
var AOldWow64RedirectionValue : LongBool;
begin
  Create();
  ///

  FParseFrom             := pfFile;
  FScanCOMTypesLibraries := AScanCOMTypesLibraries;
  FFileName              := AFileName;

// TODO In Option
//  if Assigned(Wow64DisableWow64FsRedirection) then
//    Wow64DisableWow64FsRedirection(AOldWow64RedirectionValue);
//  try
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
//  finally
//    if Assigned(Wow64RevertWow64FsRedirection) then
//      Wow64RevertWow64FsRedirection(AOldWow64RedirectionValue);
//  end;

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

{ _.GetHeaderSectionName }
function TPortableExecutable.GetHeaderSectionName(const AOffset : UInt64; const AShortName : Boolean) : String;
var ANature : TPEHeaderSectionNature;
begin
  self.GetHeaderSectionName(AOffset, AShortName, ANature);
end;

{ TPortableExecutable.SaveToFile }
function TPortableExecutable.SaveToFile(const ADestinationFile : String) : Boolean;
var AFileStream         : TFileStream;
    ASection            : TSection;
    AImageSectionHeader : TImageSectionHeader;
    I                   : Cardinal;
    pData               : Pointer;
begin
  result := False;
  ///
  
  RaiseUnparsedHeader();
  ///

  ForceDirectories(ExtractFilePath(ADestinationFile));

  AFileStream := TFileStream.Create(ADestinationFile, fmOpenWrite or fmCreate or fmShareExclusive);
  try
    // Write DOS Header
    AFileStream.Write(FImageDosHeader, SizeOf(TImageDosHeader));

    // Write DOS Stub
    AFileStream.Write(FDosStub[0], Length(FDosStub));

    // Write NT Header Signature
    AFileStream.Write(FImageNtSignature, SizeOf(DWORD));

    // Write Image File Header
    AFileStream.Write(FImageFileHeader, SizeOf(TImageFileHeader));

    // Write The Optional Header
    AFileStream.Write(FImageOptionalHeader, SizeOf(TImageOptionalHeader));

    // Write Section Headers
    for I := 0 to FSections.count -1 do begin
      ASection := FSections.Items[I];
      ///
      
      CopyMemory(
        @AImageSectionHeader,
        @ASection.ImageSectionHeader,
        SizeOf(TImageSectionHeader)
      );

      case FParseFrom of
        pfFile: ;
        pfMemory: begin
          // Align Section
          AImageSectionHeader.PointerToRawData := AImageSectionHeader.VirtualAddress;

          if (I + 1 < FSections.Count) then
            AImageSectionHeader.SizeOfRawData := (
              FSections.Items[I + 1].ImageSectionHeader.VirtualAddress - AImageSectionHeader.VirtualAddress
            );
        end;
      end;

      ///
      AFileStream.Write(AImageSectionHeader, SizeOf(TImageSectionHeader));
    end;

    // Write Section Data (JIT)
    for ASection in FSections do begin
      case FParseFrom of
        pfFile   : AFileStream.Position := ASection.ImageSectionHeader.PointerToRawData;
        pfMemory : AFileStream.Position := ASection.ImageSectionHeader.VirtualAddress;
      end;

      if ASection.DataSize > 0 then begin
        GetMem(pData, ASection.DataSize);
        try
          self.Read(ASection.DataOffset, pData, ASection.DataSize);

          ///
          AFileStream.Write(PByte(pData)^, ASection.DataSize);
        finally
          FreeMem(pData, ASection.DataSize);
        end;
      end;
    end;

    ///
    result := True;
  finally
    if Assigned(AFileStream) then
      FreeAndNil(AFileStream);
  end;

  // Recalculate PE CheckSum
  ComputePECheckSum(ADestinationFile); // TODO apply on the fly (current object) instead of reloading file.
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
function TPortableExecutable.GetExports() : TObjectList<TExportEntry>;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FExports;
end;

{ TPortableExecutable.GetIs64 }
function TPortableExecutable.GetIs64() : Boolean;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageFileHeader.Machine = IMAGE_FILE_MACHINE_AMD64;
end;

(* TSection Class *)

{ TSection.Create }
constructor TSection.Create(const AOwner : TPortableExecutable; const AImageSectionHeader : TImageSectionHeader; const AHeaderOffset : UInt64);
begin
  inherited Create();
  ///

  FOwner := AOwner;

  FHeaderOffset       := AHeaderOffset;
  FImageSectionHeader := AImageSectionHeader;

  SetString(FName, PAnsiChar(@AImageSectionHeader.Name), Length(AImageSectionHeader.Name));

  FName := FName.Trim();
end;

{ TSection.GetDataOffset }
function TSection.GetDataOffset() : UInt64;
begin
  result := FOwner.BaseOffset;
  ///

  case FOwner.ParseFrom of
    pfFile   : Inc(result, FImageSectionHeader.PointerToRawData);
    pfMemory : Inc(result, FImageSectionHeader.VirtualAddress);
  end;
end;

{ TSection.GetDataSize }
function TSection.GetDataSize() : Cardinal;
begin
  result := 0;
  ///

  case FOwner.ParseFrom of
    pfFile   : result := FImageSectionHeader.SizeOfRawData;
    pfMemory : result := FImageSectionHeader.Misc.VirtualSize;
  end;
end;

(* TExportEntry Class *)

{ TExportEntry.Create }
constructor TExportEntry.Create();
begin
  inherited Create();
  ///

  FName    := '';
  FOrdinal := 0;
end;

{ TExportEntry.Assign }
procedure TExportEntry.Assign(ASource : TPersistent);
begin
  if ASource is TExportEntry then begin
    FOrdinal := TExportEntry(ASource).Ordinal;
    FName    := TExportEntry(ASource).Name;
  end else
    inherited;
end;

{ TExportEntry.ToJson }
function TExportEntry.ToJson() : ISuperObject;
begin
  result := SO();
  ///

  result.I['ordinal'] := FOrdinal;
  result.S['name']    := FName;
end;

(* TCOMExportEntry Class *)

{ TCOMExportEntry.Create }
constructor TCOMExportEntry.Create(const ATypeName : String = '');
begin
  inherited Create();
  ///

  FTypeName := ATypeName;
end;

constructor TCOMExportEntry.Create(const AExport : TCOMExportEntry);
begin
  Create();
  ///

  self.Assign(AExport);
end;

{ TCOMExportEntry.Assign }
procedure TCOMExportEntry.Assign(ASource : TPersistent);
begin
  inherited Assign(ASource);
  ///

  if ASource is TCOMExportEntry then begin
    FTypeName := TCOMExportEntry(ASource).TypeName;
    FCOMKind  := TCOMExportEntry(ASource).COMKind;
  end;
end;

{ TCOMExportEntry.ToJson}
function TCOMExportEntry.ToJson() : ISuperObject;
begin
  result := inherited ToJson();
  ///

  result.S['type_name'] := FTypeName;
  result.I['com_kind']  := Integer(FCOMKind);
end;

(* TPEExportEntry Class *)

{ TPEExportEntry.Create }
constructor TPEExportEntry.Create();
begin
  inherited Create();
  ///

  FAddress         := 0;
  FRelativeAddress := 0;
  FForwarded       := False;
  FForwardName     := '';
end;

constructor TPEExportEntry.Create(const AExport : TPEExportEntry);
begin
  Create();
  ///

  self.Assign(AExport);
end;

{ TPEExportEntry.Assign }
procedure TPEExportEntry.Assign(ASource : TPersistent);
begin
  inherited Assign(ASource);
  ///

  if ASource is TPEExportEntry then begin
    FRelativeAddress := TPEExportEntry(ASource).RelativeAddress;
    FAddress         := TPEExportEntry(ASource).Address;
    FForwarded       := TPEExportEntry(ASource).Forwarded;
    FForwardName     := TPEExportEntry(ASource).ForwardName;
  end;
end;

{ TPEExportEntry.ToJson }
function TPEExportEntry.ToJson() : ISuperObject;
begin
  result := inherited ToJson();
  ///

  result.S['relative_addr'] := Format('0x%p', [Pointer(FRelativeAddress)]);
  result.S['address']       := Format('0x%p', [Pointer(FAddress)]);
  result.B['forwarded']     := FForwarded;

  if FForwarded then
    result.S['forward_name'] := FForwardName;
end;

initialization
  Wow64RevertWow64FsRedirection  := GetProcAddress(GetModuleHandle('Kernel32.dll'), 'Wow64RevertWow64FsRedirection');
  Wow64DisableWow64FsRedirection := GetProcAddress(GetModuleHandle('Kernel32.dll'), 'Wow64DisableWow64FsRedirection');

finalization
  Wow64RevertWow64FsRedirection  := nil;
  Wow64DisableWow64FsRedirection := nil;

end.

