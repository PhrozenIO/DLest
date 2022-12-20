object FormScanFolder: TFormScanFolder
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Scan Folder'
  ClientHeight = 283
  ClientWidth = 472
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 241
    Width = 472
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    Color = 16119285
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 206
    ExplicitWidth = 458
    object ButtonValidate: TSpeedButton
      AlignWithMargins = True
      Left = 378
      Top = 8
      Width = 90
      Height = 26
      Margins.Left = 2
      Margins.Top = 8
      Margins.Right = 4
      Margins.Bottom = 8
      Align = alRight
      Caption = 'Validate'
      ImageName = 'bug-filled-new'
      Images = FormMain.VirtualImageList
      OnClick = ButtonValidateClick
      ExplicitLeft = 321
      ExplicitHeight = 25
    end
    object ButtonCancel: TSpeedButton
      AlignWithMargins = True
      Left = 284
      Top = 8
      Width = 90
      Height = 26
      Margins.Top = 8
      Margins.Right = 2
      Margins.Bottom = 8
      Align = alRight
      Caption = 'Cancel'
      OnClick = ButtonCancelClick
      ExplicitLeft = 207
      ExplicitHeight = 25
    end
  end
  object PanelBackground: TPanel
    Left = 0
    Top = 0
    Width = 472
    Height = 241
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 458
    ExplicitHeight = 206
    object PanelCore: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 456
      Height = 225
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      ExplicitWidth = 442
      ExplicitHeight = 190
      object PanelForm: TPanel
        AlignWithMargins = True
        Left = 8
        Top = 0
        Width = 448
        Height = 225
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 0
        ExplicitWidth = 434
        ExplicitHeight = 190
        object Label1: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 8
          Width = 448
          Height = 15
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Align = alTop
          Caption = 'Target Folder'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 74
        end
        object Label2: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 57
          Width = 448
          Height = 15
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Align = alTop
          Caption = 'Filter files with exports that match regex (Expert)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 277
        end
        object EditDirectory: TButtonedEdit
          Left = 0
          Top = 26
          Width = 448
          Height = 23
          Align = alTop
          Images = FormMain.VirtualImageList
          LeftButton.ImageName = 'folder-filled-new'
          RightButton.ImageIndex = 10
          RightButton.ImageName = 'folder-filled'
          RightButton.Visible = True
          TabOrder = 0
          OnRightButtonClick = EditDirectoryRightButtonClick
          ExplicitWidth = 434
        end
        object PanelWarning: TPanel
          Left = 0
          Top = 165
          Width = 448
          Height = 60
          Align = alBottom
          AutoSize = True
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 1
          ExplicitTop = 130
          ExplicitWidth = 434
          object Label4: TLabel
            AlignWithMargins = True
            Left = 20
            Top = 0
            Width = 428
            Height = 60
            Margins.Left = 20
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alClient
            Caption = 
              'Deep Scan is a mode that scans for compatible and valid portable' +
              ' executable (PE) files on a computer or device. This includes no' +
              't only dynamic link library (DLL) files, but also other types of' +
              ' PE files such as executables (EXE) and more.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 6908265
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            ExplicitTop = -1
            ExplicitHeight = 45
          end
          object IconInfo: TVirtualImage
            AlignWithMargins = True
            Left = 0
            Top = 3
            Width = 16
            Height = 16
            ImageCollection = FormMain.ImageCollection
            ImageWidth = 0
            ImageHeight = 0
            ImageIndex = 10
            ImageName = 'button-info'
          end
        end
        object CheckBoxRecursive: TCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 108
          Width = 448
          Height = 17
          Margins.Left = 0
          Margins.Top = 10
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Recursive Lookup'
          TabOrder = 2
          ExplicitWidth = 434
        end
        object CheckBoxDeepScan: TCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 135
          Width = 448
          Height = 17
          Margins.Left = 0
          Margins.Top = 10
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Deep Scan'
          TabOrder = 3
          ExplicitWidth = 434
        end
        object EditRegex: TButtonedEdit
          Left = 0
          Top = 75
          Width = 448
          Height = 23
          Align = alTop
          LeftButton.ImageName = 'folder-filled-new'
          RightButton.Enabled = False
          RightButton.ImageIndex = 10
          RightButton.ImageName = 'folder-filled'
          RightButton.Visible = True
          TabOrder = 4
          OnRightButtonClick = EditDirectoryRightButtonClick
          ExplicitWidth = 434
        end
      end
    end
  end
end
