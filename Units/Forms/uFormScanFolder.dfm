object FormScanFolder: TFormScanFolder
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Scan Folder'
  ClientHeight = 281
  ClientWidth = 517
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
    Top = 239
    Width = 517
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    Color = 16119285
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 153
    ExplicitWidth = 503
    object ButtonValidate: TSpeedButton
      AlignWithMargins = True
      Left = 423
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
      Left = 329
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
    Width = 517
    Height = 239
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 503
    ExplicitHeight = 153
    object PanelCore: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 501
      Height = 223
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      ExplicitWidth = 487
      ExplicitHeight = 137
      object Shape1: TShape
        Left = 67
        Top = 0
        Width = 2
        Height = 223
        Align = alRight
        Pen.Color = 15790320
        ExplicitLeft = 32
        ExplicitTop = 112
        ExplicitHeight = 65
      end
      object PanelForm: TPanel
        AlignWithMargins = True
        Left = 77
        Top = 0
        Width = 424
        Height = 223
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 0
        ExplicitLeft = 78
        ExplicitTop = 5
        ExplicitHeight = 214
        object Label1: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 8
          Width = 424
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
          ExplicitLeft = -5
          ExplicitTop = 5
        end
        object Label2: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 57
          Width = 424
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
          Width = 424
          Height = 23
          Align = alTop
          Images = FormMain.VirtualImageList
          LeftButton.ImageName = 'folder-filled-new'
          RightButton.ImageIndex = 10
          RightButton.ImageName = 'folder-filled'
          RightButton.Visible = True
          TabOrder = 0
          OnRightButtonClick = EditDirectoryRightButtonClick
          ExplicitTop = 23
        end
        object PanelWarning: TPanel
          Left = 0
          Top = 163
          Width = 424
          Height = 60
          Align = alBottom
          AutoSize = True
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 1
          ExplicitTop = 77
          object Label4: TLabel
            AlignWithMargins = True
            Left = 20
            Top = 0
            Width = 404
            Height = 60
            Margins.Left = 20
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alBottom
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
          Width = 424
          Height = 17
          Margins.Left = 0
          Margins.Top = 10
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Recursive Lookup'
          TabOrder = 2
          ExplicitTop = 59
        end
        object CheckBoxDeepScan: TCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 135
          Width = 424
          Height = 17
          Margins.Left = 0
          Margins.Top = 10
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Deep Scan'
          TabOrder = 3
          ExplicitTop = 86
        end
        object ButtonedEdit1: TButtonedEdit
          Left = 0
          Top = 75
          Width = 424
          Height = 23
          Align = alTop
          LeftButton.ImageName = 'folder-filled-new'
          RightButton.Enabled = False
          RightButton.ImageIndex = 10
          RightButton.ImageName = 'folder-filled'
          RightButton.Visible = True
          TabOrder = 4
          OnRightButtonClick = EditDirectoryRightButtonClick
          ExplicitLeft = -2
          ExplicitTop = 72
        end
      end
      object PanelIcon: TPanel
        Left = 0
        Top = 0
        Width = 67
        Height = 223
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        ExplicitWidth = 53
        ExplicitHeight = 137
        object ImageIcon: TVirtualImage
          AlignWithMargins = True
          Left = 4
          Top = 8
          Width = 59
          Height = 64
          Margins.Left = 4
          Margins.Top = 8
          Margins.Right = 4
          Margins.Bottom = 8
          Align = alTop
          Center = True
          ImageCollection = FormMain.ImageCollection
          ImageWidth = 0
          ImageHeight = 0
          ImageIndex = 9
          ImageName = 'folder-open-filled-fingerprint'
          ExplicitLeft = -24
          ExplicitTop = 0
          ExplicitWidth = 67
        end
      end
    end
  end
end
