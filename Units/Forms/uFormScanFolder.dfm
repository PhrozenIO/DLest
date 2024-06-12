object FormScanFolder: TFormScanFolder
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Scan Folder'
  ClientHeight = 287
  ClientWidth = 492
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
  OnShow = FormShow
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 245
    Width = 492
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhitesmoke
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 277
    ExplicitWidth = 482
    object ButtonValidate: TSpeedButton
      AlignWithMargins = True
      Left = 398
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
      Left = 304
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
  object PanelForm: TPanel
    Left = 0
    Top = 0
    Width = 492
    Height = 245
    Margins.Left = 8
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 1
    ExplicitLeft = -5
    ExplicitHeight = 352
    object Label1: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 476
      Height = 15
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
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
      Left = 8
      Top = 71
      Width = 476
      Height = 15
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 8
      Align = alTop
      Caption = 'Advanced Export Filter:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 130
    end
    object Shape1: TShape
      AlignWithMargins = True
      Left = 8
      Top = 57
      Width = 476
      Height = 2
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Pen.Color = 16448250
      ExplicitTop = 44
      ExplicitWidth = 468
    end
    object EditRegex: TButtonedEdit
      AlignWithMargins = True
      Left = 8
      Top = 89
      Width = 476
      Height = 23
      Margins.Left = 8
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      LeftButton.ImageName = 'folder-filled-new'
      RightButton.Enabled = False
      RightButton.ImageIndex = 10
      RightButton.ImageName = 'folder-filled'
      RightButton.Visible = True
      TabOrder = 0
      OnRightButtonClick = EditDirectoryRightButtonClick
      ExplicitWidth = 466
    end
    object GroupBoxExtraOptions: TGroupBox
      AlignWithMargins = True
      Left = 8
      Top = 172
      Width = 476
      Height = 61
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Advanced Options'
      TabOrder = 1
      object CheckBoxRecursive: TCheckBox
        AlignWithMargins = True
        Left = 10
        Top = 27
        Width = 456
        Height = 17
        Margins.Left = 8
        Margins.Top = 10
        Margins.Right = 8
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Recursive Lookup'
        TabOrder = 0
        ExplicitWidth = 446
      end
    end
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 122
      Width = 476
      Height = 42
      Margins.Left = 8
      Margins.Top = 10
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      ExplicitWidth = 466
      object Label3: TLabel
        AlignWithMargins = True
        Left = 32
        Top = 4
        Width = 440
        Height = 34
        Margins.Left = 8
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        Caption = 
          'The "Advanced Export Filter" will only list libraries that conta' +
          'in export names matching the search query. The search engine sup' +
          'ports regular expressions.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clDimgray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        ExplicitWidth = 405
        ExplicitHeight = 30
      end
      object VirtualImage1: TVirtualImage
        Left = 0
        Top = 0
        Width = 24
        Height = 42
        Align = alLeft
        ImageCollection = FormMain.ImageCollection
        ImageWidth = 0
        ImageHeight = 0
        ImageIndex = 7
        ImageName = 'icons8-about'
        ExplicitLeft = -3
        ExplicitTop = -3
        ExplicitHeight = 60
      end
    end
    object PanelPath: TPanel
      Left = 0
      Top = 26
      Width = 492
      Height = 23
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 3
      ExplicitWidth = 482
      object EditDirectory: TButtonedEdit
        AlignWithMargins = True
        Left = 8
        Top = 0
        Width = 428
        Height = 23
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alClient
        Images = FormMain.VirtualImageList
        LeftButton.ImageIndex = 2
        LeftButton.ImageName = 'icons8-folder'
        RightButton.ImageIndex = 2
        RightButton.ImageName = 'icons8-folder'
        RightButton.Visible = True
        TabOrder = 0
        OnRightButtonClick = EditDirectoryRightButtonClick
        ExplicitWidth = 418
      end
      object EditFileFilter: TButtonedEdit
        AlignWithMargins = True
        Left = 440
        Top = 0
        Width = 44
        Height = 23
        Margins.Left = 2
        Margins.Top = 0
        Margins.Right = 8
        Margins.Bottom = 0
        Align = alRight
        Alignment = taCenter
        LeftButton.ImageName = 'folder-filled-new'
        RightButton.Enabled = False
        RightButton.ImageIndex = 10
        RightButton.ImageName = 'folder-filled'
        RightButton.Visible = True
        TabOrder = 1
        Text = '*.dll'
        OnRightButtonClick = EditDirectoryRightButtonClick
        ExplicitLeft = 430
      end
    end
  end
end
