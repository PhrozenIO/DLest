object FormProcessMonitorOptions: TFormProcessMonitorOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Process Spy (Debug Process)'
  ClientHeight = 319
  ClientWidth = 394
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 15
  object Label1: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 378
    Height = 15
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'Target Application '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 104
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 57
    Width = 378
    Height = 15
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Align = alTop
    Caption = 'Optional Argument(s)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 120
  end
  object EditApplication: TButtonedEdit
    AlignWithMargins = True
    Left = 8
    Top = 26
    Width = 378
    Height = 23
    Margins.Left = 8
    Margins.Top = 0
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alTop
    Images = FormMain.VirtualImageList
    LeftButton.ImageIndex = 2
    LeftButton.ImageName = 'icons8-folder'
    RightButton.ImageIndex = 2
    RightButton.ImageName = 'icons8-folder'
    RightButton.Visible = True
    TabOrder = 0
    OnRightButtonClick = EditApplicationRightButtonClick
    ExplicitWidth = 368
  end
  object GroupBoxExtraOptions: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 106
    Width = 378
    Height = 79
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Options'
    TabOrder = 1
    ExplicitWidth = 368
    object CheckBoxScanSubProcess: TCheckBox
      AlignWithMargins = True
      Left = 10
      Top = 54
      Width = 358
      Height = 17
      Margins.Left = 8
      Margins.Top = 10
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Monitor child processes.'
      Checked = True
      State = cbChecked
      TabOrder = 0
      ExplicitWidth = 348
    end
    object CheckShowProcess: TCheckBox
      AlignWithMargins = True
      Left = 10
      Top = 27
      Width = 358
      Height = 17
      Margins.Left = 8
      Margins.Top = 10
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Show process.'
      Checked = True
      State = cbChecked
      TabOrder = 1
      ExplicitWidth = 348
    end
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 195
    Width = 378
    Height = 74
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 163
    ExplicitWidth = 368
    object Label3: TLabel
      AlignWithMargins = True
      Left = 32
      Top = 4
      Width = 342
      Height = 66
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Caption = 
        'This feature executes a specified application. To avoid the risk' +
        ' of running potentially malicious software, ensure that you use ' +
        'this feature only with trusted binaries or within a virtual mach' +
        'ine or sandbox environment.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clDimgray
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 339
      ExplicitHeight = 60
    end
    object VirtualImage1: TVirtualImage
      Left = 0
      Top = 0
      Width = 24
      Height = 74
      Align = alLeft
      ImageCollection = FormMain.ImageCollection
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 17
      ImageName = 'icons8-warning'
      ExplicitLeft = -3
      ExplicitHeight = 42
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 277
    Width = 394
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhitesmoke
    ParentBackground = False
    TabOrder = 3
    ExplicitTop = 245
    ExplicitWidth = 384
    object ButtonValidate: TSpeedButton
      AlignWithMargins = True
      Left = 300
      Top = 8
      Width = 90
      Height = 26
      Margins.Left = 2
      Margins.Top = 8
      Margins.Right = 4
      Margins.Bottom = 8
      Align = alRight
      Caption = 'Run'
      ImageIndex = 17
      ImageName = 'icons8-warning'
      Images = FormMain.VirtualImageList
      OnClick = ButtonValidateClick
      ExplicitLeft = 321
      ExplicitHeight = 25
    end
    object ButtonCancel: TSpeedButton
      AlignWithMargins = True
      Left = 206
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
  object EditArguments: TButtonedEdit
    AlignWithMargins = True
    Left = 8
    Top = 75
    Width = 378
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
    TabOrder = 4
    ExplicitWidth = 368
  end
  object OpenDialog: TOpenDialog
    Filter = 'Application|*.exe;*.com;*.scr'
    Options = [ofReadOnly, ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofForceShowHidden]
    Left = 272
    Top = 152
  end
end
