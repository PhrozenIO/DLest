object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'About DLest'
  ClientHeight = 308
  ClientWidth = 388
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object Logo: TVirtualImage
    AlignWithMargins = True
    Left = 16
    Top = 16
    Width = 356
    Height = 100
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 16
    Align = alTop
    Center = True
    ImageCollection = FormMain.ImageCollection
    ImageWidth = 0
    ImageHeight = 0
    ImageIndex = 15
    ImageName = 'icon'
    ExplicitLeft = 152
    ExplicitTop = 8
    ExplicitWidth = 100
  end
  object LabelName: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 135
    Width = 382
    Height = 25
    Align = alTop
    Alignment = taCenter
    Caption = 'N/A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 36
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 166
    Width = 382
    Height = 15
    Margins.Bottom = 0
    Align = alTop
    Alignment = taCenter
    Caption = 'Coded by Jean-Pierre LESUEUR'
    ExplicitWidth = 162
  end
  object Label3: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 181
    Width = 382
    Height = 15
    Cursor = crHandPoint
    Margins.Top = 0
    Align = alTop
    Alignment = taCenter
    Caption = '@DarkCodersc'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label3Click
    ExplicitWidth = 78
  end
  object Label4: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 202
    Width = 382
    Height = 15
    Cursor = crHandPoint
    Margins.Bottom = 0
    Align = alTop
    Alignment = taCenter
    Caption = 'www.phrozen.io'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label4Click
    ExplicitWidth = 86
  end
  object Label5: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 235
    Width = 382
    Height = 15
    Cursor = crHandPoint
    Margins.Top = 0
    Align = alTop
    Alignment = taCenter
    Caption = 'www.github.com/darkcodersc'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label5Click
    ExplicitTop = 217
    ExplicitWidth = 161
  end
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 220
    Width = 382
    Height = 15
    Cursor = crHandPoint
    Margins.Bottom = 0
    Align = alTop
    Alignment = taCenter
    Caption = 'www.unprotect.it'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label1Click
    ExplicitLeft = 8
    ExplicitWidth = 377
  end
  object ButtonClose: TButton
    AlignWithMargins = True
    Left = 128
    Top = 275
    Width = 132
    Height = 25
    Margins.Left = 128
    Margins.Top = 8
    Margins.Right = 128
    Margins.Bottom = 8
    Align = alBottom
    Caption = 'Close'
    TabOrder = 0
    OnClick = ButtonCloseClick
    ExplicitTop = 229
    ExplicitWidth = 113
  end
end
