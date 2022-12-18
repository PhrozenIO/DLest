object FormTask: TFormTask
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Task In Progress'
  ClientHeight = 163
  ClientWidth = 448
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 15
  object PanelIcon: TPanel
    Left = 0
    Top = 0
    Width = 64
    Height = 121
    Align = alLeft
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitHeight = 88
    object VirtualImage1: TVirtualImage
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 48
      Height = 64
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      ImageCollection = FormMain.ImageCollection
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 8
      ImageName = 'test-filled-gear-filled'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 64
    end
  end
  object PanelBody: TPanel
    Left = 64
    Top = 0
    Width = 384
    Height = 121
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 370
    ExplicitHeight = 88
    object Label1: TLabel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 368
      Height = 60
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Caption = 
        'The action is currently underway and is expected to take a while' +
        ' to complete. Please be patient as we work to finish it. If you ' +
        'need to cancel the action at any time, you can do so by clicking' +
        ' the '#39'Cancel'#39' button.'
      WordWrap = True
      ExplicitWidth = 360
    end
    object ProgressBar: TProgressBar
      AlignWithMargins = True
      Left = 8
      Top = 84
      Width = 368
      Height = 15
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      DoubleBuffered = False
      ParentDoubleBuffered = False
      Style = pbstMarquee
      TabOrder = 0
      ExplicitWidth = 354
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 121
    Width = 448
    Height = 42
    Margins.Top = 8
    Align = alBottom
    BevelOuter = bvNone
    Color = 16119285
    ParentBackground = False
    TabOrder = 2
    ExplicitTop = 88
    ExplicitWidth = 434
    object ButtonCancel: TButton
      AlignWithMargins = True
      Left = 365
      Top = 8
      Width = 75
      Height = 26
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = ButtonCancelClick
      ExplicitLeft = 351
    end
  end
end
