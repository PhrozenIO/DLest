object FormProcessMonitor: TFormProcessMonitor
  Left = 0
  Top = 0
  Caption = 'Process Spy Event Viewer'
  ClientHeight = 395
  ClientWidth = 615
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 353
    Width = 615
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhitesmoke
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 321
    ExplicitWidth = 605
    object ButtonContinue: TSpeedButton
      Left = 289
      Top = 8
      Width = 90
      Height = 25
      Margins.Left = 2
      Margins.Top = 8
      Margins.Right = 4
      Margins.Bottom = 8
      Caption = 'Continue'
      ImageIndex = 25
      ImageName = 'icons8-double-right'
      Images = FormMain.VirtualImageList
      Enabled = False
      OnClick = ButtonContinueClick
    end
    object ButtonPlay: TSpeedButton
      Left = 175
      Top = 8
      Width = 90
      Height = 25
      Margins.Top = 8
      Margins.Right = 2
      Margins.Bottom = 8
      Caption = 'Play'
      ImageIndex = 26
      ImageName = 'icons8-play'
      Images = FormMain.VirtualImageList
      Enabled = False
      OnClick = ButtonPlayClick
    end
    object ButtonStop: TSpeedButton
      Left = 80
      Top = 8
      Width = 90
      Height = 25
      Margins.Top = 8
      Margins.Right = 2
      Margins.Bottom = 8
      Caption = 'Stop'
      ImageIndex = 27
      ImageName = 'icons8-stop'
      Images = FormMain.VirtualImageList
      OnClick = ButtonStopClick
    end
  end
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 607
    Height = 345
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AccessibleName = 'Image Path'
    Align = alClient
    Colors.BorderColor = 15987699
    Colors.DisabledColor = clGray
    Colors.DropMarkColor = 15385233
    Colors.DropTargetColor = 15385233
    Colors.DropTargetBorderColor = 15385233
    Colors.FocusedSelectionColor = 15385233
    Colors.FocusedSelectionBorderColor = 15385233
    Colors.GridLineColor = 15987699
    Colors.HeaderHotColor = clBlack
    Colors.HotColor = clBlack
    Colors.SelectionRectangleBlendColor = 15385233
    Colors.SelectionRectangleBorderColor = 15385233
    Colors.SelectionTextColor = clBlack
    Colors.TreeLineColor = 9471874
    Colors.UnfocusedColor = clGray
    Colors.UnfocusedSelectionColor = clWhite
    Colors.UnfocusedSelectionBorderColor = clWhite
    DefaultNodeHeight = 19
    Header.AutoSizeIndex = 2
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    PopupMenu = PopupMenu
    StateImages = FormMain.ImageSystem
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toMultiSelect, toRightClickSelect, toRestoreSelection]
    OnChange = VSTChange
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitLeft = -1
    Columns = <
      item
        Position = 0
        Text = 'Image Name'
        Width = 200
      end
      item
        Position = 1
        Text = 'PID / Image Base'
        Width = 150
      end
      item
        Position = 2
        Text = 'Image Path'
        Width = 267
      end>
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 440
    Top = 136
    object CalculateSelectedLibraryHashes1: TMenuItem
      Caption = 'Calculate Selected Library Hashes'
      OnClick = CalculateSelectedLibraryHashes1Click
    end
  end
end
