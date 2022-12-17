object FormProcessList: TFormProcessList
  Left = 0
  Top = 0
  Caption = 'Process List'
  ClientHeight = 426
  ClientWidth = 711
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object MultiPanel: TOMultiPanel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 703
    Height = 369
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    PanelType = ptVertical
    PanelCollection = <
      item
        Control = PanelProcess
        Position = 0.500000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = PanelModules
        Position = 1.000000000000000000
        Visible = True
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 689
    ExplicitHeight = 336
    DesignSize = (
      703
      369)
    object PanelProcess: TPanel
      Left = 0
      Top = 0
      Width = 703
      Height = 184
      Anchors = []
      BevelOuter = bvNone
      Color = 16448250
      ParentBackground = False
      TabOrder = 0
      object VSTProcess: TVirtualStringTree
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 699
        Height = 180
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        AccessibleName = 'Module Base'
        Align = alClient
        BorderStyle = bsNone
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
        Header.AutoSizeIndex = 0
        Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
        Images = FormMain.VirtualImageList
        PopupMenu = PopupProcess
        StateImages = FormMain.ImageSystem
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toRightClickSelect, toRestoreSelection]
        OnChange = VSTProcessChange
        OnCompareNodes = VSTProcessCompareNodes
        OnFocusChanged = VSTProcessFocusChanged
        OnGetText = VSTProcessGetText
        OnGetImageIndex = VSTProcessGetImageIndex
        OnGetNodeDataSize = VSTProcessGetNodeDataSize
        OnNodeClick = VSTProcessNodeClick
        OnNodeDblClick = VSTProcessNodeDblClick
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Position = 0
            Text = 'Process Name'
            Width = 120
          end
          item
            Position = 1
            Text = 'Process Id'
            Width = 140
          end
          item
            Position = 2
            Text = 'Image Path'
            Width = 300
          end>
      end
    end
    object PanelModules: TPanel
      Left = 0
      Top = 187
      Width = 703
      Height = 182
      Anchors = []
      BevelOuter = bvNone
      Color = 16448250
      ParentBackground = False
      TabOrder = 1
      object VSTModules: TVirtualStringTree
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 699
        Height = 178
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        AccessibleName = 'Module Base'
        Align = alClient
        BorderStyle = bsNone
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
        Header.AutoSizeIndex = -1
        Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
        Images = FormMain.VirtualImageList
        PopupMenu = PopupModules
        StateImages = FormMain.ImageSystem
        TabOrder = 0
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toMultiSelect, toRightClickSelect, toRestoreSelection]
        OnBeforeCellPaint = VSTModulesBeforeCellPaint
        OnChange = VSTModulesChange
        OnCompareNodes = VSTModulesCompareNodes
        OnFocusChanged = VSTModulesFocusChanged
        OnGetText = VSTModulesGetText
        OnGetImageIndex = VSTModulesGetImageIndex
        OnGetNodeDataSize = VSTModulesGetNodeDataSize
        OnNodeDblClick = VSTModulesNodeDblClick
        Touch.InteractiveGestures = [igPan, igPressAndTap]
        Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Columns = <
          item
            Position = 0
            Text = 'Module Name'
            Width = 160
          end
          item
            Position = 1
            Text = 'Process Id'
            Width = 120
          end
          item
            Position = 2
            Text = 'Module Base'
            Width = 90
          end
          item
            Position = 3
            Text = 'Module Base Size'
            Width = 110
          end
          item
            Position = 4
            Text = 'Image Path'
            Width = 200
          end>
      end
    end
  end
  object PanelMessage: TPanel
    AlignWithMargins = True
    Left = 4
    Top = 381
    Width = 703
    Height = 41
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 348
    ExplicitWidth = 689
    object VirtualImage1: TVirtualImage
      Left = 0
      Top = 0
      Width = 17
      Height = 41
      Align = alLeft
      ImageCollection = FormMain.ImageCollection
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 5
      ImageName = 'warning'
    end
    object LabelMessage: TLabel
      AlignWithMargins = True
      Left = 25
      Top = 0
      Width = 678
      Height = 41
      Margins.Left = 8
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alClient
      BiDiMode = bdLeftToRight
      Caption = 
        'The processes listed in this window are compatible with the curr' +
        'ent process in terms of both memory architecture and elevated st' +
        'atus. Incompatibilities in either of these factors will exclude ' +
        'a process from being listed.'
      ParentBiDiMode = False
      WordWrap = True
      ExplicitWidth = 671
      ExplicitHeight = 30
    end
  end
  object PopupProcess: TPopupMenu
    OnPopup = PopupProcessPopup
    Left = 560
    Top = 40
    object OpenProcess1: TMenuItem
      Caption = 'Open Process'
      OnClick = OpenProcess1Click
    end
  end
  object PopupModules: TPopupMenu
    OnPopup = PopupModulesPopup
    Left = 556
    Top = 231
    object OpenSelectedModules1: TMenuItem
      Caption = 'Open Selected Memory Mapped Module(s)'
      OnClick = OpenSelectedModules1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object OpenSelectedModulesFromFiles1: TMenuItem
      Caption = 'Open Selected Module(s) From File(s)'
      OnClick = OpenSelectedModulesFromFiles1Click
    end
  end
end
