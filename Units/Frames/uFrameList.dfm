object FrameList: TFrameList
  Left = 0
  Top = 0
  Width = 1173
  Height = 766
  Align = alClient
  Color = clWhite
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object MultiPanel: TOMultiPanel
    Left = 0
    Top = 0
    Width = 1173
    Height = 766
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    PanelType = ptVertical
    PanelCollection = <
      item
        Control = VST
        Position = 0.700000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = PanelDetail
        Position = 1.000000000000000000
        Visible = False
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    DesignSize = (
      1173
      766)
    object VST: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 1173
      Height = 536
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
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
      PopupMenu = PopupMenu
      StateImages = FormMain.ImageSystem
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toMultiSelect, toRightClickSelect, toRestoreSelection]
      OnBeforeCellPaint = VSTBeforeCellPaint
      OnChange = VSTChange
      OnCompareNodes = VSTCompareNodes
      OnFocusChanged = VSTFocusChanged
      OnFreeNode = VSTFreeNode
      OnGetText = VSTGetText
      OnGetImageIndex = VSTGetImageIndex
      OnGetNodeDataSize = VSTGetNodeDataSize
      Touch.InteractiveGestures = [igPan, igPressAndTap]
      Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Columns = <
        item
          Position = 0
          Text = 'Function Name'
          Width = 250
        end
        item
          Position = 1
          Text = 'Address'
          Width = 120
        end
        item
          Position = 2
          Text = 'Relative Address'
          Width = 120
        end
        item
          Position = 3
          Text = 'Ordinal'
          Width = 70
        end
        item
          Position = 4
          Text = 'DLL Image Path'
          Width = 300
        end>
    end
    object PanelDetail: TPanel
      Left = 0
      Top = 539
      Width = 1173
      Height = 227
      Anchors = []
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      Visible = False
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 440
    Top = 192
    object ExpandAll1: TMenuItem
      Caption = 'Expand All'
      OnClick = ExpandAll1Click
    end
    object CollapseAll1: TMenuItem
      Caption = 'Collapse All'
      OnClick = CollapseAll1Click
    end
  end
end
