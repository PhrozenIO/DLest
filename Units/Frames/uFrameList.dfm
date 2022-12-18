object FrameList: TFrameList
  Left = 0
  Top = 0
  Width = 870
  Height = 532
  Align = alClient
  Color = 16448250
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object ProgressBar: TProgressBar
    AlignWithMargins = True
    Left = 4
    Top = 497
    Width = 862
    Height = 10
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    Smooth = True
    MarqueeInterval = 6
    TabOrder = 0
    Visible = False
  end
  object PanelSearch: TPanel
    AlignWithMargins = True
    Left = 1
    Top = 512
    Width = 868
    Height = 19
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alBottom
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 1
    object EditRegex: TButtonedEdit
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 866
      Height = 17
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alClient
      BorderStyle = bsNone
      Images = FormMain.VirtualImageList
      LeftButton.ImageIndex = 11
      LeftButton.ImageName = 'symbol-cancel'
      RightButton.ImageIndex = 11
      RightButton.ImageName = 'symbol-cancel'
      TabOrder = 0
      OnChange = EditRegexChange
      OnRightButtonClick = EditRegexRightButtonClick
    end
  end
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 870
    Height = 493
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
    TabOrder = 2
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
        Width = 140
      end
      item
        Position = 2
        Text = 'Relative Address'
        Width = 140
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
    object N1: TMenuItem
      Caption = '-'
    end
    object ShowSelectedFileProperties1: TMenuItem
      Caption = 'Show Selected File Properties'
      OnClick = ShowSelectedFileProperties1Click
    end
    object ShowSelectedFileOnExplorer1: TMenuItem
      Caption = 'Show Selected File On Explorer'
      OnClick = ShowSelectedFileOnExplorer1Click
    end
    object LoadSelectedFileinNewTab1: TMenuItem
      Caption = 'Load Selected File In New Tab'
      OnClick = LoadSelectedFileinNewTab1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object ExportEntireListToJson1: TMenuItem
      Caption = 'Export Entire List To Json'
      ShortCut = 16467
      OnClick = ExportEntireListToJson1Click
    end
    object ExportVisibleFilteredItemsToJson1: TMenuItem
      Caption = 'Export Visible (Filtered) Items To Json'
      OnClick = ExportVisibleFilteredItemsToJson1Click
    end
    object ExportSelectedItemsToJson1: TMenuItem
      Caption = 'Export Selected Items To Json'
      OnClick = ExportSelectedItemsToJson1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object RenameTab1: TMenuItem
      Caption = 'Rename Tab'
      ShortCut = 16466
      OnClick = RenameTab1Click
    end
    object CloseTab1: TMenuItem
      Caption = 'Close Tab'
      ShortCut = 16472
      OnClick = CloseTab1Click
    end
  end
end
