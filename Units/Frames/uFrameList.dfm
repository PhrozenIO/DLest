object FrameList: TFrameList
  Left = 0
  Top = 0
  Width = 1191
  Height = 938
  Align = alClient
  Color = 16448250
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object PanelSearch: TPanel
    AlignWithMargins = True
    Left = 1
    Top = 918
    Width = 1189
    Height = 19
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alBottom
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 0
    object ButtonSearch: TSpeedButton
      Left = 1161
      Top = 0
      Width = 28
      Height = 19
      Align = alRight
      ImageIndex = 13
      ImageName = 'icons8-search'
      Images = FormMain.VirtualImageList
      OnClick = ButtonSearchClick
      ExplicitLeft = 840
    end
    object EditRegex: TButtonedEdit
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 1159
      Height = 17
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alClient
      BorderStyle = bsNone
      Images = FormMain.VirtualImageList
      LeftButton.ImageName = 'symbol-cancel'
      RightButton.ImageIndex = 20
      RightButton.ImageName = 'icons8-cancel'
      TabOrder = 0
      OnChange = EditRegexChange
      OnKeyDown = EditRegexKeyDown
      OnRightButtonClick = EditRegexRightButtonClick
    end
  end
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 1191
    Height = 895
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
    DefaultNodeHeight = 19
    Header.AutoSizeIndex = 0
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 3
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    StateImages = FormMain.ImageSystem
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
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
        Text = 'Export Type'
        Width = 150
      end
      item
        Position = 5
        Text = 'DLL Image Path'
        Width = 300
      end>
  end
  object PanelProgressTask: TPanel
    AlignWithMargins = True
    Left = 1
    Top = 896
    Width = 1189
    Height = 20
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alBottom
    BevelOuter = bvNone
    Color = 16448250
    ParentBackground = False
    TabOrder = 2
    object ButtonCancelTask: TSpeedButton
      Left = 1161
      Top = 0
      Width = 28
      Height = 20
      Align = alRight
      ImageIndex = 20
      ImageName = 'icons8-cancel'
      Images = FormMain.VirtualImageList
      OnClick = ButtonCancelTaskClick
      ExplicitLeft = 840
      ExplicitHeight = 19
    end
    object ProgressBarTask: TProgressBar
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 1153
      Height = 12
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Smooth = True
      MarqueeInterval = 6
      TabOrder = 0
    end
  end
  object PopupMenu: TPopupMenu
    Images = FormMain.VirtualImageList
    OnPopup = PopupMenuPopup
    Left = 440
    Top = 192
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = SelectAll1Click
    end
    object ClearSelection1: TMenuItem
      Caption = 'Clear Selection'
      OnClick = ClearSelection1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
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
    object Filter1: TMenuItem
      Caption = 'Filter'
      ImageIndex = 13
      ImageName = 'icons8-search'
      object ExportFunctions1: TMenuItem
        AutoCheck = True
        Caption = 'Export Functions'
        OnClick = FilterMenuItemClick
      end
      object ForwardedFunctions1: TMenuItem
        AutoCheck = True
        Caption = 'Forwarded Functions'
        OnClick = FilterMenuItemClick
      end
      object ExportFunctions2: TMenuItem
        Caption = '-'
      end
      object COMMethod1: TMenuItem
        AutoCheck = True
        Caption = 'COM Methods'
        OnClick = FilterMenuItemClick
      end
      object COMProperties1: TMenuItem
        AutoCheck = True
        Caption = 'COM Properties'
        OnClick = FilterMenuItemClick
      end
      object COMUnknown1: TMenuItem
        AutoCheck = True
        Caption = 'COM Unknown'
        OnClick = FilterMenuItemClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object ResetClear1: TMenuItem
        Caption = 'Reset Filter'
        OnClick = ResetClear1Click
      end
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object Google1: TMenuItem
      Caption = 'Google Search'
      object SearchLibraryName1: TMenuItem
        Caption = 'Search Selected Library Name'
        OnClick = SearchLibraryName1Click
      end
      object SearchAPIName1: TMenuItem
        Caption = 'Search Selected API Name'
        OnClick = SearchAPIName1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object SearchBoth1: TMenuItem
        Caption = 'Search Both (Selected)'
        OnClick = SearchBoth1Click
      end
    end
    object UnprotectSearch1: TMenuItem
      Caption = 'Unprotect Search'
      object SearchUnprotectSelectedLibraryName1: TMenuItem
        Caption = 'Search Selected Library Name'
        OnClick = SearchUnprotectSelectedLibraryName1Click
      end
      object SearchUnprotectSelectedAPIName1: TMenuItem
        Caption = 'Search Selected API Name'
        OnClick = SearchUnprotectSelectedAPIName1Click
      end
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
    end
    object N5: TMenuItem
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
    object N9: TMenuItem
      Caption = '-'
    end
    object CalculateSelectedLibrariesHashes1: TMenuItem
      Caption = 'Calculate Selected Library Hashes'
      OnClick = CalculateSelectedLibrariesHashes1Click
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
    object N7: TMenuItem
      Caption = '-'
    end
    object LibrariesViewExtendedInfo1: TMenuItem
      Caption = 'Libraries View (Extended Info)'
      ImageName = 'file-dll-filled-fingerprint'
      OnClick = LibrariesViewExtendedInfo1Click
    end
  end
end
