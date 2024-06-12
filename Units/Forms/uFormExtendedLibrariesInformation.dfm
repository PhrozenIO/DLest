object FormExtendedLibrariesInformation: TFormExtendedLibrariesInformation
  Left = 0
  Top = 0
  Caption = 'Extended Libraries Informations'
  ClientHeight = 337
  ClientWidth = 746
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
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 738
    Height = 329
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
    Header.AutoSizeIndex = -1
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.PopupMenu = PopupMenu
    PopupMenu = PopupMenu
    StateImages = FormMain.ImageSystem
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toMultiSelect, toRightClickSelect, toRestoreSelection]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    ExplicitHeight = 277
    Columns = <
      item
        Position = 0
        Text = 'File Name'
        Width = 250
      end
      item
        Position = 1
        Text = 'Export Count'
        Width = 90
      end
      item
        Position = 2
        Text = 'File Size'
        Width = 100
      end
      item
        Position = 3
        Text = 'Company Name'
        Width = 120
      end
      item
        Position = 4
        Text = 'File Version'
        Width = 90
      end
      item
        Position = 5
        Text = 'Location'
        Width = 250
      end>
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 168
    Top = 56
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = SelectAll1Click
    end
    object ClearSelection1: TMenuItem
      Caption = 'Clear Selection'
      OnClick = ClearSelection1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SelectedFileProperties1: TMenuItem
      Caption = 'Selected File Properties'
      ShortCut = 16464
      OnClick = SelectedFileProperties1Click
    end
    object ShowSelectedFileOnExplorer1: TMenuItem
      Caption = 'Show Selected File On Explorer'
      ShortCut = 16453
      OnClick = ShowSelectedFileOnExplorer1Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object CalculateSelectedImageFileHashes1: TMenuItem
      Caption = 'Calculate Selected Image File Hashes'
      OnClick = CalculateSelectedImageFileHashes1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object OpenSelectedFilesonanewTab1: TMenuItem
      Caption = 'Open Selected Files In A New Tab'
      ShortCut = 16468
      OnClick = OpenSelectedFilesonanewTab1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object ShowListasTree1: TMenuItem
      AutoCheck = True
      Caption = 'Show List as Tree'
      Checked = True
      OnClick = ShowListasTree1Click
    end
  end
end
