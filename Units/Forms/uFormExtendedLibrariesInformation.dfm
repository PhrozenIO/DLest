object FormExtendedLibrariesInformation: TFormExtendedLibrariesInformation
  Left = 0
  Top = 0
  Caption = 'Extended Libraries Informations'
  ClientHeight = 228
  ClientWidth = 489
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 481
    Height = 220
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
    Header.AutoSizeIndex = -1
    Header.MainColumn = 1
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.PopupMenu = PopupMenu
    PopupMenu = PopupMenu
    StateImages = FormMain.ImageSystem
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
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
    ExplicitLeft = -1
    ExplicitTop = -1
    Columns = <
      item
        Position = 0
        Text = 'File Name'
        Width = 100
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
        Text = 'MD5'
        Width = 250
      end
      item
        Position = 6
        Text = 'SHA1'
        Width = 300
      end
      item
        Position = 7
        Text = 'SHA2'
        Width = 450
      end
      item
        Position = 8
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
    object N3: TMenuItem
      Caption = '-'
    end
    object OpenSelectedFilesonanewTab1: TMenuItem
      Caption = 'Open Selected Files In A New Tab'
      ShortCut = 16468
      OnClick = OpenSelectedFilesonanewTab1Click
    end
  end
end
