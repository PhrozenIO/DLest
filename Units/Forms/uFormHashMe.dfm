object FormHashMe: TFormHashMe
  Left = 0
  Top = 0
  Caption = 'File Hash Tool'
  ClientHeight = 370
  ClientWidth = 688
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 680
    Height = 343
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AccessibleName = 'Module Base'
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
    Header.AutoSizeIndex = 0
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 0
    Images = FormMain.VirtualImageList
    PopupMenu = PopupMenu
    StateImages = FormMain.ImageSystem
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toRightClickSelect, toRestoreSelection]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'File Name'
        Width = 150
      end
      item
        Position = 1
        Text = 'MD5'
        Width = 200
      end
      item
        Position = 2
        Text = 'SHA1'
        Width = 200
      end
      item
        Position = 3
        Text = 'SHA224'
        Width = 200
      end
      item
        Position = 4
        Text = 'SHA256'
        Width = 200
      end
      item
        Position = 5
        Text = 'SHA384'
        Width = 200
      end
      item
        Position = 6
        Text = 'SHA512'
        Width = 200
      end
      item
        Alignment = taCenter
        Position = 7
        Text = 'Have Doublon'
        Width = 120
      end
      item
        Position = 8
        Text = 'File Path'
        Width = 250
      end>
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 351
    Width = 688
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 230
      end>
    ExplicitTop = 319
    ExplicitWidth = 678
  end
  object MainMenu: TMainMenu
    Left = 128
    Top = 72
    object File1: TMenuItem
      Caption = 'File'
      object AddFiles1: TMenuItem
        Caption = 'Add File(s)'
        OnClick = AddFiles1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Clear1: TMenuItem
        Caption = 'Clear'
        OnClick = Clear1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofReadOnly, ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofForceShowHidden]
    Left = 272
    Top = 152
  end
  object PopupMenu: TPopupMenu
    Left = 408
    Top = 128
    object Copy1: TMenuItem
      Caption = 'Copy'
    end
  end
end
