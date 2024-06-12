object FormProcessList: TFormProcessList
  Left = 0
  Top = 0
  Caption = 'Process List'
  ClientHeight = 393
  ClientWidth = 697
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object MultiPanel: TOMultiPanel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 689
    Height = 332
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
    DesignSize = (
      689
      332)
    object PanelProcess: TPanel
      Left = 0
      Top = 0
      Width = 689
      Height = 166
      Anchors = []
      BevelOuter = bvNone
      Color = 16448250
      ParentBackground = False
      TabOrder = 0
      object VSTProcess: TVirtualStringTree
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 685
        Height = 162
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
        DefaultNodeHeight = 19
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
      Top = 169
      Width = 689
      Height = 163
      Anchors = []
      BevelOuter = bvNone
      Color = 16448250
      ParentBackground = False
      TabOrder = 1
      object VSTModules: TVirtualStringTree
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 685
        Height = 159
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
        DefaultNodeHeight = 19
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
    Top = 344
    Width = 689
    Height = 41
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 8
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 312
    ExplicitWidth = 679
    object VirtualImage1: TVirtualImage
      AlignWithMargins = True
      Left = 8
      Top = 0
      Width = 24
      Height = 41
      Margins.Left = 8
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alLeft
      ImageCollection = FormMain.ImageCollection
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 17
      ImageName = 'icons8-warning'
      ExplicitLeft = 0
    end
    object LabelMessage: TLabel
      AlignWithMargins = True
      Left = 40
      Top = 4
      Width = 624
      Height = 30
      Margins.Left = 8
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      BiDiMode = bdLeftToRight
      Caption = 
        'The processes listed in this window are compatible with the curr' +
        'ent process in terms of both memory architecture and elevated st' +
        'atus. Incompatibilities in either of these factors will exclude ' +
        'a process from being listed.'
      ParentBiDiMode = False
      WordWrap = True
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
    object N4: TMenuItem
      Caption = '-'
    end
    object CalculateSelectedImageFileHashes1: TMenuItem
      Caption = 'Calculate Selected Image File Hashes'
      OnClick = CalculateSelectedImageFileHashes1Click
    end
  end
  object PopupModules: TPopupMenu
    OnPopup = PopupModulesPopup
    Left = 556
    Top = 247
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = SelectAll1Click
    end
    object DeselectAll1: TMenuItem
      Caption = 'Clear Selection'
      OnClick = DeselectAll1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
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
    object N2: TMenuItem
      Caption = '-'
    end
    object CalculateSelectedImageFileHashes2: TMenuItem
      Caption = 'Calculate Selected Image File Hashes'
      OnClick = CalculateSelectedImageFileHashes2Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object DumpReconstructedImageofSelectedModules1: TMenuItem
      Caption = 'Dump Reconstructed Image of Selected Module(s)'
      OnClick = DumpReconstructedImageofSelectedModules1Click
    end
  end
  object MainMenu: TMainMenu
    Left = 212
    Top = 116
    object File1: TMenuItem
      Caption = 'File'
      object Refresh1: TMenuItem
        Caption = 'Refresh'
        OnClick = Refresh1Click
      end
    end
  end
end
