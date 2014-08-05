{
  * X11 (MIT) LICENSE *

  Copyright © 2014 Jolyon Smith

  Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
   so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.


  * GPL and Other Licenses *

  The FSF deem this license to be compatible with version 3 of the GPL.
   Compatability with other licenses should be verified by reference to those
   other license terms.


  * Contact Details *

  Original author : Jolyon Smith
  skype           : deltics
  e-mail          : <EXTLINK mailto: jsmith@deltics.co.nz>jsmith@deltics.co.nz</EXTLINK>
  website         : <EXTLINK http://www.deltics.co.nz>www.deltics.co.nz</EXTLINK>
}

  unit Deltics.XSP.Forms.NC;


interface

  uses
  { vcl: }
    Classes,
    Forms,
    Graphics,
    ImgList,
    Menus,
    Windows,
  { deltics: }
    Deltics.Windows.Canvas,
  { xsp: }
    Deltics.XSP.Menus;


  type
    TNCFrameArea = (
                    faTopGrab,
                    faLeftGrab,
                    faRightGrab,
                    faBottomGrab,
                    faTopLeftGrab,
                    faTopRightGrab,
                    faBottomLeftGrab,
                    faBottomRightGrab,
                    faCaption,
                    faQuickStrip,
                    faMenuStrip,
                    faToolStrip
                   );

    TCaptionControl = (
                       ccClose,
                       ccFullScreen,
                       ccMaximize,
                       ccMinimize,
                       ccSettings,
                       ccHelp,
                       ccSysIcon
                      );

    TTrackingMode     = (tmStart, tmUpdate, tmStop);
    TTrackingContext  = (tcNone, tcControl, tcMenu, tcArea);

    TCaptionControlInfo = record
      Rect: TRect;
      MouseOver: Boolean;
      Visible: Boolean;
    end;

    TNCMouseTracking = record
      Context: TTrackingContext;
      Control: TCaptionControl;
      LButtonDown: Boolean;
      MenuIndex: Integer;
    end;


    TCaptionControls = set of TCaptionControl;

    TFormSettings = class(TComponent)
    private
      fCaptionControls: TCaptionControls;
      procedure set_CaptionControls(const aValue: TCaptionControls);
    published
      property CaptionControls: TCaptionControls read fCaptionControls write set_CaptionControls;
    end;


    TXSPNonClientArea = class(TComponent)
    private
      fActivityMode: Boolean;
      fCanvas: TDCCanvas;
      fForm: TForm;
      fMetrics: TNonClientMetrics;
      fAreas: array [TNCFrameArea] of TRect;
      fAreasRGN: array [TNCFrameArea] of HRGN;
      fControls: array [TCaptionControl] of TCaptionControlInfo;
      fActiveMenu: PXSPMenuItem;
      fMenu: array of TXSPMenuItem;
      fMouseTracking: TNCMouseTracking;
      fSettings: TFormSettings;
    private
      fIsFullScreen: Boolean;
      fIsMaximized: Boolean;
      function get_BorderWidth: Integer;
      procedure set_ActivityMode(const aValue: Boolean);
      procedure set_IsFullScreen(const aValue: Boolean);
      procedure CalculateMenu;
      procedure DeleteAreas;
      procedure Invalidate(const aMenu: PXSPMenuItem);
      procedure PaintFrame(const aDC: HDC);
      procedure PaintControl(const aControl: TCaptionControl); overload;
      procedure PaintControl(const aDC: HDC; const aControl: TCaptionControl); overload;
      procedure PaintControls(const aDC: HDC);
      procedure PaintMenu(const aDC: HDC);
      procedure PaintMenuItem(const aIndex: Integer); overload;
      procedure PaintMenuItem(const aDC: HDC; const aIndex: Integer; const aCanvasReady: Boolean); overload;
      property Form: TForm read fForm;
    public
      procedure InvalidateMenus;
      function NCCalc: TRect; overload;
      procedure NCCalc(const aRect: TRect); overload;
      procedure CalculateClientArea(var aRect: TRect);
      procedure Paint;
      function NCTrackMouse(const aMousePos: TPoint): Boolean; overload;
      function NCTrackMouse(const aMousePos: TPoint; var aContext: TTrackingContext; var aControl: TCaptionControl; var aMenu: PXSPMenuItem; var aArea: TNCFrameArea; const aMode: TTrackingMode): Boolean; overload;

      procedure DismissMenu;
      procedure DropDownMenu(const aMenu: PXSPMenuItem);
      procedure UpdateClientArea(const aImmediate: Boolean = TRUE);
      procedure UpdateMetrics;
      procedure UpdateCaptionControls;
    public
      constructor Create(const aForm: TForm); reintroduce;
      destructor Destroy; override;
      property ActivityMode: Boolean read fActivityMode write set_ActivityMode;
      property BorderWidth: Integer read get_BorderWidth;
      property IsFullScreen: Boolean read fIsFullScreen write set_IsFullScreen;
      property IsMaximized: Boolean read fIsMaximized write fIsMaximized;
    end;



implementation

  uses
  { deltics: }
    Deltics.Strings,
  { xsp: }
    Deltics.XSP,
    Deltics.XSP.Forms,
    Deltics.XSP.Themes,
    Deltics.XSP.Themes.Colors;




  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TFormSettings.set_CaptionControls(const aValue: TCaptionControls);
  begin
    fCaptionControls := aValue;
  end;










{ TXSPNonClientArea ------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TXSPNonClientArea.Create(const aForm: Forms.TForm);
  begin
    inherited Create(aForm);

    fCanvas := TDCCanvas.Create(0);
    fForm   := aForm;

    fMetrics.cbSize       := sizeof(TNonClientMetrics);
    fMetrics.iBorderWidth := -1;

    UpdateMetrics;
    UpdateCaptionControls;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TXSPNonClientArea.Destroy;
  begin
    DismissMenu;
    DeleteAreas;

    fCanvas.Free;

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TXSPNonClientArea.get_BorderWidth: Integer;
  begin
    if IsFullScreen or IsMaximized then
      result := 0
    else
      result := fMetrics.iBorderWidth;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.Invalidate(const aMenu: PXSPMenuItem);
  begin
    aMenu.Glyph.Free;
    aMenu.Window.Free;

    aMenu.Glyph   := NIL;
    aMenu.Window  := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.InvalidateMenus;
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(fMenu)) do
      Invalidate(@fMenu[i]);

    SetLength(fMenu, 0);

    NCCalc;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TXSPNonClientArea.NCCalc: TRect;
  begin
    fIsMaximized := (Form.WindowState = wsMaximized);

    result := Rect(0, 0, Form.Width, Form.Height);
    NCCalc(result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.NCCalc(const aRect: TRect);

    procedure SetArea(const aArea: TNCFrameArea); overload;
    begin
      fAreasRGN[aArea] := CreateRectRgnIndirect(fAreas[aArea]);
    end;

    procedure SetArea(const aArea: TNCFrameArea;
                      const aRect: TRect); overload;
    begin
      fAreasRGN[aArea] := CreateRectRgnIndirect(aRect);
    end;

    procedure SetArea(const aArea: TNCFrameArea;
                      const aRects: array of TRect); overload;
    var
      i: Integer;
      rgn: HRGN;
    begin
      fAreasRGN[aArea] := CreateRectRgn(0, 0, 0, 0);

      for i := 0 to Pred(Length(aRects)) do
      begin
        rgn := CreateRectRgnIndirect(aRects[i]);
        CombineRgn(fAreasRGN[aArea], fAreasRGN[aArea], rgn, RGN_OR);
        DeleteObject(rgn);
      end;
    end;

  var
    i, j: Integer;
    dc: HDC;
    idx: Integer;
    border: Integer;
    captionY: Integer;
    gadgetX, gadgetY: Integer;
    rc: TRect;
    rcA, rcB: TRect;
    ctrl: TCaptionControl;
    s: String;
    ofont: HFONT;
    menu: PXSPMenuItem;
    menuItem: TMenuItem;
    images: TCustomImageList;
  begin
    border  := BorderWidth;
    gadgetX := fMetrics.iCaptionWidth;

    if (Form.BorderIcons <> []) {TODO: or Has QuickStrip} then
    begin
      if Form.BorderStyle in [bsToolWindow, bsSizeToolWin] then
        captionY := GetSystemMetrics(SM_CYSMCAPTION)
      else
        captionY := GetSystemMetrics(SM_CYCAPTION);
    end
    else
      captionY := 0;

    gadgetY := captionY;
    DeleteAreas;

    if (border > 1) then
    begin
      SetArea(faTopGrab,    RECT(gadgetX,                       0,            aRect.Right - gadgetX,  border));
      SetArea(faBottomGrab, RECT(gadgetX,                       aRect.Bottom, aRect.Right - gadgetX,  aRect.Bottom - border));
      SetArea(faLeftGrab,   RECT(0,                             gadgetY,      border,  aRect.Bottom - border));
      SetArea(faRightGrab,  RECT(aRect.Right - border, gadgetY, aRect.Right,  aRect.Bottom - border));

      rcA := RECT(0, 0, gadgetX, border);
      rcB := RECT(0, 0, border,  gadgetY);
      SetArea(faTopLeftGrab, [rcA, rcB]);

      rcA := RECT(aRect.Right - gadgetX, 0, aRect.Right, border);
      rcB := RECT(aRect.Right - border,  0, aRect.Right, gadgetY);
      SetArea(faTopRightGrab, [rcA, rcB]);

      rcA := RECT(0, aRect.Bottom, gadgetX, aRect.Bottom - border);
      rcB := RECT(0, aRect.Bottom, border,  aRect.Bottom - gadgetY);
      SetArea(faBottomLeftGrab, [rcA, rcB]);

      rcA := RECT(0, aRect.Bottom, gadgetX, aRect.Bottom - border);
      rcB := RECT(0, aRect.Bottom, border,  aRect.Bottom - gadgetY);
      SetArea(faBottomRightGrab, [rcA, rcB]);
    end;

    rc := aRect;

    fAreas[faCaption].Top     := border;
    fAreas[faCaption].Bottom  := border + captionY - 1;

    // Calculate gadgets and areas in the caption bar.  All of these have
    //  the same top and bottom

    rc.Top     := fAreas[faCaption].Top;
    rc.Bottom  := fAreas[faCaption].Bottom;

    // First calculate rectangles for gadgets in the top-right of
    //  the frame, starting with the 'Close' gadget (if visible)

    rc.Right   := rc.Right - border;

    if fControls[ccClose].Visible then
    begin
      if NOT (Form.BorderStyle in [bsToolWindow, bsSizeToolWin]) then
        rc.Left := rc.Right - gadgetX
      else
        rc.Left := rc.Right - (rc.Bottom - rc.Top);

      fControls[ccClose].Rect := rc;
    end
    else
      rc.Left := rc.Right;

    // Now calculate remaining top-right gadget areas for visible gadgets

    for ctrl := Succ(ccClose) to ccHelp do
    begin
      if NOT fControls[ctrl].Visible then
        CONTINUE;

      rc.Right := rc.Left - 2;
      rc.Left  := rc.Right - (rc.Bottom - rc.Top);

      fControls[ctrl].Rect := rc;
    end;

    fAreas[faCaption].Right := rc.Left;

    // Now calculate rectangles for top-left gadgets, starting with the
    //  SysIcon, if visible.

    rc.Left := border;

    fAreas[faMenuStrip] := RECT(-1, -1, -1, -1);

    if NOT fActivityMode then
    begin
      if fControls[ccSysIcon].Visible then
      begin
        rc.Right  := rc.Left + captionY - 4;
        rc.Bottom := rc.Top + captionY - 4;
        fControls[ccSysIcon].Rect := rc;
      end
      else
        rc.Right  := rc.Left;

      // TODO: Calculate QuickStrip gadget areas

      fAreas[faCaption].Left := rc.Right;

      if Assigned(Form.Menu) then
      begin
        fAreas[faMenuStrip].Left    := border;
        fAreas[faMenuStrip].Right   := Form.Width - border;
        fAreas[faMenuStrip].Top     := fAreas[faCaption].Bottom;
        fAreas[faMenuStrip].Bottom  := fAreas[faMenuStrip].Top + fMetrics.iMenuHeight;

        if (xeMenuBarBorder in XSP.Theme.Elements) then
          Inc(fAreas[faMenuStrip].Bottom, 2);

        CalculateMenu;
      end;
    end
    else
      fAreas[faCaption].Left := 150;

    SetArea(faCaption);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.CalculateClientArea(var aRect: TRect);
  var
    border: Integer;
  begin
    border := BorderWidth;
    InflateRect(aRect, -border, -border);

    if (Form.BorderIcons <> []) or (Form.Caption <> '') {TODO: or Has QuickStrip} then
    begin
      if Form.BorderStyle in [bsToolWindow, bsSizeToolWin] then
        Inc(aRect.Top, fMetrics.iSmCaptionHeight)
      else
        Inc(aRect.Top, fMetrics.iCaptionHeight);
    end;

    if fActivityMode then
    begin
      Inc(aRect.Left, 150);
      EXIT;
    end;

    if Assigned(Form.Menu) then
    begin
      // TODO: Different menu placement options will affect the client rect adjustment needed

      Inc(aRect.Top, fMetrics.iMenuHeight);

      if (xeMenuBarBorder in XSP.Theme.Elements) then
        Inc(aRect.Top, 2);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.CalculateMenu;
  const
    PAD = 10;
    GLYPH_SPACING = 4;
  var
    i, j: Integer;
    idx: Integer;
    menu: PXSPMenuItem;
    menuItem: TMenuItem;
    menuBottom: Integer;
    dc: HDC;
    rc: TRect;
    s: String;
    glyphDim: Integer;
    glyphTop: Integer;
    glyphBottom: Integer;
    font, ofont: HFONT;
    images: TCustomImageList;
  begin
    dc := Form.Canvas.Handle;

    images  := Form.Menu.Images;
    font    := CreateFontIndirect(fMetrics.lfMenuFont);
    ofont   := SelectObject(dc, font);
    try
      if Assigned(images) then
      begin
        rc.TopLeft := POINT(0, 0);
        DrawTextEx(dc, 'X', 1, rc, DT_CALCRECT or DT_SINGLELINE or DT_LEFT, NIL);
        glyphDim := rc.Bottom - rc.Top;

        rc := fAreas[faMenuStrip];
        glyphTop    := rc.Top + ((rc.Bottom - rc.Top - glyphDim) div 2);
        glyphBottom := rc.Bottom - ((rc.Bottom - rc.Top - glyphDim) div 2);
      end;

      idx := 0;
      SetLength(fMenu, Form.Menu.Items.Count);

      rc := fAreas[faMenuStrip];
      menuBottom := rc.Bottom;
      if (xeMenuBarBorder in XSP.Theme.Elements) then
      begin
        OffsetRect(rc, 1, 1);
        Dec(menuBottom);
      end;

      for i := 0 to Pred(Form.Menu.Items.Count) do
      begin
        menuItem := Form.Menu.Items[i];
        if NOT menuItem.Visible then
          CONTINUE;

        menu := @fMenu[idx];
        menu.Source := menuItem;

        // TODO: Make FORCED CAPS optional

        s := STR.Uppercase(menuItem.Caption);
        menu.Caption := s;

        for j := 0 to Pred(menuItem.Count) do
        begin
          menu.Enabled := menuItem.Items[j].Visible;
          if menu.Enabled then
            BREAK;
        end;

        DrawTextEx(dc, PChar(s), Length(s), rc, DT_CALCRECT or DT_SINGLELINE or DT_LEFT, NIL);
        Inc(rc.Right, PAD * 2);  // Add some padding - top level menu items are generously spaced

        rc.Bottom := menuBottom;

        if Assigned(images) and (menu.Source.ImageIndex <> -1) then
        begin
          Inc(rc.Right, glyphDim + GLYPH_SPACING);
          menu.GlyphRC    := RECT(rc.Left + PAD, glyphTop, rc.Left + PAD + glyphDim, glyphBottom);
          menu.CaptionRC  := RECT(menu.GlyphRC.Right + GLYPH_SPACING, rc.Top, rc.Right - PAD, rc.Bottom);

          menu.Glyph := Graphics.TBitmap.Create;
          images.GetBitmap(menu.Source.ImageIndex, menu.Glyph);
          menu.Glyph.Transparent := TRUE;
        end
        else
        begin
          menu.GlyphRC    := RECT(-1, -1, -1, -1);
          menu.CaptionRC  := RECT(rc.Left + PAD, rc.Top, rc.Right - PAD, rc.Bottom);
        end;

        if (rc.Right >= fAreas[faMenuStrip].Right) then
        begin
          Dec(idx);
          BREAK;
        end;

        menu.ItemRC := rc;

        rc.Left := rc.Right;
        Inc(idx);
      end;

      SetLength(fMenu, idx);

    finally
      SelectObject(dc, ofont);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.Paint;
  var
    dc: HDC;
  begin
    dc  := GetWindowDC(Form.Handle);
    try
      PaintFrame(dc);
      PaintControls(dc);

      if NOT fActivityMode and Assigned(Form.Menu) then
        PaintMenu(dc);

    finally
      ReleaseDC(Form.Handle, dc);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.PaintFrame(const aDC: HDC);
  const
    WINDOW      = FALSE;
    TOOLWINDOW  = TRUE;
    CAPTION_ALIGNMENT : array[WINDOW..TOOLWINDOW] of Integer    = (DT_CENTER,
                                                                   DT_LEFT);
    CAPTION_FONT      : array[WINDOW..TOOLWINDOW] of TXSPFont = (xfCaption,
                                                                 xfToolCaption);
  var
    rc: TRect;
    wrgn: HRGN;
    crgn: HRGN;
    font, ofont: HFONT;
    isToolWindow: Boolean;
  begin
    rc := RECT(0, 0, Form.Width, Form.Height);

    if NOT (IsFullScreen or IsMaximized) then
    begin
      FrameRect(aDC, rc, XSP.Brush[xbBorder]);
      InflateRect(rc, -1, -1);
    end;

    wrgn := CreateRectRgnIndirect(rc);
    try
      // Remove client area from region
      CalculateClientArea(rc);
      crgn := CreateRectRgnIndirect(rc);
      CombineRgn(wrgn, wrgn, crgn, RGN_XOR);
      DeleteObject(crgn);

      if fActivityMode then
      begin
        crgn := CreateRectRgnIndirect(RECT(BorderWidth, BorderWidth, 150 + BorderWidth, Form.Height - BorderWidth));
        CombineRgn(wrgn, wrgn, crgn, RGN_XOR);
        FillRgn(aDC, crgn, XSP.Brush[xbBorder]);
        DeleteObject(crgn);
      end
      else if Assigned(Form.Menu) then // Remove menu strip from region
      begin
        crgn := CreateRectRgnIndirect(fAreas[faMenuStrip]);
        CombineRgn(wrgn, wrgn, crgn, RGN_XOR);
        DeleteObject(crgn);
      end;

      if Form.Active or (XSP.Brush[xbInactiveFrame] = 0) then
        FillRgn(aDC, wrgn, XSP.Brush[xbActiveFrame])
      else
        FillRgn(aDC, wrgn, XSP.Brush[xbInactiveFrame]);

      isToolWindow := Form.BorderStyle in [bsToolWindow, bsSizeToolWin];

      if isToolWindow then
        font := CreateFontIndirect(fMetrics.lfSmCaptionFont)
      else
        font := CreateFontIndirect(fMetrics.lfCaptionFont);

      ofont := SelectObject(aDC, font);
      try
        SetBkMode(aDC, TRANSPARENT);

        if Form.Active or (XSP.Color[xcInactiveCaptionText] = clNone) then
          SetTextColor(aDC, XSP.RGB[xcActiveCaptionText])
        else
          SetTextColor(aDC, XSP.RGB[xcInactiveCaptionText]);

        rc := fAreas[faCaption];
        InflateRect(rc, -4, 0);

        DrawTextEx(aDC, PChar(Form.Caption), Length(Form.Caption), rc,
                   CAPTION_ALIGNMENT[isToolWindow] or DT_SINGLELINE or DT_VCENTER
                    or DT_NOPREFIX or DT_END_ELLIPSIS, NIL);

      finally
        SelectObject(aDC, ofont);
        DeleteObject(font);
      end;

    finally
      DeleteObject(wrgn);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.PaintMenu(const aDC: HDC);
  var
    i: Integer;
    rc: TRect;
    font, ofont: HFONT;
  begin
    rc := fAreas[faMenuStrip];

    if (xeMenuBarBorder in XSP.Theme.Elements) then
    begin
      FrameRect(aDC, rc, XSP.Brush[xbMenuBorder]);
      InflateRect(rc, -1, -1);
    end;

    if (Length(fMenu) = 0) then
    begin
      FillRect(aDC, rc, XSP.Brush[xbMenuBackground]);
      EXIT;
    end;

    font  := CreateFontIndirect(fMetrics.lfMenuFont);
    ofont := SelectObject(aDC, font);
    try
      SetBkMode(aDC, TRANSPARENT);

      for i := 0 to Pred(Length(fMenu)) do
        PaintMenuItem(aDC, i, TRUE);

      rc.Left := fMenu[Pred(Length(fMenu))].ItemRC.Right;
      FillRect(aDC, rc, XSP.Brush[xbMenuBackground]);

    finally
      SelectObject(aDC, ofont);
      DeleteObject(font);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.PaintMenuItem(const aIndex: Integer);
  var
    dc: HDC;
  begin
    dc := GetWindowDC(Form.Handle);
    try
      PaintMenuItem(dc, aIndex, FALSE);

    finally
      ReleaseDC(Form.Handle, dc);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.PaintMenuItem(const aDC: HDC;
                                            const aIndex: Integer;
                                            const aCanvasReady: Boolean);
  var
    menu: PXSPMenuItem;
    font, ofont: HFONT;
    rc: TRect;
    s: String;
  begin
    ofont := 0;
    menu  := @fMenu[aIndex];

    if menu.Enabled then
      SetTextColor(aDC, XSP.RGB[xcMenuText])
    else
      SetTextColor(aDC, XSP.RGB[xcMenuDisabledText]);

    if NOT aCanvasReady then
    begin
      font  := CreateFontIndirect(fMetrics.lfMenuFont);
      ofont := SelectObject(aDC, font);
    end;

    try
      s   := STR.Uppercase(menu.Source.Caption);
      rc  := menu.ItemRC;

      if menu.MouseOver and menu.Enabled then
      begin
        if fMouseTracking.Context in [tcNone, tcMenu] then
          FillRect(aDC, rc, XSP.Brush[xbHotMenuBackground]);
      end
      else
        FillRect(aDC, rc, XSP.Brush[xbMenuBackground]);

      SetBkMode(aDC, TRANSPARENT);

      if Assigned(menu.Glyph) then
      begin
        fCanvas.Handle := aDC;
        fCanvas.Draw(menu.Glyph, menu.GlyphRC);
        fCanvas.Handle := 0;

        DrawTextEx(aDC, PChar(s), Length(s), menu.CaptionRC, DT_SINGLELINE or DT_VCENTER or DT_LEFT, NIL);
      end
      else
        DrawTextEx(aDC, PChar(s), Length(s), menu.CaptionRC, DT_SINGLELINE or DT_VCENTER or DT_CENTER, NIL);

    finally
      if (ofont <> 0) then
      begin
        SelectObject(aDC, ofont);
        DeleteObject(font);
      end;
    end;
  end;


  procedure TXSPNonClientArea.set_ActivityMode(const aValue: Boolean);
  begin
    fActivityMode := aValue;

    SetWindowPos(Form.Handle, 0, Form.Left, Form.Top, Form.Width, Form.Height + 1, SWP_NOMOVE);
    SetWindowPos(Form.Handle, 0, Form.Left, Form.Top, Form.Width, Form.Height - 1, SWP_NOMOVE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.set_IsFullScreen(const aValue: Boolean);
  begin
    if fIsFullScreen = aValue then
      EXIT;

    fIsFullScreen := aValue;

    UpdateClientArea(FALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.PaintControl(const aControl: TCaptionControl);
  var
    dc: HDC;
  begin
    dc := GetWindowDC(Form.Handle);
    try
      PaintControl(dc, aControl);

    finally
      ReleaseDC(Form.Handle, dc);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.PaintControl(const aDC: HDC;
                                           const aControl: TCaptionControl);
  const
    DRAWN_NORMAL  = FALSE;
    DRAWN_HOT     = TRUE;
    GLYPHCHAR : array[TCaptionControl] of String = ('r',
                                                    ' ',
                                                    '1',
                                                    '0',
                                                    ' ',
                                                    's',
                                                    ' ');
    FILL_BRUSH  : array[TCaptionControl, DRAWN_NORMAL..DRAWN_HOT] of TXSPBrush =
                  (
                   (xbGadgetClose,       xbHotGadgetClose),
                   (xbGadgetFullScreen,  xbHotGadgetFullScreen),
                   (xbGadgetMaximize,    xbHotGadgetMaximize),
                   (xbGadgetMinimize,    xbHotGadgetMinimize),
                   (xbGadgetSettings,    xbHotGadgetSettings),
                   (xbGadgetHelp,        xbHotGadgetHelp),
                   (xbActiveFrame,       xbActiveFrame)               // SysIcon - no brush
                  );
    TEXT_COLOR  : array[TCaptionControl, DRAWN_NORMAL..DRAWN_HOT] of TXSPColor =
                  (
                   (xcGadgetCloseGlyph,       xcHotGadgetCloseGlyph),
                   (xcGadgetFullScreenGlyph,  xcHotGadgetFullScreenGlyph),
                   (xcGadgetMaximizeGlyph,    xcHotGadgetMaximizeGlyph),
                   (xcGadgetMinimizeGlyph,    xcHotGadgetMinimizeGlyph),
                   (xcGadgetSettingsGlyph,    xcHotGadgetSettingsGlyph),
                   (xcGadgetHelpGlyph,        xcHotGadgetHelpGlyph),
                   (xcActiveFrame,            xcActiveFrame)         // SysIcon - no brush
                  );
  var
    oFont: HFONT;
    isHot: Boolean;
  begin
    if NOT fControls[aControl].Visible then
      EXIT;

    SetBkMode(aDC, TRANSPARENT);

    if (aControl = ccSysIcon) then
    begin
      if fActivityMode then
        EXIT;

      with fControls[ccSysIcon].Rect do
        DrawIconEx(aDC, Left, Top, Application.Icon.Handle,
                        Right - Left, Bottom - Top, 0, 0, DI_NORMAL);

      EXIT;
    end;

    isHot := fControls[aControl].MouseOver;

    if (XSP.Brush[FILL_BRUSH[aControl, isHot]] <> 0) then
      FillRect(aDC, fControls[aControl].Rect, XSP.Brush[FILL_BRUSH[aControl, isHot]])
    else if Form.Active then
      FillRect(aDC, fControls[aControl].Rect, XSP.Brush[xbActiveFrame])
    else
      FillRect(aDC, fControls[aControl].Rect, XSP.Brush[xbInactiveFrame]);

    SetTextColor(aDC, XSP.RGB[TEXT_COLOR[aControl, isHot]]);

    oFont := SelectObject(aDC, XSP.Font[xfMarlett]);
    try
      DrawTextEx(aDC, PChar(GLYPHCHAR[aControl]), 1, fControls[aControl].Rect,
                   DT_SINGLELINE or DT_CENTER or DT_VCENTER, NIL);

    finally
      SelectObject(aDC, oFont);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.PaintControls(const aDC: HDC);
  var
    ctrl: TCaptionControl;
  begin
    for ctrl := Low(TCaptionControl) to High(TCaptionControl) do
      PaintControl(aDC, ctrl);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TXSPNonClientArea.NCTrackMouse(const aMousePos: TPoint): Boolean;
  var
    notUsedContext: TTrackingContext;
    notUsedControl: TCaptionControl;
    notUsedMenu: PXSPMenuItem;
    notUsedArea: TNCFrameArea;
  begin
    result := NCTrackMouse(aMousePos, notUsedContext,
                                      notUsedControl,
                                      notUsedMenu,
                                      notUsedArea,
                                      tmUpdate);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TXSPNonClientArea.NCTrackMouse(const aMousePos: TPoint;
                                    var aContext: TTrackingContext;
                                    var aControl: TCaptionControl;
                                    var aMenu: PXSPMenuItem;
                                    var aArea: TNCFrameArea;
                                    const aMode: TTrackingMode): Boolean;
  var
    ctrl: TCaptionControl;
    menuIndex: Integer;
    area: TNCFrameArea;
  begin
    aContext := tcNone;

    case aMode of
      tmStart : fMouseTracking.LButtonDown := TRUE;
      tmStop  : fMouseTracking.LButtonDown := FALSE;
    end;

    try
      for ctrl := Low(TCaptionControl) to High(TCaptionControl) do
      begin
        if NOT fControls[ctrl].Visible then
          CONTINUE;

        if ((fMouseTracking.Context = tcNone)
             or ((fMouseTracking.Context = tcControl) and (fMouseTracking.Control = ctrl)))
         and PtInRect(fControls[ctrl].Rect, aMousePos) then
        begin
          aContext  := tcControl;
          aControl  := ctrl;

          case aMode of
            tmStart   : begin
                          fMouseTracking.Context := tcControl;
                          fMouseTracking.Control := aControl;
                          result := TRUE;
                        end;

            tmUpdate  : result := TRUE;

            tmStop    : result := (fMouseTracking.Context = tcControl)
                              and (aControl = fMouseTracking.Control);
          end;

          fControls[ctrl].MouseOver := (aMode <> tmStop);
          PaintControl(ctrl);
        end
        else if fControls[ctrl].MouseOver then
        begin
          fControls[ctrl].MouseOver := FALSE;
          PaintControl(ctrl);
        end;
      end;

      if NOT fActivityMode then
        for menuIndex := 0 to Pred(Length(fMenu)) do
        begin
          if (fMouseTracking.Context in [tcNone, tcMenu])
           and PtInRect(fMenu[menuIndex].ItemRC, aMousePos) then
          begin
            aContext  := tcMenu;
            aMenu     := @(fMenu[menuIndex]);
            fMouseTracking.MenuIndex := menuIndex;

            case aMode of
              tmStart : begin
                          fMenu[menuIndex].MouseOver := TRUE;
                          fMouseTracking.Context     := tcMenu;
                          PaintMenuItem(menuIndex);
                        end;

              tmStop  : begin
                          fMenu[menuIndex].MouseOver := TRUE;
                          fMouseTracking.Context     := tcNone;
                          PaintMenuItem(menuIndex);
                        end;
            else // tmUpdate
              if NOT aMenu.MouseOver and aMenu.Enabled then
              begin
                if NOT Assigned(fActiveMenu) then
                begin
                  fMenu[menuIndex].MouseOver := TRUE;
                  PaintMenuItem(menuIndex);
                end
                else
                  DropDownMenu(aMenu);
              end;
            end;
          end
          else if fMenu[menuIndex].MouseOver then
          begin
            fMenu[menuIndex].MouseOver := FALSE;
            PaintMenuItem(menuIndex);
          end;
        end;

      if (fMouseTracking.Context = tcNone) then
      begin
        if BorderWidth > 1 then
          area := Low(TNCFrameArea)
        else
          area := faCaption;

        for area := area to High(TNCFrameArea) do
          if PtInRegion(fAreasRGN[area], aMousePos.X, aMousePos.Y) then
          begin
            aContext  := tcArea;
            aArea     := area;
            BREAK;
          end;
      end;

    finally
      if (aMode = tmStop) and (aContext <> tcMenu) then
        fMouseTracking.Context := tcNone;

      result := (aContext <> tcNone);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.UpdateMetrics;
  const
    X_AXIS = 0;
    Y_AXIS = 1;
    BORDERMETRICS : array [TFormBorderStyle, X_AXIS..Y_AXIS] of Integer = ((-1, -1), // Not used (bsNone)
                                                                           (SM_CXBORDER,    SM_CYBORDER),
                                                                           (SM_CXSIZEFRAME, SM_CYSIZEFRAME),
                                                                           (SM_CXDLGFRAME,  SM_CYDLGFRAME),
                                                                           (SM_CXBORDER,    SM_CYBORDER),
                                                                           (SM_CXSIZEFRAME, SM_CYSIZEFRAME));
  var
    oldBorder: Integer;
    diff: Integer;
    rc: TRect;
  begin
    oldBorder := fMetrics.iBorderWidth;

    SystemParametersInfo(SPI_GETNONCLIENTMETRICS, fMetrics.cbSize, @fMetrics, 0);

    case Form.BorderStyle of
      bsNone        : fMetrics.iBorderWidth := 0;

      bsSingle,
      bsToolWindow  : fMetrics.iBorderWidth := 1;

      bsDialog,
      bsSizeable    : { NO-OP - Use metrics };

      bsSizeToolWin : fMetrics.iBorderWidth := fMetrics.iBorderWidth div 2;
    end;

    if (oldBorder = -1) then
      EXIT;

    if (fMetrics.iBorderWidth <> oldBorder) then
    begin
      diff  := fMetrics.iBorderWidth - oldBorder;
      rc    := Form.BoundsRect;
      InflateRect(rc, diff, diff);
      Form.SetBounds(rc.Left, rc.Top, rc.Right - rc.Left, rc.Bottom - rc.Top);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.UpdateCaptionControls;
  {
    Determines which of the controls in the caption bar area should be
     visible.
  }
  var
    ctrl: TCaptionControl;
    isTool: Boolean;
  begin
    isTool := Form.BorderStyle in [bsDialog, bsToolWindow, bsSizeToolWin];

    if Assigned(fSettings) then
    begin
      for ctrl := Low(TCaptionControl) to High(TCaptionControl) do
        fControls[ctrl].Visible := (ctrl in fSettings.CaptionControls);

      if isTool then
      begin
        fControls[ccMaximize].Visible := FALSE;
        fControls[ccMinimize].Visible := FALSE;
        fControls[ccSysIcon].Visible  := FALSE;
      end;
    end
    else
    begin
      fControls[ccClose].Visible      := (biSystemMenu in Form.BorderIcons);
      fControls[ccFullScreen].Visible := TRUE;
      fControls[ccMaximize].Visible   := NOT isTool and (biMaximize in Form.BorderIcons);
      fControls[ccMinimize].Visible   := NOT isTool and (biMinimize in Form.BorderIcons);
      fControls[ccHelp].Visible       := (biHelp in Form.BorderIcons);
      fControls[ccSysIcon].Visible    := NOT isTool and (biSystemMenu in Form.BorderIcons);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.UpdateClientArea(const aImmediate: Boolean);
  begin
    if aImmediate then
      SendMessage(Form.Handle, XM_UPDATECLIENTAREA, 0, 0)
    else
      PostMessage(Form.Handle, XM_UPDATECLIENTAREA, 0, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.DeleteAreas;
  var
    area: TNCFrameArea;
  begin
    for area := Low(area) to High(area) do
      if fAreasRGN[area] <> 0 then
      begin
        DeleteObject(fAreasRGN[area]);
        fAreasRGN[area] := 0;
      end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.DismissMenu;
  begin
    fMouseTracking.Context := tcNone;

    if NOT Assigned(fActiveMenu) then
      EXIT;

    fActiveMenu.Window.Hide;
    fActiveMenu := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TXSPNonClientArea.DropDownMenu(const aMenu: PXSPMenuItem);
  begin
    if Assigned(fActiveMenu) and (aMenu <> fActiveMenu) then
      fActiveMenu.Window.Hide;

    fActiveMenu := aMenu;

    if Assigned(fActiveMenu) and NOT Assigned(fActiveMenu.Window) then
      aMenu.Window := TXSPMenuWindow.Create(Form, aMenu, fMetrics.lfMenuFont);

    fActiveMenu.Window.Show;
  end;






end.
