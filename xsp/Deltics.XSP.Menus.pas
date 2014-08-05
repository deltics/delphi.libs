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

  unit Deltics.XSP.Menus;


interface

  uses
  { vcl: }
    Classes,
    Controls,
    Forms,
    Graphics,
    Menus,
    Messages,
    Windows,
  { deltics: }
    Deltics.Windows.Canvas;


  type
    TXSPMenuWindow = class;


    PXSPMenuItem = ^TXSPMenuItem;
    TXSPMenuItem = record
      ItemRC: TRect;        // Bounding RC of the menu item itself
        GlyphRC: TRect;       //    - RC of Glyph area (optional in menu bar)
        CaptionRC: TRect;     //    - RC of Caption area
        ShortcutRC: TRect;    //    - RC of Shortcut area (not used in menu bar)
      Caption: String;
      Column: Integer;
      Enabled: Boolean;
      Glyph: Graphics.TBitmap;
      MouseOver: Boolean;
      Source: TMenuItem;
      Window: TXSPMenuWindow;
    end;

    PXSPMenuColumn = ^TXSPMenuColumn;
    TXSPMenuColumn = record
      RC: TRect;
      BarBreak: Boolean;
    end;


    TMenuManager = class(TComponent)
    private
      fMenu: TMainMenu;
    end;


    TXSPMenuWindow = class(TCustomForm)
    private
      fCanvas: TDCCanvas;
      fFont: HFONT;
      fForm: TCustomForm;
      fColumns: array of TXSPMenuColumn;
      fItems: array of TXSPMenuItem;
      fItemGlyph: Graphics.TBitmap;
      fItemRC: TRect;
      fItemCaptionRC: TRect;
      fItemGlyphRC: TRect;
      fItemShortcutRC: TRect;
      fMenuRC: TRect;
      fMenu: PXSPMenuItem;
      fPos: TPoint;
      procedure Calc;
      property Form: TCustomForm read fForm;
    protected
      procedure CreateHandle; override;
      procedure CreateParams(var aParams: TCreateParams); override;
      procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
      procedure Paint; override;
      procedure PaintMenuItem(const aDC: HDC; const aMenuItem: PXSPMenuItem);
      procedure WndProc(var aMessage: TMessage); override;
    public
      constructor Create(const aForm: TCustomForm; const aMenu: PXSPMenuItem; const aFont: TLogFont); reintroduce;
      destructor Destroy; override;
      procedure Hide;
      procedure Show;
    end;


implementation

  uses
  { vcl: }
    SysUtils,
  { deltics: }
    Deltics.XSP,
    Deltics.XSP.Forms,
    Deltics.XSP.Themes,
    Deltics.XSP.Themes.Colors;


{ TXSPMenu }

  constructor TXSPMenuWindow.Create(const aForm: TCustomForm;
                                    const aMenu: PXSPMenuItem;
                                    const aFont: TLogFont);
  begin
    inherited CreateNew(aForm, 0);

    BorderIcons := [];
    BorderStyle := bsNone;
    Caption     := '';
    FormStyle   := fsNormal;

    fCanvas := TDCCanvas.Create(0);
    fFont   := CreateFontIndirect(aFont);
    fForm   := aForm;
    fMenu   := aMenu;

    fItemGlyph  := fMenu.Glyph;

    Calc;
  end;


  procedure TXSPMenuWindow.CreateHandle;
  var
    rc: TRect;
    rgn: HRGN;
    temp: HRGN;
  begin
    inherited;

    rc := fMenuRC;
    InflateRect(rc, 1, 1);
    rgn := CreateRectRgnIndirect(rc);

    if (XSP.Theme.Color[xcMenuDropShadow] <> clNone) then
    begin
      temp := CreateRectRgnIndirect(rc);
      OffsetRgn(temp, 1, 1);
      CombineRgn(rgn, rgn, temp, RGN_OR);
      DeleteObject(temp);
    end;

    rc := fItemRC;
    InflateRect(rc, 1, 1);

    temp := CreateRectRgnIndirect(rc);
    CombineRgn(rgn, rgn, temp, RGN_OR);
    DeleteObject(temp);

    GetRgnBox(rgn, rc);
    MoveWindow(Handle, fPos.X, fPos.Y, rc.Right, rc.Bottom, FALSE);
    SetWindowRgn(Handle, rgn, TRUE);
  end;


  procedure TXSPMenuWindow.Calc;
  const
    INITRC : TRect = (Left: 0; Top: 0; Right: 0; Bottom:0);
  var
    hWndForm: HWND;
    dc: HDC;
    ofont: HFONT;
    i: Integer;
    col, idx: Integer;
    s: String;
    rc: TRect;
    itemsRC: TRect;
  begin
    fItemRC := fMenu.ItemRC;

    fItemCaptionRC  := fMenu.CaptionRC;
    fItemGlyphRC    := fMenu.GlyphRC;
    fItemShortcutRC := fMenu.ShortcutRC;

    OffsetRect(fItemCaptionRC,  -fItemRC.Left, -fItemRC.Top);
    OffsetRect(fItemGlyphRC,    -fItemRC.Left, -fItemRC.Top);
    OffsetRect(fItemShortcutRC, -fItemRC.Left, -fItemRC.Top);
    OffsetRect(fItemRC,         -fItemRC.Left, -fItemRC.Top);

    if (xeMenuBarBorder in XSP.Theme.Elements) then
      InflateRect(fItemRC, 1, 1);

    hWndForm  := Form.Handle;

    dc    := GetWindowDC(hWndForm);
    ofont := SelectObject(dc, fFont);
    try
      rc      := RECT(1, 1, 1, 1);
      itemsRC := rc;

      col := 0;
      idx := 0;
      SetLength(fColumns, fMenu.Source.Count);
      SetLength(fItems,   fMenu.Source.Count);

      for i := 0 to Pred(fMenu.Source.Count) do
      begin
        if NOT fMenu.Source.Items[i].Visible then
          CONTINUE;

        fItems[idx].Source  := fMenu.Source.Items[i];
        fItems[idx].Caption := fMenu.Source.Items[i].Caption;
        fItems[idx].Enabled := fMenu.Source.Items[i].Enabled;

        s := fItems[idx].Source.Caption;

        if fItems[idx].Source.Break in [mbBreak, mbBarBreak] then
        begin
          fColumns[col].RC := itemsRC;
          fColumns[col].RC.Top    := 1;
          fColumns[col].RC.Bottom := rc.Bottom;
          fColumns[col].BarBreak  := (fItems[idx].Source.Break = mbBarBreak);

          rc.Left := itemsRC.Right;

          if fColumns[col].BarBreak then
            Inc(rc.Left);

          rc.Top  := 1;
          itemsRC := rc;
          Inc(col);
        end;

        fItems[idx].Column  := col;

        // TODO: Ignore consecutive separators

        if (s <> '-') then
        begin
          DrawTextEx(dc, PChar(s), Length(s), rc, DT_CALCRECT or DT_SINGLELINE or DT_LEFT, NIL);
          Inc(rc.Right, 25); // Space for checkmark/radio button/bitmap (to left of Caption)
          Inc(rc.Right, 75); // Space for shortcut/sub-menu indicator (to right of Caption)
          Inc(rc.Right, 26);  // Caption padding  [6]Caption[20]

          if rc.Right > itemsRC.Right then
            itemsRC.Right := rc.Right;

          rc.Bottom := rc.Bottom + 6; // Text rect is a snug fit - add 3 pixels of padding above/below
        end
        else
        begin
          rc.Right  := itemsRC.Right;
          rc.Bottom := rc.Top;
        end;

        fItems[idx].ItemRC := rc;

        rc.Top := rc.Bottom + 1;
        Inc(idx);
      end;

      fColumns[col].RC  := rc;
      fColumns[col].RC.Top := 1;

      SetLength(fColumns, col + 1);
      SetLength(fItems, idx);

      for i := 0 to Pred(Length(fItems)) do
        fItems[i].ItemRC.Right := fColumns[fItems[i].Column].RC.Right;

      itemsRC.Left := 1;
      itemsRC.Bottom := fColumns[0].RC.Bottom;
      for i := 1 to Pred(Length(fColumns)) do
        if fColumns[i].RC.Bottom > itemsRC.Bottom then
          itemsRC.Bottom := fColumns[i].RC.Bottom;

      InflateRect(itemsRC, 1, 1);
      Inc(itemsRC.Bottom);

    finally
      SelectObject(dc, ofont);
    end;

    fPos.X := Form.Left + fMenu.ItemRC.Left;
    fPos.Y := Form.Top + fMenu.ItemRC.Top;

    if (fPos.Y + fItemRC.Bottom + itemsRC.Bottom) > Screen.MonitorFromWindow(hWndForm).Height then
    begin
      OffsetRect(fItemRC, 0, itemsRC.Bottom - 1);
      OffsetRect(fItemCaptionRC,  0, itemsRC.Bottom - 1);
      OffsetRect(fItemGlyphRC,    0, itemsRC.Bottom - 1);
      OffsetRect(fItemShortcutRC, 0, itemsRC.Bottom - 1);

      fPos.Y := fPos.Y - (itemsRC.Bottom - itemsRC.Top) + 1;
    end
    else
    begin
      OffsetRect(itemsRC, 0, fItemRC.Bottom - 1);

      for i := 0 to Pred(Length(fColumns)) do
        OffsetRect(fColumns[i].RC, 0, fItemRC.Bottom - 1);

      for i := 0 to Pred(Length(fItems)) do
        OffsetRect(fItems[i].ItemRC, 0, fItemRC.Bottom - 1);
    end;

    for i := 0 to Pred(Length(fItems)) do
    begin
      rc := fItems[i].ItemRC;
      fItems[i].GlyphRC       := rc;
      fItems[i].CaptionRC     := rc;
      fItems[i].ShortcutRC    := rc;

      fItems[i].GlyphRC.Left  := fColumns[fItems[i].Column].RC.Left;
      fItems[i].GlyphRC.Right := fItems[i].GlyphRC.Left + 24;

      fItems[i].ShortcutRC.Left := fItems[i].ItemRC.Right - 75;
      fItems[i].CaptionRC.Left  := fItems[i].GlyphRC.Right + 6;
      fItems[i].CaptionRC.Right := fItems[i].ShortcutRC.Left - 20;
    end;

    fMenuRC := itemsRC;

    if (xeMenuBarBorder in XSP.Theme.Elements) then
    begin
      //
    end;
  end;


  procedure TXSPMenuWindow.CreateParams(var aParams: TCreateParams);
  begin
    inherited;

    aParams.ExStyle := aParams.ExStyle or WS_EX_NOACTIVATE;
  end;


  destructor TXSPMenuWindow.Destroy;
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(fItems)) do
    begin
      fItems[i].Glyph.Free;
      fItems[i].Window.Free;

      fItems[i].Glyph   := NIL;
      fItems[i].Window  := NIL;
    end;

    fCanvas.Free;
    DeleteObject(fFont);

    inherited Destroy;
  end;


  procedure TXSPMenuWindow.Hide;
  begin
    if Assigned(self) and HandleAllocated then
      ShowWindow(Handle, SW_HIDE);
  end;


  procedure TXSPMenuWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
  var
    i: Integer;
    pos: TPoint;
    dc: HDC;
    ofont: HFONT;
    rc: TRect;
  begin
    pos := POINT(X, Y);

    dc    := GetDC(Handle);
    ofont := SelectObject(dc, fFont);
    try
      SetBkMode(dc, TRANSPARENT);

      for i := 0 to Pred(Length(fItems)) do
      begin
        if fItems[i].Source.Caption = '-' then
          CONTINUE;

        rc := fItems[i].ItemRC;
        Inc(rc.Bottom);
        if PtInRect(rc, pos) then
        begin
          if NOT fItems[i].MouseOver then
          begin
            fItems[i].MouseOver := TRUE;
            PaintMenuItem(dc, @fItems[i]);
          end;
        end
        else
        if fItems[i].MouseOver then
        begin
          fItems[i].MouseOver := FALSE;
          PaintMenuItem(dc, @fItems[i]);
        end;
      end;

    finally
      SelectObject(dc, ofont);
      ReleaseDC(Handle, dc);
    end;
  end;


  procedure TXSPMenuWindow.Paint;
  var
    i: Integer;
    dc: HDC;
    ofont: HFONT;
    open: HPEN;
    s: String;
    rc: TRect;
  begin
    dc := GetDC(Handle);
    try
      if (XSP.Theme.Color[xcMenuDropShadow] <> clNone) then
      begin
        open := SelectObject(dc, XSP.Pen[xpMenuDropShadow]);
        MoveToEx(dc, fMenuRC.Right, fMenuRC.Top + 1, NIL);
        LineTo(dc,   fMenuRC.Right, fMenuRC.Bottom);
        LineTo(dc,   fMenuRC.Left + 1, fMenuRC.Bottom);
        SelectObject(dc, open);
      end;

      // Fill the background of the menu and fill the item caption with the
      //  HOT menu fill
      FillRect(dc, fMenuRC, XSP.Brush[xbMenuBackground]);
      FillRect(dc, fItemRC, XSP.Brush[xbHotMenuBackground]);

      // Draw any column break separators...
      open := SelectObject(dc, XSP.Pen[xpMenuBorder]);
      for i := 0 to Pred(Length(fColumns)) do
      begin
        rc := fColumns[i].RC;
        rc.Right  := rc.Left + 24;
        rc.Bottom := fMenuRC.Bottom;
        if (XSP.Brush[xbMenuGlyphBackground] = 0) then
        begin
          open := SelectObject(dc, XSP.Pen[xpMenuSeparator]);
          MoveToEx(dc, rc.Right - 1, rc.Top, NIL);
          LineTo(dc, rc.Right - 1, rc.Bottom);
          SelectObject(dc, open);
        end
        else
          FillRect(dc, rc, XSP.Brush[xbMenuGlyphBackground]);

        if fColumns[i].BarBreak then
        begin
          MoveToEx(dc, fColumns[i].RC.Right, fMenuRC.Top, NIL);
          LineTo(dc,   fColumns[i].RC.Right, fMenuRC.Bottom);
        end;
      end;
      SelectObject(dc, open);

      // Frame the ENTIRE menu
      FrameRect(dc, fItemRC,  XSP.Brush[xbMenuBorder]);
      FrameRect(dc, fMenuRC,  XSP.Brush[xbMenuBorder]);

      SetBkMode(dc, TRANSPARENT);

      ofont := SelectObject(dc, fFont);
      try
        if fMenu.Enabled then
          SetTextColor(dc, XSP.RGB[xcMenuText])
        else
          SetTextColor(dc, XSP.RGB[xcMenuDisabledText]);

        s := fMenu.Caption;

        if Assigned(fItemGlyph) then
        begin
          fCanvas.Handle := dc;
          fCanvas.Draw(fItemGlyph, fItemGlyphRC);;
          fCanvas.Handle := 0;
          DrawTextEx(dc, PChar(s), Length(s), fItemCaptionRC, DT_SINGLELINE or DT_VCENTER or DT_LEFT, NIL);
        end
        else
          DrawTextEx(dc, PChar(s), Length(s), fItemCaptionRC, DT_SINGLELINE or DT_VCENTER or DT_CENTER, NIL);

        open := SelectObject(dc, XSP.Pen[xpMenuBorder]);
        try
          MoveToEx(dc, fItemRC.Left, fItemRC.Bottom, NIL);
          LineTo(dc, fItemRC.Right, fItemRC.Bottom);

          SelectObject(dc, XSP.Pen[xpMenuSeparator]);

          for i := 0 to Pred(Length(fItems)) do
          begin
            if (fItems[i].Source.Caption = '-') then
            begin
              rc  := fItems[i].ItemRC;
              rc.Left := fItems[i].GlyphRC.Right;

              MoveToEx(dc, rc.Left, rc.Top, NIL);
              LineTo(dc, rc.Right - 1, rc.Top);
            end
            else
              PaintMenuItem(dc, @fItems[i]);
          end;

        finally
          SelectObject(dc, open);
        end;

      finally
        SelectObject(dc, ofont);
      end;

    finally
      ReleaseDC(Handle, dc);
    end;
  end;


  procedure TXSPMenuWindow.PaintMenuItem(const aDC: HDC;
                                         const aMenuItem: PXSPMenuItem);
  var
    s: String;
    rc: TRect;
    open: HPEN;
  begin
    if aMenuItem.MouseOver then
    begin
      rc := aMenuItem.ItemRC;
      Inc(rc.Bottom);
      if aMenuItem.Enabled then
        FillRect(aDC, rc, XSP.Brush[xbHotMenuBackground])
      else
        FillRect(aDC, rc, XSP.Brush[xbMenuBackground]);
    end
    else
    begin
      rc := aMenuItem.ItemRC;
      Inc(rc.Bottom);
      FillRect(aDC, rc, XSP.Brush[xbMenuBackground]);
    end;

    rc := aMenuItem.GlyphRC;
    Inc(rc.Bottom);
    if (XSP.Brush[xbMenuGlyphBackground] = 0) then
    begin
      open := SelectObject(aDC, XSP.Pen[xpMenuSeparator]);
      MoveToEx(aDC, rc.Right - 1, rc.Top, NIL);
      LineTo(aDC, rc.Right - 1, rc.Bottom);
      SelectObject(aDC, open);
    end
    else if aMenuItem.Enabled and NOT aMenuItem.MouseOver then
      FillRect(aDC, rc, XSP.Brush[xbMenuGlyphBackground]);

    s   := aMenuItem.Caption;
    rc  := aMenuItem.CaptionRC;

    if aMenuItem.Enabled then
      SetTextColor(aDC, XSP.RGB[xcMenuText])
    else
      SetTextColor(aDC, XSP.RGB[xcMenuDisabledText]);

    DrawTextEx(aDC, PChar(s), Length(s), rc, DT_SINGLELINE or DT_VCENTER or DT_LEFT, NIL);
  end;


  procedure TXSPMenuWindow.Show;
  begin
    if Assigned(self) then
      ShowWindow(Handle, SW_SHOWNOACTIVATE);
  end;






  procedure TXSPMenuWindow.WndProc(var aMessage: TMessage);
  var
    WMLButtonUp: TWMLButtonUp absolute aMessage;
    i: Integer;
    idx: Integer;
    pos: TPoint;
    rc: TRect;
  begin
    case aMessage.Msg of
      XM_SELECTMENUITEM : begin
                            SendMessage(Form.Handle, XM_DISMISSMENU, 0, 0);
                            fItems[aMessage.lParam].Source.Click;
                          end;

      WM_LBUTTONDOWN    : begin
                            SetCapture(Handle);
                          end;

      WM_LBUTTONUP      : begin
                            idx := -1;
                            pos := POINT(WMLButtonUp.XPos, WMLButtonUp.YPos);

                            for i := 0 to Pred(Length(fItems)) do
                            begin
                              if fItems[i].Source.Caption = '-' then
                                CONTINUE;

                              rc := fItems[i].ItemRC;
                              Inc(rc.Bottom);
                              if PtInRect(rc, pos) then
                              begin
                                idx := i;
                                BREAK;
                              end;
                            end;

                            if (idx <> -1) then
                              PostMessage(Handle, XM_SELECTMENUITEM, 0, i)
                            else if (GetCapture = Handle) then
                            begin
                              PostMessage(Form.Handle, XM_DISMISSMENU, 0, i);
                              ReleaseCapture;
                            end;
                          end;

      WM_MOUSEACTIVATE  : begin
                            aMessage.Result := MA_NOACTIVATE;
                          end;
    else
      inherited WndProc(aMessage);
    end;
  end;






end.
