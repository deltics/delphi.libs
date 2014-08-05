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

  unit Deltics.XSP.Forms;


interface

  uses
    Classes,
    Controls,
    Forms,
    Graphics,
    Menus,
    Messages,
    Windows,
    Deltics.XSP.Activities,
    Deltics.XSP.Forms.NC;


  const
    XM_DISMISSMENU      = WM_USER + 1;
    XM_SELECTMENUITEM   = WM_USER + 2;
    XM_UPDATECLIENTAREA = WM_USER + 3;


  type
    TCustomXSPForm = class;
    TXSPForm = class;

    TCustomXSPForm = class(Forms.TForm)
    private
      fCurrentActivity: TXSPActivity;
      fNonClient: TXSPNonClientArea;
      fNonFullRect: TRect;
      fNonFullState: TWindowState;
      fNormalRect: TRect;
      function get_FullScreen: Boolean;
      procedure set_FullScreen(const aValue: Boolean);

    protected
      procedure CreateParams(var aParams: TCreateParams); override;
      procedure CreateWnd; override;
      procedure SetActivityMode(const aEnabled: Boolean);
      procedure XMDismissMenu(var aMessage: TMessage); message XM_DISMISSMENU;
      procedure XMUpdateClientArea(var aMessage: TMessage); message XM_UPDATECLIENTAREA;

      procedure WndProc(var aMessage: TMessage); override;
    public
      constructor Create(aOwner: TComponent); override;
      procedure Maximize;
      procedure Minimize;
      procedure Restore;
      procedure StartActivity(const aActivity: TXSPActivity);
      procedure StopActivity;
      property CurrentActivity: TXSPActivity read fCurrentActivity;
      property FullScreen: Boolean read get_FullScreen write set_FullScreen;
    end;


    TXSPForm = class(TCustomXSPForm);


implementation

  uses
    Deltics.Strings,
    Deltics.XSP,
    Deltics.XSP.Menus,
    Deltics.XSP.Themes,
    Deltics.XSP.Themes.Colors;



{ TCustomXSPForm ------------------------------------------------------------------------------ }

  constructor TCustomXSPForm.Create(aOwner: TComponent);
  begin
    inherited Create(aOwner);
  end;


  procedure TCustomXSPForm.CreateParams(var aParams: TCreateParams);
  begin
    inherited;

    // Turn off system border gadgets - we will manage these ourselves.
    //  This has implications which we will have to deal with.
    //
    // e.g. (to give just one example) with no WS_CAPTION style, the form
    //  will extend under any taskbar if maximized normally.
    //
    // We have to deal with all such problems ourselves.  Them's the breaks.

    aParams.Style := aParams.Style and NOT (WS_CAPTION)
                                   and NOT (WS_MINIMIZEBOX)
                                   and NOT (WS_MAXIMIZEBOX)
                                   and NOT (WS_SYSMENU);

    fNonClient.UpdateCaptionControls;
  end;


  procedure TCustomXSPForm.CreateWnd;
  begin
    if NOT Assigned(fNonClient) then
      fNonClient := TXSPNonClientArea.Create(self);

    inherited;
  end;


  function TCustomXSPForm.get_FullScreen: Boolean;
  begin
    result := fNonClient.IsFullScreen;
  end;


  procedure TCustomXSPForm.Maximize;
  begin
    if FullScreen then
      FullScreen := FALSE;

    fNormalRect := BoundsRect;
    WindowState := wsMaximized;

    with Screen.MonitorFromWindow(Handle).WorkareaRect do
      SetBounds(Left, Top, Right - Left, Bottom - Top);
  end;


  procedure TCustomXSPForm.Minimize;
  begin
    WindowState := wsMinimized
  end;


  procedure TCustomXSPForm.Restore;
  begin
    WindowState := wsNormal;
    SetBounds(fNormalRect.Left, fNormalRect.Top, fNormalRect.Right - fNormalRect.Left, fNormalRect.Bottom - fNormalRect.Top);
  end;


  procedure TCustomXSPForm.SetActivityMode(const aEnabled: Boolean);
  begin
    fNonClient.ActivityMode := aEnabled;
  end;


  procedure TCustomXSPForm.set_FullScreen(const aValue: Boolean);
  begin
    if (FullScreen = aValue) then
      EXIT;

    fNonClient.IsFullScreen := aValue;

    if ([csDesigning, csLoading] * ComponentState <> []) then
      EXIT;

    case aValue of
      TRUE  : begin
                fNonFullRect  := self.BoundsRect;
                fNonFullState := self.WindowState;
                SetBounds(0, 0, Screen.MonitorFromWindow(Handle).Width, Screen.MonitorFromWindow(Handle).Height);

                SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0,
                             SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
              end;

      FALSE : begin
                self.WindowState := fNonFullState;
                with fNonFullRect do
                  self.SetBounds(Left, Top, Right - Left + 1, Bottom - Top + 1);

                // TODO: Check whether we can just turn this off or need to preserve and restore as appropriate

                SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
                             SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER);
              end;
    end;
  end;


  procedure TCustomXSPForm.StartActivity(const aActivity: TXSPActivity);
  begin
    fCurrentActivity := aActivity;
    SetActivityMode(Assigned(aActivity));
  end;


  procedure TCustomXSPForm.StopActivity;
  begin
    fNonClient.ActivityMode := FALSE;
    fCurrentActivity := NIL;
  end;


  procedure TCustomXSPForm.WndProc(var aMessage: TMessage);
  var
    NCCalcSize: TWMNCCalcSize absolute aMessage;
    NCMouseMove: TWMNCMouseMove absolute aMessage;
    WindowPosChanging: TWMWindowPosChanging absolute aMessage;
    context: TTrackingContext;
    control: TCaptionControl;
    menu: PXSPMenuItem;
    area: TNCFrameArea;
    mousePos: TPoint;
    tme: TTrackMouseEvent;
    rc: TRect;
  begin
    case aMessage.Msg of
      WM_ACTIVATE           : begin
                                if (aMessage.WParam = WA_INACTIVE) then
                                begin
                                  // TODO: To prevent flicker, if the window being activated
                                  //        is one of our menu windows then we don't dismiss
                                  //        anything at all...

                                  fNonClient.DismissMenu;
                                end;

                                inherited WndProc(aMessage);

                                if (WindowState <> wsMinimized) then
                                  Invalidate;

                                if (XSP.Color[xcActiveFrame] <> XSP.Color[xcInactiveFrame])
                                 or (XSP.Color[xcActiveCaptionText] <> XSP.Color[xcInactiveCaptionText]) then
                                  fNonClient.Paint;
                              end;

      WM_CONTEXTMENU        : begin
                                inherited WndProc(aMessage);
                              end;

      WM_NCACTIVATE         : begin
                                if (WindowState <> wsMinimized) then
                                begin
                                  Invalidate;
                                  aMessage.Result := -1;
                                end
                                else
                                  inherited WndProc(aMessage);
                              end;

      WM_NCCALCSIZE         : begin
                                fNonClient.CalculateClientArea(NCCalcSize.CalcSize_Params.rgrc0);

                                // This is very odd.  From a reading of the WM_NCCALCSIZE
                                //  documentation, the code below should be correct but result
                                //  in client area's not correctly reflecting changes to
                                //  window frame/border size in display settings.  Instead
                                //  the naive change above, to always set the client area
                                //  in rgrc0, does seem to do the job.
                                //
                                // But it shouldn't, should it ?  For now, it works.
                              (*
                                if NCCalcSize.CalcValidRects then
                                begin
                                  with NCCalcSize.CalcSize_Params^ do
                                  begin
                                    rgrc2 := rgrc1;
                                    rgrc1 := rgrc0;
                                    fNonClient.CalculateClientArea(rgrc0);
                                  end;
                                  aMessage.Result := WVR_VALIDRECTS;
                                end
                                else
                                begin
                                  fNonClient.CalculateClientArea(PRect(aMessage.LParam)^);
                                  aMessage.Result := 0;
                                end;
                              *)
                              end;

      WM_NCHITTEST          : begin
                                mousePos.X := NCMouseMove.XCursor - Left;
                                mousePos.Y := NCMouseMove.YCursor - Top;

                                if fNonClient.NCTrackMouse(mousePos, context, control, menu, area, tmUpdate) then
                                begin
                                  if (context <> tcMenu) then
                                    fNonClient.DismissMenu;

                                  case context of
                                    tcControl : case control of
                                                  ccClose     : aMessage.Result := HTCLOSE;
                                                  ccFullScreen: aMessage.Result := HTMAXBUTTON;
                                                  ccMinimize  : aMessage.Result := HTMINBUTTON;
                                                  ccMaximize  : aMessage.Result := HTMAXBUTTON;
                                                  ccSysIcon   : aMessage.Result := HTSYSMENU;
                                                end;

                                    tcMenu    : aMessage.Result := HTMENU;

                                    tcArea    : case area of
                                                  faCaption         : aMessage.Result := HTCAPTION;
                                                  faMenuStrip       : aMessage.Result := HTMENU;
                                                  faTopGrab         : aMessage.Result := HTTOP;
                                                  faBottomGrab      : aMessage.Result := HTBOTTOM;
                                                  faLeftGrab        : aMessage.Result := HTLEFT;
                                                  faRightGrab       : aMessage.Result := HTRIGHT;
                                                  faTopLeftGrab     : aMessage.Result := HTTOPLEFT;
                                                  faTopRightGrab    : aMessage.Result := HTTOPRIGHT;
                                                  faBottomLeftGrab  : aMessage.Result := HTBOTTOMLEFT;
                                                  faBottomRightGrab : aMessage.Result := HTBOTTOMRIGHT;
                                                else
                                                // Re-enable the call to inherited to restore
                                                //  default processing for development/debug.

                                                //  inherited WndProc(aMessage);
                                                end;
                                  end;
                                end;
                                //else
                                  //inherited WndProc(aMessage);

                                if fNonClient.IsFullScreen and (aMessage.Result = HTCAPTION) then
                                  aMessage.Result := HTNOWHERE;
                              end;

      WM_NCLBUTTONDBLCLK    : begin
                                mousePos.X := NCMouseMove.XCursor - Left;
                                mousePos.Y := NCMouseMove.YCursor - Top;

                                if fNonClient.NCTrackMouse(mousePos, context, control, menu, area, tmStop) then
                                begin
                                  case context of
                                    tcControl : case control of
                                                  ccSysIcon   : Close;
                                                end;

                                    tcArea    : case area of
                                                  faCaption : if fNonClient.IsFullScreen then
                                                                fNonClient.IsFullScreen := FALSE
                                                              else if (WindowState = wsMaximized) then
                                                                Restore
                                                              else
                                                                Maximize;
                                                end;
                                  end;
                                end
                                else
                                  inherited WndProc(aMessage);
                              end;

      WM_NCLBUTTONDOWN      : begin
                                mousePos.X := NCMouseMove.XCursor - Left;
                                mousePos.Y := NCMouseMove.YCursor - Top;

                                if fNonClient.NCTrackMouse(mousePos, context, control, menu, area, tmStart) then
                                begin
                                  case context of
                                    tcMenu  : if menu.Enabled then
                                                fNonClient.DropDownMenu(menu);

                                    tcArea  : begin
                                                fNonClient.DismissMenu;
                                                inherited WndProc(aMessage);
                                              end;
                                  end;
                                end
                                else
                                begin
                                  fNonClient.DismissMenu;
                                  inherited WndProc(aMessage);
                                end;
                              end;

      WM_NCLBUTTONUP        : begin
                                tme.cbSize    := sizeof(tme);
                                tme.dwFlags   := TME_CANCEL or TME_HOVER or TME_LEAVE or TME_NONCLIENT;
                                tme.hwndTrack := Handle;
                                TrackMouseEvent(tme);

                                mousePos.X := NCMouseMove.XCursor - Left;
                                mousePos.Y := NCMouseMove.YCursor - Top;

                                if fNonClient.NCTrackMouse(mousePos, context, control, menu, area, tmStop) then
                                begin
                                  case context of
                                    tcControl : case control of
                                                  ccClose       : Close;

                                                  ccFullScreen  : FullScreen := NOT FullScreen; // TODO

                                                  ccMaximize    : if (WindowState <> wsMaximized) or FullScreen then
                                                                    Maximize
                                                                  else
                                                                    Restore;

                                                  ccMinimize    : Minimize;
                                                end;
                                  end;
                                end
                                else
                                  inherited WndProc(aMessage);
                              end;

      WM_NCMOUSELEAVE       : begin
                                fNonClient.NCTrackMouse(mousePos);
                              end;

      WM_NCMOUSEMOVE        : begin
                                tme.cbSize    := sizeof(tme);
                                tme.dwFlags   := TME_HOVER or TME_LEAVE or TME_NONCLIENT;
                                tme.hwndTrack := Handle;
                                TrackMouseEvent(tme);

                                mousePos.X := NCMouseMove.XCursor - Left;
                                mousePos.Y := NCMouseMove.YCursor - Top;
                                if NOT fNonClient.NCTrackMouse(mousePos) then
                                  inherited WndProc(aMessage);
                              end;

      WM_NCPAINT            : fNonClient.Paint;

      WM_LBUTTONDOWN        : begin
                                fNonClient.DismissMenu;
                                inherited WndProc(aMessage);
                              end;

      WM_LBUTTONUP          : begin
                                tme.cbSize    := sizeof(tme);
                                tme.dwFlags   := TME_CANCEL or TME_HOVER or TME_LEAVE or TME_NONCLIENT;
                                tme.hwndTrack := Handle;
                                TrackMouseEvent(tme);

                                fNonClient.NCTrackMouse(mousePos, context, control, menu, area, tmStop);

                                inherited WndProc(aMessage);
                              end;

      WM_MOUSEMOVE          : begin
                                tme.cbSize    := sizeof(tme);
                                tme.dwFlags   := TME_HOVER or TME_LEAVE or TME_NONCLIENT;
                                tme.hwndTrack := Handle;
                                TrackMouseEvent(tme);

                                mousePos.X := NCMouseMove.XCursor - Left;
                                mousePos.Y := NCMouseMove.YCursor - Top;
                                fNonClient.NCTrackMouse(mousePos);

                                inherited WndProc(aMessage);
                              end;

      WM_SETTINGCHANGE      : begin
                                inherited WndProc(aMessage);
                                if (aMessage.WParam = SPI_SETNONCLIENTMETRICS) then
                                  fNonClient.UpdateMetrics;
                              end;

      WM_SIZE               : begin
                                SetWindowRgn(Handle, CreateRectRgnIndirect(fNonClient.NCCalc), TRUE);
                                inherited WndProc(aMessage);
                              end;

      WM_WINDOWPOSCHANGED   : begin
                                with WindowPosChanging.WindowPos^ do
                                begin
                                  // If the window has moved then any menu windows calculated
                                  //  by the non-client area are no longer valid
                                  if (x <> Left) or (y <> Top) then
                                  begin
                                    fNonClient.InvalidateMenus;
                                    Invalidate;
                                  end;
                                end;

                                inherited WndProc(aMessage);
                              end;

      WM_WINDOWPOSCHANGING  : begin
                                with WindowPosChanging.WindowPos^ do
                                begin
                                  // If the window has changed size then we invalidate the
                                  //  window, causing the non-client area to recalculate.
                                  if (cx <> Width) or (cy <> Height) then
                                    Invalidate;
                                end;

                                inherited WndProc(aMessage);
                              end;

    else
      if (aMessage.Msg = XSP.XM_THEMECHANGE) then
      begin
        XSP.ChangeTheme(aMessage.WParam);

        fNonClient.UpdateClientArea;
        fNonClient.InvalidateMenus;
      end
      else
        inherited WndProc(aMessage);
    end;
  end;


  procedure TCustomXSPForm.XMDismissMenu(var aMessage: TMessage);
  begin
    fNonClient.DismissMenu;
  end;


  procedure TCustomXSPForm.XMUpdateClientArea(var aMessage: TMessage);
  begin
    // TODO: Find a way to trigger NCCALCSIZE directly.  For now, we
    //        have to make do with a bit of fast-shoe-shuffle, jiggling
    //        the window size to force the issue.

    SetWindowPos(Handle, 0, Left, Top, Width, Height + 1, SWP_NOMOVE);
    SetWindowPos(Handle, 0, Left, Top, Width, Height - 1, SWP_NOMOVE);
  end;



end.
