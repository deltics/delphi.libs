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

  unit Deltics.XSP;


interface

  uses
    Graphics,
    Windows,
    Deltics.XSP.Forms,
    Deltics.XSP.Menus.Popup,
    Deltics.XSP.Themes,
    Deltics.XSP.Themes.Colors;


  type
    TForm = class(TXSPForm);
    TPopupMenu = class(TXSPPopupMenu);


    TXSPBrush = (
                 xbBorder,
                 xbActiveFrame,
                 xbInactiveFrame,
                 xbGadgetClose,
                 xbGadgetFullScreen,
                 xbGadgetMaximize,
                 xbGadgetMinimize,
                 xbGadgetSettings,
                 xbGadgetHelp,
                 xbMenuFrame,
                 xbMenuBackground,
                 xbMenuBorder,
                 xbMenuDropShadow,
                 xbMenuGlyphBackground,
                 xbHotGadgetClose,
                 xbHotGadgetFullScreen,
                 xbHotGadgetMaximize,
                 xbHotGadgetMinimize,
                 xbHotGadgetSettings,
                 xbHotGadgetHelp,
                 xbHotMenuBackground
                );

    TXSPFont = (
                xfCaption,
                xfMarlett,
                xfMenu,
                xfToolCaption
               );

    TXSPPen = (
               xpMenuBorder,
               xpMenuDropShadow,
               xpMenuSeparator
              );

    TXSPBrushArray  = array[TXSPBrush] of HBRUSH;
    TXSPFontArray  = array[TXSPFont] of HFONT;
    TXSPPenArray    = array[TXSPPen] of HPEN;

    PBrushes = ^TXSPBrushArray;
    PFonts = ^TXSPFontArray;
    PPens = ^TXSPPenArray;


    TXSP = class
    private
      fBrush: TXSPBrushArray;
      fColor: TXSPColors;
      fPen: TXSPPenArray;
      fFont: TXSPFontArray;
      fBrushes: PBrushes;
      fColors: PXSPColors;
      fFonts: PFonts;
      fPens: PPens;
      fTheme: TXSPTheme;
      fXM_THEMECHANGE: Cardinal;
      function get_RGB(const aColor: TXSPColor): TColorRef;
      function get_ThemeID: Integer;
      procedure CreateBrush(const aBrush: TXSPBrush; const aColor: TColor);
      procedure CreateFont(const aFont: TXSPFont; const aName: String; const aSize: Integer; const aStyle: TFontStyles);
      procedure CreatePen(const aPen: TXSPPen; const aColor: TColor; const aStyle: TPenStyle = psSolid; const aWidth: Integer = 0);
      procedure Init;
      procedure Cleanup;
      procedure SetTheme(const aTheme: TXSPTheme);
    public
      constructor Create;
      destructor Destroy; override;
      procedure ApplyTheme(const aID: Integer);
      procedure ChangeTheme(const aID: Integer);
      procedure Refresh;
      property Brush: PBrushes read fBrushes;
      property Color: PXSPColors read fColors;
      property Font: PFonts read fFonts;
      property Pen: PPens read fPens;
      property RGB[const aColor: TXSPColor]: TColorRef read get_RGB;
      property Theme: TXSPTheme read fTheme;
      property XM_THEMECHANGE: Cardinal read fXM_THEMECHANGE;
    end;


  var
    XSP: TXSP = NIL;


implementation

  uses
    Classes,
    Forms,
    Menus,
    SysUtils;


{ TXSP }

  constructor TXSP.Create;
  const
    XM_THEMECHANGE: PANSIChar = 'deltics.xsp.XM_THEMECHANGE';
  begin
    if Assigned(XSP) then
      raise Exception.Create('XSP singleton already created');

    XSP := self;

    inherited Create;

    fXM_THEMECHANGE := RegisterWindowMessageA(XM_THEMECHANGE);

    SetTheme(TXSPThemes.Default);

    fBrushes  := @fBrush;
    fFonts    := @fFont;
    fPens     := @fPen;

    Init;
  end;


  destructor TXSP.Destroy;
  begin
    Cleanup;

    inherited;
  end;


  function TXSP.get_RGB(const aColor: TXSPColor): TColorRef;
  begin
    result := ColorToRGB(Color[aColor]);
  end;


  function TXSP.get_ThemeID: Integer;
  begin
    result := Theme.ID;
  end;


  procedure TXSP.CreateBrush(const aBrush: TXSPBrush;
                                  const aColor: TColor);
  begin
    if (fBrush[aBrush] <> 0) then
      DeleteObject(fBrush[aBrush]);

    if (aColor <> clNone) then
      fBrush[aBrush] := CreateSolidBrush(ColorToRGB(aColor))
    else
      fBrush[aBrush] := 0;
  end;


  procedure TXSP.CreateFont(const aFont: TXSPFont;
                            const aName: String;
                            const aSize: Integer;
                            const aStyle: TFontStyles);
  var
    lf: TLogFont;
    ppi: Integer;
  begin
    if fFont[aFont] <> 0 then
    begin
      DeleteObject(fFont[aFont]);
      fFont[aFont] := 0;
    end;

    ppi := GetDeviceCaps(GetDC(0), LOGPIXELSY);

    ZeroMemory(@lf, sizeof(lf));

    lf.lfHeight := -MulDiv(aSize, ppi, 72);

    if fsBold in aStyle then
      lf.lfWeight := FW_BOLD
    else
      lf.lfWeight := FW_NORMAL;

    if fsItalic in aStyle then
      lf.lfItalic := Byte(-1);

    if fsStrikeOut in aStyle then
      lf.lfStrikeOut := Byte(-1);

    if fsUnderline in aStyle then
      lf.lfUnderline := Byte(-1);

    lf.lfCharSet        := DEFAULT_CHARSET;
    lf.lfOutPrecision   := OUT_DEFAULT_PRECIS;
    lf.lfClipPrecision  := CLIP_DEFAULT_PRECIS;
    lf.lfQuality        := ANTIALIASED_QUALITY;
    lf.lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;

    StrPCopy(@lf.lfFaceName[0], PChar(aName));

    fFont[aFont] := CreateFontIndirect(lf);
  end;


  procedure TXSP.CreatePen(const aPen: TXSPPen;
                                const aColor: TColor;
                                const aStyle: TPenStyle;
                                const aWidth: Integer);
  const
    STYLE: array[TPenStyle] of Word = (PS_SOLID,
                                       PS_DASH,
                                       PS_DOT,
                                       PS_DASHDOT,
                                       PS_DASHDOTDOT,
                                       PS_NULL,
                                       PS_INSIDEFRAME,
                                       PS_USERSTYLE,
                                       PS_ALTERNATE);
  begin
    if (fPen[aPen] <> 0) then
      DeleteObject(fPen[aPen]);

    if (aColor <> clNone) then
      fPen[aPen] := Windows.CreatePen(STYLE[aStyle], aWidth, ColorToRGB(aColor))
    else
      fPen[aPen] := 0;
  end;


  procedure TXSP.Init;
  var
    captionY: Integer;
    menuY: Integer;
  begin
    CreateBrush(xbBorder,               Color[xcBorder]);
    CreateBrush(xbActiveFrame,          Color[xcActiveFrame]);
    CreateBrush(xbInactiveFrame,        Color[xcInactiveFrame]);
    CreateBrush(xbGadgetClose,          Color[xcGadgetClose]);
    CreateBrush(xbGadgetFullScreen,     Color[xcGadgetFullScreen]);
    CreateBrush(xbGadgetMinimize,       Color[xcGadgetMinimize]);
    CreateBrush(xbGadgetMaximize,       Color[xcGadgetMaximize]);
    CreateBrush(xbMenuBackground,       Color[xcMenuBackground]);
    CreateBrush(xbMenuBorder,           Color[xcMenuBorder]);
    CreateBrush(xbMenuDropShadow,       Color[xcMenuDropShadow]);
    CreateBrush(xbMenuGlyphBackground,  Color[xcMenuGlyphBackground]);

    CreateBrush(xbHotGadgetClose,       Color[xcHotGadgetClose]);
    CreateBrush(xbHotGadgetFullScreen,  Color[xcHotGadgetFullScreen]);
    CreateBrush(xbHotGadgetMaximize,    Color[xcHotGadgetMaximize]);
    CreateBrush(xbHotGadgetMinimize,    Color[xcHotGadgetMinimize]);
    CreateBrush(xbHotMenuBackground,    Color[xcHotMenuBackground]);

    CreatePen(xpMenuBorder,     Color[xcMenuBorder]);
    CreatePen(xpMenuDropShadow, Color[xcMenuDropShadow], psSolid, 1);
    CreatePen(xpMenuSeparator,  Color[xcMenuSeparator]);

    captionY  := GetSystemMetrics(SM_CYCAPTION);
    menuY     := GetSystemMetrics(SM_CYMENU);

    CreateFont(xfCaption,      'Arial',    captionY div 2,      []);
    CreateFont(xfMarlett,      'Marlett',  captionY div 2,      []);
    CreateFont(xfMenu,         'Arial',    2 * menuY div 5,     []);
    CreateFont(xfToolCaption,  'Arial',    2 * captionY div 5,  [fsBold]);
  end;


  procedure TXSP.Cleanup;
  var
    brush: TXSPBrush;
    font: TXSPFont;
    pen: TXSPPen;
  begin
    for brush := Low(brush) to High(brush) do
      if (fBrush[brush] <> 0) then
      begin
        DeleteObject(fBrush[brush]);
        fBrush[brush] := 0;
      end;

    for font := Low(font) to High(font) do
      if (fFont[font] <> 0) then
      begin
        DeleteObject(fFont[font]);
        fFont[font] := 0;
      end;

    for pen := Low(pen) to High(pen) do
      if (fPen[pen] <> 0) then
      begin
        DeleteObject(fPen[pen]);
        fPen[pen] := 0;
      end;
  end;


  procedure TXSP.Refresh;
  begin
    Cleanup;
    Init;
  end;


  procedure TXSP.SetTheme(const aTheme: TXSPTheme);
  begin
    if Assigned(aTheme) then
      fTheme  := aTheme
    else
      fTheme  := TXSPThemes.Default;

    fColors := fTheme.Color;
    Refresh;
  end;


  procedure TXSP.ApplyTheme(const aID: Integer);
  begin
    ChangeTheme(aID);
    PostMessage(HWND_BROADCAST, fXM_THEMECHANGE, aID, 0);
  end;


  procedure TXSP.ChangeTheme(const aID: Integer);
  begin
    SetTheme(TXSPThemes.Theme(aID));
  end;




initialization
  TXSP.Create;

finalization
  XSP.Free;
end.
