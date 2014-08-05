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

  unit Deltics.XSP.Themes;


interface

  uses
    Deltics.XSP.Themes.Colors;


  type
    TXSPThemeElement  = (
                         xeMenuBarBorder
                        );
    TXSPThemeElements = set of TXSPThemeElement;

    TXSPThemeMenuStyle = (
                          xmTopBarWrap,
                          xmTopBar,
                          xmSideBar
                         );


    TXSPTheme = class
    private
      fColor: TXSPColors;
      fColorRef: PXSPColors;
      fElements: TXSPThemeElements;

      fName: String;
      function get_ID: Integer;
    protected
      procedure Init; virtual; abstract;
    public
      constructor Create;
      property Color: PXSPColors read fColorRef;
      property ID: Integer read get_ID;
      property Name: String read fName;
      property Elements: TXSPThemeElements read fElements;
    end;


    TXSPThemes = class
      class function Theme(const aIndex: Integer): TXSPTheme;
      class function ThemeCount: Integer;
      class function Default: TXSPTheme;
    end;




implementation

  uses
    Graphics,
    Windows;


  var
    _default: TXSPTheme;
    _themes: array of TXSPTheme;



{ TXSPTheme }

  constructor TXSPTheme.Create;
  begin
    inherited Create;

    Init;
    fColorRef := @fColor;

    if fColor[xcInactiveCaptionText] = clNone then fColor[xcInactiveCaptionText] := fColor[xcActiveCaptionText];
    if fColor[xcInactiveFrame] = clNone then fColor[xcInactiveFrame] := fColor[xcActiveFrame];
  end;






  type
    TSystemTheme = class(TXSPTheme)
    protected
      procedure Init; override;
    end;


  procedure TSystemTheme.Init;
  begin
    fName := '<system>';

    fColor[xcBorder]                  := clActiveBorder;
    fColor[xcActiveCaptionText]       := clCaptionText;
    fColor[xcActiveFrame]             := clActiveCaption;
    fColor[xcInactiveCaptionText]     := clInactiveCaptionText;
    fColor[xcInactiveFrame]           := clInactiveCaption;
    fColor[xcGadgetClose]             := clMaroon;
    fColor[xcGadgetCloseGlyph]        := clWhite;
    fColor[xcGadgetMinimize]          := clNone;
    fColor[xcGadgetMinimizeGlyph]     := clBlack;
    fColor[xcGadgetMaximize]          := clNone;
    fColor[xcGadgetMaximizeGlyph]     := clBlack;
    fColor[xcMenuBackground]          := clMenu;
    fColor[xcMenuBorder]              := clNone;
    fColor[xcMenuDropShadow]          := clActiveBorder;
    fColor[xcMenuGlyphBackground]     := clNone;
    fColor[xcMenuSeparator]           := clBtnShadow;
    fColor[xcMenuText]                := clMenuText;
    fColor[xcMenuDisabledText]        := clGrayText;

    fColor[xcHotGadgetClose]          := clRed;
    fColor[xcHotGadgetCloseGlyph]     := clWhite;
    fColor[xcHotGadgetMaximize]       := clNavy;
    fColor[xcHotGadgetMaximizeGlyph]  := clWhite;
    fColor[xcHotGadgetMinimize]       := clNavy;
    fColor[xcHotGadgetMinimizeGlyph]  := clWhite;
    fColor[xcHotMenuBackground]       := clMenuHighlight;
    fColor[xcHotMenuText]             := clHighlightText;
  end;




  type
    TArcticTheme = class(TXSPTheme)
    protected
      procedure Init; override;
    end;


  procedure TArcticTheme.Init;
  begin
    fName := 'Arctic';

    fColor[xcBorder]                  := clBlack;
    fColor[xcActiveCaptionText]       := clBlack;
    fColor[xcInactiveCaptionText]     := RGB(127, 127, 127);
    fColor[xcActiveFrame]             := clWhite;
    fColor[xcInactiveFrame]           := RGB(230, 230, 230);
    fColor[xcGadgetClose]             := clMaroon;
    fColor[xcGadgetCloseGlyph]        := clWhite;
    fColor[xcGadgetMinimize]          := clNone;
    fColor[xcGadgetMinimizeGlyph]     := clBlack;
    fColor[xcGadgetMaximize]          := clNone;
    fColor[xcGadgetMaximizeGlyph]     := clBlack;
    fColor[xcMenuBorder]              := clGray;
    fColor[xcMenuBackground]          := RGB(250, 250, 255);
    fColor[xcMenuDropShadow]          := clSilver;
    fColor[xcMenuText]                := clBlack;
    fColor[xcMenuDisabledText]        := clGrayText;
    fColor[xcMenuGlyphBackground]     := RGB(220, 220, 220);

    fColor[xcHotGadgetClose]          := clRed;
    fColor[xcHotGadgetCloseGlyph]     := clWhite;
    fColor[xcHotGadgetMaximize]       := clHighlight;
    fColor[xcHotGadgetMaximizeGlyph]  := clHighlightText;
    fColor[xcHotGadgetMinimize]       := clHighlight;
    fColor[xcHotGadgetMinimizeGlyph]  := clHighlightText;
    fColor[xcHotMenuBackground]       := RGB(220, 220, 255);
    fColor[xcHotMenuText]             := clBlack;
  end;



  type
    TDustTheme = class(TXSPTheme)
    protected
      procedure Init; override;
    end;


  procedure TDustTheme.Init;
  var
    clGlyphText: TColor;
  begin
    fName := 'Dust';

    clGlyphText := RGB(127, 110, 100);

    fColor[xcBorder]                  := RGB(192, 192, 192);
    fColor[xcActiveCaptionText]       := clBlack;
    fColor[xcActiveFrame]             := RGB(245, 242, 240);
    fColor[xcInactiveCaptionText]     := RGB(127, 127, 127);
    fColor[xcInactiveFrame]           := RGB(250, 250, 250);
    fColor[xcGadgetClose]             := RGB(240, 225, 220);
    fColor[xcGadgetCloseGlyph]        := clGlyphText;
    fColor[xcGadgetFullScreen]        := clNone;
    fColor[xcGadgetFullScreenGlyph]   := clGlyphText;
    fColor[xcGadgetMinimize]          := clNone;
    fColor[xcGadgetMinimizeGlyph]     := clGlyphText;
    fColor[xcGadgetMaximize]          := clNone;
    fColor[xcGadgetMaximizeGlyph]     := clGlyphText;
    fColor[xcMenuBorder]              := clGlyphText;
    fColor[xcMenuBackground]          := clWhite;
    fColor[xcMenuDropShadow]          := RGB(227, 227, 192);
    fColor[xcMenuText]                := clBlack;
    fColor[xcMenuDisabledText]        := clSilver;
    fColor[xcMenuGlyphBackground]     := RGB(227, 227, 227);

    fColor[xcHotGadgetClose]          := RGB(240, 200, 190);
    fColor[xcHotGadgetCloseGlyph]     := clBlack;
    fColor[xcHotGadgetMaximize]       := RGB(220, 220, 240);
    fColor[xcHotGadgetMaximizeGlyph]  := clBlack;
    fColor[xcHotGadgetMinimize]       := RGB(220, 220, 240);
    fColor[xcHotGadgetMinimizeGlyph]  := clBlack;
    fColor[xcHotMenuBackground]       := RGB(127, 127, 192);
    fColor[xcHotMenuText]             := clBlack;

    fElements := [xeMenuBarBorder];
  end;



  type
    TMidnightTheme = class(TXSPTheme)
    protected
      procedure Init; override;
    end;


  procedure TMidnightTheme.Init;
  begin
    fName := 'Midnight';

    fColor[xcBorder]                  := RGB(0, 0, 16);
    fColor[xcActiveCaptionText]       := clWhite;
    fColor[xcActiveFrame]             := clBlack;
    fColor[xcGadgetClose]             := clNavy;
    fColor[xcGadgetCloseGlyph]        := clWhite;
    fColor[xcGadgetMinimize]          := clBlack;
    fColor[xcGadgetMinimizeGlyph]     := clWhite;
    fColor[xcGadgetMaximize]          := clBlack;
    fColor[xcGadgetMaximizeGlyph]     := clWhite;
    fColor[xcMenuBorder]              := clBlack;
    fColor[xcMenuBackground]          := clBlack;
    fColor[xcMenuDropShadow]          := clNone;
    fColor[xcMenuText]                := clWhite;
    fColor[xcMenuDisabledText]        := clGray;
    fColor[xcMenuGlyphBackground]     := RGB(64, 64, 64);

    fColor[xcHotGadgetClose]          := clBlue;
    fColor[xcHotGadgetCloseGlyph]     := clWhite;
    fColor[xcHotGadgetMaximize]       := RGB(64, 64, 64);
    fColor[xcHotGadgetMaximizeGlyph]  := clWhite;
    fColor[xcHotGadgetMinimize]       := RGB(64, 64, 64);
    fColor[xcHotGadgetMinimizeGlyph]  := clWhite;
    fColor[xcHotMenuBackground]       := clNavy;
    fColor[xcHotMenuText]             := clWhite;
  end;


  function TXSPTheme.get_ID: Integer;
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(_themes)) do
      if (_themes[i] = self) then
      begin
        result := i;
        EXIT;
      end;

    result := -1;  // NOT FOUND... a.k.a WTF ?
  end;




{ TXSPThemes }

  class function TXSPThemes.Default: TXSPTheme;
  begin
    result := _default;
  end;


  class function TXSPThemes.Theme(const aIndex: Integer): TXSPTheme;
  begin
    result := _themes[aIndex];
  end;


  class function TXSPThemes.ThemeCount: Integer;
  begin
    result := Length(_themes);
  end;



{ TSystemTheme }


initialization

  SetLength(_themes, 4);
  _themes[0] := TSystemTheme.Create;
  _themes[1] := TArcticTheme.Create;
  _themes[2] := TDustTheme.Create;
  _themes[3] := TMidnightTheme.Create;

  _default := _themes[0];

finalization
  // There could be references to themes during finalization so we will
  //  simply allow tht themes to die and get collected when our process
  //  dies.  Yes, a memory "leak", but not an important one

end.


