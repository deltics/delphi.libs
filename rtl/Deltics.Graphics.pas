{
  * X11 (MIT) LICENSE *

  Copyright © 2008 Jolyon Smith

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

{$i deltics.rtl.inc}

{$ifdef deltics_graphics}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Graphics;


interface

  uses
  { vcl: }
    Graphics,
    Windows;


  type
    TColorID = 0..51;
    TColorSet = record Min: TColorID; Max: TColorID; end;


  const
    csNone      : TColorSet = (Min:  0; Max: 1);
    csStandard  : TColorSet = (Min:  2; Max: 17);
    csExtended  : TColorSet = (Min: 18; Max: 21);
    csSystem    : TColorSet = (Min: 22; Max: 51);
    csWeb       : TColorSet = (Min:  0; Max:  0);


  function ColorToString(Color: TColor): String;
  function StringToColor(const S: String): TColor;
  function ColorToIdent(Color: Longint; var Ident: String): Boolean;
  function IdentToColor(const Ident: String; var Color: Longint): Boolean;

  procedure GetColorInfo(const aColorID: TColorID; var aName: String; var aColor: TColor);

  function MidPoint(const aRect: TRect): TPoint;
  procedure GetMidPoint(const aRect: TRect; var aX, aY: Integer);

  function AddRGB(const aBaseColor, aColor: TColor): TColor;
  function InverseRGB(const aColor: TColor): TColor;
  function SubtractRGB(const aBaseColor, aColor: TColor): TColor;
  function TweenRGB(const aColorA, aColorB: TColor): TColor;

  function RGBDarker(const aColor: TColor; const aIntensity: Integer = 50): TColor;
  function RGBLighter(const aColor: TColor; const aIntensity: Integer = 50): TColor;

  procedure Grayscale(const aGraphic: TGraphic;
                      const aMask: Graphics.TBitmap = NIL;
                      const aBitmap: Graphics.TBitmap = NIL;
                      const aContrast: Integer = 0);


  function CentredRect(const aBoundingRect: TRect;
                       const aWidth, aHeight: Integer): TRect;
  function NormalRect(aRect: TRect): TRect ;
  procedure PositionRect(var aRect: TRect; const aX, aY: Integer);
  procedure TranslateRect(var aRect: TRect; const aDX, aDY: Integer);

  procedure DrawGlyph(const aCanvas: TCanvas;
                      const aRect: TRect;
                      const aGlyph: TGraphic;
                      const aStretch: Boolean = FALSE;
                      const aBorder: Integer = 4);
  procedure DrawLine(const aCanvas: TCanvas;
                     const aPoints: array of TPoint;
                     const aOffsetX: Integer = 0;
                     const aOffsetY: Integer = 0);

  procedure DrawRect(const aCanvas: TCanvas;
                     const aRect: TRect);

  procedure DrawTransparent(const aCanvas: TCanvas;
                            const aRect: TRect;
                            const aImage: Graphics.TBitmap;
                            const aMask: Graphics.TBitmap);


implementation

  uses
  { vcl: }
    Classes,
    Math,
    SysUtils,
    Themes;


  const
    Colors: array[TColorID] of TIdentMapEntry = (
                                                 { 0..1 = non-colors }
                                                 (Value: clNone;     Name: 'clNone'),
                                                 (Value: clDefault;  Name: 'clDefault'),

                                                 { 2..17 = standard colors }
                                                 (Value: clBlack;    Name: 'clBlack'),
                                                 (Value: clWhite;    Name: 'clWhite'),
                                                 (Value: clSilver;   Name: 'clSilver'),
                                                 (Value: clGray;     Name: 'clGray'),
                                                 (Value: clRed;      Name: 'clRed'),
                                                 (Value: clLime;     Name: 'clLime'),
                                                 (Value: clBlue;     Name: 'clBlue'),
                                                 (Value: clYellow;   Name: 'clYellow'),
                                                 (Value: clFuchsia;  Name: 'clFuchsia'),
                                                 (Value: clAqua;     Name: 'clAqua'),
                                                 (Value: clMaroon;   Name: 'clMaroon'),
                                                 (Value: clGreen;    Name: 'clGreen'),
                                                 (Value: clNavy;     Name: 'clNavy'),
                                                 (Value: clOlive;    Name: 'clOlive'),
                                                 (Value: clPurple;   Name: 'clPurple'),
                                                 (Value: clTeal;     Name: 'clTeal'),

                                                 { 18..21 = extended colors }
                                                 (Value: clMoneyGreen; Name: 'clMoneyGreen'),
                                                 (Value: clSkyBlue;    Name: 'clSkyBlue'),
                                                 (Value: clCream;      Name: 'clCream'),
                                                 (Value: clMedGray;    Name: 'clMedGray'),

                                                 { 22..51 = system colors }
                                                 (Value: clActiveBorder;             Name: 'clActiveBorder'),
                                                 (Value: clActiveCaption;            Name: 'clActiveCaption'),
                                                 (Value: clAppWorkSpace;             Name: 'clAppWorkSpace'),
                                                 (Value: clBackground;               Name: 'clBackground'),
                                                 (Value: clBtnFace;                  Name: 'clBtnFace'),
                                                 (Value: clBtnHighlight;             Name: 'clBtnHighlight'),
                                                 (Value: clBtnShadow;                Name: 'clBtnShadow'),
                                                 (Value: clBtnText;                  Name: 'clBtnText'),
                                                 (Value: clCaptionText;              Name: 'clCaptionText'),
                                                 (Value: clGradientActiveCaption;    Name: 'clGradientActiveCaption'),
                                                 (Value: clGradientInactiveCaption;  Name: 'clGradientInactiveCaption'),
                                                 (Value: clGrayText;                 Name: 'clGrayText'),
                                                 (Value: clHighlight;                Name: 'clHighlight'),
                                                 (Value: clHighlightText;            Name: 'clHighlightText'),
                                                 (Value: clHotLight;                 Name: 'clHotLight'),
                                                 (Value: clInactiveBorder;           Name: 'clInactiveBorder'),
                                                 (Value: clInactiveCaption;          Name: 'clInactiveCaption'),
                                                 (Value: clInactiveCaptionText;      Name: 'clInactiveCaptionText'),
                                                 (Value: clInfoBk;                   Name: 'clInfoBk'),
                                                 (Value: clInfoText;                 Name: 'clInfoText'),
                                                 (Value: clMenu;                     Name: 'clMenu'),
                                                 (Value: clMenuBar;                  Name: 'clMenuBar'),
                                                 (Value: clMenuHighlight;            Name: 'clMenuHighlight'),
                                                 (Value: clMenuText;                 Name: 'clMenuText'),
                                                 (Value: clScrollBar;                Name: 'clScrollBar'),
                                                 (Value: cl3DDkShadow;               Name: 'cl3DDkShadow'),
                                                 (Value: cl3DLight;                  Name: 'cl3DLight'),
                                                 (Value: clWindow;                   Name: 'clWindow'),
                                                 (Value: clWindowFrame;              Name: 'clWindowFrame'),
                                                 (Value: clWindowText;               Name: 'clWindowText')

                                                 { 52..?? = web colors }
                                                );


  function ColorToString(Color: TColor): String;
  begin
    if NOT ColorToIdent(Color, result) then
      FmtStr(result, '%s%.8x', [HexDisplayPrefix, Color]);
  end;


  function StringToColor(const S: string): TColor;
  begin
    if NOT IdentToColor(S, Longint(result)) then
      result := TColor(StrToInt(S));
  end;


  function ColorToIdent(Color: Longint; var Ident: String): Boolean;
  begin
    result := IntToIdent(Color, Ident, Colors);
  end;


  function IdentToColor(const Ident: String; var Color: Longint): Boolean;
  begin
    result := IdentToInt(Ident, Color, Colors);
  end;


  procedure GetColorInfo(const aColorID: TColorID; var aName: String; var aColor: TColor);
  begin
    with Colors[aColorID] do
    begin
      aName   := Name;
      aColor  := Value;
    end;
  end;


  function MidPoint(const aRect: TRect): TPoint;
  begin
    GetMidPoint(aRect, result.X, result.Y);
  end;


  procedure GetMidPoint(const aRect: TRect; var aX, aY: Integer);
  begin
    aX := aRect.Left + ((aRect.Right - aRect.Left) div 2);
    aY := aRect.Top + ((aRect.Bottom - aRect.Top) div 2);
  end;


  function AddRGB(const aBaseColor, aColor: TColor): TColor ;
  var
    col: TColor;
    r, g, b  : Byte ;
  begin
    col    := ColorToRGB(aColor);
    result := ColorToRGB(aBaseColor);

    r  := (GetRValue(result) + GetRValue(col)) div 2;
    g  := (GetGValue(result) + GetGValue(col)) div 2;
    b  := (GetBValue(result) + GetBValue(col)) div 2;

    result := RGB(r, g, b) ;
  end;


  function InverseRGB(const aColor: TColor): TColor ;
  var
    r, g, b  : Byte ;
  begin
    result := ColorToRGB(aColor) ;
    r  := GetRValue(result) ;
    g  := GetGValue(result) ;
    b  := GetBValue(result) ;
    result := RGB(255 - r, 255 - g, 255 - b) ;
  end ;


  function SubtractRGB(const aBaseColor, aColor: TColor): TColor ;
  var
    col: TColor;
    r, g, b  : Byte ;
  begin
    col    := ColorToRGB(aColor);
    result := ColorToRGB(aBaseColor);

    r  := Max(0, GetRValue(result) - GetRValue(col));
    g  := Max(0, GetGValue(result) - GetGValue(col));
    b  := Max(0, GetBValue(result) - GetBValue(col));

    result := RGB(r, g, b) ;
  end;


  function TweenRGB(const aColorA, aColorB: TColor): TColor ;
  var
    a, b: TColor;
    rA, gA, bA  : Byte ;
    rB, gB, bB  : Byte ;
  begin
    a := ColorToRGB(aColorA);
    b := ColorToRGB(aColorB);

    rA  := GetRValue(a);
    gA  := GetGValue(a);
    bA  := GetBValue(a);

    rB  := GetRValue(b);
    gB  := GetGValue(b);
    bB  := GetBValue(b);

    result := RGB((rA + rB) div 2,(gA + gB) div 2,(bA + bB) div 2);
  end ;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function CentredRect(const aBoundingRect: TRect;
                       const aWidth, aHeight: Integer): TRect;
  var
    rc: TRect;
    w, h: Integer;
    r: Double;
  begin
    rc := aBoundingRect;

    w := rc.Right - rc.Left + 1;
    h := rc.Bottom - rc.Top + 1;

    if (w < aWidth) or (h < aHeight) then
      r := Min(w / aWidth, h / aHeight)
    else if (w > aWidth) xor (h > aHeight) then
      r := Min(aWidth / w, aHeight / h)
    else
      r := 0;

    if r = 0 then
    begin
      w := aWidth;
      h := aHeight;
    end
    else
    begin
      w := Trunc(r * w);
      h := Trunc(r * h);
    end;

    result.Left   := rc.Left + ((rc.Right - rc.Left - w) div 2) - 1;
    result.Top    := rc.Top + ((rc.Bottom - rc.Top - h) div 2) - 1;
    result.Right  := rc.Left + w + 1;
    result.Bottom := rc.Top + h + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function NormalRect(aRect: TRect): TRect;
  begin
    result := aRect;

    if aRect.Left > aRect.Right then
    begin
      result.Left   := aRect.Right;
      result.Right  := aRect.Left;
    end ;

    if aRect.Top > aRect.Bottom then
    begin
      result.Top    := aRect.Bottom;
      result.Bottom	:= aRect.Top;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure PositionRect(var aRect: TRect;
                         const aX, aY: Integer);
  begin
    TranslateRect(aRect, aX - aRect.Left, aY - aRect.Top);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TranslateRect(var aRect: TRect;
                          const aDX, aDY: Integer);
  begin
    aRect.Left   := aRect.Left + aDX;
    aRect.Top    := aRect.Top + aDY;
    aRect.Right  := aRect.Right + aDX;
    aRect.Bottom := aRect.Bottom + aDY;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure DrawGlyph(const aCanvas: TCanvas;
                      const aRect: TRect;
                      const aGlyph: TGraphic;
                      const aStretch: Boolean;
                      const aBorder: Integer);
  var
    rc: TRect;
    w, h: Integer;
    r: Double;
  begin
    rc := aRect;
    InflateRect(rc, -aBorder, -aBorder);

    w := rc.Right - rc.Left;
    h := rc.Bottom - rc.Top;

    if (w < aGlyph.Width) or (h < aGlyph.Height) then
    begin
      r := Min(w / aGlyph.Width, h / aGlyph.Height);
      w := Trunc(r * w);
      h := Trunc(r * h);
    end
    else if NOT aStretch then
    begin
      w := aGlyph.Width;
      h := aGlyph.Height;
    end;

    rc.Left   := rc.Left + ((rc.Right - rc.Left - w) div 2);
    rc.Top    := rc.Top + ((rc.Bottom - rc.Top - h) div 2);
    rc.Right  := rc.Left + w;
    rc.Bottom := rc.Top + h;

    aGlyph.Transparent := TRUE;
    aCanvas.StretchDraw(rc, aGlyph);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure DrawLine(const aCanvas: TCanvas;
                     const aPoints: array of TPoint;
                     const aOffsetX, aOffsetY: Integer);
  var
    i: Integer;
  begin
    aCanvas.MoveTo(aPoints[0].X + aOffsetX, aPoints[0].Y + aOffsetY);
    for i := 1 to Pred(Length(aPoints)) do
      aCanvas.LineTo(aPoints[i].X + aOffsetX, aPoints[i].Y + aOffsetY);

    i := Pred(Length(aPoints)) - 1;
    aCanvas.LineTo(aPoints[i].X + aOffsetX, aPoints[i].Y + aOffsetY);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure DrawRect(const aCanvas: TCanvas;
                     const aRect: TRect);
  begin
    with aCanvas do
    begin
      MoveTo(aRect.Left,       aRect.Top);
      LineTo(aRect.Right - 1,  aRect.Top);
      LineTo(aRect.Right - 1,  aRect.Bottom - 1);
      LineTo(aRect.Left,       aRect.Bottom - 1);
      LineTo(aRect.Left,       aRect.Top);
    end
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure DrawTransparent(const aCanvas: TCanvas;
                            const aRect: TRect;
                            const aImage: Graphics.TBitmap;
                            const aMask: Graphics.TBitmap);
  var
    sw, sh: Integer;
    dw, dh: Integer;
  begin
    dw := aRect.Right - aRect.Left;
    dh := aRect.Bottom - aRect.Top;
    sw := aImage.Width;
    sh := aImage.Height;

    BitBlt(aCanvas.Handle, aRect.Left + ((dw - sw) div 2), aRect.Top + ((dw - sw) div 2), sw, sh,
                           aMask.Canvas.Handle,  0, 0, SRCAND);
    BitBlt(aCanvas.Handle, aRect.Left + ((dw - sw) div 2), aRect.Top + ((dh - sh) div 2), sw, sh,
                           aImage.Canvas.Handle, 0, 0, SRCPAINT);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function RGBDarker(const aColor: TColor; const aIntensity: Integer): TColor;
  var
    r, g, b: byte;
  begin
    r := GetRValue(ColorToRGB(aColor));
    g := GetGValue(ColorToRGB(aColor));
    b := GetBValue(ColorToRGB(aColor));

    r := ((100 - aIntensity) * r) div 100;
    g := ((100 - aIntensity) * g) div 100;
    b := ((100 - aIntensity) * b) div 100;

    result := RGB(r, g, b);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function RGBLighter(const aColor: TColor; const aIntensity: Integer): TColor;
  var
    r, g, b: byte;
  begin
    r := GetRValue(ColorToRGB(aColor));
    g := GetGValue(ColorToRGB(aColor));
    b := GetBValue(ColorToRGB(aColor));

    r := r + ((aIntensity * (255 - r)) div 100);
    g := g + ((aIntensity * (255 - g)) div 100);
    b := b + ((aIntensity * (255 - b)) div 100);

    result := RGB(r, g, b);
  end;




  procedure GrayScale(const aGraphic: TGraphic;
                      const aMask: Graphics.TBitmap;
                      const aBitmap: Graphics.TBitmap;
                      const aContrast: Integer) ;
  type
    TRGBColor = record
    case Integer of
      0 : (Color : TColor) ;
      1 : (Red, Green, Blue, Intensity : Byte) ;
      2 : (Colors : array[0..3] of Byte) ;
    end;

  var
    bmp, mask : Graphics.TBitmap;
    x, y , Avg : Integer;
    RGBColor : TRGBColor;
  begin
    if not Assigned(aGraphic) then
      Exit;

    mask := NIL;
    bmp  := Graphics.TBitmap.Create;
    try
      bmp.Height  := aGraphic.Height;
      bmp.Width   := aGraphic.Width;
      with bmp.Canvas do
      begin
        if (aGraphic is Graphics.TBitmap) then
          Brush.Color := (aGraphic as Graphics.TBitmap).TransparentColor
        else
          Brush.Color := clPurple;

        FillRect(ClipRect) ;
        Draw(0, 0, aGraphic) ;
      end;

      if NOT Assigned(aMask) then
      begin
        mask := Graphics.TBitmap.Create;
        mask.Assign(bmp) ;
        mask.Mask(bmp.TransparentColor) ;
      end
      else
        mask := aMask;

      with bmp.Canvas do
      begin
        for y := 0 to ClipRect.Bottom do
        begin
          for x := 0 to ClipRect.Right do
          begin
            if mask.Canvas.Pixels[x,y] = clBlack then
            begin
              RGBColor.Color := Pixels[x,y] ;
              with RGBColor do
              begin
                Avg := ((Red+Green+Blue) div 3) ;
                Inc(Avg, aContrast) ;
                if Avg > 255 then
                  Avg := 255
                else if Avg < 0 then
                  Avg := 0;

                Red   := Avg;
                Green := Avg;
                Blue  := Avg;
              end;

              Pixels[x,y] := RGBColor.Color;
            end;
          end;
        end;
      end;

      if Assigned(aBitmap) then
        aBitmap.Assign(bmp)
      else if aGraphic is Graphics.TBitmap then
        (aGraphic as Graphics.TBitmap).Assign(bmp);

    finally
      bmp.Free;

      if NOT Assigned(aMask) then
        mask.Free;
    end;
  end;




end.

