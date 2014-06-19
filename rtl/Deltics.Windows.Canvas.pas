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

  unit Deltics.Windows.Canvas;


interface

  uses
    Graphics,
    Windows;


  type
    TDCCanvas = class(TCanvas)
    private
      function get_Handle: HDC;
      procedure set_Handle(const aValue: HDC);
    protected
      procedure CreateHandle; override;
    public
      constructor Create(const aHandle: HDC);
      destructor Destroy; override;
      procedure Draw(const aGraphic: TGraphic; const aRect: TRect);

      property Handle: HDC read get_Handle write set_Handle;
    end;



implementation

  uses
    SysUtils;


  type
    TGraphicHelper = class(TGraphic);

    TCanvasCrack = class(TCustomCanvas)
    private
      fHandle: HDC;
      fState: TCanvasState;
    end;


  constructor TDCCanvas.Create(const aHandle: HDC);
  begin
    inherited Create;

    Handle := aHandle;
  end;


  destructor TDCCanvas.Destroy;
  begin
    Handle := 0;

    inherited Destroy;
  end;


  procedure TDCCanvas.Draw(const aGraphic: TGraphic;
                           const aRect: TRect);
  var
    omode: Integer;
  begin
    omode := GetBkMode(Handle);
    try
      TGraphicHelper(aGraphic).Draw(self, aRect);

    finally
      SetBkMode(Handle, omode);
    end;
  end;


  function TDCCanvas.get_Handle: HDC;
  begin
    result := TCanvasCrack(self).fHandle;
  end;


  procedure TDCCanvas.set_Handle(const aValue: HDC);
  begin
    if (Handle = aValue) then
      EXIT;

    TCanvasCrack(self).fHandle := aValue;

    if (aValue = 0) then
      Exclude(TCanvasCrack(self).fState, csHandleValid)
    else
      Include(TCanvasCrack(self).fState, csHandleValid);
  end;


  procedure TDCCanvas.CreateHandle;
  begin
    raise Exception.Create('TDCCanvas error: Handle (HDC) has not been assigned');
  end;




end.
