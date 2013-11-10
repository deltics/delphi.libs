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

{$ifdef deltics_sizegrip}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.SizeGrip;


interface

  uses
    Classes,
    Controls;

  type
    TSizeGrip = class(TWinControl)
    protected
      procedure CMDesignHitTest(var aMessage: TCMDesignHitTest); message CM_DESIGNHITTEST;
      procedure CreateParams(var Params: TCreateParams); override;
    public
      constructor Create(aOwner: TComponent); override;
      procedure SetBounds(aLeft, aTop, aWidth, aHeight: Integer); override;
    end;


implementation

  uses
    SysUtils,
    Windows;


{ TSizeGrip }

  constructor TSizeGrip.Create(aOwner: TComponent);
  begin
    inherited;

    ControlStyle := [csOpaque, csFixedWidth, csFixedHeight];

    Anchors := [akRight, akBottom];
    Cursor  := crSizeNWSE;
  end;


  procedure TSizeGrip.CMDesignHitTest(var aMessage: TCMDesignHitTest);
  begin
    aMessage.Result := 1;
  end;


  procedure TSizeGrip.CreateParams(var Params: TCreateParams);
  var
    rc: TRect;
  begin
    inherited;
    CreateSubClass(Params, 'SCROLLBAR');
    Params.Style := Params.Style or WS_CLIPSIBLINGS or SBS_SIZEGRIP or SBS_SIZEBOXBOTTOMRIGHTALIGN;

    // We shall initially size the control to cover the parent window
    //  client area - the VCL takes care of applying the correct
    //  dimensions

    if NOT Windows.GetClientRect(Params.WndParent, rc) then
      RaiseLastOSError;

    Params.X := rc.Left;
    Params.Y := rc.Top;
    Params.Width  := rc.Right - rc.Left;
    Params.Height := rc.Bottom - rc.Top;
  end;


  procedure TSizeGrip.SetBounds(aLeft, aTop, aWidth, aHeight: Integer);
  begin
    if (csDesigning in ComponentState) and Assigned(Parent) then
    begin
      aLeft   := Parent.ClientWidth - Width;
      aTop    := Parent.ClientHeight - Height;
      aWidth  := Width;
      aHeight := Height;
    end;
    inherited;
  end;



end.
