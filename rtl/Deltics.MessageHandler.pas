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

{$ifdef deltics_messagehandler}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.MessageHandler;


interface

  uses
  { vcl: }
    Forms,
    Windows;

  type
    TMessageHandler = class(TCustomForm)
    public
      constructor Create; reintroduce; overload; virtual;
      destructor Destroy; override;
      procedure BeforeDestruction; override;
      procedure PostMessage(const aMessageID: Cardinal; const wParam: Windows.WPARAM; const lParam: Windows.LPARAM);
      function SendMessage(const aMessageID: Cardinal; const wParam: Windows.WPARAM; const lParam: Windows.LPARAM): Integer;
    end;




implementation

  uses
  { vcl: }
    Classes,
    Controls,
    Graphics;


  var
    _VCLThreadID: Cardinal = 0;


{
 --------------------------------------------------------------------------------------------------
 Implementation notes:
 --------------------------------------------------------------------------------------------------

 1.  If a VCL application fails to reach a point where a valid Screen object exists
      then the destructor chain for the message handler results in access violations
      when the inherited methods.

     One scenario where this occurs is if you have a unit that creates a message
      handler object in it's initialization but the project DPR does not contain
      any code.  Such as might be encountered when compiling a unit list to identify
      project unit dependencies, for example (running such an application is pointless
      but if you do you would expect nothing to happen, not crashes).

     Although this is an edge case, it can be very confusing (and worrying) when it
      occurs so we avoid it by only calling inherited for the BeforeDestruction method
      and Destroy destructor if a Screen object is actually assigned.

 --------------------------------------------------------------------------------------------------
}


{ TMessageHandler -------------------------------------------------------------------------------- }

  constructor TMessageHandler.Create;
  begin
    ASSERT(_VCLThreadID = GetCurrentThreadId,
           'Message handlers must be created in the Main VCL Thread context');

    inherited CreateNew(NIL);

    Visible := FALSE;

    // Force the creation of the window handle otherwise it is highly likely that the
    //  first access to the window via its handle will be in some thread wishing to
    //  send a message, resulting in the window being created and owned in and by that
    //  thread, not the VCL thread, utterly defeating the purpose and causing lock-ups
    //  that WILL have you scratching your head, like *I* did for almost a week before
    //  I realised my own screw-up.  Dufus!

    CreateWnd;
  end;


  procedure TMessageHandler.BeforeDestruction;
  begin
    // Implementation Note #1
    if Assigned(Screen) then
      inherited;
  end;


  destructor TMessageHandler.Destroy;
  begin
    ASSERT(_VCLThreadID = GetCurrentThreadID,
           'Message handlers should be destroyed in the Main VCL Thread context');

    // Implementation Note #1
    if Assigned(Screen) then
      inherited;
  end;


  procedure TMessageHandler.PostMessage(const aMessageID: Cardinal;
                                        const wParam: Windows.WPARAM;
                                        const lParam: Windows.LPARAM);
  begin
    Windows.PostMessage(Handle, aMessageID, wParam, lParam);
  end;


  function TMessageHandler.SendMessage(const aMessageID: Cardinal;
                                       const wParam: Windows.WPARAM;
                                       const lParam: Windows.LPARAM): Integer;
  begin
    result := Windows.SendMessage(Handle, aMessageID, wParam, lParam);
  end;



initialization
  _VCLThreadID := GetCurrentThreadID;

end.

