{
  * X11 (MIT) LICENSE *

  Copyright © 2010 Jolyon Smith

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

{$ifdef deltics_finalizer}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Finalizer;


interface

  type
    TFinalizer = procedure;


  function ActiveFinalizer: String;
  procedure RegisterFinalization(const aProc: TFinalizer;
                                 const aUnitName: String);


implementation

  uses
  { vcl: }
    SysUtils,
    Windows;


  type
    TFinalizerInfo = record
      Proc: TFinalizer;
      UnitName: String;
    end;


  var
    _CurrentFinalizer: TFinalizerInfo = (Proc: NIL; UnitName: '');
    _Finalizers: array of TFinalizerInfo;



  function ActiveFinalizer: String;
  begin
    result := _CurrentFinalizer.UnitName;
  end;


  procedure RegisterFinalization(const aProc: TFinalizer;
                                 const aUnitName: String);
  begin
    SetLength(_Finalizers, Length(_Finalizers) + 1);

    _Finalizers[Pred(Length(_Finalizers))].Proc     := aProc;
    _Finalizers[Pred(Length(_Finalizers))].UnitName := aUnitName;
  end;


  procedure Finalize;
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(_Finalizers)) do
    begin
      _CurrentFinalizer := _Finalizers[i];
      try
        try
          _CurrentFinalizer.Proc;

        except
          on e: Exception do
            OutputDebugString(PChar('Exception: ' + e.Message
                                  + ' in finalizer for ''' + _CurrentFinalizer.UnitName + ''''));
        end;

      finally
        _CurrentFinalizer.Proc      := NIL;
        _CurrentFinalizer.UnitName  := '<none>';
      end;
    end;
  end;



initialization
  // We call ActiveFinaliser here in order to ensure that there is at least one
  //  reference to the function in our application.  Otherwise if there is no
  //  user code that calls this function it will be removed by the linker and so
  //  will be unavailable from the debugger, which could be annoying when trying
  //  to identify and isolate some error during finalization

  ActiveFinalizer;

finalization
  Finalize;

end.
