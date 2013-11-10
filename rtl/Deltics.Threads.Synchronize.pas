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

{$ifdef deltics_threads}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Threads.Synchronize;


interface

  type
    TSynchronisedFunction   = function: Integer of object;
    TSynchronisedProcedure  = procedure of object;


  function Synchronize(const aFunction: TSynchronisedFunction): Integer; overload;
  procedure Synchronize(const aProcedure: TSynchronisedProcedure); overload;



implementation

  uses
  { vcl: }
    Messages,
    Windows,
  { deltics: }
    Deltics.MessageHandler;


  const
    SM_SyncFunction  = WM_USER;
    SM_SyncProcedure = WM_USER + 1;


  type
    TSyncMessage = record
      MessageID: Cardinal;
      Code: Pointer;
      Data: Pointer;
      Result: LongInt;
    end;

    TSyncWindow = class(TMessageHandler)
      procedure SM_SyncFunction(var aMessage: TSyncMessage); message SM_SyncFunction;
      procedure SM_SyncProcedure(var aMessage: TSyncMessage); message SM_SyncProcedure;
    end;


  procedure TSyncWindow.SM_SyncFunction(var aMessage: TSyncMessage);
  var
    method: TMethod;
  begin
    method.Code := aMessage.Code;
    method.Data := aMessage.Data;

    aMessage.Result := TSynchronisedFunction(method)();
  end;

  procedure TSyncWindow.SM_SyncProcedure(var aMessage: TSyncMessage);
  var
    method: TMethod;
  begin
    method.Code := aMessage.Code;
    method.Data := aMessage.Data;

    TSynchronisedProcedure(method)();

    aMessage.Result := 0;
  end;


  var
    VCL: TSyncWindow = NIL;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Synchronize(const aFunction: TSynchronisedFunction): Integer;
  begin
    result := VCL.SendMessage(SM_SyncFunction, WPARAM(TMethod(aFunction).Code),
                                               LPARAM(TMethod(aFunction).Data));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure Synchronize(const aProcedure: TSynchronisedProcedure);
  begin
    VCL.SendMessage(SM_SyncProcedure, WPARAM(TMethod(aProcedure).Code),
                                      LPARAM(TMethod(aProcedure).Data));
  end;




initialization
  VCL := TSyncWindow.Create;

finalization
  VCL.Free;

end.
