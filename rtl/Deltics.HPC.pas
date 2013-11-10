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

{$ifdef deltics_hpc}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.HPC;


interface

  uses
  { deltics: }
    Deltics.Classes;


  type
    IPerformanceCounter = interface
    ['{A460C917-4B3D-4FDD-9EC1-FA2BD14297DC}']
      function get_Frequency: Int64;
      function get_Value: Int64;

      property Frequency: Int64 read get_Frequency;
      property Value: Int64 read get_Value;
    end;


  function HPC: IPerformanceCounter;


implementation

  uses
  { vcl: }
    Windows;


  var
    _HPC: IPerformanceCounter;


  function HPC: IPerformanceCounter;
  begin
    result := _HPC;
  end;


  type
    TPerformanceCounter = class(TCOMInterfacedObject, IPerformanceCounter)
    public // IPerformanceCounter
      function get_Frequency: Int64;
      function get_Value: Int64;
      property Frequency: Int64 read get_Frequency;
      property Value: Int64 read get_Value;
    end;


    TTickCounter = class(TCOMInterfacedObject, IPerformanceCounter)
    public // IPerformanceCounter
      function get_Frequency: Int64;
      function get_Value: Int64;
      property Frequency: Int64 read get_Frequency;
      property Value: Int64 read get_Value;
    end;





{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 GetHPCTime and GetHPCFreq implement required counter operations using the hardware high
  performance counter API.

 This mechanism is only available where a hardware high performance counter is present.

 If used, these functions are called via the GetTime and GetFreq function type variables.

 GetHPCTime is also called directly during initialization to determine whether or not the
  high performance counter API is available (returns 0 if not).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCounter.get_Frequency: Int64;
  begin
    QueryPerformanceFrequency(result);
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCounter.get_Value: Int64;
  begin
    QueryPerformanceCounter(result);
  end;





{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 GetTCTime and GetTCFreq implement required counter operations using the GetTickCount API.

 This mechanism is provided as a fall-back mechanism for the situation where a high
  performance hardware counter is not available.

 The frequency of the GetTickCount API is nominally 1000 ticks per second.  The actual
  resolution of timer values may not be this accurate, but this is how values are
  reported.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTickCounter.get_Frequency: Int64;
  begin
    result := 1000;
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTickCounter.get_Value: Int64;
  begin
    result := GetTickCount;
  end;





{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

  function IsPerformanceCounterSupported: Boolean;
  var
    testValue: Int64;
  begin
    QueryPerformanceCounter(testValue);
    result := (testValue <> 0);
  end;


initialization
  if IsPerformanceCounterSupported then
    _HPC := TPerformanceCounter.Create
  else
    _HPC := TTickCounter.Create;

end.
