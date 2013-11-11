{
  * X11 (MIT) LICENSE *

  Copyright © 2013 Jolyon Smith

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

{$ifdef deltics_oscillator}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Oscillator;

interface

  uses
  { deltics: }
    Deltics.Threads;

  type
    TOscillatorEvent = procedure(const aPhase: Word) of object;

    TOscillator = class(TMotile)
    private
      fFrequency: Word;
      fPhase: Word;
      fSynchronizeEvents: Boolean;
      fOnEvent: TOscillatorEvent;
      procedure set_SynchronizeEvents(const aValue: Boolean);
    protected
      procedure Initialise; override;
      procedure Execute; override;
    public
      constructor Create(const aFrequency: Word; const aEvent: TOscillatorEvent = NIL); reintroduce;
      constructor CreateSuspended(const aFrequency: Word; const aEvent: TOscillatorEvent = NIL); reintroduce;
      property Phase: Word read fPhase;
      property SynchronizeEvents: Boolean read fSynchronizeEvents write set_SynchronizeEvents;
      property OnEvent: TOscillatorEvent read fOnEvent write fOnEvent;
    end;


implementation

  uses
  { vcl: }
    Messages,
    Windows,
  { deltics: }
    Deltics.MessageHandler;


  const
    OSCM_EVENT  = WM_USER + 1;

  type
    TOscillatorHandler = class(TMessageHandler)
      procedure OSCMEvent(var aMessage: TMessage); message OSCM_EVENT;
    end;


  procedure TOscillatorHandler.OSCMEvent(var aMessage: TMessage);
  var
    sender: TOscillator;
  begin
    sender := TOscillator(aMessage.LParam);
    if Assigned(sender.OnEvent) then
      sender.OnEvent(Word(aMessage.wParam));
  end;


  var
    VCL: TOscillatorHandler = NIL;



{ TOscillator ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TOscillator.Create(const aFrequency: Word;
                                 const aEvent: TOscillatorEvent);
  begin
    CreateSuspended(aFrequency, aEvent);
    Start;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TOscillator.CreateSuspended(const aFrequency: Word;
                                          const aEvent: TOscillatorEvent);
  begin
    inherited Create(weRunContinuously, tpLower, {stack KB=} 1, {running=} FALSE, {reusable=} TRUE);

    fOnEvent            := aEvent;
    fFrequency          := aFrequency;
    fSynchronizeEvents  := NOT IsConsole;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOscillator.Execute;
  begin
    Inc(fPhase);

    if Assigned(fOnEvent) then
      if SynchronizeEvents then
        PostMessage(VCL.Handle, OSCM_EVENT, WPARAM(fPhase), LPARAM(self))
      else
        fOnEvent(fPhase);

    if (fPhase = fFrequency) then
      fPhase := 0;

    Sleep(1000 div fFrequency);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOscillator.Initialise;
  begin
    fPhase := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOscillator.set_SynchronizeEvents(const aValue: Boolean);
  begin
    fSynchronizeEvents := aValue and Assigned(VCL);
  end;





initialization
  if IsConsole then
    EXIT;

  VCL := TOscillatorHandler.Create;

finalization
  VCL.Free;

end.
