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

  unit Deltics.Threads.Worker;


interface

  uses
  { deltics: }
    Deltics.MultiCast,
    Deltics.StateList,
    Deltics.Threads;


  type
    { -------------------------------------------------------------------------------------------- }
    TWorkerThread = class(TCustomThread)
    {
      A fully specified and equipped thread class.  Threads derived from this class
       may be suspended and resumed (if required, but it is recommended that more
       sophisticated techniques be used for synchronization purposes).

      Crucially however, once a thread derived from this class has terminated it
       can be simply *re*started.
    }
    private
      fFreeOnTerminate: Boolean;
      fReturnValue: Integer;
      fState: TStateList;
      fException: TObject;
      fOn_Complete: TMultiCastNotify;
      fOn_Terminate: TMultiCastNotify;
      function get_Terminated: Boolean;
      function get_Terminating: Boolean;
      procedure OnEnterState(aSender: TObject; const aStateID: TStateID);
      procedure OnLeaveState(aSender: TObject; const aStateID: TStateID);
    protected
      procedure DoTerminate; virtual;
      procedure Execute; virtual; abstract;
      procedure Initialise; override;
      procedure Finalise; virtual;
      procedure OnComplete; virtual;
      procedure OnTerminate; virtual;
      property ReturnValue: Integer read FReturnValue write FReturnValue;
    public
      constructor Create(const aStackKB: Word = 0); override;
      constructor CreateSuspended(const aStackSize: Word = 0); virtual;
      destructor Destroy; override;
      procedure Restart;
      procedure Resume;
      procedure Suspend;
      procedure Terminate; override;
      property ThreadException: TObject read fException;
      property FreeOnTerminate: Boolean read fFreeOnTerminate write fFreeOnTerminate;
      property Name: String read get_Name write set_Name;
      property State: TStateList read fState;
      property Terminated: Boolean read get_Terminated;
      property Terminating: Boolean read get_Terminating;
      property On_Complete: TMultiCastNotify read fOn_Complete;
      property On_Terminate: TMultiCastNotify read fOn_Terminate;
    end;




implementation

  uses
  { vcl: }
    SysUtils,
    Windows,
  { deltics: }
    Deltics.Threads.Synchronize;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function WorkerProc(aThread: TWorkerThread): Integer;
  begin
    result := 0;

    if aThread.State[tsTerminated] then
      EXIT;

    try
      if aThread.State[tsTerminating] then
        EXIT;

      try
        aThread.Initialise;
        try
          aThread.State.Enter(tsRunning);
          aThread.Execute;
          aThread.State.Enter(tsCompleted);

        finally
          aThread.Finalise;
        end;

      except
        on EAbort do { NO-OP } ;
        on Exception do
        begin
          aThread.fException := AcquireExceptionObject;
          aThread.State.Enter(tsException);
        end;
      end;

    finally
      result := 0;
      aThread.State.Enter(tsTerminated);

      if aThread.FreeOnTerminate then
        aThread.Free;
    end;
  end;





{ TWorkerThread ---------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TWorkerThread.Create(const aStackKB: Word);
  begin
    CreateSuspended(aStackKB);
    Resume;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TWorkerThread.CreateSuspended(const aStackSize: Word);
  begin
    inherited Create(aStackSize);

    fOn_Complete   := TMultiCastNotify.Create(self);
    fOn_Terminate  := TMultiCastNotify.Create(self);

    fState    := TStateList.Create(self, [tsInitialising,
                                          tsCompleted,
                                          tsException,
                                          tsRunning,
                                          tsTerminated,
                                          tsWaiting]);

    State.Add([tsSuspended, tsTerminating], TRUE);
    State.Enter(tsInitialising);
    State.Enter(tsSuspended);
    State.On_Enter.Initial := OnEnterState;
    State.On_Leave.Final   := OnLeaveState;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TWorkerThread.Destroy;
  begin
    inherited;

    FreeAndNIL(fException);
    FreeAndNIL(fState);
    FreeAndNIL(fOn_Complete);
    FreeAndNIL(fOn_Terminate);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWorkerThread.get_Terminated: Boolean;
  begin
    result := State.InAny([tsTerminating, tsTerminated]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWorkerThread.get_Terminating: Boolean;
  begin
    result := State.InState[tsTerminating];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.Suspend;
  begin
    State.Enter(tsSuspended);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.Restart;
  begin
    // TODO: Need some safety checks here.
    //
    // e.g. Cannot restart unless terminated, require the thread to be
    //       marked "restartable" (thread options: restartable, NOT free on terminate etc etc)

    State.On_Enter.Disable;
    State.On_Leave.Disable;
    try
      State.Enter(tsInitialising);
      State.Leave(tsCompleted);
      State.Leave(tsTerminated);
      State.Leave(tsTerminating);
      State.Enter(tsSuspended);
    finally
      State.On_Enter.Enable;
      State.On_Leave.Enable;
    end;

    Resume;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.Resume;
  begin
    if State[tsTerminated] then
      Restart
    else
      State.Leave(tsSuspended);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.DoTerminate;
  begin
    State.Enter(tsTerminating);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.Terminate;
  begin
    DoTerminate;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.OnEnterState(aSender: TObject; const aStateID: TStateID);
  var
    iSuspendCount: Integer;
  begin
    if (aStateID = tsSuspended) then
    begin
      iSuspendCount := SuspendThread(Handle);
      CheckThreadError(iSuspendCount = 0);
      State.Leave(tsRunning);
      EXIT;
    end;

    if (aStateID = tsRunning) then
    begin
      repeat
        iSuspendCount := ResumeThread(Handle);
      until Abs(iSuspendCount) = 1;
      CheckThreadError(iSuspendCount = 1);
      EXIT;
    end;

    if (aStateID = tsTerminated) then
    begin
      State.Leave(tsRunning);
      OnTerminate;
      EXIT;
    end;

    if (aStateID = tsCompleted) then
    begin
      OnComplete;
      EXIT;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.OnLeaveState(aSender: TObject; const aStateID: TStateID);
  var
    id: Cardinal;
  begin
    if (aStateID = tsSuspended) then
    begin
      if State[tsInitialising] then
      begin
        State.Leave(tsInitialising);

        SetHandle(BeginThread(NIL, StackSize * 1024, Addr(WorkerProc), Pointer(self), CREATE_SUSPENDED, id));
        if (Handle = 0) then
          raise EThread.CreateFmt('Error creating thread: %s', [SysErrorMessage(GetLastError)]);

        SetID(id);
        ChangeAffinity(Affinity);
      end;

      State.Enter(tsRunning);
      EXIT;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.Initialise;
  begin
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.Finalise;
  begin
    // NO-OP
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.OnComplete;
  begin
    Synchronize(On_Complete.DoEvent);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWorkerThread.OnTerminate;
  begin
    Synchronize(On_Terminate.DoEvent);
  end;



end.

