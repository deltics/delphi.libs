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

  unit Deltics.Threads;


interface

  uses
  { vcl: }
    Messages,
    Windows,
    SyncObjs,
    SysUtils,
  { deltics: }
    Deltics.MultiCast,
    Deltics.StateList,
    Deltics.Types;


  type
    TThread = class;
    TCustomThread = class;
    TMotile = class;

    EThread = class(Exception);


    TCPUAffinityMask  = NativeUInt;
    TThreadID         = DWORD;


    TThreadPriority = (
                       tpIdle,
                       tpLowest,
                       tpLower,
                       tpNormal,
                       tpHigher,
                       tpHighest,
                       tpTimeCritical
                      );

    TWorkEthic = (
                  weRunOnce,
                  weRunContinuously
                 );

    TMotileProc = procedure;

    TAsyncMethod = procedure of object;
    TBooleanMethod = function: Boolean of object;


  {$ifNdef MSWINDOWS}
    {$message ERROR 'Deltics.Threads Unit does not support the current platform'}
  {$endif}


    { -------------------------------------------------------------------------------------------- }
    TThread = class
    {
      An asbtract base class from which a number of concrete thread classes derive
       all sharing the common characteristics, behaviours and operations
       encapsulated in this abstract base class.
    }
    private
      fHandle: THandle;
      fID: Cardinal;
      fPriority: TThreadPriority;
    protected
      class procedure CheckThreadError(ErrCode: Integer); overload;
      class procedure CheckThreadError(Success: Boolean); overload;
      constructor Create;
      function get_Name: String; virtual; abstract;
      function get_Priority: TThreadPriority; virtual;
      procedure set_Name(aValue: String); virtual;
      procedure set_Priority(const aValue: TThreadPriority);
      procedure Initialise; virtual;
      procedure SetHandle(const aValue: THandle);
      procedure SetID(const aValue: Cardinal);
    public
      class function WaitFor(const aMethod: TAsyncMethod;
                             const aEventName: String;
                             const aTimeOut: Cardinal): Boolean; overload;
      class function WaitForAndNIL(var aThread; const aTimeOut: Cardinal = INFINITE): Boolean;
      procedure ChangeAffinity(aValue: TCPUAffinityMask);
      procedure Terminate; virtual; abstract;
      function WaitFor(const aTimeOut: Cardinal = INFINITE): Boolean; overload;
      property Handle: THandle read fHandle;
      property ID: Cardinal read fID;
      property Name: String read get_Name;
      property Priority: TThreadPriority read get_Priority write set_Priority;
    end;


    { -------------------------------------------------------------------------------------------- }
    TCustomThread = class(TThread)
    private
      fAffinity: TCPUAffinityMask;
      fName: String;
      fPriority: TThreadPriority;
      fStackSize: Word;
      procedure set_StackSize(const aValue: Word);
    protected
      function get_Name: String; override;
      procedure set_Name(aValue: String); override;
    public
      constructor Create(const aStackSize: Word = 0); virtual;
      destructor Destroy; override;
      property Affinity: TCPUAffinityMask read fAffinity write ChangeAffinity;
      property Name: String read get_Name write set_Name;
      property Priority: TThreadPriority read get_Priority write set_Priority;
      property StackSize: Word read fStackSize write set_StackSize;
    end;


    { -------------------------------------------------------------------------------------------- }
    TMotile = class(TCustomThread)
    {
      A lightweight thread class.  A Motile is typically used to spawn some autonomous
       background process with minimal overhead.  A motile thread typically will run
       either once then terminate and die or run continuously until explcitily
       terminated.

      Whether it runs once or continuously is specified in the "work ethic" of the
       constructor.
    }
    private
      fReusable: Boolean;
      fRunning: Boolean;
      fTerminated: Boolean;
      fWorkEthic: TWorkEthic;
    protected
      function get_Name: String; override;
      procedure set_Name(aValue: String); override;
      procedure Execute; overload; virtual; abstract;
      procedure Finalise; virtual;
      procedure Initialise; override;
      property WorkEthic: TWorkEthic read fWorkEthic;
      property Terminated: Boolean read fTerminated;
    public
      constructor Create(const aWorkEthic: TWorkEthic = weRunContinuously;
                         const aPriority: TThreadPriority = tpLower;
                         const aStackSize: Word = 0;
                         const aRunning: Boolean = TRUE;
                         const aReusable: Boolean = FALSE); reintroduce; virtual;
      class procedure Execute(const aProc: TMotileProc); overload;
      procedure AfterConstruction; override;
      procedure Start;
      procedure Stop;
      procedure Terminate; override;
      property Reusable: Boolean read fReusable;
      property Running: Boolean read fRunning;
    end;


    { -------------------------------------------------------------------------------------------- }
    TCapturedThread = class(TThread)
    {
      Provides a mechanism for wrapping an arbitrary thread within a TThread
       descendant.

      The capabilities of a captured a thread are extremely limited in terms
       of what can be achieved using the encapsulating class to work with that
       thread.
    }
    protected
      function get_Name: String; override;
    public
      constructor Capture(const aHandle: THandle);
      procedure Terminate; override;
      property Priority: TThreadPriority read get_Priority write set_Priority;
    end;


  {$ifdef DELPHI2006_OR_LATER}
    TCriticalSection = SyncObjs.TCriticalSection;
  {$else}
    TCriticalSection = class(SyncObjs.TCriticalSection)
    public
      function TryEnter: Boolean;
    end;
  {$endif}

  const
    tsInitialising  : TStateID = 'tsInitialising';
    tsCompleted     : TStateID = 'tsCompleted';
    tsException     : TStateID = 'tsException';
    tsRunning       : TStateID = 'tsRunning';
    tsSuspended     : TStateID = 'tsSuspended';
    tsTerminated    : TStateID = 'tsTerminated';
    tsTerminating   : TStateID = 'tsTerminating';
    tsWaiting       : TStateID = 'tsWaiting';

    STACK_DEFAULT = 0;
    STACK_8KB     = 8;
    STACK_16KB    = 16;
    STACK_32KB    = 32;
    STACK_64KB    = 64;
    STACK_256KB   = 256;
    STACK_512KB   = 512;
    STACK_1MB     = 1024;


  function InVCLThread: Boolean;



implementation

  uses
  { vcl: }
    Classes;


  type
    // Data structure used when changing the name of a thread as reported
    //  to the debugger
    TThreadNameInfo = record
      RecType: LongWord;
      Name: PANSIChar;        // NOT Unicode
      ThreadID: LongWord;
      Flags: LongWord;
    end;


  const
    Priorities: array [TThreadPriority] of Integer = (
                                                      THREAD_PRIORITY_IDLE,
                                                      THREAD_PRIORITY_LOWEST,
                                                      THREAD_PRIORITY_BELOW_NORMAL,
                                                      THREAD_PRIORITY_NORMAL,
                                                      THREAD_PRIORITY_ABOVE_NORMAL,
                                                      THREAD_PRIORITY_HIGHEST,
                                                      THREAD_PRIORITY_TIME_CRITICAL
                                                     );

  var
    _VCLThreadID: TThreadID = 0;


  function InVCLThread: Boolean;
  begin
    result := (GetCurrentThreadID = _VCLThreadID);
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function MotileProc(aMotile: TMotile): Integer;
  begin
    result := 0;
    try
      try
        aMotile.Initialise;
        try
          case aMotile.WorkEthic of
            weRunOnce         : aMotile.Execute;
            weRunContinuously : while NOT aMotile.Terminated do aMotile.Execute;
          end;

        finally
          aMotile.Finalise;
          aMotile.fRunning    := FALSE;
          aMotile.fTerminated := FALSE;
        end;

      except
        { NO-OP }
      end;

    finally
      if NOT aMotile.Reusable then
        aMotile.Free;

      ExitThread(0);
    end;
  end;


  { ---------------------------------------------------------------------------------------------- }
  type
    TAutoMotile = class(TMotile)
    private
      fProc: TMotileProc;
      constructor Create(const aProc: TMotileProc); reintroduce;
    protected
      procedure Execute; override;
    end;


  constructor TAutoMotile.Create(const aProc: TMotileProc);
  begin
    inherited Create(weRunOnce, tpNormal);
    fProc := aProc;
  end;


  procedure TAutoMotile.Execute;
  begin
    fProc;
  end;





{ TThread ---------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TThread.CheckThreadError(ErrCode: Integer);
  begin
    if ErrCode <> 0 then
      raise EThread.CreateFmt('Unexpected thread error : %s', [SysErrorMessage(ErrCode), ErrCode]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TThread.CheckThreadError(Success: Boolean);
  begin
    if not Success then
      CheckThreadError(GetLastError);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TThread.Create;
  begin
    inherited Create;

    fPriority := tpNormal;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TThread.ChangeAffinity(aValue: TCPUAffinityMask);
  var
    processAffinity: TCPUAffinityMask;
    unused: TCPUAffinityMask;
  begin
    if (Handle = 0) then
      EXIT;

    if (aValue = 0) then
      aValue := $ffffffff;

    GetProcessAffinityMask(GetCurrentProcess, processAffinity, unused);

    if (aValue and processAffinity) <> 0 then
      SetThreadAffinityMask(Handle, (aValue and processAffinity));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TThread.WaitFor(const aMethod: TAsyncMethod;
                                 const aEventName: String;
                                 const aTimeOut: Cardinal): Boolean;
  var
    event: THandle;
  begin
    aMethod;

    event := OpenEvent(EVENT_ALL_ACCESS, FALSE, PChar(aEventName));
    try
      result := WaitForSingleObject(event, aTimeOut) = WAIT_OBJECT_0;

    finally
      CloseHandle(event);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TThread.WaitForAndNIL(var aThread;
                                       const aTimeOut: Cardinal): Boolean;
  begin
    ASSERT(TObject(aThread) is TThread);

    result := TThread(aThread).WaitFor(aTimeOut);

    TObject(aThread) := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TThread.WaitFor(const aTimeOut: Cardinal): Boolean;
  begin
    if (Handle <> 0) then
      result := (Windows.WaitForSingleObject(Handle, aTimeOut) = WAIT_OBJECT_0)
    else
      result := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TThread.get_Priority: TThreadPriority;
  var
    P: Integer;
    I: TThreadPriority;
  begin
    if fHandle <> 0 then
    begin
      P := GetThreadPriority(FHandle);
      CheckThreadError(P <> THREAD_PRIORITY_ERROR_RETURN);
      Result := tpNormal;
      for I := Low(TThreadPriority) to High(TThreadPriority) do
        if Priorities[I] = P then Result := I;
    end
    else
      result := fPriority;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TThread.Initialise;
  begin
    if Name <> '' then
      set_Name(Name);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TThread.SetHandle(const aValue: THandle);
  begin
    fHandle := aValue;

    if (fHandle <> 0) then
      set_Priority(fPriority);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TThread.SetID(const aValue: Cardinal);
  begin
    fID := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TThread.set_Name(aValue: String);
  var
    info: TThreadNameInfo;
  begin
    if (ID = 0) then
      EXIT;

    ASSERT(ID = GetCurrentThreadID, 'Thread Name can only be set in the context of the thread itself');

    info.RecType  := $1000;
    info.Name     := PANSIChar(ANSIString(aValue));
    info.ThreadID := $FFFFFFFF;
    info.Flags    := 0;
    try
      RaiseException($406D1388, 0, sizeOf(info) div sizeOf(LongWord), Pointer(@info));

    except
      // NO-OP
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TThread.set_Priority(const aValue: TThreadPriority);
  begin
    if (Handle <> 0) then
      CheckThreadError(SetThreadPriority(Handle, Priorities[aValue]))
    else
      fPriority := aValue;
  end;









{ TCapturedThread -------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCapturedThread.Capture(const aHandle: THandle);
  begin
    inherited Create;
    fHandle := aHandle;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCapturedThread.get_Name: String;
  begin
    result := Format('Captured Thread (ID=?  Handle=%x)', [Handle]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCapturedThread.Terminate;
  begin
    TerminateThread(Handle, 0)
  end;









{ TCustomThread ---------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCustomThread.Create(const aStackSize: Word);
  begin
    fStackSize := aStackSize;

    inherited Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TCustomThread.Destroy;
  begin
    if (Handle <> 0) then
    begin
      CloseHandle(Handle);
      fHandle := 0;
    end;

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCustomThread.get_Name: String;
  begin
    result := fName;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCustomThread.set_Name(aValue: String);
  begin
    fName := aValue;

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCustomThread.set_StackSize(const aValue: Word);
  resourcestring
    rsThreadAlreadyRunning = 'Cannot change StackSize once thread has started';
  begin
    ASSERT(Handle = 0, rsThreadAlreadyRunning);
    fStackSize := aValue;
  end;









{ TMotile ---------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TMotile.Create(const aWorkEthic: TWorkEthic;
                             const aPriority: TThreadPriority;
                             const aStackSize: Word;
                             const aRunning: Boolean;
                             const aReusable: Boolean);
  begin
    inherited Create(aStackSize);

    fReusable   := aReusable;
    fRunning    := aRunning;    // Signals AfterConstruction to start the thread

    fPriority   := aPriority;
    fWorkEthic  := aWorkEthic;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TMotile.Execute(const aProc: TMotileProc);
  begin
    TAutoMotile.Create(aProc);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMotile.AfterConstruction;
  begin
    inherited;

    if Running then
    begin
      fRunning := FALSE;  // Need to turn Running off otherwise Start will be a NO-OP
      Start;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TMotile.get_Name: String;
  begin
    result := fName;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMotile.set_Name(aValue: String);
  begin
    fName := aValue;
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMotile.Terminate;
  begin
    fTerminated := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMotile.Initialise;
  begin
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMotile.Finalise;
  begin
    // NO-OP
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMotile.Start;
  begin
    if Running then
      EXIT;

    fHandle:= BeginThread(NIL, StackSize * 1024, @MotileProc, Pointer(self), CREATE_SUSPENDED, fID);
    if fHandle = 0 then
      raise EThread.CreateFmt('Error creating motile: %s', [SysErrorMessage(GetLastError)]);

    SetThreadPriority(Handle, Priorities[Priority]);
    ResumeThread(Handle);

    fRunning := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMotile.Stop;
  begin
    Terminate;
  end;




{$ifNdef DELPHI2006_OR_LATER}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCriticalSection.TryEnter: Boolean;
  begin
    result := TryEnterCriticalSection(fSection);
  end;
{$endif}









initialization
  _VCLThreadID := GetCurrentThreadID;

end.

