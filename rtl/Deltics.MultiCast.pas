{@@License

  <TITLE License and Contact Details>

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

{@@Deltics.MultiCast.pas

    Provides an extensible implementation of a multicast events framework for
     Delphi (Win32), including a reference implementation of TMultiCastNotify -
     a multicast equivalent of TNotifyEvent.

    Multicast events in this framework are designed to be compatible with existing
     unicast event handler methods.

    Includes:

      - TMultiCastEvent   : base class for multicast event implementations

      - TMultiCastNotify  : a multicast implementation of TNotifyEvent

      - IOn_Destroy       : an interface establishing that the implementing
                             class supports an On_Destroy multicast TNotifyEvent

      - TOnDestroy  : class to enable IOn_Destroy interface support to be
                       easily added to any multicast listener using interface
                       delegation
}

{$i deltics.rtl.inc}

{$ifdef deltics_multicast}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.MultiCast;


interface

  uses
  { vcl: }
    Classes;


  type
    //## Classes implemented in this unit
    TMultiCastEvent  = class;
    TMultiCastNotify = class;

    //## Documentation provided below
    PMethod          = ^TMethod;
    PMultiCastNotify = ^TMultiCastNotify;


    {@@PMethod

      Pointer to a TMethod
    }
  
    {@@PMultiCastNotify

     Pointer to a TMultiCastNotify object reference.  This type is used to
      provide a type-safe implementation of TMultiCastNotify.CreateEvents to
      enable multiple TMultiCastNotify objects to be created and assigned to a
      number of reference variables in a single call.
    }


    TMultiCastEvent = class
    {
      Provides the base class for all multi-cast events.

      When implementing a multi-cast event you will derive from this class or
       from some other class that has this class as an ancestor.

      This base class provides the mechanism for storing the list of handlers
       to be called in response to an event occuring and for ensuring that
       references between events and handler objects are maintained where
       possible.

      See also
        IOn_Destroy
    }
    private
      fDisableCount: Integer;
      fActive: Boolean;
      fMethods: TList;
      function get_Count: Integer;
      function get_Enabled: Boolean;
      function get_Method(const aIndex: Integer): TMethod;

      procedure ListenerDestroyed(aSender: TObject);
    protected
      {$ifopt C+}
      class procedure CheckReferences(const aArray; const aCount: Integer);
      {$endif}

      procedure Call(const aMethod: TMethod); virtual; abstract;

      procedure Add(const aMethod: TMethod); overload;
      procedure Insert(const aMethod: TMethod); overload;
      procedure Remove(const aMethod: TMethod); overload;

      property Method[const aIndex: Integer]: TMethod read get_Method;
    public
      constructor Create; virtual;
      destructor Destroy; override;

      procedure Assign(const aSource: TMultiCastEvent);
      procedure DoEvent;
      procedure Enable;
      procedure Disable;
      procedure GetListeners(const aList: TList);

      property Active: Boolean read fActive;
      property Count: Integer read get_Count;
      property Enabled: Boolean read get_Enabled;
    end;



    IOn_Destroy = interface
    ['{D2AD6882-0CB7-40A6-839D-F527071918FC}']
      procedure Add(const aHandler: TNotifyEvent);
      procedure Remove(const aHandler: TNotifyEvent);
      procedure DoEvent;
      procedure GetListeners(const aList: TList);
    end;


    TMultiCastNotify = class(TMultiCastEvent)
    {
      TMultiCastNotify is a multi-cast equivalent of the standard TNotifyEvent.
       This multi-cast event implementation serves two purposes:

      * A ready-to-use multi-cast version of the standard TNotifyEvent
         event for use in your own applications.

      * A complete example of a multi-cast event implementation to be used
         as a guide when implementing your own multi-cast events.
    }
    private
      fSender: TObject;
    protected
      property Sender: TObject read fSender;
      procedure Call(const aMethod: TMethod); override;
    public
      class procedure CreateEvents(const aSender: TObject;
                                   const aEvents: array of PMultiCastNotify);
      constructor Create(const aSender: TObject); reintroduce; virtual;
      procedure Insert(const aHandler: TNotifyEvent); overload;
      procedure Add(const aHandler: TNotifyEvent); overload;
      procedure Remove(const aHandler: TNotifyEvent); overload;
      procedure DoEventFor(const aSender: TObject);
    end;



    TOnDestroy = class(TInterfacedObject, IOn_Destroy)
    {
      Provides a ready-made implementation of the IOn_Destroy
       interface, encapsulating a TMultiCastNotify On_Destroy event.
       This class can be used to easily add IOn_Destroy support to any
       handler of multicast events, using interface delegation.

      <code>
        private
          fOn_Destroy: IOn_Destroy;
        public
          constructor Create;
          property On_Destroy: IOn_Destroy read fOn_Destroy implements IOn_Destroy;



        constructor ...Create;
        begin
          inherited;
          fOn_Destroy := TOnDestroy.Create(self);
        end;
      </code>
    }
    private
      fEvent: TMultiCastNotify;
    public
      constructor Create(const aOwner: TObject);
      destructor Destroy; override;
    public
      //## IOn_Destroy
      procedure Add(const aHandler: TNotifyEvent);
      procedure Remove(const aHandler: TNotifyEvent);
      procedure DoEvent;
      procedure GetListeners(const aList: TList);
    end;




{-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ --}

implementation

  uses
  { vcl: }
    SysUtils;




{-- TMultiCastEvent  ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- }

  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TMultiCastEvent.Create;
  {@@TMultiCastEvent.Create

    Default constructor for multi-cast events.  Multi-cast event classes
     MUST call this constructor if they override or introduce an alternate
     constructor.
  }
  begin
    inherited Create;

    fMethods := TList.Create;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TMultiCastEvent.Destroy;
  {@@TMultiCastEvent.Destroy

    Destructor for multi-cast events.  Any handlers remaining in the
     handler list are removed.  If the implementor of a handler supports
     the IOn_Destroy interface the event removes it's handler from that
     object's On_Destroy event.
  }
  var
    i: Integer;
    obj: TObject;
    listener: IOn_Destroy;
  begin
    for i := 0 to Pred(fMethods.Count) do
    begin
      obj := TObject(PMethod(fMethods[i]).Data);

      if Supports(obj, IOn_Destroy, listener) then
        listener.Remove(ListenerDestroyed);

      Dispose(PMethod(fMethods[i]));
      fMethods[i] := NIL;
    end;

    FreeAndNIL(fMethods);

    inherited;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TMultiCastEvent.get_Count: Integer;
  {@@TMultiCastEvent.Count
  
    Returns the number of handlers currently assigned to the event.
  }
  begin
    result := fMethods.Count;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TMultiCastEvent.get_Enabled: Boolean;
  {@@TMultiCastEvent.Enabled

    Indicates whether or not the event is currently enabled.

    Handlers may be added or removed to an event when not enabled, but
     they will not be called if that event is fired.

    See Also
      TMultiCastEvent.Enable
      TMultiCastEvent.Disable
  }
  begin
    result := (fDisableCount = 0);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TMultiCastEvent.get_Method(const aIndex: Integer): TMethod;
  {@@TMultiCastEvent.Method

    Returns the TMethod at the iIndex position in the list of handlers for
     the event.
  }
  begin
    result := TMethod(fMethods[aIndex]^);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.Assign(const aSource: TMultiCastEvent);
  var
    i: Integer;
  begin
    if NOT Assigned(self) or NOT Assigned(aSource) then
      EXIT;

    ASSERT(aSource.ClassType = ClassType);

    for i := 0 to Pred(aSource.Count) do
      Add(PMethod(aSource.fMethods[i])^);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  {$ifopt C+}
  class procedure TMultiCastEvent.CheckReferences(const aArray; const aCount: Integer);
  {@@TMultiCastEvent.CheckReferences

  Parameters

    aArray - An array of pointers to references to be tested for duplicates.

    aCount - The number of pointers in the array.

  Description

    Raises an EInvalidPointer exception if the array is found to contain
     duplicate pointers or pointers to references that have already been
     assigned.

    This procedure is intended for use during development to assist in
     detecting and reporting coding errors.  This procedure is only
     available if compiling with ASSERT() statements enabled.

    Calls to this method should be made inside an $ifopt C+ conditional
     compilation directive.
  }
  type
    PointerArray  = array of Pointer;
    PPointerArray = ^PointerArray;
  var
    i, j: Integer;
    this: Pointer;
    next: Pointer;
  begin
    if aCount = 1 then
    begin
      // No need to check for coincident references if there is only 1 of them,
      //  just check that it isn't already assigned.
      if (PointerArray(aArray)[0] <> NIL) then
        raise EInvalidPointer.CreateFmt('%s.CreateEvents: The reference has already been assigned.',
                                        [ClassName]);
    end
    else
      for i := 0 to Pred(aCount) do
      begin
        this := PointerArray(@aArray)[i];
        if (Pointer(this^) <> NIL) then
          raise EInvalidPointer.CreateFmt('%s.CreateEvents: The reference at index %d has already '
                                        + 'been assigned.', [ClassName, i]);

        for j := i + 1 to Pred(aCount) do
        begin
          next := PointerArray(@aArray)[j];
          if (this = next) then
            raise EInvalidPointer.CreateFmt('%s.CreateEvents: Duplicate event references at '
                                          + 'indices %d and %d.', [ClassName, i, j]);
        end;
      end;
  end;
  {$endif}


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.Add(const aMethod: TMethod);
  {@@TMultiCastEvent.Add

  Parameters

    aMethod - A method (procedure or function implemented by an instance of a
               class type) which is to be added to the list of handlers for
               this event.

  Description

    Adds a specified method to the event.  Only one reference to a method can
     be attached to an event - if the specific method is already in the list
     of handlers for the event it will not be added again.

    The object implementing the specified method (the data pointer) is tested
     to see if it supports the IOn_Destroy interface.  If so, the event
     uses that interface to add its own ReceiverDestroyed handler to the
     On_Destroy event of the object.

    This ensures that if the object that implements the method being added is
     destroyed then its handler(s) will be removed.


  * Important *

    The implementation of Add in this class is NOT virtual and only has
     PROTECTED visibility.  It should NOT be overridden in derived classes.

    This is because the TMethod type of the parameter is potentially not
     type safe.  Derived classes should provide their own public Add and Remove
     methods accepting a specific TMethod signature (e.g. TNotifyEvent) to
     ensure that only handlers with the correct parameter list are added and
     removed from the event handler list.


  See Also
    IOn_Destroy
  }
  var
    i: Integer;
    obj: TObject;
    handler: PMethod;
    listener: IOn_Destroy;
  begin
    if NOT Assigned(self) then
      EXIT;

    // Check to ensure that the specified method is not already attached
    for i := 0 to Pred(fMethods.Count) do
    begin
      handler := fMethods[i];

      if (aMethod.Code = handler.Code) and (aMethod.Data = handler.Data) then
        EXIT;
    end;

    // Not already attached - create a new TMethod reference and copy the
    //  details from the specific method, then add to our list of handlers
    handler := New(PMethod);
    handler.Code := aMethod.Code;
    handler.Data := aMethod.Data;
    fMethods.Add(handler);

    // Check the object implementing this handler for support of the
    //  IOn_Destroy interface.  If available, add our own
    //  ReceiverDestroyed event handler to that object's On_Destroy event
    obj := TObject(aMethod.Data);

    if Supports(obj, IOn_Destroy, listener) then
      listener.Add(ListenerDestroyed);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.Insert(const aMethod: TMethod);
  {@@TMultiCastEvent.Insert

  Parameters

    aMethod - A method (procedure or function implemented by an instance of a
               class type) which is to be insert at the TOP of the list of
               handlers for this event.

  Description

    Adds a specified method to the event.  Only one reference to a method can
     be attached to an event - if the specific method is already in the list
     of handlers for the event it will not be added again.

    The object implementing the specified method (the data pointer) is tested
     to see if it supports the IOn_Destroy interface.  If so, the event
     uses that interface to add its own ReceiverDestroyed handler to the
     On_Destroy event of the object.

    This ensures that if the object that implements the method being added is
     destroyed then its handler(s) will be removed.


  * Important *

    The implementation of Add in this class is NOT virtual and only has
     PROTECTED visibility.  It should NOT be overridden in derived classes.

    This is because the TMethod type of the parameter is potentially not
     type safe.  Derived classes should provide their own public Add and Remove
     methods accepting a specific TMethod signature (e.g. TNotifyEvent) to
     ensure that only handlers with the correct parameter list are added and
     removed from the event handler list.


  See Also
    IOn_Destroy
  }
  var
    i: Integer;
    obj: TObject;
    handler: PMethod;
    listener: IOn_Destroy;
  begin
    if NOT Assigned(self) then
      EXIT;

    // Check to ensure that the specified method is not already attached
    for i := 0 to Pred(fMethods.Count) do
    begin
      handler := fMethods[i];

      if (aMethod.Code = handler.Code) and (aMethod.Data = handler.Data) then
        EXIT;
    end;

    // Not already attached - create a new TMethod reference and copy the
    //  details from the specific method, then add to our list of handlers
    handler := New(PMethod);
    handler.Code := aMethod.Code;
    handler.Data := aMethod.Data;
    fMethods.Insert(0, handler);

    // Check the object implementing this handler for support of the
    //  IOn_Destroy interface.  If available, add our own
    //  ReceiverDestroyed event handler to that object's On_Destroy event
    obj := TObject(aMethod.Data);

    if Supports(obj, IOn_Destroy, listener) then
      listener.Add(ListenerDestroyed);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.Remove(const aMethod: TMethod);
  {@@TMultiCastEvent.Remove

  Parameters

    aMethod - A method (procedure or function implemented by an instance of a
               class type) which is to be removed from the list of handlers
               for this event.

  Description

    Removes the specified method from the list of handlers for the event.
    Has no effect if the specified method was not found.

    ## NOTE: No account is taken of the case where the method being removed
    ##        is the last or only handler for the event that is implemented
    ##        by an object that implements IOn_Destroy
    ##
    ##       In that case we could remove ourselves from the objects
    ##        On_Destroy event, but doing so would require scanning all
    ##        handlers to determine whether any others are implemented by
    ##        the same object as well as testing for and acquiring the
    ##        IOn_Destroy interface.
    ##
    ##       All of which is a bit excessive given that by not doing so
    ##        all we do is waste a tiny amount of memory (at most, one
    ##        entry in that objects On_Destroy handler list) that will be
    ##        properly cleaned up when either this event or the object
    ##        is eventually destroyed.

  * Important *

    The implementation of Remove in this class is NOT virtual and only has
     PROTECTED visibility.  It should NOT be overridden in derived classes.

    This is because the TMethod type of the parameter is potentially not
     type safe.  Derived classes should introduce their own public Add and
     Remove methods accepting a specific TMethod signature (e.g. TNotifyEvent)
     to ensure that only handlers with the correct parameter list are added and
     removed from the event handler list.


  See Also
    IOn_Destroy
  }
  var
    i: Integer;
    handler: PMethod;
  begin
    if NOT Assigned(self) then
      EXIT;

    for i := 0 to Pred(fMethods.Count) do
    begin
      handler := fMethods[i];

      if (aMethod.Code = handler.Code) and (aMethod.Data = handler.Data) then
      begin
        Dispose(handler);
        fMethods.Delete(i);

        // Only one reference to any method can be attached to any one event, so
        //  once we have found and removed the method there is no need to check the
        //  remaining method references.
        BREAK;
      end;
    end;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.ListenerDestroyed(aSender: TObject);
  {@@TMultiCastEvent.ListenerDestroyed

  Parameters

    aSender - The object being destroyed that implements the IOn_Destroy
               interface and which has added one or more handlers to this event.

  Description

    This is the built-in handler for an On_Destroyed event sent from a receiver
     when that receiver is destroyed.  Inspects every handler looking for and
     removing any that are implemented by the aSender.
  }
  var
    i: Integer;
    method: PMethod;
  begin
    for i := 0 to Pred(Count) do
    begin
      method := fMethods[i];
      if (method.Data = Pointer(aSender)) then
      begin
        Dispose(method);
        fMethods[i] := NIL;
      end;
    end;

    fMethods.Pack;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.DoEvent;
  {@@TMultiCastEvent.DoEvent

  Calls every handler in the list of handlers for the event.


  * Important *

    DoEvent is NOT virtual, and is a PUBLIC method in this class.

    Since the Add and Remove methods are only PROTECTED this means that a
     TMultiCastEvent cannot be used directy to implement a multi cast event -
     a class must be derived which implements PUBLIC Add and Remove methods
     that accept methods with a specific signature.

    The default DoEvent implementation may then be used on the derived class
     to call those handlers, but ONLY if they are of a type that does not
     require any parameters OR if all required parameters are available from
     the member variables of the event class (e.g. TMultiCastNotify).

    If the methods that handle the event requires parameters, then the DoEvent
     method MUST be reintroduced in the derived class, to accept any parameters
     that are to be passed to each handler and hide this base implementation.
  }
  var
    i: Integer;
  begin
    if NOT Assigned(self) or (NOT Enabled) or Active then
      EXIT;

    fActive := TRUE;
    try
      for i := 0 to Pred(Count) do
        Call(Method[i]);
    finally
      fActive := FALSE;
    end;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.Enable;
  {
    Enables or Disables the event.  An event is create in an Enabled state
     and remains Enabled only as long as all calls to Disable have been
     balanced by a call to Enable.

    That is, if an event has been Disabled twice then it must be Enabled
     twice in order to become Enabled (un-Disabled) once again (Example 1).

    For this reason it is strongly recommended that event enabling and
     disabling should be performed in balanced try..finally constructs (Example2)

    Example

      <code>
        event.Disable;
        event.Disable;
        event.Enable;   // event is still DISabled
        event.Enable;   // event is now enabled
      </code>

    Example

      <code>
        event.Disable;
        try
          // Do work ...
        finally
          event.Enable;
        end;
      </code>
  }
  begin
    Dec(fDisableCount);
    ASSERT(fDisableCount >= 0);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.Disable;
  {
    <COMBINE TMultiCastEvent.Enable>
  }
  begin
    Inc(fDisableCount);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastEvent.GetListeners(const aList: TList);
  {
    Initialises a specified list with all (unique) listeners that have
     handlers currently assigned to the event.  The current contents of
     aList will be cleared.
  }
  var
    i: Integer;
  begin
    aList.Clear;
    for i := 0 to Pred(Count) do
      if aList.IndexOf(Method[i].Data) = -1 then
        aList.Add(Method[i].Data);
  end;









{ TMultiCastNotify ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TMultiCastNotify.CreateEvents(const aSender: TObject;
                                                const aEvents: array of PMultiCastNotify);
  {@@TMultiCastNotify.CreateEvents

    Parameters

      aSender - The ultimate sender of the events being created.

      aEvents - An array of pointers to TMultiCastNotify references.


    Description

      Creates any number of TMultiCastNotify objects and places references to those
       objects at the locations specified by the pointers in the array.


    Exceptions

      EInvalidPointer - Raised from a call to CheckReferences if the events array
                         contains duplicate pointers or references that have already
                         been assigned.

                        CheckReferences is only called if ASSERT() statements
                         are enabled.

  }
  var
    i: Integer;
  begin
    {$ifopt C+}
    CheckReferences(aEvents, Length(aEvents));
    {$endif}

    for i := Low(aEvents) to High(aEvents) do
      aEvents[i]^ := Create(aSender);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TMultiCastNotify.Create(const aSender: TObject);
  {@@TMultiCastNotify.Create

  Parameters

    aSender - The object that is to be considered the Sender of the
               notification events.  Typically this will be the creator of
               the multi-cast event itself.

  Description

    Constructor for multi-cast notification events.  Records a specified
     sender object which will be passed in the Sender parameter of the
     TNotifyEvent handlers that called when the multi-cast notify event is
     fired.
  }
  begin
    inherited Create;

    fSender := aSender;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastNotify.Add(const aHandler: TNotifyEvent);
  {@@TMultiCastNotify.Add

  Parameters

    aHandler - A TNotifyEvent handler to be added to the multi-cast handler
                list.

  Description

    Adds a specified TNotifyEvent handler to the multi-cast event.  If the
     handler is already in the list of handlers for the event it will not
     be added again.
  }
  begin
    inherited Add(TMethod(aHandler));
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastNotify.Insert(const aHandler: TNotifyEvent);
  {@@TMultiCastNotify.Insert

  Parameters

    aHandler - A TNotifyEvent handler to be added to TOP of the multi-cast
                handler list.

  Description

    Adds a specified TNotifyEvent handler to the multi-cast event.  If the
     handler is already in the list of handlers for the event it will not
     be added again.
  }
  begin
    inherited Insert(TMethod(aHandler));
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastNotify.Remove(const aHandler: TNotifyEvent);
  {@@TMultiCastNotify.Remove

  Parameters

    aHandler - A TNotifyEvent handler to be removed from the multi-cast
                handler list.

  Description

    Removes a specified TNotifyEvent handler from the multi-cast event.
  }
  begin
    inherited Remove(TMethod(aHandler));
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastNotify.Call(const aMethod: TMethod);
  {@@TMultiCastNotify.Call

    Calls the specified handler passing the event Sender in the Sender
     parameter.
  }
  begin
    TNotifyEvent(aMethod)(Sender);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TMultiCastNotify.DoEventFor(const aSender: TObject);
  {@@TMultiCastNotify.DoEventFor

    Call all handlers for the multi-cast event, specifying an object to be
     passed as the Sender parameter for each handler.

    An example of when this method might be used is in the implementation of
     some collection class, where the collection provides an On_Change event
     to notify interested parties of a change to some item in the collection
     or to the collection itself.

    Changes to the collection would call Collection.On_Change.DoEvent,
     passing the collection as the Sender to all handlers.  Changes to an
     item in the collection would call Collection.On_Change.DoEventFor(Item)
     passing the specific item as the Sender to all handlers.
  }
  var
    originalSender: TObject;
  begin
    originalSender := Sender;
    fSender := aSender;
    try
      DoEvent;
    finally
      fSender := originalSender;
    end;
  end;



{ TOn_Destroy ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- }

  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TOnDestroy.Create(const aOwner: TObject);
  {@@TOnDestroy.Create

    Creates a new instance of a TOnDestroy class encapsulating an On_Destroy
     event, implemented on behalf of the owner.  The owner should declare
     support for the IOn_Destroy interface and delegate that interface to
     an instance of this class.

    Parameters

      aOwner    The owner of the On_Destroy event implemented by the class.
                 This is the Sender parameter that will be provided when
                 the On_Destroy event is fired.

    See also
      IOn_Destroy
  }
  begin
    inherited Create;
    fEvent := TMultiCastNotify.Create(aOwner);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TOnDestroy.Destroy;
  {@@TOnDestroy.Destroy

    Destructor for an On_Destroy implementation.  Fires the On_Destroy event
     before calling the inherited destructor.
  }
  begin
    try
      fEvent.DoEvent;

    finally
      FreeAndNIL(fEvent);
    end;

    inherited;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOnDestroy.Add(const aHandler: TNotifyEvent);
  {@@TOnDestroy.Add

    Adds a specified handler to the encapsulated On_Destroy event.
  }
  begin
    fEvent.Add(aHandler);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOnDestroy.Remove(const aHandler: TNotifyEvent);
  {@@TOnDestroy.Remove

    Removes a specified handler from the encapsulated On_Destroy event.
  }
  begin
    fEvent.Remove(aHandler);
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOnDestroy.DoEvent;
  begin
    fEvent.DoEvent;
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOnDestroy.GetListeners(const aList: TList);
  begin
    fEvent.GetListeners(aList);
  end;




end.
