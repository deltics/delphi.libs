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

{$ifdef deltics_threads}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Threads.Storage;


interface

  uses
  { vcl: }
    Classes,
    Contnrs,
  { deltics: }
    Deltics.Classes;


  type
    ICreateObjectForThread = interface
    ['{0B0A06D8-CA7A-45E9-AE44-521D816B5986}']
      function CreateObject: TObject;
    end;


    TThreadObject = record
      ThreadID: Cardinal;
      Reference: TObject;
    end;
    PThreadObject = ^TThreadObject;


    TThreadObjectHandler = procedure(const aObject: PThreadObject) of object;


    TCustomPerThreadObject = class(TInterfacedObject)
    private
      fObjects: TThreadList;
      fOwnsObjects: Boolean;
      procedure DoForCurrentThreadObject(const aHandler: TThreadObjectHandler);
      function InternalAdd(const aObject: TObject; const aThreadID: Cardinal = 0): PThreadObject;
      procedure InternalClear; overload;
      procedure InternalClear(const aObject: PThreadObject); overload;
      procedure InternalDelete; overload;
      procedure InternalDelete(const aObject: PThreadObject); overload;
      function InternalGet: PThreadObject;
    public
      constructor Create(const aOwnsObjects: Boolean = TRUE);
      destructor Destroy; override;
    end;


    TPerThreadObject = class(TCustomPerThreadObject)
    public
      procedure Clear;
      function Get: TObject;
      procedure Assign(const aObject: TObject);
    end;


    TPerThreadObjectStack = class(TCustomPerThreadObject, ICreateObjectForThread)
    private
      function get_Count: Integer;
      function get_Stack: TStack;
      property Stack: TStack read get_Stack;
    public
      constructor Create; reintroduce;
      function Peek: TObject;
      function Pop: TObject;
      procedure Push(const aObject: TObject);
      property Count: Integer read get_Count;

    private // ICreateObjectForThread
      function CreateObject: TObject;
    end;



implementation

  uses
  { vcl: }
    SysUtils,
    Types,
    Windows;


{ TThreadObjects --------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCustomPerThreadObject.Create(const aOwnsObjects: Boolean);
  begin
    inherited Create;

    fObjects      := TThreadList.Create;
    fOwnsObjects  := aOwnsObjects;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TCustomPerThreadObject.Destroy;
  var
    i: Integer;
    objects: TList;
    obj: PThreadObject;
  begin
    if fOwnsObjects then
    begin
      objects := fObjects.LockList;
      try
        for i := 0 to Pred(objects.Count) do
        begin
          obj := PThreadObject(objects[i]);

          obj.Reference.Free;
          obj.Reference := NIL;

          Dispose(obj);
        end;

      finally
        fObjects.UnlockList;
      end;
    end;

    FreeAndNIL(fObjects);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCustomPerThreadObject.DoForCurrentThreadObject(const aHandler: TThreadObjectHandler);
  var
    i: Integer;
    obj: PThreadObject;
    threadID: Cardinal;
    threads: TList;
  begin
    threadID := GetCurrentThreadID;

    threads := fObjects.LockList;
    try
      for i := 0 to Pred(threads.Count) do
      begin
        obj := PThreadObject(threads[i]);

        if obj.ThreadID = threadID then
        begin
          aHandler(obj);
          BREAK;
        end;
      end;

    finally
      fObjects.UnlockList;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCustomPerThreadObject.InternalAdd(const aObject: TObject;
                                              const aThreadID: Cardinal): PThreadObject;
  begin
    New(result);

    result.ThreadID  := aThreadID;
    result.Reference := aObject;

    if result.ThreadID = 0 then
      result.ThreadID := GetCurrentThreadID;

    fObjects.LockList.Add(result);
    fObjects.UnlockList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCustomPerThreadObject.InternalClear;
  {
    Locates the ThreadObject for the current thread and NILs the reference
     but leaves it in the list.

    If the ThreadObject contains a NON-NIL reference and objects are owned
     then the referenced object is Free'd before it is NIL'd.
  }
  begin
    DoForCurrentThreadObject(InternalClear);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCustomPerThreadObject.InternalClear(const aObject: PThreadObject);
  {
    NILs the reference on the specified thread object.

    If the ThreadObject contains a NON-NIL reference and objects are owned
     then the referenced object is Free'd before it is NIL'd.
  }
  begin
    if fOwnsObjects then
      aObject.Reference.Free;

    aObject.Reference := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCustomPerThreadObject.InternalDelete;
  begin
    DoForCurrentThreadObject(InternalDelete);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCustomPerThreadObject.InternalDelete(const aObject: PThreadObject);
  {
    Locates the ThreadObject for the current thread and removes it.

    If the ThreadObject contains a NON-NIL reference and objects are owned
     then the referenced object is Free'd before it is removed.
  }
  begin
    if fOwnsObjects then
      aObject.Reference.Free;

    Dispose(aObject);

    fObjects.LockList.Remove(aObject);
    fObjects.UnlockList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCustomPerThreadObject.InternalGet: PThreadObject;
  var
    i: Integer;
    threadID: Cardinal;
    objects: TList;
    factory: ICreateObjectForThread;
  begin
    result    := NIL;
    threadID  := GetCurrentThreadID;

    objects := fObjects.LockList;
    try
      for i := 0 to Pred(objects.Count) do
        if PThreadObject(objects[i]).ThreadID = threadID then
        begin
          result := PThreadObject(objects[i]);
          EXIT;
        end;

      if GetInterface(ICreateObjectForThread, factory) then
        result := InternalAdd(factory.CreateObject, threadID);

    finally
      fObjects.UnlockList;
    end;
  end;







{ TPerThreadObject ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerThreadObject.Assign(const aObject: TObject);
  var
    obj: PThreadObject;
  begin
    obj := InternalGet;
    if Assigned(obj) then
    begin
      InternalClear(obj);

      obj.Reference := aObject;
    end
    else
      InternalAdd(aObject);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerThreadObject.Clear;
  begin
    InternalDelete;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerThreadObject.Get: TObject;
  var
    obj: PThreadObject;
  begin
    obj := InternalGet;
    if Assigned(obj) then
      result := obj.Reference
    else
      result := NIL;
  end;






{ TPerThreadObjectStack -------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TPerThreadObjectStack.Create;
  begin
    inherited Create(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerThreadObjectStack.CreateObject: TObject;
  begin
    result := TStack.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerThreadObjectStack.get_Count: Integer;
  begin
    result := Stack.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerThreadObjectStack.get_Stack: TStack;
  begin
    result := TStack(InternalGet.Reference);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerThreadObjectStack.Peek: TObject;
  begin
    result := TObject(Stack.Peek);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerThreadObjectStack.Pop: TObject;
  begin
    result := TObject(Stack.Pop);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerThreadObjectStack.Push(const aObject: TObject);
  begin
    Stack.Push(aObject);
  end;





initialization
  {$ifNdef ALLHINTS}
    if FALSE then TPerThreadObjectStack.Create.InternalDelete(NIL);
  {$endif}

end.
