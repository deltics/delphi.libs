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

{$ifdef deltics_classes}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Classes;


interface

  uses
  { vcl:}
    Classes,
    Contnrs,
    Types,
  { deltics: }
    Deltics.MultiCast;


  type
    IAsObject = interface;

    PUnknown = ^IUnknown;


    TNamedNotifyEvent = procedure(const aSender: TObject; const aName: String) of object;


    TInterfacedObject = class(TObject, IUnknown,
                                       IOn_Destroy)
      // IUnknown
      protected
        function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;

      // IOn_Destroy
      private
        fOn_Destroy: IOn_Destroy;
        function get_On_Destroy: IOn_Destroy;
      public
        property On_Destroy: IOn_Destroy read get_On_Destroy implements IOn_Destroy;
    end;


    TInterfacedPersistent = class(TPersistent, IUnknown,
                                               IOn_Destroy)
      // IUnknown
      protected
        function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;

      // IOn_Destroy
      private
        fOn_Destroy: IOn_Destroy;
        function get_On_Destroy: IOn_Destroy;
      public
        property On_Destroy: IOn_Destroy read get_On_Destroy implements IOn_Destroy;
    end;


    TCOMInterfacedObject = class(TObject, IUnknown,
                                          IOn_Destroy)
      // IOn_Destroy
      private
        fDestroying: Boolean;
        fRefCount: Integer;
        fOn_Destroy: IOn_Destroy;
        function get_On_Destroy: IOn_Destroy;

      public // IUnknown
        function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;

      public
        class function NewInstance: TObject; override;
        procedure AfterConstruction; override;
        procedure BeforeDestruction; override;
        property RefCount: Integer read fRefCount;
        property On_Destroy: IOn_Destroy read get_On_Destroy implements IOn_Destroy;
    end;


    TCOMInterfacedPersistent = class(Classes.TInterfacedPersistent, IOn_Destroy)
      // IOn_Destroy
      private
        fOn_Destroy: IOn_Destroy;
        function get_On_Destroy: IOn_Destroy;
      public
        property On_Destroy: IOn_Destroy read get_On_Destroy implements IOn_Destroy;
    end;


    TFlexInterfacedObject = class(TObject, IUnknown,
                                           IOn_Destroy)
      private
        fRefCount: Integer;
        fRefCountDisabled: Boolean;
      public
        procedure AfterConstruction; override;
        procedure BeforeDestruction; override;
        class function NewInstance: TObject; override;

      public
        procedure DisableRefCount;
        property RefCount: Integer read fRefCount;

      // IUnknown
      protected
        function QueryInterface(const aIID: TGUID; out aObj): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;

      // IOn_Destroy
      private
        fOn_Destroy: IOn_Destroy;
        function get_On_Destroy: IOn_Destroy;
      public
        property On_Destroy: IOn_Destroy read get_On_Destroy implements IOn_Destroy;
    end;


    TTypedList = class(TInterfacedObject)
    private
      fList: TList;
      function get_Count: Integer;
      function get_Item(const aIndex: Integer): TObject;
      function get_OwnsObjects: Boolean;
      procedure set_Item(const aIndex: Integer; const aValue: TObject);
    protected
      procedure Add(const aItem: TObject);
      function IndexOf(const aItem: TObject): Integer;
      procedure Insert(const aIndex: Integer; const aItem: TObject);
      procedure Remove(const aItem: TObject);
      property Items[const aIndex: Integer]: TObject read get_Item write set_Item;
      property List: TList read fList;
    public
      constructor Create(const aOwnsObjects: Boolean = FALSE);
      destructor Destroy; override;
      procedure Clear;
      procedure Delete(const aIndex: Integer);
      property Count: Integer read get_Count;
      property OwnsObjects: Boolean read get_OwnsObjects;
    end;


    TInterface = class(TObject, IUnknown)
    private
      fRef: IUnknown;
    protected // IUnknown
      {
        IUnknown is delegated to the contained reference using "implements"
         ALL methods of IUnknown are delegated to the fRef, meaning that
         TInterface does not need to worry about being reference counted
         itself (it won't be).
      }
      property Ref: IUnknown read fRef implements IUnknown;
    public
      constructor Create(const aRef: IUnknown);
      function IsEqual(const aOther: IUnknown): Boolean;
    end;


    TWeakInterface = class(TObject, IUnknown)
    private
      fRef: Pointer;
      function get_Ref: IUnknown;
    protected // IUnknown
      {
        IUnknown is delegated to the contained reference using "implements"
         ALL methods of IUnknown are delegated to the fRef, meaning that
         TWeakInterface does not need to worry about being reference counted
         itself (it won't be).
      }
      property Ref: IUnknown read get_Ref implements IUnknown;
    public
      constructor Create(const aRef: IUnknown);
      procedure Update(const aRef: IUnknown);
    end;


    TInterfaceDelegate = class(TObject, IUnknown)
    protected
      fOwner: TObject;
      fRefCount: Integer;
      fRefCountEnabled: Boolean;
      function QueryInterface(const aIID: TGUID; out aObj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    public
      constructor Create(const aOwner: TObject);
      procedure DisableRefCount;
      procedure EnableRefCount;
    end;


    TGUIDList = class
    private
      fCount: Integer;
      fIsSorted: Boolean;
      fItems: array of TGUID;
      fSorted: Boolean;
      function get_Capacity: Integer;
      function get_Item(const aIndex: Integer): TGUID;
      procedure set_Capacity(const aValue: Integer);
      procedure set_Item(const aIndex: Integer; const aValue: TGUID);
      procedure set_Sorted(const aValue: Boolean);
    protected
      procedure IncreaseCapacity;
    public
      procedure Add(const aGUID: TGUID);
      procedure Clear;
      function Contains(const aGUID: TGUID): Boolean;
      procedure Delete(const aGUID: TGUID);
      function IndexOf(const aGUID: TGUID): Integer;
      procedure Sort;
      property Capacity: Integer read get_Capacity write set_Capacity;
      property Count: Integer read fCount;
      property Items[const aIndex: Integer]: TGUID read get_Item write set_Item; default;
      property Sorted: Boolean read fSorted write set_Sorted;
    end;


    TInterfacedObjectList = class
    private
      fItems: TObjectList;
      function get_Count: Integer;
      function get_Item(const aIndex: Integer): TObject;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(const aObject: TObject): Integer;
      property Count: Integer read get_Count;
      property Items[const aIndex: Integer]: TObject read get_Item; default;
    end;


    IAsObject = interface
    ['{668F826E-A31B-4CB3-B5F9-BF91967A5716}']
      function get_AsObject: TObject;
      property AsObject: TObject read get_AsObject;
    end;


  {$ifdef DELPHI5_OR_LATER}
    TCollection = Classes.TOwnedCollection;
  {$else}
    TCollection = class(Classes.TCollection)
    private
      fOwner: TPersistent;
    protected
      function GetOwner: TPersistent; override;
    public
      constructor Create(aOwner: TPersistent; aItemClass: TCollectionItemClass);
    end;
  {$endif}


    TStringlist = class(Classes.TStringList)
    private
      function get_Integer(const aIndex: Integer): Integer;
    public
      function Add(const aString: String): Integer; reintroduce; overload; override;
      function Add(const aString: String; const aInteger: Integer): Integer; reintroduce; overload;
      function Contains(const aString: String): Boolean;
      function ContainsName(const aName: String): Boolean;
      procedure Remove(const aString: String);
      property Integers[const aIndex: Integer]: Integer read get_Integer;
    end;



implementation

  uses
  { vcl: }
    Windows,
  { deltics: }
    Deltics.SysUtils;


{ TInterfacedObject ------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedObject.QueryInterface(const IID: TGUID;
                                            out Obj): HResult;
  begin
    if GetInterface(IID, Obj) then
      Result := 0
    else
      Result := E_NOINTERFACE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedObject._AddRef: Integer;
  begin
    result := 1; { NO-OP }
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedObject._Release: Integer;
  begin
    result := 1; { NO-OP }
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedObject.get_On_Destroy: IOn_Destroy;
  // Create the multi-cast event on demand, since we cannot
  //  guarantee any particular constructor call order and there
  //  may be dependencies created during construction (e.g. if
  //  multi-cast event handlers are added before/after any call
  //  to a particular inherited constructor etc etc)
  begin
    if NOT Assigned(fOn_Destroy) then
      fOn_Destroy := TOnDestroy.Create(self);

    result := fOn_Destroy;
  end;







{ TInterfacedPersistent -------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedPersistent.QueryInterface(const IID: TGUID;
                                                out Obj): HResult;
  begin
    if GetInterface(IID, Obj) then
      Result := 0
    else
      Result := E_NOINTERFACE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedPersistent._AddRef: Integer;
  begin
    result := 1; { NO-OP }
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedPersistent._Release: Integer;
  begin
    result := 1; { NO-OP }
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedPersistent.get_On_Destroy: IOn_Destroy;
  // Create the multi-cast event on demand, since we cannot
  //  guarantee any particular constructor call order and there
  //  may be dependencies created during construction (e.g. if
  //  multi-cast event handlers are added before/after any call
  //  to a particular inherited constructor etc etc)
  begin
    if NOT Assigned(fOn_Destroy) then
      fOn_Destroy := TOnDestroy.Create(self);

    result := fOn_Destroy;
  end;








{ TCOMInterfacedObject --------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCOMInterfacedObject._AddRef: Integer;
  begin
    if NOT (fDestroying) then
      result := InterlockedIncrement(fRefCount)
    else
      result := 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCOMInterfacedObject._Release: Integer;
  begin
    if NOT (fDestroying) then
    begin
      result := InterlockedDecrement(fRefCount);
      if (result = 0) then
        Destroy;
    end
    else
      result := 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCOMInterfacedObject.AfterConstruction;
  begin
    inherited;
    InterlockedDecrement(fRefCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCOMInterfacedObject.BeforeDestruction;
  begin
    fDestroying := TRUE;
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCOMInterfacedObject.get_On_Destroy: IOn_Destroy;
  begin
    if NOT Assigned(fOn_Destroy) then
      fOn_Destroy := TOnDestroy.Create(self);

    result := fOn_Destroy;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TCOMInterfacedObject.NewInstance: TObject;
  begin
    result := inherited NewInstance;
    InterlockedIncrement(TCOMInterfacedObject(result).fRefCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCOMInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    if GetInterface(IID, Obj) then
      result := 0
    else
      result := E_NOINTERFACE;
  end;







{ TCOMInterfacedPersistent ----------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCOMInterfacedPersistent.get_On_Destroy: IOn_Destroy;
  begin
    if NOT Assigned(fOn_Destroy) then
      fOn_Destroy := TOnDestroy.Create(self);

    result := fOn_Destroy;
  end;




{ TTypedList ------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTypedList.Create(const aOwnsObjects: Boolean);
  begin
    inherited Create;

    if aOwnsObjects then
      fList := TObjectList.Create(TRUE)
    else
      fList := TList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTypedList.Destroy;
  begin
    FreeAndNIL(fList);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTypedList.get_Count: Integer;
  begin
    result := fList.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTypedList.get_Item(const aIndex: Integer): TObject;
  begin
    result := TObject(fList[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTypedList.get_OwnsObjects: Boolean;
  begin
    result := (fList is TObjectList);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTypedList.set_Item(const aIndex: Integer; const aValue: TObject);
  begin
    fList[aIndex] := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTypedList.Add(const aItem: TObject);
  begin
    fList.Add(aItem)
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTypedList.Clear;
  begin
    fList.Clear;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTypedList.Delete(const aIndex: Integer);
  begin
    fList.Delete(aIndex);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTypedList.IndexOf(const aItem: TObject): Integer;
  begin
    result := fList.IndexOf(aItem);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTypedList.Insert(const aIndex: Integer; const aItem: TObject);
  begin
    fList.Insert(aIndex, aItem);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTypedList.Remove(const aItem: TObject);
  begin
    fList.Remove(aItem);
  end;







{ TInterface ------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TInterface.Create(const aRef: IInterface);
  begin
    inherited Create;

    fRef := aRef as IUnknown;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterface.IsEqual(const aOther: IInterface): Boolean;
  begin
    if Assigned(self) then
      result := (aOther as IUnknown) = fRef
    else
      result := (aOther = NIL);
  end;








{ TWeakInterface --------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TWeakInterface.Create(const aRef: IInterface);
  begin
    inherited Create;

    fRef := Pointer(aRef);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWeakInterface.get_Ref: IUnknown;
  begin
    result := IUnknown(fRef);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWeakInterface.Update(const aRef: IInterface);
  begin
    fRef := Pointer(aRef);
  end;








{ TInterfaceDelegate ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TInterfaceDelegate.Create(const aOwner: TObject);
  begin
    inherited Create;

    fOwner := aOwner;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInterfaceDelegate.DisableRefCount;
  begin
    fRefCountEnabled := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInterfaceDelegate.EnableRefCount;
  begin
    fRefCountEnabled := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfaceDelegate.QueryInterface(const aIID: TGUID; out aObj): HResult;
  const
    SUCCESS = 0;
  begin
    if fOwner.GetInterface(aIID, aObj) then
      result := SUCCESS
    else
      result := E_NOINTERFACE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfaceDelegate._AddRef: Integer;
  begin
    result := InterlockedIncrement(fRefCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfaceDelegate._Release: Integer;
  begin
    result := InterlockedDecrement(fRefCount);

    if fRefCountEnabled and (fRefCount = 0) then
      fOwner.Free;
  end;







{ TFlexInterfacedObject - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TFlexInterfacedObject.AfterConstruction;
  begin
    if fRefCountDisabled then
      EXIT;

    // Release the constructor's implicit refcount
    InterlockedDecrement(fRefCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TFlexInterfacedObject.BeforeDestruction;
  begin
    if NOT fRefCountDisabled and (fRefCount <> 0) then
      System.Error(reInvalidPtr);

    DisableRefCount;  // To avoid problems if we reference ourselves (or are
                      //  referenced by others) using an interface during
                      //  execution of the destructor chain
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TFlexInterfacedObject.DisableRefCount;
  begin
    fRefCountDisabled := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TFlexInterfacedObject.get_On_Destroy: IOn_Destroy;
  begin
    if NOT Assigned(fOn_Destroy) then
      fOn_Destroy := TOnDestroy.Create(self);

    result := fOn_Destroy;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TFlexInterfacedObject.NewInstance: TObject;
  begin
    result := inherited NewInstance;
    TFlexInterfacedObject(result).fRefCount := 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TFlexInterfacedObject.QueryInterface(const aIID: TGUID; out aObj): HResult;
  begin
    if GetInterface(aIID, aObj) then
      result := 0
    else
      result := E_NOINTERFACE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TFlexInterfacedObject._AddRef: Integer;
  begin
    if fRefCountDisabled then
      result := 1
    else
      result := InterlockedIncrement(fRefCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TFlexInterfacedObject._Release: Integer;
  begin
    if fRefCountDisabled then
      result := 1
    else
    begin
      result := InterlockedDecrement(fRefCount);
      if (result = 0) then
        Destroy;
    end;
  end;







{ TGUIDList -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TGUIDList.Add(const aGUID: TGUID);
  begin
    if (fCount = Capacity) then
      IncreaseCapacity;

    fItems[fCount] := aGUID;

    Inc(fCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TGUIDList.Clear;
  begin
    fCount := 0;
    SetLength(fItems, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TGUIDList.Contains(const aGUID: TGUID): Boolean;
  begin
    result := (IndexOf(aGUID) <> -1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TGUIDList.Delete(const aGUID: TGUID);
  var
    i: Integer;
  begin
    i := IndexOf(aGUID);

    if i = -1 then
      EXIT;

    while (i < Pred(Count)) do
      fItems[i] := fItems[i + 1];

    Dec(fCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TGUIDList.get_Capacity: Integer;
  begin
    result := Length(fItems);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TGUIDList.get_Item(const aIndex: Integer): TGUID;
  begin
    result := fItems[aIndex];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TGUIDList.IncreaseCapacity;
  var
    i: Integer;
  begin
    case Capacity of
      0     : i := 4;
      1..64 : i := Capacity * 2
    else
      i := Capacity div 4;
    end;

    SetLength(fItems, i);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TGUIDList.IndexOf(const aGUID: TGUID): Integer;
  begin
    if fIsSorted then
    begin
      // TODO: Binary search algorithm
    end
    else
      for result := 0 to Pred(Count) do
        if SameGUID(aGUID, fItems[result]) then
          EXIT;

    result := -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TGUIDList.set_Capacity(const aValue: Integer);
  begin
    SetLength(fItems, aValue);
    if (Capacity < fCount) then
      fCount := Capacity;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TGUIDList.set_Item(const aIndex: Integer; const aValue: TGUID);
  begin
    fItems[aIndex] := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TGUIDList.set_Sorted(const aValue: Boolean);
  begin
    if fSorted = aValue then
      EXIT;

    fSorted := aValue;

    if fSorted and NOT fIsSorted then
      Sort;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TGUIDList.Sort;
  begin
    ASSERT(FALSE, 'Oops, hasn''t been implemented yet!');

    fIsSorted := TRUE;
  end;






{ TInterfacedObjectList -------------------------------------------------------------------------- }

  type
    TInterfacedObjectListItem = class
      ItemObject: TObject;
      ItemInterface: IUnknown;
    end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TInterfacedObjectList.Create;
  begin
    inherited Create;

    fItems := TObjectList.Create(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TInterfacedObjectList.Destroy;
  begin
    FreeAndNIL(fItems);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedObjectList.Add(const aObject: TObject): Integer;
  var
    item: TInterfacedObjectListItem;
  begin
    item := TInterfacedObjectListItem.Create;
    item.ItemObject     := aObject;

    if Assigned(aObject) then
      aObject.GetInterface(IUnknown, item.ItemInterface);

    result := fItems.Add(item);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedObjectList.get_Count: Integer;
  begin
    result := fItems.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfacedObjectList.get_Item(const aIndex: Integer): TObject;
  begin
    result := TInterfacedObjectListItem(fItems[aIndex]).ItemObject;
  end;







{$ifNdef DELPHI5_OR_LATER}
{ TCollection ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCollection.Create(aOwner: TPersistent;
                                 aItemClass: TCollectionItemClass);
  begin
    inherited Create(aItemClass);
    fOwner := aOwner;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCollection.GetOwner: TPersistent;
  begin
    result := fOwner;
  end;
{$endif}








{ TStringList ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringlist.Add(const aString: String): Integer;
  begin
    result := inherited Add(aString);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringlist.Add(const aString: String;
                           const aInteger: Integer): Integer;
  begin
    result := AddObject(aString, TObject(aInteger));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringlist.Contains(const aString: String): Boolean;
  begin
    result := (IndexOf(aString) <> -1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringlist.ContainsName(const aName: String): Boolean;
  begin
    result := IndexOfName(aName) <> -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringlist.get_Integer(const aIndex: Integer): Integer;
  begin
    result := Integer(Objects[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TStringlist.Remove(const aString: String);
  var
    idx: Integer;
  begin
    idx := IndexOf(aString);
    if idx <> -1 then
      Delete(idx);
  end;








end.
