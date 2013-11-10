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

{@@Deltics.RTTI.pas

  Contains classes and routines to make it easier to work with various
   elements of RTTI.  Only fairly basic support is currently implemented,
   although support for more detailed RTTI - in particular for methods -
   is planned.

  As an example, to populate a listbox with the names of properties
   published by an object:

    <code>
    var
      props: TPropertyList;
    begin
      props := TPropertyList.Create(obj);
      try
        props.GetNames(ListBox.Items);
      finally
        props.Free;
      end;
    end;
    </code>

  Or the same for the list of methods:

    <code>
    var
      methods: TMethodList;
    begin
      methods := TMethodList.Create(obj);
      try
        methods.GetNames(ListBox.Items);
      finally
        methods.Free;
      end;
    end;
    </code>
}


{$i deltics.rtl.inc}

{$ifdef deltics_rtti}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.RTTI;


interface

  uses
  { vcl: }
    Classes,
    TypInfo,
    Variants
  {$ifdef DELPHI2006_OR_LATER}
    {$ifdef DELPHIXE_OR_EARLIER}
    ,Windows          // For inlining ANSISameText (TODO: Implement ANSI.SameText in Deltics.Strings
    {$endif}
  {$endif};



  type
    TRTTIList = class
    {
      This is the base class for lists containing RTTI information relating to
       a specific class (or instance of a class).

      This base list class currently encapsulates only that (currently at
       least), all RTTI information is associated with a named entity, be that
       a property or a method etc, and so the base list provides support for
       the name of each item even though what those items are is only decided
       by the RTTI-type specific derived lists.

      These lists are designed to be populated by identifying a type (i.e. class)
       in the constructor.  They are not designed to be general purpose containers
       and so no capabilities are provided for adding or removing items.
    }
    protected
      function get_Count: Integer; virtual; abstract;
      function get_Name(const aIndex: Integer): String; virtual; abstract;
    public
      constructor Create(const aClass: TClass); overload; virtual; abstract;
      constructor Create(const aObject: TObject); overload; virtual;
      procedure GetNames(const aList: TStrings);
      function IndexOf(const aName: String): Integer;
      property Count: Integer read get_Count;
      property Name[const aIndex: Integer]: String read get_Name; default;
    end;


    TMethodList = class(TRTTIList)
    {
      A list of methods - that is public and/or published procedures and
       functions, not event handlers, for a specified class or instance of
       a class.

     ## TODO:
     ##
     ## In addition to the Names[] array property supported by all RTTI item
     ##  lists, this list class also supports a number of additional array
     ##  type properties that expose additional information relating to each
     ##  method in the list.
    }
    private
      fNames: array of String;
    protected
      function get_Count: Integer; override;
      function get_Name(const aIndex: Integer): String; override;
    public
      constructor Create(const aClass: TClass); override;
      destructor Destroy; override;
    end;


    TPropertyList = class(TRTTIList)
    {
      A list of properties published by a specified class or instance of
       a class.

      In addition to the Names[] array property supported by all RTTI item
       lists, this list class also supports Index and Kind array properties.
    }
    private
      fContext: TObject;
      fCount: Integer;
      fPropList: PPropList;
      function get_AsString(const aIndex: Integer): String;
      function get_Index(const aIndex: Integer): Integer;
      function get_Kind(const aIndex: Integer): TTypeKind;
      procedure set_AsString(const aIndex: Integer; const aValue: String);
    protected
      function get_Count: Integer; override;
      function get_Name(const aIndex: Integer): String; override;
    public
      constructor Create(const aClass: TClass); override;
      constructor Create(const aObject: TObject); override;
      destructor Destroy; override;
      procedure GetNames(const aList: TStrings; const aKind: TTypeKind); overload;
      property AsString[const aIndex: Integer]: String read get_AsString write set_AsString;
      property Index[const aIndex: Integer]: Integer read get_Index;
      property Kind[const aIndex: Integer]: TTypeKind read get_Kind;
    end;


  const
    tkAny         = [Low(TTypeKind)..High(TTypeKind)];
    tkMethods     = [tkMethod];
    tkProperties  = tkAny - tkMethods - [tkUnknown];


  function EnumToStr(const aEnumInfo: PTypeInfo; const aEnum: Variant): String;
  function StrToEnum(const aEnumInfo: PTypeInfo; const aStr: String): Integer;


implementation

  uses
  { vcl: }
    SysUtils,
  {$ifdef DELPHI2010_OR_LATER}
    RTTI;
  {$else}
  { deltics: }
    Deltics.VMT;
  {$endif}



  function EnumToStr(const aEnumInfo: PTypeInfo; const aEnum: Variant): String;
  {
    A slightly easier to use wrapper around GetEnumName with a more intuitive
     partner in the form of StrToEnum.

    Normally you would call:

    <code>
      GetEnumName(TypeInfo(TEnumType), Ord(someValue));
    </code>

    You may instead now use:

    <code>
      EnumToStr(TypeInfo(PTypeInfo), someValue);
    </code>
  }
  begin
    result := TypInfo.GetEnumName(aEnumInfo, VarAsType(aEnum, vtInteger));
  end;


  function StrToEnum(const aEnumInfo: PTypeInfo; const aStr: String): Integer;
  {
    A wrapper around GetEnumValue providing a more consistent and intuitive
     partner to EnumToStr.
  }
  begin
    result := TypInfo.GetEnumValue(aEnumInfo, aStr);
  end;






{-- TRTTIList -------------------------------------------------------------------------------------}

  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TRTTIList.Create(const aObject: TObject);
  begin
    Create(aObject.ClassType);
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TRTTIList.GetNames(const aList: TStrings);
  {
    Adds the names of all the items in the RTTI list to the specified
     string list.
  }
  var
    i: Integer;
  begin
    for i := 0 to Pred(Count) do
      aList.Add(Name[i]);
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TRTTIList.IndexOf(const aName: String): Integer;
  {
    Returns the index of the named item in the list or -1 if no there is
     no item in the list with the specified name.
  }
  begin
    for result := 0 to Pred(Count) do
      if ANSISameText(aName, Name[result]) then
        EXIT;

    result := -1;
  end;


  {@@TRTTIList.Count

    Returns the number of items in the list.
  }


  {@@TRTTIList.Name

    Returns the name of the specified item in the list.
  }





{-- TMethodList -----------------------------------------------------------------------------------}

  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TMethodList.Create(const aClass: TClass);
  {
    Creates a method list containing RTTI for all public/published methods
     in the specified class.
  }
{$ifdef DELPHI2010_OR_LATER}
  var
    i, j: Integer;
    ctx: TRTTIContext;
    methods: TArray<TRTTIMethod>;
  begin
    methods := ctx.GetType(aClass).GetMethods;

    j := 0;
    SetLength(fNames, Length(methods));
    for i := 0 to Pred(Length(methods)) do
      if methods[i].Visibility = mvPublished then
      begin
        fNames[j] := methods[i].Name;
        Inc(j);
      end;

    SetLength(fNames, j);
  end;
{$else}
  var
    i: Integer;
    method: PPublishedMethod;
  begin
    inherited;

    // Set the length of the array of names to accomodate all published
    //  methods in the specified class
    SetLength(fNames, GetPublishedMethodCount(aClass));
    if Count = 0 then
      EXIT;

    // Get the first method name into the 0'th element of the array
    method := GetFirstPublishedMethod(aClass);
    fNames[0] := String(method.Name);

    // Now get any more method names and place them in the array in turn
    for i := 1 to Pred(Count) do
    begin
      method := GetNextPublishedMethod(method);
      fNames[i] := String(method.Name);
    end;
  end;
{$endif}


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TMethodList.Destroy;
  begin
    SetLength(fNames, 0);
    inherited;
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TMethodList.get_Count: Integer;
  begin
    result := Length(fNames);
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TMethodList.get_Name(const aIndex: Integer): String;
  begin
    result := fNames[aIndex];
  end;






{-- TPropertyList ---------------------------------------------------------------------------------}

  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TPropertyList.Create(const aClass: TClass);
  begin
    inherited;

    fCount := GetPropList(aClass.ClassInfo, tkProperties, nil);
    GetMem(fPropList, Count * SizeOf(PPropInfo));

    GetPropList(aClass.ClassInfo, tkProperties, fPropList);
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TPropertyList.Create(const aObject: TObject);
  begin
    inherited;

    fContext := aObject;
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TPropertyList.Destroy;
  begin
    FreeMem(fPropList);
    inherited;
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPropertyList.get_Index(const aIndex: Integer): Integer;
  {@@TPropertyList.Index

    The Index value of the property at the specified index in the list, if
     available.
  }
  begin
    result := fPropList[aIndex].Index;
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPropertyList.get_Kind(const aIndex: Integer): TTypeKind;
  {@@TPropertyList.Kind

    The TTypeKind of the property at the specified index in the list.
  }
  begin
    result := fPropList[aIndex].PropType^.Kind;
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPropertyList.get_Name(const aIndex: Integer): String;
  begin
    result := String(fPropList[aIndex].Name);
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPropertyList.set_AsString(const aIndex: Integer; const aValue: String);
  begin
    SetPropValue(fContext, Name[aIndex], aValue);
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPropertyList.get_AsString(const aIndex: Integer): String;
  begin
    result := GetPropValue(fContext, Name[aIndex], TRUE);
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPropertyList.get_Count: Integer;
  begin
    result := fCount;
  end;


  {-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPropertyList.GetNames(const aList: TStrings;
                                   const aKind: TTypeKind);
  {
    Adds the names of the properties in the list to a specified list of
     strings.

    GetNames in TPropertyList is an overload that is called with a type
     kind filter - only properties of this type kind will be added to the
     string list.

    The GetNames inherited from TRTTIList may still be called (without a
     type kind parameter) to get the names of all properties.
  }
  var
    i: Integer;
  begin
    for i := 0 to Pred(Count) do
      if (Kind[i] = aKind) then
        aList.Add(Name[i]);
  end;





end.
