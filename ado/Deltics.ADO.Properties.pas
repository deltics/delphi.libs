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

{$i Deltics.ADO.inc}

{$ifdef debug_Deltics_ADO_Properties}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Properties;


interface

  uses
  { vcl: }
    Classes,
    Contnrs,
  { deltics: }
    Deltics.JSON,
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.Utils;


  type
    TADOProperty = class
    private
      fJSON: TJSONObject;
      fADOType: TADOType;
      fName: String;
      fProperty: Property_;
      fReadOnly: Boolean;
      fRequired: Boolean;
      fValue: Variant;
      function get_AsJSON: TJSONObject;
      procedure set_AsJSON(const aValue: TJSONObject);
    protected
      constructor Create(const aJSON: TJSONObject); overload;
    public
      constructor Create(const aProperty: Property_); overload;
      destructor Destroy; override;
      property AsJSON: TJSONObject read get_AsJSON write set_AsJSON;
      property Name: String read fName;
      property ReadOnly: Boolean read fReadOnly;
      property Required: Boolean read fRequired;
      property ADOType: TADOType read fADOType;
    end;


    TADOPropertyList = class
    private
      fJSON: TJSONArray;
      fItems: TObjectList;
      function get_AsJSON: TJSONArray;
      function get_Count: Integer;
      function get_Item(const aName: String): TADOProperty;
      function get_ItemByIndex(const aIndex: Integer): TADOProperty;
      procedure set_AsJSON(const aValue: TJSONArray);
    public
      constructor Create(const aJSON: TJSONArray); overload;
      constructor Create(const aProperties: Properties); overload;
      destructor Destroy; override;
      property AsJSON: TJSONArray read get_AsJSON write set_AsJSON;
      property ByIndex[const aIndex: Integer]: TADOProperty read get_ItemByIndex;
      property Count: Integer read get_Count;
      property Items[const aName: String]: TADOProperty read get_Item;
    end;



implementation

  uses
  { vcl: }
    SysUtils,
    Variants;


  const
    PROP_Name       = 'name';
    PROP_ReadOnly   = 'readonly';
    PROP_Required   = 'required';
    PROP_ADOType    = 'adotype';
    PROP_Value      = 'value';
    PROP_ValueType  = 'valuetype';



{ TADOProperty ----------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADOProperty.Create(const aJSON: TJSONObject);
  begin
    inherited Create;

    AsJSON := aJSON;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADOProperty.Create(const aProperty: Property_);
  begin
    inherited Create;

    fProperty := aProperty;

    fADOType    := TADOType(fProperty.type_);
    fName       := fProperty.Name;
    fReadOnly   := ((fProperty.Attributes and adPropRead) <> 0)
                    and ((fProperty.Attributes and adPropWrite) = 0);
    fRequired   := ((fProperty.Attributes and adPropRequired) <> 0);
    fValue      := fProperty.Value;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TADOProperty.Destroy;
  begin
    FreeAndNIL(fJSON);
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProperty.get_AsJSON: TJSONObject;
  begin
    if NOT Assigned(fJSON) then
    begin
      fJSON := TJSONObject.Create;

      fJSON.Add(PROP_ADOType,   Ord(fADOType),  TypeInfo(TADOType));

      fJSON.Add(PROP_Name,      fName);
      fJSON.Add(PROP_ReadOnly,  fReadOnly);
      fJSON.Add(PROP_Required,  fRequired);
      fJSON.Add(PROP_ValueType, VarType(fValue));

      if VarIsNull(fValue) then
        fJSON.AddNull(PROP_Value)
      else
        fJSON.Add(PROP_Value, VarToStr(fValue));
    end;

    result := fJSON;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOProperty.set_AsJSON(const aValue: TJSONObject);
  begin
    FreeAndNIL(fJSON);

    fADOType  := TADOType(aValue[PROP_ADOType].AsEnum[TypeInfo(TADOType)]);

    fName     := aValue[PROP_Name].AsString;
    fReadOnly := aValue[PROP_ReadOnly].AsBoolean;
    fRequired := aValue[PROP_Required].AsBoolean;

    if aValue[PROP_Value].IsNull then
      fValue := Null
    else
      fValue := VarAsType(aValue[PROP_Value].AsString, aValue[PROP_ValueType].AsInteger);
  end;






{ TADOPropertyList ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADOPropertyList.Create(const aProperties: Properties);
  var
    i: Integer;
  begin
    inherited Create;

    fItems := TObjectList.Create(TRUE);

    for i := 0 to Pred(aProperties.Count) do
      fItems.Add(TADOProperty.Create(aProperties.Item[i]));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADOPropertyList.Create(const aJSON: TJSONArray);
  begin
    inherited Create;

    fItems := TObjectLIst.Create(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TADOPropertyList.Destroy;
  begin
    FreeAndNIL(fJSON);
    FreeAndNIL(fItems);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOPropertyList.get_AsJSON: TJSONArray;
  var
    i: Integer;
  begin
    if NOT Assigned(fJSON) then
      fJSON := TJSONArray.Create;

    fJSON.Clear;

    for i := 0 to Pred(Count) do
      fJSON.Add(ByIndex[i].AsJSON);

    result := fJSON;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOPropertyList.get_Count: Integer;
  begin
    result := fItems.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOPropertyList.get_Item(const aName: String): TADOProperty;
  var
    i: Integer;
  begin
    for i := 0 to Pred(Count) do
    begin
      result := ByIndex[i];
      if ANSISameText(result.Name, aName) then
        EXIT;
    end;

    result := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOPropertyList.get_ItemByIndex(const aIndex: Integer): TADOProperty;
  begin
    result := TADOProperty(fItems[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOPropertyList.set_AsJSON(const aValue: TJSONArray);
  var
    i: Integer;
  begin
    fItems.Clear;

    for i := 0 to Pred(aValue.Count) do
      fItems.Add(TADOProperty.Create(aValue[i] as TJSONObject));
  end;






end.
