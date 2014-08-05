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

{$ifdef debug_Deltics_ADO_Fields}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


    unit Deltics.ADO.Fields;


interface

  uses
  { vcl: }
    Classes,
    Contnrs,
    TypInfo,
    Variants,
  { deltics: }
    Deltics.Types,
    Deltics.ADO.TypeLibrary;


  type
    TADOField = class;
    TADOFields = class;

    
    TDataType = (
                 dtUnknown,
                 dtBoolean,
                 dtDateTime,
                 dtDouble,
                 dtExtended,
                 dtGUID,
                 dtInteger,
                 dtInt64,
                 dtSingle,
                 dtString,
                 dtUnicode,
                 dtBLOB,
                 dtUnicodeBLOB
                );

    TValueGetter = function: Variant of object;

    TADOField = class(TInterfacedObject)
    private
      fADOField: Field;
      fNullValue: Variant;
      fGetValue: TValueGetter;

      function get_AsBoolean: Boolean;
      function get_AsDate: TDateTime;
      function get_AsDateTime: TDateTime;
      function get_AsDouble: Double;
      function get_AsEnum(const aTypeInfo: PTypeInfo): Integer;
      function get_AsExtended: Extended;
      function get_AsGUID: TGUID;
      function get_AsInt64: Int64;
      function get_AsInteger: Integer;
      function get_AsSingle: Single;
      function get_AsString: String;
      function get_AsWideString: WideString;
      function get_DataType: TDataType;
      function get_GUIDAsString: String;
      function get_IsNull: Boolean;
      function get_Name: String;
      function get_Size: Integer;
      function get_Value: Variant;
      procedure set_NullValue(const Value: Variant);

      function _get_Value: Variant;
      function _get_NullableValue: Variant;
    protected
      constructor Create(const aField: Field);
    public
      procedure SaveToStream(const aStream: TStream);
      property ADOField: Field read fADOField;
      property DataType: TDataType read get_DataType;
      property AsBoolean: Boolean read get_AsBoolean;
      property AsDate: TDateTime read get_AsDate;
      property AsDateTime: TDateTime read get_AsDateTime;
      property AsDouble: Double read get_AsDouble;
      property AsEnum[const aTypeInfo: PTypeInfo]: Integer read get_AsEnum;
      property AsExtended: Extended read get_AsExtended;
      property AsGUID: TGUID read get_AsGUID;
      property AsInt64: Int64 read get_AsInt64;
      property AsInteger: Integer read get_AsInteger;
      property AsSingle: Single read get_AsSingle;
      property AsString: String read get_AsString;
      property AsWideString: WideString read get_AsWideString;
      property GUIDAsString: String read get_GUIDAsString;
      property IsNull: Boolean read get_IsNull;
      property Name: String read get_Name;
      property NullValue: Variant read fNullValue write set_NullValue;
      property Size: Integer read get_Size;
      property Value: Variant read get_Value;
    end;


    TADOFields = class(TInterfacedObject)
    private
      fItems: TObjectList;
      function get_AsString: String;
      function get_Count: Integer;
      function get_Items(const aIndex: Integer): TADOField;
    protected
      procedure Add(const aField: Field);
      procedure Clear;
    public
      constructor Create;
      destructor Destroy; override;
      function FindField(const aName: String; var aField: TADOField): Boolean;
      property AsString: String read get_AsString;
      property Count: Integer read get_Count;
      property Items[const aIndex: Integer]: TADOField read get_Items; default;
    end;



implementation

  uses
  { VCL: }
    ActiveX,
    ComObj,
    SysUtils,
  { deltics}
    Deltics.ADO.Utils;


{ TField }

  constructor TADOField.Create(const aField: Field);
  begin
    inherited Create;
    fADOField  := aField;
    fNullValue := Null;
    fGetValue  := _get_Value;
  end;


  function TADOField.get_AsBoolean: Boolean;
  begin
    result := Value;
  end;


  function TADOField.get_AsDate: TDateTime;
  begin
    result := Trunc(AsDateTime);
  end;


  function TADOField.get_AsDateTime: TDateTime;
  begin
    result := Value;
  end;


  function TADOField.get_AsDouble: Double;
  begin
    result := Value;
  end;


  function TADOField.get_AsEnum(const aTypeInfo: PTypeInfo): Integer;
  begin
    result := GetEnumValue(aTypeInfo, AsString);
  end;


  function TADOField.get_AsExtended: Extended;
  begin
    result := Value;
  end;


  function TADOField.get_AsGUID: TGUID;
  var
    s: String;
  begin
    s := Value;
    result := StringToGUID(s);
  end;


  function TADOField.get_AsInt64: Int64;
  begin
    result := Value;
  end;


  function TADOField.get_AsInteger: Integer;
  begin
    result := Value;
  end;


  function TADOField.get_AsSingle: Single;
  begin
    result := Value;
  end;


  function TADOField.get_AsString: String;
  begin
    if IsNull then
      result := ''
    else
      result := Value;
  end;


  function TADOField.get_AsWideString: WideString;
  begin
    if IsNull then
      result := ''
    else
      result := Value;
  end;


  function TADOField.get_DataType: TDataType;
  begin
    case ADOField.Type_ of
      adChar,
      adVarChar       : result := dtString;

      adDate,
      adDBDate,
      adDBTime,
      adDBTimeStamp   : result := dtDateTime;

      adBoolean       : result := dtBoolean;

      adSmallInt,
      adTinyInt,
      adInteger       : result := dtInteger;

      adBigInt        : result := dtInt64;

      adGUID          : result := dtGUID;

      adWChar,
      adVarWChar      : result := dtUnicode;

      adNumeric,
      adDecimal       : result := dtExtended;

      adDouble        : result := dtDouble;

      adSingle        : result := dtSingle;

      adLongVarChar,
      adLongVarBinary : result := dtBLOB;

      adLongVarWChar  : result := dtUnicodeBLOB;
    else
      result := dtUnknown;
//      result := TDataType(ADOField.Type_);
    end;
  end;


  function TADOField.get_GUIDAsString: String;
  begin
    result := GUIDToString(AsGUID);
  end;


  function TADOField.get_IsNull: Boolean;
  begin
    result := VarIsNull(Value);
  end;


  function TADOField.get_Name: String;
  begin
    result := fADOField.Name;
  end;


  function TADOField.get_Size: Integer;
  begin
    result := ADOField.DefinedSize;
  end;


  function TADOField.get_Value: Variant;
  begin
    result := fGetValue;
  end;


  procedure TADOField.SaveToStream(const aStream: TStream);
  var
    ptr: Pointer;
    val: Variant;
    size: Integer;
  begin
    val   := fADOField.Value;
    size  := (VarArrayHighBound(val, 1) - VarArrayLowBound(val, 1)) + 1;

    if size = 0 then
      EXIT;

    ptr := VarArrayLock(val);
    try
      aStream.WriteBuffer(ptr^, size);
    finally
      VarArrayUnlock(val);
    end;
  end;


  procedure TADOField.set_NullValue(const Value: Variant);
  begin
    fNullValue := Value;
    case VarIsNull(fNullValue) of
      TRUE  : fGetValue := _get_Value;
      FALSE : fGetValue := _get_NullableValue;
    end;
  end;


  function TADOField._get_NullableValue: Variant;
  begin
    result := _get_Value;

    if VarIsNull(result) then
      result := NullValue;
  end;


  function TADOField._get_Value: Variant;
  begin
    try
      result := fADOField.Value;

    except
      on e: EOLEException do
      begin
        case ResultCode(e.ErrorCode) of
          adErrNoCurrentRecord: raise EADONoCurrentRecord.Create(e, '');
        else
          raise;
        end;
      end;
    end;
  end;




{ TFields }

  constructor TADOFields.Create;
  begin
    inherited Create;
    fItems := TObjectList.Create(TRUE);
  end;


  destructor TADOFields.Destroy;
  begin
    FreeAndNIL(fItems);
    inherited;
  end;


  function TADOFields.get_AsString: String;
  const
    BOOLSTR: array[FALSE..TRUE] of String = ('FALSE', 'TRUE');
  var
    i: Integer;
  begin
    result := '';

    for i := 0 to Pred(Count) do
    begin
      if Items[i].IsNull then
        result := result + 'NULL, '
      else case Items[i].DataType of
        dtBoolean : result := result + BOOLSTR[Items[i].AsBoolean] + ', ';
        dtString  : result := result + '"' + Items[i].AsString + '", ';
      else
        result := result + Items[i].AsString + ', ';
      end;
    end;

    if Length(result) > 0 then
      SetLength(result, Length(result) - 2);
  end;


  function TADOFields.get_Count: Integer;
  begin
    result := fItems.Count;
  end;


  function TADOFields.get_Items(const aIndex: Integer): TADOField;
  begin
    result := TADOField(fItems[aIndex]);
  end;


  procedure TADOFields.Add(const aField: Field);
  begin
    fItems.Add(TADOField.Create(aField));
  end;


  procedure TADOFields.Clear;
  begin
    fItems.Clear;
  end;


  function TADOFields.FindField(const aName: String; var aField: TADOField): Boolean;
  var
    i: Integer;
  begin
    result := FALSE;
    try
      for i := 0 to Pred(fItems.Count) do
      begin
        aField := TADOField(fItems[i]);
        if ANSISameText(aField.Name, aName) then
          EXIT;
      end;

      aField := NIL;
    finally
      result := Assigned(aField);
    end;
  end;






end.
