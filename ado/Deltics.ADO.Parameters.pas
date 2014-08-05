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

{$ifdef debug_Deltics_ADO_Parameters}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Parameters;


interface

  uses
  { vcl: }
    Classes,
    Contnrs,
    TypInfo,
    Variants,
  { deltics: }
    Deltics.Classes,
  { deltics.iADO: }
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.Utils;


  type
    TParameterDirection = (
                           pdInput,
                           pdInputOutput,
                           pdOutput
                          );

    TParameterType = (
                      ptUnknown,
                      ptBoolean,
                      ptDateTime,
                      ptDouble,
                      ptGUID,
                      ptInteger,
                      ptInt64,
                      ptString,
                      ptUnicode,
                      ptBLOB,
                      ptStringBLOB,
                      ptUnicodeBLOB
                     );

  const
    ADOParameterType : array[TParameterType] of Integer = (
                                                           adEmpty,
                                                           adBoolean,
                                                           adDate,
                                                           adDouble,
                                                           adGUID,
                                                           adInteger,
                                                           adBigInt,
                                                           adVarChar,
                                                           adVarWChar,
                                                           adLongVarBinary,
                                                           adLongVarChar,
                                                           adLongVarWChar
                                                          );
  type
    TADOParameter = class(TInterfacedObject)
    private
      fDirection: TParameterDirection;
      fName: String;
      fParamType: TParameterType;
      fValue: Variant;
      fValueSize: Int64;
      function get_AsBoolean: Boolean;
      function get_AsDate: TDateTime;
      function get_AsDateTime: TDateTime;
      function get_AsDouble: Double;
      function get_AsEnum(const aTypeInfo: PTypeInfo): Integer;
      function get_AsGUID: TGUID;
      function get_AsInteger: Integer;
      function get_AsInt64: Int64;
      function get_AsString: String;
      function get_AsWideString: WideString;
      function get_GUIDAsString: String;
      function get_IsNull: Boolean;
      procedure set_AsBoolean(const aValue: Boolean);
      procedure set_AsDate(const aValue: TDateTime);
      procedure set_AsDateTime(const aValue: TDateTime);
      procedure set_AsDouble(const aValue: Double);
      procedure set_AsEnum(const aTypeInfo: PTypeInfo; const aValue: Integer);
      procedure set_AsGUID(const aValue: TGUID);
      procedure set_AsInteger(const aValue: Integer);
      procedure set_AsInt64(const aValue: Int64);
      procedure set_AsString(const aValue: String);
      procedure set_AsWideString(const aValue: WideString);
      procedure set_GUIDAsString(const aValue: String);
      procedure set_IsNull(const aValue: Boolean);
      procedure SetValue(const aValue: Variant; const aType: TParameterType);
      procedure SetValueSize(const aSize: Int64);
    protected
      constructor Create(const aName: String);
      procedure Apply(const aParam: Parameter);
      procedure InitADOParam(const aParam: Parameter);
    public
      procedure LoadFromStream(const aStream: TStream;
                               const aType: TParameterType = ptBLOB);
      procedure SetNull(const aType: TParameterType);
      property AsBoolean: Boolean read get_AsBoolean write set_AsBoolean;
      property AsDate: TDateTime read get_AsDate write set_AsDate;
      property AsDateTime: TDateTime read get_AsDateTime write set_AsDateTime;
      property AsDouble: Double read get_AsDouble write set_AsDouble;
      property AsEnum[const aTypeInfo: PTypeInfo]: Integer read get_AsEnum write set_AsEnum;
      property AsGUID: TGUID read get_AsGUID write set_AsGUID;
      property AsInteger: Integer read get_AsInteger write set_AsInteger;
      property AsInt64: Int64 read get_AsInt64 write set_AsInt64;
      property AsString: String read get_AsString write set_AsString;
      property AsWideString: WideString read get_AsWideString write set_AsWideString;
      property Direction: TParameterDirection read fDirection write fDirection;
      property GUIDAsString: String read get_GUIDAsString write set_GUIDAsString;
      property IsNull: Boolean read get_IsNull write set_IsNull;
      property Name: String read fName;
      property ParamType: TParameterType read fParamType write fParamType;
      property Value: Variant read fValue;
      property ValueSize: Int64 read fValueSize;
    end;


    TADOParameterBinding = record
      Parameter: TADOParameter;
      ADOParam: Parameter;
    end;


    TADOParameters = class(TInterfacedObject)
    private
      fItems: TObjectList;
      fBindings: array of TADOParameterBinding;
      function get_ADOParam(const aIndex: Integer): Parameter;
      function get_ADOParamCount: Integer;
      function get_Count: Integer;
      function get_Item(const aIndex: Integer): TADOParameter;
    protected
      procedure Apply;
      procedure Clear;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(const aName: String): TADOParameter;
      function FindParam(const aName: String; var aParam: TADOParameter): Boolean;
      property ADOParamCount: Integer read get_ADOParamCount;
      property ADOParams[const aIndex: Integer]: Parameter read get_ADOParam;
      property Count: Integer read get_Count;
      property Items[const aIndex: Integer]: TADOParameter read get_Item; default;
    end;



implementation

  uses
  { vcl: }
    SysUtils,
  { deltics: }
    Deltics.SysUtils;


  const
  {$ifdef UNICODE}
    MAX_VARCHAR_LEN = 4000;
  {$else}
    MAX_VARCHAR_LEN = 2000;
  {$endif}

  resourcestring
    rsfParameterNotSet = 'Parameter ''%s'' has not been set';


{ TParameter }

  constructor TADOParameter.Create(const aName: String);
  begin
    inherited Create;

    fDirection  := pdInput;
    fName       := aName;
    fParamType  := ptUnknown;
  end;


  procedure TADOParameter.InitADOParam(const aParam: Parameter);
  const
    PARAMDIRECTION: array[TParameterDirection] of ParameterDirectionEnum = (adParamInput,
                                                                            adParamInputOutput,
                                                                            adParamOutput);
  begin
    if (ParamType = ptUnknown) then
      raise EADOException.CreateFmt(rsfParameterNotSet, [Name]);

    aParam.Direction  := PARAMDIRECTION[Direction];
    aParam.Name       := Name;
    aParam.Type_      := ADOParameterType[ParamType];
  end;


  function TADOParameter.get_AsBoolean: Boolean;
  begin
    result := fValue;
  end;


  function TADOParameter.get_AsDate: TDateTime;
  begin
    result := Trunc(AsDateTime);
  end;


  function TADOParameter.get_AsDateTime: TDateTime;
  begin
    result := fValue;
  end;


  function TADOParameter.get_AsDouble: Double;
  begin
    result := fValue;
  end;


  function TADOParameter.get_AsEnum(const aTypeInfo: PTypeInfo): Integer;
  begin
    result := GetEnumValue(aTypeInfo, AsString);
  end;


  function TADOParameter.get_AsGUID: TGUID;
  begin
    result := StringToGUID(fValue);
  end;


  function TADOParameter.get_AsInteger: Integer;
  begin
    result := fValue;
  end;


  function TADOParameter.get_AsInt64: Int64;
  begin
    result := fValue;
  end;


  function TADOParameter.get_AsString: String;
  begin
    result := fValue;
  end;


  function TADOParameter.get_AsWideString: WideString;
  begin
    result := fValue;
  end;


  function TADOParameter.get_GUIDAsString: String;
  begin
    result := GUIDToString(AsGUID);
  end;


  function TADOParameter.get_IsNull: Boolean;
  begin
    result := VarIsNull(fValue);
  end;


  procedure TADOParameter.set_AsBoolean(const aValue: Boolean);
  begin
    SetValue(aValue, ptBoolean);
  end;


  procedure TADOParameter.set_AsDate(const aValue: TDateTime);
  begin
    SetValue(Trunc(aValue), ptDateTime);
  end;


  procedure TADOParameter.set_AsDateTime(const aValue: TDateTime);
  begin
    // On SQL Server, setting date/time values loses milliseconds, but this can
    //  be avoided by treating them as doubles when setting the value

    //    Usually would be:  SetValue(aValue, ptDateTime);

    // NOTE: This may not work/be required on NON-SQL Server back-ends, but for
    //        now it is only SQL Server that I care about

    //       This also means that the ADO Provider does not correct the date/time
    //        values to reflect the different baseline dates used for Delphi
    //        TDateTime vs SQL Server datetime.
    //
    //           SQL Server day 0 = 1900/01/01
    //           Delphi     day 0 = 1899/30/12
    //
    //       Therefore we need to adjust the value ourselves.  Again, this is
    //        SQL Server specific as far as I know.

    SetValue(aValue - 2, ptDouble);
  end;


  procedure TADOParameter.set_AsDouble(const aValue: Double);
  begin
    SetValue(aValue, ptDouble);
  end;


  procedure TADOParameter.set_AsEnum(const aTypeInfo: PTypeInfo; const aValue: Integer);
  begin
    AsString := GetEnumName(aTypeInfo, aValue);
  end;


  procedure TADOParameter.set_AsGUID(const aValue: TGUID);
  var
    s: String;
  begin
    s := GUIDToString(aValue);
    SetValue(s, ptGUID);
    SetValueSize(Max(Length(s), 1));
  end;


  procedure TADOParameter.set_AsInteger(const aValue: Integer);
  begin
    SetValue(aValue, ptInteger)
  end;


  procedure TADOParameter.set_AsInt64(const aValue: Int64);
  begin
    SetValue(aValue, ptInt64);
  end;


  procedure TADOParameter.set_AsString(const aValue: String);
{$ifdef UNICODE}
  begin
    AsWideString := aValue;
  end;
{$else}
  var
    strm: TStringStream;
  begin
    if (Length(aValue) > MAX_VARCHAR_LEN) then
    begin
      strm := TStringStream.Create(aValue);
      try
        LoadFromStream(strm, ptStringBLOB);
      finally
        strm.Free;
      end;
    end
    else
    begin
      SetValue(aValue, ptString);
      SetValueSize(Length(aValue) + 1);
    end;
  end;
{$endif}


  procedure TADOParameter.set_AsWideString(const aValue: WideString);
  var
    strm: TStringStream;
  begin
    if (Length(aValue) > MAX_VARCHAR_LEN) then
    begin
      strm := TStringStream.Create(aValue);
      try
        LoadFromStream(strm, ptUnicodeBLOB);
      finally
        strm.Free;
      end;
    end
    else
    begin
      SetValue(aValue, ptUnicode);
      SetValueSize(Length(aValue));
    end;
  end;


  procedure TADOParameter.set_GUIDAsString(const aValue: String);
  begin
    AsGUID := StringToGUID(aValue);
  end;


  procedure TADOParameter.set_IsNull(const aValue: Boolean);
  begin
    ASSERT(ParamType <> ptUnknown, 'ParamType unknown - use SetNull');
    fValue := Null;
  end;


  procedure TADOParameter.SetValue(const aValue: Variant;
                                   const aType: TParameterType);
  begin
    fValue     := aValue;
    fParamType := aType;
  end;


  procedure TADOParameter.SetValueSize(const aSize: Int64);
  begin
    fValueSize := aSize;
  end;


  procedure TADOParameter.Apply(const aParam: Parameter);
  begin
    try
      // For string, GUID and BLOB type parameters the parameter value
      //  size must be indicated (set in the appropriate set_As... setter)

      // NOTE: w.r.t setting MAX_VARCHAR_LEN or MaxInt : I don't like this,
      //        but setting the actual value size causes problems when
      //        re-using parameters (the first value size set is applied
      //        to ALL uses of that parameter

      case ParamType of
        ptGUID        : aParam.Size := fValueSize;

        ptString,
        ptUnicode     : aParam.Size := MAX_VARCHAR_LEN - 1;

        ptBLOB,
        ptStringBLOB,
        ptUnicodeBLOB : aParam.Size := MaxInt;
      end;

      aParam.Value := fValue;

    except
      on e: Exception do
      begin
        e.Message := e.Message + #13#13 + 'Applying parameter: ' + aParam.Name;
        raise;
      end;
    end;
  end;


  procedure TADOParameter.LoadFromStream(const aStream: TStream;
                                         const aType: TParameterType);
  var
    ptr: Pointer;
  begin
    fParamType := aType;

    if aStream.Size > 0 then
    begin
      fValue := VarArrayCreate([0, aStream.Size - 1], varByte);
      ptr    := VarArrayLock(fValue);
      try
        aStream.Position := 0;
        aStream.ReadBuffer(ptr^, aStream.Size);
      finally
        VarArrayUnlock(fValue);
      end;
    end
    else
      fValue := Null;

    SetValueSize(aStream.Size);
  end;


  procedure TADOParameter.SetNull(const aType: TParameterType);
  begin
    SetValue(Null, aType);
    SetValueSize(1);
  end;










{ TParameters }

  constructor TADOParameters.Create;
  begin
    inherited Create;
    fItems := TObjectList.Create(TRUE);
  end;


  destructor TADOParameters.Destroy;
  begin
    SetLength(fBindings, 0);
    FreeAndNIL(fItems);
    inherited;
  end;


  function TADOParameters.get_ADOParam(const aIndex: Integer): Parameter;
  begin
    result := fBindings[aIndex].ADOParam;
  end;


  function TADOParameters.get_ADOParamCount: Integer;
  begin
    result := Length(fBindings);
  end;


  function TADOParameters.get_Count: Integer;
  begin
    result := fItems.Count;
  end;


  function TADOParameters.get_Item(const aIndex: Integer): TADOParameter;
  begin
    result := TADOParameter(fItems[aIndex]);
  end;


  function TADOParameters.Add(const aName: String): TADOParameter;
  var
    iBind: Integer;
  begin
    if NOT FindParam(aName, result) then
    begin
      result := TADOParameter.Create(aName);
      fItems.Add(result);
    end;

    iBind := Length(fBindings);
    SetLength(fBindings, iBind + 1);
    with fBindings[iBind] do
    begin
      Parameter := result;
      ADOParam  := NIL;
    end;
  end;


  procedure TADOParameters.Apply;
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(fBindings)) do
    begin
      with fBindings[i] do
      begin
        if NOT Assigned(ADOParam) then
        begin
          ADOParam := CoParameter.Create;
          Parameter.InitADOParam(ADOParam);
        end;

        Parameter.Apply(ADOParam);
      end;
    end;
  end;


  procedure TADOParameters.Clear;
  begin
    fItems.Clear;
    SetLength(fBindings, 0);
  end;


  function TADOParameters.FindParam(const aName: String;
                                    var aParam: TADOParameter): Boolean;
  var
    i: Integer;
  begin
    result := FALSE;
    try
      for i := 0 to Pred(fItems.Count) do
      begin
        aParam := TADOParameter(fItems[i]);
        if ANSISameText(aParam.Name, aName) then
          EXIT;
      end;

      aParam := NIL;
    finally
      result := (aParam <> NIL);
    end;
  end;









end.
