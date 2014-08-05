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

{$ifdef debug_Deltics_ADO_ConnectionString}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.ConnectionString;


interface

  uses
    Classes,
    Deltics.Classes,
    Deltics.JSON;


  type
    TADOConnectionString = class
    private
      fJSON: TJSONArray;
      fOnChange: TNamedNotifyEvent;
      fValues: TStringList;
      function get_AsJSON: TJSONArray;
      function get_AsString: String;
      function get_Name(const aIndex: Integer): String;
      function get_NameCount: Integer;
      function get_Value(const aName: String): String;
      procedure set_AsJSON(const aValue: TJSONArray);
      procedure set_Value(const aName, aValue: String);
    protected
      procedure DoChange(const aName: String);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      property AsJSON: TJSONArray read get_AsJSON write set_AsJSON;
      property AsString: String read get_AsString;
      property NameCount: Integer read get_NameCount;
      property Names[const aIndex: Integer]: String read get_Name;
      property Value[const aName: String]: String read get_Value write set_Value; default;
      property OnChange: TNamedNotifyEvent read fOnChange write fOnChange;
    end;



implementation

  uses
    SysUtils,
    Deltics.SysUtils;


{ TADOConnectionString --------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADOConnectionString.Create;
  begin
    inherited Create;

    fValues := TStringList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TADOConnectionString.Destroy;
  begin
    FreeAndNIL(fValues);
    FreeAndNIL(fJSON);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOConnectionString.DoChange(const aName: String);
  var
    handler: TNamedNotifyEvent;
  begin
    FreeAndNIL(fJSON);

    if NOT Assigned(fOnChange) then
      EXIT;

    handler := fOnChange;
    fOnChange := NIL;
    try
      handler(self, aName);
    finally
      fOnChange := handler;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOConnectionString.get_AsJSON: TJSONArray;
  var
    i: Integer;
  begin
    if NOT Assigned(fJSON) then
    begin
      fJSON := TJSONArray.Create;

      for i := 0 to Pred(NameCount) do
        fJSON.Add(Names[i], Value[Names[i]]);
    end;

    result := fJSON;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOConnectionString.get_AsString: String;
  const
    SEP = '; ';
  var
    values: TStringList;

    procedure Append(const aName, aValue: String); overload;
    var
      idx: Integer;
    begin
      if (aValue <> '') then
        result := result + Format('%s=%s' + SEP, [aName, aValue]);

      idx := values.IndexOfName(aName);
      if (idx <> -1) then
        values.Delete(idx);
    end;

    procedure Append(const aName: String); overload;
    var
      idx: Integer;
    begin
      idx := values.IndexOfName(aName);
      if (idx <> -1) then
        Append(aName, values.Values[aName]);
    end;

  begin
    result := '';
    values := TStringList.Create;
    try
      values.Assign(fValues);

      // Some parameters need to be listed first, if present
      Append('Provider');
      Append('Driver');
      Append('Data Source');

      while (values.Count > 0) do
        Append(values.Names[0], values.Values[values.Names[0]]);

    finally
      values.Free;
    end;

    if Length(result) > 0 then
      SetLength(result, Length(result) - Length(SEP)); // remove trailing SEP
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOConnectionString.get_Name(const aIndex: Integer): String;
  begin
    result := fValues.Names[aIndex];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOConnectionString.get_NameCount: Integer;
  begin
    result := fValues.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOConnectionString.get_Value(const aName: String): String;
  begin
    result := fValues.Values[aName];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOConnectionString.set_AsJSON(const aValue: TJSONArray);
  var
    i: Integer;
  begin
    Clear;

    for i := 0 to Pred(aValue.Count) do
      Value[aValue[i].Name] := aValue[i].AsString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOConnectionString.set_Value(const aName, aValue: String);
  var
    value: String;
  begin
    value := Trim(aValue);

    if self.Value[aName] = value then
      EXIT;

    fValues.Values[aName] := value;

    if (value = '') then
      fValues.Add(aName + '=');

    DoChange(aName);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOConnectionString.Clear;
  var
    i: Integer;
    names: TStringList;
  begin
    names := NIL;

    if Assigned(fOnChange) then
    begin
      names := TStringList.Create;
      for i := 0 to Pred(fValues.Count) do
        names.Add(fValues.Names[i]);
    end;

    fValues.Clear;

    if Assigned(names) then
    begin
      for i := 0 to Pred(fValues.Count) do
        fOnChange(self, names[i]);

      names.Free;
    end;
  end;




end.
