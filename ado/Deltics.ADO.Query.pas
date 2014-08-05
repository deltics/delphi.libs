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

{$ifdef debug_Deltics_ADO_Query}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Query;


interface

  uses
  { deltics: }
    Deltics.Classes,
    Deltics.JSON,
    Deltics.ADO.Command,
    Deltics.ADO.Connection,
    Deltics.ADO.Fields,
    Deltics.ADO.RecordSet,
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.Utils;


  type
    TADOQuery = class(TADOCustomCommand)
    private
      fIsBidirectional: Boolean;
      fJSON: TJSONObject;
      fRecordSet: TADORecordSet;
      procedure NeedADORecordset;
    protected
      function get_BOF: Boolean;
      function get_EOF: Boolean;
      function get_Fields: TADOFields;
      function get_Filter: String;
      function get_IsEmpty: Boolean;
      function get_IsOpen: Boolean;
      function get_JSON: TJSONObject;
      procedure set_Filter(const aValue: String);
    protected
      function DoExecute(var aAffected: OleVariant): RecordSet; override;
      procedure DoSQLChanged; override;
    public
      constructor Create(const aConnection: TADOConnection); override;
      destructor Destroy; override;
      procedure Close;
      function FieldByName(const aName: String): TADOField;
      procedure First;
      procedure Last;
      procedure Next;
      procedure Open;
      procedure Previous;
      property BOF: Boolean read get_BOF;
      property EOF: Boolean read get_EOF;
      property Fields: TADOFields read get_Fields;
      property Filter: String read get_Filter write set_Filter;
      property IsBidirectional: Boolean read fIsBidirectional write fIsBidirectional;
      property IsEmpty: Boolean read get_IsEmpty;
      property IsOpen: Boolean read get_IsOpen;
      property JSON: TJSONObject read get_JSON;
    end;



implementation

  uses
  { deltics: }
    Deltics.SysUtils;


  type
    TConnectionHelper = class(TADOConnection);


{ TADOQuery }

  constructor TADOQuery.Create(const aConnection: TADOConnection);
  begin
    inherited Create(aConnection);
    fRecordSet := TADORecordSet.Create(NIL);
  end;


  destructor TADOQuery.Destroy;
  begin
    FreeAndNIL(fRecordSet);
    FreeAndNIL(fJSON);

    inherited;
  end;


  function TADOQuery.get_BOF: Boolean;
  begin
    NeedADORecordset;
    result := fRecordset.ADORecordset.BOF;
  end;


  function TADOQuery.get_EOF: Boolean;
  begin
    NeedADORecordset;
    result := fRecordset.ADORecordset.EOF;
  end;


  function TADOQuery.get_Fields: TADOFields;
  begin
    result := fRecordSet.Fields;
  end;


  function TADOQuery.get_IsEmpty: Boolean;
  begin
    NeedADORecordset;
    result := (fRecordset.ADORecordset.BOF and fRecordset.ADORecordset.EOF);
  end;


  function TADOQuery.get_IsOpen: Boolean;
  begin
    result := Assigned(fRecordset.ADORecordset) and (fRecordset.ADORecordset.State = adStateOpen);
  end;


  function TADOQuery.get_JSON: TJSONObject;
  var
    i: Integer;
    fld: TADOField;
  begin
    if NOT Assigned(fJSON) then
      fJSON := TJSONObject.Create
    else
      fJSON.Clear;

    for i := 0 to Pred(Fields.Count) do
    begin
      fld := Fields[i];
      if fld.IsNull then
        fJSON.AddNull(fld.Name)
      else
        case fld.DataType of
          dtInt64     : fJSON.Add(fld.Name, fld.AsInt64);
          dtInteger   : fJSON.Add(fld.Name, fld.AsInteger);
          dtGUID      : fJSON.Add(fld.Name, fld.AsGUID);
          dtDateTime  : fJSON.Add(fld.Name, fld.AsDateTime);
        else
          fJSON.Add(fld.Name, fld.AsString);
        end;
    end;

    result := fJSON;
  end;


  procedure TADOQuery.NeedADORecordset;
  begin
    if Assigned(fRecordset.ADORecordset) then
      EXIT;

    if State[csComplete] then
      raise EADOException.Create('Unexpected error - no recordset for query', SQL.Text)
    else
      raise EADOException.Create('Query has not been executed', SQL.Text);
  end;


  function TADOQuery.DoExecute(var aAffected: OleVariant): RecordSet;
  const
    CURSOR_LOCATION : array[FALSE..TRUE] of Integer = (adUseServer, adUseClient);
    CURSOR_TYPE     : array[FALSE..TRUE] of Integer = (adOpenForwardOnly, adOpenStatic);
  var
    rs: Variant;
  begin
    result := CoRecordset.Create;
    result.CursorLocation := CURSOR_LOCATION[IsBidirectional];
    result.CursorType     := CURSOR_TYPE[IsBidirectional];
    result._Set_Source(ADOCommand);

    rs := result;
    rs.Open;

    fRecordSet.ADORecordSet := result;
  end;


  procedure TADOQuery.DoSQLChanged;
  begin
    inherited;
    Close;
  end;


  function TADOQuery.FieldByName(const aName: String): TADOField;
  begin
    if NOT fRecordSet.Fields.FindField(aName, result) then
      raise EADOException.CreateFmt('No such field (%s) in SQL', [aName], SQL.Text);
  end;


  procedure TADOQuery.First;
  begin
    fRecordset.ADORecordset.MoveFirst;
  end;


  procedure TADOQuery.Last;
  begin
    fRecordset.ADORecordset.MoveLast;
  end;


  procedure TADOQuery.Next;
  begin
    fRecordset.ADORecordset.MoveNext;
  end;


  procedure TADOQuery.Previous;
  begin
    fRecordset.ADORecordset.MovePrevious;
  end;


  procedure TADOQuery.Close;
  begin
    if NOT Assigned(fRecordset.ADORecordset) then
      EXIT;

    if (fRecordset.ADORecordSet.State = adStateOpen) then
      fRecordset.ADORecordSet.Close;

    fRecordset.ADORecordSet := NIL;
  end;


  procedure TADOQuery.Open;
  begin
    Execute;
  end;


  function TADOQuery.get_Filter: String;
  begin
    result := fRecordSet.ADORecordset.Filter;
  end;


  procedure TADOQuery.set_Filter(const aValue: String);
  begin
    fRecordSet.ADORecordset.Filter := aValue;
  end;

end.

