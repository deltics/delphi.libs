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

{$ifdef debug_Deltics_ADO_Recordset}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Recordset;


interface

  uses
  { deltics: }
    Deltics.Classes,
    Deltics.ADO.Fields,
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.Utils;


  type
    TADORecordset = class(TInterfacedObject)
    private
      fADORecordSet: Recordset;
      fFields: TADOFields;
      function get_BOF: Boolean;
      function get_EOF: Boolean;
      function get_IsEmpty: Boolean;
      procedure set_RecordSet(const aValue: Recordset);
    public
      constructor Create(const aRecordSet: RecordSet);
      destructor Destroy; override;
      procedure Next;
      procedure Previous;
      property ADORecordSet: Recordset read fADORecordSet write set_Recordset;
      property BOF: Boolean read get_BOF;
      property EOF: Boolean read get_EOF;
      property Fields: TADOFields read fFields;
      property IsEmpty: Boolean read get_IsEmpty;
    end;


implementation

  uses
  { deltics: }
    Deltics.SysUtils;
    

  type
    TFieldsHelper = class(TADOFields);


{ TADORecordset }

  constructor TADORecordset.Create(const aRecordSet: RecordSet);
  begin
    inherited Create;

    fFields := TADOFields.Create;
    ADORecordSet := aRecordSet;
  end;


  destructor TADORecordset.Destroy;
  begin
    FreeAndNIL(fFields);
    inherited;
  end;


  function TADORecordset.get_BOF: Boolean;
  begin
    result := fADORecordSet.BOF;
  end;


  function TADORecordset.get_EOF: Boolean;
  begin
    result := fADORecordSet.EOF;
  end;


  function TADORecordset.get_IsEmpty: Boolean;
  begin
    result := (fADORecordSet.BOF and fADORecordSet.EOF);
  end;


  procedure TADORecordset.Next;
  begin
    fADORecordset.MoveNext;
  end;


  procedure TADORecordset.Previous;
  begin
    fADORecordset.MovePrevious;
  end;


  procedure TADORecordset.set_Recordset(const aValue: Recordset);
  var
    i: Integer;
  begin
    if (ADORecordset = aValue) then
      EXIT;

    fADORecordset := aValue;

    TFieldsHelper(fFields).Clear;

    if Assigned(fADORecordset) then
      for i := 0 to Pred(fADORecordset.Fields.Count) do
        TFieldsHelper(fFields).Add(fADORecordset.Fields.Item[i]);
  end;



end.
