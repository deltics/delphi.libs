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

{$ifdef debug_Deltics_ADO_MetaData}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.MetaData;


interface

  uses
  { vcl: }
    Contnrs,
  { deltics: }
    Deltics.ADO.Fields;


  type
    TDBColumn = class;
    TDBIndex = class;
    TDBTable = class;


    TDBObjectState = (
                      osExisting,
                      osNew,
                      osModified,
                      osDropped
                     );



    TDBObject = class
    private
      fID: Integer;
      fName: String;
      fState: TDBObjectState;
    protected

    public
      constructor Create(const aID: Integer;
                         const aName: String);
      constructor CreateNew(const aName: String);
      property ID: Integer read fID write fID;
      property Name: String read fName write fName;
      property State: TDBObjectState read fState write fState;
    end;



    TDBColumn = class(TDBObject)
    private
      fDataType: TDataType;
      fLength: Integer;
      fNullable: Boolean;
      fPrecision: Integer;
      fScale: Integer;
      fDefaultExpr: String;
      procedure set_DefaultExpr(const aValue: String);
      procedure set_Length(const aValue: Integer);
      procedure set_Nullable(const aValue: Boolean);
    public
      constructor Create(const aID: Integer;
                         const aName: String;
                         const aDataType: TDataType;
                         const aLength: Integer;
                         const aNullable: Boolean;
                         const aPrecision: Integer = 0;
                         const aScale: Integer = 0;
                         const aDefaultExpr: String = '');
      constructor CreateNew(const aName: String;
                            const aDataType: TDataType;
                            const aLength: Integer;
                            const aNullable: Boolean;
                            const aPrecision: Integer = 0;
                            const aScale: Integer = 0;
                            const aDefaultExpr: String = '');
      function DDL: String;
      property DataType: TDataType read fDataType write fDataType;
      property DefaultExpr: String read fDefaultExpr write set_DefaultExpr;
      property Length: Integer read fLength write set_Length;
      property Nullable: Boolean read fNullable write set_Nullable;
      property Precision: Integer read fPrecision write fPrecision;
      property Scale: Integer read fScale write fScale;
    end;



    TDBIndex = class(TDBObject)
    private
      fClustered: Boolean;
      fColumns: TObjectList;
      fIsPrimaryKey: Boolean;
      fIsUnique: Boolean;
      fTable: TDBTable;
      function get_Column(const aIndex: Integer): TDBColumn;
      function get_ColumnCount: Integer;
      function get_ColumnNames: String;
      procedure set_Clustered(const aValue: Boolean);
      procedure set_IsPrimaryKey(const aValue: Boolean);
      procedure set_IsUnique(const aValue: Boolean);
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;

      procedure Add(const aColumn: TDBColumn); overload;
      procedure Add(const aColumnName: String); overload;
      function Contains(const aColumnName: String): Boolean;
      procedure Drop(const aColumnName: String); overload;
      procedure Drop(const aIndex: Integer); overload;

      property Clustered: Boolean read fClustered write set_Clustered;
      property Column[const aIndex: Integer]: TDBColumn read get_Column;
      property ColumnCount: Integer read get_ColumnCount;
      property ColumnNames: String read get_ColumnNames;
      property IsPrimaryKey: Boolean read fIsPrimaryKey write set_IsPrimaryKey;
      property IsUnique: Boolean read fIsUnique write set_IsUnique;
    end;



    TDBTable = class(TDBObject)
    private
      fColumns: TObjectList;
      fIndexes: TObjectList;
      fPrimaryKey: TDBIndex;
      fPrimaryKeyState: TDBObjectState;
      function get_ColumnCount: Integer;
      function get_Column(const aIndex: Integer): TDBColumn;
      function get_Index(const aIndex: Integer): TDBIndex;
      function get_IndexCount: Integer;
      function get_Modified: Boolean;
      procedure set_PrimaryKey(const aValue: TDBIndex);
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;

      procedure Add(const aColumn: TDBColumn); overload;
      procedure Add(const aIndex: TDBIndex); overload;
      function ColumnByName(const aName: String): TDBColumn;
      procedure DropColumn(const aName: String);
      function FindIndex(const aColumns: array of String): TDBIndex;
      function IndexByName(const aName: String): TDBIndex;
      function NextIndexID(const aPrefix: String): Integer;

      property Column[const aIndex: Integer]: TDBColumn read get_Column;
      property ColumnCount: Integer read get_ColumnCount;
      property Index[const aIndex: Integer]: TDBIndex read get_Index;
      property IndexCount: Integer read get_IndexCount;
      property Modified: Boolean read get_Modified;
      property PrimaryKey: TDBIndex read fPrimaryKey write set_PrimaryKey;
      property PrimaryKeyState: TDBObjectState read fPrimaryKeyState write fPrimaryKeyState;
    end;





implementation

  uses
    Classes,
    SysUtils;




{ TDBObject -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDBObject.Create(const aID: Integer;
                               const aName: String);
  begin
    inherited Create;

    fID     := aID;
    fName   := aName;
    fState  := osExisting;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDBObject.CreateNew(const aName: String);
  begin
    inherited Create;

    fID     := 0;
    fName   := aName;
    fState  := osNew;
  end;








{ TDBTable --------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDBTable.AfterConstruction;
  begin
    inherited;

    fColumns  := TObjectList.Create(TRUE);
    fIndexes  := TObjectList.Create(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDBTable.BeforeDestruction;
  begin
    inherited;

    FreeAndNIL(fIndexes);
    FreeAndNIL(fColumns);

    fPrimaryKey := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.get_Column(const aIndex: Integer): TDBColumn;
  begin
    result := TDBColumn(fColumns[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.get_ColumnCount: Integer;
  begin
    result := fColumns.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.get_Index(const aIndex: Integer): TDBIndex;
  begin
    result := TDBIndex(fIndexes[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.get_IndexCount: Integer;
  begin
    result := fIndexes.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.get_Modified: Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Pred(ColumnCount) do
    begin
      result := (Column[i].State <> osExisting);
      if result then
        EXIT;
    end;

    result := (fPrimaryKeyState <> osExisting)
                or (Assigned(fPrimaryKey) and (fPrimaryKey.State <> osExisting));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDBTable.set_PrimaryKey(const aValue: TDBIndex);
  begin
    if (fPrimaryKey = aValue) then
      EXIT;

    if NOT Assigned(fPrimaryKey) and Assigned(aValue) then
      fPrimaryKeyState := osNew
    else if Assigned(fPrimaryKey) and NOT Assigned(aValue) then
    begin
      if fPrimaryKeyState <> osNew then
        fPrimaryKeyState := osDropped;
    end
    else
      fPrimaryKeyState := osModified;

    fPrimaryKey := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDBTable.Add(const aColumn: TDBColumn);
  begin
    fColumns.Add(aColumn);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDBTable.Add(const aIndex: TDBIndex);
  begin
    ASSERT(NOT Assigned(aIndex.fTable));

    fIndexes.Add(aIndex);

    aIndex.fTable := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.ColumnByName(const aName: String): TDBColumn;
  var
    i: Integer;
  begin
    for i := 0 to Pred(ColumnCount) do
      if ANSISameText(Column[i].Name, aName) then
      begin
        result := Column[i];
        EXIT;
      end;

    result := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDBTable.DropColumn(const aName: String);
  var
    col: TDBColumn;
  begin
    col := ColumnByName(aName);
    if NOT Assigned(col) then
      EXIT;

    if (col.State = osNew) then
      fColumns.Remove(col)
    else
      col.State := osDropped;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.FindIndex(const aColumns: array of String): TDBIndex;
  var
    i, j: Integer;
  begin
    result := NIL;

    for i := 0 to Pred(IndexCount) do
    begin
      if (Index[i].ColumnCount <> Length(aColumns)) then
        CONTINUE;

      result := Index[i];
      for j := 0 to Pred(Length(aColumns)) do
        if NOT result.Contains(aColumns[j]) then
        begin
          result := NIL;
          BREAK;
        end;

      if Assigned(result) then
        BREAK;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.IndexByName(const aName: String): TDBIndex;
  var
    i: Integer;
  begin
    for i := 0 to Pred(IndexCount) do
      if ANSISameText(Index[i].Name, aName) then
      begin
        result := Index[i];
        EXIT;
      end;

    result := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBTable.NextIndexID(const aPrefix: String): Integer;
  begin
    result := 1;
    while IndexByName(aPrefix + Name + IntToStr(result)) <> NIL do
      Inc(result);
  end;
















{ TDBColumn -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDBColumn.Create(const aID: Integer;
                               const aName: String;
                               const aDataType: TDataType;
                               const aLength: Integer;
                               const aNullable: Boolean;
                               const aPrecision: Integer;
                               const aScale: Integer;
                               const aDefaultExpr: String);
  begin
    inherited Create(aID, aName);

    fDataType     := aDataType;
    fLength       := aLength;
    fNullable     := aNullable;
    fPrecision    := aPrecision;
    fScale        := aScale;

    // Unicode (UTF16) = 2 bytes per char!)
    if (aDataType = dtUnicode) then
      fLength := fLength div 2;

    fDefaultExpr  := aDefaultExpr;

    while (System.Length(fDefaultExpr) > 0)
     and (fDefaultExpr[1] = '(')
     and (fDefaultExpr[System.Length(fDefaultExpr)] = ')') do
    begin
      Delete(fDefaultExpr, 1, 1);
      SetLength(fDefaultExpr, System.Length(fDefaultExpr) - 1);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDBColumn.CreateNew(const aName: String;
                                  const aDataType: TDataType;
                                  const aLength: Integer;
                                  const aNullable: Boolean;
                                  const aPrecision: Integer;
                                  const aScale: Integer;
                                  const aDefaultExpr: String);
  begin
    inherited CreateNew(aName);

    fDataType   := aDataType;
    fLength     := aLength;
    fNullable   := aNullable;
    fPrecision  := aPrecision;
    fScale      := aScale;

    fDefaultExpr  := aDefaultExpr;

    while (System.Length(fDefaultExpr) > 0)
     and (fDefaultExpr[1] = '(')
     and (fDefaultExpr[System.Length(fDefaultExpr)] = ')') do
    begin
      Delete(fDefaultExpr, 1, 1);
      SetLength(fDefaultExpr, System.Length(fDefaultExpr) - 1);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDBColumn.DDL: String;
  begin
    result := '[' + Name + '] ';

    case DataType of
      dtBoolean     : result := result + 'bit';
      dtBLOB        : result := result + 'varbinary';
      dtDateTime    : result := result + 'datetime';
      dtDouble      : result := result + 'float(53)';
      dtGUID        : result := result + 'uniqueidentifier';
      dtInteger     : result := result + 'int';
      dtInt64       : result := result + 'bigint';
      dtString      : result := result + 'varchar(' + IntToStr(Length) + ')';
      dtUnicode     : result := result + 'nvarchar(' + IntToStr(Length) + ')';
      dtUnicodeBLOB : result := result + 'ntext';
    end;

    if NOT Nullable then
      result := result + ' not null';

    if (DefaultExpr <> '') then
      result := result + ' default ' + DefaultExpr;
  end;


  procedure TDBColumn.set_DefaultExpr(const aValue: String);
  begin
    if (DefaultExpr = aValue) then
      EXIT;

    fDefaultExpr := aValue;

    if State = osExisting then
      State := osModified;
  end;


  procedure TDBColumn.set_Length(const aValue: Integer);
  begin
    if (Length = aValue) then
      EXIT;

    fLength := aValue;

    if State = osExisting then
      State := osModified;
  end;


  procedure TDBColumn.set_Nullable(const aValue: Boolean);
  begin
    if (Nullable = aValue) then
      EXIT;

    fNullable := aValue;

    if State = osExisting then
      State := osModified;
  end;







{ TDBIndex }

  procedure TDBIndex.AfterConstruction;
  begin
    inherited;

    fColumns := TObjectList.Create(FALSE);
  end;


  procedure TDBIndex.BeforeDestruction;
  begin
    inherited;

    FreeAndNIL(fColumns);
  end;


  function TDBIndex.get_Column(const aIndex: Integer): TDBColumn;
  begin
    result := TDBColumn(fColumns[aIndex]);
  end;


  function TDBIndex.get_ColumnCount: Integer;
  begin
    result := fColumns.Count;
  end;


  function TDBIndex.get_ColumnNames: String;
  var
    i: Integer;
  begin
    result := '';

    if (ColumnCount = 0) then
      EXIT;

    for i := 0 to PreD(ColumnCount) do
      result := result + Column[i].Name + ',';

    SetLength(result, Length(result) - 1);
  end;


  procedure TDBIndex.set_Clustered(const aValue: Boolean);
  begin
    fClustered := aValue;
  end;


  procedure TDBIndex.set_IsPrimaryKey(const aValue: Boolean);
  begin
    if IsUnique = aValue then
      EXIT;

    fIsPrimaryKey := aValue;

    if State = osExisting then
      State := osModified;
  end;


  procedure TDBIndex.set_IsUnique(const aValue: Boolean);
  begin
    if IsUnique = aValue then
      EXIT;

    fIsUnique := aValue;

    if State = osExisting then
      State := osModified;
  end;


  procedure TDBIndex.Add(const aColumn: TDBColumn);
  begin
    ASSERT(Assigned(aColumn) and (fTable.fColumns.IndexOf(aColumn) <> -1));

    fColumns.Add(aColumn);
  end;


  procedure TDBIndex.Add(const aColumnName: String);
  var
    col: TDBColumn;
  begin
    col := fTable.ColumnByName(aColumnName);

    ASSERT(Assigned(col));

    fColumns.Add(col);

    if State = osExisting then
      State := osModified;
  end;


  function TDBIndex.Contains(const aColumnName: String): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Pred(ColumnCount) do
    begin
      result := ANSISameText(Column[i].Name, aColumnName);
      if result then
        EXIT;
    end;

    result := FALSE;
  end;


  procedure TDBIndex.Drop(const aColumnName: String);
  var
    col: TDBColumn;
  begin
    col := fTable.ColumnByName(aColumnName);

    if fColumns.IndexOf(col) = -1 then
      EXIT;

    fColumns.Remove(col);

    if State = osExisting then
      fState := osModified;
  end;


  procedure TDBIndex.Drop(const aIndex: Integer);
  begin
    Drop(Column[aIndex].Name);
  end;









end.
