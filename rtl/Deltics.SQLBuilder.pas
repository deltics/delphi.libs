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

{$i Deltics.RTL.inc}

{$ifdef debug_Deltics_SQLBuilder}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.SQLBuilder;


interface

  uses
  { VCL: }
    Classes;


  type
    TArrayOfString = array of String;

    TSQLIdentType  = (
                      itLiteral,
                      itQuoted,
                      itSQLServer
                     );

    TStatementType = (
                      stUnknown,
                      stSelect,
                      stInsert,
                      stUpdate,
                      stDelete,
                      stExists,
                      stUnion
                     );

    TLogicalOperator = (
                        loAND,
                        loOR
                       );

    TRelationalOperator = (
                           roEquals,
                           roGreaterThan,
                           roLessThan,
                           roIsNull,
                           roNotEqual,
                           roNotLessThan,
                           roNotGreaterThan,
                           roIsNotNull
                          );
    TSortOrder = (
                  soAscending,
                  soDescending
                 );


    TSQLBuilder = class
    private
      fSchema: String;
      fTable: String;
      fTableAlias: String;
      fColumns: TArrayOfString;
      fConditions: TArrayOfString;
      fConditionOps: TArrayOfString;
      fHavingConditions: TArrayOfString;
      fHavingConditionOps: TArrayOfString;
      fIdentType: TSQLIdentType;
      fKeyColumns: TArrayOfString;
      fKeyValues: TArrayOfString;
      fGroupBy: TArrayOfString;
      fGrouped: Boolean;
      fOnChanged: TNotifyEvent;
      fOrderBy: TArrayOfString;
      fOrderByOrder: TArrayOfString;
      fSQL: String;
      fStatementType: TStatementType;
      fUpdateCount: Integer;
      fUnions: TStringList;
      fValues: TArrayOfString;
      function get_ColumnValue(const aColumn: string): String;
      function get_IsUpdating: Boolean;
      function get_SQL: String;
      function get_Union(const aID: String): TSQLBuilder;
      procedure set_ColumnValue(const aColumn: String; const aValue: String);
      procedure set_Grouped(const aValue: Boolean);
      procedure set_IdentType(const aValue: TSQLIdentType);
      procedure set_Schema(const aValue: String);
      procedure set_StatementType(const aValue: TStatementType);
      procedure set_Table(const aValue: String);
      procedure DoAddColumns(const aColumns: array of String);
      procedure DoAddKeyColumns(const aColumns: array of String);
      function getDeleteSQL: String;
      function getExistsSQL: String;
      function getGroupBySQL: String;
      function getHavingSQL: String;
      function getInsertSQL: String;
      function getOrderBySQL: String;
      function getSelectSQL: String;
      function getUnionSQL: String;
      function getUpdateSQL: String;
      function getWhereSQL: String;
      procedure Init(const aStatementType: TStatementType; const aTable: String);
    protected
      procedure DoChanged;
      property IsUpdating: Boolean read get_IsUpdating;
      function IndexOfColumn(const aColumn: string): Integer;
      function Ident(const aName: String): String;
      function QTableName: String;
      function QColumnName(const aIndex: Integer): String;
      function QKeyName(const aIndex: Integer): String;
      function QOrderBy(const aIndex: Integer): String;
    public
      constructor Create;
      destructor Destroy; override;
      class function Aliased(const aColumns: array of String; aAlias: String): TArrayOfString;
      class function Columns(const aColumns: array of String; const aTableAlias: String = ''): String;
      class function Conditions(const aColumns: array of String; const aTableAlias: String = ''; const aOperator: TLogicalOperator = loAND): String;
      procedure Exists(const aTable: String; const aKeys: array of String);
      procedure Delete(const aTable: String); overload;
      procedure Delete(const aTable: String; const aKeys: array of String); overload;
      procedure Insert(const aTable: String); overload;
      procedure Insert(const aTable: String; const aColumns: array of String); overload;
      procedure Select(const aTable: String); overload;
      procedure Select(const aTable: String; const aColumns: array of String); overload;
      procedure Select(const aTable: String; const aColumns, aKeys: array of String); overload;
      procedure Select(const aTables, aColumns: array of String); overload;
      procedure Select(const aTables, aColumns, aKeys: array of String); overload;
      procedure Union;
      procedure Update(const aTable: String); overload;
      procedure Update(const aTable: String; const aColumns: array of String); overload;
      procedure Update(const aTable: String; const aColumns, aKeys: array of String); overload;
      procedure AddColumn(const aColumn: String); overload;
      procedure AddColumn(const aColumn, aLiteral: String; const aIsParam: Boolean = FALSE); overload;
      procedure AddColumn(const aColumn : String; const aLiteral: integer); overload;
      procedure AddColumns(const aColumns: array of String);
      procedure AddCondition(const aColumn: String; const aColumnOp: TRelationalOperator; const aOperator: TLogicalOperator = loAND); overload;
      procedure AddCondition(const aCondition: String; const aOperator: TLogicalOperator = loAND); overload;
      procedure AddConditions(const aConditions: array of String;
                              const aInnerOp: TLogicalOperator = loAND;
                              const aOuterOp: TLogicalOperator = loAND);
      procedure AddColumnConditions(const aColumns: array of String;
                                    const aTableAlias: String = '';
                                    const aInnerOp: TLogicalOperator = loAND;
                                    const aOuterOp: TLogicalOperator = loAND);
      procedure AddGroupBy(const aColumns: array of String);
      procedure AddHavingCondition(const aCondition : String; const aOperator: TLogicalOperator = loAND);
      procedure AddKey(const aColumn: String); overload;
      procedure AddKey(const aColumn, aLiteral: String; const aIsParam: Boolean = FALSE); overload;
      procedure AddKeys(const aColumns: array of String);
      procedure AddOrderBy(const aColumns: array of String; const aOrder: TSortOrder = soAscending);
      procedure AddTable(const aTable : String);
      procedure AddTables(const aTables : array of String);
      function AddUnion(const aID: String): TSQLBuilder;
      procedure AddValue(const aValue: String);
      procedure SetAlias(const aAlias: String);
      procedure Assign(const aSource: TSQLBuilder);
      function Clone: TSQLBuilder;
      procedure BeginUpdate;
      procedure EndUpdate;
      property ColumnValues[const aColumn: String]: String read get_ColumnValue write set_ColumnValue;
      property Grouped: Boolean read fGrouped write set_Grouped;
      property IdentType: TSQLIdentType read fIdentType write set_IdentType;
      property Schema: String read fSchema write set_Schema;
      property SQL: String read get_SQL;
      property StatementType: TStatementType read fStatementType write set_StatementType;
      property Table: String read fTable write set_Table;
      property Unions[const aID: String]: TSQLBuilder read get_Union;

      property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
    end;

  function Distinct(const aValue : String) : string;
  function Negate(const aValue : String) : string;



implementation

  uses
  { VCL: }
    SysUtils;


  const
    OPSQL : array[loAND..loOR] of String = (' and ', ' or ');

  const
    COLON = ':';

    COMMA_JOIN = ', ';
    COMMA_JOIN_LEN = Length(COMMA_JOIN);

    DISTINCT_PREFIX = 'distinct ';
    DISTINCT_PREFIX_LEN = Length(DISTINCT_PREFIX);

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ArrayAdd(var aArray: TArrayOfString;
                     const aValue: String); overload;
  begin
    SetLength(aArray, Length(aArray) + 1);
    aArray[Length(aArray) - 1] := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ArrayAdd(var aArray: TArrayOfString;
                     const aValue: array of String); overload;
  var
    i: Integer;
  begin
    if Length(aValue) = 0 then
      EXIT;

    SetLength(aArray, Length(aArray) + Length(aValue));
    for i := Length(aArray) - Length(aValue) to Length(aArray) - 1 do
      aArray[i] := aValue[i - (Length(aArray) - Length(aValue))];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ArrayAdd(var aArray: TArrayOfString;
                     const aCount: Integer); overload;
  var
    i: Integer;
  begin
    if (aCount <= 0) then
      EXIT;

    SetLength(aArray, Length(aArray) + aCount);
    for i := Length(aArray) - aCount to Length(aArray) - 1 do
      aArray[i] := '';
  end;




{ TSQLBuilder ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TSQLBuilder.Create;
  begin
    inherited;

    fUnions := TStringList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TSQLBuilder.Destroy;
  begin
    // Ensures we clean up any owned resources etc before destroying
    //  any containers etc.  etc. 
    Init(stUnknown, '');

    FreeAndNIL(fUnions);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.get_ColumnValue(const aColumn: String): String;
  {
    Returns the value associated with a specified column. It is an
     error to ask for the value of a column that does not exist
  }
  begin
    result := fValues[IndexOfColumn(aColumn)];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.get_IsUpdating: Boolean;
  begin
    result := (fUpdateCount > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.get_SQL: String;
  {
    Getter for the SQL property - invokes the appropriate method to
     assemble the SQL statement from the various elements as required.
  }
  begin
    if (fSQL = '') then
    begin
      case fStatementType of
        stDelete  : fSQL := getDeleteSQL;
        stExists  : fSQL := getExistsSQL;
        stInsert  : fSQL := getInsertSQL;
        stSelect  : fSQL := getSelectSQL;
        stUnion   : fSQL := getUnionSQL;
        stUpdate  : fSQL := getUpdateSQL;
      else
      {@stb}
        ASSERT(FALSE, 'StatementType is unknown');
      {@ste}
      end;
    end;

    result := fSQL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.get_Union(const aID: String): TSQLBuilder;
  var
    idx: Integer;
  begin
  {@stb}
    ASSERT(StatementType = stUnion, 'Unions property is only valid on a UNION statement');
  {@ste}

    idx    := fUnions.IndexOf(Uppercase(aID));
    result := TSQLBuilder(fUnions.Objects[idx]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.set_ColumnValue(const aColumn: String;
                                        const aValue: String);
  {
    Changes the value associated with a column.  If an entry for the
     column does not exist then one is added.
  }
  var
    idx: Integer;
  begin
    idx := IndexOfColumn(aColumn);
    if (idx <> -1) then
      fValues[idx] := aValue
    else
      AddColumn(aColumn, aValue);

    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.set_Grouped(const aValue: Boolean);
  begin
    if (fGrouped = aValue) then
      EXIT;

  {@stb}
    ASSERT((aValue = FALSE) or (fStatementType = stSelect),
           'Only SELECT statements may be Grouped');
  {@ste}
    fGrouped := aValue;
    DoChanged;
  end;


  procedure TSQLBuilder.set_IdentType(const aValue: TSQLIdentType);
  begin
    fIdentType := aValue;
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.set_Schema(const aValue: String);
  begin
    fSchema := aValue;
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.set_StatementType(const aValue: TStatementType);
  begin
    if (fStatementType = aValue) then
      EXIT;

  {@stb}
    ASSERT((fStatementType in [stInsert, stUpdate, stDelete])
           and (aValue in [stInsert, stUpdate, stDelete]), 'Can only change an INSERT, UPDATE or DELETE to an INSERT, UPDATE or DELETE');
  {@ste}

    fStatementType := aValue;
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.set_Table(const aValue: String);
  begin
    if fTable = aValue then
      EXIT;

    fTable := aValue;
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.DoAddColumns(const aColumns: array of String);
  begin
    ArrayAdd(fColumns, aColumns);
    ArrayAdd(fValues, Length(aColumns));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.DoAddKeyColumns(const aColumns: array of String);
  begin
    ArrayAdd(fKeyColumns, aColumns);
    ArrayAdd(fKeyValues, Length(aColumns));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getDeleteSQL: String;
  var
    swhere: String;
  begin
  {@stb}
    swhere := getWhereSQL;
    result := Trim(Format('delete from %s %s', [QTableName, swhere]));
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getExistsSQL: String;
  var
    swhere: String;
  begin
  {@stb}
    swhere := getWhereSQL;
    result := Trim(Format('select COUNT(1) as COUNT from %s %s', [QTableName, swhere]));
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getGroupBySQL: String;
  {
    Returns a GROUP BY clause formed either automatically by performing
     a simple analysis of the columns list or manually from a list of
     column expressions added using the AddGroupBy() method.

    NOTE: The two mechanisms cannot be mixed.

    Once any one or more column expression is added using AddGroupBy
     then the explicit GROUP BY column list will be deemed definitive
     and must form the complete GROUP BY clause required to satisfy
     the query.

    The functionality should be tested using:

     tests\manual\SQLBuilder\GroupByTest\GroupByTest.dpr
  }
  const
  {@stb}
    ALIAS_SEP = ' ';
    GROUP_BY_PREFIX = 'group by ';
    PARAM_START = '(';
    PARAM_END = ')';
  {@ste}

    function AutoGroupBy: String;
    var
      i: Integer;
      sColumn: String;
      iAliasPos: Integer;
      bIsFunction: Boolean;
    begin
      for i := Low(fColumns) to High(fColumns) do
      begin
        sColumn := fColumns[i];

        // If there are parentheses in the column expression then
        //  we assume that it is some function, e.g. SUM(..) or
        //  COUNT(..) etc and is not included in a GROUP BY clause
        //  as formed by this function.

        bIsFunction := (Pos(PARAM_START, sColumn) <> 0)
                        and (Pos(PARAM_END, sColumn) <> 0);

        if bIsFunction then
          CONTINUE;

        iAliasPos := Pos(ALIAS_SEP, sColumn);
        while (iAliasPos > 0) do
        begin
          // If the column expression has a DISTINCT predicate we remove it,
          //  otherwise we remove any alias, leaving what we assume is the
          //  original column name to the left of the separator

          if SameText(Copy(sColumn, 1, DISTINCT_PREFIX_LEN), DISTINCT_PREFIX) then
            System.Delete(sColumn, 1, DISTINCT_PREFIX_LEN)
          else
            SetLength(sColumn, iAliasPos - 1);

          iAliasPos := Pos(ALIAS_SEP, sColumn);
        end;

        if (sColumn <> '') then
          result := result + sColumn + COMMA_JOIN;
      end;

      if (result <> GROUP_BY_PREFIX) then
        SetLength(result, Length(result) - COMMA_JOIN_LEN);
    end;

    function ExplicitGrouping: String;
    var
      i: Integer;
    begin
      for i := Low(fGroupBy) to High(fGroupBy) do
        result := result + fGroupBy[i];

      if (result <> GROUP_BY_PREFIX) then
        SetLength(result, Length(result) - COMMA_JOIN_LEN);
    end;

  begin
    result := '';

    if NOT Grouped or (Length(fColumns) = 0) then
      EXIT;

    result := GROUP_BY_PREFIX;

    if Length(fGroupBy) = 0 then
      result := result + AutoGroupBy
    else
      result := result + ExplicitGrouping;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getHavingSQL: String;
  var
    i: Integer;
  begin
    result := '';

    if Length(fHavingConditions) = 0 then
      EXIT;

    result := fHavingConditions[0];

    for i := 1 to High(fHavingConditions) do
      result := result + fHavingConditionOps[i] + fHavingConditions[i];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getInsertSQL: String;
  var
    i: Integer;
    scol: String;
    sval: String;
  begin
  {@stb}
    scol := '';
    sval := '';

    for i := 0 to Pred(Length(fColumns)) do
    begin
      scol := scol + QColumnName(i) + COMMA_JOIN;
      if (fValues[i] = '') then
        sval := sval + ':' + fColumns[i] + COMMA_JOIN
      else
        sval := sval + fValues[i] + COMMA_JOIN;
    end;

    for i := 0 to Pred(Length(fKeyColumns)) do
    begin
      scol := scol + QKeyName(i) + COMMA_JOIN;
      if (fKeyValues[i] = '') then
        sval := sval + ':' + fKeyColumns[i] + COMMA_JOIN
      else
        sval := sval + fKeyValues[i] + COMMA_JOIN;
    end;

    SetLength(scol, Length(scol) - COMMA_JOIN_LEN);
    SetLength(sval, Length(sval) - COMMA_JOIN_LEN);

    result := Format('insert into %s (%s) values (%s)', [QTableName, scol, sval]);
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getOrderBySQL: String;
  var
    i: Integer;
  begin
  {@stb}
    result := '';

    if (Length(fOrderBy) = 0) then
      EXIT;

    result := 'order by ';

    for i := 0 to Pred(Length(fOrderBy)) do
      result := result + QOrderBy(i) + ' ' + fOrderByOrder[i] + COMMA_JOIN;

    // Remove the trailing COMMA_JOIN
    SetLength(result, Length(result) - COMMA_JOIN_LEN);
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getSelectSQL: String;
  var
    i: Integer;
    tbl: String;
  begin
  {@stb}
    result := '';

    for i := 0 to Pred(Length(fColumns)) do
    begin
      if (fValues[i] = '') then
        result := result + QColumnName(i) + COMMA_JOIN
      else
        result := result + fValues[i] + COMMA_JOIN;
    end;

    SetLength(result, Length(result) - COMMA_JOIN_LEN);

    tbl := QTableName;
    if (fTableAlias <> '') then
      tbl := tbl + ' ' + Ident(fTableAlias);

    result := Trim(Format(
      'select %s from %s %s %s %s %s',
      [result, tbl, getWhereSQL, getGroupBySQL, getOrderBySQL, getHavingSQL]));
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getUnionSQL: String;
  const
    UNION = #13#13' union '#13#13;
  var
    i: Integer;
  begin
    result := '';

    if (fUnions.Count = 0) then
      EXIT;

    result := TSQLBuilder(fUnions.Objects[0]).SQL;

    for i := 1 to Pred(fUnions.Count) do
      result := result + UNION + TSQLBuilder(fUnions.Objects[i]).SQL;

    result := result + getOrderBySQL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getUpdateSQL: String;
  var
    i: Integer;
    sUpdate: String;
    sWhere: String;
  begin
  {@stb}
    sUpdate := '';
    sWhere  := '';

    for i := 0 to Pred(Length(fColumns)) do
    begin
      if (fValues[i] = '') then
        sUpdate := sUpdate + QColumnName(i) + ' = :' + fColumns[i] + COMMA_JOIN
      else
        sUpdate := sUpdate + QColumnName(i) + ' = ' + fValues[i] + COMMA_JOIN;
    end;

    SetLength(sUpdate, Length(sUpdate) - COMMA_JOIN_LEN);

    sWhere := getWhereSQL;

    result := Trim(Format('update %s set %s %s', [QTableName, sUpdate, sWhere]));
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.getWhereSQL: String;
  {
    Returns the WHERE clause for any key columns and any additional
     conditions that may have been specified and is assembled as follows.

    First any key columns/values are combined into a single clause using
     the AND operator to comine multiple key columns as required:

      ((COLUMN = :COLUMN) and (COLUMN2 = :COLUMN2) and (COLUMN3 = VALUE3))

    This entire clause is referred to below as "KEY".

    Then, any additional conditions are appended as specified and combined
     using the operator specified for each condition:

      (KEY) OP1 (CONDITION1) OP2 (CONDITION2)

    If there is no KEY clause then OP1 is ignored and the WHERE clause is
     formed as

      (CONDITION1) OP2 (CONDITION2)
  }
  var
    i: Integer;
  begin
  {@stb}
    result := '';

    if (Length(fKeyColumns) = 0)
     and (Length(fConditions) = 0) then
      EXIT;

    result := 'where ';

    if Length(fKeyColumns) > 0 then
    begin
      // * The key is an "atomic" condition, so if it consists of
      //    more than a single column we parenthesise the entire key
      //    but since each condition in the key is itself parenthesised
      //    we do not add this key parenthesis if there is ONLY one
      //    key column (admittedly primarily for aesthetics)

      if Length(fKeyColumns) > 1 then
        result := result + '(';

      // Add the 'column = :param/value' clauses to the key
      for i := 0 to Pred(Length(fKeyColumns)) do
      begin
        result := result + '(' + QKeyName(i) + ' = ';
        if (fKeyValues[i] = '') then
          result := result + COLON + fKeyColumns[i]
        else
          result := result + fKeyValues[i];
        result := result + ') and ';
      end;

      // Remove the trailing ' and '
      SetLength(result, Length(result) - 5);

      // (see comment * above)
      if Length(fKeyColumns) > 1 then
        result := result + ')';
    end;

    // So far we have added initial key conditions - now we have to
    //  consider any additional conditions that may be specified, if any

    if Length(fConditions) = 0 then
      EXIT;

    // If we are appending additional conditions to key conditions
    //  then we respect the operator of the initial additional
    //  condition otherwise we simply add the condition without
    //  any reference to it's operator

    if Length(fKeyColumns) > 0 then
      result := result + fConditionOps[0] + fConditions[0]
    else
      result := result + fConditions[0];

    // Any remaining additional conditions are added with their
    //  required specified operator

    for i := 1 to High(fConditions) do
      result := result + fConditionOps[i] + fConditions[i];
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TSQLBuilder.Aliased(const aColumns: array of String; aAlias: String): TArrayOfString;
  var
    i: Integer;
  begin
    SetLength(result, Length(aColumns));

    for i := 0 to Pred(Length(aColumns)) do
      result[i] := aAlias + '.' + aColumns[i];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TSQLBuilder.Columns(const aColumns: array of String;
                                     const aTableAlias: String): String;
  {
    This function operates in isolation from the result of the rest of
     the SQLBuilder implementation.  It simply formats a specified array of
     column names into a comma delimited list.

    If a table alias is specified then this is prefixed to the column names.
  }
  var
    i: Integer;
    alias: String;
  begin
  {@stb}
    result := '';

    if (Length(aColumns) = 0) then
      EXIT;

    alias := aTableAlias;
    if (alias <> '') then
      alias := alias + '.';

    for i := 0 to Pred(Length(aColumns)) do
      result := result + alias + aColumns[i] + COMMA_JOIN;

    SetLength(result, Length(result) - COMMA_JOIN_LEN);
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TSQLBuilder.Conditions(const aColumns: array of String;
                                        const aTableAlias: String;
                                        const aOperator: TLogicalOperator): String;
  {
    This function operates independently from the result of the rest of
     the SQLBuilder implementation.  It simply formats a specified array of
     column names into a list of '(NAME = :NAME)' parameterised conditions,
     combined using the specified logical operator.

    If a table alias is specified then this is prefixed to the column name,
     but NOT the value parameter:  '(ALIAS.NAME = :NAME)'
  }
  {@stb}
  var
    i: Integer;
    alias: String;
  begin
    result := '';

    if (Length(aColumns) = 0) then
      EXIT;

    alias := aTableAlias;
    if (alias <> '') then
      alias := alias + '.';

    for i := 0 to Pred(Length(aColumns)) do
      result := result + '(' + alias + aColumns[i] + ' = :' + aColumns[i] + ')' + OPSQL[aOperator];

    SetLength(result, Length(result) - Length(OPSQL[aOperator]));
  {@ste}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Delete(const aTable: String);
  begin
    BeginUpdate;
    Init(stDelete, aTable);
    DoChanged;
    EndUpdate;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Delete(const aTable: String;
                               const aKeys: array of String);
  begin
    BeginUpdate;
    Init(stDelete, aTable);
    DoAddKeyColumns(aKeys);
    DoChanged;
    EndUpdate;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.DoChanged;
  begin
    if (fSQL = '') then
      EXIT;

    fSQL := '';

    if NOT IsUpdating and Assigned(fOnChanged) then
      fOnChanged(self);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Exists(const aTable: String;
                               const aKeys: array of String);
  begin
    BeginUpdate;
    Init(stExists, aTable);
    DoAddKeyColumns(aKeys);
    DoChanged;
    EndUpdate;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Insert(const aTable: String);
  begin
    Insert(aTable, []);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Insert(const aTable: String;
                               const aColumns: array of String);
  begin
    BeginUpdate;
    Init(stInsert, aTable);
    DoAddColumns(aColumns);
    DoChanged;
    EndUpdate;
  end;


  function TSQLBuilder.QColumnName(const aIndex: Integer): String;
  begin
    result := Ident(fColumns[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.QKeyName(const aIndex: Integer): String;
  begin
    result := Ident(fKeyColumns[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.QOrderBy(const aIndex: Integer): String;
  begin
    result := Ident(fOrderBy[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.QTableName: String;
  begin
    result := Ident(fTable);

    if fSchema = '' then
      EXIT;

    result := Ident(fSchema) + '.' + result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Select(const aTable: String);
  begin
    Select(aTable, [], []);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Select(const aTable: String;
                               const aColumns: array of String);
  begin
    Select(aTable, aColumns, []);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Select(const aTable: String;
                               const aColumns, aKeys: array of String);
  begin
    BeginUpdate;
    Init(stSelect, aTable);
    DoAddColumns(aColumns);
    DoAddKeyColumns(aKeys);
    DoChanged;
    EndUpdate;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Select(const aTables, aColumns: array of String);
  begin
    Select(aTables, aColumns, []);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Select(const aTables: array of String;
                               const aColumns: array of String;
                               const aKeys: array of String);
  var
    i: Integer;
    sTableList: String;
  begin
  {@stb}
    ASSERT(Length(aTables) > 0, 'At least one table name must be specified');
  {@ste}

    sTableList := '';

    for i := Low(aTables) to High(aTables) do
      sTableList := sTableList + aTables[i] + COMMA_JOIN;

    SetLength(sTableList, Length(sTableList) - COMMA_JOIN_LEN);  // Remove trailing separator

    Select(sTableList, aColumns, aKeys);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.SetAlias(const aAlias: String);
  begin
    fTableAlias := aAlias;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Union;
  begin
    Init(stUnion, '');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Update(const aTable: String);
  begin
    Update(aTable, [], []);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Update(const aTable: String;
                               const aColumns: array of String);
  begin
    Update(aTable, aColumns, []);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Update(const aTable: String;
                               const aColumns, aKeys: array of String);
  begin
    BeginUpdate;
    Init(stUpdate, aTable);
    DoAddColumns(aColumns);
    DoAddKeyColumns(aKeys);
    DoChanged;
    EndUpdate;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Init(const aStatementType: TStatementType;
                             const aTable: String);
  var
    i: Integer;
  begin
    fStatementType  := aStatementType;
    fTable          := aTable;

    fColumns      := NIL;
    fConditions   := NIL;
    fConditionOps := NIL;
    fValues       := NIL;
    fKeyColumns   := NIL;
    fKeyValues    := NIL;
    fOrderBy      := NIL;
    fOrderByOrder := NIL;
    fGrouped      := FALSE;

    fHavingConditions   := NIL;
    fHavingConditionOps := NIL;

    for i := 0 to Pred(fUnions.Count) do
      fUnions.Objects[i].Free;

    fUnions.Clear;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddColumn(const aColumn: String);
  {
    Adds a column to a select, insert or update statement.

    For insert and update statements a corresponding parameter will be
     added with the same name, i.e. AddColumn('ID') adds a column called
     'ID' and a parameter ':ID' to associate a value with that column.
  }
  begin
  {@stb}
    ASSERT(fStatementType in [stSelect, stInsert, stUpdate],
           'Columns may only be added to SELECT, INSERT and UPDATE statements');
  {@ste}
    ArrayAdd(fColumns, Trim(aColumn));
    ArrayAdd(fValues, '');
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddColumn(const aColumn, aLiteral: String;
                                  const aIsParam: Boolean);
  {
    Adds a column with an associated literal expression to an insert or
     update statement.

    AddColumn('ID', 'NULL') adds a column called 'ID' and an associated
     value expression of 'NULL'.

    If the literal expression is required to be a string value then
     appropriate string delimiters must be included in the literal.

    This method may also be used to add columns with associated
     parameter names where the parameter name is required to be
     different from the column name by specifying aIsParam as TRUE
     (by default this param is FALSE and may be omitted in all other
     situations).

    Examples (for an update statement):

      AddColumn('NAME', 'NULL');        // >>   NAME = NULL
      AddColumn('NAME', '''Bob''');     // >>   NAME = 'Bob'
      AddColumn('NAME', 'CUST_NAME', TRUE);  // >>   NAME = :CUST_NAME
  }
  begin
  {@stb}
    ASSERT(fStatementType in [stInsert, stUpdate],
           'Columns with associated literal values may only be added to INSERT and UPDATE statements');
  {@ste}
    ArrayAdd(fColumns, Trim(aColumn));
    if aIsParam then
      ArrayAdd(fValues, COLON + Trim(aLiteral))
    else
      ArrayAdd(fValues, Trim(aLiteral));
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddColumn(const aColumn : String;
                                  const aLiteral: Integer);
  {
    Adds a column with an associated literal integer value to an insert
     or update statement.  e.g. for an update statement:

      AddColumn('ID', 10);     // >>   ID = 10
  }
  begin
    AddColumn(aColumn, IntToStr(aLiteral));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddColumns(const aColumns: array of String);
  {
    Adds a list of column names to a select, insert or update statement.

    For insert and update statements each column added will have an
     associated parameter also added, with the same name as the
     corresponding column.

    e.g. AddColumns(['ID', 'NAME']) will add the columns 'ID' and 'NAME'
          and parameters ':ID' and ':NAME'.
  }
  begin
  {@stb}
    ASSERT(fStatementType in [stSelect, stInsert, stUpdate],
           'Columns may only be added to SELECT, INSERT and UPDATE statements');
  {@ste}
    ArrayAdd(fColumns, aColumns);
    ArrayAdd(fValues, Length(aColumns));
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddCondition(const aColumn: String;
                                     const aColumnOp: TRelationalOperator;
                                     const aOperator: TLogicalOperator);
  var
    condition: String;
  begin
    condition := '(' + Ident(aColumn);

    case aColumnOp of
      roEquals          : condition := condition + ' = :' + aColumn;
      roGreaterThan     : condition := condition + ' > :' + aColumn;
      roLessThan        : condition := condition + ' < :' + aColumn;
      roIsNull          : condition := condition + ' is null';
      roNotEqual        : condition := condition + ' <> :' + aColumn;
      roNotGreaterThan  : condition := condition + ' <= :' + aColumn;
      roNotLessThan     : condition := condition + ' >= :' + aColumn;
      roIsNotNull       : condition := condition + ' is not null';
    end;

    condition := condition + ')';

    AddCondition(condition, aOperator);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddCondition(const aCondition: String;
                                     const aOperator: TLogicalOperator);
  {
    Adds a fully formed condition clause to a select, update or delete
     statement.  That is, the condition specified must be a complete,
     valid SQL condition such as:

      ID IS NOT NULL
      NAME LIKE 'S%'
      A.ID = B.PARENT_ID

    The operator specified is used to determine how the condition is
     to be combined with other conditions in the same statement.  If
     only one condition is specified in a statement then the operator
     is ignored, as it is for the first condition in a statement even
     when multiple conditions exist.

    For a UNION statement the condition is added to EACH individual
     union previously added to that statement.
  }
  var
    i: Integer;
  begin
  {@stb}
    ASSERT(fStatementType in [stSelect, stDelete, stUnion, stUpdate],
           'Conditions may only be added to SELECT, UNION, DELETE and UPDATE statements');
  {@ste}

    // For a union we add the condition to EACH individual statement
    //  in the union
    if StatementType = stUnion then
    begin
      for i := 0 to Pred(fUnions.Count) do
        TSQLBuilder(fUnions.Objects[i]).AddCondition(aCondition, aOperator);
    end
    else
    begin
      ArrayAdd(fConditions, '(' + aCondition + ')');
      ArrayAdd(fConditionOps, OPSQL[aOperator]);
      DoChanged;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddConditions(const aConditions: array of String;
                                      const aInnerOp, aOuterOp: TLogicalOperator);
  {
    When adding to a UNION statement, the condition is added to EACH
     statement previously added to that union.
  }
  var
    i: Integer;
    clause: String;
  begin
    if Length(aConditions) = 0 then
      EXIT;

    clause := '';
    for i := Low(aConditions) to High(aConditions) do
      clause := clause + '(' + aConditions[i] + ')' + OPSQL[aInnerOp];

    // Remove the trailing 'inner' operator
    SetLength(clause, Length(clause) - Length(OPSQL[aInnerOp]));

    AddCondition(clause, aOuterOp);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddColumnConditions(const aColumns: array of String;
                                            const aTableAlias: String;
                                            const aInnerOp, aOuterOp: TLogicalOperator);
  {
    Adds a single condition consisting of a group of parameterised conditions
     formed by combining a list of column names (with associated parameter
     names formed from the column names).

    Each column condition is combined using the "Inner" operator.  The
     entire condition formed is added with the "Outer" operator.

    e.g.:

      AddConditions(['ID', 'STATUS'], 'QUOTE', loOR, loAND);

    yields the condition:

      (QUOTE.ID = :ID) OR (QUOTE.STATUS = :STATUS)

    which in a statement with other conditions will be combined as:

      AND ( (QUOTE.ID = :ID) OR (QUOTE.STATUS = :STATUS) )

    The OUTER operator specified is used to determine how the condition is
     to be combined with other conditions in the same statement.  If
     only one condition is specified in a statement then the operator
     is ignored, as it is for the first condition in a statement even
     when multiple conditions exist.

    When adding to a UNION statement, the condition is added to EACH
     statement previously added to that union.
  }
  begin
    AddCondition(Conditions(aColumns, aTableAlias, aInnerOp), aOuterOp);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddGroupBy(const aColumns: array of String);
  {
    Establishes or adds a list of GROUP BY columns for a select statement.

    When explicit GROUP BY columns are specified using this method then
     the GROUP BY clause MUST be completely specified for valid SQL to
     be obtained.

    In general you should use this method only if a valid GROUP BY clause
     cannot be reliably determined from the column list due to some
     exotic expression in that column list.

    In most cases it should be possible to simply set the Grouped property
     to TRUE and allow SQLBuilder to determine the GROUP BY clause from
     the columns in the statement.  This will remove any DISTINCT predicates
     or column alias names as well as ignoring columns involved in function
     expressions, i.e. SUM(..), COUNT(..) etc.
  }
  begin
  {@stb}
    ASSERT((fStatementType = stSelect), 'Only SELECT statements may be Grouped');
  {@ste}
    if Length(aColumns) = 0 then
      EXIT;

    BeginUpdate;
    try
      Grouped := TRUE;
      ArrayAdd(fGroupBy, aColumns);
    finally
      EndUpdate;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddHavingCondition(const aCondition: string;
                                           const aOperator: TLogicalOperator);
  {
    Adds a condition to a HAVING clause of a grouped select statement.
     The condition specified must be a complete valid SQL condition
     such as:

      ID IS NOT NULL
      NAME LIKE 'S%'
      A.ID = B.PARENT_ID

    The operator specified is used to determine how the condition is
     to be combined with other HAVING conditions in the same statement.
     If only one HAVING condition is specified in a statement then the
     operator is ignored, as it is for the first HAVING condition in a
     statement even when multiple HAVING conditions exist.
  }
  begin
  {@stb}
    ArrayAdd(fHavingConditions, '(' + aCondition + ')');
  {@ste}
    ArrayAdd(fHavingConditionOps, OPSQL[aOperator]);
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddKey(const aColumn: String);
  {
    Adds a key condition for a column to a select, delete or update statement.
     The condition will be parameterised, with a parameter having the same
     name as the column.

      AddKey('ID');     // >>   ID = :ID
  }
  begin
  {@stb}
    ASSERT(fStatementType in [stSelect, stDelete, stUpdate],
           'Key columns may only be added to SELECT, DELETE and UPDATE statements');
  {@ste}
    ArrayAdd(fKeyColumns, aColumn);
    ArrayAdd(fKeyValues,  '');
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddKey(const aColumn, aLiteral: String;
                               const aIsParam: Boolean);
  {
    Adds a key condition for a column to a select, delete or update statement.
     The condition will consist of a column name and a literal value.

    Examples (for an update statement):

      AddKey('NAME', 'NULL');        // >>   NAME = NULL
      AddKey('NAME', '''Bob''');     // >>   NAME = 'Bob'
      AddKey('NAME', 'CUST_NAME', TRUE);  // >>   NAME = :CUST_NAME
  }
  begin
  {@stb}
    ASSERT(fStatementType in [stSelect, stDelete, stUpdate],
           'Key columns with associated literals may only be added to SELECT, DELETE and UPDATE statements');
  {@ste}
    ArrayAdd(fKeyColumns, aColumn);
    if aIsParam then
      ArrayAdd(fKeyValues, COLON + aLiteral)
    else
      ArrayAdd(fKeyValues,  aLiteral);
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddKeys(const aColumns: array of String);
  {
    Adds a list of column name key conditionss to a select, delete or
     update statement.

      AddKeys(['ID', 'REC_STATUS']);     // >>   (ID = :ID) and (REC_STATUS = :REC_STATUS)
  }
  begin
  {@stb}
    ASSERT(fStatementType in [stSelect, stDelete, stUpdate],
           'Key columns with associated literals may only be added to SELECT, DELETE and UPDATE statements');
  {@ste}
    ArrayAdd(fKeyColumns, aColumns);
    ArrayAdd(fKeyValues, Length(aColumns));
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddOrderBy(const aColumns: array of String;
                                   const aOrder: TSortOrder);
  {
    Adds a set of columns to the ORDER BY clause of a select or union
     statement. Each column added will be sorted in the order specified.
     To sort different columns with different ordering (asc vs desc)
     you must add each column(s) separately, specifying the appropriate
     sort order in each case (ascending is the default):

    e.g.

      AddOrderBy(['SURNAME', 'FORENAME']);
      AddOrderBy(['BIRTHDATE'], soDESC);

    will yield:

      ORDER BY SURNAME asc, FORENAME asc, BIRTHDATE desc
  }
  const
  {@stb}
    MODIFIER: array[soAscending..soDescending] of String = (' asc', ' desc');
  {@ste}
  var
    i: Integer;
  begin
  {@stb}
    ASSERT(fStatementType in [stSelect, stUnion],
           'Order by columns may only be added to SELECT or UNION statements');
  {@ste}

    ArrayAdd(fOrderBy, Length(aColumns));
    ArrayAdd(fOrderByOrder, Length(aColumns));

    for i := 0 to Pred(Length(aColumns)) do
    begin
      fOrderBy[Length(fOrderBy) - Length(aColumns) + i]       := aColumns[i];
      fOrderByOrder[Length(fOrderBy) - Length(aColumns) + i]  := MODIFIER[aOrder];
    end;

    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.AddUnion(const aID: String): TSQLBuilder;
  {
    Adds a statement to a union.  The statement is initialised as a Select
     statement, but no table is specified.
  }
  begin
  {@stb}
    ASSERT(StatementType = stUnion, 'Unions may only be added to UNION statements');
    ASSERT(fUnions.IndexOf(Uppercase(aID)) = -1, 'A union with ID ''' + aID + ''' has already been added to the statement');
  {@ste}

    result := TSQLBuilder.Create;
    result.Select('');

    fUnions.AddObject(Uppercase(aID), result);

    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddValue(const aValue: String);
  {
    Adds a literal value to the list of values in the result set of
     a select statement.
  }
  begin
  {@stb}
    ASSERT(fStatementType in [stSelect],
           'Literal values may only be added to SELECT statements');
  {@ste}
    ArrayAdd(fColumns, '');
    ArrayAdd(fValues, aValue);
    DoChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.Assign(const aSource: TSQLBuilder);
  var
    i: Integer;
  begin
    if Assigned(aSource) then
    begin
      Init(stUnknown, '');

      fTable         := aSource.fTable;
      fColumns       := aSource.fColumns;
      fConditions    := aSource.fConditions;
      fConditionOps  := aSource.fConditionOps;
      fValues        := aSource.fValues;
      fKeyColumns    := aSource.fKeyColumns;
      fKeyValues     := aSource.fKeyValues;
      fOrderBy       := aSource.fOrderBy;
      fOrderByOrder  := aSource.fOrderByOrder;
      fGroupBy       := aSource.fGroupBy;
      fGrouped       := aSource.fGrouped;
      fSQL           := aSource.fSQL;

      fStatementType       := aSource.fStatementType;
      fOnChanged           := aSource.fOnChanged;
      fUpdateCount         := aSource.fUpdateCount;
      fHavingConditions    := aSource.fHavingConditions;
      fHavingConditionOps  := aSource.fHavingConditionOps;

      for i := 0 to Pred(aSource.fUnions.Count) do
        fUnions.AddObject(aSource.fUnions[i],
                          TSQLBuilder(aSource.fUnions.Objects[i]).Clone);
    end
    else
      Init(stUnknown, '');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.BeginUpdate;
  begin
    Inc(fUpdateCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.Clone: TSQLBuilder;
  begin
    result := TSQLBuilder.Create;
    result.Assign(self);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.EndUpdate;
  begin
    Dec(fUpdateCount);
    if (fUpdateCount = 0) then
    begin
      fSQL := ' '; // Forces the update to be applied
      DoChanged;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddTable(const aTable: String);
  {
    Adds a table to the list of tables involved in the statement.  If a
     tablename requires an alias, then this should simply be specified
     in the table name expression:

    Add simple table name:      AddTable('CUSTOMER');

    Add table name with alias:  AddTable('CUSTOMER C');

    ? No restrictions currently but perhaps should only be permitted
      for select statements ?
  }
  begin
  {$ifdef DEBUG}
  {@stb}
    ASSERT(Trim(aTable) <> '', 'Cannot add an empty table');
  {@ste}
  {$endif}

    if fTable = '' then
      Table := Trim(aTable)
    else
    begin
      fTable := fTable + ', ' + Trim(aTable);
      DoChanged;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSQLBuilder.AddTables(const aTables: array of String);
  {
    Adds a set of table names (with aliases if included in the table name
     expressions) to the list of tables involved in the statement.

    ? No restrictions currently but perhaps should only be permitted
      for select statements ?
  }
  var
    i: Integer;
  begin
    BeginUpdate;
    try
      for i := Low(aTables) to High(aTables) do
        AddTable(aTables[i]);
    finally
      EndUpdate;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.Ident(const aName: String): String;
  begin
    case IdentType of
      itLiteral   : result := aName;
      itQuoted    : result := '"' + aName + '"';
      itSQLServer : result := '[' + aName + ']';
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSQLBuilder.IndexOfColumn(const aColumn: string): integer;
  var
    i: Integer;
  begin
    result := -1;

    for i := Low(fColumns) to High(fColumns) do
    begin
      if (fColumns[i] = aColumn) then
      begin
        result := i;
        BREAK;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Distinct(const aValue: String) : string;
  begin
    Result := DISTINCT_PREFIX + AValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Negate(const aValue: String) : string;
  begin
    {@stb}
    Result := 'not (' + aValue + ')';
    {@ste}
  end;






end.
