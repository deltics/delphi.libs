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

{$ifdef debug_Deltics_ADO_Command}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Command;


interface

  uses
  { VCL: }
    Classes,
  { Deltics.RTL: }
    Deltics.Classes,
    Deltics.JSON,
    Deltics.MultiCast,
    Deltics.SQLBuilder,
    Deltics.StateList,
  { Deltics.Tokeniser: }
    Deltics.Tokeniser,
    Deltics.Tokeniser.Tokens,
  { Deltics.ADO: }
    Deltics.ADO.Connection,
    Deltics.ADO.DataSource,
    Deltics.ADO.Parameters,
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.Utils;


  type
    TADOCustomCommand = class(TCOMInterfacedObject)
    private
      fADOCommand: _Command;
      fCommandType: CommandTypeEnum;
      fConnection: TADOConnection;
      fLiteral: Boolean;
      fParameters: TADOParameters;
      fRecordsAffected: Integer;
      fSQL: TStringList;
      fSQLBuilder: TSQLBuilder;
      fSQLTokens: ITokenList;
      fState: TStateList;
      fOn_Complete: TMulticastNotify;
      fOn_SQLChanged: TMulticastNotify;
      procedure set_CommandType(const aValue: CommandTypeEnum);
      procedure set_Connection(const aValue: TADOConnection);
      procedure set_Literal(const aValue: Boolean);
    private
      procedure ClearParameters;
      procedure CreateParameters;
      procedure ApplyParameters;
      procedure ApplySQL;
      procedure OnSQLBuilderChanged(Sender: TObject);
      procedure OnSQLChanged(Sender: TObject);
      procedure OnSQLChangedWhileChanging(Sender: TObject);
      procedure Reset;
      procedure TranslateException;
      procedure UpdateSQLTokens;
    protected
      function get_SQL: TStringList;
      function get_SQLBuilder: TSQLBuilder;
      procedure DoComplete(const aRecordsAffected: Integer);
      function DoExecute(var aAffected: OleVariant): RecordSet; virtual;
      procedure DoExecuteAsync; virtual;
      procedure DoSQLChanged; virtual;
      function GetCommandType: CommandTypeEnum; virtual;
      property ADOCommand: _Command read fADOCommand;
    public
      constructor Create(const aConnection: TADOConnection); virtual;
      destructor Destroy; override;
      procedure Execute;
      procedure ExecuteAsync;
      procedure Prepare;
      function HasParam(const aName: String): Boolean;
      function ParamByName(const aName: String): TADOParameter;
      property CommandType: CommandTypeEnum read fCommandType write set_CommandType;
      property Connection: TADOConnection read fConnection write set_Connection;
      property Literal: Boolean read fLiteral write set_Literal;
      property Parameters: TADOParameters read fParameters;
      property RecordsAffected: Integer read fRecordsAffected;
      property SQL: TStringList read get_SQL;
      property SQLBuilder: TSQLBuilder read get_SQLBuilder;
      property SQLTokens: ITokenList read fSQLTokens;
      property State: TStateList read fState;
      property On_Complete: TMulticastNotify read fOn_Complete;
      property On_SQLChanged: TMulticastNotify read fOn_SQLChanged;
    end;


    TADOCommand = class(TADOCustomCommand)
    protected
      function GetCommandType: CommandTypeEnum; override;
    end;


    TADOStoredProcedure = class(TADOCustomCommand)
    private
      fParamString: String;
      fProcedureName: String;
      function get_ParamString: String;
    protected
      function GetCommandType: CommandTypeEnum; override;
      procedure DoSQLChanged; override;
    public
      property ParamString: String read get_ParamString;
      property ProcedureName: String read fProcedureName;
    end;



  const
    csPrepared    : TStateID = 'csPrepared';
    csExecuting   : TStateID = 'csExecuting';
    csComplete    : TStateID = 'csComplete';
    csError       : TStateID = 'csError';
    csSQLChanged  : TStateID = 'csSQLChanged';
    csBoundParams : TStateID = 'csBoundParams';



implementation

  uses
  { VCL: }
    ComObj,
    SysUtils,
    Variants,
  { deltics: }
    Deltics.SysUtils,
    Deltics.Tokeniser.Dictionary.SQL,
  { Deltics.ADO: }
    Deltics.ADO.Provider.Adapter;


  type
    TConnectionHelper = class(TADOConnection);
    TDataSourceHelper = class(TADODataSource);
    TParameterHelper = class(TADOParameter);
    TParametersHelper = class(TADOParameters);



  constructor TADOCustomCommand.Create(const aConnection: TADOConnection);
  begin
    inherited Create;

    fCommandType  := GetCommandType;

    fADOCommand   := CoCommand.Create;
    fSQL          := TStringList.Create;
    fState        := TStateList.Create(self, [csPrepared,
                                              csExecuting,
                                              csComplete,
                                              csError,
                                              csSQLChanged,
                                              csBoundParams]);
    fParameters   := TADOParameters.Create;

    fSQL.OnChange := OnSQLChanged;

    TMultiCastNotify.CreateEvents(self, [@fOn_Complete,
                                         @fOn_SQLChanged]);
    Connection := aConnection;
  end;


  destructor TADOCustomCommand.Destroy;
  begin
    FreeAndNIL([@fState,
                @fSQL,
                @fSQLBuilder,
                @fParameters,
                @fOn_Complete,
                @fOn_SQLChanged]);
    inherited;
  end;


  procedure TADOCustomCommand.set_CommandType(const aValue: CommandTypeEnum);
  begin
    if (ClassType <> TADOCustomCommand) then
      raise Exception.Create('Cannot change the CommandType of a ' + ClassName);

    fCommandType := aValue;
  end;


  procedure TADOCustomCommand.set_Connection(const aValue: TADOConnection);
  begin
    if (Connection = aValue) then
      EXIT;

    if Assigned(Connection) then
      TConnectionHelper(Connection).NotifyCommand(self, opRemove);

    fConnection := aValue;

    if Assigned(Connection) then
      TConnectionHelper(Connection).NotifyCommand(self, opAdd);
  end;


  procedure TADOCustomCommand.set_Literal(const aValue: Boolean);
  begin
    if fLiteral = aValue then
      EXIT;

    fLiteral := aValue;

    case Literal of
      TRUE  : begin
                // NO-OP as yet
              end;

      FALSE : begin
                // NO-OP as yet
              end;
    end;
  end;


  procedure TADOCustomCommand.DoSQLChanged;
  begin
    ClearParameters;
  end;


  procedure TADOCustomCommand.OnSQLBuilderChanged(Sender: TObject);
  begin
    SQL.Text := SQLBuilder.SQL;
  end;


  procedure TADOCustomCommand.OnSQLChanged(Sender: TObject);
  begin
    // Redirect the SQL OnChange event whilst processing an
    //  SQL OnChange event.  This allows us to detect when an
    //  event handler or sub-class has modified the changed
    //  SQL without causing recursion issues
    fSQL.OnChange := OnSQLChangedWhileChanging;
    try
      Reset;

      if Literal then
        EXIT;

      // Tokenise the new SQL - if event handlers or subclasses
      //  wish to modify the new SQL a tokenised list may help them
      UpdateSQLTokens;

      // Give event handlers and subclasses an opportunity to
      //  further modify the new SQL if required
      On_SQLChanged.DoEvent;
      DoSQLChanged;

      CreateParameters;

    finally
      // If an event handler or subclass modified the changed SQL then
      //  the SQLChanged state will have been set and we should re-tokenise
      if State[csSQLChanged] then
        UpdateSQLTokens;

      // Restore the OnChange SQL event and remove the SQL Changed state
      fSQL.OnChange := OnSQLChanged;
      State.Leave(csSQLChanged);
    end;
  end;


  procedure TADOCustomCommand.OnSQLChangedWhileChanging(Sender: TObject);
  begin
    State.Enter(csSQLChanged);
  end;


  procedure TADOCustomCommand.ApplyParameters;
  var
    i: Integer;
  begin
    if Literal then
      EXIT;

    // Will create or update the ADO parameter objects required for each
    //  parameter identified in this command
    TParametersHelper(fParameters).Apply;

    // If not yet bound to any command parameters we must populate the
    //  command parameters collection
    if State[csBoundParams] then
      EXIT;

    {
    // Ensure that the current command parameters collection is empty
    //  before adding the ADO parameters
    while ADOCommand.Parameters.Count > 0 do
      ADOCommand.Parameters.Delete(0);
    }

    for i := 0 to Pred(Parameters.ADOParamCount) do
      ADOCommand.Parameters.Append(Parameters.ADOParams[i]);

    State.Enter(csBoundParams);
  end;


  procedure TADOCustomCommand.ApplySQL;
  var
    i: Integer;
    scmd: String;
    adapter: TADOProviderAdapter;
    toks: ITokenList;
  begin
    adapter := NIL;
    scmd    := SQL.Text;

    if NOT Literal then
    begin
      if (CommandType <> adCmdUnknown)
       and GetProviderAdapter(Connection.DataSource.Provider, adapter) then
      begin
        if self.InheritsFrom(TADOStoredProcedure) then
          scmd := adapter.Format(TADOStoredProcedure(self));
      end;

      // TODO: Check that this use of scmd is correct in the case of a stored procedure
      //        Looks very dodgy to me (scmd is formatted by the adapter but then
      //        overwritten by the parameter substitution.... surely wrong ?

      toks := SQLTokens;
      scmd := '';
      for i := 0 to Pred(toks.Count) do
        with toks[i] do
          if Assigned(Definition)
           and (Definition.ID = sqlParameter) then
            scmd := scmd + '?'
          else
            scmd := scmd + Text;
    end;

    ADOCommand.CommandText := scmd;
  end;


  procedure TADOCustomCommand.ClearParameters;
  begin
    while ADOCommand.Parameters.Count > 0 do
      ADOCommand.Parameters.Delete(0);
      
    TParametersHelper(fParameters).Clear;
  end;


  procedure TADOCustomCommand.CreateParameters;
  var
    i: Integer;
    name: String;
    cur: ITokenCursor;
  begin
    TParametersHelper(fParameters).Clear;

    cur := TTokenCursor.Create(SQLTokens);
    cur.First(sqlParameter);
    while NOT cur.EOF do
    begin
      name := cur.Token.Text;
      Delete(name, 1, 1);

      TParametersHelper(fParameters).Add(name);

      cur.Next(sqlParameter);
    end;
  end;


  procedure TADOCustomCommand.Execute;
  var
    affected: OleVariant;
  begin
    Prepare;
    try
      ApplyParameters;

      State.Enter(csExecuting);

      DoExecute(affected);
      DoComplete(affected);
    except
      on e: EOleException do
        TranslateException;
    end;
  end;


  procedure TADOCustomCommand.ExecuteAsync;
  begin
    Prepare;
    try
      State.Enter(csExecuting);
      DoExecuteAsync;
    except
      on EOleException do
        TranslateException;
    end;
  end;


  function TADOCustomCommand.ParamByName(const aName: String): TADOParameter;
  begin
    if NOT Parameters.FindParam(aName, result) then
      raise EADOException.CreateFmt('No such parameter (%s) in SQL', [aName], SQL.Text);
  end;


  procedure TADOCustomCommand.Prepare;
  begin
    if State.InState[csPrepared] then
      EXIT;

    ApplySQL;

    ADOCommand.Prepared := TRUE;
    State.Enter(csPrepared);
  end;


  procedure TADOCustomCommand.TranslateException;
  var
    e: EOleException;
  begin
    ASSERT(ExceptObject is EOleException);

    e := EOleException(ExceptObject);
    if Assigned(Connection.DataSource) then
      raise Connection.DataSource.TranslateException(e, SQL.Text) at ExceptAddr
    else
      raise EADOException.Create(e.Message, SQL.Text) at ExceptAddr
  end;


  procedure TADOCustomCommand.UpdateSQLTokens;
  begin
    fSQLTokens := TTokenList.Create(SQL.Text, CoreSQL, []);
  end;


  procedure TADOCustomCommand.DoComplete(const aRecordsAffected: Integer);
  begin
    State.Leave(csExecuting);
    State.Enter(csComplete);
    fRecordsAffected := aRecordsAffected;
    // TODO: Event notification
  end;


  function TADOCustomCommand.DoExecute(var aAffected: OleVariant): RecordSet;
  var
    unused: OleVariant;
  begin
    unused := EmptyParam;
    result := ADOCommand.Execute(aAffected, unused, fCommandType);
  end;


  procedure TADOCustomCommand.DoExecuteAsync;
  var
    affected: OleVariant;
    unused: OleVariant;
  begin
    // We must pass an OleVariant for the affected records param even though
    //  it is useless in an async call, otherwise we get an invalid parameter
    //  error from ADO
    affected  := EmptyParam;
    unused    := EmptyParam;
    ADOCommand.Execute(affected, unused, fCommandType or adAsyncExecute);
  end;


  function TADOCustomCommand.GetCommandType: CommandTypeEnum;
  begin
    result := adCmdUnknown;
  end;


  function TADOCustomCommand.get_SQL: TStringList;
  begin
    result := fSQL;
  end;


  function TADOCustomCommand.get_SQLBuilder: TSQLBuilder;
  begin
    if NOT Assigned(fSQLBuilder) then
    begin
      fSQLBuilder := TSQLBuilder.Create;
      fSQLBuilder.OnChanged := OnSQLBuilderChanged;
    end;

    result := fSQLBuilder;
  end;


  function TADOCustomCommand.HasParam(const aName: String): Boolean;
  var
    unused: TADOParameter;
  begin
    result := fParameters.FindParam(aName, unused);
  end;


  procedure TADOCustomCommand.Reset;
  begin
    fRecordsAffected := -1;

    State.Leave([csComplete,
                 csExecuting,
                 csPrepared,
                 csSQLChanged,
                 csBoundParams]);

//    TParametersHelper(fParameters).Clear;
  end;






{ TCommand }

  function TADOCommand.GetCommandType: CommandTypeEnum;
  begin
    result := adCmdText;
  end;




{ TStoredProcedure }

  function TADOStoredProcedure.get_ParamString: String;
  var
    i: Integer;
  begin
    result := fParamString;

    if (result = '') and (fParameters.Count > 0) then
    begin
      for i := 0 to Pred(fParameters.Count) do
        result := result + ':' + fParameters[i].Name + ', ';

      SetLength(result, Length(result) - 2);
    end;
  end;


  function TADOStoredProcedure.GetCommandType: CommandTypeEnum;
  begin
    result := adCmdStoredProc;
  end;


  procedure TADOStoredProcedure.DoSQLChanged;
  var
    scmd: String;
    lparen: Integer;
    rparen: Integer;
  begin
    inherited;

    fParamString    := '';
    fProcedureName  := '';

    scmd    := Trim(SQL.Text);
    lparen  := Pos('(', scmd);
    if (lparen > 0) then
    begin
      fProcedureName := scmd;
      SetLength(fProcedureName, lparen - 1);
      fProcedureName := Trim(fProcedureName);

      fParamString := Trim(Copy(scmd, lparen + 1, (Length(scmd) - lparen)));

      // TODO: Assumes only one pair of parentheses - will break if there are nested parentheses

      rparen := Pos(')', fParamString);
      SetLength(fParamString, rparen - 1);
    end
    else
      fProcedureName := scmd;
  end;








end.
