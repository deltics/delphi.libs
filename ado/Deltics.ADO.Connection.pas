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

{$i deltics.ado.inc}

{$ifdef debug_Deltics_ADO_Connection}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Connection;


interface

  uses
  { vcl: }
    Classes,
    ComObj,
  { deltics: }
    Deltics.Classes,
    Deltics.ADO,
    Deltics.ADO.DataSource,
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.Utils;


  type
    TADOConnection = class(TCOMInterfacedObject)
    private
      fADOConnection: TCoConnection;
      fCommands: TList;
      fConnectionString: TStringList;
      fDataSource: TADODataSource;
      fTransactionAborted: Boolean;
      fTransactionDepth: Integer;
      function get_ADOConnection: _Connection;
      function get_ConnectionString: String;
      function get_InTransaction: Boolean;
      function get_IsConnected: Boolean;
      function get_UserID: String;
      procedure set_ConnectionString(const aValue: String);
      procedure set_DataSource(const aValue: TADODataSource);
      procedure set_Password(const aValue: String);
      procedure set_UserID(const aValue: String);
    private
      procedure ApplyDataSourceProperties;
      procedure DataSourceDestroyed(Sender: TObject);
      function GetCommand(const aCommand: _Command; var aRef): Boolean;
      procedure _OnExecuteComplete(      aSender: TObject;
                                         aRecordsAffected: Integer;
                                   const aError: Error;
                                   var   adStatus: EventStatusEnum;
                                   const aCommand: _Command;
                                   const aRecordset: _Recordset;
                                   const aConnection: _Connection);
    protected
      function TranslateException(const aException: EOleException;
                                  const aSQL: String = ''): EADOException;
      procedure NotifyCommand(const aCommand: TObject;
                              const aOperation: TNotificationOperation);
      property ADOConnection: _Connection read get_ADOConnection;
    public
      constructor Create(const aDataSource: TADODataSource);
      destructor Destroy; override;
      procedure BeginTransaction;
      procedure Commit;
      procedure Rollback;
      function Connect(const aUserID: String = ''; const aPassword: String = ''): Boolean;
      procedure Disconnect;
      procedure ExecSQL(const aCommand: String);
      property ConnectionString: String read get_ConnectionString write set_ConnectionString;
      property DataSource: TADODataSource read fDataSource write set_DataSource;
      property InTransaction: Boolean read get_InTransaction;
      property IsConnected: Boolean read get_IsConnected;
      property Password: String write set_Password;
      property UserID: String read get_UserID write set_UserID;
    end;



implementation

  uses
    SysUtils,
    Variants,
    Deltics.SysUtils,
    Deltics.ADO.Command;



  type
    TCustomCommandHelper = class(TADOCustomCommand);




  constructor TADOConnection.Create(const aDataSource: TADODataSource);
  begin
    inherited Create;

    fADOConnection := TCoConnection.Create(NIL);
    fADOConnection.OnExecuteComplete := _OnExecuteComplete;

    fCommands         := TList.Create;
    fConnectionString := TStringList.Create;

    DataSource        := aDataSource;
  end;


  procedure TADOConnection.DataSourceDestroyed(Sender: TObject);
  begin
    DataSource := NIL;
  end;


  destructor TADOConnection.Destroy;
  begin
    DataSource := NIL;

    FreeAndNIL([@fConnectionString,
                @fCommands,
                @fADOConnection]);

    inherited;
  end;


  function TADOConnection.get_ADOConnection: _Connection;
  begin
    result := fADOConnection.DefaultInterface;
  end;


  function TADOConnection.get_ConnectionString: String;
  begin
    if Assigned(fDataSource) then
      result := fDataSource.ConnectionString.AsString
    else
      result := fConnectionString.Text;
  end;


  function TADOConnection.get_InTransaction: Boolean;
  begin
    result := (fTransactionDepth > 0);
  end;


  function TADOConnection.get_IsConnected: Boolean;
  begin
    result := (ADOConnection.State = adStateOpen)
  end;


  function TADOConnection.get_UserID: String;
  begin
    result := fConnectionString.Values['User ID'];
  end;


  procedure TADOConnection.set_ConnectionString(const aValue: String);
  begin
    ASSERT(NOT Assigned(fDataSource), 'Attempt to set ConnectionString when DataSource is used');

    fConnectionString.Text := aValue;
  end;


  procedure TADOConnection.set_DataSource(const aValue: TADODataSource);
  begin
    if (DataSource = aValue) then
      EXIT;

    if Assigned(DataSource) then
      DataSource.On_Destroy.Remove(DataSourceDestroyed);

    Disconnect;

    fDataSource := aValue;

    if Assigned(DataSource) then
      DataSource.On_Destroy.Add(DataSourceDestroyed);
  end;


  procedure TADOConnection.set_Password(const aValue: String);
  begin
    ASSERT(NOT IsConnected);
    fConnectionString.Values['Password'] := aValue;
  end;


  procedure TADOConnection.set_UserID(const aValue: String);
  begin
    ASSERT(NOT IsConnected);
    fConnectionString.Values['User ID'] := aValue;
  end;


  function TADOConnection.TranslateException(const aException: EOleException;
                                             const aSQL: String): EADOException;
  begin
    if Assigned(DataSource) then
      result := DataSource.TranslateException(aException, aSQL)
    else
      result := EADOException.Create(aException.Message, aSQL);
  end;


  function TADOConnection.Connect(const aUserID, aPassword: String): Boolean;
  begin
    ASSERT(NOT IsConnected);
    ADOConnection.ConnectionString := ConnectionString;
    ApplyDataSourceProperties;

    try
      ADOConnection.Open(ConnectionString, aUserID, aPassword, Integer(adConnectUnspecified));
    except
      on e: EOleException do
        raise TranslateException(e) at ExceptAddr;
    end;
    
    result := IsConnected;
  end;


  procedure TADOConnection.Disconnect;
  begin
    if IsConnected then
      ADOConnection.Close;
  end;


  procedure TADOConnection.ExecSQL(const aCommand: String);
  var
    affected: OleVariant;
  begin
    ASSERT(IsConnected);
    ADOConnection.Execute(aCommand, affected, adCmdUnknown);
  end;


  procedure TADOConnection.ApplyDataSourceProperties;
  var
    i: Integer;
    sName: String;
  begin
    if NOT Assigned(DataSource) then
      EXIT;

    for i := 0 to Pred(DataSource.PropertyCount) do
    begin
      sName := DataSource.PropertyName[i];
      ADOConnection.Properties.Item[sName].Value := DataSource.Properties[sName];
    end;
  end;


  function TADOConnection.GetCommand(const aCommand: _Command; var aRef): Boolean;
  var
    i: Integer;
    cmd: TADOCustomCommand absolute aRef;
  begin
    cmd := NIL;

    for i := 0 to Pred(fCommands.Count) do
    begin
      if (aCommand = TCustomCommandHelper(fCommands[i]).ADOCommand) then
      begin
        cmd := TADOCustomCommand(fCommands[i]);
        BREAK;
      end;
    end;

    result := Assigned(cmd);
  end;


  procedure TADOConnection.NotifyCommand(const aCommand: TObject;
                                      const aOperation: TNotificationOperation);
  begin
    case aOperation of
      opAdd     : begin
                    TCustomCommandHelper(aCommand).ADOCommand.Set_ActiveConnection(ADOConnection);
                    fCommands.Add(aCommand);
                  end;
      opRemove  : begin
                    fCommands.Remove(aCommand);
                    TCustomCommandHelper(aCommand).ADOCommand.Set_ActiveConnection(Null);
                  end;
    end;
  end;


  procedure TADOConnection._OnExecuteComplete(      aSender: TObject;
                                                 aRecordsAffected: Integer;
                                           const aError: Error;
                                           var   adStatus: EventStatusEnum;
                                           const aCommand: _Command;
                                           const aRecordset: _Recordset;
                                           const aConnection: _Connection);
  var
    cmd: TADOCustomCommand;
  begin
    if (aConnection <> ADOConnection)
     or NOT GetCommand(aCommand, cmd) then
      EXIT;

    TCustomCommandHelper(cmd).DoComplete(aRecordsAffected);
  end;



  procedure TADOConnection.BeginTransaction;
  begin
    // Attempting to start a transaction when unwinding an aborted
    //  transaction is unwise, so we raise an Abort exception
    //  to prevent any such attempt
    if fTransactionAborted and (fTransactionDepth > 0) then
      ABORT;

    Inc(fTransactionDepth);

    if (fTransactionDepth = 1) then
    begin
      // Just to be on the safe side, ensure that the transaction
      //  abort flag is cleared since we are actually starting a
      //  whole new transaction at this very point
      fTransactionAborted := FALSE;
      ADOConnection.BeginTrans;
    end;
  end;


  procedure TADOConnection.Commit;
  begin
    Dec(fTransactionDepth);

    if fTransactionAborted then
      EXIT;

    if (fTransactionDepth = 0) then
      ADOConnection.CommitTrans;

    ASSERT(fTransactionDepth >= 0, 'Database transaction error - too many Commit or Rollback statements');
  end;


  procedure TADOConnection.Rollback;
  begin
    Dec(fTransactionDepth);

    ASSERT(fTransactionDepth >= 0, 'Database transaction error - too many Commit or Rollback statements');

    if fTransactionAborted then
      EXIT;

    ADOConnection.RollbackTrans;

    // We set the transaction aborted flag if we are still within
    //  a notional transaction, indicating that we are still unwinding
    //  that transaction.  If the depth is zero then there is nothing
    //  to unwind and we can clear the aborted flag
    fTransactionAborted := (fTransactionDepth > 0);
  end;




end.
