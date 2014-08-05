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

{$ifdef debug_Deltics_ADO_DataSource_JET}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.DataSource.JET;


interface

  uses
  { vcl: }
    Classes,
    ComObj,
    SysUtils,
  { deltics: }
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.DataSource,
    Deltics.ADO.Utils;


  type
    TMicrosoftJetDataSource = class(TADOCustomDataSource)
    private
      function get_Database: String;
      function get_DatabasePassword: String;
      function get_SystemDatabase: String;
      procedure set_Database(const aValue: String);
      procedure set_DatabasePassword(const aValue: String);
      procedure set_SystemDatabase(const aValue: String);
    protected
      function get_ProviderType: TProviderType; override;
      function ExceptionClassForError(const aErrorCode: Integer): EADOExceptionClass; override;
    public
      property Database: String read get_Database write set_Database;
      property DatabasePassword: String read get_DatabasePassword write set_DatabasePassword;
      property SystemDatabase: String read get_SystemDatabase write set_SystemDatabase;
    end;


    EJetException = class(EADOException);
    EJetInvalidPassword = class(EJetException);
    EJetNeedWorkgroupFile = class(EJetException);

    TJetExceptionClass = class of EJetException;

    

implementation

  const
    // Names of connection string values
    JETCS_Database  = 'Data Source';

    // Names of properties
    JETPROP_SystemDatabase    = 'Jet OLEDB:System database';
    JETPROP_DatabasePassword  = 'Jet OLEDB:Database Password';



{ TMicrosoftJetDataSource }

  function TMicrosoftJetDataSource.get_Database: String;
  begin
    result := ConnectionString[JETCS_Database];
  end;


  function TMicrosoftJetDataSource.get_DatabasePassword: String;
  begin
    result := Properties[JETPROP_DatabasePassword];
  end;


  function TMicrosoftJetDataSource.get_ProviderType: TProviderType;
  begin
    result := ptJET;
  end;


  function TMicrosoftJetDataSource.get_SystemDatabase: String;
  begin
    result := Properties[JETPROP_SystemDatabase];
  end;


  function TMicrosoftJetDataSource.ExceptionClassForError(const aErrorCode: Integer): EADOExceptionClass;
  begin
    case aErrorCode of
      5003028: result := EJetNeedWorkgroupFile;
      5003031: result := EJetInvalidPassword;
    else
      result := inherited ExceptionClassForError(aErrorCode);
    end;
  end;



  procedure TMicrosoftJetDataSource.set_Database(const aValue: String);
  begin
    ConnectionString[JETCS_Database] := aValue;
  end;


  procedure TMicrosoftJetDataSource.set_DatabasePassword(const aValue: String);
  begin
    Properties[JETPROP_DatabasePassword] := aValue;
  end;


  procedure TMicrosoftJetDataSource.set_SystemDatabase(const aValue: String);
  begin
    Properties[JETPROP_SystemDatabase] := aValue;
  end;



end.
