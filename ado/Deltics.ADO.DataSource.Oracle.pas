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

{$ifdef debug_Deltics_ADO_DataSource_Oracle}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.DataSource.Oracle;


interface

  uses
  { vcl: }
    Classes,
  { deltics: }
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.Datasource,
    Deltics.ADO.Utils;


  type
    TOracleDataSource = class(TADOCustomDataSource)
    private
      function get_Database: String;
      procedure set_Database(const aValue: String);
    protected
      function get_ProviderType: TProviderType; override;
    public
      property Database: String read get_Database write set_Database;
    end;




implementation

  const
    // Names of connection string values
    SQLCS_Database  = 'Data Source';


    
{ TOracleDataSource }

  function TOracleDataSource.get_Database: String;
  begin
    result := ConnectionString[SQLCS_Database];
  end;


  function TOracleDataSource.get_ProviderType: TProviderType;
  begin
    result := ptOracle;
  end;


  procedure TOracleDataSource.set_Database(const aValue: String);
  begin
    ConnectionString[SQLCS_Database] := aValue;
  end;



end.
