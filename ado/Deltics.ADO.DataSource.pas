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

{$ifdef debug_Deltics_ADO_DataSource}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.DataSource;


interface

  uses
    Classes,
    ComObj,
    Deltics.Classes,
    Deltics.MultiCast,
    Deltics.ADO.ConnectionString,
    Deltics.ADO.TypeLibrary,
    Deltics.ADO.Utils;


  type
    TADODataSource = class;
    TADOCustomDataSource = class;


    TADODataSource = class(TInterfacedObject)
    private
      fConnectionString: TADOConnectionString;
      fProperties: TStringList;
      function get_Property(const aName: String): String;
      function get_PropertyCount: Integer;
      function get_PropertyName(const aIndex: Integer): String;
      procedure set_Property(const aName, aValue: String);
      procedure DoProviderChanged;
    protected
      function get_Provider: String; virtual;
      function get_ProviderType: TProviderType; virtual; abstract;
      procedure set_Provider(const aValue: String); virtual;
      procedure set_ProviderType(const aValue: TProviderType); virtual;
      function ExceptionClassForError(const aErrorCode: Integer): EADOExceptionClass; virtual;
    protected
      property ProviderType: TProviderType read get_ProviderType write set_ProviderType;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AfterConstruction; override;
      function TranslateException(const aException: EOleException;
                                  const aSQL: String = ''): EADOException;
      property ConnectionString: TADOConnectionString read fConnectionString;
      property Properties[const aName: String]: String read get_Property write set_Property;
      property PropertyCount: Integer read get_PropertyCount;
      property PropertyName[const aIndex: Integer]: String read get_PropertyName;
      property Provider: String read get_Provider write set_Provider;
    end;


    TADOCustomDataSource = class(TADODataSource)
    private
      fProvider: String;
    protected
      function get_Provider: String; override;
      function get_ProviderType: TProviderType; override;
      procedure set_Provider(const aValue: String); override;
      procedure set_ProviderType(const aValue: TProviderType); override;
    public
      property ProviderType;
    end;



implementation

  uses
    SysUtils,
    Deltics.SysUtils;


{ TADODataSource --------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADODataSource.Create;
  begin
    inherited Create;

    fConnectionString   := TADOConnectionString.Create;
    fProperties         := TStringList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADODataSource.AfterConstruction;
  begin
    inherited;
    DoProviderChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TADODataSource.Destroy;
  begin
    FreeAndNIL(fConnectionString);
    FreeAndNIL(fProperties);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADODataSource.DoProviderChanged;
  begin
    ConnectionString['Provider'] := Provider;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADODataSource.get_Property(const aName: String): String;
  begin
    result := fProperties.Values[aName];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADODataSource.get_PropertyCount: Integer;
  begin
    result := fProperties.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADODataSource.get_PropertyName(const aIndex: Integer): String;
  begin
    result := fProperties.Names[aIndex];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADODataSource.get_Provider: String;
  begin
    result := ProviderFromType(ProviderType);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADODataSource.ExceptionClassForError(const aErrorCode: Integer): EADOExceptionClass;
  begin
    result := EADOException;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADODataSource.TranslateException(const aException: EOleException;
                                             const aSQL: String): EADOException;
  var
    exceptionClass: EADOExceptionClass;
  begin
    exceptionClass := ExceptionClassForError(aException.HelpContext);

    if NOT Assigned(exceptionClass) then
      exceptionClass := EADOException;

    result := exceptionClass.Create(aException, aSQL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADODataSource.set_Property(const aName, aValue: String);
  var
    value: String;
  begin
    // TODO: Cannot change properties with active connections

    value := Trim(aValue);
    fProperties.Values[aName] := value;

    // If value is '' then thanks to the way TStringList works we just
    //  removed the value (!) so we now need to explicitly add a 'Name='
    //  entry

    if (value = '') then
      fProperties.Add(aName + '=');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADODataSource.set_Provider(const aValue: String);
  begin
    raise Exception.CreateFmt('Cannot change the Provider of a %s', [ClassName]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADODataSource.set_ProviderType(const aValue: TProviderType);
  begin
    raise Exception.CreateFmt('Cannot change the ProviderType of a %s', [ClassName]);
  end;







{ TADOCustomDataSource --------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOCustomDataSource.get_Provider: String;
  begin
    result := fProvider;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOCustomDataSource.get_ProviderType: TProviderType;
  begin
    result := ProviderToType(Provider);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOCustomDataSource.set_Provider(const aValue: String);
  begin
    if (Provider = aValue) then
      EXIT;

    // TODO: Cannot change provider with active connections

    fProvider := aValue;
    DoProviderChanged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOCustomDataSource.set_ProviderType(const aValue: TProviderType);
  begin
    if (ProviderType = aValue) then
      EXIT;

    // TODO: Cannot change provider with active connections

    if (aValue = ptUnknown) then
      Provider := ''
    else
      Provider := ProviderFromType(aValue);
  end;






end.
