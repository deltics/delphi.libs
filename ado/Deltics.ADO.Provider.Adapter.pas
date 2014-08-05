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

{$ifdef debug_Deltics_ADO_Provider_Adapter}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Provider.Adapter;


interface

  uses
  { vcl: }
    Classes,
  { deltics: }
    Deltics.Classes,
  { deltics.iADO: }
    Deltics.ADO.Command,
    Deltics.ADO.Utils;


  type
    TADOProviderAdapter = class
    private
      fProvider: String;
    public
      constructor Create(const aProvider: String); overload;
      constructor Create(const aProviderType: TProviderType); overload;
      function Format(const aCommand: TADOStoredProcedure): String; overload; virtual;
      property Provider: String read fProvider;
    end;


    TJETAdapter = class(TADOProviderAdapter)
    public
      function Format(const aCommand: TADOStoredProcedure): String; override;
    end;


    TSQLServerAdapter = class(TADOProviderAdapter);


    TODBCAdapter = class(TADOProviderAdapter)
    public
      function Format(const aCommand: TADOStoredProcedure): String; override;
    end;



  function GetProviderAdapter(const aProvider: String; var aAdapter: TADOProviderAdapter): Boolean;



implementation

  uses
  { vcl: }
    Contnrs,
    SysUtils;


  var
    _Adapters: TObjectList;


  function GetProviderAdapter(const aProvider: String; var aAdapter: TADOProviderAdapter): Boolean;
  var
    i: Integer;
    name: String;
  begin
    result := FALSE;

    name := Trim(aProvider);
    for i := 0 to Pred(_Adapters.Count) do
    begin
      aAdapter := TADOProviderAdapter(_Adapters[i]);
      result := SameText(aAdapter.Provider, name);
      if result then
        EXIT;
    end;

    aAdapter := NIL;
  end;




{ TProviderAdapter }

  constructor TADOProviderAdapter.Create(const aProvider: String);
  var
    adapter: TADOProviderAdapter;
  begin
    if GetProviderAdapter(aProvider, adapter) then
      raise EInvalidOperation.CreateFmt('Adapter already created for ADO provider %s', [aProvider]);

    inherited Create;
    fProvider := aProvider;

    _Adapters.Add(self);
  end;


  constructor TADOProviderAdapter.Create(const aProviderType: TProviderType);
  begin
    ASSERT(aProviderType <> ptUnknown);
    Create(ProviderFromType(aProviderType));
  end;


  function TADOProviderAdapter.Format(const aCommand: TADOStoredProcedure): String;
  begin
    result := aCommand.ProcedureName + '(' + aCommand.ParamString + ')';
  end;





{ TJETAdapter }

  function TJETAdapter.Format(const aCommand: TADOStoredProcedure): String;
  begin
    result := aCommand.ProcedureName + ' ' + aCommand.ParamString;
  end;





{ TODBCAdapter }

  function TODBCAdapter.Format(const aCommand: TADOStoredProcedure): String;
  begin
    result := '{call ' + aCommand.ProcedureName + '(' + aCommand.ParamString + ')}';
  end;




initialization
  _Adapters := TObjectList.Create(TRUE);

  TJETAdapter.Create(ptJET);
  TSQLServerAdapter.Create(ptSQLServer);
  TODBCAdapter.Create(ptODBC);

finalization
  _Adapters.Free;

end.
