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

{$ifdef debug_Deltics_ADO_Providers}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Providers;


interface

  uses
  { vcl: }
    Contnrs,
    Registry,
  { deltics: }
    Deltics.ADO.Properties,
    Deltics.JSON;


  type
    TOLEDBServices  = (
                       odsAllServices,
                       odsNoServices,
                       odsNoAggregation,
                       odsPoolingDisabled,
                       odsPoolingAndEnlistmentDisabled,
                       odsClientCursorDisabled,
                       odsPoolingEnlistmentAndClientCursorDisabled
                      );


    TADOProvider = class
    private
      fJSON: TJSONObject;
      fCLSID: TGUID;
      fInprocServer32: String;
      fName: String;
      fProgID: String;
      fProperties: TADOPropertyList;
      fVersionIndependentProgID: String;
      function get_AsJSON: TJSONObject;
      function get_DisplayName: String;
      function get_Properties: TADOPropertyList;
      function get_ProviderID: String;
      procedure set_AsJSON(const aValue: TJSONObject);
    protected
      constructor Create(const aRegistry: TRegistry;
                         const aCLSID: TGUID); overload;
    public
      constructor Create(const aJSON: TJSONObject); overload;
      destructor Destroy; override;
      property AsJSON: TJSONObject read get_AsJSON write set_AsJSON;
      property CLSID: TGUID read fCLSID;
      property DisplayName: String read get_DisplayName;
      property InprocServer32: String read fInprocServer32;
      property Name: String read fName;
      property ProgID: String read fProgID;
      property Properties: TADOPropertyList read get_Properties;
      property ProviderID: String read get_ProviderID;
      property VersionIndependentProgID: String read fVersionIndependentProgID;
    end;


    TADOProviderList = class
    private
      fJSON: TJSONArray;
      fItems: TObjectList;
      function get_AsJSON: TJSONArray;
      function get_Count: Integer;
      function get_Item(const aIndex: Integer): TADOProvider;
      procedure set_AsJSON(const aValue: TJSONArray);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(const aProvider: TADOProvider);
      function FindProvider(const aCLSID: TGUID; var aProvider: TADOProvider): Boolean; overload;
      function FindProvider(const aProgID: String; var aProvider: TADOProvider): Boolean; overload;
      function IsValidProgID(const aID: String): Boolean;
      procedure Load(const aServices: TOLEDBServices = odsAllServices);
      property AsJSON: TJSONArray read get_AsJSON write set_AsJSON;
      property Count: Integer read get_Count;
      property Items[const aIndex: Integer]: TADOProvider read get_Item; default;
    end;




implementation

  uses
  { vcl: }
    Classes,
    SysUtils,
    Windows,
  { deltics: }
    Deltics.ADO.TypeLibrary,
    Deltics.SysUtils;


  const
    ENUM_CLSID                    = 'CLSID';

    KEY_CLSID                     = 'CLSID\%s';
    KEY_CLSID_Key                 = 'CLSID\%s\%s';
    KEY_OLEDB_Provider            = 'OLE DB Provider';
    KEY_InprocServer32            = 'InprocServer32';
    KEY_ProgID                    = 'ProgID';
    KEY_VersionIndependentProgID  = 'VersionIndependentProgID';

    VAL_OLEDB_SERVICES  = 'OLEDB_SERVICES';

    JSON_CLSID                    = 'clsid';
    JSON_InprocServer32           = 'inprocserver32';
    JSON_Name                     = 'name';
    JSON_ProgID                   = 'progid';
    JSON_VersionIndependentProgID = 'versionindependentprogid';




{ TADOProvider ----------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADOProvider.Create(const aRegistry: TRegistry;
                                  const aCLSID: TGUID);

    function ReadKeyDefault(const aKey: String): String;
    const
      DEFAULT = '';
    begin
      if aRegistry.OpenKey(Format(KEY_CLSID_Key, [GUIDToString(aCLSID), aKey]), FALSE) then
      begin
        result := aRegistry.ReadString(DEFAULT);
        aRegistry.CloseKey;
      end
      else
        result := '';
    end;

  begin
    inherited Create;

    fCLSID  := aCLSID;

    fName                     := ReadKeyDefault(KEY_OLEDB_Provider);
    fInprocServer32           := ReadKeyDefault(KEY_InprocServer32);
    fProgID                   := ReadKeyDefault(KEY_ProgID);
    fVersionIndependentProgID := ReadKeyDefault(KEY_VersionIndependentProgID);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADOProvider.Create(const aJSON: TJSONObject);
  begin
    inherited Create;

    AsJSON := aJSON;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TADOProvider.Destroy;
  begin
    FreeAndNIL(fProperties);
    FreeAndNIL(fJSON);
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProvider.get_AsJSON: TJSONObject;
  begin
    if NOT Assigned(fJSON) then
    begin
      fJSON := TJSONObject.Create;

      fJSON.Add(JSON_CLSID,           CLSID);
      fJSON.Add(JSON_InprocServer32,  InprocServer32);
      fJSON.Add(JSON_Name,            Name);
      fJSON.Add(JSON_ProgID,          ProgID);

      fJSON.Add(JSON_VersionIndependentProgID, VersionIndependentProgID);
    end;

    result := fJSON;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProvider.get_DisplayName: String;
  begin
    result := Name;
    if (result = '') then
      result := ProviderID;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProvider.get_Properties: TADOPropertyList;
  var
    cnc: Connection;
  begin
    if NOT Assigned(fProperties) then
    begin
      cnc := CoConnection.Create;
      cnc.Provider := ProgID;

      fProperties := TADOPropertyList.Create(cnc.Properties);
    end;

    result := fProperties;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProvider.get_ProviderID: String;
  begin
    result := VersionIndependentProgID;
    if (result = '') then
      result := ProgID;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOProvider.set_AsJSON(const aValue: TJSONObject);
  begin
    fCLSID          := aValue[JSON_CLSID].AsGUID;
    fInprocServer32 := aValue[JSON_InprocServer32].AsString;
    fName           := aValue[JSON_Name].AsString;
    fProgID         := aValue[JSON_ProgID].AsString;

    fVersionIndependentProgID := aValue[JSON_VersionIndependentProgID].AsString;

    FreeAndNIL(fJSON);
  end;











{ TADOProviderList ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TADOProviderList.Create;
  begin
    inherited Create;

    fItems := TObjectList.Create(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TADOProviderList.Destroy;
  begin
    FreeAndNIL(fItems);
    FreeAndNIL(fJSON);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProviderList.get_AsJSON: TJSONArray;
  var
    i: Integer;
  begin
    if NOT Assigned(fJSON) then
    begin
      fJSON := TJSONArray.Create;

      for i := 0 to Pred(Count) do
        fJSON.Add(Items[i].AsJSON);
    end;

    result := fJSON;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProviderList.get_Count: Integer;
  begin
    result := fItems.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProviderList.get_Item(const aIndex: Integer): TADOProvider;
  begin
    result := TADOProvider(fItems[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOProviderList.set_AsJSON(const aValue: TJSONArray);
  var
    i: Integer;
  begin
    FreeAndNIL(fJSON);

    fItems.Clear;

    for i := 0 to Pred(aValue.Count) do
      fItems.Add(TADOProvider.Create(aValue[i] as TJSONObject));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProviderList.FindProvider(const aCLSID: TGUID;
                                         var aProvider: TADOProvider): Boolean;
  var
    i: Integer;
  begin
    aProvider := NIL;

    for i := 0 to Pred(Count) do
    begin
      aProvider := Items[i];

      if SameGUID(aProvider.CLSID, aCLSID) then
        BREAK;

      aProvider := NIL;
    end;

    result := Assigned(aProvider);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProviderList.FindProvider(const aProgID: String;
                                         var aProvider: TADOProvider): Boolean;
  var
    i: Integer;
  begin
    aProvider := NIL;

    for i := 0 to Pred(Count) do
    begin
      aProvider := Items[i];

      if ANSISameText(aProvider.VersionIndependentProgID, aProgID)
        or ANSISameText(aProvider.ProgID, aProgID) then
        BREAK;

      aProvider := NIL;
    end;

    result := Assigned(aProvider);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TADOProviderList.IsValidProgID(const aID: String): Boolean;
  var
    notUsed: TADOProvider;
  begin
    result := FindProvider(aID, notUsed);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOProviderList.Add(const aProvider: TADOProvider);
  begin
    FreeAndNIL(fJSON);
    fItems.Add(aProvider);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TADOProviderList.Load(const aServices: TOLEDBServices);
  var
    i: Integer;
    reg: TRegistry;
    clsid: TStringList;
  begin
    FreeAndNIL(fJSON);

    reg := TRegistry.Create(KEY_READ);
    try
      reg.RootKey := HKEY_CLASSES_ROOT;

      clsid := TStringList.Create;
      try
        if NOT reg.OpenKey(ENUM_CLSID, FALSE) then
          EXIT;

        reg.GetKeyNames(clsid);
        reg.CloseKey;

        for i := 0 to Pred(clsid.Count) do
        begin
          if reg.OpenKey(Format(KEY_CLSID, [clsid[i]]), FALSE) then
            if reg.KeyExists(KEY_OLEDB_Provider) or reg.ValueExists(VAL_OLEDB_SERVICES) then
            begin
              reg.CloseKey;
              fItems.Add(TADOProvider.Create(reg, StringToGUID(clsid[i])))
            end
            else
              reg.CloseKey;
        end;

      finally
        clsid.Free;
      end;
    finally
      reg.Free;
    end;
  end;















end.
