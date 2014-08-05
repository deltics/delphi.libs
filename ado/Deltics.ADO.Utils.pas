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

{$ifdef debug_Deltics_ADO_Utils}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.ADO.Utils;


interface

  uses
    ComObj,
    SysUtils,
    Deltics.ADO.TypeLibrary;


  type
    TNotificationOperation  = (
                               opAdd,
                               opRemove
                              );

    TADOType = (
                atUnknown,
                atEmpty,
                atSmallInt,
                atInteger,
                atSingle,
                atDouble,
                atCurrency,
                atDate,
                atBSTR,
                atIDispatch,
                atError,
                atBoolean,
                atVariant,
                atIUnknown,
                atDecimal,
                atTinyInt,
                atUnsignedTinyInt,
                atUnsignedSmallInt,
                atUnsignedInt,
                atBigInt,
                atUnsignedBigInt,
                atFileTime,
                atGUID,
                atBinary,
                atChar,
                atWChar,
                atNumeric,
                atUserDefined,
                atDBDate,
                atDBTime,
                atDBTimeStamp,
                atChapter,
                atPropVariant,
                atVarNumeric,
                atVarChar,
                atLongVarChar,
                atVarWChar,
                atLongVarWChar,
                atVarBinary,
                atLongVarBinary,

                atEmptyArray,
                atSmallIntArray,
                atIntegerArray,
                atSingleArray,
                atDoubleArray,
                atCurrencyArray,
                atDateArray,
                atBSTRArray,
                atIDispatchArray,
                atErrorArray,
                atBooleanArray,
                atVariantArray,
                atIUnknownArray,
                atDecimalArray,
                atTinyIntArray,
                atUnsignedTinyIntArray,
                atUnsignedSmallIntArray,
                atUnsignedIntArray,
                atBigIntArray,
                atUnsignedBigIntArray,
                atFileTimeArray,
                atGUIDArray,
                atBinaryArray,
                atCharArray,
                atWCharArray,
                atNumericArray,
                atUserDefinedArray,
                atDBDateArray,
                atDBTimeArray,
                atDBTimeStampArray,
                atChapterArray,
                atPropVariantArray,
                atVarNumericArray,
                atVarCharArray,
                atLongVarCharArray,
                atVarWCharArray,
                atLongVarWCharArray,
                atVarBinaryArray,
                atLongVarBinaryArray
               );

    TProviderType           = (
                               ptODBC,
                               ptJET,
                               ptMSOracle,
                               ptOracle,
                               ptSQLNativeClient,
                               ptSQLServer,
                               ptUnknown
                              );

    EADOException = class(EOleException)
    private
      fSQL: String;
    public
      constructor Create(const aMessage, aSQL: String); overload;
      constructor Create(const aOLEException: EOleException; const aSQL: String); overload;
      constructor CreateFmt(const aMessage: String; const aArgs: array of const; const aSQL: String); overload;
      procedure Assign(const aSource: EOleException);
      function MessageContains(const aSubString: String): Boolean;
      property SQL: String read fSQL;
    end;
    EADOExceptionClass = class of EADOException;

    EADONoCurrentRecord = class(EADOException);


    IProvider = interface
    ['{C89E4E68-CBDA-40EE-A944-F74CDEE271BA}']
    end;


  function ADOTypeFromDataType(const aDataType: TADOType): Integer;
  function ADOTypeToDataType(const aType: Integer): TADOType;

  function ADOTypeFromString(const aString: String): TADOType;
  function ADOTypeToString(const aDataType: TADOType): String;

  function ProviderFromType(const aProviderType: TProviderType): String;
  function ProviderName(const aProviderType: TProviderType): String;
  function ProviderToType(const aProvider: String): TProviderType;
  function ProviderTypeToStr(const aProviderType: TProviderType): String;


implementation

  uses
  { vcl: }
    TypInfo,
  { deltics: }
    Deltics.StrUtils;


  const
    ADO_TYPE  : array[TADOType] of Integer =
                 (
                  -1,
                  Deltics.ADO.TypeLibrary.adEmpty,
                  Deltics.ADO.TypeLibrary.adSmallInt,
                  Deltics.ADO.TypeLibrary.adInteger,
                  Deltics.ADO.TypeLibrary.adSingle,
                  Deltics.ADO.TypeLibrary.adDouble,
                  Deltics.ADO.TypeLibrary.adCurrency,
                  Deltics.ADO.TypeLibrary.adDate,
                  Deltics.ADO.TypeLibrary.adBSTR,
                  Deltics.ADO.TypeLibrary.adIDispatch,
                  Deltics.ADO.TypeLibrary.adError,
                  Deltics.ADO.TypeLibrary.adBoolean,
                  Deltics.ADO.TypeLibrary.adVariant,
                  Deltics.ADO.TypeLibrary.adIUnknown,
                  Deltics.ADO.TypeLibrary.adDecimal,
                  Deltics.ADO.TypeLibrary.adTinyInt,
                  Deltics.ADO.TypeLibrary.adUnsignedTinyInt,
                  Deltics.ADO.TypeLibrary.adUnsignedSmallInt,
                  Deltics.ADO.TypeLibrary.adUnsignedInt,
                  Deltics.ADO.TypeLibrary.adBigInt,
                  Deltics.ADO.TypeLibrary.adUnsignedBigInt,
                  Deltics.ADO.TypeLibrary.adFileTime,
                  Deltics.ADO.TypeLibrary.adGUID,
                  Deltics.ADO.TypeLibrary.adBinary,
                  Deltics.ADO.TypeLibrary.adChar,
                  Deltics.ADO.TypeLibrary.adWChar,
                  Deltics.ADO.TypeLibrary.adNumeric,
                  Deltics.ADO.TypeLibrary.adUserDefined,
                  Deltics.ADO.TypeLibrary.adDBDate,
                  Deltics.ADO.TypeLibrary.adDBTime,
                  Deltics.ADO.TypeLibrary.adDBTimeStamp,
                  Deltics.ADO.TypeLibrary.adChapter,
                  Deltics.ADO.TypeLibrary.adPropVariant,
                  Deltics.ADO.TypeLibrary.adVarNumeric,
                  Deltics.ADO.TypeLibrary.adVarChar,
                  Deltics.ADO.TypeLibrary.adLongVarChar,
                  Deltics.ADO.TypeLibrary.adVarWChar,
                  Deltics.ADO.TypeLibrary.adLongVarWChar,
                  Deltics.ADO.TypeLibrary.adVarBinary,
                  Deltics.ADO.TypeLibrary.adLongVarBinary,

                  $2000 + Deltics.ADO.TypeLibrary.adEmpty,
                  $2000 + Deltics.ADO.TypeLibrary.adSmallInt,
                  $2000 + Deltics.ADO.TypeLibrary.adInteger,
                  $2000 + Deltics.ADO.TypeLibrary.adSingle,
                  $2000 + Deltics.ADO.TypeLibrary.adDouble,
                  $2000 + Deltics.ADO.TypeLibrary.adCurrency,
                  $2000 + Deltics.ADO.TypeLibrary.adDate,
                  $2000 + Deltics.ADO.TypeLibrary.adBSTR,
                  $2000 + Deltics.ADO.TypeLibrary.adIDispatch,
                  $2000 + Deltics.ADO.TypeLibrary.adError,
                  $2000 + Deltics.ADO.TypeLibrary.adBoolean,
                  $2000 + Deltics.ADO.TypeLibrary.adVariant,
                  $2000 + Deltics.ADO.TypeLibrary.adIUnknown,
                  $2000 + Deltics.ADO.TypeLibrary.adDecimal,
                  $2000 + Deltics.ADO.TypeLibrary.adTinyInt,
                  $2000 + Deltics.ADO.TypeLibrary.adUnsignedTinyInt,
                  $2000 + Deltics.ADO.TypeLibrary.adUnsignedSmallInt,
                  $2000 + Deltics.ADO.TypeLibrary.adUnsignedInt,
                  $2000 + Deltics.ADO.TypeLibrary.adBigInt,
                  $2000 + Deltics.ADO.TypeLibrary.adUnsignedBigInt,
                  $2000 + Deltics.ADO.TypeLibrary.adFileTime,
                  $2000 + Deltics.ADO.TypeLibrary.adGUID,
                  $2000 + Deltics.ADO.TypeLibrary.adBinary,
                  $2000 + Deltics.ADO.TypeLibrary.adChar,
                  $2000 + Deltics.ADO.TypeLibrary.adWChar,
                  $2000 + Deltics.ADO.TypeLibrary.adNumeric,
                  $2000 + Deltics.ADO.TypeLibrary.adUserDefined,
                  $2000 + Deltics.ADO.TypeLibrary.adDBDate,
                  $2000 + Deltics.ADO.TypeLibrary.adDBTime,
                  $2000 + Deltics.ADO.TypeLibrary.adDBTimeStamp,
                  $2000 + Deltics.ADO.TypeLibrary.adChapter,
                  $2000 + Deltics.ADO.TypeLibrary.adPropVariant,
                  $2000 + Deltics.ADO.TypeLibrary.adVarNumeric,
                  $2000 + Deltics.ADO.TypeLibrary.adVarChar,
                  $2000 + Deltics.ADO.TypeLibrary.adLongVarChar,
                  $2000 + Deltics.ADO.TypeLibrary.adVarWChar,
                  $2000 + Deltics.ADO.TypeLibrary.adLongVarWChar,
                  $2000 + Deltics.ADO.TypeLibrary.adVarBinary,
                  $2000 + Deltics.ADO.TypeLibrary.adLongVarBinary
                 );


    PROVIDER_INFO : array[TProviderType] of record
                      Provider: String;
                      Name: String;
                    end = (
                            (Provider: 'MSDASQL';                 Name: 'Microsoft Provider for ODBC'),
                            (Provider: 'Microsoft.Jet.OLEDB.4.0'; Name: 'Microsoft Provider for Jet 4.0'),
                            (Provider: 'MSDAORA';                 Name: 'Microsoft Provider for Oracle'),
                            (Provider: 'OraOLEDB.Oracle';         Name: 'Oracle Provider'),
                            (Provider: 'SQLNCLI';                 Name: 'Microsoft SQL Server Native Client'),
                            (Provider: 'SQLOLEDB.1';              Name: 'Microsoft Provider SQL Server'),
                            (Provider: '(unknown)';               Name: 'Unknown Provider')
                          );


  function ADOTypeFromDataType(const aDataType: TADOType): Integer;
  begin
    result := ADO_TYPE[aDataType];
  end;


  function ADOTypeToDataType(const aType: Integer): TADOType;
  begin
    for result := Low(TADOType) to High(TADOType) do
      if ADO_TYPE[result] = aType then
        EXIT;

    result := atUnknown;
  end;


  function ADOTypeFromString(const aString: String): TADOType;
  begin
    result := TADOType(GetEnumValue(TypeInfo(TADOType), aString));
  end;


  function ADOTypeToString(const aDataType: TADOType): String;
  begin
    result := GetEnumName(TypeInfo(TADOType), Ord(aDataType));
  end;


  function ProviderFromType(const aProviderType: TProviderType): String;
  begin
    result := PROVIDER_INFO[aProviderType].Provider;
  end;


  function ProviderName(const aProviderType: TProviderType): String;
  begin
    result := PROVIDER_INFO[aProviderType].Name;
  end;


  function ProviderToType(const aProvider: String): TProviderType;
  begin
    for result := Low(TProviderType) to High(TProviderType) do
      if SameText(PROVIDER_INFO[result].Provider, aProvider) then
        EXIT;

    result := ptUnknown;
  end;


  function ProviderTypeToStr(const aProviderType: TProviderType): String;
  begin
    result := GetEnumName(TypeInfo(TProviderType), Ord(aProviderType));
  end;




{ EADOException }

  constructor EADOException.Create(const aMessage, aSQL: String);
  begin
    inherited Create(aMessage, 0, '', '', 0);

    fSQL := aSQL;
  end;


  constructor EADOException.Create(const aOLEException: EOleException;
                                   const aSQL: String);
  begin
    inherited Create(aOLEException.Message,
                     aOLEException.ErrorCode,
                     aOLEException.Source,
                     aOLEException.HelpFile,
                     aOLEException.HelpContext);

    fSQL := aSQL;
  end;


  constructor EADOException.CreateFmt(const aMessage: String;
                                      const aArgs: array of const;
                                      const aSQL: String);
  begin
    inherited Create(Format(aMessage, aArgs), 0, '', '', 0);

    fSQL := aSQL;
  end;


  function EADOException.MessageContains(const aSubString: String): Boolean;
  begin
    result := StrContainsText(Message, aSubString);
  end;


  procedure EADOException.Assign(const aSource: EOleException);
  begin
    self.HelpFile    := aSource.HelpFile;
    self.HelpContext := aSource.HelpContext;
    self.Message     := aSource.Message;
    self.ErrorCode   := aSource.ErrorCode;
    self.Source      := aSource.Source;

    if (aSource is EADOException) then
      self.fSQL := EADOException(aSource).SQL
    else
      self.fSQL := '';
  end;





end.
