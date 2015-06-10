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

{$i deltics.bonjour.inc}

{$ifdef deltics_bonjour_txt}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Bonjour.TXT;


interface

  uses
  { vcl: }
    Classes,
    Contnrs,
  { deltics: }
    Deltics.Strings,
  { bonjour: }
    Deltics.Bonjour,
    Deltics.Bonjour.API;


  type
    TKeyState = (
                 ksNoSuchKey,
                 ksNoValue,
                 ksEmptyValue,
                 ksHasValue
                );


    TTXT = class
    private
      function get_Count: Integer; virtual; abstract;
      function get_Key(const aIndex: Integer): String; virtual; abstract;
      function get_Text: UnicodeString; virtual; abstract;
      function get_Value(const aKey: String): UnicodeString; virtual; abstract;
      function get_ValueByIndex(const aIndex: Integer): UnicodeString;
      procedure set_Value(const aKey: String; const aValue: UnicodeString); virtual; abstract;
      procedure set_ValueByIndex(const aIndex: Integer; const aValue: UnicodeString);   protected
      procedure ValidateKey(const aKey: ASCIIString);
    public
      function Contains(const aKey: String): Boolean;
      function IndexOf(const aKey: String): Integer; virtual; abstract;
      procedure GetData(const aKey: String; var aData: Pointer);
      property Count: Integer read get_Count;
      property Keys[const aIndex: Integer]: String read get_Key;
      property Text: UnicodeString read get_Text;
      property ValueByIndex[const aIndex: Integer]: UnicodeString read get_ValueByIndex write set_ValueByIndex;
      property Values[const aKey: String]: UnicodeString read get_Value write set_Value; default;
    end;
    TTXTClass = class of TTXT;


    TTXTRecord = class(TTXT)
    private
      fHandle: TTXTRecordRef;
      fOnChange: TNotifyEvent;
      function get_Version: Integer;
      procedure InitRecord(const aVersion: Integer);
    protected
      function get_Count: Integer; override;
      function get_Data: PTXTRecordData; virtual;
      function get_Key(const aIndex: Integer): String; override;
      function get_Len: Integer; virtual;
      function get_Text: UnicodeString; override;
      function get_Value(const aKey: String): UnicodeString; override;
      procedure set_Value(const aKey: String; const aValue: UnicodeString); override;
      procedure DoChange;
      property OnChange: TNotifyEvent read fOnChange write fOnChange;
    public
      constructor Create(const aVersion: Integer = 0);
      destructor Destroy; override;
      procedure Clear;
      procedure Delete(const aKey: String);
      function IndexOf(const aKey: String): Integer; override;
      property Handle: TTXTRecordRef read fHandle;
      property Count: Integer read get_Count;
      property Data: PTXTRecordData read get_Data;
      property Len: Integer read get_Len;
      property Value[const aKey: String]: UnicodeString read get_Value write set_Value; default;
      property Version: Integer read get_Version;
    end;


    TTXTInfo = class(TTXT)
    private
      fKeys: array of String;
      fData: array of Pointer;
      fDataSize: array of Byte;
      fValues: array of UnicodeString;
    protected
      function get_Count: Integer; override;
      function get_Key(const aIndex: Integer): String; override;
      function get_Text: UnicodeString; override;
      function get_Value(const aKey: String): UnicodeString; override;
      procedure set_Value(const aKey: String; const aValue: UnicodeString); override;
    public
      constructor Create(const aData: PTXTRecordData; const aSize: Word);
      destructor Destroy; override;
      function IndexOf(const aKey: String): Integer; override;
    end;



implementation

  uses
  { vcl: }
    SysUtils,
    Windows;


  const
    TXTKEY_Version = 'txtvers';




{ TTXT ------------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXT.GetData(const aKey: String; var aData: Pointer);
  begin
    // TODO:
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXT.get_ValueByIndex(const aIndex: Integer): UnicodeString;
  begin
    result := Values[Keys[aIndex]];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXT.set_ValueByIndex(const aIndex: Integer;
                                  const aValue: UnicodeString);
  begin
    Values[Keys[aIndex]] := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXT.Contains(const aKey: String): Boolean;
  begin
    result := (IndexOf(aKey) <> -1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXT.ValidateKey(const aKey: ASCIIString);
  var
    i: Integer;
    c: ASCIIChar;
  begin
    for i := 1 to Length(aKey) do
    begin
      c := aKey[i];
      if NOT (c in [#32..#127]) then
        raise EBonjour.CreateFmt('''%s'' is not a valid TXT key (''%s'' at position %d is invalid)', [c, aKey, i]);
    end;
  end;






{ TTXTRecord ------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTXTRecord.Create(const aVersion: Integer);
  begin
    inherited Create;

    InitRecord(aVersion);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTXTRecord.Destroy;
  begin
    TXTRecordDeallocate(fHandle);
    ZeroMemory(@fHandle, Sizeof(fHandle));
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXTRecord.InitRecord(const aVersion: Integer);
  begin
    TXTRecordCreate(fHandle, 0, NIL);

    if (aVersion > 0) then
      Values[TXTKEY_Version] := IntToStr(aVersion);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXTRecord.DoChange;
  begin
    if Assigned(fOnChange) then
      fOnChange(self);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTRecord.get_Count: Integer;
  begin
    result := TXTRecordGetCount(Len, Data);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTRecord.get_Data: PTXTRecordData;
  begin
    result := TXTRecordGetBytesPtr(Handle);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTRecord.get_Len: Integer;
  begin
    result := TXTRecordGetLength(Handle);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTRecord.get_Text: UnicodeString;
  begin
    // TODO:
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTRecord.get_Value(const aKey: String): UnicodeString;
  var
    key: ASCIIString;
    value: PUTF8Char;
    len: Byte;
  begin
    result  := '';

    key   := ASCII.Encode(aKey);
    value := TXTRecordGetValuePtr(Len, Data, @aKey[1], len);

    if Assigned(value) then
      result := WIDE.FromUTF8(value, len + 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTRecord.get_Version: Integer;
  begin
    if Contains(TXTKEY_Version) then
      result := StrToIntDef(Values[TXTKEY_Version], 0)
    else
      result := -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTRecord.IndexOf(const aKey: String): Integer;
  begin
    for result := 0 to Pred(Count) do
      if SameText(Keys[result], aKey) then
        EXIT;

    result := -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTRecord.get_Key(const aIndex: Integer): String;
  var
    key: array[0..255] of ASCIIChar;
    unusedValueLen: Byte;
    unusedValueBuf: PUTF8Char;
  begin
    CheckResult(TXTRecordGetItemAtIndex(Len, Data, aIndex, Length(key), @key,
                                        unusedValueLen, unusedValueBuf));
  {$ifdef UNICODE}
    result := UnicodeString(PANSIChar(@key[0]));
  {$else}
    result := StrPas(PANSIChar(@key[0]));
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXTRecord.set_Value(const aKey: String;
                                 const aValue: UnicodeString);
  var
    key: ASCIIString;
    value: UTF8String;
  begin
    key := ASCII.Encode(aKey);
    ValidateKey(key);

    value := UTF8.FromWide(aValue);
    TXTRecordSetValue(Handle,
                      Pointer(key),
                      Length(value),
                      PUTF8Char(value));
    DoChange;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXTRecord.Clear;
  var
    ver: Integer;
  begin
    ver := Version;

    TXTRecordDeallocate(fHandle);
    ZeroMemory(@fHandle, sizeof(fHandle));

    InitRecord(ver);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXTRecord.Delete(const aKey: String);
  var
    key: ASCIIString;
  begin
    key := ASCII.Encode(aKey);
    TXTRecordRemoveValue(Handle, Pointer(key));
  end;







{ TTXTInfo --------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTXTInfo.Create(const aData: PTXTRecordData;
                              const aSize: Word);
  var
    i: Integer;
    idx: Integer;
    count: Integer;
    keybuf: array[0..255] of UTF8Char;
    key: PUTF8Char;
    value: PUTF8Char;
  begin
    inherited Create;

    key   := @keybuf[0];
    count := TXTRecordGetCount(aSize, aData);

    SetLength(fKeys,      count);
    SetLength(fData,      count);
    SetLength(fDataSize,  count);
    SetLength(fValues,    count);

    i := 0;
    try
      for idx := 0 to Pred(count) do
      begin
        CheckResult(TXTRecordGetItemAtIndex(aSize, aData, idx,
                                            Length(keybuf), key,
                                            fDataSize[i], value));

        fKeys[i] := STR.FromUTF8(key);
        GetMem(fData[i], fDataSize[i] + 1);
        CopyMemory(fData[i], value, fDataSize[i] + 1);

        Inc(i);
      end;

    finally
      SetLength(fKeys,      i);
      SetLength(fData,      i);
      SetLength(fDataSize,  i);
      SetLength(fValues,    i);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTXTInfo.Destroy;
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(fKeys)) do
      FreeMem(fData[i], fDataSize[i]);

    SetLength(fKeys,      0);
    SetLength(fData,      0);
    SetLength(fDataSize,  0);
    SetLength(fValues,    0);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTInfo.get_Count: Integer;
  begin
    result := Length(fKeys);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTInfo.get_Key(const aIndex: Integer): String;
  begin
    result := fKeys[aIndex];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTInfo.get_Text: UnicodeString;
  begin
//    result := fData.Text;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTInfo.get_Value(const aKey: String): UnicodeString;
  var
    idx: Integer;
  begin
    idx := IndexOf(aKey);
    if idx = -1 then
      raise EBonjour.CreateFmt('No such key ''%s'' in TXT record', [aKey]);

    if (fValues[idx] = '') then
      fValues[idx] := WIDE.FromUTF8(fData[idx], fDataSize[idx]);

    result := fValues[idx];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTXTInfo.IndexOf(const aKey: String): Integer;
  begin
    for result := 0 to Pred(Count) do
      if SameText(fKeys[result], aKey) then
        EXIT;

    result := -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTXTInfo.set_Value(const aKey: String;
                               const aValue: UnicodeString);
  begin
    ASSERT(FALSE, 'Cannot modify a remote service TXT record');
  end;



end.
