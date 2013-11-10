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

{$i deltics.rtl.inc}

{$ifdef deltics_sysutils}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.SysUtils;


interface

  uses
  { vcl: }
    Classes,
    SysUtils,
  {$ifdef MSWINDOWS}
    TlHelp32,
  {$endif}
  { deltics: }
    Deltics.Strings,
    Deltics.Types;


  const
    EmptyStr: String = '';
    NullGUID: TGUID = '{00000000-0000-0000-0000-000000000000}';



  type
    IAutoFree = interface
    ['{9C3B2944-EA08-4301-A6E1-C0EB94D54771}']
      procedure Add(const aReferences: array of PObject); overload;
      procedure Add(const aObjects: array of TObject); overload;
    end;

    Exception = SysUtils.Exception;
    ENotImplemented = class(Exception)
    public
      constructor Create(const aClass: TClass; const aSignature: String); overload;
      constructor Create(const aObject: TObject; const aSignature: String); overload;
    end;

    EAccessDenied = class(EOSError);


    TGUIDFormat = (
                   gfDefault,
                   gfNoBraces,
                   gfNoHyphens,
                   gfDigitsOnly
                  );

    TRoundingStrategy = (
                         rsDefault,
                         rsAwayFromZero,
                         rsTowardsZero
                        );

    TComponentProc = procedure(const aComponent: TComponent);


    TriBoolean = (
                  tbUnknown,
                  tbTRUE,
                  tbFALSE
                 );


  procedure CloneList(const aSource: TList; const aDest: TList);

  function AutoFree(const aReference: PObject): IUnknown; overload;
  function AutoFree(const aReferences: array of PObject): IUnknown; overload;

  procedure FreeAndNIL(var aObject); overload;
  procedure FreeAndNIL(var aPointer; const aSize: Cardinal); overload;
  procedure FreeAndNIL(const aObjects: array of PObject); overload;
  procedure NILRefs(const aObjects: array of PObject);

  function IfThen(aValue: Boolean; aTrue, aFalse: Boolean): Boolean; overload;
  function IfThen(aValue: Boolean; aTrue, aFalse: TObject): TObject; overload;
  function IfThen(aValue: Boolean; aTrue, aFalse: Integer): Integer; overload;
  function IfThen(aValue: Boolean; aTrue, aFalse: ANSIString): ANSIString; overload;
  function IfThen(aValue: Boolean; aTrue, aFalse: UnicodeString): UnicodeString; overload;
  function IfThenInt(aValue: Boolean; aTrue, aFalse: Integer): Integer;

  function StringIndex(const aString: String; const aCases: array of String): Integer;
  function TextIndex(const aString: String; const aCases: array of String): Integer;

  function Min64(ValueA, ValueB: Int64): Int64; 
  function Min(ValueA, ValueB: Cardinal): Cardinal; overload;
  function Min(ValueA, ValueB: Integer): Integer; overload;

  function Max64(ValueA, ValueB: Int64): Int64;
  function Max(ValueA, ValueB: Cardinal): Cardinal; overload;
  function Max(ValueA, ValueB: Integer): Integer; overload;

  procedure Exchange(var A, B; aSize: LongWord = 4);

  function GUIDToString(const aGUID: TGUID; const aFormat: TGUIDFormat = gfDefault): String;
  function StringToGUID(const aString: String): TGUID;
  function TryStringToGUID(const aString: String; var aGUID: TGUID): Boolean;

  function IsNull(const aValue: TGUID): Boolean;
  function NewGUID: TGUID;
  function SameGUID(const GUIDA, GUIDB: TGUID): Boolean;

  procedure AddTrailingBackslash(var aString: String);
  procedure RemoveTrailingBackslash(var aString: String);

  function BinToHex(const aBuf: Pointer; const aSize: Integer): String;
  function HexToBin(const aString: String; var aSize: Integer): Pointer; overload;
  procedure HexToBin(const aString: String; var aBuf; const aSize: Integer); overload;
  procedure FillZero(var aDest; const aSize: Integer); overload;
  function ReverseBytes(const aValue: Word): Word; overload;
  function ReverseBytes(const aValue: LongWord): LongWord; overload;

  function Round(const aValue: Extended;
                 const aStrategy: TRoundingStrategy = rsDefault): Integer;


  procedure ForEachComponent(const aComponent: TComponent;
                             const aProc: TComponentProc;
                             const aRecursive: Boolean = TRUE;
                             const aClass: TComponentClass = NIL);


{$ifdef MSWINDOWS}
  function Exec(const aEXE: String; const aCommandLine: String = ''): Cardinal;
  procedure ExecAndWait(const aEXE: String; const aCommandLine: String = '');
  function FindProcess(const aEXEName: String; var aProcess: TProcessEntry32): Boolean;
  function ProcessExists(const aEXEName: String): Boolean;
  function RegisterDLL(const aFileName: String; const aRegistrationProc: String = 'Register'): Boolean;
{$endif}

  function IsTRUE(const aTri: TriBoolean): Boolean;
  function IsFALSE(const aTri: TriBoolean): Boolean;
  function IsKnown(const aTri: TriBoolean): Boolean;




implementation

  uses
  { vcl: }
    Contnrs,
    ActiveX,
    Math,
    Windows,
  { deltics: }
    Deltics.Windows;



{ ------------------------------------------------------------------------------------------------ }

  type
    TAutoFree = class(TInterfacedObject, IUnknown,
                                         IAutoFree)
    private
      fObjects: TObjectList;
      fReferences: TList;
    public
      constructor Create; overload;
      constructor Create(const aObjects: array of TObject); overload;
      constructor CreateByRef(const aReferences: array of PObject;
                              const aInitialise: Boolean);
      destructor Destroy; override;
      procedure Add(const aReferences: array of PObject); overload;
      procedure Add(const aObjects: array of TObject); overload;
    end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TAutoFree.Create;
  begin
    inherited Create;

    fObjects    := TObjectList.Create(TRUE);
    fReferences := TList.Create;
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TAutoFree.CreateByRef(const aReferences: array of PObject;
                                    const aInitialise: Boolean);
  var
    i: Integer;
  begin
    Create;

    Add(aReferences);

    if aInitialise then
      for i := 0 to Pred(fReferences.Count) do
        Pointer(fReferences[i]^) := NIL;
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TAutoFree.Create(const aObjects: array of TObject);
  begin
    Create;

    Add(aObjects);
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TAutoFree.Destroy;
  var
    i: Integer;
  begin
    for i := 0 to Pred(fReferences.Count) do
      FreeAndNIL(fReferences[i]^);

    FreeAndNIL(fReferences);
    FreeAndNIL(fObjects);

    inherited;
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TAutoFree.Add(const aReferences: array of PObject);
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(aReferences)) do
      if (fReferences.IndexOf(aReferences[i]) = -1) then
        fReferences.Add(aReferences[i]);
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TAutoFree.Add(const aObjects: array of TObject);
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(aObjects)) do
      if Assigned(aObjects[i]) and (fObjects.IndexOf(aObjects[i]) = -1) then
        fObjects.Add(aObjects[i]);
  end;







{ ------------------------------------------------------------------------------------------------ }



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure CloneList(const aSource: TList; const aDest: TList);
  begin
    aDest.Count := aSource.Count;
    Move(PByte(aSource.List)^, PByte(aDest.List)^, (aSource.Count * sizeof(Pointer)));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function AutoFree(const aReference: PObject): IUnknown;
  begin
    result := TAutoFree.CreateByRef([aReference], FALSE);
  end;

  function AutoFree(const aReferences: array of PObject): IUnknown;
  begin
    result := TAutoFree.CreateByRef(aReferences, FALSE);
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure FreeAndNIL(var aObject); overload;
  begin
    SysUtils.FreeAndNIL(aObject);
  end;

  procedure FreeAndNIL(var aPointer; const aSize: Cardinal); overload;
  var
    ptr: Pointer;
  begin
    ptr := Pointer(aPointer);

    if NOT Assigned(ptr) then
      EXIT;

    Pointer(aPointer) := NIL;

    FreeMem(ptr, aSize);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure FreeAndNIL(const aObjects: array of PObject); overload;
  var
    i: Integer;
    obj: TObject;
    {$ifopt C+}
    errs: Integer;
    errMsg: String;
    {$endif}
  begin
    {$ifopt C+}errs := 0;{$endif}

    for i := Low(aObjects) to High(aObjects) do
    begin
      try
        obj := aObjects[i]^;
        aObjects[i]^ := NIL;
        obj.Free;
      except
        {$ifopt C+}
        on E: Exception do
        begin
          Inc(errs);
          errMsg := errMsg + Format('Object #%d : %s', [i, e.Message + #13]);

        end;
        {$endif}
      end;
    end;

    {$ifopt C+}
    if (errs > 0) then
      raise Exception.CreateFmt('%d of %d objects when free''d caused an exception:'#13#13'%s',
                                [errs, High(aObjects), errMsg]);
    {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure NILRefs(const aObjects: array of PObject);
  var
    i: Integer;
  begin
    for i := Low(aObjects) to High(aObjects) do
      aObjects[i]^ := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IfThen(aValue: Boolean; aTrue, aFalse: Boolean): Boolean; 
  begin
    if aValue then
      result := aTrue
    else
      result := aFalse;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IfThen(aValue: Boolean; aTrue, aFalse: Integer): Integer;
  begin
    if aValue then
      result := aTrue
    else
      result := aFalse;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IfThen(aValue: Boolean; aTrue, aFalse: ANSIString): ANSIString;
  begin
    if aValue then
      result := aTrue
    else
      result := aFalse;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IfThen(aValue: Boolean; aTrue, aFalse: UnicodeString): UnicodeString;
  begin
    if aValue then
      result := aTrue
    else
      result := aFalse;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IfThen(aValue: Boolean; aTrue, aFalse: TObject): TObject;
  begin
    if aValue then
      result := aTrue
    else
      result := aFalse;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IfThenInt(aValue: Boolean; aTrue, aFalse: Integer): Integer;
  begin
    if aValue then
      result := aTrue
    else
      result := aFalse;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Min64(ValueA, ValueB: Int64): Int64;
  begin
    if (ValueA < ValueB) then
      result := ValueA
    else
      result := ValueB;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Min(ValueA, ValueB: Cardinal): Cardinal;
  begin
    if (ValueA < ValueB) then
      result := ValueA
    else
      result := ValueB;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Min(ValueA, ValueB: Integer): Integer;
  begin
    if (ValueA < ValueB) then
      result := ValueA
    else
      result := ValueB;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Max64(ValueA, ValueB: Int64): Int64;
  begin
    if (ValueA > ValueB) then
      result := ValueA
    else
      result := ValueB;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Max(ValueA, ValueB: Cardinal): Cardinal;
  begin
    if (ValueA > ValueB) then
      result := ValueA
    else
      result := ValueB;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Max(ValueA, ValueB: Integer): Integer;
  begin
    if (ValueA > ValueB) then
      result := ValueA
    else
      result := ValueB;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure Exchange(var A, B; aSize: LongWord);
  var
    a8: Byte absolute A;
    b8: Byte absolute B;
    a16: Word absolute A;
    b16: Word absolute B;
    a32: LongWord absolute A;
    b32: LongWord absolute B;
    a64: Int64 absolute A;
    b64: Int64 absolute B;
    aE: Extended absolute A;
    bE: Extended absolute B;
    i8: Byte;
    i16: Word;
    i32: LongWord;
    i64: Int64;
  {$ifNdef WIN64}
    iE: Extended;
  {$endif}
    ap8: PByte;
    bp8: PByte;
  begin
    case aSize of
      sizeof(Byte)      : begin
                            i8 := a8;
                            a8 := b8;
                            b8 := i8;
                          end;

      sizeof(Word)      : begin
                            i16 := a16;
                            a16 := b16;
                            b16 := i16;
                          end;

      sizeof(LongWord)  : begin
                            i32 := a32;
                            a32 := b32;
                            b32 := i32;
                          end;

      sizeof(Int64)     : begin
                            i64 := a64;
                            a64 := b64;
                            b64 := i64;
                          end;

    {$ifNdef WIN64} // Extended is an alias for "Double" on Win64 and thus 8-bytes, not 10 as on Win32
      sizeof(Extended)  : begin
                            iE := aE;
                            aE := bE;
                            bE := iE;
                          end;
    {$endif}
    else
      ap8 := PByte(@A);
      bp8 := PByte(@B);
      i32 := aSize;
      while (i32 > 0) do
      begin
        i8   := ap8^;
        ap8^ := bp8^;
        bp8^ := i8;
        Inc(ap8);
        Inc(bp8);
        Dec(i32);
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function GUIDToString(const aGUID: TGUID;
                        const aFormat: TGUIDFormat): String;
  begin
    result := SysUtils.GUIDToString(aGUID);

    case aFormat of
      gfDefault     : EXIT;

      gfNoBraces    : result := Copy(result, 2, Length(result) - 2);

      gfNoHyphens   : result := StringReplace(result, '-', '', [rfReplaceAll]);

      gfDigitsOnly  : begin
                        result := Copy(result, 2, Length(result) - 2);
                        result := StringReplace(result, '-', '', [rfReplaceAll]);
                      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StringToGUID(const aString: String): TGUID;
  begin
    if NOT TryStringToGUID(aString, result) then
      raise EConvertError.CreateFmt('''%s'' is not a valid GUID', [aString]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TryStringToGUID(const aString: String;
                           var aGUID: TGUID): Boolean;

    function HexByte(p: PChar): Byte;
    begin
      result := 0;

      case p[0] of
        '0'..'9':  result := Byte(p[0]) - Byte('0');
        'a'..'f':  result := (Byte(p[0]) - Byte('a')) + 10;
        'A'..'F':  result := (Byte(p[0]) - Byte('A')) + 10;
      end;

      case p[1] of
        '0'..'9':  result := (result shl 4) or (Byte(p[1]) - Byte('0'));
        'a'..'f':  result := (result shl 4) or ((Byte(p[1]) - Byte('a')) + 10);
        'A'..'F':  result := (result shl 4) or ((Byte(p[1]) - Byte('A')) + 10);
      end;
    end;

  var
    s: String;
    src: PChar;
    dest: array[0..15] of Byte absolute aGUID;
  begin
    s       := '';
    result  := FALSE;

    if (Length(aString) = 32) then
      s := Copy(aString, 1, 8)  + '-'
         + Copy(aString, 10, 4) + '-'
         + Copy(aString, 14, 4) + '-'
         + Copy(aString, 18, 4) + '-'
         + Copy(aString, 22, 12);

    if (Length(aString) = 36) then
      s := aString;

    if ((Length(aString) = 38) and (aString[1] = '{') and (aString[38] = '}')) then
      s := Copy(aString, 2, 36);

    if (s = '') then
      EXIT;

    if (s[9] <> '-') or (s[14] <> '-') or (s[19] <> '-') or (s[24] <> '-') then
      EXIT;

    src := PChar(s);

    //            1  1 1  1 2  2 2 2 3 3 3
    // 0 2 4 6 -9 1 -4 6 -9 1 -4 6 8 0 2 4
    // ..XX..XX ..XX ..XX ..XX ..XX..XX..XX

    dest[0] := HexByte(@src[6]);
    dest[1] := HexByte(@src[4]);
    dest[2] := HexByte(@src[2]);
    dest[3] := HexByte(@src[0]);

    dest[4] := HexByte(@src[11]);
    dest[5] := HexByte(@src[9]);

    dest[6] := HexByte(@src[16]);
    dest[7] := HexByte(@src[14]);

    dest[8] := HexByte(@src[19]);
    dest[9] := HexByte(@src[21]);

    dest[10]  := HexByte(@src[24]);
    dest[11]  := HexByte(@src[26]);
    dest[12]  := HexByte(@src[28]);
    dest[13]  := HexByte(@src[30]);
    dest[14]  := HexByte(@src[32]);
    dest[15]  := HexByte(@src[34]);

    result := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IsNull(const aValue: TGUID): Boolean;
  begin
    result := CompareMem(@aValue, @NullGUID, sizeof(TGUID));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function NewGUID: TGUID;
  begin
    CreateGUID(result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function SameGUID(const GUIDA, GUIDB: TGUID): Boolean;
  begin
    result := CompareMem(@GUIDA, @GUIDB, sizeof(TGUID));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure AddTrailingBackslash(var aString: String);
  var
    len: Integer;
  begin
    len := Length(aString);
    case len of
      0: { NO-OP };
    else
      case aString[len] of
        '\': { NO-OP };
      else
        aString := aString + '\';
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure RemoveTrailingBackslash(var aString: String);
  var
    len: Integer;
  begin
    len := Length(aString);
    case len of
      0: { NO-OP };
    else
      case aString[len] of
        '\': SetLength(aString, len - 1);
      else
        { NO-OP }
      end;
    end;
  end;





{ ENotImplemented -------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor ENotImplemented.Create(const aClass: TClass;
                                     const aSignature: String);
  begin
    inherited CreateFmt('%s.%s', [aClass.ClassName, aSignature]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor ENotImplemented.Create(const aObject: TObject;
                                     const aSignature: String);
  begin
    inherited CreateFmt('%s.%s', [aObject.ClassName, aSignature]);
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function ReverseBytes(const aValue: Word): Word;
  begin
    result :=  (((aValue and $ff00) shr 8)
             or ((aValue and $00ff) shl 8));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function ReverseBytes(const aValue: LongWord): LongWord;
  begin
    result :=  (((aValue and $ff000000) shr 24)
            or  ((aValue and $00ff0000) shr 8)
            or  ((aValue and $0000ff00) shl 8)
            or  ((aValue and $000000ff) shl 24));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function BinToHex(const aBuf: Pointer;
                    const aSize: Integer): String;
  const
    DIGITS = '0123456789abcdef';
  var
    i: Integer;
    c: PByte;
  begin
    result  := '';
    c       := aBuf;

    for i := 1 to aSize do
    begin
      result := result + DIGITS[(c^ and $F0) shr 4 + 1];
      result := result + DIGITS[(c^ and $0F) + 1];
      Inc(c);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function HexToBin(const aString: String;
                    var aSize: Integer): Pointer;
  begin
    result  := NIL;
    aSize   := Length(aString) div 2;

    if aSize = 0 then
      EXIT;

    result := AllocMem(aSize);
    HexToBin(aString, result^, aSize);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure HexToBin(const aString: String;
                     var aBuf;
                     const aSize: Integer);
  begin
    if aSize = 0 then
      EXIT;

    Classes.HexToBin(PChar(aString), PANSIChar(@aBuf), aSize);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure FillZero(var aDest; const aSize: Integer);
  begin
    FillChar(aDest, aSize, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Round(const aValue: Extended;
                 const aStrategy: TRoundingStrategy = rsDefault): Integer;
  var
    remainder: Extended;
  begin
    if (aStrategy = rsDefault) then
      result := System.Round(aValue)
    else
    begin
      result    := Trunc(aValue);
      remainder := Frac(aValue);

      case aStrategy of
        rsAwayFromZero  : if (remainder < 0) then
                            Dec(result)
                          else if (remainder > 0) then
                            Inc(result);

        rsTowardsZero   : if (remainder < 0) then
                            Inc(result)
                          else if (remainder > 0) then
                            Dec(result);
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ForEachComponent(const aComponent: TComponent;
                             const aProc: TComponentProc;
                             const aRecursive: Boolean;
                             const aClass: TComponentClass);
  var
    i: Integer;
    comp: TComponent;
  begin
    for i := 0 to Pred(aComponent.ComponentCount) do
    begin
      comp := aComponent.Components[i];

      if NOT Assigned(aClass) or comp.InheritsFrom(aClass) then
        aProc(comp);

      if aRecursive then
        ForEachComponent(comp, aProc, TRUE, aClass);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StringIndex(const aString: String;
                       const aCases: array of String): Integer;
  begin
    for result := 0 to Pred(Length(aCases)) do
      if ANSISameText(aString, aCases[result]) then
        EXIT;

    result := -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TextIndex(const aString: String;
                     const aCases: array of String): Integer;
  begin
    for result := 0 to Pred(Length(aCases)) do
      if ANSISameStr(aString, aCases[result]) then
        EXIT;

    result := -1;
  end;


{$ifdef MSWINDOWS}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Exec(const aEXE: String;
                const aCommandLine: String): Cardinal;
  var
    startup: STARTUPINFO;
    info: PROCESS_INFORMATION;
  begin
    startup.cb := sizeof(startup);
    ZeroMemory(@startup, startup.cb);

    CreateProcess(PChar(aEXE), PChar(aEXE + ' ' + aCommandLine), NIL, NIL, FALSE, 0, NIL, NIL, startup, info);

    result := info.dwProcessID;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ExecAndWait(const aEXE: String;
                        const aCommandLine: String);
  var
    startup: STARTUPINFO;
    info: PROCESS_INFORMATION;
  begin
    startup.cb := sizeof(startup);
    ZeroMemory(@startup, startup.cb);

    CreateProcess(PChar(aEXE), PChar(aEXE + ' ' + aCommandLine), NIL, NIL, FALSE, 0, NIL, NIL, startup, info);
    WaitForSingleObject(info.hProcess, INFINITE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function FindProcess(const aEXEName: String;
                       var aProcess: TProcessEntry32): Boolean;
  var
    snapshot: THandle;
    process: TProcessEntry32;
  begin
    result := FALSE;

    snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    process.dwSize := SizeOf(process);

    if Process32First(snapshot, process) then
      while TRUE do
      begin
        result := ANSISameText(ExtractFileName(process.szExeFile), aEXEName)
               or ANSISameText(process.szExeFile, aEXEName);

        if result then
          aProcess  := process;

        if result or NOT Process32Next(snapshot, process) then
          BREAK;
      end;

    CloseHandle(snapshot);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function ProcessExists(const aEXEName: String): Boolean;
  var
    notUsed: TProcessEntry32;
  begin
    result := FindProcess(aEXEName, notUsed);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function RegisterDLL(const aFileName: String;
                       const aRegistrationProc: String): Boolean;
  var
    hLib: THandle;
    ptr: Pointer;
    proc: TStdCallBooleanFunction;
  begin
    hLib := LoadLibrary(PChar(aFileName));
    if (hLib = 0) then
      RaiseLastOSError;

    result := FALSE;
    try
      ptr  := GetProcAddress(hLib, PChar(aRegistrationProc));

      if NOT Assigned(ptr) then
        raise Exception.CreateFmt('Registration procedure (%s) not found in library ''%s''.  Check case of procedure name.', [aRegistrationProc, aFileName]);

      proc := TStdCallBooleanFunction(ptr);
      try
        result := proc;

      except
        //
      end;

    finally
      FreeLibrary(hLib);
    end;
  end;
{$endif}

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IsTRUE(const aTri: TriBoolean): Boolean;
  begin
    result := (aTri = tbTRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IsFALSE(const aTri: TriBoolean): Boolean;
  begin
    result := (aTri = tbFALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IsKnown(const aTri: TriBoolean): Boolean;
  begin
    result := (aTri <> tbUnknown);
  end;





end.
