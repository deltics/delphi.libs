{
  * X11 (MIT) LICENSE *

  Copyright © 2013 Jolyon Smith

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

{$ifdef deltics_strings}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Strings;


interface

  uses
    Classes,
    SysUtils,
    Deltics.Strings.WideStringList;


  type
  {$ifNdef UNICODE}
    UTF8String    = type ANSIString;
    UnicodeString = type WideString;

    TANSIStrings    = Classes.TStrings;
    TANSIStringList = Classes.TStringList;

    TWIDEStrings    = TALTStrings;
    TWIDEStringList = TALTStringList;
  {$else}
    TWIDEStrings    = Classes.TStrings;
    TWIDEStringList = Classes.TStringList;

    TANSIStrings    = TALTStrings;
    TANSIStringList = TALTStringList;
  {$endif}

    TReplaceFlags = SysUtils.TReplaceFlags;


    ASCIIString = type ANSIString;
    ASCIIChar   = type ANSIChar;
    PASCIIChar  = ^ASCIIChar;

    UTF8Char    = type ANSIChar;
    PUTF8Char   = ^UTF8Char;

    TCharIndexArray   = array of Integer;
    TStringArray      = array of String;
    TANSIStringArray  = array of ANSIString;
    TUTF8StringArray  = array of UTF8String;
    TWIDEStringArray  = array of UnicodeString;

    TANSICharArray  = array of ANSIChar;
    TUTF8CharArray  = array of UTF8Char;
    TWIDECharArray  = array of WIDEChar;

    TAlphaCase  = (
                   acNotAlpha,
                   acLower,
                   acUpper
                  );


    TCaseSensitive = (
                      caseSensitive,
                      caseNotSensitive
                     );


  type
    TANSICaseFn = function(aChar: ANSIChar): LongBool; stdcall;
    TWIDECaseFn = function(aChar: WIDEChar): LongBool; stdcall;

    ANSIFn = class
    private
      class function CheckCase(const aString: ANSIString; const aCaseFn: TANSICaseFn): Boolean;
    public
      class function Compare(const A, B: ANSIString): Integer;
      class function CompareText(const A, B: ANSIString): Integer;
      class function Coalesce(const aArray: array of ANSIString; const aSep: ANSIChar): ANSIString; overload;
      class function Concat(const aArray: array of ANSIString): ANSIString; overload;
      class function Concat(const aArray: array of ANSIString; const aSep: ANSIChar): ANSIString; overload;
      class function Embrace(const aString: ANSIString; const aBraceChar: ASCIIChar = '('): ANSIString;
      class function Encode(const aString: String): ANSIString;
      class function Enquote(const aString: ANSIString; const aQuoteChar: ANSIChar = ''''): ANSIString;
      class function FromUTF8(const aString: UTF8String): ANSIString; overload;
      class function FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): ANSIString; overload;
      class function FromWIDE(const aString: UnicodeString): ANSIString;
      class function IsLowercase(const aChar: ANSIChar): Boolean; overload;
      class function IsLowercase(const aString: ANSIString): Boolean; overload;
      class function IsUppercase(const aChar: ANSIChar): Boolean; overload;
      class function IsUppercase(const aString: ANSIString): Boolean; overload;
      class function Len(const aBuffer: PANSIChar): Integer;
      class function Lowercase(const aChar: ANSIChar): ANSIChar; overload;
      class function Lowercase(const aString: ANSIString): ANSIString; overload;
      class function PadLeft(const aString: ANSIString; const aCount: Integer; const aChar: ASCIIChar = ' '): ANSIString; overload;
      class function PadRight(const aString: ANSIString; const aCount: Integer; const aChar: ASCIIChar = ' '): ANSIString; overload;
      class function PadToLengthLeft(const aString: ANSIString; const aMaxLen: Integer; const aChar: ASCIIChar = ' '): ANSIString; overload;
      class function PadToLengthRight(const aString: ANSIString; const aMaxLen: Integer; const aChar: ASCIIChar = ' '): ANSIString; overload;
      class function Remove(const aString, aStringToRemove: ANSIString; const aFlags: TReplaceFlags): ANSIString;
      class function RepeatString(const aString: ANSIString; const aCount: Integer): ANSIString; overload;
      class function Replace(const aString, aFindStr, aReplaceStr: ANSIString; const aFlags: TReplaceFlags): ANSIString;
      class function SameText(const A, B: ANSIString): Boolean;
      class function StringOfChar(const aChar: ASCIIChar; const aCount: Integer): ANSIString; overload;
      class function Trim(const aString: ANSIString; const aChar: ASCIIChar = ' '): ANSIString; overload;
      class function TrimLeft(const aString: ANSIString; const aChar: ASCIIChar = ' '): ANSIString; overload;
      class function TrimLeft(const aString: ANSIString; const aCount: Integer): ANSIString; overload;
      class function TrimRight(const aString: ANSIString; const aChar: ASCIIChar = ' '): ANSIString; overload;
      class function TrimRight(const aString: ANSIString; const aCount: Integer): ANSIString; overload;
      class function Unbrace(const aString: ANSIString): ANSIString;
      class function Unquote(const aString: ANSIString): ANSIString;
      class function Uppercase(const aChar: ANSIChar): ANSIChar; overload;
      class function Uppercase(const aString: ANSIString): ANSIString; overload;
    end;

    UTF8Fn = class
      class function Compare(const A, B: UTF8String): Integer;
      class function CompareText(const A, B: UTF8String): Integer;
      class function Encode(const aString: String): UTF8String;
      class function Decode(const aString: UTF8String): String; overload;
      class function Decode(const aBuffer: PUTF8Char; const aMaxLen: Integer): String; overload;
      class function FromANSI(const aString: ANSIString): UTF8String;
      class function FromWIDE(const aString: UnicodeString): UTF8String;
      class function Len(const aString: PUTF8Char): Integer;
      class function SameText(const A, B: UTF8String): Boolean;
    end;

    WIDEFn = class
    private
      class function CheckCase(const aString: UnicodeString; const aCaseFn: TWIDECaseFn): Boolean;
    public
      class function Compare(const A, B: UnicodeString): Integer;
      class function CompareText(const A, B: UnicodeString): Integer;
      class function Coalesce(const aArray: array of UnicodeString; const aSep: WIDEChar): UnicodeString;
      class function Concat(const aArray: array of UnicodeString): UnicodeString; overload;
      class function Concat(const aArray: array of UnicodeString; const aSep: WIDEChar): UnicodeString; overload;
      class function Embrace(const aString: UnicodeString; const aBraceChar: ASCIIChar = '('): UnicodeString;
      class function Encode(const aString: String): UnicodeString;
      class function Enquote(const aString: UnicodeString; const aQuoteChar: WIDEChar = ''''): UnicodeString;
      class function FromANSI(const aString: ANSIString): UnicodeString; overload;
      class function FromANSI(const aBuffer: PANSIChar; const aMaxLen: Integer): UnicodeString; overload;
      class function FromUTF8(const aString: UTF8String): UnicodeString; overload;
      class function FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): UnicodeString; overload;
      class function IsLowercase(const aChar: WIDEChar): Boolean; overload;
      class function IsLowercase(const aString: UnicodeString): Boolean; overload;
      class function IsUppercase(const aChar: WIDEChar): Boolean; overload;
      class function IsUppercase(const aString: UnicodeString): Boolean; overload;
      class function Len(const aBuffer: PWIDEChar): Integer;
      class function Lowercase(const aChar: WIDEChar): WIDEChar; overload;
      class function Lowercase(const aString: UnicodeString): UnicodeString; overload;
      class function PadLeft(const aString: UnicodeString; const aCount: Integer; const aChar: ASCIIChar = ' '): UnicodeString; overload;
      class function PadRight(const aString: UnicodeString; const aCount: Integer; const aChar: ASCIIChar = ' '): UnicodeString; overload;
      class function PadToLengthLeft(const aString: UnicodeString; const aMaxLen: Integer; const aChar: ASCIIChar = ' ' ): UnicodeString; overload;
      class function PadToLengthRight(const aString: UnicodeString; const aMaxLen: Integer; const aChar: ASCIIChar = ' '): UnicodeString; overload;
      class function Remove(const aString, aStringToRemove: UnicodeString; const aFlags: TReplaceFlags): UnicodeString; overload;
      class function RepeatString(const aString: UnicodeString; const aCount: Integer): UnicodeString; overload;
      class function Replace(const aString: UnicodeString; const aFindStr, aReplaceStr: UnicodeString; const aFlags: TReplaceFlags): UnicodeString;
      class function SameText(const A, B: UnicodeString): Boolean;
      class function StringOfChar(const aChar: ASCIIChar; const aCount: Integer): UnicodeString; overload;
      class function Trim(const aString: UnicodeString; const aChar: ASCIIChar = ' '): UnicodeString;
      class function TrimLeft(const aString: UnicodeString; const aChar: ASCIIChar = ' '): UnicodeString; overload;
      class function TrimLeft(const aString: UnicodeString; const aCount: Integer): UnicodeString; overload;
      class function TrimRight(const aString: UnicodeString; const aChar: ASCIIChar = ' '): UnicodeString; overload;
      class function TrimRight(const aString: UnicodeString; const aCount: Integer): UnicodeString; overload;
      class function Unbrace(const aString: UnicodeString): UnicodeString;
      class function Unquote(const aString: UnicodeString): UnicodeString;
      class function Uppercase(const aChar: WIDEChar): WIDEChar; overload;
      class function Uppercase(const aString: UnicodeString): UnicodeString; overload;
    end;


    IChar = interface
      function AlphaCase: TAlphaCase;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function ToANSI: ANSIChar;
      function ToWIDE: WIDEChar;
    end;


    IStringBase = interface
      function ByteCount: Integer;
      function Length: Integer;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function ToANSI: ANSIString;
      function ToString: String;
      function ToUTF8: UTF8String;
      function ToWIDE: UnicodeString;
    end;


    // TODO: Consider also a IDBCSChar and IDBCSString implementation.
    //
    //       These interfaces would require specific codepage support.
    //
    //       The char interface would require BOX'ing a PANSIChar and could make no
    //        assumptions about the number of bytes pointed to.


    IANSIChar = interface(IChar)
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function Lowercase: ANSIChar;
      function Uppercase: ANSIChar;
    end;


    IANSIString = interface(IStringBase)
      function BeginsWith(const aString: ANSIString): Boolean;
      function BeginsWithText(const aString: ANSIString): Boolean;
      function CompareWith(const aString: ANSIString): Integer;
      function CompareWithText(const aString: ANSIString): Integer;
      function Contains(const aString: ANSIString): Boolean; overload;
      function Contains(const aChar: ANSIChar): Boolean; overload;
      function ContainsText(const aChar: ANSIChar): Boolean; overload;
      function ContainsText(const aText: ANSIString): Boolean; overload;
      function Data: PANSIChar;
      function EqualsText(const aString: ANSIString): Boolean;
      function Find(const aChar: ANSIChar; var aPos: TCharIndexArray): Boolean; overload;
      function Find(const aString: ANSIString; var aPos: TCharIndexArray): Boolean; overload;
      function FindText(const aString: ANSIString; var aPos: TCharIndexArray): Boolean; overload;
      function FindFirst(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindFirst(const aString: ANSIString; var aPos: Integer): Boolean; overload;
      function FindFirstText(const aString: ANSIString; var aPos: Integer): Boolean; overload;
      function FindNext(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindNext(const aString: ANSIString; var aPos: Integer): Boolean; overload;
      function FindLast(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindLast(const aString: ANSIString; var aPos: Integer): Boolean; overload;
      function IndexIn(const aArray: array of ANSIString): Integer;
      function IsIn(const aArray: array of ANSIString): Boolean;
      function Leftmost(const aCount: Integer): ANSIString;
      function Rightmost(const aCount: Integer): ANSIString;
      function Split(const aChar: ANSIChar; var aLeft, aRight: ANSIString): Boolean; overload;
      function Split(const aChar: ANSIChar; var aParts: TANSIStringArray): Boolean; overload;

      function Delete(const aStart, aCount: Integer): ANSIString;
      function Embrace(const aBraceChar: ASCIIChar = '('): ANSIString;
      function Enquote(const aQuoteChar: ANSIChar = ''''): ANSIString;
      function ExtractLeft(const aCount: Integer): ANSIString;
      function ExtractRight(const aCount: Integer): ANSIString;
      function Lowercase: ANSIString;
      function PadLeft(const aCount: Integer; const aChar: ASCIIChar): ANSIString; overload;
      function PadRight(const aCount: Integer; const aChar: ASCIIChar): ANSIString; overload;
      function PadToLengthLeft(const aMaxLen: Integer; const aChar: ASCIIChar = ' '): ANSIString; overload;
      function PadToLengthRight(const aMaxLen: Integer; const aChar: ASCIIChar = ' '): ANSIString; overload;
      function Remove(const aString: ANSIString; const aFlags: TReplaceFlags): ANSIString;
      function Replace(const aString: ANSIString; const aFindStr, aReplaceStr: ANSIString; const aFlags: TReplaceFlags): ANSIString;
      function Trim(const aChar: ASCIIChar = ' '): ANSIString; overload;
      function TrimLeft(const aChar: ASCIIChar = ' '): ANSIString; overload;
      function TrimLeft(const aCount: Integer): ANSIString; overload;
      function TrimRight(const aChar: ASCIIChar = ' '): ANSIString; overload;
      function TrimRight(const aCount: Integer): ANSIString; overload;
      function Unbrace: ANSIString;
      function Unquote: ANSIString;
      function Uppercase: ANSIString;
    end;


    IUTF8String = interface(IStringBase)
      function BeginsWith(const aString: ANSIString): Boolean; overload;
      function BeginsWith(const aString: UTF8String): Boolean; overload;
      function BeginsWith(const aString: WIDEString): Boolean; overload;
      function BeginsWithText(const aString: UTF8String): Boolean;
      function CompareWith(const aString: UTF8String): Integer;
      function CompareWithText(const aString: UTF8String): Integer;
      function Contains(const aChar: ANSIChar): Boolean; overload;
      function Contains(const aChar: WIDEChar): Boolean; overload;
      function Contains(const aString: UTF8String): Boolean; overload;
      function ContainsText(const aChar: ANSIChar): Boolean; overload;
      function ContainsText(const aChar: WIDEChar): Boolean; overload;
      function ContainsText(const aString: UTF8String): Boolean; overload;
      function Data: PUTF8Char;
      function EqualsText(const aString: UTF8String): Boolean;
      function Find(const aChar: ANSIChar; var aPos: TCharIndexArray): Boolean; overload;
      function Find(const aChar: WIDEChar; var aPos: TCharIndexArray): Boolean; overload;
      function Find(const aString: UTF8String; var aPos: TCharIndexArray): Boolean; overload;
      function FindFirst(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindFirst(const aChar: WIDEChar; var aPos: Integer): Boolean; overload;
      function FindFirst(const aString: UTF8String; var aPos: Integer): Boolean; overload;
      function FindLast(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindLast(const aChar: WIDEChar; var aPos: Integer): Boolean; overload;
      function FindLast(const aString: UTF8String; var aPos: Integer): Boolean; overload;
      function FindNext(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindNext(const aChar: WIDEChar; var aPos: Integer): Boolean; overload;
      function FindNext(const aString: UTF8String; var aPos: Integer): Boolean; overload;

      function Lowercase: UTF8String;
      function Uppercase: UTF8String;
    end;


    IWIDEChar = interface(IChar)
      function Lowercase: WIDEChar;
      function Uppercase: WIDEChar;
    end;


    IWIDEString = interface(IStringBase)
      function BeginsWith(const aString: UnicodeString): Boolean;
      function BeginsWithText(const aString: UnicodeString): Boolean;
      function CompareWith(const aString: UnicodeString): Integer;
      function CompareWithText(const aString: UnicodeString): Integer;
      function Contains(const aChar: WIDEChar): Boolean; overload;
      function Contains(const aString: UnicodeString): Boolean; overload;
      function ContainsText(const aChar: WIDEChar): Boolean; overload;
      function ContainsText(const aString: UnicodeString): Boolean; overload;
      function CopyFrom(const aStart, aCount: Integer): UnicodeString;
      function CopyRange(const aStart, aEnd: Integer): UnicodeString;
      function Data: PWIDEChar;
      function EqualsText(const aString: UnicodeString): Boolean;
      function Find(const aChar: ANSIChar; var aPos: TCharIndexArray): Boolean; overload;
      function Find(const aChar: WIDEChar; var aPos: TCharIndexArray): Boolean; overload;
      function Find(const aString: UnicodeString; var aPos: TCharIndexArray): Boolean; overload;
      function FindText(const aChar: ANSIChar; var aPos: TCharIndexArray): Boolean; overload;
      function FindText(const aChar: WIDEChar; var aPos: TCharIndexArray): Boolean; overload;
      function FindText(const aString: UnicodeString; var aPos: TCharIndexArray): Boolean; overload;
      function FindFirst(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindFirst(const aChar: WIDEChar; var aPos: Integer): Boolean; overload;
      function FindFirst(const aString: UnicodeString; var aPos: Integer): Boolean; overload;
      function FindFirstText(const aString: UnicodeString; var aPos: Integer): Boolean; overload;
      function FindLast(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindLast(const aChar: WIDEChar; var aPos: Integer): Boolean; overload;
      function FindLast(const aString: UnicodeString; var aPos: Integer): Boolean; overload;
      function FindNext(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      function FindNext(const aChar: WIDEChar; var aPos: Integer): Boolean; overload;
      function FindNext(const aString: UnicodeString; var aPos: Integer): Boolean; overload;
      function IndexIn(const aArray: array of UnicodeString): Integer;
      function IsIn(const aArray: array of UnicodeString): Boolean;
      function Leftmost(const aCount: Integer): UnicodeString;
      function Rightmost(const aCount: Integer): UnicodeString;
      function Split(const aChar: ANSIChar; var aLeft, aRight: UnicodeString): Boolean; overload;
      function Split(const aChar: WIDEChar; var aLeft, aRight: UnicodeString): Boolean; overload;
      function Split(const aChar: ANSIChar; var aParts: TWIDEStringArray): Boolean; overload;
      function Split(const aChar: WIDEChar; var aParts: TWIDEStringArray): Boolean; overload;

      function Delete(const aStart, aCount: Integer): UnicodeString;
      function Embrace(const aBraceChar: ASCIIChar = '('): UnicodeString;
      function Enquote(const aQuoteChar: WIDEChar = ''''): UnicodeString;
      function ExtractLeft(const aCount: Integer): UnicodeString;
      function ExtractRight(const aCount: Integer): UnicodeString;
      function Lowercase: UnicodeString;
      function PadLeft(const aCount: Integer; const aChar: ASCIIChar): UnicodeString; overload;
      function PadRight(const aCount: Integer; const aChar: ASCIIChar): UnicodeString; overload;
      function PadToLengthLeft(const aMaxLen: Integer; const aChar: ASCIIChar = ' '): UnicodeString; overload;
      function PadToLengthRight(const aMaxLen: Integer; const aChar: ASCIIChar = ' '): UnicodeString; overload;
      function Remove(const aString: UnicodeString; const aFlags: TReplaceFlags): UnicodeString;
      function Replace(const aFindStr, aReplaceStr: UnicodeString; const aFlags: TReplaceFlags): UnicodeString;
      function Trim(const aChar: ASCIIChar = ' '): UnicodeString; overload;
      function TrimLeft(const aChar: ASCIIChar = ' '): UnicodeString; overload;
      function TrimLeft(const aCount: Integer): UnicodeString; overload;
      function TrimRight(const aChar: ASCIIChar = ' '): UnicodeString; overload;
      function TrimRight(const aCount: Integer): UnicodeString; overload;
      function Unbrace: UnicodeString;
      function Unquote: UnicodeString;
      function Uppercase: UnicodeString;
    end;


  {$ifdef UNICODE}
    STRFn = class(WIDEFn)
      class function FromWIDE(const aString: UnicodeString): UnicodeString;
    end;

    IString = IWIDEString;
  {$else}
    STRFn = class(ANSIFn)
      class function FromANSI(const aString: ANSIString): ANSIString;
    end;

    IString = IANSIString;
  {$endif}


  ANSIClass = class of ANSIFn;
  UTF8Class = class of UTF8Fn;
  WIDEClass = class of WIDEFn;
  STRClass  = class of STRFn;

  function ANSI: ANSIClass; overload;
  function ANSI(const aChar: ANSIChar): IANSIChar; overload;
  function ANSI(const aString: ANSIString): IANSIString; overload;
  function ANSIFromUTF8(const aString: UTF8String): IANSIString;
  function ANSIFromWIDE(const aString: UnicodeString): IANSIString;

  function WIDE: WIDEClass; overload;
  function WIDE(const aChar: WIDEChar): IWIDEChar; overload;
  function WIDE(const aString: UnicodeString): IWIDEString; overload;
  function WIDEFromANSI(const aString: ANSIString): IWIDEString;
  function WIDEFromUTF8(const aString: UTF8String): IWIDEString;


  function STR: STRClass; overload;
{$ifdef UNICODE}
  function STR(const aChar: WIDEChar): IWIDEChar; overload;
  function STR(const aString: UnicodeString): IString; overload;
{$else}
  function STR(const aChar: ANSIChar): IANSIChar; overload;
  function STR(const aString: ANSIString): IString; overload;
{$endif}


  function UTF8: UTF8Class; overload;
  function UTF8(const aString: UTF8String): IUTF8String; overload;
  function UTF8FromANSI(const aString: ANSIString): IUTF8String;
  function UTF8FromSTR(const aString: String): IUTF8String;
  function UTF8FromWIDE(const aString: UnicodeString): IUTF8String;





implementation

  uses
  {$ifdef FASTSTRINGS}
    FastStrings,
  {$endif}
  { vcl: }
  {$ifdef DELPHIXE4_OR_LATER}
    ANSIStrings,
  {$endif}
    Windows,
    Deltics.Strings.ANSI,
    Deltics.Strings.UTF8,
    Deltics.Strings.WIDE;




  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.CheckCase(const aString: ANSIString;
                                     const aCaseFn: TANSICaseFn): Boolean;
  var
    i: Integer;
    bAlpha: Boolean;
    pc: PANSIChar;
  begin
    result  := FALSE;
    bAlpha  := FALSE;

    pc := PANSIChar(aString);
    for i := 0 to Pred(Length(aString)) do
    begin
      if Windows.IsCharAlphaA(pc^) then
      begin
        if NOT aCaseFn(pc^) then
          EXIT;

        bAlpha := TRUE;
      end;
      Inc(pc);
    end;

    if bAlpha then
      result := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Compare(const A, B: ANSIString): Integer;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, 0,
                             PANSIChar(A), Length(A),
                             PANSIChar(B), Length(B)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.CompareText(const A, B: ANSIString): Integer;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PANSIChar(A), Length(A),
                             PANSIChar(B), Length(B)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Coalesce(const aArray: array of ANSIString;
                                 const aSep: ANSIChar): ANSIString;
  var
    i: Integer;
  begin
    result := '';
    for i := 0 to Pred(Length(aArray)) do
      if (Length(aArray[i]) > 0) then
        result := result + aArray[i] + aSep;

    if Length(result) > 0 then
      SetLength(result, Length(result) - 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Concat(const aArray: array of ANSIString): ANSIString;
  var
    i: Integer;
  begin
    result := '';
    for i := 0 to Pred(Length(aArray)) do
      result := result + aArray[i];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Concat(const aArray: array of ANSIString;
                               const aSep: ANSIChar): ANSIString;
  var
    i: Integer;
  begin
    result := '';
    for i := 0 to Pred(Length(aArray)) do
      result := result + aArray[i] + aSep;

    if Length(result) > 0 then
      SetLength(result, Length(result) - 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Embrace(const aString: ANSIString;
                                const aBraceChar: ASCIIChar): ANSIString;
  var
    slen, rlen: Integer;
  begin
    slen := Length(aString);
    rlen := slen + 2;

    SetLength(result, rlen);
    CopyMemory(@result[2], PANSIChar(aString), slen);

    result[1] := ANSIChar(aBraceChar);

    case aBraceChar of
      '(' : result[rlen] := ANSIChar(')');
      '{' : result[rlen] := ANSIChar('}');
      '[' : result[rlen] := ANSIChar(']');
      '<' : result[rlen] := ANSIChar('>');
    else
      result[rlen] := ANSICHar(aBraceChar);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Encode(const aString: String): ANSIString;
  begin
  {$ifdef UNICODE}
    result := FromWIDE(aString);
  {$else}
    result := aString;
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Enquote(const aString: ANSIString;
                                const aQuoteChar: ANSIChar): ANSIString;
  var
    i, j: Integer;
    strlen: Integer;
    pc: PANSIChar;
  begin
    strlen := System.Length(aString);
    SetLength(result, (strlen * 2) + 2);

    j := 2;

    if strlen > 0 then
    begin
      pc  := PANSIChar(aString);
      for i := 1 to strlen do
      begin
        result[j] := pc^;

        if (pc^ = aQuoteChar) then
        begin
          result[j + 1] := pc^;
          Inc(j, 2);
        end
        else
          Inc(j);

        Inc(pc);
      end;
    end;

    result[1] := aQuoteChar;
    result[j] := aQuoteChar;
    SetLength(result, j);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.FromUTF8(const aString: UTF8String): ANSIString;
  begin
    // TODO: Can we do this more directly / efficiently ?
    result := ANSI.FromWIDE(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer): ANSIString;
  begin
    // TODO: Can we do this more directly / efficiently ?
    result := ANSI.FromWIDE(WIDE.FromUTF8(aBuffer, aMaxLen));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.FromWIDE(const aString: UnicodeString): ANSIString;
  var
    len: Integer;
  begin
    len := WideCharToMultiByte(CP_ACP, 0, PWIDEChar(aString), -1, NIL, 0, NIL, NIL);
    Dec(len);

    SetLength(result, len);
    WideCharToMultiByte(CP_ACP, 0, PWIDEChar(aString), System.Length(aString), PANSIChar(result), System.Length(result), NIL, NIL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.IsLowercase(const aChar: ANSIChar): Boolean;
  begin
    result := IsCharLowerA(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.IsLowercase(const aString: ANSIString): Boolean;
  begin
    result := CheckCase(aString, IsCharLowerA);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.IsUppercase(const aChar: ANSIChar): Boolean;
  begin
    result := IsCharUpperA(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.IsUppercase(const aString: ANSIString): Boolean;
  begin
    result := CheckCase(aString, IsCharUpperA);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Len(const aBuffer: PANSIChar): Integer;
  begin
  {$ifdef DELPHIXE4_OR_LATER}
    result := ANSIStrings.StrLen(aBuffer);
  {$else}
    result := SysUtils.StrLen(aBuffer);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Lowercase(const aChar: ANSIChar): ANSIChar;
  begin
    result := aChar;
    CharLowerBuffA(@result, 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Lowercase(const aString: ANSIString): ANSIString;
  var
    len: Integer;
  begin
    len := System.Length(aString);
    SetString(result, PANSIChar(aString), len);
    if len > 0 then
      CharLowerBuffA(PANSIChar(result), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.PadLeft(const aString: ANSIString;
                                const aCount: Integer;
                                const aChar: ASCIIChar): ANSIString;
  begin
    result := StringOfChar(aChar, aCount) + aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.PadRight(const aString: ANSIString;
                                 const aCount: Integer;
                                 const aChar: ASCIIChar): ANSIString;
  begin
    result := aString + StringOfChar(aChar, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.PadToLengthLeft(const aString: ANSIString;
                                        const aMaxLen: Integer;
                                        const aChar: ASCIIChar): ANSIString;
  var
    len: Integer;
  begin
    result := aString;

    len := System.Length(aString);
    if len >= aMaxLen then
      EXIT;

    result := StringOfChar(aChar, aMaxLen - len) + result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.PadToLengthRight(const aString: ANSIString;
                                         const aMaxLen: Integer;
                                         const aChar: ASCIIChar): ANSIString;
  var
    len: Integer;
  begin
    result := aString;

    len := System.Length(aString);
    if len >= aMaxLen then
      EXIT;

    result := result + StringOfChar(aChar, aMaxLen - len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.RepeatString(const aString: ANSIString;
                                     const aCount: Integer): ANSIString;
  var
    i: Integer;
    bytes: Integer;
    pr: PANSIChar;
  begin
    SetLength(result, System.Length(aString) * aCount);

    bytes := System.Length(aString);
    pr    := PANSIChar(result);

    for i := 1 to aCount do
    begin
      CopyMemory(pr, PANSIChar(aString), bytes);
      Inc(pr, bytes);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Remove(const aString: ANSIString;
                               const aStringToRemove: ANSIString;
                               const aFlags: TReplaceFlags): ANSIString;
{$ifNdef UNICODE}
  begin
    result := StringReplace(aString, aStringToRemove, '', aFlags);
  end;
{$else}
  var
    i: Integer;
    pa: TCharIndexArray;
    pr, ps: PANSIChar;
    p, flen, rlen, clen: Integer;
  begin
    if (rfReplaceAll in aFlags) then
    begin
      if (rfIgnoreCase in aFlags) then
        ANSI(aString).FindText(aStringToRemove, pa)
      else
        ANSI(aString).Find(aStringToRemove, pa);
    end
    else
    begin
      SetLength(pa, 1);
      if (rfIgnoreCase in aFlags) then
      begin
        if ANSI(aString).FindFirstText(aStringToRemove, p) then
          pa[0] := p;
      end
      else if ANSI(aString).FindFirst(aStringToRemove, p) then
        pa[0] := p;

      if pa[0] = 0 then
        SetLength(pa, 0);
    end;

    if Length(pa) = 0 then
      EXIT;

    flen := Length(aStringToRemove);
    rlen := Length(aString) - (Length(pa) * flen);
    SetLength(result, rlen);

    pr := PANSIChar(result);
    ps := PANSIChar(aString);

    CopyMemory(pr, ps, (pa[0] - 1));
    Inc(pr, pa[0] - 1);
    Inc(ps, flen);

    Inc(ps, pa[0] - 1);

    for i := 1 to Pred(Length(pa)) do
    begin
      clen := pa[i] - (pa[i - 1] + flen);
      if (clen > 0) then
      begin
        CopyMemory(pr, ps, clen);
        Inc(pr, clen);
      end;

      Inc(ps, clen + flen);
    end;

    if (pr - PANSIChar(result)) < rlen then
    begin
      clen := Length(aString) - (ps - PANSIChar(aString));
      CopyMemory(pr, ps, clen);
    end;
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Replace(const aString: ANSIString;
                                const aFindStr: ANSIString;
                                const aReplaceStr: ANSIString;
                                const aFlags: TReplaceFlags): ANSIString;
{$ifNdef UNICODE}
  begin
    result := StringReplace(aString, aFindStr, aReplaceStr, aFlags);
  end;
{$else}
  var
    i: Integer;
    pa: TCharIndexArray;
    pr, ps, px: PANSIChar;
    p, flen, xlen, rlen, clen: Integer;
  begin
    if (rfReplaceAll in aFlags) then
    begin
      if (rfIgnoreCase in aFlags) then
        ANSI(aString).FindText(aFindStr, pa)
      else
        ANSI(aString).Find(aFindStr, pa);
    end
    else
    begin
      SetLength(pa, 1);
      if (rfIgnoreCase in aFlags) then
      begin
        if ANSI(aString).FindFirstText(aFindStr, p) then
          pa[0] := p;
      end
      else if ANSI(aString).FindFirst(aFindStr, p) then
        pa[0] := p;

      if pa[0] = 0 then
        SetLength(pa, 0);
    end;

    if Length(pa) = 0 then
      EXIT;

    flen := Length(aFindStr);
    xlen := Length(aReplaceStr);
    rlen := Length(aString) + (Length(pa) * ((xlen - flen)));
    SetLength(result, rlen);

    pr := PANSIChar(result);
    ps := PANSIChar(aString);
    px := PANSIChar(aReplaceStr);

    CopyMemory(pr, ps, (pa[0] - 1));
    Inc(pr, pa[0] - 1);
    Inc(ps, flen);

    if (xlen > 0) then
    begin
      CopyMemory(pr, px, xlen);
      Inc(pr, xlen);
    end;

    Inc(ps, pa[0] - 1);

    for i := 1 to Pred(Length(pa)) do
    begin
      clen := pa[i] - (pa[i - 1] + flen);
      if (clen > 0) then
      begin
        CopyMemory(pr, ps, clen);
        Inc(pr, clen);
      end;

      if (xlen > 0) then
      begin
        CopyMemory(pr, px, xlen);
        Inc(pr, xlen);
      end;

      Inc(ps, clen + flen);
    end;

    if (pr - PANSIChar(result)) < rlen then
    begin
      clen := Length(aString) - (ps - PANSIChar(aString));
      CopyMemory(pr, ps, clen);
    end;
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.SameText(const A, B: ANSIString): Boolean;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PANSIChar(A), Length(A),
                             PANSIChar(B), Length(B)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.StringOfChar(const aChar: ASCIIChar;
                                     const aCount: Integer): ANSIString;
  var
    i: Integer;
    ac: ANSIChar;
    pr: PANSIChar;
  begin
    SetLength(result, aCount);

    ac  := ANSIChar(aChar);
    pr  := PANSIChar(result);

    for i := 1 to aCount do
    begin
      pr^ := ac;
      Inc(pr);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.TrimLeft(const aString: ANSIString;
                                 const aChar: ASCIIChar): ANSIString;
  var
    i, p: Integer;
    pc: PANSIChar;
  begin
    p   := 1;
    pc  := PANSIChar(aString);

    for i := 1 to System.Length(aString) do
      if (pc^ <> aChar) then
      begin
        p := i;
        BREAK;
      end
      else
        Inc(pc);

    if (p > 1) then
      result := Copy(aString, p, 1 + System.Length(aString) - p)
    else
      result := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Trim(const aString: ANSIString;
                             const aChar: ASCIIChar): ANSIString;
  begin
    result := aString;
    result := TrimLeft(result, aChar);
    result := TrimRight(result, aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.TrimLeft(const aString: ANSIString;
                                 const aCount: Integer): ANSIString;
  begin
    result := Copy(aString, aCount + 1, System.Length(aString) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.TrimRight(const aString: ANSIString;
                                  const aChar: ASCIIChar): ANSIString;
  var
    i, p: Integer;
    len: Integer;
    pc: PANSIChar;
  begin
    result  := aString;

    len := System.Length(aString);

    if (len = 0) then
      EXIT;

    p   := 0;
    pc  := @aString[len];

    for i := len downto 1 do
      if (pc^ <> aChar) then
      begin
        p := i;
        BREAK;
      end
      else
        Dec(pc);

    if (p < len) then
      SetLength(result, len - p);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.TrimRight(const aString: ANSIString;
                                  const aCount: Integer): ANSIString;
  begin
    result := aString;
    SetLength(result, System.Length(result) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Unbrace(const aString: ANSIString): ANSIString;
  var
    rlen: Integer;
  begin
    rlen := Length(aString) - 2;
    SetLength(result, rlen);
    CopyMemory(PANSIChar(result), @aString[2], rlen);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Unquote(const aString: ANSIString): ANSIString;
  var
    i, j, maxi: Integer;
    qc: ANSIChar;
  begin
    qc := aString[1];
    if (aString[Length(aString)] <> qc)
     or ((qc <> '''') and (qc <> '"') and (qc <> '`')) then
    begin
      result := aString;
      EXIT;
    end;

    SetLength(result, Length(aString) - 2);

    i     := 2;
    maxi  := System.Length(aString) - 1;
    j     := 1;

    while i <= maxi do
    begin
      result[j] := aString[i];
      if (aString[i] = qc) then
      begin
        if (aString[i + 1] = qc) then
          Inc(i, 2)
        else
          raise Exception.Create(STR.Concat(['Quoted string ''', STR.FromANSI(aString), ''' contains incorrectly escaped quotes']));
      end
      else
        Inc(i);

      Inc(j);
    end;

    SetLength(result, j - 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Uppercase(const aChar: ANSIChar): ANSIChar;
  begin
    result := aChar;
    CharUpperBuffA(@result, 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Uppercase(const aString: ANSIString): ANSIString;
  var
    len: Integer;
  begin
    len := System.Length(aString);
    SetString(result, PANSIChar(aString), len);
    if len > 0 then
      CharUpperBuffA(PANSIChar(result), len);
  end;










  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.Decode(const aString: UTF8String): String;
  begin
  {$ifdef UNICODE}
    result := WIDE.FromUTF8(aString);
  {$else}
    result := ANSI.FromUTF8(aString);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.Decode(const aBuffer: PUTF8Char;
                               const aMaxLen: Integer): String;
  begin
  {$ifdef UNICODE}
    result := WIDE.FromUTF8(aBuffer, aMaxLen);
  {$else}
    result := ANSI.FromUTF8(aBuffer, aMaxLen);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.Encode(const aString: String): UTF8String;
  begin
  {$ifdef UNICODE}
    result := UTF8.FromWIDE(aString);
  {$else}
    result := UTF8.FromANSI(aString);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.FromANSI(const aString: ANSIString): UTF8String;
  begin
    result := UTF8.FromWIDE(WIDE.FromANSI(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.FromWIDE(const aString: UnicodeString): UTF8String;
  var
    len: Integer;
  begin
    len := WideCharToMultiByte(CP_UTF8, 0, PWIDEChar(aString), -1, NIL, 0, NIL, NIL);
    Dec(len);

    SetLength(result, len);
    WideCharToMultiByte(CP_UTF8, 0, PWIDEChar(aString), Length(aString), PANSIChar(result), Length(result), NIL, NIL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.Len(const aString: PUTF8Char): Integer;
  begin
  {$ifdef DELPHIXE4_OR_LATER}
    result := ANSIStrings.StrLen(PANSIChar(aString));
  {$else}
    result := SysUtils.StrLen(PANSIChar(aString));
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.SameText(const A, B: UTF8String): Boolean;
  begin
    result := WIDE.SameText(WIDE.FromUTF8(A), WIDE.FromUTF8(B));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.Compare(const A, B: UTF8String): Integer;
  begin
    result := WIDE.Compare(WIDE.FromUTF8(A), WIDE.FromUTF8(B));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8Fn.CompareText(const A, B: UTF8String): Integer;
  begin
    result := WIDE.CompareText(WIDE.FromUTF8(A), WIDE.FromUTF8(B));
  end;


















  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.CheckCase(const aString: UnicodeString;
                                  const aCaseFn: TWIDECaseFn): Boolean;
  var
    i: Integer;
    bAlpha: Boolean;
    pc: PWIDEChar;
  begin
    result  := FALSE;
    bAlpha  := FALSE;

    pc := PWIDEChar(aString);
    for i := 0 to Pred(Length(aString)) do
    begin
      if Windows.IsCharAlphaW(pc^) then
      begin
        if NOT aCaseFn(pc^) then
          EXIT;

        bAlpha := TRUE;
      end;
      Inc(pc);
    end;

    if bAlpha then
      result := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Compare(const A, B: UnicodeString): Integer;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, 0,
                             PWIDEChar(A), System.Length(A),
                             PWIDEChar(B), System.Length(B)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.CompareText(const A, B: UnicodeString): Integer;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PWIDEChar(A), System.Length(A),
                             PWIDEChar(B), System.Length(B)) - CSTR_EQUAL;
  end;


  class function WIDEFn.Coalesce(const aArray: array of UnicodeString;
                                 const aSep: WIDEChar): UnicodeString;
  var
    i: Integer;
  begin
    result := '';
    for i := 0 to Pred(Length(aArray)) do
      if (Length(aArray[i]) > 0) then
        result := result + aArray[i] + aSep;

    if Length(result) > 0 then
      SetLength(result, Length(result) - 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Concat(const aArray: array of UnicodeString): UnicodeString;
  var
    i: Integer;
  begin
    result := '';
    for i := 0 to Pred(Length(aArray)) do
      result := result + aArray[i];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Concat(const aArray: array of UnicodeString;
                               const aSep: WIDEChar): UnicodeString;
  var
    i: Integer;
  begin
    result := '';
    for i := 0 to Pred(Length(aArray)) do
      result := result + aArray[i] + aSep;

    if Length(result) > 0 then
      SetLength(result, Length(result) - 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Embrace(const aString: UnicodeString;
                                const aBraceChar: ASCIIChar): UnicodeString;
  var
    slen, rlen: Integer;
  begin
    slen := Length(aString);
    rlen := slen + 2;

    SetLength(result, rlen);
    CopyMemory(@result[2], PWIDEChar(aString), slen * 2);

    result[1] := WIDEChar(aBraceChar);

    case aBraceChar of
      '(' : result[rlen] := WIDEChar(')');
      '{' : result[rlen] := WIDEChar('}');
      '[' : result[rlen] := WIDEChar(']');
      '<' : result[rlen] := WIDEChar('>');
    else
      result[rlen] := WIDECHar(aBraceChar);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Encode(const aString: String): UnicodeString;
  begin
  {$ifdef UNICODE}
    result := aString;
  {$else}
    result := FromANSI(aString);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Enquote(const aString: UnicodeString;
                                const aQuoteChar: WIDEChar): UnicodeString;
  var
    i, j: Integer;
    strlen: Integer;
    pc: PWIDEChar;
  begin
    strlen := System.Length(aString);
    SetLength(result, (strlen * 2) + 2);

    j := 2;

    if strlen > 0 then
    begin
      pc  := PWIDEChar(aString);
      for i := 1 to strlen do
      begin
        result[j] := pc^;

        if (pc^ = aQuoteChar) then
        begin
          result[j + 1] := pc^;
          Inc(j, 2);
        end
        else
          Inc(j);

        Inc(pc);
      end;
    end;

    result[1] := aQuoteChar;
    result[j] := aQuoteChar;
    SetLength(result, j);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.FromANSI(const aString: ANSIString): UnicodeString;
  begin
    result := FromANSI(@aString[1], System.Length(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.FromANSI(const aBuffer: PANSIChar;
                                 const aMaxLen: Integer): UnicodeString;
  var
    len: Integer;
  begin
    result := '';

    if (aMaxLen = 0) then
      EXIT;

    len := MultiByteToWideChar(CP_ACP, 0, aBuffer, aMaxLen, NIL, 0);

    // Reported length includes null terminator which we discount since a null terminator
    //  will be appended to the string anyway

    if (aMaxLen = -1) then
      Dec(len);

    SetLength(result, len);
    MultiByteToWideChar(CP_ACP, 0, aBuffer, aMaxLen, @result[1], len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.FromUTF8(const aString: UTF8String): UnicodeString;
  begin
    result := FromUTF8(PUTF8Char(aString), System.Length(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.FromUTF8(const aBuffer: PUTF8Char;
                                 const aMaxLen: Integer): UnicodeString;
  var
    len: Integer;
  begin
    result := '';

    if (aMaxLen = 0) then
      EXIT;

    len := MultiByteToWideChar(CP_UTF8, 0, PANSIChar(aBuffer), aMaxLen, NIL, 0);

    // Reported length includes null terminator which we discount since a null terminator
    //  will be appended to the string anyway

    if (aMaxLen = -1) then
      Dec(len);

    SetLength(result, len);
    MultiByteToWideChar(CP_UTF8, 0, PANSIChar(aBuffer), aMaxLen, @result[1], len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.IsLowercase(const aChar: WIDEChar): Boolean;
  begin
    result := IsCharLowerW(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.IsLowercase(const aString: UnicodeString): Boolean;
  begin
    result := CheckCase(aString, IsCharLowerW);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.IsUppercase(const aChar: WIDEChar): Boolean;
  begin
    result := IsCharUpperW(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.IsUppercase(const aString: UnicodeString): Boolean;
  begin
    result := CheckCase(aString, IsCharUpperW);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Len(const aBuffer: PWIDEChar): Integer;
  {$ifdef UNICODE}
    begin
      result := SysUtils.StrLen(aBuffer);
    end;
  {$else}
    var
      p: PWIDEChar;
    begin
      p := aBuffer;
      while p^ <> WIDEChar(0) do
        Inc(p);

      result := (p - aBuffer);
    end;
  {$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Lowercase(const aChar: WIDEChar): WIDEChar;
  begin
    result := aChar;
    CharLowerBuffW(@result, 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Lowercase(const aString: UnicodeString): UnicodeString;
  var
    len: Integer;
  begin
    len := System.Length(aString);
    SetString(result, PWIDEChar(aString), len);
    if len > 0 then
      CharLowerBuffW(PWIDEChar(result), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.PadLeft(const aString: UnicodeString;
                                const aCount: Integer;
                                const aChar: ASCIIChar): UnicodeString;
  begin
    result := StringOfChar(aChar, aCount) + aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.PadRight(const aString: UnicodeString;
                                 const aCount: Integer;
                                 const aChar: ASCIIChar): UnicodeString;
  begin
    result := aString + StringOfChar(aChar, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.PadToLengthLeft(const aString: UnicodeString;
                                        const aMaxLen: Integer;
                                        const aChar: ASCIIChar): UnicodeString;
  var
    len: Integer;
  begin
    result := aString;

    len := System.Length(aString);
    if len >= aMaxLen then
      EXIT;

    result := StringOfChar(aChar, aMaxLen - len) + result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.PadToLengthRight(const aString: UnicodeString;
                                         const aMaxLen: Integer;
                                         const aChar: ASCIIChar): UnicodeString;
  var
    len: Integer;
  begin
    result := aString;

    len := System.Length(aString);
    if len >= aMaxLen then
      EXIT;

    result := result + StringOfChar(aChar, aMaxLen - len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Remove(const aString: UnicodeString;
                               const aStringToRemove: UnicodeString;
                               const aFlags: TReplaceFlags): UnicodeString;
{$ifdef UNICODE}
  begin
    result := StringReplace(aString, aStringToRemove, '', aFlags);
  end;
{$else}
  var
    i: Integer;
    pa: TCharIndexArray;
    pr, ps: PWIDEChar;
    p, flen, rlen, clen: Integer;
  begin
    if (rfReplaceAll in aFlags) then
    begin
      if (rfIgnoreCase in aFlags) then
        WIDE(aString).FindText(aStringToRemove, pa)
      else
        WIDE(aString).Find(aStringToRemove, pa);
    end
    else
    begin
      SetLength(pa, 1);
      if (rfIgnoreCase in aFlags) then
      begin
        if WIDE(aString).FindFirstText(aStringToRemove, p) then
          pa[0] := p;
      end
      else if WIDE(aString).FindFirst(aStringToRemove, p) then
        pa[0] := p;

      if pa[0] = 0 then
        SetLength(pa, 0);
    end;

    if Length(pa) = 0 then
      EXIT;

    flen := Length(aStringToRemove);
    rlen := Length(aString) + (Length(pa) * flen);

    SetLength(result, rlen);

    pr := PWIDEChar(result);
    ps := PWIDEChar(aString);

    CopyMemory(pr, ps, (pa[0] - 1) * 2);
    Inc(pr, pa[0] - 1);
    Inc(ps, flen);

    Inc(ps, pa[0] - 1);

    for i := 1 to Pred(Length(pa)) do
    begin
      clen := pa[i] - (pa[i - 1] + flen);
      if (clen > 0) then
      begin
        CopyMemory(pr, ps, clen * 2);
        Inc(pr, clen);
      end;

      Inc(ps, clen + flen);
    end;

    if (pr - PWIDEChar(result)) < rlen then
    begin
      clen := Length(aString) - (ps - PWIDEChar(aString));
      CopyMemory(pr, ps, clen * 2);
    end;
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.RepeatString(const aString: UnicodeString;
                                     const aCount: Integer): UnicodeString;
  var
    i: Integer;
    bytes: Integer;
    pr: PWIDEChar;
  begin
    SetLength(result, System.Length(aString) * aCount);

    bytes := System.Length(aString) * 2;
    pr    := PWIDEChar(result);

    for i := 1 to aCount do
    begin
      CopyMemory(pr, PWIDEChar(aString), bytes);
      Inc(pr, bytes);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Replace(const aString: UnicodeString;
                                const aFindStr: UnicodeString;
                                const aReplaceStr: UnicodeString;
                                const aFlags: TReplaceFlags): UnicodeString;
{$ifdef UNICODE}
  begin
    result := StringReplace(aString, aFindStr, aReplaceStr, aFlags);
  end;
{$else}
  var
    i: Integer;
    pa: TCharIndexArray;
    pr, ps, px: PWIDEChar;
    p, flen, xlen, rlen, clen: Integer;
  begin
    if (rfReplaceAll in aFlags) then
    begin
      if (rfIgnoreCase in aFlags) then
        WIDE(aString).FindText(aFindStr, pa)
      else
        WIDE(aString).Find(aFindStr, pa);
    end
    else
    begin
      SetLength(pa, 1);
      if (rfIgnoreCase in aFlags) then
      begin
        if WIDE(aString).FindFirstText(aFindStr, p) then
          pa[0] := p;
      end
      else if WIDE(aString).FindFirst(aFindStr, p) then
        pa[0] := p;

      if pa[0] = 0 then
        SetLength(pa, 0);
    end;

    if Length(pa) = 0 then
      EXIT;

    flen := Length(aFindStr);
    xlen := Length(aReplaceStr);
    rlen := Length(aString) + (Length(pa) * ((xlen - flen)));

    SetLength(result, rlen);

    pr := PWIDEChar(result);
    ps := PWIDEChar(aString);
    px := PWIDEChar(aReplaceStr);

    CopyMemory(pr, ps, (pa[0] - 1) * 2);
    Inc(pr, pa[0] - 1);
    Inc(ps, flen);

    if (xlen > 0) then
    begin
      CopyMemory(pr, px, xlen * 2);
      Inc(pr, xlen);
    end;

    Inc(ps, pa[0] - 1);

    for i := 1 to Pred(Length(pa)) do
    begin
      clen := pa[i] - (pa[i - 1] + flen);
      if (clen > 0) then
      begin
        CopyMemory(pr, ps, clen * 2);
        Inc(pr, clen);
      end;

      if (xlen > 0) then
      begin
        CopyMemory(pr, px, xlen * 2);
        Inc(pr, xlen);
      end;

      Inc(ps, clen + flen);
    end;

    if (pr - PWIDEChar(result)) < rlen then
    begin
      clen := Length(aString) - (ps - PWIDEChar(aString));
      CopyMemory(pr, ps, clen * 2);
    end;
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.TrimLeft(const aString: UnicodeString;
                                 const aChar: ASCIIChar): UnicodeString;
  var
    i, p: Integer;
    pc: PWIDEChar;
    wc: WIDEChar;
  begin
    p   := 1;
    wc  := WIDEChar(aChar);
    pc  := PWIDEChar(aString);

    for i := 1 to System.Length(aString) do
      if (pc^ <> wc) then
      begin
        p := i;
        BREAK;
      end
      else
        Inc(pc);

    if (p > 1) then
      result := Copy(aString, p, 1 + System.Length(aString) - p)
    else
      result := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Trim(const aString: UnicodeString;
                             const aChar: ASCIIChar): UnicodeString;
  begin
    result := aString;
    result := TrimLeft(result, aChar);
    result := TrimRight(result, aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.TrimLeft(const aString: UnicodeString;
                                 const aCount: Integer): UnicodeString;
  begin
    result := Copy(aString, aCount + 1, System.Length(aString) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.TrimRight(const aString: UnicodeString;
                                  const aChar: ASCIIChar): UnicodeString;
  var
    i, p: Integer;
    len: Integer;
    wc: WIDEChar;
    pc: PWIDEChar;
  begin
    result  := aString;
    len     := System.Length(aString);

    if (len = 0) then
      EXIT;

    p   := 0;
    wc  := WIDEChar(aChar);
    pc  := @aString[len];

    for i := len downto 1 do
      if (pc^ <> wc) then
      begin
        p := i;
        BREAK;
      end
      else
        Dec(pc);

    if (p < len) then
      SetLength(result, len - p);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.TrimRight(const aString: UnicodeString;
                                  const aCount: Integer): UnicodeString;
  begin
    result := aString;
    SetLength(result, System.Length(result) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.SameText(const A, B: UnicodeString): Boolean;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PWIDEChar(A), Length(A),
                             PWIDEChar(B), Length(B)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.StringOfChar(const aChar: ASCIIChar;
                                     const aCount: Integer): UnicodeString;
  var
    i: Integer;
    wc: WIDEChar;
    pr: PWIDEChar;
  begin
    SetLength(result, aCount * 2);

    wc  := WIDEChar(aChar);
    pr  := PWIDEChar(result);

    for i := 1 to aCount do
    begin
      pr^ := wc;
      Inc(pr);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Unbrace(const aString: UnicodeString): UnicodeString;
  var
    rlen: Integer;
  begin
    rlen := Length(aString) - 2;
    SetLength(result, rlen);
    CopyMemory(PWIDEChar(result), @aString[2], rlen * 2);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Unquote(const aString: UnicodeString): UnicodeString;
  var
    i, j, maxi: Integer;
    qc: WIDEChar;
  begin
    qc := aString[1];
    if (aString[Length(aString)] <> qc)
     or ((qc <> '''') and (qc <> '"') and (qc <> '`')) then
    begin
      result := aString;
      EXIT;
    end;

    SetLength(result, Length(aString) - 2);

    i     := 2;
    maxi  := System.Length(aString) - 1;
    j     := 1;

    while i <= maxi do
    begin
      result[j] := aString[i];
      if (aString[i] = qc) then
      begin
        if (aString[i + 1] = qc) then
          Inc(i, 2)
        else
          raise Exception.Create('Quoted string ''' + aString + ''' contains incorrectly escaped quotes');
      end
      else
        Inc(i);

      Inc(j);
    end;

    SetLength(result, j - 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Uppercase(const aChar: WIDEChar): WIDEChar;
  begin
    result := aChar;
    CharUpperBuffW(@result, 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Uppercase(const aString: UnicodeString): UnicodeString;
  var
    len: Integer;
  begin
    len := System.Length(aString);
    SetString(result, PWIDEChar(aString), len);
    if len > 0 then
      CharUpperBuffW(PWIDEChar(result), len);
  end;










{$ifdef UNICODE}
  class function STRFn.FromWIDE(const aString: UnicodeString): UnicodeString;
  begin
    result := aString;
  end;
{$else}
  class function STRFn.FromANSI(const aString: ANSIString): ANSIString;
  begin
    result := aString;
  end;
{$endif}









  function ANSI: ANSIClass;
  begin
    result := ANSIFn;
  end;

  function ANSI(const aChar: ANSIChar): IANSIChar;
  begin
    result := TANSIChar.Create(aChar);
  end;

  function ANSI(const aString: ANSIString): IANSIString;
  begin
    result := TANSIString.Create(aString);
  end;


  function ANSIFromUTF8(const aString: UTF8String): IANSIString;
  begin
    result := TANSIString.Create(ANSI.FromUTF8(aString));
  end;


  function ANSIFromWIDE(const aString: UnicodeString): IANSIString;
  begin
    result := TANSIString.Create(ANSI.FromWIDE(aString));
  end;






  function WIDE: WIDEClass;
  begin
    result := WIDEFn;
  end;


  function WIDE(const aChar: WIDEChar): IWIDEChar;
  begin
    result := TWIDEChar.Create(aChar);
  end;


  function WIDE(const aString: UnicodeString): IWIDEString;
  begin
    result := TWIDEString.Create(aString);
  end;


  function WIDEFromANSI(const aString: ANSIString): IWIDEString;
  begin
    result := TWIDEString.Create(WIDE.FromANSI(aString));
  end;


  function WIDEFromUTF8(const aString: UTF8String): IWIDEString;
  begin
    result := TWIDEString.Create(WIDE.FromUTF8(aString));
  end;





  function STR: STRClass; overload;
  begin
    result := STRFn;
  end;


{$ifdef UNICODE}
  function STR(const aChar: WIDEChar): IWIDEChar;
  begin
    result := TWIDEChar.Create(aChar);
  end;

  function STR(const aString: UnicodeString): IWIDEString;
  begin
    result := TWIDEString.Create(aString);
  end;
{$else}
  function STR(const aChar: ANSIChar): IANSIChar;
  begin
    result := TANSIChar.Create(aChar);
  end;

  function STR(const aString: ANSIString): IANSIString;
  begin
    result := TANSIString.Create(aString);
  end;
{$endif}






  function UTF8: UTF8Class;
  begin
    result := UTF8Fn;
  end;


  function UTF8(const aString: UTF8String): IUTF8String;
  begin
    result := TUTF8String.Create(aString);
  end;


  function UTF8FromANSI(const aString: ANSIString): IUTF8String;
  begin
    result := TUTF8String.Create(UTF8.FromANSI(aString));
  end;


  function UTF8FromSTR(const aString: String): IUTF8String;
  begin
    result := TUTF8String.Create(UTF8.Encode(aString));
  end;


  function UTF8FromWIDE(const aString: UnicodeString): IUTF8String;
  begin
    result := TUTF8String.Create(UTF8.FromWIDE(aString));
  end;


end.
