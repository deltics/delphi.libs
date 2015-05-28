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
    Deltics.Strings.Encoding,
    Deltics.Strings.StringList;


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

    TStringProcessingFlag = (
                             rsFromStart,
                             rsFromEnd,
                             rsIgnoreCase
                            );
    TReplaceStringFlag  = rsFromStart..rsIgnoreCase;
    TReplaceStringFlags = set of TReplaceStringFlag;

  const
    ssFromStart   = rsFromStart;
    ssFromEnd     = rsFromEnd;
    ssIgnoreCase  = rsIgnoreCase;

    isLesser  = -1;
    isEqual   = 0;
    isGreater = 1;


  type
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

    TCaseSensitivity = (
                        csCaseSensitive,
                        csIgnoreCase
                       );

    TCompareResult = isLesser..isGreater;

    TContainNeeds = (
                     cnAny,
                     cnEvery,
                     cnOneOf
                    );

    TStringScope = (
                    ssAll,
                    ssFirst,
                    ssLast
                   );


  type
    IStringSearch = interface
    ['{6AF9A17A-C7B5-43F9-8A52-FE73AC2EA9C0}']
      function get_EOF: Boolean;
      function get_Pos: Integer;
      procedure set_Pos(const aValue: Integer);

      function First: Boolean;
      function Last: Boolean;
      function Next: Boolean;
      function Previous: Boolean;
      procedure Reset;

      property EOF: Boolean read get_EOF;
      property Pos: Integer read get_Pos write set_Pos;
    end;


  type
    TEncoding = Deltics.Strings.Encoding.TEncoding;


    TANSICaseFn = function(aChar: ANSIChar): LongBool; stdcall;
    TWIDECaseFn = function(aChar: WIDEChar): LongBool; stdcall;

    ANSIFn = class
    private
      class function CheckCase(const aString: ANSIString; const aCaseFn: TANSICaseFn): Boolean;
    public
      // Transcoding
      class function Encode(const aString: String): ANSIString;
      class function FromUTF8(const aString: UTF8String): ANSIString; overload;
      class function FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): ANSIString; overload;
      class function FromWIDE(const aString: UnicodeString): ANSIString; overload;
      class function FromWIDE(const aBuffer: PWIDEChar; const aMaxLen: Integer = -1): ANSIString; overload;

      // Buffer (SZ pointer) routines
      class function AllocANSI(const aSource: ANSIString): PANSIChar;
      class function AllocUTF8(const aSource: ANSIString): PUTF8Char;
      class function AllocWIDE(const aSource: ANSIString): PWIDEChar;
      class procedure CopyToBuffer(const aSource: ANSIString; const aDest: PANSIChar; const aMaxBytes: Integer = -1); overload;
      class procedure CopyToBuffer(const aSource: ANSIString; const aDest: PUTF8Char; const aMaxBytes: Integer = -1); overload;
      class procedure CopyToBuffer(const aSource: ANSIString; const aDest: PWIDEChar; const aMaxChars: Integer = -1); overload;
      class function Len(const aBuffer: PANSIChar): Integer;

      // Inspection routines
      class function Compare(const A, B: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): TCompareResult;
      class function SameString(const A, B: ANSIString): Boolean;
      class function SameText(const A, B: ANSIString): Boolean;
      class function IsLowercase(const aChar: ANSIChar): Boolean; overload;
      class function IsLowercase(const aString: ANSIString): Boolean; overload;
      class function IsUppercase(const aChar: ANSIChar): Boolean; overload;
      class function IsUppercase(const aString: ANSIString): Boolean; overload;
      class function Find(const aString: ANSIString; const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      class function Find(const aString: ANSIString; const aSubstr: ANSIString; var aPos: Integer): Boolean; overload;
      class function FindNext(const aString: ANSIString; const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      class function FindNext(const aString: ANSIString; const aSubstr: ANSIString; var aPos: Integer): Boolean; overload;

      // Assembly routines
      class function Concat(const aArray: array of ANSIString): ANSIString; overload;
      class function Concat(const aArray: array of ANSIString; const aSeparator: ANSIString): ANSIString; overload;
      class function Format(const aString: ANSIString; const aValue: Double): ANSIString; overload;
      class function Format(const aString: ANSIString; const aValue: Integer): ANSIString; overload;
      class function Format(const aString: ANSIString; const aArgs: array of const): ANSIString; overload;
      class function StringOf(const aChar: ANSIChar; const aCount: Integer): ANSIString; overload;
      class function StringOf(const aString: ANSIString; const aCount: Integer): ANSIString; overload;

      // Case conversion
      class function Lowercase(const aChar: ANSIChar): ANSIChar; overload;
      class function Lowercase(const aString: ANSIString): ANSIString; overload;
      class function Uppercase(const aChar: ANSIChar): ANSIChar; overload;
      class function Uppercase(const aString: ANSIString): ANSIString; overload;

      // Embellishment (add to the string)
      class function Embrace(const aString: ANSIString; const aBraceChar: ANSIChar = '('): ANSIString;
      class function Enquote(const aString: ANSIString): ANSIString; overload;
      class function Enquote(const aString: ANSIString; const aQuoteChar: ANSIChar): ANSIString; overload;
      class function Enquote(const aString: ANSIString; const aQuoteChar: ANSIChar; const aEscapeChar: ANSIChar): ANSIString; overload;
      class function ExtendLeft(const aString: ANSIString; const aLength: Integer; const aChar: ANSIChar = ' '): ANSIString; overload;
      class function ExtendRight(const aString: ANSIString; const aLength: Integer; const aChar: ANSIChar = ' '): ANSIString; overload;
      class function PadLeft(const aString: ANSIString; const aCount: Integer; const aChar: ANSIChar = ' '): ANSIString; overload;
      class function PadRight(const aString: ANSIString; const aCount: Integer; const aChar: ANSIChar = ' '): ANSIString; overload;
      class function Remove(const aScope: TStringScope; const aString, aStringToRemove: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): ANSIString;
      class function Replace(const aScope: TStringScope; const aString, aFindStr, aReplaceStr: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): ANSIString;

      // Reduction (remove from the string)
      class function Trim(const aString: ANSIString; const aCount: Integer): ANSIString; overload;
      class function Trim(const aString: ANSIString; const aChar: ANSIChar = ' '): ANSIString; overload;
      class function RemoveLeading(const aString: ANSIString; const aCount: Integer): ANSIString; overload;
      class function RemoveLeading(const aString: ANSIString; const aChar: ANSIChar = ' '): ANSIString; overload;
      class function RemoveTrailing(const aString: ANSIString; const aCount: Integer): ANSIString; overload;
      class function RemoveTrailing(const aString: ANSIString; const aChar: ANSIChar = ' '): ANSIString; overload;
      class function Unbrace(const aString: ANSIString): ANSIString;
      class function Unquote(const aString: ANSIString): ANSIString;
    end;

    UTF8Fn = class
      class function Compare(const A, B: UTF8String; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer;
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
      // Transcoding
      class function Encode(const aString: String): UnicodeString;
      class function FromANSI(const aString: ANSIString): UnicodeString; overload;
      class function FromANSI(const aBuffer: PANSIChar; const aMaxLen: Integer): UnicodeString; overload;
      class function FromUTF8(const aString: UTF8String): UnicodeString; overload;
      class function FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): UnicodeString; overload;

      // Buffer (SZ pointer) routines
      class function AllocANSI(const aSource: UnicodeString): PANSIChar;
      class function AllocUTF8(const aSource: UnicodeString): PUTF8Char;
      class function AllocWIDE(const aSource: UnicodeString): PWIDEChar;
      class procedure CopyToBuffer(const aSource: UnicodeString; const aDest: PANSIChar; const aMaxBytes: Integer); overload;
      class procedure CopyToBuffer(const aSource: UnicodeString; const aDest: PUTF8Char; const aMaxBytes: Integer); overload;
      class procedure CopyToBuffer(const aSource: UnicodeString; const aDest: PWIDEChar; const aMaxChars: Integer); overload;
      class function FromBuffer(const aBuffer: PWIDEChar; const aMaxLen: Integer): UnicodeString;
      class function Len(const aBuffer: PWIDEChar): Integer;

      class function Compare(const A, B: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer;
      class function SameString(const A, B: UnicodeString): Boolean;
      class function SameText(const A, B: UnicodeString): Boolean;
      class function IsLowercase(const aChar: WIDEChar): Boolean; overload;
      class function IsLowercase(const aString: UnicodeString): Boolean; overload;
      class function IsUppercase(const aChar: WIDEChar): Boolean; overload;
      class function IsUppercase(const aString: UnicodeString): Boolean; overload;
      class function Find(const aString: UnicodeString; const aChar: WIDEChar; var aPos: Integer): Boolean; overload;
      class function Find(const aString: UnicodeString; const aSubstr: UnicodeString; var aPos: Integer): Boolean; overload;
      class function FindNext(const aString: UnicodeString; const aChar: WIDEChar; var aPos: Integer): Boolean; overload;
      class function FindNext(const aString: UnicodeString; const aSubstr: UnicodeString; var aPos: Integer): Boolean; overload;

      class function Concat(const aArray: array of UnicodeString): UnicodeString; overload;
      class function Concat(const aArray: array of UnicodeString; const aSeparator: UnicodeString): UnicodeString; overload;
      class function Format(const aString: UnicodeString; const aValue: Double): UnicodeString; overload;
      class function Format(const aString: UnicodeString; const aValue: Integer): UnicodeString; overload;
      class function Format(const aString: UnicodeString; const aArgs: array of const): UnicodeString; overload;
      class function StringOf(const aChar: WIDEChar; const aCount: Integer): UnicodeString; overload;
      class function StringOf(const aString: UnicodeString; const aCount: Integer): UnicodeString; overload;

      class function Lowercase(const aChar: WIDEChar): WIDEChar; overload;
      class function Lowercase(const aString: UnicodeString): UnicodeString; overload;
      class function Uppercase(const aChar: WIDEChar): WIDEChar; overload;
      class function Uppercase(const aString: UnicodeString): UnicodeString; overload;

      class function Embrace(const aString: UnicodeString; const aBraceChar: WIDEChar = '('): UnicodeString;
      class function Enquote(const aString: UnicodeString): UnicodeString; overload;
      class function Enquote(const aString: UnicodeString; const aQuoteChar: WIDEChar): UnicodeString; overload;
      class function Enquote(const aString: UnicodeString; const aQuoteChar: WIDEChar; const aEscapeChar: WIDEChar): UnicodeString; overload;
      class function PadLeft(const aString: UnicodeString; const aCount: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;
      class function PadRight(const aString: UnicodeString; const aCount: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;
      class function ExtendLeft(const aString: UnicodeString; const aLength: Integer; const aChar: WIDEChar = ' ' ): UnicodeString; overload;
      class function ExtendRight(const aString: UnicodeString; const aLength: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;

      class function Remove(const aScope: TStringScope; const aString, aStringToRemove: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): UnicodeString; overload;
      class function Replace(const aScope: TStringScope; const aString: UnicodeString; const aFindStr, aReplaceStr: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): UnicodeString;
      class function RemoveLeading(const aString: UnicodeString; const aCount: Integer): UnicodeString; overload;
      class function RemoveLeading(const aString: UnicodeString; const aChar: WIDEChar = ' '): UnicodeString; overload;
      class function RemoveTrailing(const aString: UnicodeString; const aCount: Integer): UnicodeString; overload;
      class function RemoveTrailing(const aString: UnicodeString; const aChar: WIDEChar = ' '): UnicodeString; overload;
      class function Trim(const aString: UnicodeString; const aCount: Integer): UnicodeString; overload;
      class function Trim(const aString: UnicodeString; const aChar: WIDEChar = ' '): UnicodeString; overload;
      class function Unbrace(const aString: UnicodeString): UnicodeString;
      class function Unquote(const aString: UnicodeString): UnicodeString;
    end;


    IChar = interface
      function AlphaCase: TAlphaCase;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function ToANSI: ANSIChar;
      function ToWIDE: WIDEChar;
    end;


    IStringBase = interface
      function get_Length: Integer;
      procedure set_Length(const aValue: Integer);

      function ByteCount: Integer;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function ToANSI: ANSIString;
      function ToString: String;
      function ToUTF8: UTF8String;
      function ToWIDE: UnicodeString;

      property Length: Integer read get_Length write set_Length;
    end;


    IANSIChar = interface(IChar)
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function Lowercase: ANSIChar;
      function Uppercase: ANSIChar;
    end;


    IANSIString = interface(IStringBase)
      function get_Value: ANSIString;
      procedure set_Value(const aValue: ANSIString);

      function AsPointer: PANSIChar;

      function AllocANSI: PANSIChar;
      function AllocUTF8: PUTF8Char;
      function AllocWIDE: PWIDEChar;
      procedure CopyToBuffer(const aDest: PANSIChar; const aMaxBytes: Integer = -1); overload;
      procedure CopyToBuffer(const aDest: PUTF8Char; const aMaxBytes: Integer = -1); overload;
      procedure CopyToBuffer(const aDest: PWIDEChar; const aMaxChars: Integer = -1); overload;

      function BeginsWith(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean;
      function CompareWith(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): TCompareResult;
      function Contains(const aChar: ANSIChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Contains(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Contains(const aNeed: TContainNeeds; const aChars: array of ANSIChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Contains(const aNeed: TContainNeeds; const aStrings: array of ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function EndsWith(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean;
      function EqualsText(const aString: ANSIString): Boolean;

      function Find(const aChar: ANSIChar; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Find(const aChar: ANSIString; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Find(const aChar: ANSIChar; var aPos: TCharIndexArray; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer; overload;
      function Find(const aChar: ANSIString; var aPos: TCharIndexArray; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer; overload;
//      function FindFirst(const aChar: ANSIChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
//      function FindFirst(const aChar: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
      function FindLast(const aChar: ANSIChar; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function FindLast(const aChar: ANSIString; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
//      function FindLast(const aChar: ANSIChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
//      function FindLast(const aChar: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;

      function IndexIn(const aArray: array of ANSIString; const aCaseMode: TCaseSensitivity = csIgnoreCase): Integer;
      function IsOneOf(const aArray: array of ANSIString; const aCaseMode: TCaseSensitivity = csIgnoreCase): Boolean;

      function Leading(const aCount: Integer): ANSIString;
      function Substr(const aStart: Integer; const aCount: Integer): ANSIString;
      function Trailing(const aCount: Integer): ANSIString;

      function Split(const aChar: ANSIChar; var aLeft, aRight: ANSIString): Boolean; overload;
      function Split(const aChar: ANSIChar; var aParts: TANSIStringArray): Boolean; overload;

      function Embrace(const aBraceChar: ANSIChar = '('): ANSIString;
      function Enquote(const aQuoteChar: ANSIChar = ''''): ANSIString;
      function Lowercase: ANSIString;
      function PadLeft(const aCount: Integer; const aChar: ANSIChar): ANSIString; overload;
      function PadRight(const aCount: Integer; const aChar: ANSIChar): ANSIString; overload;
      function ExtendLeft(const aMaxLen: Integer; const aChar: ANSIChar = ' '): ANSIString; overload;
      function ExtendRight(const aMaxLen: Integer; const aChar: ANSIChar = ' '): ANSIString; overload;

      function Remove(const aScope: TStringScope; const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): ANSIString;
      function Replace(const aScope: TStringScope; const aFindStr, aReplaceStr: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): ANSIString;
      function Uppercase: ANSIString;

      function Delete(const aStart, aCount: Integer): ANSIString;
      function Extract(const aStart, aCount: Integer): ANSIString;
      function ExtractLeading(const aCount: Integer): ANSIString;
      function ExtractTrailing(const aCount: Integer): ANSIString;
      function RemoveLeading(const aCount: Integer): ANSIString; overload;
      function RemoveLeading(const aChar: ANSIChar = ' '): ANSIString; overload;
      function RemoveTrailing(const aCount: Integer): ANSIString; overload;
      function RemoveTrailing(const aChar: ANSIChar = ' '): ANSIString; overload;
      function Trim(const aCount: Integer): ANSIString; overload;
      function Trim(const aChar: ANSIChar = ' '): ANSIString; overload;
      function Unbrace: ANSIString;
      function Unquote: ANSIString;

      property Value: ANSIString read get_Value write set_Value;
    end;


    IUTF8String = interface(IStringBase)
      function BeginsWith(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function BeginsWith(const aString: UTF8String; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function BeginsWith(const aString: WIDEString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function CompareWith(const aString: UTF8String; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer;
      function Contains(const aChar: ANSIChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Contains(const aChar: WIDEChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Contains(const aString: UTF8String; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
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
      function AllocANSI: PANSIChar;
      function AllocUTF8: PUTF8Char;
      function AllocWIDE: PWIDEChar;
      function BeginsWith(const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean;
      function CompareWith(const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer;
      function Contains(const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Contains(const aChar: WIDEChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Contains(const aNeed: TContainNeeds; const aChars: array of WIDEChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Contains(const aNeed: TContainNeeds; const aStrings: array of UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function CopyFrom(const aStart, aCount: Integer): UnicodeString;
      function CopyRange(const aStart, aEnd: Integer): UnicodeString;
      procedure CopyTo(const aDest: PANSIChar; const aMaxChars: Integer = -1); overload;
      procedure CopyTo(const aDest: PUTF8Char; const aMaxChars: Integer = -1); overload;
      procedure CopyTo(const aDest: PWIDEChar; const aMaxChars: Integer = -1); overload;
      function Data: PWIDEChar;
      function EndsWith(const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean;
      function EqualsText(const aString: UnicodeString): Boolean;

      function Find(const aChar: WIDEChar; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Find(const aChar: UnicodeString; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Find(const aChar: WIDEChar; var aPos: TCharIndexArray; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer; overload;
      function Find(const aChar: UnicodeString; var aPos: TCharIndexArray; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer; overload;
//      function FindFirst(const aChar: WIDEChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
//      function FindFirst(const aChar: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
      function FindLast(const aChar: WIDEChar; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function FindLast(const aChar: UnicodeString; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
//      function FindLast(const aChar: WIDEChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
//      function FindLast(const aChar: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;

      function IndexIn(const aArray: array of UnicodeString; const aCaseMode: TCaseSensitivity = csIgnoreCase): Integer;
      function IsOneOf(const aArray: array of UnicodeString; const aCaseMode: TCaseSensitivity = csIgnoreCase): Boolean;
      function Leading(const aCount: Integer): UnicodeString;
      function Trailing(const aCount: Integer): UnicodeString;
      function Split(const aChar: ANSIChar; var aLeft, aRight: UnicodeString): Boolean; overload;
      function Split(const aChar: WIDEChar; var aLeft, aRight: UnicodeString): Boolean; overload;
      function Split(const aChar: ANSIChar; var aParts: TWIDEStringArray): Boolean; overload;
      function Split(const aChar: WIDEChar; var aParts: TWIDEStringArray): Boolean; overload;

      function Delete(const aStart, aCount: Integer): UnicodeString;
      function Embrace(const aBraceChar: WIDEChar = '('): UnicodeString;
      function Enquote(const aQuoteChar: WIDEChar = ''''): UnicodeString;
      function ExtractLeft(const aCount: Integer): UnicodeString;
      function ExtractRight(const aCount: Integer): UnicodeString;
      function Lowercase: UnicodeString;
      function PadLeft(const aCount: Integer; const aChar: WIDEChar): UnicodeString; overload;
      function PadRight(const aCount: Integer; const aChar: WIDEChar): UnicodeString; overload;
      function PadToLengthLeft(const aMaxLen: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;
      function PadToLengthRight(const aMaxLen: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;
      function Remove(const aScope: TStringScope; const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): UnicodeString; overload;
      function Replace(const aScope: TStringScope; const aFindStr, aReplaceStr: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): UnicodeString; overload;
      function RemoveLeading(const aChar: WIDEChar = ' '): UnicodeString; overload;
      function RemoveLeading(const aCount: Integer): UnicodeString; overload;
      function RemoveTrailing(const aChar: WIDEChar = ' '): UnicodeString; overload;
      function RemoveTrailing(const aCount: Integer): UnicodeString; overload;
      function Trim(const aChar: WIDEChar = ' '): UnicodeString; overload;
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
  {$ifNdef DELPHIXE3_OR_LATER}
    StrUtils,
  {$endif}  
    Windows,
    Deltics.Classes,
    Deltics.SysUtils,
    Deltics.Strings.FXUtils,
    Deltics.Strings.ANSI,
    Deltics.Strings.UTF8,
    Deltics.Strings.WIDE;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.AllocANSI(const aSource: ANSIString): PANSIChar;
  var
    len: Integer;
  begin
    len     := Length(aSource) + 1;
    result  := AllocMem(len);

    CopyMemory(result, PANSIChar(aSource), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.AllocUTF8(const aSource: ANSIString): PUTF8Char;
  var
    s: UTF8String;
    len: Integer;
  begin
    s := UTF8.FromANSI(aSource);

    len     := Length(s) + 1;
    result  := AllocMem(len);

    CopyMemory(result, PUTF8Char(s), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.AllocWIDE(const aSource: ANSIString): PWIDEChar;
  var
    s: WIDEString;
    len: Integer;
  begin
    s := WIDE.FromANSI(aSource);

    len     := (Length(s) + 1) * 2;
    result  := AllocMem(len);

    CopyMemory(result, PWIDEChar(s), len);
  end;


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
  class function ANSIFn.Compare(const A, B: ANSIString;
                                const aCaseMode: TCaseSensitivity): TCompareResult;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                             PANSIChar(A), Length(A),
                             PANSIChar(B), Length(B)) - CSTR_EQUAL;

    result := IntToCompareResult(result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Concat(const aArray: array of ANSIString): ANSIString;
  var
    i: Integer;
    len: Integer;
    pResult: PANSIChar;
  begin
    case Length(aArray) of
      0: result := '';
      1: result := aArray[0];
    else
      len := 0;
      for i := 0 to Pred(Length(aArray)) do
        Inc(len, Length(aArray[i]));

      SetLength(result, len);
      if len = 0 then
        EXIT;

      pResult := PANSIChar(result);
      for i := 0 to Pred(Length(aArray)) do
      begin
        len := Length(aArray[i]);

        case len of
          0 : { NO-OP} ;
          1 : begin
                pResult^ := aArray[i][1];
                Inc(pResult);
              end;
        else
          CopyMemory(pResult, PANSIChar(aArray[i]), len);
          Inc(pResult, len);
        end;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Concat(const aArray: array of ANSIString;
                               const aSeparator: ANSIString): ANSIString;
  var
    p: PANSIChar;

    procedure DoValue(const aIndex: Integer);
    var
      value: ANSIString;
      len: Integer;
    begin
      value := aArray[aIndex];
      len   := Length(value);

      case len of
        0 : { NO-OP} ;
        1 : begin
              p^ := value[1];
              Inc(p);
            end;
      else
        CopyMemory(p, PANSIChar(value), len);
        Inc(p, len);
      end;
    end;

    procedure DoWithChar(const aChar: ANSIChar);
    var
      i: Integer;
    begin
      for i := 0 to High(aArray) - 1 do
      begin
        DoValue(i);

        p^ := aChar;
        Inc(p);
      end;
    end;

    procedure DoWithString(const aLength: Integer);
    var
      i: Integer;
    begin
      for i := 0 to High(aArray) - 1 do
      begin
        DoValue(i);

        CopyMemory(p, PANSIChar(aSeparator), aLength);
        Inc(p, aLength);
      end;
    end;

  var
    i: Integer;
    len: Integer;
    sepLen: Integer;
  begin
    case Length(aArray) of
      0: result := '';
      1: result := aArray[0];
    else
      sepLen := Length(aSeparator);

      len := (Length(aArray) - 1) * seplen;
      for i := 0 to Pred(Length(aArray)) do
        Inc(len, Length(aArray[i]));

      SetLength(result, len);
      if len = 0 then
        EXIT;

      p := PANSIChar(result);

      case sepLen of
        1 : DoWithChar(aSeparator[1]);
      else
        DoWithString(sepLen);
      end;

      DoValue(High(aArray));
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Format(const aString: ANSIString;
                               const aValue: Double): ANSIString;
  begin
    result := Format(aString, [aValue]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Format(const aString: ANSIString;
                               const aValue: Integer): ANSIString;
  begin
    result := Format(aString, [aValue]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Format(const aString: ANSIString;
                               const aArgs: array of const): ANSIString;
  begin
    result := Format(aString, aArgs);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure ANSIFn.CopyToBuffer(const aSource: ANSIString;
                                      const aDest: PANSIChar;
                                      const aMaxBytes: Integer);
  var
    len: Integer;
  begin
    len := Length(aSource);

    case aMaxBytes of
      -1  : { NO-OP};
       0  : EXIT;
    else
      if (len > aMaxBytes) then
        len := aMaxBytes;
    end;

    CopyMemory(aDest, PANSIChar(aSource), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure ANSIFn.CopyToBuffer(const aSource: ANSIString;
                                      const aDest: PUTF8Char;
                                      const aMaxBytes: Integer);
  var
    len: Integer;
    s: UTF8String;
  begin
    s   := UTF8.FromANSI(aSource);
    len := Length(s);

    case aMaxBytes of
      -1  : { NO-OP};
       0  : EXIT;
    else
      if (len > aMaxBytes) then
        len := aMaxBytes;
    end;

    CopyMemory(aDest, PUTF8Char(s), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure ANSIFn.CopyToBuffer(const aSource: ANSIString;
                                      const aDest: PWIDEChar;
                                      const aMaxChars: Integer);
  var
    len: Integer;
    s: UnicodeString;
  begin
    s   := WIDE.FromANSI(aSource);
    len := Length(s);

    case aMaxChars of
      -1  : { NO-OP};
       0  : EXIT;
    else
      if (len > aMaxChars) then
        len := aMaxChars;
    end;

    CopyMemory(aDest, PWIDEChar(s), len * 2);
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
  class function ANSIFn.Embrace(const aString: ANSIString;
                                const aBraceChar: ANSIChar): ANSIString;
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
  class function ANSIFn.Enquote(const aString: ANSIString): ANSIString;
  begin
    result := Enquote(aString, '''', '''');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Enquote(const aString: ANSIString;
                                const aQuoteChar: ANSIChar): ANSIString;
  begin
    result := Enquote(aString, aQuoteChar, aQuoteChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Enquote(const aString: ANSIString;
                                const aQuoteChar: ANSIChar;
                                const aEscapeChar: ANSIChar): ANSIString;
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
        if (pc^ = aQuoteChar) then
        begin
          result[j] := aEscapeChar;
          Inc(j);
        end;

        result[j] := pc^;
        Inc(j);

        Inc(pc);
      end;
    end;

    result[1] := aQuoteChar;
    result[j] := aQuoteChar;
    SetLength(result, j);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.ExtendLeft(const aString: ANSIString;
                                   const aLength: Integer;
                                   const aChar: ANSIChar): ANSIString;
  var
    fill: Integer;
  begin
    fill := Max(0, aLength - System.Length(aString));
    if fill > 0 then
      result := PadLeft(aString, fill, aChar)
    else
      result := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.ExtendRight(const aString: ANSIString;
                                    const aLength: Integer;
                                    const aChar: ANSIChar): ANSIString;
  var
    fill: Integer;
  begin
    fill := Max(0, aLength - System.Length(aString));
    if fill > 0 then
      result := PadRight(aString, fill, aChar)
    else
      result := aString;
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
  class function ANSIFn.FromWIDE(const aBuffer: PWIDEChar;
                                 const aMaxLen: Integer): ANSIString;
  var
    len: Integer;
  begin
    result := '';
    if (aMaxLen = 0) then
      EXIT;

    len := WideCharToMultiByte(CP_ACP, 0, aBuffer, aMaxLen, NIL, 0, NIL, NIL);
    if aMaxLen = -1 then
      Dec(len);

    SetLength(result, len);
    WideCharToMultiByte(CP_ACP, 0, aBuffer, aMaxLen, PANSIChar(result), System.Length(result), NIL, NIL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.FromWIDE(const aString: UnicodeString): ANSIString;
  begin
    result := FromWIDE(PWIDEChar(aString), System.Length(aString));
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
  class function ANSIFn.Find(const aString: ANSIString;
                             const aChar: ANSIChar;
                             var   aPos: Integer): Boolean;
  var
    i: Integer;
    p: PANSIChar;
  begin
    p := PANSIChar(aString);

    for i := 1 to Length(aString) do
    begin
      if (p^ = aChar) then
      begin
        aPos    := i;
        result  := TRUE;
        EXIT;
      end;

      Inc(p);
    end;

    aPos    := 0;
    result  := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Find(const aString: ANSIString;
                             const aSubstr: ANSIString;
                             var   aPos: Integer): Boolean;
  begin
    if aPos < 1 then
      aPos := 1;

  {$ifdef DELPHIXE3_OR_LATER}
    aPos := Pos(aSubstr, aString, aPos);
  {$else}
    aPos := PosEx(aSubstr, aString, aPos);
  {$endif}

    result  := (aPos > 0);
  end;
(*
  var
    i, j: Integer;
    p: PANSIChar;
    sub: PANSIChar;
    subLen: Integer;
    pc, subc: PANSIChar;
  begin
    result  := FALSE;
    
    p       := PANSIChar(aString);
    sub     := PANSIChar(aSubStr);
    subLen  := Length(aSubStr);

    for i := 1 to Length(aString) - subLen do
    begin
      if p^ = sub^ then
      begin
        pc    := p;
        subc  := sub;

        for j := 2 to subLen do
        begin
          Inc(pc);
          Inc(subc);

          result := pc^ = subc^;
          if NOT result then
            BREAK;
        end;

        if result then
        begin
          aPos := i;
          EXIT;
        end;
      end;

      Inc(p);
    end;

    aPos := 0;
  end;
*)


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.FindNext(const aString: ANSIString;
                                 const aChar: ANSIChar;
                                 var   aPos: Integer): Boolean;
  var
    i: Integer;
    p: PANSIChar;
    iters: Integer;
  begin
    result := FALSE;

    if aPos < 0 then
      aPos := 0;

    iters := Length(aString) - aPos;

    p := PANSIChar(aString);
    Inc(p, aPos - 1);

    for i := 0 to iters do
    begin
      if (p^ = aChar) then
      begin
        aPos    := aPos + i;
        result  := TRUE;
        EXIT;
      end;

      Inc(p);
    end;

    aPos := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.FindNext(const aString: ANSIString;
                                 const aSubstr: ANSIString;
                                 var   aPos: Integer): Boolean;
  begin
    if aPos < 1 then
      aPos := 1;

  {$ifdef DELPHIXE3_OR_LATER}
    aPos := Pos(aSubstr, aString, aPos);
  {$else}
    aPos := PosEx(aSubstr, aString, aPos);
  {$endif}

    result  := (aPos > 0);
  end;
(*
  var
    i, j: Integer;
    p: PANSIChar;
    sub: PANSIChar;
    subLen: Integer;
    iters: Integer;
    pc, subc: PANSIChar;
  begin
    if aPos < 1 then
      aPos := 1;

    p       := PANSIChar(aString);
    sub     := PANSIChar(aSubStr);
    subLen  := Length(aSubStr);

    Inc(p, aPos - 1);

    iters := (Length(aString) - subLen) - aPos + 1;

    for i := 0 to iters do
    begin
      if (p^ = sub^) then
      begin
        pc    := p;
        subc  := sub;

        for j := 2 to subLen do
        begin
          Inc(pc);
          Inc(subc);

          result := pc^ = subc^;
          if NOT result then
            BREAK
          else if (j = subLen) then
          begin
            aPos := aPos + i;
            EXIT;
          end;
        end;
      end;

      Inc(p);
    end;

    aPos    := 0;
    result  := FALSE;
  end;
*)


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
                                const aChar: ANSIChar): ANSIString;
  var
    strLen: Integer;
    p: PANSIChar;
  begin
    strLen := System.Length(aString);
    SetLength(result, strLen + aCount);

    p := PANSIChar(result);
    if aCount > 0 then
    begin
      FillMemory(p, aCount, Byte(aChar));
      Inc(p, aCount);
    end;

    if strLen > 0 then
      CopyMemory(p, PANSIChar(aString), strLen);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.PadRight(const aString: ANSIString;
                                 const aCount: Integer;
                                 const aChar: ANSIChar): ANSIString;
  var
    strLen: Integer;
    p: PANSIChar;
  begin
    strLen := System.Length(aString);
    SetLength(result, strLen + aCount);

    p := PANSIChar(result);
    if strLen > 0 then
    begin
      CopyMemory(p, PANSIChar(aString), strLen);
      Inc(p, strLen);
    end;

    if aCount > 0 then
      FillMemory(p, aCount, Byte(aChar));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Remove(const aScope: TStringScope;
                               const aString: ANSIString;
                               const aStringToRemove: ANSIString;
                               const aCaseMode: TCaseSensitivity): ANSIString;
  var
    i: Integer;
    pa: TCharIndexArray;
    pr, ps: PANSIChar;
    p, flen, rlen, clen: Integer;
  begin
    if (aScope = ssFirst) then
    begin
      SetLength(pa, 1);
      if ANSI(aString).Find(aStringToRemove, p, aCaseMode) then
        pa[0] := p
      else
        SetLength(pa, 0);
    end
    else
      ANSI(aString).Find(aStringToRemove, pa, aCaseMode);

    if Length(pa) = 0 then
      EXIT;

    if (aScope = ssLast) then
    begin
      pa[0] := pa[Length(pa) - 1];
      SetLength(pa, 1);
    end;

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


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Replace(const aScope: TStringScope;
                                const aString: ANSIString;
                                const aFindStr: ANSIString;
                                const aReplaceStr: ANSIString;
                                const aCaseMode: TCaseSensitivity): ANSIString;
  var
    i: Integer;
    pa: TCharIndexArray;
    pr, ps, px: PANSIChar;
    p, flen, xlen, rlen, clen: Integer;
  begin
    if (aScope = ssFirst) then
    begin
      SetLength(pa, 1);
      if ANSI(aString).Find(aFindStr, p, aCaseMode) then
        pa[0] := p
      else
        SetLength(pa, 0);
    end
    else
      ANSI(aString).Find(aFindStr, pa, aCaseMode);

    case Length(pa) of
      0 : EXIT;
      1 : { NO-OP };
    else
      if (aScope = ssLast) then
      begin
        pa[0] := pa[Length(pa) - 1];
        SetLength(pa, 1);
      end;
    end;

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


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.SameString(const A, B: ANSIString): Boolean;
  begin
    result := (Length(A) = Length(B))
          and (CompareStringA(LOCALE_USER_DEFAULT, 0,
                              PANSIChar(A), Length(A),
                              PANSIChar(B), Length(B)) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.SameText(const A, B: ANSIString): Boolean;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PANSIChar(A), Length(A),
                             PANSIChar(B), Length(B)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.StringOf(const aChar: ANSIChar;
                                 const aCount: Integer): ANSIString;
  var
    i: Integer;
  begin
    SetLength(result, aCount);

    for i := Pred(aCount) downto 0 do
      result[i + 1] := aChar;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.StringOf(const aString: ANSIString;
                                 const aCount: Integer): ANSIString;
  var
    i: Integer;
    bytes: Integer;
    pr: PANSIChar;
  begin
    SetLength(result, System.Length(aString) * aCount);

    if aCount = 0 then
      EXIT;

    bytes := System.Length(aString);
    pr    := PANSIChar(result);

    for i := 1 to aCount do
    begin
      CopyMemory(pr, PANSIChar(aString), bytes);
      Inc(pr, bytes);
    end;
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Trim(const aString: ANSIString;
                             const aCount: Integer): ANSIString;
  begin
    result := aString;
    result := RemoveLeading(result, aCount);
    result := RemoveTrailing(result, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Trim(const aString: ANSIString;
                             const aChar: ANSIChar): ANSIString;
  begin
    result := aString;
    result := RemoveLeading(result, aChar);
    result := RemoveTrailing(result, aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.RemoveLeading(const aString: ANSIString;
                                      const aCount: Integer): ANSIString;
  begin
    result := System.Copy(aString, aCount + 1, System.Length(aString) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.RemoveLeading(const aString: ANSIString;
                                      const aChar: ANSIChar): ANSIString;
  var
    i, p: Integer;
    len: Integer;
    pc: PANSIChar;
  begin
    p   := 1;
    pc  := PANSIChar(aString);
    len := System.Length(aString);

    for i := 1 to len do
    begin
      if (pc^ <> aChar) then
      begin
        p := i;
        BREAK;
      end
      else
        Inc(pc);
    end;

    if (p > 1) then
      result := System.Copy(aString, p, 1 + len - p)
    else
      result := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.RemoveTrailing(const aString: ANSIString;
                                       const aChar: ANSIChar): ANSIString;
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
    begin
      if (pc^ <> aChar) then
      begin
        p := i;
        BREAK;
      end
      else
        Dec(pc);
    end;

    if (p < len) then
      SetLength(result, p);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.RemoveTrailing(const aString: ANSIString;
                                  const aCount: Integer): ANSIString;
  begin
    result := aString;
    SetLength(result, System.Length(result) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSIFn.Unbrace(const aString: ANSIString): ANSIString;
  const
    MATCH_SET : set of ANSIChar = ['!','@','#','%','&','*','-','_',
                                   '+','=',':','/','?','\','|','~'];
  var
    rlen: Integer;
    firstChar: ANSIChar;
    lastChar: ANSIChar;
  begin
    rlen := Length(aString) - 2;
    if rlen < 0 then
      EXIT;

    // First determine whether the string is braced - if not we return the
    //  original string

    result := aString;

    firstChar := aString[1];
    lastChar  := aString[Length(aString)];

    case firstChar of
      '{' : if lastChar <> '}' then EXIT;
      '<' : if lastChar <> '>' then EXIT;
      '(' : if lastChar <> ')' then EXIT;
      '[' : if lastChar <> ']' then EXIT;
    else
      if NOT (firstChar in MATCH_SET)
          or (lastChar <> firstChar) then
        EXIT;
    end;

    SetLength(result, rlen);

    if rlen > 0 then
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
  class function UTF8Fn.Compare(const A, B: UTF8String;
                                const aCaseMode: TCaseSensitivity): Integer;
  begin
    result := WIDE.Compare(WIDE.FromUTF8(A), WIDE.FromUTF8(B), aCaseMode);
  end;


















{ WIDEFn ----------------------------------------------------------------------------------------- }

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
  class function WIDEFn.FromANSI(const aString: ANSIString): UnicodeString;
  begin
    result := FromANSI(PANSIChar(aString), System.Length(aString));
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

    // If aMaxLen is -1 then the reported length INCLUDES the null terminator
    //  which is automatically part of result

    if (aMaxLen = -1) then
      Dec(len);

    SetLength(result, len);
    MultiByteToWideChar(CP_ACP, 0, aBuffer, aMaxLen, @result[1], len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.FromBuffer(const aBuffer: PWIDEChar;
                                   const aMaxLen: Integer): UnicodeString;
  begin
    result := '';

    if aMaxLen = 0 then
      EXIT;

    case aBuffer[0] of
      BOMCHAR_UTF16LE : begin
                          if aMaxLen = 1 then
                            EXIT;

                          SetLength(result, aMaxLen - 1);
                          CopyMemory(@result[1], @aBuffer[1], (aMaxLen - 1) * 2);
                        end;

      BOMCHAR_UTF16BE : begin
                          if aMaxLen = 1 then
                            EXIT;

                          SetLength(result, aMaxLen - 1);
                          CopyMemory(@result[1], @aBuffer[1], (aMaxLen - 1) * 2);
                          ReverseBytes(System.PWord(@result[1]), aMaxLen - 1);
                        end;
    else
      SetLength(result, aMaxLen);
      CopyMemory(@result[1], @aBuffer[0], aMaxLen * 2);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.FromUTF8(const aString: UTF8String): UnicodeString;
  begin
    result := FromUTF8(PUTF8Char(aString), System.Length(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.AllocANSI(const aSource: UnicodeString): PANSIChar;
  var
    len: Integer;
    s: ANSIString;
  begin
    s := ANSI.FromWIDE(aSource);

    len     := Length(s) + 1;
    result  := AllocMem(len);

    CopyMemory(result, PANSIChar(s), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.AllocUTF8(const aSource: UnicodeString): PUTF8Char;
  var
    len: Integer;
    s: UTF8String;
  begin
    s := UTF8.FromWIDE(aSource);

    len     := Length(s) + 1;
    result  := AllocMem(len);

    CopyMemory(result, PUTF8Char(s), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.AllocWIDE(const aSource: UnicodeString): PWIDEChar;
  var
    bytes: Integer;
  begin
    bytes   := (Length(aSource) + 1) * 2;
    result  := AllocMem(bytes);

    CopyMemory(result, PWIDEChar(aSource), bytes);
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
  class function WIDEFn.Compare(const A, B: UnicodeString;
                                const aCaseMode: TCaseSensitivity): Integer;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                             PWIDEChar(A), System.Length(A),
                             PWIDEChar(B), System.Length(B)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.SameString(const A, B: UnicodeString): Boolean;
  begin
    result := (Length(A) = Length(B))
          and (CompareStringW(LOCALE_USER_DEFAULT, 0,
                              PWIDEChar(A), Length(A),
                              PWIDEChar(B), Length(B)) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.SameText(const A, B: UnicodeString): Boolean;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PWIDEChar(A), Length(A),
                             PWIDEChar(B), Length(B)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Concat(const aArray: array of UnicodeString): UnicodeString;
  var
    i: Integer;
    len: Integer;
    pResult: PWIDEChar;
  begin
    case Length(aArray) of
      0: result := '';
      1: result := aArray[0];
    else
      len := 0;
      for i := 0 to Pred(Length(aArray)) do
        Inc(len, Length(aArray[i]));

      SetLength(result, len);
      if len = 0 then
        EXIT;

      pResult := PWIDEChar(result);

      for i := 0 to Pred(Length(aArray)) do
      begin
        len := Length(aArray[i]);

        case len of
          0 : { NO-OP} ;
          1 : begin
                pResult^ := aArray[i][1];
                Inc(pResult);
              end;
        else
          CopyMemory(pResult, PWIDEChar(aArray[i]), len * 2);
          Inc(pResult, len);
        end;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Concat(const aArray: array of UnicodeString;
                               const aSeparator: UnicodeString): UnicodeString;
  var
    p: PWIDEChar;

    procedure DoValue(const aIndex: Integer);
    var
      value: WIDEString;
      len: Integer;
    begin
      value := aArray[aIndex];
      len   := Length(value);

      case len of
        0 : { NO-OP} ;
        1 : begin
              p^ := value[1];
              Inc(p);
            end;
      else
        CopyMemory(p, PWIDEChar(value), len * 2);
        Inc(p, len);
      end;
    end;

    procedure DoWithChar(const aChar: WIDEChar);
    var
      i: Integer;
    begin
      for i := 0 to High(aArray) - 1 do
      begin
        DoValue(i);

        p^ := aChar;
        Inc(p);
      end;
    end;

    procedure DoWithString(const aLength: Integer);
    var
      i: Integer;
    begin
      for i := 0 to High(aArray) - 1 do
      begin
        DoValue(i);

        CopyMemory(p, PWIDEChar(aSeparator), aLength * 2);
        Inc(p, aLength);
      end;
    end;

  var
    i: Integer;
    len: Integer;
    sepLen: Integer;
  begin
    case Length(aArray) of
      0: result := '';
      1: result := aArray[0];
    else
      sepLen := Length(aSeparator);

      len := (Length(aArray) - 1) * seplen;
      for i := 0 to Pred(Length(aArray)) do
        Inc(len, Length(aArray[i]));

      SetLength(result, len);
      if len = 0 then
        EXIT;

      p := PWIDEChar(result);

      case sepLen of
        1 : DoWithChar(aSeparator[1]);
      else
        DoWithString(sepLen);
      end;

      DoValue(High(aArray));
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Format(const aString: UnicodeString;
                               const aValue: Double): UnicodeString;
  begin
    result := WIDEFormat(aString, [aValue]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Format(const aString: UnicodeString;
                               const aValue: Integer): UnicodeString;
  begin
    result := WIDEFormat(aString, [aValue]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Format(const aString: UnicodeString;
                               const aArgs: array of const): UnicodeString;
  begin
    result := WIDEFormat(aString, aArgs);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure WIDEFn.CopyToBuffer(const aSource: UnicodeString;
                                      const aDest: PANSIChar;
                                      const aMaxBytes: Integer);
  var
    len: Integer;
    s: ANSIString;
  begin
    s   := ANSI.FromWIDE(aSource);
    len := Length(s);

    case aMaxBytes of
      -1  : { NO-OP};
       0  : EXIT;
    else
      if (len > aMaxBytes) then
        len := aMaxBytes;
    end;

    CopyMemory(aDest, PANSIChar(s), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure WIDEFn.CopyToBuffer(const aSource: UnicodeString;
                                      const aDest: PUTF8Char;
                                      const aMaxBytes: Integer);
  var
    len: Integer;
    s: UTF8String;
  begin
    s   := UTF8.FromWIDE(aSource);
    len := Length(s);

    case aMaxBytes of
      -1  : { NO-OP};
       0  : EXIT;
    else
      if (len > aMaxBytes) then
        len := aMaxBytes;
    end;

    CopyMemory(aDest, PUTF8Char(s), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure WIDEFn.CopyToBuffer(const aSource: UnicodeString;
                                      const aDest: PWIDEChar;
                                      const aMaxChars: Integer);
  var
    len: Integer;
  begin
    len := Length(aSource);

    case aMaxChars of
      -1  : { NO-OP};
       0  : EXIT;
    else
      if (len > aMaxChars) then
        len := aMaxChars;
    end;

    CopyMemory(aDest, PWIDEChar(aSource), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Embrace(const aString: UnicodeString;
                                const aBraceChar: WIDEChar): UnicodeString;
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
      result[rlen] := WIDEChar(aBraceChar);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Enquote(const aString: UnicodeString): UnicodeString;
  begin
    result := Enquote(aString, WIDEChar(''''), WIDEChar(''''));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Enquote(const aString: UnicodeString;
                                const aQuoteChar: WIDEChar): UnicodeString;
  begin
    result := Enquote(aString, aQuoteChar, aQuoteChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Enquote(const aString: UnicodeString;
                                const aQuoteChar: WIDEChar;
                                const aEscapeChar: WIDEChar): UnicodeString;
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
        if (pc^ = aQuoteChar) then
        begin
          result[j] := aEscapeChar;
          Inc(j);
        end;

        result[j] := pc^;
        Inc(j);

        Inc(pc);
      end;
    end;

    result[1] := aQuoteChar;
    result[j] := aQuoteChar;
    SetLength(result, j);
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
  class function WIDEFn.Find(const aString: UnicodeString;
                             const aChar: WIDEChar;
                             var   aPos: Integer): Boolean;
  var
    i: Integer;
    p: PWIDEChar;
  begin
    p := PWIDEChar(aString);

    for i := 1 to Length(aString) do
    begin
      if (p^ = aChar) then
      begin
        aPos    := i;
        result  := TRUE;
        EXIT;
      end;

      Inc(p);
    end;

    aPos    := 0;
    result  := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Find(const aString: UnicodeString;
                             const aSubstr: UnicodeString;
                             var   aPos: Integer): Boolean;
{$ifdef DELPHIXE3_OR_LATER}
  begin
    if aPos < 1 then
      aPos := 1;

    aPos    := Pos(aSubstr, aString, aPos);
    result  := (aPos > 0);
  end;
{$else}
  var
    i, j: Integer;
    p: PWIDEChar;
    sub: PWIDEChar;
    subLen: Integer;
    pc, subc: PWIDEChar;
  begin
    result  := FALSE;

    p       := PWIDEChar(aString);
    sub     := PWIDEChar(aSubStr);
    subLen  := Length(aSubStr);

    for i := 1 to Length(aString) - subLen do
    begin
      if p^ = sub^ then
      begin
        pc    := p;
        subc  := sub;

        for j := 2 to subLen do
        begin
          Inc(pc);
          Inc(subc);

          result := pc^ = subc^;
          if NOT result then
            BREAK;
        end;

        if result then
        begin
          aPos := i;
          EXIT;
        end;
      end;

      Inc(p);
    end;

    aPos := 0;
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.FindNext(const aString: UnicodeString;
                                 const aChar: WIDEChar;
                                 var   aPos: Integer): Boolean;
{$ifdef DELPHIXE3_OR_LATER}
  begin
    if aPos < 1 then
      aPos := 1;

    aPos    := Pos(aChar, aString, aPos);
    result  := aPos <> 0;
  end;
{$else}
  var
    i: Integer;
    p: PWIDEChar;
    iters: Integer;
  begin
    result := FALSE;

    if aPos < 0 then
      aPos := 0;

    iters := Length(aString) - aPos;

    p := PWIDEChar(aString);
    Inc(p, aPos - 1);

    for i := 0 to iters do
    begin
      if (p^ = aChar) then
      begin
        aPos    := aPos + i;
        result  := TRUE;
        EXIT;
      end;

      Inc(p);
    end;

    aPos := 0;
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.FindNext(const aString: UnicodeString;
                                 const aSubstr: UnicodeString;
                                 var   aPos: Integer): Boolean;
{$ifdef DELPHIXE3_OR_LATER}
  begin
    if aPos < 1 then
      aPos := 1;

    aPos    := Pos(aSubstr, aString, aPos);
    result  := aPos <> 0;
  end;
{$else}
  var
    i, j: Integer;
    p: PWIDEChar;
    sub: PWIDEChar;
    subLen: Integer;
    iters: Integer;
    pc, subc: PWIDEChar;
  begin
    if aPos < 1 then
      aPos := 1;

    p       := PWIDEChar(aString);
    sub     := PWIDEChar(aSubStr);
    subLen  := Length(aSubStr);

    Inc(p, aPos - 1);

    iters := (Length(aString) - subLen) - aPos + 1;

    for i := 0 to iters do
    begin
      if (p^ = sub^) then
      begin
        pc    := p;
        subc  := sub;

        for j := 2 to subLen do
        begin
          Inc(pc);
          Inc(subc);

          result := pc^ = subc^;
          if NOT result then
            BREAK
          else if (j = subLen) then
          begin
            aPos := aPos + i;
            EXIT;
          end;
        end;
      end;

      Inc(p);
    end;

    aPos    := 0;
    result  := FALSE;
  end;
{$endif}

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
                                const aChar: WIDEChar): UnicodeString;
  var
    i: Integer;
    strLen: Integer;
    p: PWIDEChar;
  begin
    strLen := System.Length(aString);
    SetLength(result, strLen + aCount);

    p := PWIDEChar(result);
    for i := Pred(aCount) downto 0 do
      p[i] := aChar;

    if strLen > 0 then
    begin
      Inc(p, aCount);
      CopyMemory(p, PWIDEChar(aString), strLen * 2);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.PadRight(const aString: UnicodeString;
                                 const aCount: Integer;
                                 const aChar: WIDEChar): UnicodeString;
  var
    i: Integer;
    strLen: Integer;
    p: PWIDEChar;
  begin
    strLen := System.Length(aString);
    SetLength(result, strLen + aCount);

    p := PWIDEChar(result);
    if strLen > 0 then
    begin
      CopyMemory(p, PWIDEChar(aString), strLen * 2);
      Inc(p, strLen);
    end;

    for i := Pred(aCount) downto 0 do
      p[i] := aChar;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.ExtendLeft(const aString: UnicodeString;
                                   const aLength: Integer;
                                   const aChar: WIDEChar): UnicodeString;
  var
    fill: Integer;
  begin
    fill := Max(0, aLength - System.Length(aString));
    if fill > 0 then
      result := PadLeft(aString, fill, aChar)
    else
      result := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.ExtendRight(const aString: UnicodeString;
                                    const aLength: Integer;
                                    const aChar: WIDEChar): UnicodeString;
  var
    fill: Integer;
  begin
    fill := Max(0, aLength - System.Length(aString));
    if fill > 0 then
      result := PadRight(aString, fill, aChar)
    else
      result := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Remove(const aScope: TStringScope;
                               const aString: UnicodeString;
                               const aStringToRemove: UnicodeString;
                               const aCaseMode: TCaseSensitivity): UnicodeString;
  var
    i: Integer;
    pa: TCharIndexArray;
    pr, ps: PWIDEChar;
    flen, rlen, clen: Integer;
  begin
    if aScope in [ssFirst, ssLast] then
    begin
      SetLength(pa, 1);
      if NOT WIDE(aString).Find(aStringToRemove, pa[0], aCaseMode) then
        SetLength(pa, 0);
    end
    else
      WIDE(aString).Find(aStringToRemove, pa, aCaseMode);

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


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Replace(const aScope: TStringScope;
                                const aString: UnicodeString;
                                const aFindStr: UnicodeString;
                                const aReplaceStr: UnicodeString;
                                const aCaseMode: TCaseSensitivity): UnicodeString;
  var
    i: Integer;
    pa: TCharIndexArray;
    pr, ps, px: PWIDEChar;
    p, flen, xlen, rlen, clen: Integer;
  begin
    if (aScope = ssFirst) then
    begin
      SetLength(pa, 1);
      if WIDE(aString).Find(aFindStr, p, aCaseMode) then
        pa[0] := p
      else
        SetLength(pa, 0);
    end
    else
      WIDE(aString).Find(aFindStr, pa, aCaseMode);

    case Length(pa) of
      0 : EXIT;
      1 : { NO-OP };
    else
      if (aScope = ssLast) then
      begin
        pa[0] := pa[Length(pa) - 1];
        SetLength(pa, 1);
      end;
    end;

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


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.RemoveLeading(const aString: UnicodeString;
                                      const aCount: Integer): UnicodeString;
  begin
    result := System.Copy(aString, aCount + 1, System.Length(aString) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.RemoveLeading(const aString: UnicodeString;
                                      const aChar: WIDEChar): UnicodeString;
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
      result := System.Copy(aString, p, 1 + System.Length(aString) - p)
    else
      result := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.RemoveTrailing(const aString: UnicodeString;
                                       const aCount: Integer): UnicodeString;
  begin
    result := aString;
    SetLength(result, System.Length(result) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.RemoveTrailing(const aString: UnicodeString;
                                       const aChar: WIDEChar): UnicodeString;
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
      SetLength(result, p);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Trim(const aString: UnicodeString;
                             const aCount: Integer): UnicodeString;
  begin
    result := aString;
    result := RemoveLeading(result, aCount);
    result := RemoveTrailing(result, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Trim(const aString: UnicodeString;
                             const aChar: WIDEChar): UnicodeString;
  begin
    result := RemoveLeading(aString, aChar);
    result := RemoveTrailing(result, aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.StringOf(const aChar: WIDEChar;
                                 const aCount: Integer): UnicodeString;
  var
    i: Integer;
  begin
    SetLength(result, aCount);

    for i := Pred(aCount) downto 0 do
      result[i + 1] := aChar;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.StringOf(const aString: UnicodeString;
                                 const aCount: Integer): UnicodeString;
  var
    i: Integer;
    chars: Integer;
    bytes: Integer;
    pr: PWIDEChar;
  begin
    chars := System.Length(aString);
    SetLength(result, chars * aCount);

    bytes := chars * 2;
    pr    := PWIDEChar(result);

    for i := 1 to aCount do
    begin
      CopyMemory(pr, PWIDEChar(aString), bytes);
      Inc(pr, chars);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Unbrace(const aString: UnicodeString): UnicodeString;
  const
    MATCH_SET : set of ANSIChar = ['!','@','#','%','&','*','-','_',
                                   '+','=',':','/','?','\','|','~'];
  var
    rlen: Integer;
    firstChar: WIDEChar;
    lastChar: WIDEChar;
  begin
    rlen := Length(aString) - 2;
    if rlen < 0 then
      EXIT;

    // First determine whether the string is braced - if not we return the
    //  original string

    result := aString;

    firstChar := aString[1];
    lastChar  := aString[Length(aString)];

    if (Ord(firstChar) > 127) or (Ord(lastChar) > 127) then
      EXIT;

    case firstChar of
      '{' : if lastChar <> '}' then EXIT;
      '<' : if lastChar <> '>' then EXIT;
      '(' : if lastChar <> ')' then EXIT;
      '[' : if lastChar <> ']' then EXIT;
    else
      if NOT (ANSIChar(firstChar) in MATCH_SET)
          or (lastChar <> firstChar) then
        EXIT;
    end;

    SetLength(result, rlen);

    if rlen > 0 then
      CopyMemory(PWIDEChar(result), @aString[2], rlen * 2);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDEFn.Unquote(const aString: UnicodeString): UnicodeString;
  var
    i, j, maxi: Integer;
    qc: WIDEChar;
  begin
    if Length(aString) < 2 then
      EXIT;

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







{ Factories -------------------------------------------------------------------------------------- }

  { ANSI - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

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




  { WIDE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

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



  { STR  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

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




  { UTF8 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

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
