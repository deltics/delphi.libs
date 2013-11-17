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

  type
  {$ifNdef UNICODE}
//    UCS4Char    = Cardinal;
//    UCS4String  = array of UCS4Char;

    UTF8String    = type ANSIString;
    UnicodeString = type WideString;
  {$endif}

//    TSetOfChar = set of ANSIChar;

    ASCIIString = type ANSIString;
    ASCIIChar   = type ANSIChar;
    PASCIIChar  = ^ASCIIChar;

    UTF8Char    = type ANSIChar;
    PUTF8Char   = ^UTF8Char;

(*
    UTF32Char       = UCS4Char;
    UTF32CharArray  = array of UCS4Char;
    UTF32String     = UCS4String;
*)
    TCharIndexArray = array of Integer;


    ANSISupport = class
      // Transcoding
      class function FromUTF8(const aString: UTF8String): ANSIString; overload;
      class function FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): ANSIString; overload;
      class function FromWide(const aString: UnicodeString): ANSIString;
      // Analysis
      class function Pos(const aString: ANSIString; const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      class function Pos(const aString: ANSIString; const aSubStr: ANSIString; var aPos: Integer): Boolean; overload;
      class function Pos(const aString: ANSIString; const aChar: ANSIChar; var aPositions: TCharIndexArray): Boolean; overload;
      class function Pos(const aString: ANSIString; const aSubStr: ANSIString; var aPositions: TCharIndexArray): Boolean; overload;
      class function NPos(const aString: ANSIString; const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      class function NPos(const aString: ANSIString; const aSubStr: ANSIString; var aPos: Integer): Boolean; overload;
      class function RPos(const aString: ANSIString; const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
      class function RPos(const aString: ANSIString; const aSubStr: ANSIString; var aPos: Integer): Boolean; overload;
      // Evaluations
      class function Compare(const S1, S2: ANSIString): Integer;
      class function CompareText(const S1, S2: ANSIString): Integer;
      class function Contains(const aString: ANSIString; const aChar: ANSIChar): Boolean; overload;
      class function Contains(const aString, aSubStr: ANSIString): Boolean; overload;
      class function IsLowercase(const aChar: ANSIChar): Boolean; overload;
      class function IsLowercase(const aString: ANSIString): Boolean; overload;
      class function IsUppercase(const aChar: ANSIChar): Boolean; overload;
      class function IsUppercase(const aString: ANSIString): Boolean; overload;
      class function SameText(const S1, S2: ANSIString): Boolean;
      // Transformations
      class function Lowercase(const aString: ANSIString): ANSIString;
      class function Uppercase(const aString: ANSIString): ANSIString;
    end;


    ASCII = class
      // Transcoding
      class function Encode(const aString: String): ASCIIString;
      class function Decode(const aString: ASCIIString): String;
    end;


    UTF8 = class
      // Transcoding
      class function Encode(const aString: String): UTF8String;
      class function Decode(const aString: UTF8String): String; overload;
      class function Decode(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): String; overload;
      class function FromANSI(const aString: ANSIString): UTF8String;
      class function FromWide(const aString: UnicodeString): UTF8String;
      // Analysis
      class function Pos(const aString: UTF8String; const aChar: UTF8Char; var aPos: Integer): Boolean; overload;
      class function Pos(const aString: UTF8String; const aSubStr: UTF8String; var aPos: Integer): Boolean; overload;
      class function Pos(const aString: UTF8String; const aChar: UTF8Char; var aPositions: TCharIndexArray): Boolean; overload;
      class function Pos(const aString: UTF8String; const aSubStr: UTF8String; var aPositions: TCharIndexArray): Boolean; overload;
      class function RPos(const aString: UTF8String; const aChar: UTF8Char; var aPos: Integer): Boolean; overload;
      class function RPos(const aString: UTF8String; const aSubStr: UTF8String; var aPos: Integer): Boolean; overload;
      class function NPos(const aString: UTF8String; const aChar: UTF8Char; var aPos: Integer): Boolean; overload;
      class function NPos(const aString: UTF8String; const aSubStr: UTF8String; var aPos: Integer): Boolean; overload;
      // Evaluations
      class function Compare(const S1, S2: UTF8String): Integer;
      class function CompareText(const S1, S2: UTF8String): Integer;
      class function Contains(const aString: UTF8String; const aChar: UTF8Char): Boolean; overload;
      class function Contains(const aString, aSubStr: UTF8String): Boolean; overload;
      class function IsLowercase(const aString: UTF8String): Boolean;
      class function IsUppercase(const aString: UTF8String): Boolean;
      class function SameText(const S1, S2: UTF8String): Boolean;
      // Transformations
      class function Lowercase(const aString: UTF8String): UTF8String;
      class function Uppercase(const aString: UTF8String): UTF8String;
    end;


    WIDESupport = class
      // Transcoding
      class function FromANSI(const aString: ANSIString): UnicodeString; overload;
      class function FromANSI(const aBuffer: PANSIChar; const aMaxLen: Integer = -1): UnicodeString; overload;
      class function FromASCII(const aString: ASCIIString): UnicodeString; overload;
      class function FromUTF8(const aString: UTF8String): UnicodeString; overload;
      class function FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): UnicodeString; overload;
      // Analysis
      class function Pos(const aString: UnicodeString; const aChar: WideChar; var aPos: Integer): Boolean; overload;
      class function Pos(const aString: UnicodeString; const aSubStr: UnicodeString; var aPos: Integer): Boolean; overload;
      class function Pos(const aString: UnicodeString; const aChar: WideChar; var aPositions: TCharIndexArray): Boolean; overload;
      class function Pos(const aString: UnicodeString; const aSubStr: UnicodeString; var aPositions: TCharIndexArray): Boolean; overload;
      class function RPos(const aString: UnicodeString; const aChar: WideChar; var aPos: Integer): Boolean; overload;
      class function RPos(const aString: UnicodeString; const aSubStr: UnicodeString; var aPos: Integer): Boolean; overload;
      class function NPos(const aString: UnicodeString; const aChar: WideChar; var aPos: Integer): Boolean; overload;
      class function NPos(const aString: UnicodeString; const aSubStr: UnicodeString; var aPos: Integer): Boolean; overload;
      // Evaluations
      class function BeginsWith(const aString, aLead: UnicodeString): Boolean;
      class function BeginsWithText(const aString, aLead: UnicodeString): Boolean;
      class function Compare(const S1, S2: UnicodeString): Integer;
      class function CompareText(const S1, S2: UnicodeString): Integer;
      class function Contains(const aString: UnicodeString; const aChar: WideChar): Boolean; overload;
      class function Contains(const aString, aSubStr: UnicodeString): Boolean; overload;
      class function IsLowercase(const aChar: WideChar): Boolean; overload;
      class function IsLowercase(const aString: UnicodeString): Boolean; overload;
      class function IsUppercase(const aChar: WideChar): Boolean; overload;
      class function IsUppercase(const aString: UnicodeString): Boolean; overload;
      class function SameText(const S1, S2: UnicodeString): Boolean;
      // Transformations
      class function Lowercase(const aString: UnicodeString): UnicodeString;
      class function Uppercase(const aString: UnicodeString): UnicodeString;
    end;


    ANSI = class(ANSISupport)
      // Transcoding
      class function Encode(const aString: String): ANSIString;
      class function Decode(const aString: ANSIString): String;
    end;


    WIDE = class(WIDESupport)
      // Transcoding
      class function Encode(const aString: String): UnicodeString;
      class function Decode(const aString: UnicodeString): String;
    end;


    STR = class({$ifdef UNICODE}WIDESupport
                {$else}         ANSISupport{$endif})
      // Transcoding
    {$ifdef UNICODE}
      class function FromWide(const aString: UnicodeString): String;
    {$else}
      class function FromANSI(const aString: ANSIString): String;
    {$endif}
    end;



implementation

  uses
    SysUtils,
    Windows;


{ Implementation Helpers ------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  type
    TANSICaseFn = function(aChar: ANSIChar): LongBool; stdcall;
    TWideCaseFn = function(aChar: WideChar): LongBool; stdcall;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function CheckCase(const aString: ANSIString;
                     const aCaseFn: TANSICaseFn): Boolean; overload;
  var
    i: Integer;
    bAlpha: Boolean;
  begin
    result  := FALSE;
    bAlpha  := FALSE;

    for i := 0 to Pred(Length(aString)) do
      if Windows.IsCharAlphaA(aString[i]) then
      begin
        if NOT aCaseFn(aString[i]) then
          EXIT;

        bAlpha := TRUE;
      end;

    if bAlpha then
      result := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function CheckCase(const aString: UnicodeString;
                     const aCaseFn: TWideCaseFn): Boolean; overload;
  var
    i: Integer;
    bAlpha: Boolean;
  begin
    result  := FALSE;
    bAlpha  := FALSE;

    for i := 0 to Pred(Length(aString)) do
      if Windows.IsCharAlphaW(aString[i]) then
      begin
        if NOT aCaseFn(aString[i]) then
          EXIT;

        bAlpha := TRUE;
      end;

    if bAlpha then
      result := TRUE;
  end;






{ ANSISupport ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.FromUTF8(const aString: UTF8String): ANSIString;
  begin
    // TODO: Can we do this more directly / efficiently ?
    result := ANSISupport.FromWide(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer): ANSIString;
  begin
    // TODO: Can we do this more directly / efficiently ?
    result := ANSISupport.FromWide(WIDE.FromUTF8(aBuffer, aMaxLen));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.FromWide(const aString: UnicodeString): ANSIString;
  var
    len: Integer;
  begin
    len := WideCharToMultiByte(CP_ACP, 0, PWideChar(aString), -1, NIL, 0, NIL, NIL);
    Dec(len);

    SetLength(result, len);
    WideCharToMultiByte(CP_ACP, 0, PWideChar(aString), Length(aString), PANSIChar(result), Length(result), NIL, NIL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Pos(const aString: ANSIString;
                          const aChar: ANSIChar;
                          var   aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aChar, aString);
    result  := (aPos <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Pos(const aString: ANSIString;
                          const aSubStr: ANSIString;
                          var   aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aSubStr, aString);
    result  := (aPos <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.RPos(const aString: ANSIString;
                           const aChar: ANSIChar;
                           var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    firstChar: PANSIChar;
    currChar: PANSIChar;
  begin
    strLen  := Length(aString);
    aPos    := 0;
    result  := FALSE;
    if (strLen = 0) then
      EXIT;

    firstChar := @aString[1];
    currChar  := @aString[strLen];

    for i := Pred(strLen) downto 0 do
    begin
      result := (currChar^ = aChar);
      if result then
        BREAK;

      Dec(currChar);
    end;

    if result then
      aPos := (currChar - firstChar) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.RPos(const aString: ANSIString;
                                 const aSubStr: ANSIString;
                                 var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PANSIChar;
    currChar: PANSIChar;
    initialChar: PANSIChar;
  begin
    strLen  := Length(aString);
    subLen  := Length(aSubStr);
    aPos    := 0;
    result  := FALSE;
    if (subLen > strLen) or (subLen = 0) or (strLen = 0) then
      EXIT;

    firstChar   := @aString[1];
    initialChar := @aSubStr[1];
    currChar    := @aString[Length(aString)];

    for i := (strLen - subLen) downto 0 do
    begin
      result := (currChar^ = initialChar^);
      if result and CompareMem(currChar, initialChar, subLen) then
        BREAK;

      Dec(currChar);
    end;

    if result then
      aPos := (currChar - firstChar) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.NPos(const aString: ANSIString;
                                 const aChar: ANSIChar;
                                 var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    currChar: PANSIChar;
  begin
  ASSERT(aPos >= 0);

    result  := FALSE;
    strLen  := Length(aString);
    if (aPos >= strLen) then
    begin
      aPos := 0;
      EXIT;
    end;

    currChar := @aString[aPos + 1];
    for i := Pred(strLen) downto aPos do
    begin
      result := (currChar^ = aChar);
      if result then
        BREAK;

      Inc(currChar);
    end;

    if result then
      aPos := (currChar - @aString[1]) + 1
    else
      aPos := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.NPos(const aString: ANSIString;
                                 const aSubStr: ANSIString;
                                 var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PANSIChar;
    initialChar: PANSIChar;
    currChar: PANSIChar;
  begin
  ASSERT(aPos >= 0);

    result  := FALSE;
    strLen  := Length(aString);
    subLen  := Length(aSubStr);
    if ((aPos + subLen) > strLen) or (strLen = 0) or (subLen = 0) then
    begin
      aPos := 0;
      EXIT;
    end;

    firstChar   := @aString[1];
    initialChar := @aSubStr[1];
    currChar    := @aString[aPos + 1];

    for i := (strLen - subLen) downto aPos do
    begin
      result := (currChar^ = initialChar^) and CompareMem(currChar, initialChar, subLen);
      if result then
        BREAK;

      Inc(currChar);
    end;

    if result then
      aPos := (currChar - firstChar) + 1
    else
      aPos := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Pos(const aString: ANSIString;
                                   const aChar: ANSIChar;
                                   var   aPositions: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strLen: Integer;
    firstChar: PANSIChar;
    currChar: PANSIChar;
  begin
    result  := FALSE;
    strLen  := Length(aString);
    if (strLen = 0) then
      EXIT;

    SetLength(aPositions, strLen);

    j         := 0;
    firstChar := @aString[1];
    currChar  := firstChar;

    for i := Pred(strLen) downto 0 do
    begin
      if (currChar^ = aChar) then
      begin
        aPositions[j] := (currChar - firstChar) + 1;
        Inc(j);
      end;
      Inc(currChar);
    end;

    SetLength(aPositions, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Pos(const aString: ANSIString;
                                   const aSubStr: ANSIString;
                                   var   aPositions: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PANSIChar;
    currChar: PANSIChar;
    initialChar: PANSIChar;
  begin
    result  := FALSE;
    subLen  := Length(aSubStr);
    strLen  := Length(aString);
    if (strLen = 0) then
      EXIT;

    SetLength(aPositions, strLen);

    j           := 0;
    firstChar   := @aString[1];
    currChar    := firstChar;
    initialChar := @aSubStr[1];

    for i := Pred(strLen) downto 0 do
    begin
      if (currChar^ = initialChar^) and CompareMem(currChar, initialChar, subLen) then
      begin
        aPositions[j] := (currChar - firstChar) + 1;
        Inc(j);
      end;
      Inc(currChar);
    end;

    SetLength(aPositions, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Compare(const S1, S2: ANSIString): Integer;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, 0,
                             PANSIChar(S1), Length(S1),
                             PANSIChar(S2), Length(S2)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.CompareText(const S1, S2: ANSIString): Integer;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PANSIChar(S1), Length(S1),
                             PANSIChar(S2), Length(S2)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Contains(const aString: ANSIString; const aChar: ANSIChar): Boolean;
  begin
    result := System.Pos(aChar, aString) <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Contains(const aString, aSubStr: ANSIString): Boolean;
  begin
    result := System.Pos(aSubStr, aString) <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.IsLowercase(const aChar: ANSIChar): Boolean;
  begin
    result := Windows.IsCharLowerA(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.IsLowercase(const aString: ANSIString): Boolean;
  begin
    result := CheckCase(aString, Windows.IsCharLowerA);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.IsUppercase(const aChar: ANSIChar): Boolean;
  begin
    result := Windows.IsCharUpperA(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.IsUppercase(const aString: ANSIString): Boolean;
  begin
    result := CheckCase(aString, Windows.IsCharUpperA);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.SameText(const S1, S2: ANSIString): Boolean;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PANSIChar(S1), Length(S1),
                             PANSIChar(S2), Length(S2)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Lowercase(const aString: ANSIString): ANSIString;
  var
    len: Integer;
  begin
    len := Length(aString);
    SetString(result, PANSIChar(aString), len);
    if len > 0 then
      CharLowerBuffA(PANSIChar(result), len);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSISupport.Uppercase(const aString: ANSIString): ANSIString;
  var
    len: Integer;
  begin
    len := Length(aString);
    SetString(result, PANSIChar(aString), len);
    if len > 0 then
      CharUpperBuffA(PANSIChar(result), len);
  end;





{ ANSI ------------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSI.Encode(const aString: String): ANSIString;
  begin
  {$ifdef UNICODE}
    result := ANSI.FromWide(aString);
  {$else}
    result := aString;
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSI.Decode(const aString: ANSIString): String;
  begin
  {$ifdef UNICODE}
    result := WIDE.FromANSI(aString);
  {$else}
    result := aString;
  {$endif}
  end;











{ ASCII ------------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ASCII.Encode(const aString: String): ASCIIString;
  begin
  {$ifdef UNICODE}
    result := ASCIIString(UTF8.FromWide(aString));
  {$else}
    result := ASCIIString(aString);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ASCII.Decode(const aString: ASCIIString): String;
  begin
  {$ifdef UNICODE}
    result := WIDE.FromASCII(aString);
  {$else}
    result := String(aString);
  {$endif}
  end;







{ UTF8 ------------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Encode(const aString: String): UTF8String;
  begin
  {$ifdef UNICODE}
    result := UTF8.FromWide(aString);
  {$else}
    result := UTF8.FromANSI(aString);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Decode(const aString: UTF8String): String;
  begin
  {$ifdef UNICODE}
    result := WIDE.FromUTF8(aString);
  {$else}
    result := ANSI.FromUTF8(aString);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Decode(const aBuffer: PUTF8Char; const aMaxLen: Integer): String;
  begin
  {$ifdef UNICODE}
    result := WIDE.FromUTF8(aBuffer, aMaxLen);
  {$else}
    result := ANSI.FromUTF8(aBuffer, aMaxLen);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.FromANSI(const aString: ANSIString): UTF8String;
  begin
    result := FromWide(WIDE.FromANSI(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.FromWide(const aString: UnicodeString): UTF8String;
  var
    len: Integer;
  begin
    len := WideCharToMultiByte(CP_UTF8, 0, PWideChar(aString), -1, NIL, 0, NIL, NIL);
    Dec(len);

    SetLength(result, len);
    WideCharToMultiByte(CP_UTF8, 0, PWideChar(aString), Length(aString), PANSIChar(result), Length(result), NIL, NIL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Pos(const aString: UTF8String;
                          const aChar: UTF8Char;
                          var   aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aChar, aString);
    result  := (aPos <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Pos(const aString: UTF8String;
                          const aSubStr: UTF8String;
                          var   aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aSubStr, aString);
    result  := (aPos <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.RPos(const aString: UTF8String;
                           const aChar: UTF8Char;
                           var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    firstChar: PUTF8Char;
    currChar: PUTF8Char;
  begin
    strLen  := Length(aString);
    aPos    := 0;
    result  := FALSE;
    if (strLen = 0) then
      EXIT;

    firstChar := @aString[1];
    currChar  := @aString[strLen];

    for i := Pred(strLen) downto 0 do
    begin
      result := (currChar^ = aChar);
      if result then
        BREAK;

      Dec(currChar);
    end;

    if result then
      aPos := (PANSIChar(currChar) - PANSIChar(firstChar)) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.RPos(const aString: UTF8String;
                                 const aSubStr: UTF8String;
                                 var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PUTF8Char;
    currChar: PUTF8Char;
    initialChar: PUTF8Char;
  begin
    strLen  := Length(aString);
    subLen  := Length(aSubStr);
    aPos    := 0;
    result  := FALSE;
    if (subLen > strLen) or (subLen = 0) or (strLen = 0) then
      EXIT;

    firstChar   := @aString[1];
    initialChar := @aSubStr[1];
    currChar    := @aString[Length(aString)];

    for i := (strLen - subLen) downto 0 do
    begin
      result := (currChar^ = initialChar^);
      if result and CompareMem(currChar, initialChar, subLen) then
        BREAK;

      Dec(currChar);
    end;

    if result then
      aPos := (PANSIChar(currChar) - PANSIChar(firstChar)) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.NPos(const aString: UTF8String;
                                 const aChar: UTF8Char;
                                 var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    currChar: PUTF8Char;
  begin
  ASSERT(aPos >= 0);

    result  := FALSE;
    strLen  := Length(aString);
    if (aPos >= strLen) then
    begin
      aPos := 0;
      EXIT;
    end;

    currChar := @aString[aPos + 1];
    for i := Pred(strLen) downto aPos do
    begin
      result := (currChar^ = aChar);
      if result then
        BREAK;

      Inc(currChar);
    end;

    if result then
      aPos := (PANSIChar(currChar) - PANSIChar(@aString[1])) + 1
    else
      aPos := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.NPos(const aString: UTF8String;
                                 const aSubStr: UTF8String;
                                 var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PUTF8Char;
    initialChar: PUTF8Char;
    currChar: PUTF8Char;
  begin
  ASSERT(aPos >= 0);

    result  := FALSE;
    strLen  := Length(aString);
    subLen  := Length(aSubStr);
    if ((aPos + subLen) > strLen) or (strLen = 0) or (subLen = 0) then
    begin
      aPos := 0;
      EXIT;
    end;

    firstChar   := @aString[1];
    initialChar := @aSubStr[1];
    currChar    := @aString[aPos + 1];

    for i := (strLen - subLen) downto aPos do
    begin
      result := (currChar^ = initialChar^) and CompareMem(currChar, initialChar, subLen);
      if result then
        BREAK;

      Inc(currChar);
    end;

    if result then
      aPos := (PANSIChar(currChar) - PANSIChar(firstChar)) + 1
    else
      aPos := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Pos(const aString: UTF8String;
                                   const aChar: UTF8Char;
                                   var   aPositions: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strLen: Integer;
    firstChar: PUTF8Char;
    currChar: PUTF8Char;
  begin
    result  := FALSE;
    strLen  := Length(aString);
    if (strLen = 0) then
      EXIT;

    SetLength(aPositions, strLen);

    j         := 0;
    firstChar := @aString[1];
    currChar  := firstChar;

    for i := Pred(strLen) downto 0 do
    begin
      if (currChar^ = aChar) then
      begin
        aPositions[j] := (PANSIChar(currChar) - PANSIChar(firstChar)) + 1;
        Inc(j);
      end;
      Inc(currChar);
    end;

    SetLength(aPositions, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Pos(const aString: UTF8String;
                                   const aSubStr: UTF8String;
                                   var   aPositions: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PUTF8Char;
    currChar: PUTF8Char;
    initialChar: PUTF8Char;
  begin
    result  := FALSE;
    subLen  := Length(aSubStr);
    strLen  := Length(aString);
    if (strLen = 0) then
      EXIT;

    SetLength(aPositions, strLen);

    j           := 0;
    firstChar   := @aString[1];
    currChar    := firstChar;
    initialChar := @aSubStr[1];

    for i := Pred(strLen) downto 0 do
    begin
      if (currChar^ = initialChar^) and CompareMem(currChar, initialChar, subLen) then
      begin
        aPositions[j] := (PANSIChar(currChar) - PANSIChar(firstChar)) + 1;
        Inc(j);
      end;
      Inc(currChar);
    end;

    SetLength(aPositions, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Compare(const S1, S2: UTF8String): Integer;
  begin
    result := WIDE.Compare(WIDE.FromUTF8(S1), WIDE.FromUTF8(S2));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.CompareText(const S1, S2: UTF8String): Integer;
  begin
    result := WIDE.CompareText(WIDE.FromUTF8(S1), WIDE.FromUTF8(S2));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Contains(const aString: UTF8String;
                               const aChar: UTF8Char): Boolean;
  begin
    result := System.Pos(aChar, aString) <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Contains(const aString: UTF8String;
                               const aSubStr: UTF8String): Boolean;
  begin
    result := System.Pos(aSubStr, aString) <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.IsLowercase(const aString: UTF8String): Boolean;
  begin
    result := WIDE.IsLowercase(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.IsUppercase(const aString: UTF8String): Boolean;
  begin
    result := WIDE.IsUppercase(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.SameText(const S1, S2: UTF8String): Boolean;
  begin
    result := WIDE.SameText(WIDE.FromUTF8(S1), WIDE.FromUTF8(S2));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Lowercase(const aString: UTF8String): UTF8String;
  begin
    result := UTF8.FromWide(WIDE.Lowercase(WIDE.FromUTF8(aString)));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function UTF8.Uppercase(const aString: UTF8String): UTF8String;
  begin
    result := UTF8.FromWide(WIDE.Uppercase(WIDE.FromUTF8(aString)));
  end;











{ WIDESupport ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.FromANSI(const aString: ANSIString): UnicodeString;
  begin
    result := FromANSI(@aString[1], Length(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.FromANSI(const aBuffer: PANSIChar; const aMaxLen: Integer): UnicodeString;
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
  class function WIDESupport.FromASCII(const aString: ASCIIString): UnicodeString;
  begin
    result := WIDESupport.FromUTF8(UTF8String(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.FromUTF8(const aString: UTF8String): UnicodeString;
  begin
    result := FromUTF8(@aString[1], Length(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer): UnicodeString;
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
  class function WIDESupport.Pos(const aString: UnicodeString;
                          const aChar: WideChar;
                          var   aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aChar, aString);
    result  := (aPos <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.Pos(const aString: UnicodeString;
                          const aSubStr: UnicodeString;
                          var   aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aSubStr, aString);
    result  := (aPos <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.RPos(const aString: UnicodeString;
                           const aChar: WideChar;
                           var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    firstChar: PWideChar;
    currChar: PWideChar;
  begin
    strLen  := Length(aString);
    aPos    := 0;
    result  := FALSE;
    if (strLen = 0) then
      EXIT;

    firstChar := @aString[1];
    currChar  := @aString[strLen];

    for i := Pred(strLen) downto 0 do
    begin
      result := (currChar^ = aChar);
      if result then
        BREAK;

      Dec(currChar);
    end;

    if result then
      aPos := (currChar - firstChar) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.RPos(const aString: UnicodeString;
                           const aSubStr: UnicodeString;
                           var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PWideChar;
    currChar: PWideChar;
    initialChar: PWideChar;
  begin
    strLen  := Length(aString);
    subLen  := Length(aSubStr);
    aPos    := 0;
    result  := FALSE;
    if (subLen > strLen) or (subLen = 0) or (strLen = 0) then
      EXIT;

    firstChar   := @aString[1];
    initialChar := @aSubStr[1];
    currChar    := @aString[Length(aString)];

    for i := (strLen - subLen) downto 0 do
    begin
      result := (currChar^ = initialChar^);
      if result and CompareMem(currChar, initialChar, subLen * 2) then
        BREAK;

      Dec(currChar);
    end;

    if result then
      aPos := (currChar - firstChar) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.NPos(const aString: UnicodeString;
                           const aChar: WideChar;
                           var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    currChar: PWideChar;
  begin
  ASSERT(aPos >= 0);

    result  := FALSE;
    strLen  := Length(aString);
    if (aPos >= strLen) then
    begin
      aPos := 0;
      EXIT;
    end;

    currChar := @aString[aPos + 1];
    for i := Pred(strLen) downto aPos do
    begin
      result := (currChar^ = aChar);
      if result then
        BREAK;

      Inc(currChar);
    end;

    if result then
      aPos := (currChar - @aString[1]) + 1
    else
      aPos := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.NPos(const aString: UnicodeString;
                           const aSubStr: UnicodeString;
                           var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PWideChar;
    initialChar: PWideChar;
    currChar: PWideChar;
  begin
  ASSERT(aPos >= 0);

    result  := FALSE;
    strLen  := Length(aString);
    subLen  := Length(aSubStr);
    if ((aPos + subLen) > strLen) or (strLen = 0) or (subLen = 0) then
    begin
      aPos := 0;
      EXIT;
    end;

    firstChar   := @aString[1];
    initialChar := @aSubStr[1];
    currChar    := @aString[aPos + 1];

    for i := (strLen - subLen) downto aPos do
    begin
      result := (currChar^ = initialChar^) and CompareMem(currChar, initialChar, subLen * 2);
      if result then
        BREAK;

      Inc(currChar);
    end;

    if result then
      aPos := (currChar - firstChar) + 1
    else
      aPos := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.Pos(const aString: UnicodeString;
                          const aChar: WideChar;
                          var   aPositions: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strLen: Integer;
    firstChar: PWideChar;
    currChar: PWideChar;
  begin
    result  := FALSE;
    strLen  := Length(aString);
    if (strLen = 0) then
      EXIT;

    SetLength(aPositions, strLen);

    j         := 0;
    firstChar := @aString[1];
    currChar  := firstChar;

    for i := Pred(strLen) downto 0 do
    begin
      if (currChar^ = aChar) then
      begin
        aPositions[j] := (currChar - firstChar) + 1;
        Inc(j);
      end;
      Inc(currChar);
    end;

    SetLength(aPositions, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.Pos(const aString: UnicodeString;
                          const aSubStr: UnicodeString;
                          var   aPositions: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PWideChar;
    currChar: PWideChar;
    initialChar: PWideChar;
  begin
    result  := FALSE;
    subLen  := Length(aSubStr);
    strLen  := Length(aString);
    if (strLen = 0) then
      EXIT;

    SetLength(aPositions, strLen);

    j           := 0;
    firstChar   := @aString[1];
    currChar    := firstChar;
    initialChar := @aSubStr[1];

    for i := Pred(strLen) downto 0 do
    begin
      if (currChar^ = initialChar^) and CompareMem(currChar, initialChar, subLen * 2) then
      begin
        aPositions[j] := (currChar - firstChar) + 1;
        Inc(j);
      end;
      Inc(currChar);
    end;

    SetLength(aPositions, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.BeginsWith(const aString, aLead: UnicodeString): Boolean;
  begin
    result := (Length(aLead) <= Length(aString))
          and (CompareStringW(LOCALE_USER_DEFAULT, 0,
                              PWideChar(aString), Length(aLead),
                              PWideChar(aLead), Length(aLead)) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.BeginsWithText(const aString, aLead: UnicodeString): Boolean;
  begin
    result := (Length(aLead) <= Length(aString))
          and (CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                              PWideChar(aString), Length(aLead),
                              PWideChar(aLead), Length(aLead)) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.Compare(const S1, S2: UnicodeString): Integer;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, 0,
                             PWideChar(S1), Length(S1),
                             PWideChar(S2), Length(S2)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.CompareText(const S1, S2: UnicodeString): Integer;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PWideChar(S1), Length(S1),
                             PWideChar(S2), Length(S2)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.Contains(const aString: UnicodeString;
                               const aChar: WideChar): Boolean;
  begin
    result := System.Pos(aChar, aString) <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.Contains(const aString: UnicodeString;
                               const aSubStr: UnicodeString): Boolean;
  begin
    result := System.Pos(aSubStr, aString) <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.IsLowercase(const aChar: WideChar): Boolean;
  begin
    result := IsCharLowerW(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.IsLowercase(const aString: UnicodeString): Boolean;
  begin
    result  := CheckCase(aString, Windows.IsCharLowerW);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.IsUppercase(const aChar: WideChar): Boolean;
  begin
    result := IsCharUpperW(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.IsUppercase(const aString: UnicodeString): Boolean;
  begin
    result  := CheckCase(aString, Windows.IsCharUpperW);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.SameText(const S1, S2: UnicodeString): Boolean;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PWideChar(S1), Length(S1),
                             PWideChar(S2), Length(S2)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.Lowercase(const aString: UnicodeString): UnicodeString;
  begin
    result := WideLowerCase(aString);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDESupport.Uppercase(const aString: UnicodeString): UnicodeString;
  begin
    result := WideUpperCase(aString);
  end;



{ WIDE ------------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDE.Encode(const aString: String): UnicodeString;
  begin
  {$ifdef UNICODE}
    result := aString;
  {$else}
    result := WIDE.FromANSI(aString);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDE.Decode(const aString: UnicodeString): String;
  begin
  {$ifdef UNICODE}
    result := aString;
  {$else}
    result := ANSI.FromWide(aString);
  {$endif}
  end;










{ STR -------------------------------------------------------------------------------------------- }

{$ifdef UNICODE}
  class function STR.FromWide(const aString: UnicodeString): String;
  begin
    result := aString;
  end;
{$else}
  class function STR.FromANSI(const aString: ANSIString): String;
  begin
    result := aString;
  end;
{$endif}








end.
