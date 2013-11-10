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
    UCS4Char    = Cardinal;
    UCS4String  = array of UCS4Char;

    UTF8String  = type ANSIString;

    UnicodeString = type WideString;
  {$endif}

//    TSetOfChar = set of ANSIChar;


    ASCIIString = type ANSIString;
    ASCIIChar   = type ANSIChar;
    PASCIIChar  = ^ASCIIChar;

    UTF8Char    = type ANSIChar;
    PUTF8Char   = ^UTF8Char;

    UTF32Char       = UCS4Char;
    UTF32CharArray  = array of UCS4Char;
    UTF32String     = UCS4String;


    ANSI = class
      class function Encode(const aString: String): ANSIString;
      class function Decode(const aString: ANSIString): String;
      class function FromUTF8(const aString: UTF8String): ANSIString; overload;
      class function FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): ANSIString; overload;
      class function FromWide(const aString: UnicodeString): ANSIString;
      class function IsLower(const aChar: ANSIChar): Boolean; overload;
      class function IsLower(const aString: ANSIString): Boolean; overload;
      class function Lowercase(const aString: ANSIString): ANSIString;
    end;

    ASCII = class
      class function Encode(const aString: String): ASCIIString;
      class function Decode(const aString: ASCIIString): String;
    end;

    UTF8 = class
      class function Encode(const aString: String): UTF8String;
      class function Decode(const aString: UTF8String): String; overload;
      class function Decode(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): String; overload;
      class function FromANSI(const aString: ANSIString): UTF8String;
      class function FromWide(const aString: UnicodeString): UTF8String;
    end;

    WIDE = class
      class function FromANSI(const aString: ANSIString): UnicodeString; overload;
      class function FromANSI(const aBuffer: PANSIChar; const aMaxLen: Integer = -1): UnicodeString; overload;
      class function FromASCII(const aString: ASCIIString): UnicodeString; overload;
      class function FromUTF8(const aString: UTF8String): UnicodeString; overload;
      class function FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer = -1): UnicodeString; overload;
      class function IsLower(const aChar: WideChar): Boolean; overload;
      class function IsLower(const aString: UnicodeString): Boolean; overload;
    end;

    UTF32 = class
    end;

  function UTF32ToString(const aString: UTF32String): UnicodeString;


implementation

  uses
    Windows;

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


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSI.FromUTF8(const aString: UTF8String): ANSIString;
  begin
    // TODO: Can we do this more directly / efficiently ?
    result := ANSI.FromWide(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSI.FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer): ANSIString;
  begin
    // TODO: Can we do this more directly / efficiently ?
    result := ANSI.FromWide(WIDE.FromUTF8(aBuffer, aMaxLen));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSI.FromWide(const aString: UnicodeString): ANSIString;
  var
    len: Integer;
  begin
    len := WideCharToMultiByte(CP_ACP, 0, PWideChar(aString), -1, NIL, 0, NIL, NIL);
    Dec(len);

    SetLength(result, len);
    WideCharToMultiByte(CP_ACP, 0, PWideChar(aString), Length(aString), PANSIChar(result), Length(result), NIL, NIL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSI.IsLower(const aChar: ANSIChar): Boolean;
  begin
    result := Windows.IsCharLowerA(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSI.IsLower(const aString: ANSIString): Boolean;
  var
    i: Integer;
  begin
    result := FALSE;

    for i := 0 to Pred(Length(aString)) do
      if NOT Windows.IsCharLowerA(aString[i]) then
        EXIT;

    result := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function ANSI.Lowercase(const aString: ANSIString): ANSIString;
  var
    len: Integer;
  begin
    len := Length(aString);
    SetString(result, PANSIChar(aString), len);
    if len > 0 then
      CharLowerBuffA(PANSIChar(result), len);
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












{ WIDE ------------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDE.FromANSI(const aString: ANSIString): UnicodeString;
  begin
    result := FromANSI(@aString[1], Length(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDE.FromANSI(const aBuffer: PANSIChar; const aMaxLen: Integer): UnicodeString;
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
  class function WIDE.FromASCII(const aString: ASCIIString): UnicodeString;
  begin
    result := WIDE.FromUTF8(UTF8String(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDE.FromUTF8(const aString: UTF8String): UnicodeString;
  begin
    result := FromUTF8(@aString[1], Length(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDE.FromUTF8(const aBuffer: PUTF8Char; const aMaxLen: Integer): UnicodeString;
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
  class function WIDE.IsLower(const aChar: WideChar): Boolean;
  begin
    result := IsCharUpperW(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function WIDE.IsLower(const aString: UnicodeString): Boolean;
  var
    i: Integer;
  begin
    result := FALSE;

    for i := 0 to Pred(Length(aString)) do
      if NOT Windows.IsCharLowerW(aString[i]) then
        EXIT;

    result := TRUE;
  end;











{ UTF32 ------------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function UTF32ToString(const aString: UTF32String): UnicodeString;
  var
    i: Integer;
    len: Integer;
  begin
    SetLength(result, Length(aString) * 2 - 1); // Maximum possible number of characters

    len := 0;
    for i := 0 to Pred(Length(aString)) do
    begin
      if aString[i] >= $10000 then
      begin
        Inc(len, 2);
        result[len - 1] := WideChar((((aString[i] - $00010000) shr 10) and $000003FF) or $D800);
        result[len]     := WideChar(((aString[i] - $00010000) and $000003FF)or $DC00);
      end
      else
      begin
        Inc(len);
        result[len] := WideChar(aString[i]);
      end;
    end;

    SetLength(result, len);
  end;



end.
