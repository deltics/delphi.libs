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

{$ifdef deltics_strutils}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.StrUtils;


interface

  uses
  { vcl: }
    Classes,
  { deltics: }
    Deltics.Classes;


  const
    LIKENESS_MATCH = #255;

  type
    TANSIStringArray = array of ANSIString;
    TStringArray = array of String;


    IStringList = interface
    ['{7623F313-9BC7-4D9C-9F9F-0A8C8E650874}']
      function get_Count: Integer;
      function get_Item(const aIndex: Integer): String;
      function get_List: TStringList;
      function get_Name(const aIndex: Integer): String;
      function get_Value(const aName: String): String;

      function Add(const aString: String): Integer;
      procedure Clear;
      function Contains(const aString: String): Boolean;
      procedure Delete(const aIndex: Integer);
      procedure Insert(const aIndex: Integer; const aString: String);

      property Count: Integer read get_Count;
      property Items[const aIndex: Integer]: String read get_Item;
      property List: TStringList read get_List;
      property Names[const aIndex: Integer]: String read get_Name;
      property Values[const aName: String]: String read get_Value;
    end;


    IStringTool = interface
    ['{1C8E3BD6-29A5-4598-839E-8F2F1133C598}']
      function Contains(const aSubStr: String): Boolean;
    end;

    TStringTool = class(TInterfacedObject, IStringTool)
    private
      fString: String;
    public
      constructor Create(const aString: String);
      function Contains(const aSubStr: String): Boolean;
    end;


  procedure ArrayInit(var aArray: TANSIStringArray; const aSize: Integer); overload;
  procedure ArrayAdd(var aArray: TANSIStringArray; const aValue: ANSIString); overload;
  procedure ArrayAdd(var aArray: TANSIStringArray; const aValue: array of ANSIString); overload;
  procedure ArrayExtend(var aArray: TANSIStringArray; const aCount: Integer); overload;


  function Likeness(const A: ANSIString;
                    const B: ANSIString): Integer;
  function LikenessEx(const A: ANSIString;
                      const B: ANSIString;
                      var strA: ANSIString;
                      var strB: ANSIString): Integer; overload;
  function LikenessEx(const A: ANSIString;
                      const B: ANSIString;
                      var strA: ANSIString;
                      var strB: ANSIString;
                      var iSame: Integer;
                      var iTotal: Integer): Integer; overload;

  function CamelCapsToWords(const aString: String): String;
  function ReverseStr(const aString: String): String; overload;
  {$ifdef UNICODE}
  function ReverseStr(const aString: ANSIString): ANSIString; overload;
  {$endif UNICODE}
  function Split(const aString: String; const aDelim: Char; const aSlices: TStrings): Integer; overload;
  function Split(const aString: String; const aDelim: Char; var aSlices: TStringArray): Integer; overload;
  procedure Split(const aString: ANSIString; const aDelim: ANSIChar; var aSlices: TANSIStringArray); overload;

  function StrBeginsWith(const aString: String; const aBeginning: String): Boolean;
  function StrBeginsWithText(const aString: String; const aBeginning: String): Boolean;
  function StrContains(const aString: String; const aSubString: String): Boolean;
  function StrContainsText(const aString: String; const aSubString: String): Boolean;
  function StrEndsWith(const aString: String; const aEnd: String): Boolean;
  function StrEndsWithText(const aString: String; const aEnd: String): Boolean;

  function StrLPad(const aString: String; const aWidth: Integer; const aTrunc: Boolean = FALSE): String;
  function StrRPad(const aString: String; const aWidth: Integer; const aTrunc: Boolean = FALSE): String;

  function StrDequote(const aString: String): String;
  function StrPop(var aString: String; const aDelim: Char): String;
  function StrPopQuoted(var aString: String; const aDelim: Char): String;


  function IsAlpha(const aChar: Char): Boolean;
  function IsDigit(const aChar: Char): Boolean;
  function IsLower(const aChar: Char): Boolean;
  function IsUpper(const aChar: Char): Boolean;


  type
    TManagedStringList = class(TCOMInterfacedObject, IStringList)
    private
      fList: TStringList;
    public
      function get_Count: Integer;
      function get_Item(const aIndex: Integer): String;
      function get_List: TStringList;
      function get_Name(const aIndex: Integer): String;
      function get_Value(const aName: String): String;

      function Add(const aString: String): Integer;
      procedure Clear;
      function Contains(const aString: String): Boolean;
      procedure Delete(const aIndex: Integer);
      procedure Insert(const aIndex: Integer; const aString: String);
    end;


implementation

  uses
  {$ifdef FASTSTRINGS}
    FastStrings,
  {$endif}
    Math,
    SysUtils,
    Windows,
  { deltics: }
    Deltics.Strings;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ArrayInit(var aArray: TANSIStringArray; const aSize: Integer); overload;
  var
    i: Integer;
  begin
    SetLength(aArray, aSize);
    for i := 0 to Pred(aSize) do
      aArray[i] := '';
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ArrayAdd(var aArray: TANSIStringArray; const aValue: ANSIString);
  begin
    SetLength(aArray, Length(aArray) + 1);
    aArray[Length(aArray) - 1] := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ArrayAdd(var aArray: TANSIStringArray; const aValue: array of ANSIString);
  var
    i: Integer;
  begin
    SetLength(aArray, Length(aArray) + Length(aValue));
    for i := Length(aArray) - Length(aValue) to Length(aArray) - 1 do
      aArray[i] := aValue[i - (Length(aArray) - Length(aValue))];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure ArrayExtend(var aArray: TANSIStringArray; const aCount: Integer);
  var
    i: Integer;
  begin
    SetLength(aArray, Length(aArray) + aCount);
    for i := Length(aArray) - aCount to Length(aArray) - 1 do
      aArray[i] := '';
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Likeness(const A: ANSIString;
                    const B: ANSIString): Integer;
  var
    strA: ANSIString;
    strB: ANSIString;
    iSame: Integer;
    iTotal: Integer;
  begin
    result := LikenessEx(A, B, strA, strB, iSame, iTotal);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function LikenessEx(const A: ANSIString;
                      const B: ANSIString;
                      var strA: ANSIString;
                      var strB: ANSIString): Integer;
  var
    iSame: Integer;
    iTotal: Integer;
  begin
    result := LikenessEx(A, B, strA, strB, iSame, iTotal);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function LikenessEx(const A: ANSIString;
                      const B: ANSIString;
                      var strA: ANSIString;
                      var strB: ANSIString;
                      var iSame: Integer;
                      var iTotal: Integer): Integer;
  {
    Crude likeness comparison - processes ANSIString A ANSIChar by ANSIChar looking
     for the same ANSIChar in B.  The result is the (crude) %age similarity of
     the two strings, A and B:

          100 = the strings are identical
           50 = half of the strings are the same, the other half is different
            0 = the strings have not one single character in common

    The two var params strA and strB contain the "match map" resulting
     from the comparison.  In the change maps, all matched character
     positions contain the MATCHED ANSIChar, not the actual ANSIString ANSIChar.
  }
  var
    c: ANSIChar;
    i, j: Integer;
    fa, la: Integer;
    fb, lb: Integer;
    iLenA: Integer;
    iLenB: Integer;
    startA: array[#1..#254] of Integer;
    startB: array[#1..#254] of Integer;
  begin
    result  := 0;

    iSame   := 0;
    strA    := A;
    strB    := B;
    iLenA   := Length(A);
    iLenB   := Length(B);
    iTotal  := (iLenA + iLenB);

    if (iLenA = 0) xor (iLenB = 0) then
      EXIT;

    fa := 1;
    fb := 1;
    la := iLenA;
    lb := iLenB;
    j := 1;
    for i := 1 to iLenA do
    begin
      if (i > iLenB) then
        BREAK;

      if (A[i] = B[j]) then
      begin
        strA[i] := LIKENESS_MATCH;
        strB[j] := LIKENESS_MATCH;
        Inc(j);
        Inc(iSame, 2);
      end
      else
      begin
        fa := i;
        fb := j;
        BREAK;
      end;
    end;

    j := iLenB;
    for i := iLenA downto 1 do
    begin
      if (strA[i] = LIKENESS_MATCH) or (strB[j] = LIKENESS_MATCH) then
      begin
        la := fa;
        lb := fb;
        BREAK;
      end;

      if (A[i] = B[j]) then
      begin
        strA[i] := LIKENESS_MATCH;
        strB[j] := LIKENESS_MATCH;
        Dec(j);
        Inc(iSame, 2);

        if (j < 1) then
          BREAK;
      end
      else
      begin
        la := i;
        lb := j;
        BREAK;
      end;
    end;

    try
      if (fa = la) and (fb = lb) then
        EXIT;

      ZeroMemory(@startA, 254 * sizeof(Integer));
      ZeroMemory(@startB, 254 * sizeof(Integer));
      for i := fa to la do
      begin
        case startA[A[i]] of
          -1: CONTINUE;
           0: startA[A[i]] := i;
        else
           startA[A[i]] := -1;
        end;
      end;

      for i := fb to lb do
      begin
        case startB[B[i]] of
          -1: CONTINUE;
           0: startB[B[i]] := i;
        else
           startB[B[i]] := -1;
        end;
      end;

      for c := #1 to #254 do
      begin
        if (startA[c] > 0) and (startB[c] > 0) then
        begin
          i := startA[c];
          j := startB[c];

          if (strA[i] = LIKENESS_MATCH) then
            CONTINUE;

          while (i >= fa) and (j >= fb)
            and (strA[i] = strB[j]) do
          begin
            strA[i] := LIKENESS_MATCH;
            strB[j] := LIKENESS_MATCH;
            Dec(i);
            Dec(j);
            Inc(iSame, 2);
          end;

          i := Succ(startA[c]);
          j := Succ(startB[c]);

          while (i <= la) and (j <= lb)
            and (strA[i] = strB[j]) do
          begin
            strA[i] := LIKENESS_MATCH;
            strB[j] := LIKENESS_MATCH;
            Inc(i);
            Inc(j);
            Inc(iSame, 2);
          end;
        end;
      end;

{
      for i := 1 to iLenA do
      begin
        if (strA[i] = LIKENESS_MATCH) then
          CONTINUE;

        j := FastCharPos(strB, strA[i], fb);
        if (j = 0) then
          CONTINUE;

        Inc(iSame, 2);
        strA[i] := LIKENESS_MATCH;
        strB[j] := LIKENESS_MATCH;
      end;
}
    finally
      if (iSame < iTotal) then
        result := Min(99, (100 * iSame) div iTotal)
      else
        result := 100;
    end;
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function CamelCapsToWords(const aString: String): String;
  var
    i: Integer;
  begin
    result := aString;

    i := 2;
    while i < Length(result) do
    begin
      if IsAlpha(result[i]) and NOT (result[i - 1] = ' ') then
      begin
        if IsUpper(result[i]) and (result[i - 1] <> '_')
         and (((NOT IsUpper(result[i - 1]) or NOT IsAlpha(result[i - 1])))
          or (IsUpper(result[i - 1]) and NOT IsUpper(result[i + 1]) and NOT IsDigit(result[i + 1]))) then
        begin
          result := Copy(result, 1, i - 1) + ' ' + Copy(result, i, Length(result));
          Inc(i);
        end;
      end;
      Inc(i);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function ReverseStr(const aString: String): String;
  var
    i: Integer;
    c: Char;
  begin
    result := aString;

    for i := 1 to Length(result) div 2 do
    begin
      c := result[i];
      result[i] := result[Length(result) - Pred(i)];
      result[Length(result) - Pred(i)] := c;
    end;
  end;


  {$ifdef UNICODE}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function ReverseStr(const aString: ANSIString): ANSIString;
  var
    i: Integer;
    c: ANSIChar;
  begin
    result := aString;

    for i := 1 to Length(result) div 2 do
    begin
      c := result[i];
      result[i] := result[Length(result) - Pred(i)];
      result[Length(result) - Pred(i)] := c;
    end;
  end;
  {$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Split(const aString: String;
                 const aDelim: Char;
                 const aSlices: TStrings): Integer;
  var
    s: String;
    pos: Integer;
  begin
    result := 0;
    aSlices.Clear;

    if (aString = '') then
      EXIT;

    try
      s := aString;
      while (Length(s) > 0) do
      begin
      {$ifdef FASTSTRINGS}
        pos := FastCharPos(s, aDelim, 1);
      {$else}
        pos := System.Pos(aDelim, s);
      {$endif}

        case pos of
          0 : begin
                aSlices.Add(s);
                EXIT;
              end;

          1 : begin
                aSlices.Add('');
                Delete(s, 1, 1);
              end;
        else
          aSlices.Add(Copy(s, 1, Pred(pos)));
          if (pos = Length(s)) then
          begin
            aSlices.Add('');
            EXIT;
          end
          else
            Delete(s, 1, pos);
        end;
      end;

    finally
      result := aSlices.Count;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Split(const aString: String;
                 const aDelim: Char;
                 var aSlices: TStringArray): Integer;
  var
    i: Integer;
    s: String;
    pos: Integer;
  begin
    result := 0;
    i      := 0;

    if (aString = '') then
      SetLength(aSlices, 0)
    else
    try
      SetLength(aSlices, Length(aString) + 1);

      s := aString;
      while (Length(s) > 0) do
      begin
      {$ifdef FASTSTRINGS}
        pos := FastCharPos(s, aDelim, 1);
      {$else}
        pos := System.Pos(aDelim, s);
      {$endif}

        case pos of
          0 : begin
                aSlices[i] := s;
                Inc(i);
                EXIT;
              end;

          1 : begin
                aSlices[i] := '';
                Inc(i);

                Delete(s, 1, 1);
              end;
        else
          aSlices[i] := Copy(s, 1, Pred(pos));
          Inc(i);
          if (pos = Length(s)) then
          begin
            aSlices[i] := '';
            Inc(i);
            EXIT;
          end
          else
            Delete(s, 1, pos);
        end;
      end;

    finally
      SetLength(aSlices, i);
      result := i;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure Split(const aString: ANSIString;
                  const aDelim: ANSIChar;
                  var aSlices: TANSIStringArray);
  var
    i: Integer;
    s: ANSIString;
    pos: Integer;
  begin
    i := 0;

    if (aString = '') then
      SetLength(aSlices, 0)
    else
    try
      SetLength(aSlices, Length(aString) + 1);

      s := aString;
      while (Length(s) > 0) do
      begin
      {$ifdef FASTSTRINGS}
        pos := FastCharPos(aString, aDelim, 1);
      {$else}
        pos := System.Pos(aDelim, aString);
      {$endif}

        case pos of
          0 : begin
                aSlices[i] := s;
                Inc(i);
                EXIT;
              end;

          1 : begin
                aSlices[i] := '';
                Inc(i);

                if (Length(s) = 1) then
                begin
                  aSlices[i] := '';
                  Inc(i);
                  EXIT;
                end;

                Delete(s, 1, 1);
              end;
        else
          aSlices[i] := Copy(s, 1, Pred(pos));
          Inc(i);
          Delete(s, 1, pos);
        end;
      end;
    finally
      SetLength(aSlices, i);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrBeginsWith(const aString: String; const aBeginning: String): Boolean;
  begin
    result := (Length(aBeginning) > 0)
               and (Length(aBeginning) <= Length(aString))
               and ANSISameStr(Copy(aString, 1, Length(aBeginning)), aBeginning);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrBeginsWithText(const aString: String; const aBeginning: String): Boolean;
  begin
    result := (Length(aBeginning) > 0)
               and (Length(aBeginning) <= Length(aString))
               and ANSISameText(Copy(aString, 1, Length(aBeginning)), aBeginning);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrContains(const aString: String; const aSubString: String): Boolean;
  begin
    result := (ANSIPos(aSubString, aString) <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrContainsText(const aString: String; const aSubString: String): Boolean;
  begin
    result := (ANSIPos(ANSIUpperCase(aSubString), ANSIUpperCase(aString)) <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrEndsWith(const aString: String; const aEnd: String): Boolean;
  begin
    result := (Length(aEnd) > 0)
               and (Length(aEnd) <= Length(aString))
               and AnsiSameStr(Copy(aString, Length(aString) - (Length(aEnd) - 1), Length(aEnd)), aEnd);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrEndsWithText(const aString: String; const aEnd: String): Boolean;
  begin
    result := (Length(aEnd) > 0)
               and (Length(aEnd) <= Length(aString))
               and ANSISameText(Copy(aString, Length(aString) - (Length(aEnd) - 1), Length(aEnd)), aEnd);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrLPad(const aString: String;
                   const aWidth: Integer;
                   const aTrunc: Boolean): String;
  begin
    if Length(aString) >= aWidth then
    begin
      if aTrunc then
        result := Copy(aString, Length(aString) - aWidth + 2, aWidth)
      else
        result := aString;
    end
    else
      result := StringOfChar(' ', aWidth - Length(aString)) + aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrRPad(const aString: String;
                   const aWidth: Integer;
                   const aTrunc: Boolean): String;
  begin
    if Length(aString) >= aWidth then
    begin
      if aTrunc then
        result := Copy(aString, 1, aWidth)
      else
        result := aString;
    end
    else
      result := aString + StringOfChar(' ', aWidth - Length(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrDequote(const aString: String): String;
  var
    i: Integer;
    s: String;
    qc: Char;
    inQuote: Boolean;
  begin
    s := Trim(aString);

    result := s;

    if (Length(result) = 0) then
      EXIT;

    qc := s[1];
    if NOT (ANSIChar(qc) in ['''', '"']) then
      EXIT;

    if (s[Length(s)] <> qc) then
      EXIT;

    result  := '';
    inQuote := FALSE;

    i := 1;
    while (i < Length(s)) do
    begin
      if inQuote then
      begin
        if (s[i] = qc) then
        begin
          if (i = Length(s)) then
            BREAK;

          if (s[i + 1] = qc) then
          begin
            result := result + qc;
            Inc(i);
          end
          else
            inQuote := FALSE;
        end
        else
          result := result + s[i];
      end
      else if (s[i] = qc) then
        inQuote := TRUE
      else
        result := result + s[i];

      Inc(i);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrPop(var aString: String; const aDelim: Char): String;
  var
    p: Integer;
  begin
    result := aString;
    try
      p := Pos(aDelim, aString);
      if p = 0 then
        EXIT;

      result := Copy(aString, 1, p - 1);

    finally
      Delete(aString, 1, Length(result) + Length(aDelim));
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StrPopQuoted(var aString: String;
                        const aDelim: Char): String;
  {
  }
  var
    i: Integer;
    inQuote: Boolean;
  begin
    inQuote := FALSE;
    result  := '';
    i       := 1;

    try
      while i <= Length(aString) do
      begin
        if inQuote then
        begin
          if (i = Length(aString))
           or ((aString[i] = '"') and (aString[i + 1] <> '"')) then
            inQuote := FALSE;
        end
        else if aString[i] = '"' then
          inQuote := TRUE
        else if aString[i] = aDelim then
          BREAK;

        Inc(i);
      end;

    finally
      if (i > 1) then
        result := Copy(aString, 1, i - 1);

      Delete(aString, 1, i);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IsAlpha(const aChar: Char): Boolean;
  begin
    result := IsCharAlpha(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IsDigit(const aChar: Char): Boolean;
  begin
    result := IsCharAlphaNumeric(aChar) and NOT IsCharAlpha(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IsLower(const aChar: Char): Boolean;
  begin
    result := IsCharLower(aChar);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IsUpper(const aChar: Char): Boolean;
  begin
  {$ifdef UNICODE}
    result := IsCharUpper(aChar);
  {$else}
    result := aChar in ['A'..'Z'];
  {$endif}
  end;



{ TStringTool ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TStringTool.Create(const aString: String);
  begin
    inherited Create;
    fString := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringTool.Contains(const aSubStr: String): Boolean;
  begin
    result := (Pos(aSubStr, fString) <> 0);
  end;





{ TManagedStringList ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  function TManagedStringList.Add(const aString: String): Integer;
  begin
    result := fList.Add(aString);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  procedure TManagedStringList.Clear;
  begin
    fList.Clear;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  function TManagedStringList.Contains(const aString: String): Boolean;
  begin
    result := (fList.IndexOf(aString) <> -1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  procedure TManagedStringList.Delete(const aIndex: Integer);
  begin
    fList.Delete(aIndex);
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  function TManagedStringList.get_Count: Integer;
  begin
    result := fList.Count;
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  function TManagedStringList.get_Item(const aIndex: Integer): String;
  begin
    result := fList[aIndex];
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  function TManagedStringList.get_List: TStringList;
  begin
    result := fList;
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  function TManagedStringList.get_Name(const aIndex: Integer): String;
  begin
    result := fList.Names[aIndex];
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  function TManagedStringList.get_Value(const aName: String): String;
  begin
    result := fList.Values[aName];
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  procedure TManagedStringList.Insert(const aIndex: Integer;
                                      const aString: String);
  begin
    fList.Insert(aIndex, aString);
  end;




end.

