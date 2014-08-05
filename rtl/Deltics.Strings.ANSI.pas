

  unit Deltics.Strings.ANSI;


interface

  uses
    Deltics.Classes,
    Deltics.Strings;


  type
    TANSIChar = class(TCOMInterfacedObject, IANSIChar)
    private
      BOX: ANSIChar;
    public
      constructor Create(const aChar: ANSIChar);

    private
      function AlphaCase: TAlphaCase;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function Lowercase: ANSIChar;
      function Uppercase: ANSIChar;
      function ToANSI: ANSIChar;
      function ToWIDE: WIDEChar;
    end;


    TANSIString = class(TCOMInterfacedObject, IANSIString)
    private
      BOX: ANSIString;
    public
      constructor Create(const aString: ANSIString);

    private // IString
      function ByteCount: Integer;
      function Length: Integer;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function ToANSI: ANSIString;
      function ToString: String; reintroduce;
      function ToUTF8: UTF8String;
      function ToWIDE: UnicodeString;

    private // IANSIString
      function BeginsWith(const aString: ANSIString): Boolean;
      function BeginsWithText(const aString: ANSIString): Boolean;
      function CompareWith(const aString: ANSIString): Integer;
      function CompareWithText(const aString: ANSIString): Integer;
      function Contains(const aChar: ANSIChar): Boolean; overload;
      function Contains(const aString: ANSIString): Boolean; overload;
      function ContainsText(const aChar: ANSIChar): Boolean; overload;
      function ContainsText(const aString: ANSIString): Boolean; overload;
      function Data: PANSIChar;
      function EqualsText(const aString: ANSIString): Boolean;
      function Find(const aChar: ANSIChar; var aPos: TCharIndexArray): Boolean; overload;
      function Find(const aString: ANSIString; var aPos: TCharIndexArray): Boolean; overload;
      function FindText(const aString: ANSIString; var aPos: TCharIndexArray): Boolean; overload;
      function FindFirst(const aString: ANSIString; var aPos: Integer): Boolean; overload;
      function FindFirst(const aChar: ANSIChar; var aPos: Integer): Boolean; overload;
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
      function Embrace(const aBraceChar: ASCIIChar = ''''): ANSIString;
      function Enquote(const aQuoteChar: ANSIChar = ''''): ANSIString;
      function ExtractLeft(const aCount: Integer): ANSIString;
      function ExtractRight(const aCount: Integer): ANSIString;
      function Lowercase: ANSIString;
      function PadLeft(const aCount: Integer; const aChar: ASCIIChar): ANSIString; overload;
      function PadRight(const aCount: Integer; const aChar: ASCIIChar): ANSIString; overload;
      function PadToLengthLeft(const aMaxLen: Integer; const aChar: ASCIIChar): ANSIString; overload;
      function PadToLengthRight(const aMaxLen: Integer; const aChar: ASCIIChar): ANSIString; overload;
      function Remove(const aString: ANSIString; const aFlags: TReplaceFlags): ANSIString; overload;
      function Replace(const aString: ANSIString; const aFindStr, aReplaceStr: ANSIString; const aFlags: TReplaceFlags): ANSIString; overload;
      function Trim(const aChar: ASCIIChar): ANSIString; overload;
      function TrimLeft(const aChar: ASCIIChar): ANSIString; overload;
      function TrimLeft(const aCount: Integer): ANSIString; overload;
      function TrimRight(const aChar: ASCIIChar): ANSIString; overload;
      function TrimRight(const aCount: Integer): ANSIString; overload;
      function Unbrace: ANSIString;
      function Unquote: ANSIString;
      function Uppercase: ANSIString;
    end;


implementation

  uses
  {$ifdef FASTSTRINGS}
    FastStrings,
  {$endif}
  { vcl: }
  {$ifdef DELPHIXE4_OR_LATER}
    ANSIStrings,
  {$endif}
    SysUtils,
    Windows;





{ TANSIChar }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TANSIChar.Create(const aChar: ANSIChar);
  begin
    inherited Create;
    BOX := aChar;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIChar.AlphaCase: TAlphaCase;
  begin
    result := acNotAlpha;
    if NOT IsCharAlphaA(BOX) then
      EXIT;

    if IsCharUpperA(BOX) then
      result := acUpper
    else
      result := acLower;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIChar.ToANSI: ANSIChar;
  begin
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIChar.IsLowercase: Boolean;
  begin
    result := IsCharLowerA(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIChar.IsUppercase: Boolean;
  begin
    result := IsCharUpperA(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIChar.ToWIDE: WIDEChar;
  begin
    MultiByteToWideChar(CP_ACP, 0, @BOX, 1, @result, 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIChar.Lowercase: ANSIChar;
  begin
    CharLowerBuffA(PANSIChar(BOX), 1);
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIChar.Uppercase: ANSIChar;
  begin
    CharUpperBuffA(PANSIChar(BOX), 1);
    result := BOX;
  end;












  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TANSIString.Create(const aString: ANSIString);
  begin
    inherited Create;
    BOX := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.BeginsWith(const aString: ANSIString): Boolean;
  var
    alen: Integer;
  begin
    alen := System.Length(aString);

    result := (alen <= System.Length(BOX))
          and (CompareStringA(LOCALE_USER_DEFAULT, 0,
                              PANSIChar(BOX), alen,
                              PANSIChar(aString), alen) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.BeginsWithText(const aString: ANSIString): Boolean;
  var
    alen: Integer;
  begin
    alen := System.Length(aString);

    result := (alen <= System.Length(BOX))
          and (CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                              PANSIChar(BOX), alen,
                              PANSIChar(aString), alen) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ByteCount: Integer;
  begin
    result := System.Length(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Leftmost(const aCount: Integer): ANSIString;
  begin
    result := Copy(BOX, 1, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Length: Integer;
  begin
    result := System.Length(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.IsLowercase: Boolean;
  begin
    result := ANSI.IsLowercase(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.IsUppercase: Boolean;
  begin
    result := ANSI.IsUppercase(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.CompareWith(const aString: ANSIString): Integer;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, 0,
                             PANSIChar(BOX), System.Length(BOX),
                             PANSIChar(aString), System.Length(aString)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.CompareWithText(const aString: ANSIString): Integer;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PANSIChar(BOX), System.Length(BOX),
                             PANSIChar(aString), System.Length(aString)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Contains(const aChar: ANSIChar): Boolean;
  begin
  {$ifdef FASTSTRINGS}
    result := FastStrings.FastCharPos(BOX, aChar, 1) <> 0;
  {$else}
    result := System.Pos(aChar, BOX) <> 0;
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Contains(const aString: ANSIString): Boolean;
  begin
  {$ifdef FASTSTRINGS}
    result := FastStrings.FastPos(BOX, aString, System.Length(BOX), System.Length(aString), 1) <> 0;
  {$else}
    result := System.Pos(aString, BOX) <> 0;
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ContainsText(const aChar: ANSIChar): Boolean;
  var
    pos: Integer;
  begin
    result := FindFirstText(aChar, pos);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ContainsText(const aString: ANSIString): Boolean;
  var
    pos: Integer;
  begin
    result := FindFirstText(aString, pos);
  end;


{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Data: PANSIChar;
  begin
    result := PANSIChar(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.EqualsText(const aString: ANSIString): Boolean;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PANSIChar(BOX), System.Length(BOX),
                             PANSIChar(aString), System.Length(aString)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Find(const aChar: ANSIChar;
                            var aPos: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strLen: Integer;
    firstChar: PANSIChar;
    currChar: PANSIChar;
  begin
    result  := FALSE;
    strLen  := System.Length(BOX);
    if (strLen = 0) then
      EXIT;

    SetLength(aPos, strLen);
    j         := 0;
    firstChar := @BOX[1];
    currChar  := firstChar;

    for i := Pred(strLen) downto 0 do
    begin
      if (currChar^ = aChar) then
      begin
        aPos[j] := (currChar - firstChar) + 1;
        Inc(j);
      end;
      Inc(currChar);
    end;
    SetLength(aPos, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Find(const aString: ANSIString;
                            var aPos: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    currChar: PANSIChar;
    firstChar: PANSIChar;
    psub: PANSIChar;
    strlen: Integer;
    sublen: Integer;
  begin
    result  := FALSE;
    subLen  := System.Length(aString);
    strLen  := System.Length(BOX);
    if (strLen = 0) or (strlen = 0) or (sublen > strlen)then
      EXIT;

    SetLength(aPos, strlen);

    j         := 0;
    firstChar := PANSIChar(BOX);
    currChar  := firstChar;
    psub      := PANSIChar(aString);

    for i := (strlen - sublen) downto 0 do
    begin
      result := Windows.CompareStringA(LOCALE_USER_DEFAULT, 0,
                                       currChar, sublen,
                                       psub, sublen) = CSTR_EQUAL;
      if result then
      begin
        aPos[j] := (currChar - firstChar) + 1;
        Inc(j)
      end;

      Inc(currChar);
    end;

    SetLength(aPos, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.FindText(const aString: ANSIString;
                                var aPos: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    currChar: PANSIChar;
    firstChar: PANSIChar;
    psub: PANSIChar;
    strlen: Integer;
    sublen: Integer;
  begin
    result  := FALSE;
    subLen  := System.Length(aString);
    strLen  := System.Length(BOX);
    if (strLen = 0) or (strlen = 0) or (sublen > strlen)then
      EXIT;

    SetLength(aPos, strlen);

    j         := 0;
    firstChar := PANSIChar(BOX);
    currChar  := firstChar;
    psub      := PANSIChar(aString);

    for i := (strlen - sublen) downto 0 do
    begin
      result := Windows.CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                                       currChar, sublen,
                                       psub, sublen) = CSTR_EQUAL;
      if result then
      begin
        aPos[j] := (currChar - firstChar) + 1;
        Inc(j)
      end;

      Inc(currChar);
    end;

    SetLength(aPos, j);
    result := (j > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.FindFirst(const aString: ANSIString;
                                 var aPos: Integer): Boolean;
  begin
  {$ifdef FASTSTRINGS}
    aPos := FastStrings.FastPos(BOX, aString, System.Length(BOX), System.Length(aString), 1);
  {$else}
    aPos := System.Pos(aString, BOX);
  {$endif}
    result := aPos <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.FindFirstText(const aString: ANSIString;
                                     var aPos: Integer): Boolean;
  var
    i: Integer;
    currChar: PANSIChar;
    firstChar: PANSIChar;
    psub: PANSIChar;
    strlen: Integer;
    sublen: Integer;
  begin
    result  := FALSE;
    subLen  := System.Length(aString);
    strLen  := System.Length(BOX);
    if (strLen = 0) or (strlen = 0) or (sublen > strlen)then
      EXIT;

    firstChar := PANSIChar(BOX);
    currChar  := firstChar;
    psub      := PANSIChar(aString);
    for i := (strlen - sublen) downto 0 do
    begin
      result := Windows.CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                                       currChar, sublen,
                                       psub, sublen) = CSTR_EQUAL;
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
  function TANSIString.FindLast(const aChar: ANSIChar;
                                var   aPos: Integer): Boolean;
  {$ifdef FASTSTRINGS}
    begin
      aPos    := FastStrings.FastPosBack(BOX, aChar, System.Length(BOX), 1, System.Length(BOX));
      result  := aPos <> 0;
    end;
  {$else}
    var
      i: Integer;
      strLen: Integer;
      firstChar: PANSIChar;
      currChar: PANSIChar;
    begin
      strLen  := System.Length(BOX);
      aPos    := 0;
      result  := FALSE;
      if (strLen = 0) then
        EXIT;

      firstChar := @BOX[1];
      currChar  := @BOX[strLen];

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
  {$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.FindLast(const aString: ANSIString;
                                var   aPos: Integer): Boolean;
  {$ifdef FASTSTRINGS}
    begin
      aPos    := FastStrings.FastPosBack(BOX, aString, System.Length(BOX), System.Length(aString), System.Length(BOX));
      result  := aPos <> 0;
    end;
  {$else}
    var
      i: Integer;
      strLen: Integer;
      subLen: Integer;
      firstChar: PANSIChar;
      currChar: PANSIChar;
      initialChar: PANSIChar;
    begin
      strLen  := System.Length(BOX);
      subLen  := System.Length(aString);
      aPos    := 0;
      result  := FALSE;

      if (subLen > strLen) or (subLen = 0) or (strLen = 0) then
        EXIT;

      firstChar   := @BOX[1];
      initialChar := @aString[1];
      currChar    := @BOX[strLen];

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
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.FindNext(const aChar: ANSIChar;
                                var aPos: Integer): Boolean;
  {$ifdef FASTSTRINGS}
    begin
      ASSERT(aPos >= 0);

      aPos := FastStrings.FastCharPos(BOX, aChar, aPos);
      result := aPos > 0;
    end;
  {$else}
  var
    i: Integer;
    strLen: Integer;
    currChar: PANSIChar;
  begin
    ASSERT(aPos >= 0);

    result  := FALSE;
    strLen  := System.Length(BOX);

    if (aPos >= strLen) then
    begin
      aPos := 0;
      EXIT;
    end;

    currChar := @BOX[aPos + 1];
    for i := Pred(strLen) downto aPos do
    begin
      result := (currChar^ = aChar);
      if result then
        BREAK;
      Inc(currChar);
    end;

    if result then
      aPos := (currChar - @BOX[1]) + 1
    else
      aPos := 0;
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.FindNext(const aString: ANSIString;
                                var aPos: Integer): Boolean;
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
    strLen  := System.Length(BOX);
    subLen  := System.Length(aString);

    if ((aPos + subLen) > strLen) or (strLen = 0) or (subLen = 0) then
    begin
      aPos := 0;
      EXIT;
    end;

    firstChar   := @BOX[1];
    initialChar := @aString[1];
    currChar    := @BOX[aPos + 1];
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
  function TANSIString.FindFirst(const aChar: ANSIChar;
                                 var aPos: Integer): Boolean;
  begin
  {$ifdef FASTSTRINGS}
    aPos := FastStrings.FastCharPos(BOX, aChar, 1);
  {$else}
    aPos := System.Pos(aChar, BOX);
  {$endif}
    result := aPos > 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.IndexIn(const aArray: array of ANSIString): Integer;
  var
    i: Integer;
  begin
    for i := 0 to Pred(System.Length(aArray)) do
      if (aArray[i] = BOX) then
      begin
        result := i;
        EXIT;
      end;

    result := -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.IsIn(const aArray: array of ANSIString): Boolean;
  begin
    result := IndexIn(aArray) <> -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Split(const aChar: ANSIChar;
                             var aLeft, aRight: ANSIString): Boolean;
  var
    p: Integer;
  begin
    aLeft   := BOX;
    aRight  := '';

    result := FindFirst(aChar, p);
    if NOT result then
      EXIT;

    SetLength(aLeft, p - 1);
    aRight := Copy(BOX, p + 1, System.Length(BOX) - p);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Split(const aChar: ANSIChar;
                             var aParts: TANSIStringArray): Boolean;
  var
    i: Integer;
    p: TCharIndexArray;
    plen: Integer;
  begin
    result := Find(aChar, p);
    if NOT result then
      EXIT;

    plen := System.Length(p);
    SetLength(aParts, plen + 1);

    aParts[0] := Copy(BOX, 1, p[0] - 1);
    for i := 1 to Pred(plen) do
      aParts[i] := Copy(BOX, p[i - 1] + 1, p[i] - p[i - 1] - 1);

    i := p[Pred(plen)] + 1;
    aParts[plen] := Copy(BOX, i, System.Length(BOX) - i + 1)
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ToANSI: ANSIString;
  begin
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ToString: String;
  begin
  {$ifdef UNICODE}
    result := WIDE.FromANSI(BOX);
  {$else}
    result := BOX;
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ToUTF8: UTF8String;
  begin
    result := UTF8.FromANSI(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ToWIDE: UnicodeString;
  begin
    result := WIDE.FromANSI(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Rightmost(const aCount: Integer): ANSIString;
  begin
    result := Copy(BOX, (System.Length(BOX) - aCount) + 1, aCount);
  end;








  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Delete(const aStart, aCount: Integer): ANSIString;
  begin
    System.Delete(BOX, aStart, aCount);;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Embrace(const aBraceChar: ASCIIChar): ANSIString;
  begin
    BOX     := ANSI.Embrace(BOX, aBraceChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Enquote(const aQuoteChar: ANSIChar): ANSIString;
  begin
    BOX     := ANSI.Enquote(BOX, aQuoteChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ExtractLeft(const aCount: Integer): ANSIString;
  begin
    result := Copy(BOX, 1, aCount);
    System.Delete(BOX, 1, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ExtractRight(const aCount: Integer): ANSIString;
  begin
    result := Copy(BOX, (System.Length(BOX) - aCount) + 1, aCount);
    SetLength(BOX, System.Length(BOX) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Lowercase: ANSIString;
  var
    len: Integer;
  begin
    len := System.Length(BOX);
    if len > 0 then
      CharLowerBuffA(PANSIChar(BOX), len);

    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.PadLeft(const aCount: Integer;
                               const aChar: ASCIIChar): ANSIString;
  begin
    BOX     := ANSI.PadLeft(BOX, aCount, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.PadRight(const aCount: Integer;
                                const aChar: ASCIIChar): ANSIString;
  begin
    BOX     := ANSI.PadRight(BOX, aCount, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.PadToLengthLeft(const aMaxLen: Integer;
                                       const aChar: ASCIIChar): ANSIString;
  begin
    BOX     := ANSI.PadToLengthLeft(BOX, aMaxLen, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.PadToLengthRight(const aMaxLen: Integer;
                                        const aChar: ASCIIChar): ANSIString;
  begin
    BOX     := ANSI.PadToLengthRight(BOX, aMaxLen, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Remove(const aString: ANSIString;
                              const aFlags: TReplaceFlags): ANSIString;
  begin
    BOX     := ANSI.Replace(BOX, aString, '', aFlags);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Replace(const aString: ANSIString;
                               const aFindStr: ANSIString;
                               const aReplaceStr: ANSIString;
                               const aFlags: TReplaceFlags): ANSIString;
  begin
    BOX     := ANSI.Replace(BOX, aFindStr, aReplaceStr, aFlags);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.TrimLeft(const aChar: ASCIIChar): ANSIString;
  begin
    BOX     := ANSI.TrimLeft(BOX, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Trim(const aChar: ASCIIChar): ANSIString;
  begin
    BOX       := ANSI.Trim(BOX, aChar);
    result    := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.TrimLeft(const aCount: Integer): ANSIString;
  begin
    BOX     := ANSI.TrimLeft(BOX, aCount);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.TrimRight(const aChar: ASCIIChar): ANSIString;
  begin
    BOX     := ANSI.TrimRight(BOX, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.TrimRight(const aCount: Integer): ANSIString;
  begin
    BOX     := ANSI.TrimRight(BOX, aCount);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Unbrace: ANSIString;
  begin
    BOX     := ANSI.Unbrace(BOX);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Unquote: ANSIString;
  begin
    BOX     := ANSI.Unquote(BOX);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Uppercase: ANSIString;
  var
    len: Integer;
  begin
    len := System.Length(BOX);
    if len > 0 then
      CharUpperBuffA(PANSIChar(BOX), len);

    result := BOX;
  end;








end.
