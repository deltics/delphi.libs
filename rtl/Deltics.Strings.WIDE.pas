

  unit Deltics.Strings.WIDE;


interface

  uses
    Deltics.Classes,
    Deltics.Strings;

  type
    TWIDEChar = class(TCOMInterfacedObject, IWIDEChar)
    private
      BOX: WIDEChar;
    public
      constructor Create(const aChar: WIDEChar);

    private
      function AlphaCase: TAlphaCase;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function Lowercase: WIDEChar;
      function Uppercase: WIDEChar;
      function ToANSI: ANSIChar;
      function ToWIDE: WIDEChar;
    end;


    TWIDEString = class(TCOMInterfacedObject, IWIDEString)
    private
      BOX: UnicodeString;
    public
      constructor Create(const aString: UnicodeString);

    private
      function ByteCount: Integer;
      function Length: Integer;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function ToANSI: ANSIString;
      function ToString: String; reintroduce;
      function ToUTF8: UTF8String;
      function ToWIDE: UnicodeString;
    private
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
      function FindFirstText(const aString: UnicodeString; var aPos: Integer): Boolean;
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

      function Delete(const aStart, aCount: Integer): UnicodeString; overload;
      function Embrace(const aBraceChar: ASCIIChar = '('): UnicodeString;
      function Enquote(const aQuoteChar: WIDEChar = ''''): UnicodeString;
      function ExtractLeft(const aCount: Integer): UnicodeString;
      function ExtractRight(const aCount: Integer): UnicodeString;
      function Lowercase: UnicodeString;
      function PadLeft(const aCount: Integer; const aChar: ASCIIChar): UnicodeString; overload;
      function PadRight(const aCount: Integer; const aChar: ASCIIChar): UnicodeString; overload;
      function PadToLengthLeft(const aMaxLen: Integer; const aChar: ASCIIChar): UnicodeString; overload;
      function PadToLengthRight(const aMaxLen: Integer; const aChar: ASCIIChar): UnicodeString; overload;
      function Remove(const aString: UnicodeString; const aFlags: TReplaceFlags): UnicodeString; overload;
      function Replace(const aFindStr, aReplaceStr: UnicodeString; const aFlags: TReplaceFlags): UnicodeString; overload;
      function Trim(const aChar: ASCIIChar): UnicodeString; overload;
      function TrimLeft(const aChar: ASCIIChar): UnicodeString; overload;
      function TrimLeft(const aCount: Integer): UnicodeString; overload;
      function TrimRight(const aChar: ASCIIChar): UnicodeString; overload;
      function TrimRight(const aCount: Integer): UnicodeString; overload;
      function Unbrace: UnicodeString;
      function Unquote: UnicodeString;
      function Uppercase: UnicodeString;
    end;




implementation

  uses
    SysUtils,
    Windows;



  function TWIDEChar.AlphaCase: TAlphaCase;
  begin
    result := acNotAlpha;
    if NOT IsCharAlphaW(BOX) then
      EXIT;

    if IsCharUpperW(BOX) then
      result := acUpper
    else
      result := acLower;
  end;


  constructor TWIDEChar.Create(const aChar: WIDEChar);
  begin
    inherited Create;
    BOX := aChar;
  end;


  function TWIDEChar.IsLowercase: Boolean;
  begin
    result := IsCharLowerW(BOX);
  end;


  function TWIDEChar.IsUppercase: Boolean;
  begin
    result := IsCharUpperW(BOX);
  end;


  function TWIDEChar.Lowercase: WIDEChar;
  begin
    result := BOX;
    CharLowerBuffW(@result, 1);
  end;


  function TWIDEChar.Uppercase: WIDEChar;
  begin
    result := BOX;
    CharUpperBuffW(@result, 1);
  end;


  function TWIDEChar.ToANSI: ANSIChar;
  begin
    WideCharToMultiByte(CP_ACP, 0, @BOX, 1, @result, 1, NIL, NIL);
  end;











  function TWIDEChar.ToWIDE: WIDEChar;
  begin
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TWIDEString.Create(const aString: UnicodeString);
  begin
    inherited Create;
    BOX := aString;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Data: PWIDEChar;
  begin
    result := PWIDEChar(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ByteCount: Integer;
  begin
    result := System.Length(BOX) * 2;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Length: Integer;
  begin
    result := System.Length(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.IsLowercase: Boolean;
  begin
    result := WIDE.IsLowercase(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.IsUppercase: Boolean;
  begin
    result := WIDE.IsUppercase(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ToANSI: ANSIString;
  begin
    result := ANSI.FromWIDE(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ToString: String;
  begin
  {$ifdef UNICODE}
    result := BOX;
  {$else}
    result := ANSI.FromWIDE(BOX);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ToUTF8: UTF8String;
  begin
    result := UTF8.FromWIDE(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ToWIDE: UnicodeString;
  begin
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.BeginsWith(const aString: UnicodeString): Boolean;
  var
    alen: Integer;
  begin
    alen := System.Length(aString);

    result := (alen <= System.Length(BOX))
          and (CompareStringW(LOCALE_USER_DEFAULT, 0,
                              PWIDEChar(BOX), alen,
                              PWIDEChar(aString), alen) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.BeginsWithText(const aString: UnicodeString): Boolean;
  var
    alen: Integer;
  begin
    alen := System.Length(aString);

    result := (alen <= System.Length(BOX))
          and (CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                              PWIDEChar(BOX), alen,
                              PWIDEChar(aString), alen) = CSTR_EQUAL);
  end;


  function TWIDEString.CompareWith(const aString: UnicodeString): Integer;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, 0,
                             PWIDEChar(BOX), System.Length(BOX),
                             PWIDEChar(aString), System.Length(aString)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.CompareWithText(const aString: UnicodeString): Integer;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PWIDEChar(BOX), System.Length(BOX),
                             PWIDEChar(aString), System.Length(aString)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Contains(const aString: UnicodeString): Boolean;
  begin
    result := System.Pos(aString, BOX) <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Contains(const aChar: WIDEChar): Boolean;
  begin
    result := System.Pos(aChar, BOX) <> 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ContainsText(const aChar: WIDEChar): Boolean;
  var
    pos: Integer;
  begin
    result := FindFirstText(aChar, pos);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ContainsText(const aString: UnicodeString): Boolean;
  var
    pos: Integer;
  begin
    result := FindFirstText(aString, pos);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.CopyFrom(const aStart, aCount: Integer): UnicodeString;
  begin
    result := Copy(BOX, aStart, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.CopyRange(const aStart, aEnd: Integer): UnicodeString;
  begin
    result := Copy(BOX, aStart, aEnd - aStart);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.EqualsText(const aString: UnicodeString): Boolean;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PWIDEChar(BOX), System.Length(BOX),
                             PWIDEChar(aString), System.Length(aString)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Find(const aChar: ANSIChar;
                            var aPos: TCharIndexArray): Boolean;
  begin
    result := Find(ANSI(aChar).ToWIDE, aPos);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Find(const aChar: WIDEChar;
                            var aPos: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strlen: Integer;
    firstChar: PWIDEChar;
    currChar: PWIDEChar;
  begin
    result  := FALSE;
    strlen  := System.Length(BOX);

    if (strlen = 0) then
      EXIT;

    SetLength(aPos, strlen);
    j         := 0;
    firstChar := PWIDEChar(BOX);
    currChar  := firstChar;

    for i := Pred(strlen) downto 0 do
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
  function TWIDEString.Find(const aString: UnicodeString;
                            var aPos: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    currChar: PWIDEChar;
    firstChar: PWIDEChar;
    psub: PWIDEChar;
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
    firstChar := PWIDEChar(BOX);
    currChar  := firstChar;
    psub      := PWIDEChar(aString);
    for i := (strlen - sublen) downto 0 do
    begin
      if (Windows.CompareStringW(LOCALE_USER_DEFAULT, 0,
                                 currChar, sublen,
                                 psub, sublen) = CSTR_EQUAL) then
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
  function TWIDEString.FindText(const aChar: ANSIChar;
                                var aPos: TCharIndexArray): Boolean;
  begin
    result := FindText(ANSI(aChar).ToWIDE, aPos);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindText(const aChar: WIDEChar;
                                var aPos: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    currChar: PWIDEChar;
    firstChar: PWIDEChar;
    psub: PWIDEChar;
    strlen: Integer;
  begin
    result  := FALSE;
    strlen  := System.Length(BOX);
    if (strlen = 0) then
      EXIT;

    SetLength(aPos, strlen);
    j         := 0;
    firstChar := PWIDEChar(BOX);
    currChar  := firstChar;
    psub      := @aChar;
    for i := (strlen - 1) downto 0 do
    begin
      if (Windows.CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                                 currChar, 1,
                                 psub, 1) = CSTR_EQUAL) then
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
  function TWIDEString.FindText(const aString: UnicodeString;
                                var aPos: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    currChar: PWIDEChar;
    firstChar: PWIDEChar;
    psub: PWIDEChar;
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
    firstChar := PWIDEChar(BOX);
    currChar  := firstChar;
    psub      := PWIDEChar(aString);
    for i := (strlen - sublen) downto 0 do
    begin
      if (Windows.CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                                 currChar, sublen,
                                 psub, sublen) = CSTR_EQUAL) then
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
  function TWIDEString.FindFirst(const aChar: ANSIChar;
                                 var aPos: Integer): Boolean;
  begin
    result := FindFirst(ANSI(aChar).ToWIDE, aPos);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindFirst(const aChar: WIDEChar;
                                 var aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aChar, BOX);
    result  := (aPos <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindFirst(const aString: UnicodeString;
                                 var aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aString, BOX);
    result  := (aPos <> 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindFirstText(const aString: UnicodeString;
                                     var aPos: Integer): Boolean;
  var
    i: Integer;
    currChar: PWIDEChar;
    firstChar: PWIDEChar;
    psub: PWIDEChar;
    strlen: Integer;
    sublen: Integer;
  begin
    result  := FALSE;
    subLen  := System.Length(aString);
    strLen  := System.Length(BOX);
    if (strlen = 0) or (sublen > strlen)then
      EXIT;

    firstChar := PWIDEChar(BOX);
    currChar  := firstChar;
    psub      := PWIDEChar(aString);
    for i := (strlen - sublen) downto 0 do
    begin
      result := Windows.CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
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
  function TWIDEString.FindLast(const aChar: ANSIChar;
                                var aPos: Integer): Boolean;
  begin
    result := FindLast(ANSI(aChar).ToWIDE, aPos);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindLast(const aChar: WIDEChar;
                                var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    firstChar: PWIDEChar;
    currChar: PWIDEChar;
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


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindLast(const aString: UnicodeString;
                                var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PWIDEChar;
    currChar: PWIDEChar;
    initialChar: PWIDEChar;
  begin
    strLen  := System.Length(BOX);
    subLen  := System.Length(aString);
    aPos    := 0;
    result  := FALSE;

    if (subLen > strLen) or (subLen = 0) or (strLen = 0) then
      EXIT;

    firstChar   := @BOX[1];
    initialChar := @aString[1];
    currChar    := @BOX[strlen];

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
  function TWIDEString.FindNext(const aChar: ANSIChar;
                                var aPos: Integer): Boolean;
  begin
    result := FindNext(ANSI(aChar).ToWIDE, aPos);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindNext(const aChar: WIDEChar;
                                var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    currChar: PWIDEChar;
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
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindNext(const aString: UnicodeString;
                                var   aPos: Integer): Boolean;
  var
    i: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PWIDEChar;
    initialChar: PWIDEChar;
    currChar: PWIDEChar;
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
  function TWIDEString.IndexIn(const aArray: array of UnicodeString): Integer;
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
  function TWIDEString.IsIn(const aArray: array of UnicodeString): Boolean;
  begin
    result := IndexIn(aArray) <> -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Leftmost(const aCount: Integer): UnicodeString;
  begin
    result := Copy(BOX, 1, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Remove(const aString: UnicodeString;
                              const aFlags: TReplaceFlags): UnicodeString;
  begin
    BOX     := WIDE.Replace(BOX, aString, '', aFlags);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Replace(const aFindStr: UnicodeString;
                               const aReplaceStr: UnicodeString;
                               const aFlags: TReplaceFlags): UnicodeString;
  begin
    BOX     := WIDE.Replace(BOX, aFindStr, aReplaceStr, aFlags);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Rightmost(const aCount: Integer): UnicodeString;
  begin
    result := Copy(BOX, (System.Length(BOX) - aCount) + 1, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Split(const aChar: ANSIChar;
                             var aLeft, aRight: UnicodeString): Boolean;
  begin
    result := Split(ANSI(aChar).ToWIDE, aLeft, aRight);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Split(const aChar: WIDEChar;
                             var aLeft, aRight: UnicodeString): Boolean;
  var
    p: Integer;
  begin
    aLeft   := BOX;
    aRight  := '';

    result  := FindFirst(aChar, p);
    if NOT result then
      EXIT;

    SetLength(aLeft, p - 1);
    aRight := Copy(BOX, p + 1, System.Length(BOX) - p);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Split(const aChar: ANSIChar;
                             var aParts: TWIDEStringArray): Boolean;
  begin
    result := Split(ANSI(aChar).ToWIDE, aParts);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Split(const aChar: WIDEChar;
                             var aParts: TWIDEStringArray): Boolean;
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
  function TWIDEString.Delete(const aStart, aCount: Integer): UnicodeString;
  begin
    System.Delete(BOX, aStart, aCount);
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Embrace(const aBraceChar: ASCIIChar): UnicodeString;
  begin
    BOX     := WIDE.Embrace(BOX, aBraceChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Enquote(const aQuoteChar: WIDEChar): UnicodeString;
  begin
    BOX     := WIDE.Enquote(BOX, aQuoteChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ExtractLeft(const aCount: Integer): UnicodeString;
  begin
    result := Copy(BOX, 1, aCount);
    System.Delete(BOX, 1, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.ExtractRight(const aCount: Integer): UnicodeString;
  begin
    result := Copy(BOX, (System.Length(BOX) - aCount) + 1, aCount);
    SetLength(BOX, System.Length(BOX) - aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Lowercase: UnicodeString;
  var
    len: Integer;
  begin
    len := System.Length(BOX);
    if len > 0 then
      CharLowerBuffW(PWIDEChar(BOX), len);

    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.PadLeft(const aCount: Integer;
                               const aChar: ASCIIChar): UnicodeString;
  begin
    BOX     := WIDE.PadLeft(BOX, aCount, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.PadRight(const aCount: Integer;
                                const aChar: ASCIIChar): UnicodeString;
  begin
    BOX     := WIDE.PadRight(BOX, aCount, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.PadToLengthLeft(const aMaxLen: Integer;
                                       const aChar: ASCIIChar): UnicodeString;
  begin
    BOX     := WIDE.PadLeft(BOX, aMaxLen, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.PadToLengthRight(const aMaxLen: Integer;
                                        const aChar: ASCIIChar): UnicodeString;
  begin
    BOX     := WIDE.PadRight(BOX, aMaxLen, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.TrimLeft(const aChar: ASCIIChar): UnicodeString;
  begin
    BOX     := WIDE.TrimLeft(BOX, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Trim(const aChar: ASCIIChar): UnicodeString;
  begin
    BOX       := WIDE.Trim(BOX, aChar);
    result    := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.TrimLeft(const aCount: Integer): UnicodeString;
  begin
    System.Delete(BOX, 1, aCount);
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.TrimRight(const aChar: ASCIIChar): UnicodeString;
  begin
    BOX     := WIDE.TrimRight(BOX, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.TrimRight(const aCount: Integer): UnicodeString;
  begin
    SetLength(BOX, System.Length(BOX) - aCount);
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Unbrace: UnicodeString;
  begin
    BOX     := WIDE.Unbrace(BOX);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Unquote: UnicodeString;
  begin
    BOX     := WIDE.Unquote(BOX);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Uppercase: UnicodeString;
  var
    len: Integer;
  begin
    len := System.Length(BOX);
    if len > 0 then
      CharUpperBuffW(PWIDEChar(BOX), len);

    result := BOX;
  end;








end.
