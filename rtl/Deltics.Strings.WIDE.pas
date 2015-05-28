

  unit Deltics.Strings.WIDE;


interface

  uses
    SysUtils,
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
      function get_Length: Integer;
      procedure set_Length(const aValue: Integer);

      function ByteCount: Integer;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function ToANSI: ANSIString;
      function ToString: String; reintroduce;
      function ToUTF8: UTF8String;
      function ToWIDE: UnicodeString;
    private
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
      procedure CopyTo(const aDest: PANSIChar; const aMaxBytes: Integer = -1); overload;
      procedure CopyTo(const aDest: PUTF8Char; const aMaxBytes: Integer = -1); overload;
      procedure CopyTo(const aDest: PWIDEChar; const aMaxChars: Integer = -1); overload;
      function Data: PWIDEChar;
      function EndsWith(const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean;
      function EqualsText(const aString: UnicodeString): Boolean;

      function Find(const aChar: WIDEChar; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Find(const aString: UnicodeString; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Find(const aChar: WIDEChar; var aPos: TCharIndexArray; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer; overload;
      function Find(const aString: UnicodeString; var aPos: TCharIndexArray; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer; overload;
//      function FindFirst(const aChar: WIDEChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
//      function FindFirst(const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
      function FindLast(const aChar: WIDEChar; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function FindLast(const aString: UnicodeString; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
//      function FindLast(const aChar: WIDEChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
//      function FindLast(const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;

      function IndexIn(const aArray: array of UnicodeString; const aCaseMode: TCaseSensitivity): Integer;
      function IsOneOf(const aArray: array of UnicodeString; const aCaseMode: TCaseSensitivity): Boolean;
      function Leading(const aCount: Integer): UnicodeString;
      function Trailing(const aCount: Integer): UnicodeString;
      function Split(const aChar: ANSIChar; var aLeft, aRight: UnicodeString): Boolean; overload;
      function Split(const aChar: WIDEChar; var aLeft, aRight: UnicodeString): Boolean; overload;
      function Split(const aChar: ANSIChar; var aParts: TWIDEStringArray): Boolean; overload;
      function Split(const aChar: WIDEChar; var aParts: TWIDEStringArray): Boolean; overload;

      function Delete(const aStart, aCount: Integer): UnicodeString; overload;
      function Embrace(const aBraceChar: WIDEChar = '('): UnicodeString;
      function Enquote(const aQuoteChar: WIDEChar = ''''): UnicodeString;
      function ExtractLeft(const aCount: Integer): UnicodeString;
      function ExtractRight(const aCount: Integer): UnicodeString;
      function Lowercase: UnicodeString;
      function PadLeft(const aCount: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;
      function PadRight(const aCount: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;
      function PadToLengthLeft(const aMaxLen: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;
      function PadToLengthRight(const aMaxLen: Integer; const aChar: WIDEChar = ' '): UnicodeString; overload;
      function Remove(const aScope: TStringScope; const aString: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): UnicodeString; overload;
      function Replace(const aScope: TStringScope; const aFindStr, aReplaceStr: UnicodeString; const aCaseMode: TCaseSensitivity = csCaseSensitive): UnicodeString; overload;
      function Trim(const aChar: WIDEChar): UnicodeString; overload;
      function RemoveLeading(const aChar: WIDEChar): UnicodeString; overload;
      function RemoveLeading(const aCount: Integer): UnicodeString; overload;
      function RemoveTrailing(const aChar: WIDEChar): UnicodeString; overload;
      function RemoveTrailing(const aCount: Integer): UnicodeString; overload;
      function Unbrace: UnicodeString;
      function Unquote: UnicodeString;
      function Uppercase: UnicodeString;
    end;




implementation

  uses
    Windows,
    Deltics.Strings.FXUtils;


  type TStepCharProcW = procedure(var aChar: PWIDEChar);

  procedure StepForward(var aChar: PWIDEChar);  begin Inc(aChar); end;
  procedure StepBack(var aChar: PWIDEChar);     begin Dec(aChar); end;



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
  function TWIDEString.get_Length: Integer;
  begin
    result := Length(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEString.set_Length(const aValue: Integer);
  begin
    SetLength(BOX, aValue);
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
  function TWIDEString.AllocANSI: PANSIChar;
  begin
    result := WIDE.AllocANSI(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.AllocUTF8: PUTF8Char;
  begin
    result := WIDE.AllocUTF8(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.AllocWIDE: PWIDEChar;
  begin
    result := WIDE.AllocWIDE(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.BeginsWith(const aString: UnicodeString;
                                  const aCaseMode: TCaseSensitivity): Boolean;
  var
    alen: Integer;
  begin
    alen := System.Length(aString);

    result := (alen <= System.Length(BOX))
          and (CompareStringW(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                              PWIDEChar(BOX), alen,
                              PWIDEChar(aString), alen) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.CompareWith(const aString: UnicodeString;
                                   const aCaseMode: TCaseSensitivity): Integer;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                             PWIDEChar(BOX), System.Length(BOX),
                             PWIDEChar(aString), System.Length(aString)) - CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Contains(const aChar: WIDEChar;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    notUsed: Integer;
  begin
    result := Find(aChar, notUsed, aCaseMode);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Contains(const aString: UnicodeString;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    notUsed: Integer;
  begin
    result := Find(aString, notUsed, aCaseMode);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Contains(const aNeed: TContainNeeds;
                                const aChars: array of WIDEChar;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    notUsed: Integer;
    foundOne: Boolean;
  begin
    foundOne := FALSE;

    for i := Low(aChars) to High(aChars) do
    begin
      result := Find(aChars[i], notUsed, aCaseMode);

      if FXContains.HasResult(aNeed, result, foundOne) then
        EXIT;
    end;

    FXContains.CheckFinalResult(aNeed, result, foundOne);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Contains(const aNeed: TContainNeeds;
                                const aStrings: array of UnicodeString;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    notUsed: Integer;
    foundOne: Boolean;
  begin
    foundOne := FALSE;

    for i := Low(aStrings) to High(aStrings) do
    begin
      result := Find(aStrings[i], notUsed, aCaseMode);

      if FXContains.HasResult(aNeed, result, foundOne) then
        EXIT;
    end;

    FXContains.CheckFinalResult(aNeed, result, foundOne);
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
  procedure TWIDEString.CopyTo(const aDest: PANSIChar;
                               const aMaxBytes: Integer);
  begin
    WIDE.CopyToBuffer(BOX, aDest, aMaxBytes);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEString.CopyTo(const aDest: PUTF8Char;
                               const aMaxBytes: Integer);
  begin
    WIDE.CopyToBuffer(BOX, aDest, aMaxBytes);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEString.CopyTo(const aDest: PWIDEChar;
                               const aMaxChars: Integer);
  begin
    WIDE.CopyToBuffer(BOX, aDest, aMaxChars);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.EndsWith(const aString: UnicodeString;
                                const aCaseMode: TCaseSensitivity): Boolean;
  const
    NORMALISE: array[FALSE..TRUE] of Cardinal = (0, NORM_IGNORECASE);
  var
    alen: Integer;
  begin
    alen := System.Length(aString);

    result := (alen <= System.Length(BOX))
          and (CompareStringW(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                              PWIDEChar(Integer(@BOX) - alen), alen,
                              PWIDEChar(aString), alen) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.EqualsText(const aString: UnicodeString): Boolean;
  begin
    result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                             PWIDEChar(BOX), System.Length(BOX),
                             PWIDEChar(aString), System.Length(aString)) = CSTR_EQUAL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Find(const aChar: WIDEChar;
                            var   aPos: Integer;
                            const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    boxLen: Integer;
    first: PWIDEChar;
    curr: PWIDEChar;
  begin
    aPos    := 0;
    result  := FALSE;

    boxLen := System.Length(BOX);
    if (boxLen = 0) then
      EXIT;

    first := PWIDEChar(BOX);
    curr  := first;

    case aCaseMode of
      csCaseSensitive : for i := Pred(boxLen) downto 0 do
                        begin
                          result := (curr^ = aChar);
                          if result then
                            BREAK;

                          Inc(curr);
                        end;

      csIgnoreCase    : for i := Pred(boxLen) downto 0 do
                        begin
                          result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                                                   curr, 1, @aChar, 1) = CSTR_EQUAL;
                          if result then
                            BREAK;

                          Inc(curr);
                        end;
    end;

    if result then
      aPos := (curr - first) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Find(const aString: UnicodeString;
                            var   aPos: Integer;
                            const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    boxLen: Integer;
    strLen: Integer;
    first: PWIDEChar;
    curr: PWIDEChar;
  begin
    aPos   := 0;
    result := FALSE;

    boxLen := System.Length(BOX);
    strLen := System.Length(aString);
    if (boxLen = 0) or (strLen = 0) or (strLen > boxLen) then
      EXIT;

    first := PWIDEChar(BOX);
    curr  := first;

    for i := (boxLen - strLen) downto 0 do
    begin
      result := CompareStringW(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                               curr, strLen, PWIDEChar(aString), strLen) = CSTR_EQUAL;
      if result then
        BREAK;

      Inc(curr);
    end;

    if result then
      aPos := (curr - first) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindLast(const aChar: WIDEChar;
                                var   aPos: Integer;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    boxLen: Integer;
    first: PWIDEChar;
    curr: PWIDEChar;
  begin
    aPos    := 0;
    result  := FALSE;

    boxLen  := System.Length(BOX);
    if (boxLen = 0) then
      EXIT;

    first := @BOX[1];
    curr  := @BOX[boxLen];

    case aCaseMode of
      csCaseSensitive : for i := Pred(boxLen) downto 0 do
                        begin
                          result := (curr^ = aChar);
                          if result then
                            BREAK;

                          Dec(curr);
                        end;

      csIgnoreCase    : for i := Pred(boxLen) downto 0 do
                        begin
                          result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                                                   curr, 1, @aChar, 1) = CSTR_EQUAL;
                          if result then
                            BREAK;

                          Dec(curr);
                        end;
    end;

    if result then
      aPos := (curr - first) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.FindLast(const aString: UnicodeString;
                                var   aPos: Integer;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    boxLen: Integer;
    strLen: Integer;
    first: PWIDEChar;
    curr: PWIDEChar;
  begin
    aPos    := 0;
    result  := FALSE;

    boxLen  := System.Length(BOX);
    strLen  := System.Length(aString);
    if (boxLen = 0) or (strLen = 0) or (strLen > boxLen) then
      EXIT;

    first := @BOX[1];
    curr  := @BOX[(boxLen - strLen) + 1];

    for i := (boxLen - strLen) downto 0 do
    begin
      result := CompareStringW(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                               curr, strLen, PWIDEChar(aString), strLen) = CSTR_EQUAL;
      if result then
        BREAK;

      Dec(curr);
    end;

    if result then
      aPos := (curr - first) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Find(const aChar: WIDEChar;
                            var   aPos: TCharIndexArray;
                            const aCaseMode: TCaseSensitivity): Integer;
  var
    i: Integer;
    strlen: Integer;
    firstChar: PWIDEChar;
    currChar: PWIDEChar;
  begin
    result := 0;
    SetLength(aPos, 0);

    strlen  := System.Length(BOX);

    if (strlen = 0) then
      EXIT;

    SetLength(aPos, strlen);
    firstChar := PWIDEChar(BOX);
    currChar  := firstChar;

    case aCaseMode of
      csCaseSensitive : for i := Pred(strlen) downto 0 do
                        begin
                          if (currChar^ = aChar) then
                          begin
                            aPos[result] := (currChar - firstChar) + 1;
                            Inc(result);
                          end;

                          Inc(currChar);
                        end;

      csIgnoreCase    : for i := Pred(strlen) downto 0 do
                        begin
                          if CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
                                            currChar, 1, @aChar, 1) = CSTR_EQUAL then
                          begin
                            aPos[result] := (currChar - firstChar) + 1;
                            Inc(result);
                          end;

                          Inc(currChar);
                        end;
    end;

    SetLength(aPos, result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Find(const aString: UnicodeString;
                            var   aPos: TCharIndexArray;
                            const aCaseMode: TCaseSensitivity): Integer;
  var
    i: Integer;
    currChar: PWIDEChar;
    firstChar: PWIDEChar;
    pstr: PWIDEChar;
    boxLen: Integer;
    strLen: Integer;
  begin
    result  := 0;
    SetLength(aPos, 0);

    strLen  := System.Length(aString);
    boxLen  := System.Length(BOX);
    if (boxLen = 0) or (strLen = 0) or (strLen > boxLen)then
      EXIT;

    SetLength(aPos, boxLen);
    pstr      := PWIDEChar(aString);
    firstChar := PWIDEChar(BOX);
    currChar  := firstChar;

    for i := (boxLen - strLen) downto 0 do
    begin
      if (Windows.CompareStringW(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                                 currChar, strLen, pstr, strLen) = CSTR_EQUAL) then
      begin
        aPos[result] := (currChar - firstChar) + 1;
        Inc(result)
      end;

      Inc(currChar);
    end;

    SetLength(aPos, result);
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.IndexIn(const aArray: array of UnicodeString;
                               const aCaseMode: TCaseSensitivity): Integer;
  var
    i: Integer;
  begin
    case aCaseMode of
      csCaseSensitive : for i := 0 to Pred(System.Length(aArray)) do
                          if (BOX = aArray[i]) then
                          begin
                            result := i;
                            EXIT;
                          end;

      csIgnoreCase    : for i := 0 to Pred(System.Length(aArray)) do
                          if EqualsText(aArray[i]) then
                          begin
                            result := i;
                            EXIT;
                          end;
    end;

    result := -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.IsOneOf(const aArray: array of UnicodeString;
                               const aCaseMode: TCaseSensitivity): Boolean;
  begin
    result := IndexIn(aArray, aCaseMode) <> -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Leading(const aCount: Integer): UnicodeString;
  begin
    result := Copy(BOX, 1, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Remove(const aScope: TStringScope;
                              const aString: UnicodeString;
                              const aCaseMode: TCaseSensitivity): UnicodeString;
  begin
    BOX     := WIDE.Replace(aScope, BOX, aString, '', aCaseMode);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Replace(const aScope: TStringScope;
                               const aFindStr: UnicodeString;
                               const aReplaceStr: UnicodeString;
                               const aCaseMode: TCaseSensitivity): UnicodeString;
  begin
    BOX     := WIDE.Replace(aScope, BOX, aFindStr, aReplaceStr, aCaseMode);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Trailing(const aCount: Integer): UnicodeString;
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

    result  := Find(aChar, p);
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
    result := Find(aChar, p) > 0;
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
  function TWIDEString.Embrace(const aBraceChar: WIDEChar): UnicodeString;
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
                               const aChar: WIDEChar): UnicodeString;
  begin
    BOX     := WIDE.PadLeft(BOX, aCount, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.PadRight(const aCount: Integer;
                                const aChar: WIDEChar): UnicodeString;
  begin
    BOX     := WIDE.PadRight(BOX, aCount, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.PadToLengthLeft(const aMaxLen: Integer;
                                       const aChar: WIDEChar): UnicodeString;
  begin
    BOX     := WIDE.PadLeft(BOX, aMaxLen, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.PadToLengthRight(const aMaxLen: Integer;
                                        const aChar: WIDEChar): UnicodeString;
  begin
    BOX     := WIDE.PadRight(BOX, aMaxLen, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.Trim(const aChar: WIDEChar): UnicodeString;
  begin
    BOX       := WIDE.Trim(BOX, aChar);
    result    := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.RemoveLeading(const aChar: WIDEChar): UnicodeString;
  begin
    BOX     := WIDE.RemoveLeading(BOX, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.RemoveLeading(const aCount: Integer): UnicodeString;
  begin
    System.Delete(BOX, 1, aCount);
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.RemoveTrailing(const aChar: WIDEChar): UnicodeString;
  begin
    BOX     := WIDE.RemoveTrailing(BOX, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TWIDEString.RemoveTrailing(const aCount: Integer): UnicodeString;
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
