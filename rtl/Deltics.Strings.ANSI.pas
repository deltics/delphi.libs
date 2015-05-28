

  unit Deltics.Strings.ANSI;


interface

  uses
    SysUtils,
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
      function get_Length: Integer;
      procedure set_Length(const aValue: Integer);

      function ByteCount: Integer;
      function IsLowercase: Boolean;
      function IsUppercase: Boolean;
      function ToANSI: ANSIString;
      function ToString: String; reintroduce;
      function ToUTF8: UTF8String;
      function ToWIDE: UnicodeString;

    private // IANSIString
      function get_Value: ANSIString;
      procedure set_Value(const aValue: ANSIString);

      function AsPointer: PANSIChar;

      function AllocANSI: PANSIChar;
      function AllocUTF8: PUTF8Char;
      function AllocWIDE: PWIDEChar;
      procedure CopyToBuffer(const aDest: PANSIChar; const aMaxBytes: Integer = -1); overload;
      procedure CopyToBuffer(const aDest: PUTF8Char; const aMaxBytes: Integer = -1); overload;
      procedure CopyToBuffer(const aDest: PWIDEChar; const aMaxChars: Integer = -1); overload;

      function BeginsWith(const aString: ANSIString; const aCaseMode: TCaseSensitivity): Boolean;
      function CompareWith(const aString: ANSIString; const aCaseMode: TCaseSensitivity): TCompareResult;
      function Contains(const aChar: ANSIChar; const aCaseMode: TCaseSensitivity): Boolean; overload;
      function Contains(const aString: ANSIString; const aCaseMode: TCaseSensitivity): Boolean; overload;
      function Contains(const aNeed: TContainNeeds; const aChars: array of ANSIChar; const aCaseMode: TCaseSensitivity): Boolean; overload;
      function Contains(const aNeed: TContainNeeds; const aStrings: array of ANSIString; const aCaseMode: TCaseSensitivity): Boolean; overload;
      function EndsWith(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean;
      function EqualsText(const aString: ANSIString): Boolean;

      function Find(const aChar: ANSIChar; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Find(const aString: ANSIString; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function Find(const aChar: ANSIChar; var aPos: TCharIndexArray; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer; overload;
      function Find(const aString: ANSIString; var aPos: TCharIndexArray; const aCaseMode: TCaseSensitivity = csCaseSensitive): Integer; overload;
//      function FindFirst(const aChar: ANSIChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
//      function FindFirst(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
      function FindLast(const aChar: ANSIChar; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function FindLast(const aString: ANSIString; var aPos: Integer; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
//      function FindLast(const aChar: ANSIChar; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;
//      function FindLast(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): IStringSearch; overload;

      function IndexIn(const aArray: array of ANSIString; const aCaseMode: TCaseSensitivity): Integer;
      function IsOneOf(const aArray: array of ANSIString; const aCaseMode: TCaseSensitivity): Boolean;
      function Leading(const aCount: Integer): ANSIString;
      function Substr(const aStart: Integer; const aCount: Integer): ANSIString;
      function Trailing(const aCount: Integer): ANSIString;

      function Split(const aChar: ANSIChar; var aLeft, aRight: ANSIString): Boolean; overload;
      function Split(const aChar: ANSIChar; var aParts: TANSIStringArray): Boolean; overload;

      function Lowercase: ANSIString;
      function Uppercase: ANSIString;

      function Embrace(const aBraceChar: ANSIChar): ANSIString;
      function Enquote(const aQuoteChar: ANSIChar): ANSIString;
      function ExtendLeft(const aMaxLen: Integer; const aChar: ANSIChar): ANSIString; overload;
      function ExtendRight(const aMaxLen: Integer; const aChar: ANSIChar): ANSIString; overload;
      function PadLeft(const aCount: Integer; const aChar: ANSIChar): ANSIString; overload;
      function PadRight(const aCount: Integer; const aChar: ANSIChar): ANSIString; overload;

      function Delete(const aStart, aCount: Integer): ANSIString;
      function Extract(const aStart, aCount: Integer): ANSIString;
      function ExtractLeading(const aCount: Integer): ANSIString;
      function ExtractTrailing(const aCount: Integer): ANSIString;
      function Remove(const aScope: TStringScope; const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): ANSIString;
      function Replace(const aScope: TStringScope; const aFindStr, aReplaceStr: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): ANSIString;
      function RemoveLeading(const aCount: Integer): ANSIString; overload;
      function RemoveLeading(const aChar: ANSIChar): ANSIString; overload;
      function RemoveTrailing(const aCount: Integer): ANSIString; overload;
      function RemoveTrailing(const aChar: ANSIChar): ANSIString; overload;
      function Trim(const aCount: Integer): ANSIString; overload;
      function Trim(const aChar: ANSIChar): ANSIString; overload;
      function Unbrace: ANSIString;
      function Unquote: ANSIString;
  end;


(*
    TANSIStringSearch = class(TCOMInterfacedObject, IStringSearch)
    private
      fInitialPos: Integer;
      fPos: Integer;
      fString: IANSIString;
      fTarget: ANSIString;
    public
      constructor CreateFromStart(const aString: IANSIString; const aTarget: ANSIString);
      constructor CreateFromEnd(const aString: IANSIString; const aTarget: ANSIString);

    private // IStringSearch -------------------------------------
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
*)



implementation

  uses
  { vcl: }
  {$ifdef DELPHIXE4_OR_LATER}
    ANSIStrings,
  {$endif}
    Windows,
  { deltics: }
    Deltics.Strings.FXUtils;


  type TStepCharProcA = procedure(var aChar: PANSIChar);

  procedure StepForward(var aChar: PANSIChar);  begin Inc(aChar); end;
  procedure StepBack(var aChar: PANSIChar);     begin Dec(aChar); end;


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
  function TANSIString.AsPointer: PANSIChar;
  begin
    result := PANSIChar(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.AllocANSI: PANSIChar;
  begin
    result := ANSI.AllocANSI(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.AllocUTF8: PUTF8Char;
  begin
    result := ANSI.AllocUTF8(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.AllocWIDE: PWIDEChar;
  begin
    result := ANSI.AllocWIDE(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.BeginsWith(const aString: ANSIString;
                                  const aCaseMode: TCaseSensitivity): Boolean;
  var
    alen: Integer;
  begin
    alen := System.Length(aString);

    result := (alen <= System.Length(BOX))
          and (CompareStringA(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                              PANSIChar(BOX), alen,
                              PANSIChar(aString), alen) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ByteCount: Integer;
  begin
    result := System.Length(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.EndsWith(const aString: ANSIString;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    alen: Integer;
  begin
    alen := System.Length(aString);

    result := (alen <= System.Length(BOX))
          and (CompareStringA(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                              PANSIChar(Integer(@BOX) - alen), alen,
                              PANSIChar(aString), alen) = CSTR_EQUAL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Leading(const aCount: Integer): ANSIString;
  begin
    result := System.Copy(BOX, 1, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Substr(const aStart: Integer;
                              const aCount: Integer): ANSIString;
  begin
    result := System.Copy(BOX, aStart, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.get_Length: Integer;
  begin
    result := Length(BOX);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.get_Value: ANSIString;
  begin
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIString.set_Value(const aValue: ANSIString);
  begin
    BOX := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIString.set_Length(const aValue: Integer);
  begin
    SetLength(BOX, aValue);
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
  function TANSIString.CompareWith(const aString: ANSIString;
                                   const aCaseMode: TCaseSensitivity): TCompareResult;
  begin
    result := CompareStringA(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                             PANSIChar(BOX), System.Length(BOX),
                             PANSIChar(aString), System.Length(aString)) - CSTR_EQUAL;

    result := IntToCompareResult(result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Contains(const aChar: ANSIChar;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    notUsed: Integer;
  begin
    result := Find(aChar, notUsed, aCaseMode);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Contains(const aString: ANSIString;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    notUsed: Integer;
  begin
    result := Find(aString, notUsed, aCaseMode);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Contains(const aNeed: TContainNeeds;
                                const aChars: array of ANSIChar;
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
  function TANSIString.Contains(const aNeed: TContainNeeds;
                                const aStrings: array of ANSIString;
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
  procedure TANSIString.CopyToBuffer(const aDest: PANSIChar;
                                     const aMaxBytes: Integer);
  begin
    ANSI.CopyToBuffer(BOX, aDest, aMaxBytes);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIString.CopyToBuffer(const aDest: PUTF8Char;
                                     const aMaxBytes: Integer);
  begin
    ANSI.CopyToBuffer(BOX, aDest, aMaxBytes);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIString.CopyToBuffer(const aDest: PWIDEChar;
                                     const aMaxChars: Integer);
  begin
    ANSI.CopyToBuffer(BOX, aDest, aMaxChars);
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
                            var   aPos: Integer;
                            const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    boxLen: Integer;
    first: PANSIChar;
    curr: PANSIChar;
  begin
    aPos    := 0;
    result  := FALSE;

    boxLen  := System.Length(BOX);
    if (boxLen = 0) then
      EXIT;

    first := PANSIChar(BOX);
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
                          result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
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
  function TANSIString.Find(const aString: ANSIString;
                            var   aPos: Integer;
                            const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    boxLen: Integer;
    strLen: Integer;
    first: PANSIChar;
    curr: PANSIChar;
  begin
    aPos   := 0;
    result := FALSE;

    boxLen := System.Length(BOX);
    strLen := System.Length(aString);
    if (boxLen = 0) or (strLen = 0) or (strLen > boxLen) then
      EXIT;

    first := PANSIChar(BOX);
    curr  := first;

    for i := (boxLen - strLen) downto 0 do
    begin
      result := CompareStringA(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                               curr, strLen, PANSIChar(aString), strLen) = CSTR_EQUAL;
      if result then
        BREAK;

      Inc(curr);
    end;

    if result then
      aPos := (curr - first) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Find(const aChar: ANSIChar;
                            var   aPos: TCharIndexArray;
                            const aCaseMode: TCaseSensitivity): Integer;
  var
    i: Integer;
    strlen: Integer;
    firstChar: PANSIChar;
    currChar: PANSIChar;
  begin
    result := 0;
    SetLength(aPos, 0);

    strlen  := System.Length(BOX);

    if (strlen = 0) then
      EXIT;

    SetLength(aPos, strlen);
    firstChar := PANSIChar(BOX);
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
                          if CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
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
  function TANSIString.Find(const aString: ANSIString;
                            var   aPos: TCharIndexArray;
                            const aCaseMode: TCaseSensitivity): Integer;
  const
    NORMALISE: array[FALSE..TRUE] of Cardinal = (0, NORM_IGNORECASE);
  var
    i: Integer;
    currChar: PANSIChar;
    firstChar: PANSIChar;
    pstr: PANSIChar;
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
    pstr      := PANSIChar(aString);
    firstChar := PANSIChar(BOX);
    currChar  := firstChar;

    for i := (boxLen - strLen) downto 0 do
    begin
      if (Windows.CompareStringA(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
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
  function TANSIString.FindLast(const aChar: ANSIChar;
                                var   aPos: Integer;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    boxLen: Integer;
    first: PANSIChar;
    curr: PANSIChar;
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
                          result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
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
  function TANSIString.FindLast(const aString: ANSIString;
                                var   aPos: Integer;
                                const aCaseMode: TCaseSensitivity): Boolean;
  var
    i: Integer;
    boxLen: Integer;
    strLen: Integer;
    first: PANSIChar;
    curr: PANSIChar;
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
      result := CompareStringA(LOCALE_USER_DEFAULT, FXCOMPAREFLAG_CASE[aCaseMode],
                               curr, strLen, PANSIChar(aString), strLen) = CSTR_EQUAL;
      if result then
        BREAK;

      Dec(curr);
    end;

    if result then
      aPos := (curr - first) + 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.IndexIn(const aArray: array of ANSIString;
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
  function TANSIString.IsOneOf(const aArray: array of ANSIString;
                               const aCaseMode: TCaseSensitivity): Boolean;
  begin
    result := IndexIn(aArray, aCaseMode) <> -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Split(const aChar: ANSIChar;
                             var aLeft, aRight: ANSIString): Boolean;
  var
    p: Integer;
  begin
    aLeft   := BOX;
    aRight  := '';

    result := Find(aChar, p);
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
  function TANSIString.Trailing(const aCount: Integer): ANSIString;
  begin
    result := Copy(BOX, (System.Length(BOX) - aCount) + 1, aCount);
  end;








  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Delete(const aStart, aCount: Integer): ANSIString;
  begin
    System.Delete(BOX, aStart, aCount);;
    result := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Embrace(const aBraceChar: ANSIChar): ANSIString;
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
  function TANSIString.Extract(const aStart, aCount: Integer): ANSIString;
  begin
    result := System.Copy(BOX, aStart, aCount);
    System.Delete(BOX, aStart, aCount);;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ExtractLeading(const aCount: Integer): ANSIString;
  begin
    result := System.Copy(BOX, 1, aCount);
    System.Delete(BOX, 1, aCount);;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ExtractTrailing(const aCount: Integer): ANSIString;
  var
    start: Integer;
  begin
    start   := (System.Length(BOX) - aCount) + 1;
    result  := Copy(BOX, start, aCount);

    SetLength(BOX, Pred(start));
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
                               const aChar: ANSIChar): ANSIString;
  begin
    BOX     := ANSI.PadLeft(BOX, aCount, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.PadRight(const aCount: Integer;
                                const aChar: ANSIChar): ANSIString;
  begin
    BOX     := ANSI.PadRight(BOX, aCount, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ExtendLeft(const aMaxLen: Integer;
                                  const aChar: ANSIChar): ANSIString;
  begin
    BOX     := ANSI.ExtendLeft(BOX, aMaxLen, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.ExtendRight(const aMaxLen: Integer;
                                   const aChar: ANSIChar): ANSIString;
  begin
    BOX     := ANSI.ExtendRight(BOX, aMaxLen, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Remove(const aScope: TStringScope;
                              const aString: ANSIString;
                              const aCaseMode: TCaseSensitivity): ANSIString;
  begin
    BOX     := ANSI.Replace(aScope, BOX, aString, '', aCaseMode);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Replace(const aScope: TStringScope;
                               const aFindStr: ANSIString;
                               const aReplaceStr: ANSIString;
                               const aCaseMode: TCaseSensitivity): ANSIString;
  begin
    BOX     := ANSI.Replace(aScope, BOX, aFindStr, aReplaceStr, aCaseMode);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.RemoveLeading(const aChar: ANSIChar): ANSIString;
  begin
    BOX     := ANSI.RemoveLeading(BOX, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Trim(const aCount: Integer): ANSIString;
  begin
    BOX     := ANSI.Trim(BOX, aCount);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.Trim(const aChar: ANSIChar): ANSIString;
  begin
    BOX       := ANSI.Trim(BOX, aChar);
    result    := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.RemoveLeading(const aCount: Integer): ANSIString;
  begin
    BOX     := ANSI.RemoveLeading(BOX, aCount);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.RemoveTrailing(const aChar: ANSIChar): ANSIString;
  begin
    BOX     := ANSI.RemoveTrailing(BOX, aChar);
    result  := BOX;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TANSIString.RemoveTrailing(const aCount: Integer): ANSIString;
  begin
    BOX     := ANSI.RemoveTrailing(BOX, aCount);
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
