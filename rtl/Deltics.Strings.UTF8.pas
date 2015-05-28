

  unit Deltics.Strings.UTF8;


interface

  uses
    Deltics.Classes,
    Deltics.Strings;


  type
    TUTF8String = class(TCOMInterfacedObject, IUTF8String)
    private
      BOX: UTF8String;
    public
      constructor Create(const aString: UTF8String);

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
      function BeginsWith(const aString: ANSIString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function BeginsWith(const aString: UTF8String; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function BeginsWith(const aString: WIDEString; const aCaseMode: TCaseSensitivity = csCaseSensitive): Boolean; overload;
      function CompareWith(const aString: UTF8String; const aCaseMode: TCaseSensitivity): Integer;
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



implementation

  uses
    SysUtils,
    Windows;


  constructor TUTF8String.Create(const aString: UTF8String);
  begin
    inherited Create;
    BOX := aString;
  end;


  function TUTF8String.get_Length: Integer;
  begin
    result := Length(BOX);
  end;


  procedure TUTF8String.set_Length(const aValue: Integer);
  begin
    SetLength(BOX, aValue);
  end;


  function TUTF8String.Data: PUTF8Char;
  begin
    result := PUTF8Char(BOX);
  end;


  function TUTF8String.ToANSI: ANSIString;
  begin
    result := ANSI.FromWIDE(ToWIDE);
  end;


  function TUTF8String.ToString: String;
  begin
  {$ifdef UNICODE}
    result := WIDE.FromUTF8(BOX);
  {$else}
    result := ANSI.FromUTF8(BOX);
  {$endif}
  end;


  function TUTF8String.ToUTF8: UTF8String;
  begin
    result := BOX;
  end;


  function TUTF8String.ToWIDE: UnicodeString;
  begin
    result := WIDE.FromUTF8(BOX);
  end;


  function TUTF8String.BeginsWith(const aString: UTF8String;
                                  const aCaseMode: TCaseSensitivity): Boolean;
  begin
    result := WIDE(WIDE.FromUTF8(BOX)).BeginsWith(WIDE.FromUTF8(aString), aCaseMode);
  end;


  function TUTF8String.BeginsWith(const aString: ANSIString;
                                  const aCaseMode: TCaseSensitivity): Boolean;
  begin
    result := WIDE(WIDE.FromUTF8(BOX)).BeginsWith(WIDE.FromANSI(aString), aCaseMode);
  end;


  function TUTF8String.BeginsWith(const aString: WIDEString;
                                  const aCaseMode: TCaseSensitivity): Boolean;
  begin
    result := WIDE(WIDE.FromUTF8(BOX)).BeginsWith(aString, aCaseMode);
  end;


  function TUTF8String.ByteCount: Integer;
  begin
    result := System.Length(BOX);
  end;


  function TUTF8String.Lowercase: UTF8String;
  begin
    BOX     := UTF8.FromWIDE(WIDE(ToWIDE).Lowercase);
    result  := BOX;
  end;


  function TUTF8String.CompareWith(const aString: UTF8String;
                                   const aCaseMode: TCaseSensitivity): Integer;
  begin
    result := WIDE.Compare(ToWIDE, WIDE.FromUTF8(aString), aCaseMode);
  end;


  function TUTF8String.Contains(const aString: UTF8String;
                                const aCaseMode: TCaseSensitivity): Boolean;
  begin
    result := WIDE(ToWIDE).Contains(WIDE.FromUTF8(aString));
  end;


  function TUTF8String.Contains(const aChar: WIDEChar;
                                const aCaseMode: TCaseSensitivity): Boolean;
  begin
    result := WIDE(ToWIDE).Contains(aChar);
  end;


  function TUTF8String.Contains(const aChar: ANSIChar;
                                const aCaseMode: TCaseSensitivity): Boolean;
  begin
    result := WIDE(ToWIDE).Contains(ANSI(aChar).ToWIDE);
  end;


  function TUTF8String.EqualsText(const aString: UTF8String): Boolean;
  begin
    result := WIDE(ToWIDE).EqualsText(UTF8(aString).ToWIDE);
  end;


  function TUTF8String.Find(const aChar: WIDEChar;
                            var aPos: TCharIndexArray): Boolean;
  var
    str: UnicodeString;
  begin
    str     := aChar;
    result  := Find(WIDE(str).ToUTF8, aPos);
  end;


  function TUTF8String.Find(const aChar: ANSIChar;
                            var aPos: TCharIndexArray): Boolean;
  var
    str: ANSIString;
  begin
    str     := aChar;
    result  := Find(ANSI(str).ToUTF8, aPos);
  end;


  function TUTF8String.Find(const aString: UTF8String;
                            var aPos: TCharIndexArray): Boolean;
  var
    i, j: Integer;
    strLen: Integer;
    subLen: Integer;
    firstChar: PUTF8Char;
    currChar: PUTF8Char;
    initialChar: PUTF8Char;
  begin
    result  := FALSE;
    subLen  := System.Length(aString);
    strLen  := System.Length(BOX);

    if (strLen = 0) then
      EXIT;

    SetLength(aPos, strLen);

    j           := 0;
    firstChar   := Addr(BOX[1]);
    currChar    := firstChar;
    initialChar := Addr(aString[1]);

    for i := Pred(strLen) downto 0 do
    begin
      if (currChar^ = initialChar^) and CompareMem(currChar, initialChar, subLen) then
      begin
        // Need to use PANSIChar for pointer arithmetic
        aPos[j] := (PANSIChar(currChar) - PANSIChar(firstChar)) + 1;
        Inc(j);
      end;
      Inc(currChar);
    end;

    SetLength(aPos, j);
    result := (j > 0);
  end;



  function TUTF8String.FindFirst(const aChar: ANSIChar;
                                 var aPos: Integer): Boolean;
  var
    str: ANSIString;
  begin
    str     := aChar;
    result  := FindFirst(ANSI(str).ToUTF8, aPos);
  end;


  function TUTF8String.FindFirst(const aChar: WIDEChar;
                                 var aPos: Integer): Boolean;
  var
    str: UnicodeString;
  begin
    str     := aChar;
    result  := FindFirst(WIDE(str).ToUTF8, aPos);
  end;


  function TUTF8String.FindFirst(const aString: UTF8String;
                                 var aPos: Integer): Boolean;
  begin
    aPos    := System.Pos(aString, BOX);
    result  := aPos <> 0;
  end;


  function TUTF8String.FindLast(const aChar: WIDEChar;
                                var aPos: Integer): Boolean;
  var
    str: UnicodeString;
  begin
    str     := aChar;
    result  := FindLast(WIDE(str).ToUTF8, aPos);
  end;


  function TUTF8String.FindLast(const aChar: ANSIChar;
                                var aPos: Integer): Boolean;
  var
    str: ANSIString;
  begin
    str     := aChar;
    result  := FindLast(ANSI(str).ToUTF8, aPos);
  end;


  function TUTF8String.FindLast(const aString: UTF8String;
                                var aPos: Integer): Boolean;
  var
    i: Integer;
    strlen: Integer;
    sublen: Integer;
    firstChar: PUTF8Char;
    currChar: PUTF8Char;
    initialChar: PUTF8Char;
  begin
    strlen  := System.Length(BOX);
    sublen  := System.Length(aString);
    aPos    := 0;
    result  := FALSE;

    if (sublen > strlen) or (sublen = 0) or (strlen = 0) then
      EXIT;

    firstChar   := Addr(BOX[1]);
    initialChar := Addr(aString[1]);
    currChar    := Addr(BOX[strlen]);

    for i := (strlen - sublen) downto 0 do
    begin
      result := (currChar^ = initialChar^);
      if result and CompareMem(currChar, initialChar, sublen) then
        BREAK;

      Dec(currChar);
    end;

    if result then
      aPos := (PANSIChar(currChar) - PANSIChar(firstChar)) + 1;
  end;


  function TUTF8String.FindNext(const aChar: WIDEChar;
                                var aPos: Integer): Boolean;
  var
    str: UnicodeString;
  begin
    str     := aChar;
    result  := FindNext(WIDE(str).ToUTF8, aPos);
  end;


  function TUTF8String.FindNext(const aChar: ANSIChar;
                                var aPos: Integer): Boolean;
  var
    str: ANSIString;
  begin
    str     := aChar;
    result  := FindNext(ANSI(str).ToUTF8, aPos);
  end;


  function TUTF8String.FindNext(const aString: UTF8String;
                                var aPos: Integer): Boolean;
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
    strLen  := System.Length(BOX);
    subLen  := System.Length(aString);

    if ((aPos + subLen) > strLen) or (strLen = 0) or (subLen = 0) then
    begin
      aPos := 0;
      EXIT;
    end;

    firstChar   := Addr(BOX[1]);
    initialChar := Addr(aString[1]);
    currChar    := Addr(BOX[aPos + 1]);

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


  function TUTF8String.IsLowercase: Boolean;
  begin
    result := WIDE(ToWIDE).IsLowercase;
  end;


  function TUTF8String.IsUppercase: Boolean;
  begin
    result := WIDE(ToWIDE).IsUppercase;
  end;


  function TUTF8String.Uppercase: UTF8String;
  begin
    BOX     := UTF8.FromWIDE(WIDE(ToWIDE).Uppercase);
    result  := BOX;
  end;






end.
