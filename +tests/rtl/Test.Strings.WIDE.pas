
  unit Test.Strings.WIDE;

interface

  uses
    Deltics.Smoketest;


  type
    TWIDEFnTests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Alloc;
      procedure fn_Len;

      procedure fn_Compare;
      procedure fn_SameString;
      procedure fn_SameText;
      procedure fn_IsLowercase;
      procedure fn_IsUppercase;
      procedure fn_Find;
      procedure fn_FindNext;

      procedure fn_Concat;
      procedure fn_StringOf;

      procedure fn_Lowercase;
      procedure fn_Uppercase;

      procedure fn_Embrace;
      procedure fn_Enquote;
      procedure fn_ExtendLeft;
      procedure fn_ExtendRight;
      procedure fn_PadLeft;
      procedure fn_PadRight;

      procedure fn_RemoveLeading;
      procedure fn_RemoveTrailing;
      procedure fn_Trim;
      procedure fn_Unbrace;
      procedure fn_Unquote;
    end;


    TWIDETests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Len;
      procedure fn_Find;
      procedure fn_NPos;
      procedure fn_RPos;
      procedure fn_Split;
      procedure fn_Compare;
      procedure fn_Contains;
      procedure fn_ContainsText;
      procedure fn_IsLowercase;
      procedure fn_IsUppercase;
      procedure fn_Replace;
      procedure fn_SameText;
      procedure fn_Lowercase;
      procedure fn_Uppercase;
      procedure fn_Embrace;
      procedure fn_Enquote;
      procedure fn_Unbrace;
      procedure fn_Unquote;
    end;



implementation

  uses
    Math,
    SysUtils,
    Deltics.Strings,
    Test.Strings;




{ TWIDEFnTests }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.Transcoding;
  begin
    Test('WIDE.Encode!').Expect(WIDE.Encode(SRCS)).Equals(SRCW);

    Test('WIDE.FromANSI(string)!').Expect(WIDE.FromANSI(SRCA)).Equals(SRCW);
    Test('WIDE.FromANSI(buffer)!').Expect(WIDE.FromANSI(PANSIChar(SRCA))).Equals(SRCW);

    Test('WIDE.FromUTF8(string)!').Expect(WIDE.FromUTF8(SRCU)).Equals(SRCW);
    Test('WIDE.FromUTF8(buffer)!').Expect(WIDE.FromUTF8(PUTF8Char(SRCU))).Equals(SRCW);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Alloc;
  var
    p: Pointer;
    pANSI: PANSIChar absolute p;
    pWIDE: PWIDEChar absolute p;
    pUTF8: PUTF8Char absolute p;
  begin
    pANSI := WIDE.AllocANSI(SRCW);

    Test('WIDE.AllocANSI result').Expect(p).IsAssigned;
    Test('WIDE.AllocANSI buffer length').Expect(ANSI.Len(pANSI)).Equals(Length(SRCA));
    Test('WIDE.AllocANSI buffer content').Expect(p).Equals(PANSIString(SRCA), Length(SRCA));

    FreeMem(pANSI);

    pWIDE := WIDE.AllocWIDE(SRCW);

    Test('WIDE.AllocWIDE result').Expect(p).IsAssigned;
    Test('WIDE.AllocWIDE buffer length').Expect(WIDE.Len(pWIDE)).Equals(Length(SRCW));
    Test('WIDE.AllocWIDE buffer content').Expect(p).Equals(PWIDEString(SRCW), Length(SRCW) * 2);

    FreeMem(pWIDE);

    pUTF8 := WIDE.AllocUTF8(SRCW);

    Test('WIDE.AllocUTF8 result').Expect(p).IsAssigned;
    Test('WIDE.AllocUTF8 buffer length').Expect(UTF8.Len(pUTF8)).Equals(Length(SRCU));
    Test('WIDE.AllocUTF8 buffer content').Expect(p).Equals(PUTF8String(SRCU), Length(SRCU));

    FreeMem(pUTF8);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Len;
  const
    STR: WIDEString = 'test';
    BUFFER: array[0..7] of WIDEChar = ('b','u','f','f','e','r',#0,#0);
    NULLBUFFER: array[0..7] of WIDEChar = (#0,#0,#0,#0,#0,#0,#0,#0);
  begin
    Test('WIDE.Len(string)').Expect(WIDE.Len(PWIDEChar(STR))).Equals(4);
    Test('WIDE.Len(buffer)').Expect(WIDE.Len(@BUFFER[0])).Equals(6);
    Test('WIDE.Len(null buffer)').Expect(WIDE.Len(@NULLBUFFER[0])).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Compare;
  begin
    Test('WIDE.Compare(a, A)').Expect(WIDE.Compare('a', 'A')).LessThan(0);
    Test('WIDE.Compare(A, a)').Expect(WIDE.Compare('A', 'a')).GreaterThan(0);
    Test('WIDE.Compare(a, a)').Expect(WIDE.Compare('a', 'a')).Equals(0);

    Test('WIDE.Compare(c, A)').Expect(WIDE.Compare('c', 'A')).Equals(1);
    Test('WIDE.Compare(c, a)').Expect(WIDE.Compare('c', 'a')).Equals(1);
    Test('WIDE.Compare(a, c)').Expect(WIDE.Compare('a', 'c')).Equals(-1);
    Test('WIDE.Compare(A, c)').Expect(WIDE.Compare('A', 'c')).Equals(-1);

    Test('WIDE.Compare(a, A, csIgnoreCase)').Expect(WIDE.Compare('a', 'A', csIgnoreCase)).Equals(0);
    Test('WIDE.Compare(A, a, csIgnoreCase)').Expect(WIDE.Compare('A', 'a', csIgnoreCase)).Equals(0);
    Test('WIDE.Compare(a, a, csIgnoreCase)').Expect(WIDE.Compare('a', 'a', csIgnoreCase)).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_SameString;
  const
    TRUE_CASES: array[0..3] of TWIDEStringAB = (
                                                (A: '';                    B: ''),
                                                (A: 'Mixed Case';           B: 'Mixed Case'),
                                                (A: '*not uppercase*';     B: '*not uppercase*'),
                                                (A: 'Microsoft Windows™';  B: 'Microsoft Windows™')
                                               );
    FALSE_CASES: array[0..3] of TWIDEStringAB = (
                                                 (A: '';                    B: ' '),
                                                 (A: 'LowerCase';           B: 'lowercase'),
                                                 (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                                 (A: 'Microsoft Windows™';  B: 'microsoft Windows™')
                                                );
  var
    i: Integer;
  begin
    for i := 0 to High(TRUE_CASES) do
      with TRUE_CASES[i] do
        Test('WIDE.SameString(%s, %s)', [A, B]).Expect(WIDE.SameString(A, B)).IsTRUE;

    for i := 0 to High(FALSE_CASES) do
      with FALSE_CASES[i] do
        Test('WIDE.SameString(%s, %s)', [A, B]).Expect(WIDE.SameString(A, B)).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_SameText;
  const
    TRUE_CASES: array[0..3] of TWIDEStringAB = (
                                                (A: '';                    B: ''),
                                                (A: 'LowerCase';           B: 'lowercase'),
                                                (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                                (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                               );
    FALSE_CASES: array[0..3] of TWIDEStringAB = (
                                                 (A: '';                    B: ' '),
                                                 (A: 'LowerCase';           B: 'lower-case'),
                                                 (A: '*NOT UPPERCASE*';     B: 'not uppercase'),
                                                 (A: 'Microsoft Windows™';  B: 'microsoft word™')
                                                );
  var
    i: Integer;
  begin
    for i := 0 to High(TRUE_CASES) do
      with TRUE_CASES[i] do
        Test('WIDE.SameText(%s, %s)', [A, B]).Expect(WIDE.SameText(A, B)).IsTRUE;

    for i := 0 to High(FALSE_CASES) do
      with FALSE_CASES[i] do
        Test('WIDE.SameText(%s, %s)', [A, B]).Expect(WIDE.SameText(A, B)).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_IsLowercase;
  begin
    Test('WIDE.IsLowercase(a)').Expect(WIDE.IsLowercase('a')).IsTRUE;
    Test('WIDE.IsLowercase(A)').Expect(WIDE.IsLowercase('A')).IsFALSE;
    Test('WIDE.IsLowercase(1)').Expect(WIDE.IsLowercase('1')).IsFALSE;
    Test('WIDE.IsLowercase(?)').Expect(WIDE.IsLowercase('?')).IsFALSE;
    Test('WIDE.IsLowercase(™)').Expect(WIDE.IsLowercase('™')).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_IsUppercase;
  begin
    Test('WIDE.IsUppercase(a)').Expect(WIDE.IsUppercase('a')).IsFALSE;
    Test('WIDE.IsUppercase(A)').Expect(WIDE.IsUppercase('A')).IsTRUE;
    Test('WIDE.IsUppercase(1)').Expect(WIDE.IsUppercase('1')).IsFALSE;
    Test('WIDE.IsUppercase(?)').Expect(WIDE.IsUppercase('?')).IsFALSE;
    Test('WIDE.IsUppercase(™)').Expect(WIDE.IsUppercase('™')).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Find;
  const
    STR : UnicodeString = 'abcdefghijklmn';
    f   : WIDEChar      = 'f';
    G   : WIDEChar      = 'G';
    z   : WIDEChar      = 'z';
    def : UnicodeString = 'def';
    GHI : UnicodeString = 'GHI';
    xyz : UnicodeString = 'xyz';
  var
    p: Integer;
  begin
  // Find Char tests

    Test('WIDE.Find(%s, %s, POS)!', [STR, f]).Expect(WIDE.Find(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    Test('WIDE.Find(%s, %s, POS)!', [STR, G]).Expect(WIDE.Find(STR, G, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    Test('WIDE.Find(%s, %s, POS)!', [STR, z]).Expect(WIDE.Find(STR, z, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

  // Find String tests

    Test('WIDE.Find(%s, %s, POS)!', [STR, def]).Expect(WIDE.Find(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    Test('WIDE.Find(%s, %s, POS)!', [STR, GHI]).Expect(WIDE.Find(STR, GHI, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    Test('WIDE.Find(%s, %s, POS)!', [STR, xyz]).Expect(WIDE.Find(STR, xyz, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_FindNext;
  const              // 0         1         2
    STR : UnicodeString  = 'abcdefghijklmnabcdef';
    f   : WIDEChar    = 'f';
    G   : WIDEChar    = 'G';
    z   : WIDEChar    = 'z';
    def : UnicodeString  = 'def';
    GHI : UnicodeString  = 'GHI';
    xyz : UnicodeString  = 'xyz';
  var
    p: Integer;
  begin
  // FindNext Char tests

    p := -1;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(WIDE.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    p := 0;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(WIDE.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    p := 1;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(WIDE.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    p := 6;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(WIDE.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    p := 7;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(WIDE.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(20);

    p := 20;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(WIDE.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(20);

    p := 21;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(WIDE.FindNext(STR, f, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    p := 10;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, G, p]).Expect(WIDE.FindNext(STR, G, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    p := 10;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, z, p]).Expect(WIDE.FindNext(STR, z, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);


  // FindNext String tests

    p := -1;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(WIDE.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    p := 0;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(WIDE.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    p := 1;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(WIDE.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    p := 4;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(WIDE.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    p := 5;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(WIDE.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(18);

    p := 0;
    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, GHI, p]).Expect(WIDE.FindNext(STR, GHI, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    Test('WIDE.FindNext(%s, %s, POS [=%d])!', [STR, xyz, p]).Expect(WIDE.FindNext(STR, xyz, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);
  end;







  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Concat;
  const
    COMMA       : WIDEChar      = ',';
    COMMASPACE  : UnicodeString = ', ';
  begin
    Test('WIDE.Concat(array)').Expect(WIDE.Concat(['a', 'bee', 'c'])).Equals('abeec');
    Test('WIDE.Concat(array)').Expect(WIDE.Concat(['a', '', 'c'])).Equals('ac');

    Test('WIDE.Concat(array)').Expect(WIDE.Concat(['a', 'bee', 'c'], COMMA)).Equals('a,bee,c');
    Test('WIDE.Concat(array)').Expect(WIDE.Concat(['a', '', 'c'], COMMA)).Equals('a,,c');

    Test('WIDE.Concat(array)').Expect(WIDE.Concat(['a', 'bee', 'c'], COMMASPACE)).Equals('a, bee, c');
    Test('WIDE.Concat(array)').Expect(WIDE.Concat(['a', '', 'c'], COMMASPACE)).Equals('a, , c');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_StringOf;
  begin
    Test('WIDE.StringOf(''foo'', 0)').Expect(WIDE.StringOf('*', 0)).Equals('');
    Test('WIDE.StringOf(''foo'', 3)').Expect(WIDE.StringOf('*', 3)).Equals('***');

    Test('WIDE.StringOf('''', 10)').Expect(WIDE.StringOf('', 10)).Equals('');
    Test('WIDE.StringOf(''foo'', 0)').Expect(WIDE.StringOf('foo', 0)).Equals('');
    Test('WIDE.StringOf(''foo'', 3)').Expect(WIDE.StringOf('foo', 3)).Equals('foofoofoo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Lowercase;
  begin
    Test('WIDE.Lowercase(a)').Expect(WIDE.Lowercase('a')).Equals('a');
    Test('WIDE.Lowercase(A)').Expect(WIDE.Lowercase('A')).Equals('a');
    Test('WIDE.Lowercase(1)').Expect(WIDE.Lowercase('1')).Equals('1');
    Test('WIDE.Lowercase(?)').Expect(WIDE.Lowercase('?')).Equals('?');
    Test('WIDE.Lowercase(â„¢)').Expect(WIDE.Lowercase('â„¢')).Equals('â„¢');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Uppercase;
  begin
    Test('WIDE.Uppercase(a)').Expect(WIDE.Uppercase('a')).Equals('A');
    Test('WIDE.Uppercase(A)').Expect(WIDE.Uppercase('A')).Equals('A');
    Test('WIDE.Uppercase(1)').Expect(WIDE.Uppercase('1')).Equals('1');
    Test('WIDE.Uppercase(?)').Expect(WIDE.Uppercase('?')).Equals('?');
    Test('WIDE.Uppercase(™)').Expect(WIDE.Uppercase('™')).Equals('™');
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Embrace;
  begin
    // Default braces ( )
    Test('WIDE.Embrace()').Expect(WIDE.Embrace('')).Equals('()');
    Test('WIDE.Embrace(abc)').Expect(WIDE.Embrace('abc')).Equals('(abc)');

    // Embracing an empty string
    Test('WIDE.Embrace('''', [)').Expect(WIDE.Embrace('', '[')).Equals('[]');
    Test('WIDE.Embrace('''', {)').Expect(WIDE.Embrace('', '{')).Equals('{}');
    Test('WIDE.Embrace('''', <)').Expect(WIDE.Embrace('', '<')).Equals('<>');
    Test('WIDE.Embrace('''', #)').Expect(WIDE.Embrace('', '#')).Equals('##');
    Test('WIDE.Embrace('''', !)').Expect(WIDE.Embrace('', '!')).Equals('!!');

    // Embracing a string with a braced pair
    Test('WIDE.Embrace(abc, ()').Expect(WIDE.Embrace('abc', '(')).Equals('(abc)');
    Test('WIDE.Embrace(abc, [)').Expect(WIDE.Embrace('abc', '[')).Equals('[abc]');
    Test('WIDE.Embrace(abc, {)').Expect(WIDE.Embrace('abc', '{')).Equals('{abc}');
    Test('WIDE.Embrace(abc, <)').Expect(WIDE.Embrace('abc', '<')).Equals('<abc>');

    // Embracing a string with non-brace character
    Test('WIDE.Embrace(abc, #)').Expect(WIDE.Embrace('abc', '#')).Equals('#abc#');
    Test('WIDE.Embrace(abc, !)').Expect(WIDE.Embrace('abc', '!')).Equals('!abc!');
    Test('WIDE.Embrace(abc, @)').Expect(WIDE.Embrace('abc', '@')).Equals('@abc@');
    Test('WIDE.Embrace(abc, $)').Expect(WIDE.Embrace('abc', '$')).Equals('$abc$');
    Test('WIDE.Embrace(abc, &)').Expect(WIDE.Embrace('abc', '&')).Equals('&abc&');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Enquote;
  begin
    Test('WIDE.Enquote('''')').Expect(WIDE.Enquote('')).Equals('''''');
    Test('WIDE.Enquote('''', ''"'')').Expect(WIDE.Enquote('', WIDEChar('"'))).Equals('""');

    Test('WIDE.Enquote(''Mother knows best'')').Expect(WIDE.Enquote('Mother knows best')).Equals('''Mother knows best''');
    Test('WIDE.Enquote(''Mother knows best'', ''"'')').Expect(WIDE.Enquote('Mother knows best', WIDEChar('"'))).Equals('"Mother knows best"');

    Test('WIDE.Enquote(''Some Mothers Do ''Ave ''Em'')').Expect(WIDE.Enquote('Some Mothers Do ''Ave ''Em')).Equals('''Some Mothers Do ''''Ave ''''Em''');
    Test('WIDE.Enquote(''Some Mothers Do ''Ave ''Em'', '''''')').Expect(WIDE.Enquote('Some Mothers Do ''Ave ''Em', WIDEChar(''''))).Equals('''Some Mothers Do ''''Ave ''''Em''');
    Test('WIDE.Enquote(''Some Mothers Do ''Ave ''Em'', ''"'')').Expect(WIDE.Enquote('Some Mothers Do ''Ave ''Em', WIDEChar('"'))).Equals('"Some Mothers Do ''Ave ''Em"');
    Test('WIDE.Enquote(''Some Mothers Do ''Ave ''Em'', '''''', ''\'')').Expect(WIDE.Enquote('Some Mothers Do ''Ave ''Em', WIDEChar(''''), WIDEChar('\'))).Equals('''Some Mothers Do \''Ave \''Em''');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_ExtendLeft;
  begin
    Test('WIDE.ExtendLeft(''foo'', 10)').Expect(WIDE.ExtendLeft('foo', 10)).Equals('       foo');
    Test('WIDE.ExtendLeft(''foo'', 10, '' '')').Expect(WIDE.ExtendLeft('foo', 10, WIDEChar(' '))).Equals('       foo');
    Test('WIDE.ExtendLeft(''foo'', 10, ''x'')').Expect(WIDE.ExtendLeft('foo', 10, WIDEChar('x'))).Equals('xxxxxxxfoo');

    Test('WIDE.ExtendLeft(''foo'', 3, '' '')').Expect(WIDE.ExtendLeft('foo', 3, WIDEChar(' '))).Equals('foo');
    Test('WIDE.ExtendLeft(''foo'', 2, '' '')').Expect(WIDE.ExtendLeft('foo', 2, WIDEChar(' '))).Equals('foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_ExtendRight;
  begin
    Test('WIDE.ExtendRight(''foo'', 10)').Expect(WIDE.ExtendRight('foo', 10)).Equals('foo       ');
    Test('WIDE.ExtendRight(''foo'', 10, '' '')').Expect(WIDE.ExtendRight('foo', 10, WIDEChar(' '))).Equals('foo       ');
    Test('WIDE.ExtendRight(''foo'', 10, ''x'')').Expect(WIDE.ExtendRight('foo', 10, WIDEChar('x'))).Equals('fooxxxxxxx');

    Test('WIDE.ExtendRight(''foo'', 3, '' '')').Expect(WIDE.ExtendRight('foo', 3, WIDEChar(' '))).Equals('foo');
    Test('WIDE.ExtendRight(''foo'', 2, '' '')').Expect(WIDE.ExtendRight('foo', 2, WIDEChar(' '))).Equals('foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_PadLeft;
  begin
    Test('WIDE.PadLeft(''foo'', 4, '' '')').Expect(WIDE.PadLeft('foo', 4)).Equals('    foo');
    Test('WIDE.PadLeft(''foo'', 3, '' '')').Expect(WIDE.PadLeft('foo', 3)).Equals('   foo');
    Test('WIDE.PadLeft(''foo'', 2, '' '')').Expect(WIDE.PadLeft('foo', 2)).Equals('  foo');

    Test('WIDE.PadLeft(''1'', 4, ''0'')').Expect(WIDE.PadLeft('1', 4, STR('0').ToWIDE)).Equals('00001');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_PadRight;
  begin
    Test('WIDE.PadRight(''foo'', 4, '' '')').Expect(WIDE.PadRight('foo', 4)).Equals('foo    ');
    Test('WIDE.PadRight(''foo'', 3, '' '')').Expect(WIDE.PadRight('foo', 3)).Equals('foo   ');
    Test('WIDE.PadRight(''foo'', 2, '' '')').Expect(WIDE.PadRight('foo', 2)).Equals('foo  ');
  end;




  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_RemoveLeading;
  begin
    Test('WIDE.TrimLeft(''   foo'')').Expect(WIDE.RemoveLeading('   foo')).Equals('foo');
    Test('WIDE.TrimLeft(''   foo'', ''.'')').Expect(WIDE.RemoveLeading('   foo', WIDEChar('.'))).Equals('   foo');
    Test('WIDE.TrimLeft(''   foo'', 1)').Expect(WIDE.RemoveLeading('   foo', 1)).Equals('  foo');
    Test('WIDE.TrimLeft(''   foo'', 4)').Expect(WIDE.RemoveLeading('   foo', 4)).Equals('oo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_RemoveTrailing;
  begin
    Test('WIDE.TrimRight(''foo   '')').Expect(WIDE.RemoveTrailing('foo   ')).Equals('foo');
    Test('WIDE.TrimRight(''foo   '', ''.'')').Expect(WIDE.RemoveTrailing('foo   ', WIDEChar('.'))).Equals('foo   ');
    Test('WIDE.TrimRight(''foo   '', 1)').Expect(WIDE.RemoveTrailing('foo   ', 1)).Equals('foo  ');
    Test('WIDE.TrimRight(''foo   '', 4)').Expect(WIDE.RemoveTrailing('foo   ', 4)).Equals('fo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Trim;
  begin
    Test('WIDE.Trim(''   foo   '')').Expect(WIDE.Trim('   foo   ')).Equals('foo');
    Test('WIDE.Trim(''   foo   '', ''.'')').Expect(WIDE.Trim('   foo   ', WIDEChar('.'))).Equals('   foo   ');
    Test('WIDE.Trim(''   foo   '', 1)').Expect(WIDE.Trim('   foo   ', 1)).Equals('  foo  ');
    Test('WIDE.Trim(''   foo   '', 4)').Expect(WIDE.Trim('   foo   ', 4)).Equals('o');
    Test('WIDE.Trim(''   foo   '', 5)').Expect(WIDE.Trim('   foo   ', 5)).Equals('');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Unbrace;
  begin
    Test('WIDE.Unbrace(''(abc)'')').Expect(WIDE.Unbrace('(abc)')).Equals('abc');
    Test('WIDE.Unbrace(''(abc)'')').Expect(WIDE.Unbrace('(abc)')).Equals('abc');
    Test('WIDE.Unbrace(''[abc]'')').Expect(WIDE.Unbrace('[abc]')).Equals('abc');
    Test('WIDE.Unbrace(''{abc}'')').Expect(WIDE.Unbrace('{abc}')).Equals('abc');
    Test('WIDE.Unbrace(''<abc>'')').Expect(WIDE.Unbrace('<abc>')).Equals('abc');
    Test('WIDE.Unbrace(''#abc#'')').Expect(WIDE.Unbrace('#abc#')).Equals('abc');
    Test('WIDE.Unbrace(''?abc?'')').Expect(WIDE.Unbrace('?abc?')).Equals('abc');
    Test('WIDE.Unbrace(''abc'')').Expect(WIDE.Unbrace('abc')).Equals('abc');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDEFnTests.fn_Unquote;
  begin
    Test('WIDE.Unquote(''Some Mothers Do ''''Ave ''''Em'')').Expect(WIDE.Unquote('''Some Mothers Do ''''Ave ''''Em''')).Equals('Some Mothers Do ''Ave ''Em');
    Test('WIDE.Unquote("Some Mothers Do ''Ave ''Em")').Expect(WIDE.Unquote('"Some Mothers Do ''Ave ''Em"')).Equals('Some Mothers Do ''Ave ''Em');
    Test('WIDE.Unquote(''Mother knows best'')').Expect(WIDE.Unquote('''Mother knows best''')).Equals('Mother knows best');
    Test('WIDE.Unquote("Mother knows best")').Expect(WIDE.Unquote('"Mother knows best"')).Equals('Mother knows best');
  end;












{ TWIDETests ------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.Transcoding;
  begin
    Test('WIDE.Encode!').Expect(WIDE.Encode(SRCS)).Equals(SRCW);

    Test('WIDE.FromANSI!').Expect(WIDE.FromANSI(SRCA)).Equals(SRCW);
    Test('WIDE.FromUTF8!').Expect(WIDE.FromUTF8(SRCU)).Equals(SRCW);

    Test('WIDE.ToANSI!').Expect(WIDE(SRCW).ToANSI).Equals(SRCA);
    Test('WIDE.ToString!').Expect(WIDE(SRCW).ToString).Equals(SRCS);
    Test('WIDE.ToUTF8!').Expect(WIDE(SRCW).ToUTF8).Equals(SRCU);
    Test('WIDE.ToWIDE!').Expect(WIDE(SRCW).ToWIDE).Equals(SRCW);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Find;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    Note('aCaseMode = csCaseSensitive');

    WIDE(STR).Find('i', p);      Test('FirstPos of ''i''').Expect(p).Equals(7);
    WIDE(STR).Find('I', p);      Test('FirstPos of ''I''').Expect(p).Equals(24);

    WIDE(STR).Find('T', p);      Test('FirstPos of ''T''').Expect(p).Equals(1);
    WIDE(STR).Find('!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    WIDE(STR).Find('q', p);      Test('FirstPos of ''q''').Expect(p).Equals(5);
    WIDE(STR).Find('Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    WIDE(STR).Find('The', p);    Test('FirstPos of ''The''').Expect(p).Equals(1);
    WIDE(STR).Find('fox!', p);   Test('FirstPos of ''fox!''').Expect(p).Equals(18);
    WIDE(STR).Find('quick', p);  Test('FirstPos of ''quick''').Expect(p).Equals(5);
    WIDE(STR).Find('brown', p);  Test('FirstPos of ''brown''').Expect(p).Equals(0);

    WIDE(STR).Find('T', pa); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                             Test('First ''T''').Expect(pa[0]).Equals(1);
                             Test('Second ''T''').Expect(pa[1]).Equals(32);

    WIDE(STR).Find('!', pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                             Test('First ''!''').Expect(pa[0]).Equals(21);
                             Test('Second ''!''').Expect(pa[1]).Equals(45);

    WIDE(STR).Find('q', pa); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                             Test('First ''q''').Expect(pa[0]).Equals(5);
                             Test('Second ''q''').Expect(pa[1]).Equals(12);
                             Test('Third ''q''').Expect(pa[2]).Equals(36);

    WIDE(STR).Find('z', pa); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);

    Note('aCaseMode = csIgnoreCase');

    WIDE(STR).Find('i', p, csIgnoreCase);      Test('First ''i''').Expect(p).Equals(7);
    WIDE(STR).Find('I', p, csIgnoreCase);      Test('First ''I''').Expect(p).Equals(7);

    WIDE(STR).Find('t', p, csIgnoreCase);      Test('First ''t''').Expect(p).Equals(1);
    WIDE(STR).Find('!', p, csIgnoreCase);      Test('First ''!''').Expect(p).Equals(21);
    WIDE(STR).Find('Q', p, csIgnoreCase);      Test('First ''Q''').Expect(p).Equals(5);
    WIDE(STR).Find('Z', p, csIgnoreCase);      Test('First ''Z''').Expect(p).Equals(0);

    WIDE(STR).Find('THE',   p, csIgnoreCase);  Test('First ''THE''').Expect(p).Equals(1);
    WIDE(STR).Find('FOX!',  p, csIgnoreCase);  Test('First ''FOX!''').Expect(p).Equals(18);
    WIDE(STR).Find('QUICK', p, csIgnoreCase);  Test('First ''QUICK''').Expect(p).Equals(5);
    WIDE(STR).Find('BROWN', p, csIgnoreCase);  Test('First ''BROWN''').Expect(p).Equals(0);

    WIDE(STR).Find('T', pa, csIgnoreCase); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                           Test('First ''T''').Expect(pa[0]).Equals(1);
                                           Test('Second ''T''').Expect(pa[1]).Equals(32);

    WIDE(STR).Find('!', pa, csIgnoreCase); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                           Test('First ''!''').Expect(pa[0]).Equals(21);
                                           Test('Second ''!''').Expect(pa[1]).Equals(45);

    WIDE(STR).Find('q', pa, csIgnoreCase); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                           Test('First ''q''').Expect(pa[0]).Equals(5);
                                           Test('Second ''q''').Expect(pa[1]).Equals(12);
                                           Test('Third ''q''').Expect(pa[2]).Equals(36);

    WIDE(STR).Find('z', pa, csIgnoreCase); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_NPos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
//  var
//    p: Integer;
  begin
(*
    p := 0;
    WIDE(STR).FindNext('T', p);   Test('FirstPos of ''T''').Expect(p).Equals(1);
    WIDE(STR).FindNext('T', p);   Test('NextPos of ''T''').Expect(p).Equals(32);
    WIDE(STR).FindNext('T', p);   Test('NextPos of ''T''').Expect(p).Equals(0);

    p := 0;
    WIDE(STR).FindNext('!', p);   Test('FirstPos of ''!''').Expect(p).Equals(21);
    WIDE(STR).FindNext('!', p);   Test('NextPos of ''!''').Expect(p).Equals(45);
    WIDE(STR).FindNext('!', p);   Test('NextPos of ''!''').Expect(p).Equals(0);

    p := 0;
    WIDE(STR).FindNext('q', p);   Test('FirstPos of ''q''').Expect(p).Equals(5);
    WIDE(STR).FindNext('q', p);   Test('NextPos of ''q''').Expect(p).Equals(12);
    WIDE(STR).FindNext('q', p);   Test('NextPos of ''q''').Expect(p).Equals(36);
    WIDE(STR).FindNext('q', p);   Test('NextPos of ''q''').Expect(p).Equals(0);

    p := 0;
    WIDE(STR).FindNext('z', p);   Test('FirstPos of ''z''').Expect(p).Equals(0);
    WIDE(STR).FindNext('z', p);   Test('NextPos of ''z''').Expect(p).Equals(0);
*)
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_RPos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
//  var
//    p: Integer;
  begin
(*
    WIDE(STR).FindLast('T', p);       Test('LastPos of ''T''').Expect(p).Equals(32);
    WIDE(STR).FindLast('!', p);       Test('LastPos of ''!''').Expect(p).Equals(45);
    WIDE(STR).FindLast('q', p);       Test('LastPos of ''q''').Expect(p).Equals(36);
    WIDE(STR).FindLast('Z', p);       Test('LastPos of ''Z''').Expect(p).Equals(0);

    WIDE(STR).FindLast('The', p);     Test('LastPos of ''The''').Expect(p).Equals(32);
    WIDE(STR).FindLast('fox!', p);    Test('LastPos of ''fox!''').Expect(p).Equals(42);
    WIDE(STR).FindLast('quick', p);   Test('LastPos of ''quick''').Expect(p).Equals(36);
    WIDE(STR).FindLast('brown', p);   Test('LastPos of ''brown''').Expect(p).Equals(0);
*)
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Compare;
  const
    VECTOR: array[0..8] of TWIDEStringAB = (
                                            (A: 'a.12'; B: 'a.2'),
                                            (A: '1';   B: '11'),
                                            (A: 'a';   B: 'abc'),
                                            (A: 'abc'; B: 'def'),
                                            (A: 'abc'; B: 'ABC'),
                                            (A: 'abc'; B: 'abc'),
                                            (A: 'ABC'; B: 'abc'),
                                            (A: 'def'; B: 'abc'),
                                            (A: 'def'; B: 'd')
                                           );
    NUM_LT  = 5;
    NUM_EQ  = 1;
  var
    i: Integer;
  begin
    for i := 0 to Pred(NUM_LT) do
      Test(VECTOR[i].A + ' < ' + VECTOR[i].B + '!')
        .Expect(WIDE.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(-1);

    for i := 0 to Pred(NUM_LT) do
      Test('(' + VECTOR[i].A + ').CompareWith < ' + VECTOR[i].B + '!')
        .Expect(WIDE(VECTOR[i].A).CompareWith(VECTOR[i].B)).Equals(-1);

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(VECTOR[i].A + ' = ' + VECTOR[i].B + '!')
        .Expect(WIDE.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(0);

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test('(' + VECTOR[i].A + ').CompareWith = ' + VECTOR[i].B + '!')
        .Expect(WIDE(VECTOR[i].A).CompareWith(VECTOR[i].B)).Equals(0);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A + ' > ' + VECTOR[i].B + '!')
        .Expect(WIDE.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(1);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(VECTOR)) do
      Test('(' + VECTOR[i].A + ').CompareWith > ' + VECTOR[i].B + '!')
        .Expect(WIDE(VECTOR[i].A).CompareWith(VECTOR[i].B)).Equals(1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Contains;
  const
    STR: UnicodeString = 'The quick fox!';
  begin
    Test('contains ''T''').Expect(WIDE(STR).Contains('T')).IsTRUE;
    Test('contains ''f''').Expect(WIDE(STR).Contains('!')).IsTRUE;
    Test('contains ''q''').Expect(WIDE(STR).Contains('q')).IsTRUE;
    Test('contains ''Z''').Expect(WIDE(STR).Contains('Z')).IsFALSE;

    Test('contains ''The''').Expect(WIDE(STR).Contains('The')).IsTRUE;
    Test('contains ''fox!''').Expect(WIDE(STR).Contains('fox!')).IsTRUE;
    Test('contains ''quick''').Expect(WIDE(STR).Contains('quick')).IsTRUE;
    Test('contains ''brown''').Expect(WIDE(STR).Contains('brown')).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_ContainsText;
  const
    STR: UnicodeString = 'The quick fox!';
  begin
    Test('contains ''t''').Expect(WIDE(STR).Contains('t', csIgnoreCase)).IsTRUE;
    Test('contains ''!''').Expect(WIDE(STR).Contains('!', csIgnoreCase)).IsTRUE;
    Test('contains ''Q''').Expect(WIDE(STR).Contains('Q', csIgnoreCase)).IsTRUE;
    Test('contains ''Z''').Expect(WIDE(STR).Contains('Z', csIgnoreCase)).IsFALSE;

    Test('contains ''the''').Expect(WIDE(STR).Contains('the', csIgnoreCase)).IsTRUE;
    Test('contains ''Fox!''').Expect(WIDE(STR).Contains('Fox!', csIgnoreCase)).IsTRUE;
    Test('contains ''QUICK''').Expect(WIDE(STR).Contains('QUICK', csIgnoreCase)).IsTRUE;
    Test('contains ''brown''').Expect(WIDE(STR).Contains('brown', csIgnoreCase)).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_IsLowercase;
  const
    STR_VECTOR: array[0..2] of TWIDEStringAB = (
                                                (A: 'LowerCase';           B: 'lowercase'),
                                                (A: '*NOT LOWERCASE*';     B: '*not lowercase*'),
                                                (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                               );
    CHAR_VECTOR: array[0..3] of TWIDECharAB = (
                                               (A: ' ';   B: ' '),
                                               (A: 'L';   B: 'l'),
                                               (A: '*';   B: '*'),
                                               (A: '™';   B: '™')
                                              );
  var
    i: Integer;
  begin
    Note('Tests for WIDE(string).IsLowercase / IsLowercase(UnicodeString)...');

    Test('Empty String is NOT considered lowercase!').Expect(WIDE.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(WIDE.IsLowercase(STR_VECTOR[i].A)).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(WIDE(STR_VECTOR[i].A).IsLowercase).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].B).Expect(WIDE.IsLowercase(STR_VECTOR[i].B)).IsTRUE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].B).Expect(WIDE(STR_VECTOR[i].B).IsLowercase).IsTRUE;

    Note('Tests for WIDE(char).IsLowercase / IsLowercase(WideChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(WIDE.IsLowercase(CHAR_VECTOR[i].A)).IsFALSE;

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(WIDE(CHAR_VECTOR[i].A).IsLowercase).IsFALSE;

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].B).Expect(WIDE.IsLowercase(CHAR_VECTOR[i].B)).Equals(i = 1);

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].B).Expect(WIDE(CHAR_VECTOR[i].B).IsLowercase).Equals(i = 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_IsUppercase;
  const
    STR_VECTOR: array[0..2] of TWIDEStringAB = (
                                                (A: 'UpperCase';           B: 'UPPERCASE'),
                                                (A: '*NOT uppercase*';     B: '*NOT UPPERCASE*'),
                                                (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                               );
    CHAR_VECTOR: array[0..3] of TWIDECharAB = (
                                               (A: ' ';   B: ' '),
                                               (A: 'L';   B: 'l'),
                                               (A: '*';   B: '*'),
                                               (A: '™';   B: '™')
                                              );
  var
    i: Integer;
  begin
    Note('Tests for IsUppercase(UnicodeString)...');

    Test('Empty String is NOT considered uppercase!').Expect(WIDE.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(WIDE.IsUppercase(STR_VECTOR[i].A)).IsFALSE;

    for i := 1 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].B).Expect(WIDE.IsUppercase(STR_VECTOR[i].B)).IsTRUE;

    Note('Tests for IsUppercase(WideChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(WIDE.IsUppercase(CHAR_VECTOR[i].B)).IsFALSE;

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].B).Expect(WIDE.IsUppercase(CHAR_VECTOR[i].A)).Equals(i = 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_SameText;
  const
    VECTOR: array[0..3] of TWIDEStringAB = (
                                            (A: '';                    B: ''),
                                            (A: 'LowerCase';           B: 'lowercase'),
                                            (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                            (A: 'Microsoft Windowsâ„¢';  B: 'microsoft windowsâ„¢')
                                           );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A + ' same text as ' + VECTOR[i].B + '!')
        .Expect(WIDE(VECTOR[i].A).EqualsText(VECTOR[i].B)).IsTRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Len;
  var
    s: UnicodeString;
  begin
    s := #0;
    Test('Zero length').Expect(WIDE.Len(PWideChar(s))).Equals(0);
    s := 'short';
    Test('Non-zero length').Expect(WIDE.Len(PWideChar(s))).Equals(5);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Split;
  const
    STAR: WideChar = '*';
  var
    s: UnicodeString;
    left, right: UnicodeString;
    parts: TWideStringArray;
  begin
    Test('Split('''', ''*'')').Expect(WIDE('').Split(STAR, left, right)).IsFALSE;
    Test('')['left'].Expect(left).Equals('');
    Test('')['right'].Expect(right).Equals('');

    Test('Split(''left'', ''*'')').Expect(WIDE('left').Split(STAR, left, right)).IsFALSE;
    Test('left')['left'].Expect(left).Equals('left');
    Test('left')['right'].Expect(right).Equals('');

    Test('Split(''*right'', ''*'')').Expect(WIDE('*right').Split(STAR, left, right)).IsTRUE;
    Test('*right')['left'].Expect(left).Equals('');
    Test('*right')['right'].Expect(right).Equals('right');

    Test('Split(''left*right'', ''*'')').Expect(WIDE('left*right').Split(STAR, left, right)).IsTRUE;
    Test('left*right')['left'].Expect(left).Equals('left');
    Test('left*right')['right'].Expect(right).Equals('right');

    s := 'left*mid-left*middle*mid-right*right';
    Test('Split(''%s'', ''*'')', [s]).Expect(WIDE(s).Split(STAR, parts)).IsTRUE;
    Test('Split(''%s'', ''*'')', [s])['no. of parts'].Expect(Length(parts)).Equals(5);
    Test('part')[0].Expect(parts[0]).Equals('left');
    Test('part')[1].Expect(parts[1]).Equals('mid-left');
    Test('part')[2].Expect(parts[2]).Equals('middle');
    Test('part')[3].Expect(parts[3]).Equals('mid-right');
    Test('part')[4].Expect(parts[4]).Equals('right');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Lowercase;
  const
    STR_VECTOR: array[0..3] of TWIDEStringAB = (
                                                (A: '';                    B: ''),
                                                (A: 'LowerCase';           B: 'lowercase'),
                                                (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                                (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                               );
    CHAR_VECTOR: array[0..3] of TWIDECharAB = (
                                               (A: ' ';   B: ' '),
                                               (A: 'L';   B: 'l'),
                                               (A: '*';   B: '*'),
                                               (A: '™';   B: '™')
                                              );
  var
    i: Integer;
  begin
    Note('Tests for Lowercase(UnicodeString)...');

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(WIDE.Lowercase(STR_VECTOR[i].A)).Equals(STR_VECTOR[i].B);

    Note('Tests for Lowercase(WideChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(WIDE.Lowercase(CHAR_VECTOR[i].A)).Equals(CHAR_VECTOR[i].B);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Uppercase;
  const
    VECTOR: array[0..3] of TWIDEStringAB = (
                                            (A: '';                    B: ''),
                                            (A: 'upperCase';           B: 'UPPERCASE'),
                                            (A: '*NOT UPPERCASE*';     B: '*NOT UPPERCASE*'),
                                            (A: 'microsoft windows™';  B: 'MICROSOFT WINDOWS™')
                                           );
    CHAR_VECTOR: array[0..3] of TWIDECharAB = (
                                               (A: ' ';   B: ' '),
                                               (A: 'l';   B: 'L'),
                                               (A: '*';   B: '*'),
                                               (A: '™';   B: '™')
                                              );
  var
    i: Integer;
  begin
    Note('Tests for Uppercase(UnicodeString)...');

    for i := 0 to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A).Expect(WIDE.Uppercase(VECTOR[i].A)).Equals(VECTOR[i].B);

    Note('Tests for Uppercase(WideChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(WIDE.Uppercase(CHAR_VECTOR[i].A)).Equals(CHAR_VECTOR[i].B);
  end;




  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Embrace;
  begin
    Test.Expect(WIDE.Embrace('abc')).Equals('(abc)');
    Test.Expect(WIDE.Embrace('abc', '(')).Equals('(abc)');
    Test.Expect(WIDE.Embrace('abc', '[')).Equals('[abc]');
    Test.Expect(WIDE.Embrace('abc', '{')).Equals('{abc}');
    Test.Expect(WIDE.Embrace('abc', '<')).Equals('<abc>');
    Test.Expect(WIDE.Embrace('abc', '#')).Equals('#abc#');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Enquote;
  begin
    Test.Expect(WIDE.Enquote('abc')).Equals('''abc''');
    Test.Expect(WIDE.Enquote('abc', WIDEChar('"'))).Equals('"abc"');
    Test.Expect(WIDE.Enquote('Harry ''Flash'' Gordon')).Equals('''Harry ''''Flash'''' Gordon''');
    Test.Expect(WIDE.Enquote('Harry ''Flash'' Gordon', WIDEChar('"'))).Equals('"Harry ''Flash'' Gordon"');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Unbrace;
  begin
    Test('WIDE.Unbrace(''(abc)'')').Expect(WIDE.Unbrace('(abc)')).Equals('abc');
    Test('WIDE.Unbrace(''(abc)'')').Expect(WIDE.Unbrace('(abc)')).Equals('abc');
    Test('WIDE.Unbrace(''[abc]'')').Expect(WIDE.Unbrace('[abc]')).Equals('abc');
    Test('WIDE.Unbrace(''{abc}'')').Expect(WIDE.Unbrace('{abc}')).Equals('abc');
    Test('WIDE.Unbrace(''<abc>'')').Expect(WIDE.Unbrace('<abc>')).Equals('abc');
    Test('WIDE.Unbrace(''#abc#'')').Expect(WIDE.Unbrace('#abc#')).Equals('abc');
    Test('WIDE.Unbrace(''abc'')').Expect(WIDE.Unbrace('abc')).Equals('abc');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Unquote;
  begin
    Test.Expect(WIDE.Unquote('''abc''')).Equals('abc');
    Test.Expect(WIDE.Unquote('"abc"')).Equals('abc');
    Test.Expect(WIDE.Unquote('''Harry ''''Flash'''' Gordon''')).Equals('Harry ''Flash'' Gordon');
    Test.Expect(WIDE.Unquote('"Harry ''Flash'' Gordon"')).Equals('Harry ''Flash'' Gordon');
  end;





  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TWIDETests.fn_Replace;
    { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
begin
    Test('WIDE.Replace(''Food of the Gods'', ''o'', '''')').Expect(WIDE.Replace(ssAll, 'Food of the Gods', 'o', '')).Equals('Fd f the Gds');
    Test('WIDE.Replace(''Food of the Gods'', ''od'', '''')').Expect(WIDE.Replace(ssAll, 'Food of the Gods', 'od', '')).Equals('Fo of the Gs');
    Test('WIDE.Replace(''Waiting for Godo'', ''o'', '''')').Expect(WIDE.Replace(ssAll, 'Waiting for Godo', 'o', '')).Equals('Waiting fr Gd');

    Test('WIDE.Replace(''Food of the Gods'', ''o'', ''oo'')').Expect(WIDE.Replace(ssAll, 'Food of the Gods', 'o', 'oo')).Equals('Fooood oof the Goods');

    Test('WIDE.Replace(''Food of the Gods'', ''o'', '''')').Expect(WIDE.Replace(ssFirst, 'Food of the Gods', 'o', '')).Equals('Fod of the Gods');
    Test('WIDE.Replace(''Food of the Gods'', ''o'', '''')').Expect(WIDE.Replace(ssLast, 'Food of the Gods', 'o', '')).Equals('Food of the Gds');
  end;





end.

