
  unit Test.Strings.ANSI;

interface

  uses
    Deltics.Smoketest;


  type
    TANSIFnTests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Alloc;
      procedure fn_CopyToBuffer;
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


    TANSITests = class(TTestCase)
      procedure Transcoding;
      procedure fn_BeginsWith;
      procedure fn_Compare;
      procedure fn_Contains;
      procedure fn_EqualsText;
      procedure fn_Find;
      procedure fn_IsLowercase;
      procedure fn_IsUppercase;
      procedure fn_Remove;
      procedure fn_Replace;
      procedure fn_Split;
      procedure fn_Lowercase;
      procedure fn_Uppercase;
    end;



implementation

  uses
    Math,
    SysUtils,
    Windows,
    Deltics.Strings,
    Test.Strings;





{ TANSIFnTests }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.Transcoding;
  begin
    Test('ANSI.Encode!').Expect(ANSI.Encode(SRCS)).Equals(SRCA);

    Test('FromUTF8(string)!').Expect(ANSI.FromUTF8(SRCU)).Equals(SRCA);
    Test('FromUTF8(buffer)!').Expect(ANSI.FromUTF8(PUTF8Char(SRCU))).Equals(SRCA);

    Test('FromWIDE(string)!').Expect(ANSI.FromWide(SRCW)).Equals(SRCA);
    Test('FromWIDE(buffer)!').Expect(ANSI.FromWide(PWIDEChar(SRCW))).Equals(SRCA);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Alloc;
  var
    p: Pointer;
    pANSI: PANSIChar absolute p;
    pWIDE: PWIDEChar absolute p;
    pUTF8: PUTF8Char absolute p;
  begin
    pANSI := ANSI.AllocANSI(SRCA);

    Test('ANSI.AllocANSI result').Expect(p).IsAssigned;
    Test('ANSI.AllocANSI buffer length').Expect(ANSI.Len(pANSI)).Equals(Length(SRCA));
    Test('ANSI.AllocANSI buffer content').Expect(p).Equals(PANSIString(SRCA), Length(SRCA));

    FreeMem(pANSI);

    pWIDE := ANSI.AllocWIDE(SRCA);

    Test('ANSI.AllocWIDE result').Expect(p).IsAssigned;
    Test('ANSI.AllocWIDE buffer length').Expect(WIDE.Len(pWIDE)).Equals(Length(SRCW));
    Test('ANSI.AllocWIDE buffer content').Expect(p).Equals(PWIDEString(SRCW), Length(SRCW) * 2);

    FreeMem(pWIDE);

    pUTF8 := ANSI.AllocUTF8(SRCA);

    Test('ANSI.AllocUTF8 result').Expect(p).IsAssigned;
    Test('ANSI.AllocUTF8 buffer length').Expect(UTF8.Len(pUTF8)).Equals(Length(SRCU));
    Test('ANSI.AllocUTF8 buffer content').Expect(p).Equals(PUTF8String(SRCU), Length(SRCU));

    FreeMem(pUTF8);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_CopyToBuffer;
  begin

  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Len;
  const
    STR: ANSIString = 'test';
    BUFFER: array[0..7] of ANSIChar = ('b','u','f','f','e','r',#0,#0);
    NULLBUFFER: array[0..7] of ANSIChar = (#0,#0,#0,#0,#0,#0,#0,#0);
  begin
    Test('ANSI.Len(string)').Expect(ANSI.Len(PANSIChar(STR))).Equals(4);
    Test('ANSI.Len(buffer)').Expect(ANSI.Len(@BUFFER[0])).Equals(6);
    Test('ANSI.Len(null buffer)').Expect(ANSI.Len(@NULLBUFFER[0])).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Compare;
  begin
    Test('ANSI.Compare(a, A)').Expect(ANSI.Compare('a', 'A')).Equals(isLesser);
    Test('ANSI.Compare(A, a)').Expect(ANSI.Compare('A', 'a')).Equals(isGreater);
    Test('ANSI.Compare(a, a)').Expect(ANSI.Compare('a', 'a')).Equals(isEqual);

    Test('ANSI.Compare(c, A)').Expect(ANSI.Compare('c', 'A')).Equals(isGreater);
    Test('ANSI.Compare(c, a)').Expect(ANSI.Compare('c', 'a')).Equals(isGreater);
    Test('ANSI.Compare(a, c)').Expect(ANSI.Compare('a', 'c')).Equals(isLesser);
    Test('ANSI.Compare(A, c)').Expect(ANSI.Compare('A', 'c')).Equals(isLesser);

    Test('ANSI.Compare(a, A, csIgnoreCase)').Expect(ANSI.Compare('a', 'A', csIgnoreCase)).Equals(isEqual);
    Test('ANSI.Compare(A, a, csIgnoreCase)').Expect(ANSI.Compare('A', 'a', csIgnoreCase)).Equals(isEqual);
    Test('ANSI.Compare(a, a, csIgnoreCase)').Expect(ANSI.Compare('a', 'a', csIgnoreCase)).Equals(isEqual);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_SameString;
  begin
    Test('ANSI.SameString(abc, ABC)').Expect(ANSI.SameString('abc', 'ABC')).IsFALSE;
    Test('ANSI.SameString(ABC, abc)').Expect(ANSI.SameString('ABC', 'abc')).IsFALSE;
    Test('ANSI.SameString(abc, abc)').Expect(ANSI.SameString('abc', 'abc')).IsTRUE;
    Test('ANSI.SameString(abc, abc)').Expect(ANSI.SameString('abc', 'abcd')).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_SameText;
  begin
    Test('ANSI.SameText(abc, ABC)').Expect(ANSI.SameText('abc', 'ABC')).IsTRUE;
    Test('ANSI.SameText(ABC, abc)').Expect(ANSI.SameText('ABC', 'abc')).IsTRUE;
    Test('ANSI.SameText(abc, abc)').Expect(ANSI.SameText('abc', 'abc')).IsTRUE;
    Test('ANSI.SameText(abc, abc)').Expect(ANSI.SameText('abc', 'abcd')).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_IsLowercase;
  const
    CHARS : array[0..6] of ANSIChar   = ('a', 'â',                         {} 'A', 'Â', '1', '?', '™');
    STRS  : array[0..8] of ANSIString = ('abc', 'a1', 'âbc?', 'windows™',  {} 'ABC', 'A1', 'ABC?', 'Windows™', '');
    LAST_LOWER_CHAR = 1;
    LAST_LOWER_STR  = 3;
  var
    i: Integer;
  begin
    for i := Low(CHARS) to High(CHARS) do
      Test('ANSI.IsLowercase(%s)', [CHARS[i]]).Expect(ANSI.IsLowercase(CHARS[i])).Equals(i <= LAST_LOWER_CHAR);

    for i := Low(STRS) to High(STRS) do
      Test('ANSI.IsLowercase(%s)', [STRS[i]]).Expect(ANSI.IsLowercase(STRS[i])).Equals(i <= LAST_LOWER_STR);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_IsUppercase;
  const
    CHARS : array[0..6] of ANSIChar   = ('A', 'Â',                         {} 'a', 'â', '1', '?', '™');
    STRS  : array[0..8] of ANSIString = ('ABC', 'A1', 'ÂBC?', 'WINDOWS™',  {} 'abc', 'a1', 'abc?', 'Windows™', '');
    LAST_UPPER_CHAR = 1;
    LAST_UPPER_STR  = 3;
  var
    i: Integer;
  begin
    for i := Low(CHARS) to High(CHARS) do
      Test('ANSI.IsUppercase(%s)', [CHARS[i]]).Expect(ANSI.IsUppercase(CHARS[i])).Equals(i <= LAST_UPPER_CHAR);

    for i := Low(STRS) to High(STRS) do
      Test('ANSI.IsUppercase(%s)', [STRS[i]]).Expect(ANSI.IsUppercase(STRS[i])).Equals(i <= LAST_UPPER_STR);
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Find;
  const
    STR : ANSIString  = 'abcdefghijklmn';
    f   : ANSIChar    = 'f';
    G   : ANSIChar    = 'G';
    z   : ANSIChar    = 'z';
    def : ANSIString  = 'def';
    GHI : ANSIString  = 'GHI';
    xyz : ANSIString  = 'xyz';
  var
    p: Integer;
  begin
  // Find Char tests

    Test('ANSI.Find(%s, %s, POS)!', [STR, f]).Expect(ANSI.Find(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    Test('ANSI.Find(%s, %s, POS)!', [STR, G]).Expect(ANSI.Find(STR, G, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    Test('ANSI.Find(%s, %s, POS)!', [STR, z]).Expect(ANSI.Find(STR, z, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

  // Find String tests

    Test('ANSI.Find(%s, %s, POS)!', [STR, def]).Expect(ANSI.Find(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    Test('ANSI.Find(%s, %s, POS)!', [STR, GHI]).Expect(ANSI.Find(STR, GHI, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    Test('ANSI.Find(%s, %s, POS)!', [STR, xyz]).Expect(ANSI.Find(STR, xyz, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_FindNext;
  const              // 0         1         2
    STR : ANSIString  = 'abcdefghijklmnabcdef';
    f   : ANSIChar    = 'f';
    G   : ANSIChar    = 'G';
    z   : ANSIChar    = 'z';
    def : ANSIString  = 'def';
    GHI : ANSIString  = 'GHI';
    xyz : ANSIString  = 'xyz';
  var
    p: Integer;
  begin
  // FindNext Char tests

    p := -1;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(ANSI.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    p := 0;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(ANSI.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    p := 1;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(ANSI.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    p := 6;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(ANSI.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(6);

    p := 7;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(ANSI.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(20);

    p := 20;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(ANSI.FindNext(STR, f, p)).IsTRUE;
    Test('POS').Expect(p).Equals(20);

    p := 21;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, f, p]).Expect(ANSI.FindNext(STR, f, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    p := 10;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, G, p]).Expect(ANSI.FindNext(STR, G, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    p := 10;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, z, p]).Expect(ANSI.FindNext(STR, z, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);


  // FindNext String tests

    p := -1;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(ANSI.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    p := 0;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(ANSI.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    p := 1;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(ANSI.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    p := 4;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(ANSI.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(4);

    p := 5;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, def, p]).Expect(ANSI.FindNext(STR, def, p)).IsTRUE;
    Test('POS').Expect(p).Equals(18);

    p := 0;
    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, GHI, p]).Expect(ANSI.FindNext(STR, GHI, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);

    Test('ANSI.FindNext(%s, %s, POS [=%d])!', [STR, xyz, p]).Expect(ANSI.FindNext(STR, xyz, p)).IsFALSE;
    Test('POS').Expect(p).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Concat;
  begin
    Test('ANSI.Concat(array)').Expect(ANSI.Concat(['a', 'bee', 'c'])).Equals('abeec');
    Test('ANSI.Concat(array)').Expect(ANSI.Concat(['a', '', 'c'])).Equals('ac');

    Test('ANSI.Concat(array)').Expect(ANSI.Concat(['a', 'bee', 'c'], ',')).Equals('a,bee,c');
    Test('ANSI.Concat(array)').Expect(ANSI.Concat(['a', '', 'c'], ',')).Equals('a,,c');

    Test('ANSI.Concat(array)').Expect(ANSI.Concat(['a', 'bee', 'c'], ', ')).Equals('a, bee, c');
    Test('ANSI.Concat(array)').Expect(ANSI.Concat(['a', '', 'c'], ', ')).Equals('a, , c');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_StringOf;
  begin
    Test('ANSI.StringOf('''', 10)').Expect(ANSI.StringOf('', 10)).Equals('');
    Test('ANSI.StringOf(''foo'', 0)').Expect(ANSI.StringOf('foo', 0)).Equals('');
    Test('ANSI.StringOf(''foo'', 3)').Expect(ANSI.StringOf('foo', 3)).Equals('foofoofoo');

    Test('ANSI.StringOf(''*'', 0)').Expect(ANSI.StringOf('*', 0)).Equals('');
    Test('ANSI.StringOf(''*'', 3)').Expect(ANSI.StringOf('*', 3)).Equals('***');
  end;






  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Lowercase;
  const
    SOURCE  : array[0..6] of ANSIString = ('a', 'âbc', 'ÂBC', 'A', '1', 'Exp', 'Windows™');
    RESULT  : array[0..6] of ANSIString = ('a', 'âbc', 'âbc', 'a', '1', 'exp', 'windows™');
  var
    i: Integer;
  begin
    for i := Low(SOURCE) to High(Source) do
      Test('ANSI.Lowercase(%s)', [SOURCE[i]]).Expect(ANSI.Lowercase(SOURCE[i])).Equals(RESULT[i]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Uppercase;
  const
    SOURCE  : array[0..6] of ANSIString = ('a', 'âbc', 'ÂBC', 'A', '1', 'Exp', 'Windows™');
    RESULT  : array[0..6] of ANSIString = ('A', 'ÂBC', 'ÂBC', 'A', '1', 'EXP', 'WINDOWS™');
  var
    i: Integer;
  begin
    for i := Low(SOURCE) to High(Source) do
      Test('ANSI.Uppercase(%s)', [SOURCE[i]]).Expect(ANSI.Uppercase(SOURCE[i])).Equals(RESULT[i]);
  end;






  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Embrace;
  begin
    // Default braces ( )
    Test('ANSI.Embrace()').Expect(ANSI.Embrace('')).Equals('()');
    Test('ANSI.Embrace(abc)').Expect(ANSI.Embrace('abc')).Equals('(abc)');

    // Embracing an empty string
    Test('ANSI.Embrace('''', [)').Expect(ANSI.Embrace('', '[')).Equals('[]');
    Test('ANSI.Embrace('''', {)').Expect(ANSI.Embrace('', '{')).Equals('{}');
    Test('ANSI.Embrace('''', <)').Expect(ANSI.Embrace('', '<')).Equals('<>');
    Test('ANSI.Embrace('''', #)').Expect(ANSI.Embrace('', '#')).Equals('##');
    Test('ANSI.Embrace('''', !)').Expect(ANSI.Embrace('', '!')).Equals('!!');

    // Embracing a string with a braced pair
    Test('ANSI.Embrace(abc, ()').Expect(ANSI.Embrace('abc', '(')).Equals('(abc)');
    Test('ANSI.Embrace(abc, [)').Expect(ANSI.Embrace('abc', '[')).Equals('[abc]');
    Test('ANSI.Embrace(abc, {)').Expect(ANSI.Embrace('abc', '{')).Equals('{abc}');
    Test('ANSI.Embrace(abc, <)').Expect(ANSI.Embrace('abc', '<')).Equals('<abc>');

    // Embracing a string with non-brace character
    Test('ANSI.Embrace(abc, #)').Expect(ANSI.Embrace('abc', '#')).Equals('#abc#');
    Test('ANSI.Embrace(abc, !)').Expect(ANSI.Embrace('abc', '!')).Equals('!abc!');
    Test('ANSI.Embrace(abc, @)').Expect(ANSI.Embrace('abc', '@')).Equals('@abc@');
    Test('ANSI.Embrace(abc, $)').Expect(ANSI.Embrace('abc', '$')).Equals('$abc$');
    Test('ANSI.Embrace(abc, &)').Expect(ANSI.Embrace('abc', '&')).Equals('&abc&');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Enquote;
  begin
    Test('ANSI.Enquote('''')').Expect(ANSI.Enquote('')).Equals('''''');
    Test('ANSI.Enquote('''', ''"'')').Expect(ANSI.Enquote('', '"')).Equals('""');

    Test('ANSI.Enquote(''Mother knows best'')').Expect(ANSI.Enquote('Mother knows best')).Equals('''Mother knows best''');
    Test('ANSI.Enquote(''Mother knows best'', ''"'')').Expect(ANSI.Enquote('Mother knows best', '"')).Equals('"Mother knows best"');

    Test('ANSI.Enquote(''Some Mothers Do ''Ave ''Em'')').Expect(ANSI.Enquote('Some Mothers Do ''Ave ''Em')).Equals('''Some Mothers Do ''''Ave ''''Em''');
    Test('ANSI.Enquote(''Some Mothers Do ''Ave ''Em'', '''''')').Expect(ANSI.Enquote('Some Mothers Do ''Ave ''Em', '''')).Equals('''Some Mothers Do ''''Ave ''''Em''');
    Test('ANSI.Enquote(''Some Mothers Do ''Ave ''Em'', ''"'')').Expect(ANSI.Enquote('Some Mothers Do ''Ave ''Em', '"')).Equals('"Some Mothers Do ''Ave ''Em"');
    Test('ANSI.Enquote(''Some Mothers Do ''Ave ''Em'', '''''', ''\'')').Expect(ANSI.Enquote('Some Mothers Do ''Ave ''Em', '''', '\')).Equals('''Some Mothers Do \''Ave \''Em''');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_PadLeft;
  begin
    Test('ANSI.PadLeft(''foo'', 4, '' '')').Expect(ANSI.PadLeft('foo', 4, ' ')).Equals('    foo');
    Test('ANSI.PadLeft(''foo'', 3, '' '')').Expect(ANSI.PadLeft('foo', 3, ' ')).Equals('   foo');
    Test('ANSI.PadLeft(''foo'', 2, '' '')').Expect(ANSI.PadLeft('foo', 2, ' ')).Equals('  foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_PadRight;
  begin
    Test('ANSI.PadRight(''foo'', 4, '' '')').Expect(ANSI.PadRight('foo', 4, ' ')).Equals('foo    ');
    Test('ANSI.PadRight(''foo'', 3, '' '')').Expect(ANSI.PadRight('foo', 3, ' ')).Equals('foo   ');
    Test('ANSI.PadRight(''foo'', 2, '' '')').Expect(ANSI.PadRight('foo', 2, ' ')).Equals('foo  ');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_ExtendLeft;
  begin
    Test('ANSI.ExtendLeft(''foo'', 10)').Expect(ANSI.ExtendLeft('foo', 10)).Equals('       foo');
    Test('ANSI.ExtendLeft(''foo'', 10, '' '')').Expect(ANSI.ExtendLeft('foo', 10, ' ')).Equals('       foo');
    Test('ANSI.ExtendLeft(''foo'', 10, ''x'')').Expect(ANSI.ExtendLeft('foo', 10, 'x')).Equals('xxxxxxxfoo');

    Test('ANSI.ExtendLeft(''foo'', 3, '' '')').Expect(ANSI.ExtendLeft('foo', 3, ' ')).Equals('foo');
    Test('ANSI.ExtendLeft(''foo'', 2, '' '')').Expect(ANSI.ExtendLeft('foo', 2, ' ')).Equals('foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_ExtendRight;
  begin
    Test('ANSI.ExtendRight(''foo'', 10)').Expect(ANSI.ExtendRight('foo', 10)).Equals('foo       ');
    Test('ANSI.ExtendRight(''foo'', 10, '' '')').Expect(ANSI.ExtendRight('foo', 10, ' ')).Equals('foo       ');
    Test('ANSI.ExtendRight(''foo'', 10, ''x'')').Expect(ANSI.ExtendRight('foo', 10, 'x')).Equals('fooxxxxxxx');

    Test('ANSI.ExtendRight(''foo'', 3, '' '')').Expect(ANSI.ExtendRight('foo', 3, ' ')).Equals('foo');
    Test('ANSI.ExtendRight(''foo'', 2, '' '')').Expect(ANSI.ExtendRight('foo', 2, ' ')).Equals('foo');
  end;







  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_RemoveLeading;
  begin
    Test('ANSI.RemoveLeading(''   foo'')').Expect(ANSI.RemoveLeading('   foo')).Equals('foo');
    Test('ANSI.RemoveLeading(''   foo'', ''.'')').Expect(ANSI.RemoveLeading('   foo', '.')).Equals('   foo');
    Test('ANSI.RemoveLeading(''   foo'', 1)').Expect(ANSI.RemoveLeading('   foo', 1)).Equals('  foo');
    Test('ANSI.RemoveLeading(''   foo'', 4)').Expect(ANSI.RemoveLeading('   foo', 4)).Equals('oo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_RemoveTrailing;
  begin
    Test('ANSI.RemoveTrailing(''foo   '')').Expect(ANSI.RemoveTrailing('foo   ')).Equals('foo');
    Test('ANSI.RemoveTrailing(''foo   '', ''.'')').Expect(ANSI.RemoveTrailing('foo   ', '.')).Equals('foo   ');
    Test('ANSI.RemoveTrailing(''foo   '', 1)').Expect(ANSI.RemoveTrailing('foo   ', 1)).Equals('foo  ');
    Test('ANSI.RemoveTrailing(''foo   '', 4)').Expect(ANSI.RemoveTrailing('foo   ', 4)).Equals('fo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Trim;
  begin
    Test('ANSI.Trim(''   foo   '')').Expect(ANSI.Trim('   foo   ')).Equals('foo');
    Test('ANSI.Trim(''   foo   '', ''.'')').Expect(ANSI.Trim('   foo   ', '.')).Equals('   foo   ');
    Test('ANSI.Trim(''   foo   '', 1)').Expect(ANSI.Trim('   foo   ', 1)).Equals('  foo  ');
    Test('ANSI.Trim(''   foo   '', 4)').Expect(ANSI.Trim('   foo   ', 4)).Equals('o');
    Test('ANSI.Trim(''   foo   '', 5)').Expect(ANSI.Trim('   foo   ', 5)).Equals('');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Unbrace;
  begin
    Test('ANSI.Unbrace(''(abc)'')').Expect(ANSI.Unbrace('(abc)')).Equals('abc');
    Test('ANSI.Unbrace(''(abc)'')').Expect(ANSI.Unbrace('(abc)')).Equals('abc');
    Test('ANSI.Unbrace(''[abc]'')').Expect(ANSI.Unbrace('[abc]')).Equals('abc');
    Test('ANSI.Unbrace(''{abc}'')').Expect(ANSI.Unbrace('{abc}')).Equals('abc');
    Test('ANSI.Unbrace(''<abc>'')').Expect(ANSI.Unbrace('<abc>')).Equals('abc');
    Test('ANSI.Unbrace(''#abc#'')').Expect(ANSI.Unbrace('#abc#')).Equals('abc');
    Test('ANSI.Unbrace(''?abc?'')').Expect(ANSI.Unbrace('?abc?')).Equals('abc');
    Test('ANSI.Unbrace(''abc'')').Expect(ANSI.Unbrace('abc')).Equals('abc');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSIFnTests.fn_Unquote;
  begin
    Test('ANSI.Unquote(''Some Mothers Do ''''Ave ''''Em'')').Expect(ANSI.Unquote('''Some Mothers Do ''''Ave ''''Em''')).Equals('Some Mothers Do ''Ave ''Em');
    Test('ANSI.Unquote("Some Mothers Do ''Ave ''Em")').Expect(ANSI.Unquote('"Some Mothers Do ''Ave ''Em"')).Equals('Some Mothers Do ''Ave ''Em');
    Test('ANSI.Unquote(''Mother knows best'')').Expect(ANSI.Unquote('''Mother knows best''')).Equals('Mother knows best');
    Test('ANSI.Unquote("Mother knows best")').Expect(ANSI.Unquote('"Mother knows best"')).Equals('Mother knows best');
  end;











{ TANSITests ------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.Transcoding;
  var
    str: IANSIString;
  begin
    str := ANSI(SRCA);

    Test('ANSI(string).ToANSI').Expect(str.ToANSI).Equals(SRCW);
    Test('ANSI(string).ToString').Expect(str.ToString).Equals(SRCS);
    Test('ANSI(string).ToUTF8').Expect(str.ToUTF8).Equals(SRCU);
    Test('ANSI(string).ToWIDE').Expect(str.ToWIDE).Equals(SRCW);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_BeginsWith;
  begin

  end;


  procedure TANSITests.fn_Compare;
  const
    CASES: array[0..8] of TANSIStringAB = (
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
      Test(WIDE.FromANSI(CASES[i].A + ' < ' + CASES[i].B + '!'))
        .Expect(ANSI(CASES[i].A).CompareWith(CASES[i].B)).Equals(-1);

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(WIDE.FromANSI(CASES[i].A + ' = ' + CASES[i].B + '!'))
        .Expect(ANSI(CASES[i].A).CompareWith(CASES[i].B)).Equals(0);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(CASES)) do with CASES[i] do
      Test('ANSI(%s).CompareWith(%s) = isGreater!', [A, B]).Expect(ANSI(A).CompareWith(B)).Equals(1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_Contains;
  const
    STR: ANSIString = 'The quick fox!';
  begin
    Note('Case Sensitive');

    Test('contains ''T''').Expect(ANSI(STR).Contains('T')).IsTRUE;
    Test('contains ''t''').Expect(ANSI(STR).Contains('t')).IsFALSE;
    Test('contains ''!''').Expect(ANSI(STR).Contains('!')).IsTRUE;
    Test('contains ''Q''').Expect(ANSI(STR).Contains('Q')).IsFALSE;
    Test('contains ''q''').Expect(ANSI(STR).Contains('q')).IsTRUE;
    Test('contains ''Z''').Expect(ANSI(STR).Contains('Z')).IsFALSE;

    Test('contains ''The''').Expect(ANSI(STR).Contains('The')).IsTRUE;
    Test('contains ''fox!''').Expect(ANSI(STR).Contains('fox!')).IsTRUE;
    Test('contains ''quick''').Expect(ANSI(STR).Contains('quick')).IsTRUE;
    Test('contains ''brown''').Expect(ANSI(STR).Contains('brown')).IsFALSE;

    Note('Ignore Case');

    Test('contains ''T''').Expect(ANSI(STR).Contains('T', csIgnoreCase)).IsTRUE;
    Test('contains ''t''').Expect(ANSI(STR).Contains('t', csIgnoreCase)).IsTRUE;
    Test('contains ''!''').Expect(ANSI(STR).Contains('!', csIgnoreCase)).IsTRUE;
    Test('contains ''Q''').Expect(ANSI(STR).Contains('Q', csIgnoreCase)).IsTRUE;
    Test('contains ''q''').Expect(ANSI(STR).Contains('q', csIgnoreCase)).IsTRUE;
    Test('contains ''Z''').Expect(ANSI(STR).Contains('Z', csIgnoreCase)).IsFALSE;

    Test('contains ''the''').Expect(ANSI(STR).Contains('the', csIgnoreCase)).IsTRUE;
    Test('contains ''Fox!''').Expect(ANSI(STR).Contains('Fox!', csIgnoreCase)).IsTRUE;
    Test('contains ''QUICK''').Expect(ANSI(STR).Contains('QUICK', csIgnoreCase)).IsTRUE;
    Test('contains ''brown''').Expect(ANSI(STR).Contains('brown', csIgnoreCase)).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_EqualsText;
  const
    CASES: array[0..3] of TANSIStringAB = (
                                       (A: '';                    B: ''),
                                       (A: 'LowerCase';           B: 'lowercase'),
                                       (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                       (A: 'Microsoft Windowsâ„¢';  B: 'microsoft windowsâ„¢')
                                      );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(CASES)) do with CASES[i] do
      Test('ANSI(%s).EqualsText(%s)!', [A, B]).Expect(ANSI(A).EqualsText(B)).IsTRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_Find;
  const            // 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!â„¢';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    Note('aCaseMode = csCaseSensitive');

    ANSI(STR).Find('â„¢', p);    Test('FirstPos of ''â„¢''').Expect(p).Equals(46);

    ANSI(STR).Find('T', p);      Test('FirstPos of ''T''').Expect(p).Equals(1);
    ANSI(STR).Find('!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    ANSI(STR).Find('q', p);      Test('FirstPos of ''q''').Expect(p).Equals(5);
    ANSI(STR).Find('Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    ANSI(STR).Find('The', p);    Test('FirstPos of ''The''').Expect(p).Equals(1);
    ANSI(STR).Find('fox!', p);   Test('FirstPos of ''fox!''').Expect(p).Equals(18);
    ANSI(STR).Find('quick', p);  Test('FirstPos of ''quick''').Expect(p).Equals(5);
    ANSI(STR).Find('brown', p);  Test('FirstPos of ''brown''').Expect(p).Equals(0);

    ANSI(STR).Find('T', pa); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                             Test('First ''T''').Expect(pa[0]).Equals(1);
                             Test('Second ''T''').Expect(pa[1]).Equals(32);

    ANSI(STR).Find('!', pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                             Test('First ''!''').Expect(pa[0]).Equals(21);
                             Test('Second ''!''').Expect(pa[1]).Equals(45);

    ANSI(STR).Find('q', pa); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                             Test('First ''q''').Expect(pa[0]).Equals(5);
                             Test('Second ''q''').Expect(pa[1]).Equals(12);
                             Test('Third ''q''').Expect(pa[2]).Equals(36);

    ANSI(STR).Find('z', pa); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);


    Note('aCaseMode = csIgnoreCase');

    ANSI(STR).Find('i', p, csIgnoreCase);      Test('First ''i''').Expect(p).Equals(7);
    ANSI(STR).Find('I', p, csIgnoreCase);      Test('First ''I''').Expect(p).Equals(7);

    ANSI(STR).Find('t', p, csIgnoreCase);      Test('First ''t''').Expect(p).Equals(1);
    ANSI(STR).Find('!', p, csIgnoreCase);      Test('First ''!''').Expect(p).Equals(21);
    ANSI(STR).Find('Q', p, csIgnoreCase);      Test('First ''Q''').Expect(p).Equals(5);
    ANSI(STR).Find('Z', p, csIgnoreCase);      Test('First ''Z''').Expect(p).Equals(0);

    ANSI(STR).Find('THE',   p, csIgnoreCase);  Test('First ''THE''').Expect(p).Equals(1);
    ANSI(STR).Find('FOX!',  p, csIgnoreCase);  Test('First ''FOX!''').Expect(p).Equals(18);
    ANSI(STR).Find('QUICK', p, csIgnoreCase);  Test('First ''QUICK''').Expect(p).Equals(5);
    ANSI(STR).Find('BROWN', p, csIgnoreCase);  Test('First ''BROWN''').Expect(p).Equals(0);

    ANSI(STR).Find('T', pa, csIgnoreCase); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                           Test('First ''T''').Expect(pa[0]).Equals(1);
                                           Test('Second ''T''').Expect(pa[1]).Equals(32);

    ANSI(STR).Find('!', pa, csIgnoreCase); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                           Test('First ''!''').Expect(pa[0]).Equals(21);
                                           Test('Second ''!''').Expect(pa[1]).Equals(45);

    ANSI(STR).Find('q', pa, csIgnoreCase); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                           Test('First ''q''').Expect(pa[0]).Equals(5);
                                           Test('Second ''q''').Expect(pa[1]).Equals(12);
                                           Test('Third ''q''').Expect(pa[2]).Equals(36);

    ANSI(STR).Find('z', pa, csIgnoreCase); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


(*
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_FindFirstText;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    ANSI(STR).FindFirstText('i', p);      Test('FirstPos of ''i''').Expect(p).Equals(7);
    ANSI(STR).FindFirstText('I', p);      Test('FirstPos of ''I''').Expect(p).Equals(7);

    ANSI(STR).FindFirstText('t', p);      Test('FirstPos of ''t''').Expect(p).Equals(1);
    ANSI(STR).FindFirstText('!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    ANSI(STR).FindFirstText('Q', p);      Test('FirstPos of ''Q''').Expect(p).Equals(5);
    ANSI(STR).FindFirstText('Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    ANSI(STR).FindFirstText('THE', p);    Test('FirstPos of ''THE''').Expect(p).Equals(1);
    ANSI(STR).FindFirstText('FOX!', p);   Test('FirstPos of ''FOX!''').Expect(p).Equals(18);
    ANSI(STR).FindFirstText('QUICK', p);  Test('FirstPos of ''QUICK''').Expect(p).Equals(5);
    ANSI(STR).FindFirstText('BROWN', p);  Test('FirstPos of ''BROWN''').Expect(p).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_FindNext;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    ANSI(STR).FindFirst('T', p);      Test('Pos of ''T''').Expect(p).Equals(1);
    ANSI(STR).FindNext('T', p);       Test('NextPos of ''T''').Expect(p).Equals(32);
    ANSI(STR).FindNext('T', p);       Test('NextPos of ''T''').Expect(p).Equals(0);

    ANSI(STR).FindFirst('!', p);      Test('Pos of ''!''').Expect(p).Equals(21);
    ANSI(STR).FindNext('!', p);       Test('NextPos of ''!''').Expect(p).Equals(45);
    ANSI(STR).FindNext('!', p);       Test('NextPos of ''!''').Expect(p).Equals(0);

    ANSI(STR).FindFirst('q', p);      Test('Pos of ''q''').Expect(p).Equals(5);
    ANSI(STR).FindNext('q', p);       Test('NextPos of ''q''').Expect(p).Equals(12);
    ANSI(STR).FindNext('q', p);       Test('NextPos of ''q''').Expect(p).Equals(36);
    ANSI(STR).FindNext('q', p);       Test('NextPos of ''q''').Expect(p).Equals(0);

    ANSI(STR).FindFirst('z', p);      Test('Pos of ''z''').Expect(p).Equals(0);
    ANSI(STR).FindNext('z', p);       Test('NextPos of ''z''').Expect(p).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_FindLast;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    ANSI(STR).FindLast('T', p);       Test('FindLast of ''T''').Expect(p).Equals(32);
    ANSI(STR).FindLast('!', p);       Test('FindLast of ''!''').Expect(p).Equals(45);
    ANSI(STR).FindLast('q', p);       Test('FindLast of ''q''').Expect(p).Equals(36);
    ANSI(STR).FindLast('Z', p);       Test('FindLast of ''Z''').Expect(p).Equals(0);

    ANSI(STR).FindLast('The', p);     Test('FindLast of ''The''').Expect(p).Equals(32);
    ANSI(STR).FindLast('fox!', p);    Test('FindLast of ''fox!''').Expect(p).Equals(42);
    ANSI(STR).FindLast('quick', p);   Test('FindLast of ''quick''').Expect(p).Equals(36);
    ANSI(STR).FindLast('brown', p);   Test('FindLast of ''brown''').Expect(p).Equals(0);
  end;
*)

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_IsLowercase;
  const
    STR_VECTOR: array[0..2] of TANSIStringAB = (
                                                (A: 'LowerCase';           B: 'lowercase'),
                                                (A: '*NOT LOWERCASE*';     B: '*not lowercase*'),
                                                (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                               );
    CHAR_VECTOR: array[0..3] of TANSICharAB = (
                                               (A: ' ';   B: ' '),
                                               (A: 'L';   B: 'l'),
                                               (A: '*';   B: '*'),
                                               (A: '™';   B: '™')
                                              );
  var
    i: Integer;
  begin
    Note('Tests for IsLowercase(ANSIString)...');

    Test('Empty String is NOT considered lowercase!').Expect(ANSI('').IsLowercase).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(ANSI(STR_VECTOR[i].A).IsLowercase).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].B).Expect(ANSI(STR_VECTOR[i].B).IsLowercase).IsTRUE;

    Note('Tests for IsLowercase(ANSIChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(WIDE.FromANSI(CHAR_VECTOR[i].A)).Expect(ANSI(CHAR_VECTOR[i].A).IsLowercase).IsFALSE;

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(WIDE.FromANSI(CHAR_VECTOR[i].B)).Expect(ANSI(CHAR_VECTOR[i].B).IsLowercase).Equals(i = 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_IsUppercase;
  const
    STR_VECTOR: array[0..2] of TANSIStringAB = (
                                                (A: 'UpperCase';           B: 'UPPERCASE'),
                                                (A: '*NOT uppercase*';     B: '*NOT UPPERCASE*'),
                                                (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                               );
    CHAR_VECTOR: array[0..3] of TANSICharAB = (
                                               (A: ' ';   B: ' '),
                                               (A: 'L';   B: 'l'),
                                               (A: '*';   B: '*'),
                                               (A: '™';   B: '™')
                                              );
  var
    i: Integer;
  begin
    Note('Tests for IsUppercase(ANSIString)...');

    Test('Empty String is NOT considered uppercase!').Expect(ANSI('').IsUppercase).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(ANSI(STR_VECTOR[i].A).IsUppercase).IsFALSE;

    for i := 1 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].B).Expect(ANSI(STR_VECTOR[i].B).IsUppercase).IsTRUE;

    Note('Tests for IsUppercase(ANSIChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(ANSI(CHAR_VECTOR[i].B).IsUppercase).IsFALSE;

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].B).Expect(ANSI(CHAR_VECTOR[i].A).IsUppercase).Equals(i = 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_Split;
  const
    STAR: ANSIChar = '*';
  var
    s: ANSIString;
    left, right: ANSIString;
    parts: TANSIStringArray;
  begin
    Test('Split('''', ''*'')').Expect(ANSI('').Split(STAR, left, right)).IsFALSE;
    Test('')['left'].Expect(left).Equals('');
    Test('')['right'].Expect(right).Equals('');

    Test('Split(''left'', ''*'')').Expect(ANSI('left').Split(STAR, left, right)).IsFALSE;
    Test('left')['left'].Expect(left).Equals('left');
    Test('left')['right'].Expect(right).Equals('');

    Test('Split(''*right'', ''*'')').Expect(ANSI('*right').Split(STAR, left, right)).IsTRUE;
    Test('*right')['left'].Expect(left).Equals('');
    Test('*right')['right'].Expect(right).Equals('right');

    Test('Split(''left*right'', ''*'')').Expect(ANSI('left*right').Split(STAR, left, right)).IsTRUE;
    Test('left*right')['left'].Expect(left).Equals('left');
    Test('left*right')['right'].Expect(right).Equals('right');

    s := 'left*mid-left*middle*mid-right*right';
    Test('Split(''%s'', ''*'')', [s]).Expect(ANSI(s).Split(STAR, parts)).IsTRUE;
    Test('Split(''%s'', ''*'')', [s])['no. of parts'].Expect(Length(parts)).Equals(5);
    Test('part')[0].Expect(parts[0]).Equals('left');
    Test('part')[1].Expect(parts[1]).Equals('mid-left');
    Test('part')[2].Expect(parts[2]).Equals('middle');
    Test('part')[3].Expect(parts[3]).Equals('mid-right');
    Test('part')[4].Expect(parts[4]).Equals('right');
  end;





  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_Lowercase;
  const
    STR_VECTOR: array[0..3] of TANSIStringAB = (
                                                (A: '';                    B: ''),
                                                (A: 'LowerCase';           B: 'lowercase'),
                                                (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                                (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                               );
    CHAR_VECTOR: array[0..3] of TANSICharAB = (
                                               (A: ' ';   B: ' '),
                                               (A: 'L';   B: 'l'),
                                               (A: '*';   B: '*'),
                                               (A: '™';   B: '™')
                                              );
  var
    i: Integer;
  begin
    Note('Tests for Lowercase(ANSIString)...');

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(ANSI.Lowercase(STR_VECTOR[i].A)).Equals(STR_VECTOR[i].B);

    Note('Tests for Lowercase(ANSIChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(ANSI.Lowercase(CHAR_VECTOR[i].A)).Equals(CHAR_VECTOR[i].B);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_Remove;
  begin
    Test('ANSI.Remove(''Food of the Gods'', ''o'')').Expect(ANSI.Remove(ssAll, 'Food of the Gods', 'o')).Equals('Fd f the Gds');
    Test('ANSI.Remove(''Food of the Gods'', ''od'')').Expect(ANSI.Remove(ssAll, 'Food of the Gods', 'od')).Equals('Fo of the Gs');

    Test('ANSI.Remove(''Food of the Gods'', ''f'')').Expect(ANSI.Remove(ssAll, 'Food of the Gods', 'f')).Equals('Food o the Gods');
    Test('ANSI.Remove(''Food of the Gods'', ''f'', [rsIgnoreCase])').Expect(ANSI.Remove(ssAll, 'Food of the Gods', 'f', csIgnoreCase)).Equals('ood o the Gods');

    Test('ANSI.Remove(''Food of the Gods'', ''o'', [rsOnce])').Expect(ANSI.Remove(ssFirst, 'Food of the Gods', 'o')).Equals('Fod of the Gods');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_Replace;
  begin
    Test('ANSI.Replace(''Food of the Gods'', ''o'', '''')').Expect(ANSI.Replace(ssAll, 'Food of the Gods', 'o', '')).Equals('Fd f the Gds');
    Test('ANSI.Replace(''Food of the Gods'', ''od'', '''')').Expect(ANSI.Replace(ssAll, 'Food of the Gods', 'od', '')).Equals('Fo of the Gs');
    Test('ANSI.Replace(''Waiting for Godo'', ''o'', '''')').Expect(ANSI.Replace(ssAll, 'Waiting for Godo', 'o', '')).Equals('Waiting fr Gd');

    Test('ANSI.Replace(''Food of the Gods'', ''o'', ''oo'')').Expect(ANSI.Replace(ssAll, 'Food of the Gods', 'o', 'oo')).Equals('Fooood oof the Goods');

    Test('ANSI.Replace(''Food of the Gods'', ''o'', '''')').Expect(ANSI.Replace(ssFirst, 'Food of the Gods', 'o', '')).Equals('Fod of the Gods');
    Test('ANSI.Replace(''Food of the Gods'', ''o'', '''')').Expect(ANSI.Replace(ssLast, 'Food of the Gods', 'o', '')).Equals('Food of the Gds');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TANSITests.fn_Uppercase;
  const
    STR_VECTOR: array[0..3] of TANSIStringAB = (
                                                (A: '';                    B: ''),
                                                (A: 'UpperCase';           B: 'UPPERCASE'),
                                                (A: '*NOT LOWERCASE*';     B: '*NOT LOWERCASE*'),
                                                (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                               );
    CHAR_VECTOR: array[0..3] of TANSICharAB = (
                                               (A: ' ';   B: ' '),
                                               (A: 'l';   B: 'L'),
                                               (A: '*';   B: '*'),
                                               (A: '™';   B: '™')
                                              );
  var
    i: Integer;
  begin
    Note('Tests for Uppercase(ANSIString)...');

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(ANSI.Uppercase(STR_VECTOR[i].A)).Equals(STR_VECTOR[i].B);

    Note('Tests for Uppercase(ANSIChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(ANSI.Uppercase(CHAR_VECTOR[i].A)).Equals(CHAR_VECTOR[i].B);
  end;












end.
