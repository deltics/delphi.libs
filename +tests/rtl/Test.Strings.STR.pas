
  unit Test.Strings.STR;

interface

  uses
    Deltics.Smoketest;


  type
    TSTRFnTests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Alloc;
      procedure fn_Len;

      procedure fn_Compare;
      procedure fn_SameString;
      procedure fn_SameText;
      procedure fn_IsLowercase;
      procedure fn_IsUppercase;

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



implementation

  uses
    Math,
    Deltics.Strings,
    Test.Strings;


{ TSTRTests }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.Transcoding;
  begin
    Test('FromANSI!').Expect(STR.FromANSI(SRCA)).Equals(SRCS);

    Test('FromUTF8(string)!').Expect(STR.FromUTF8(SRCU)).Equals(SRCS);
    Test('FromUTF8(buffer)!').Expect(STR.FromUTF8(PUTF8Char(SRCU))).Equals(SRCS);

    Test('FromWIDE(string)!').Expect(STR.FromWIDE(SRCW)).Equals(SRCS);
    Test('FromWIDE(buffer)!').Expect(STR.FromWIDE(PWIDEChar(SRCW))).Equals(SRCS);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Alloc;
  var
    p: Pointer;
    pANSI: PANSIChar absolute p;
    pWIDE: PWIDEChar absolute p;
    pUTF8: PUTF8Char absolute p;
  begin
    pANSI := STR.AllocANSI(SRCS);

    Test('STR.AllocANSI result').Expect(p).IsAssigned;
    Test('STR.AllocANSI buffer length').Expect(ANSI.Len(pANSI)).Equals(Length(SRCA));
    Test('STR.AllocANSI buffer content').Expect(p).Equals(PANSIString(SRCA), Length(SRCA));

    FreeMem(pANSI);

    pWIDE := STR.AllocWIDE(SRCS);

    Test('STR.AllocWIDE result').Expect(p).IsAssigned;
    Test('STR.AllocWIDE buffer length').Expect(WIDE.Len(pWIDE)).Equals(Length(SRCW));
    Test('STR.AllocWIDE buffer content').Expect(p).Equals(PWIDEString(SRCW), Length(SRCW) * 2);

    FreeMem(pWIDE);

    pUTF8 := STR.AllocUTF8(SRCS);

    Test('STR.AllocUTF8 result').Expect(p).IsAssigned;
    Test('STR.AllocUTF8 buffer length').Expect(UTF8.Len(pUTF8)).Equals(Length(SRCU));
    Test('STR.AllocUTF8 buffer content').Expect(p).Equals(PUTF8String(SRCU), Length(SRCU));

    FreeMem(pUTF8);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Len;
  const
    STRTEST: String = 'test';
    BUFFER: array[0..7] of Char = ('b','u','f','f','e','r',#0,#0);
    NULLBUFFER: array[0..7] of Char = (#0,#0,#0,#0,#0,#0,#0,#0);
  begin
    Test('STR.Len(string)').Expect(STR.Len(PChar(STRTEST))).Equals(4);
    Test('STR.Len(buffer)').Expect(STR.Len(@BUFFER[0])).Equals(6);
    Test('STR.Len(null buffer)').Expect(STR.Len(@NULLBUFFER[0])).Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Compare;
  const
    CASES: array[0..8] of TStringAB = (
                                       (A: 'a.12'; B: 'a.2'),
                                       (A: '1';    B: '11'),
                                       (A: 'a';    B: 'abc'),
                                       (A: 'abc';  B: 'def'),
                                       (A: 'abc';  B: 'ABC'),
                                       (A: 'abc';  B: 'abc'),
                                       (A: 'ABC';  B: 'abc'),
                                       (A: 'def';  B: 'abc'),
                                       (A: 'def';  B: 'd')
                                      );
    NUM_LT  = 5;
    NUM_EQ  = 1;
  var
    i: Integer;
  begin
    for i := 0 to Pred(NUM_LT) do
      Test(CASES[i].A + ' < ' + CASES[i].B + '!')
        .Expect(STR(CASES[i].A).CompareWith(CASES[i].B)).Equals(-1);

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(CASES[i].A + ' = ' + CASES[i].B + '!')
        .Expect(STR(CASES[i].A).CompareWith(CASES[i].B)).Equals(0);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(CASES)) do
      Test(CASES[i].A + ' > ' + CASES[i].B + '!')
        .Expect(STR(CASES[i].A).CompareWith(CASES[i].B)).Equals(1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_SameString;
  const
    TRUE_CASES: array[0..3] of TStringAB = (
                                            (A: '';                    B: ''),
                                            (A: 'Mixed Case';           B: 'Mixed Case'),
                                            (A: '*not uppercase*';     B: '*not uppercase*'),
                                            (A: 'Microsoft Windows™';  B: 'Microsoft Windows™')
                                           );
    FALSE_CASES: array[0..3] of TStringAB = (
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
        Test('STR.SameString(%s, %s)', [A, B]).Expect(STR.SameString(A, B)).IsTRUE;

    for i := 0 to High(FALSE_CASES) do
      with FALSE_CASES[i] do
        Test('STR.SameString(%s, %s)', [A, B]).Expect(STR.SameString(A, B)).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_SameText;
  const
    TRUE_CASES: array[0..3] of TStringAB = (
                                            (A: '';                    B: ''),
                                            (A: 'LowerCase';           B: 'lowercase'),
                                            (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                            (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                           );
    FALSE_CASES: array[0..3] of TStringAB = (
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
        Test('STR.SameText(%s, %s)', [A, B]).Expect(STR.SameText(A, B)).IsTRUE;

    for i := 0 to High(FALSE_CASES) do
      with FALSE_CASES[i] do
        Test('STR.SameText(%s, %s)', [A, B]).Expect(STR.SameText(A, B)).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_IsLowercase;
  const
    CASES: array[0..2] of TStringAB = (
                                       (A: 'LowerCase';           B: 'lowercase'),
                                       (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                       (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                      );
  var
    i: Integer;
  begin
    Test('Empty String is NOT considered lowercase!').Expect(STR.IsLowercase('')).IsFALSE;

    for i := 0 to High(CASES) do
      Test(CASES[i].A).Expect(STR.IsLowercase(CASES[i].A)).IsFALSE;

    for i := 0 to High(CASES) do
      Test(CASES[i].B).Expect(STR.IsLowercase(CASES[i].B)).IsTRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_IsUppercase;
  const
    CASES: array[0..2] of TStringAB = (
                                       (A: 'UpperCase';           B: 'UPPERCASE'),
                                       (A: '*NOT lowercase*';     B: '*NOT LOWERCASE*'),
                                       (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                      );
  var
    i: Integer;
  begin
    Test('Empty String is NOT considered uppercase!').Expect(STR.IsLowercase('')).IsFALSE;

    for i := 0 to High(CASES) do
      Test(CASES[i].A).Expect(STR.IsUppercase(CASES[i].A)).IsFALSE;

    for i := 1 to High(CASES) do
      Test(CASES[i].B).Expect(STR.IsUppercase(CASES[i].B)).IsTRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Concat;
  begin
    Test('STR.Concat(array)').Expect(STR.Concat(['a', 'bee', 'c'])).Equals('abeec');
    Test('STR.Concat(array)').Expect(STR.Concat(['a', '', 'c'])).Equals('ac');

    Test('STR.Concat(array)').Expect(STR.Concat(['a', 'bee', 'c'], ',')).Equals('a,bee,c');
    Test('STR.Concat(array)').Expect(STR.Concat(['a', '', 'c'], ',')).Equals('a,,c');

    Test('STR.Concat(array)').Expect(STR.Concat(['a', 'bee', 'c'], ', ')).Equals('a, bee, c');
    Test('STR.Concat(array)').Expect(STR.Concat(['a', '', 'c'], ', ')).Equals('a, , c');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_StringOf;
  begin
    Test('STR.StringOf('''', 10)').Expect(STR.StringOf('', 10)).Equals('');
    Test('STR.StringOf(''foo'', 0)').Expect(STR.StringOf('foo', 0)).Equals('');
    Test('STR.StringOf(''foo'', 3)').Expect(STR.StringOf('foo', 3)).Equals('foofoofoo');

    Test('STR.StringOf(''*'', 0)').Expect(STR.StringOf('*', 0)).Equals('');
    Test('STR.StringOf(''*'', 3)').Expect(STR.StringOf('*', 3)).Equals('***');
  end;







  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Lowercase;
  const
    CASES: array[0..3] of TStringAB = (
                                       (A: '';                    B: ''),
                                       (A: 'LowerCase';           B: 'lowercase'),
                                       (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                       (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                      );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A).Expect(STR.Lowercase(CASES[i].A)).Equals(CASES[i].B);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Uppercase;
  const
    CASES: array[0..3] of TStringAB = (
                                       (A: '';                    B: ''),
                                       (A: 'UpperCase';           B: 'UPPERCASE'),
                                       (A: '*NOT LOWERCASE*';     B: '*NOT LOWERCASE*'),
                                       (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                      );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A).Expect(STR.Uppercase(CASES[i].A)).Equals(CASES[i].B);
  end;








  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Embrace;
  begin
    // Default braces ( )
    Test('STR.Embrace()').Expect(STR.Embrace('')).Equals('()');
    Test('STR.Embrace(abc)').Expect(STR.Embrace('abc')).Equals('(abc)');

    // Embracing an empty string
    Test('STR.Embrace('''', [)').Expect(STR.Embrace('', '[')).Equals('[]');
    Test('STR.Embrace('''', {)').Expect(STR.Embrace('', '{')).Equals('{}');
    Test('STR.Embrace('''', <)').Expect(STR.Embrace('', '<')).Equals('<>');
    Test('STR.Embrace('''', #)').Expect(STR.Embrace('', '#')).Equals('##');
    Test('STR.Embrace('''', !)').Expect(STR.Embrace('', '!')).Equals('!!');

    // Embracing a string with a braced pair
    Test('STR.Embrace(abc, ()').Expect(STR.Embrace('abc', '(')).Equals('(abc)');
    Test('STR.Embrace(abc, [)').Expect(STR.Embrace('abc', '[')).Equals('[abc]');
    Test('STR.Embrace(abc, {)').Expect(STR.Embrace('abc', '{')).Equals('{abc}');
    Test('STR.Embrace(abc, <)').Expect(STR.Embrace('abc', '<')).Equals('<abc>');

    // Embracing a string with non-brace character
    Test('STR.Embrace(abc, #)').Expect(STR.Embrace('abc', '#')).Equals('#abc#');
    Test('STR.Embrace(abc, !)').Expect(STR.Embrace('abc', '!')).Equals('!abc!');
    Test('STR.Embrace(abc, @)').Expect(STR.Embrace('abc', '@')).Equals('@abc@');
    Test('STR.Embrace(abc, $)').Expect(STR.Embrace('abc', '$')).Equals('$abc$');
    Test('STR.Embrace(abc, &)').Expect(STR.Embrace('abc', '&')).Equals('&abc&');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Enquote;
  begin
    Test('STR.Enquote('''')').Expect(STR.Enquote('')).Equals('''''');
    Test('STR.Enquote('''', ''"'')').Expect(STR.Enquote('', '"')).Equals('""');

    Test('STR.Enquote(''Mother knows best'')').Expect(STR.Enquote('Mother knows best')).Equals('''Mother knows best''');
    Test('STR.Enquote(''Mother knows best'', ''"'')').Expect(STR.Enquote('Mother knows best', '"')).Equals('"Mother knows best"');

    Test('STR.Enquote(''Some Mothers Do ''Ave ''Em'')').Expect(STR.Enquote('Some Mothers Do ''Ave ''Em')).Equals('''Some Mothers Do ''''Ave ''''Em''');
    Test('STR.Enquote(''Some Mothers Do ''Ave ''Em'', '''''')').Expect(STR.Enquote('Some Mothers Do ''Ave ''Em', '''')).Equals('''Some Mothers Do ''''Ave ''''Em''');
    Test('STR.Enquote(''Some Mothers Do ''Ave ''Em'', ''"'')').Expect(STR.Enquote('Some Mothers Do ''Ave ''Em', '"')).Equals('"Some Mothers Do ''Ave ''Em"');
    Test('STR.Enquote(''Some Mothers Do ''Ave ''Em'', '''''', ''\'')').Expect(STR.Enquote('Some Mothers Do ''Ave ''Em', '''', '\')).Equals('''Some Mothers Do \''Ave \''Em''');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_ExtendLeft;
  begin
    Test('STR.ExtendLeft(''foo'', 10)').Expect(STR.ExtendLeft('foo', 10)).Equals('       foo');
    Test('STR.ExtendLeft(''foo'', 10, '' '')').Expect(STR.ExtendLeft('foo', 10, ' ')).Equals('       foo');
    Test('STR.ExtendLeft(''foo'', 10, ''x'')').Expect(STR.ExtendLeft('foo', 10, 'x')).Equals('xxxxxxxfoo');

    Test('STR.ExtendLeft(''foo'', 3, '' '')').Expect(STR.ExtendLeft('foo', 3, ' ')).Equals('foo');
    Test('STR.ExtendLeft(''foo'', 2, '' '')').Expect(STR.ExtendLeft('foo', 2, ' ')).Equals('foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_ExtendRight;
  begin
    Test('STR.ExtendRight(''foo'', 10)').Expect(STR.ExtendRight('foo', 10)).Equals('foo       ');
    Test('STR.ExtendRight(''foo'', 10, '' '')').Expect(STR.ExtendRight('foo', 10, ' ')).Equals('foo       ');
    Test('STR.ExtendRight(''foo'', 10, ''x'')').Expect(STR.ExtendRight('foo', 10, 'x')).Equals('fooxxxxxxx');

    Test('STR.ExtendRight(''foo'', 3, '' '')').Expect(STR.ExtendRight('foo', 3, ' ')).Equals('foo');
    Test('STR.ExtendRight(''foo'', 2, '' '')').Expect(STR.ExtendRight('foo', 2, ' ')).Equals('foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_PadLeft;
  begin
    Test('STR.PadLeft(''foo'', 4, '' '')').Expect(STR.PadLeft('foo', 4, ' ')).Equals('    foo');
    Test('STR.PadLeft(''foo'', 3, '' '')').Expect(STR.PadLeft('foo', 3, ' ')).Equals('   foo');
    Test('STR.PadLeft(''foo'', 2, '' '')').Expect(STR.PadLeft('foo', 2, ' ')).Equals('  foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_PadRight;
  begin
    Test('STR.PadRight(''foo'', 4, '' '')').Expect(STR.PadRight('foo', 4, ' ')).Equals('foo    ');
    Test('STR.PadRight(''foo'', 3, '' '')').Expect(STR.PadRight('foo', 3, ' ')).Equals('foo   ');
    Test('STR.PadRight(''foo'', 2, '' '')').Expect(STR.PadRight('foo', 2, ' ')).Equals('foo  ');
  end;






  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_RemoveLeading;
  begin
    Test('STR.RemoveLeading(''   foo'')').Expect(STR.RemoveLeading('   foo')).Equals('foo');
    Test('STR.RemoveLeading(''   foo'', 1)').Expect(STR.RemoveLeading('   foo', 1)).Equals('  foo');
    Test('STR.RemoveLeading(''   foo'', 4)').Expect(STR.RemoveLeading('   foo', 4)).Equals('oo');

    Test('STR.RemoveLeading(''   foo'', ''.'')').Expect(STR.RemoveLeading('   foo', '.')).Equals('   foo');
    Test('STR.RemoveLeading(''   foo'', '' '')').Expect(STR.RemoveLeading('   foo', ' ')).Equals('foo');
    Test('STR.RemoveLeading(''xxxfoo'', '' '')').Expect(STR.RemoveLeading('xxxfoo', 'x')).Equals('foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_RemoveTrailing;
  begin
    Test('STR.RemoveTrailing(''foo   '')').Expect(STR.RemoveTrailing('foo   ')).Equals('foo');
    Test('STR.RemoveTrailing(''foo   '', ''.'')').Expect(STR.RemoveTrailing('foo   ', '.')).Equals('foo   ');
    Test('STR.RemoveTrailing(''foo   '', 1)').Expect(STR.RemoveTrailing('foo   ', 1)).Equals('foo  ');
    Test('STR.RemoveTrailing(''foo   '', 4)').Expect(STR.RemoveTrailing('foo   ', 4)).Equals('fo');
    Test('STR.RemoveTrailing(''fooxxx'', ''x'')').Expect(STR.RemoveTrailing('fooxxx', 'x')).Equals('foo');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Trim;
  begin
    Test('STR.Trim(''   foo   '')').Expect(STR.Trim('   foo   ')).Equals('foo');
    Test('STR.Trim(''   foo   '', ''.'')').Expect(STR.Trim('   foo   ', '.')).Equals('   foo   ');
    Test('STR.Trim(''   foo   '', 1)').Expect(STR.Trim('   foo   ', 1)).Equals('  foo  ');
    Test('STR.Trim(''   foo   '', 4)').Expect(STR.Trim('   foo   ', 4)).Equals('o');
    Test('STR.Trim(''   foo   '', 5)').Expect(STR.Trim('   foo   ', 5)).Equals('');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Unbrace;
  begin
    Test('STR.Unbrace(''(abc)'')').Expect(STR.Unbrace('(abc)')).Equals('abc');
    Test('STR.Unbrace(''(abc)'')').Expect(STR.Unbrace('(abc)')).Equals('abc');
    Test('STR.Unbrace(''[abc]'')').Expect(STR.Unbrace('[abc]')).Equals('abc');
    Test('STR.Unbrace(''{abc}'')').Expect(STR.Unbrace('{abc}')).Equals('abc');
    Test('STR.Unbrace(''<abc>'')').Expect(STR.Unbrace('<abc>')).Equals('abc');
    Test('STR.Unbrace(''#abc#'')').Expect(STR.Unbrace('#abc#')).Equals('abc');
    Test('STR.Unbrace(''?abc?'')').Expect(STR.Unbrace('?abc?')).Equals('abc');
    Test('STR.Unbrace(''abc'')').Expect(STR.Unbrace('abc')).Equals('abc');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSTRFnTests.fn_Unquote;
  begin
    Test('STR.Unquote(''Some Mothers Do ''''Ave ''''Em'')').Expect(STR.Unquote('''Some Mothers Do ''''Ave ''''Em''')).Equals('Some Mothers Do ''Ave ''Em');
    Test('STR.Unquote("Some Mothers Do ''Ave ''Em")').Expect(STR.Unquote('"Some Mothers Do ''Ave ''Em"')).Equals('Some Mothers Do ''Ave ''Em');
    Test('STR.Unquote(''Mother knows best'')').Expect(STR.Unquote('''Mother knows best''')).Equals('Mother knows best');
    Test('STR.Unquote("Mother knows best")').Expect(STR.Unquote('"Mother knows best"')).Equals('Mother knows best');
  end;







end.
