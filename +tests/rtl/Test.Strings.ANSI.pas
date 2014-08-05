
  unit Test.Strings.ANSI;

interface

  uses
    Deltics.Smoketest;


  type
    TANSIFnTests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Compare;
      procedure fn_CompareText;
      procedure fn_Coalesce;
      procedure fn_Concat;
      procedure fn_Embrace;
      procedure fn_Enquote;
      procedure fn_IsLowercase;
      procedure fn_IsUppercase;
      procedure fn_Len;
      procedure fn_Lowercase;
      procedure fn_PadLeft;
      procedure fn_PadRight;
      procedure fn_PadToLengthLeft;
      procedure fn_PadToLengthRight;
      procedure fn_RepeatString;
      procedure fn_SameText;
      procedure fn_StringOfChar;
      procedure fn_TrimLeft;
      procedure fn_TrimRight;
      procedure fn_Unbrace;
      procedure fn_Unquote;
      procedure fn_Uppercase;
    end;


    TANSITests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Compare;
      procedure fn_Contains;
      procedure fn_ContainsText;
      procedure fn_EqualsText;
      procedure fn_Find;
      procedure fn_FindText;
      procedure fn_FindFirst;
      procedure fn_FindFirstText;
      procedure fn_FindNext;
      procedure fn_FindLast;
      procedure fn_IsLowercase;
      procedure fn_IsUppercase;
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

  procedure TANSIFnTests.Transcoding;
  begin
    Test('ANSI.Encode!').Expect(ANSI.Encode(SRCS)).Equals(SRCA);
    Test('STR.FromANSI!').Expect(STR.FromANSI(SRCA)).Equals(SRCS);

    Test('FromUTF8(utf8string)!').Expect(ANSI.FromUTF8(SRCU)).Equals(SRCA);
    Test('FromUTF8(buffer)!').Expect(ANSI.FromUTF8(@SRCU[1], Length(SRCU))).Equals(SRCA);

    Test('FromWIDE!').Expect(ANSI.FromWide(SRCW)).Equals(SRCA);
  end;


  procedure TANSIFnTests.fn_Coalesce;
  begin
    Test('ANSI.Coalesce(array)').Expect(ANSI.Coalesce(['a', 'bee', 'c'], ',')).Equals('a,bee,c');
    Test('ANSI.Coalesce(array)').Expect(ANSI.Coalesce(['a', '', 'c'], ',')).Equals('a,c');
  end;


  procedure TANSIFnTests.fn_Compare;
  begin
    Test('ANSI.Compare(a, A)').Expect(ANSI.Compare('a', 'A')).LessThan(0);
    Test('ANSI.Compare(A, a)').Expect(ANSI.Compare('A', 'a')).GreaterThan(0);
    Test('ANSI.Compare(a, a)').Expect(ANSI.Compare('a', 'a')).Equals(0);
  end;


  procedure TANSIFnTests.fn_CompareText;
  begin
    Test('ANSI.CompareText(a, A)').Expect(ANSI.CompareText('a', 'A')).Equals(0);
    Test('ANSI.CompareText(A, a)').Expect(ANSI.CompareText('A', 'a')).Equals(0);
    Test('ANSI.CompareText(a, a)').Expect(ANSI.CompareText('a', 'a')).Equals(0);
  end;


  procedure TANSIFnTests.fn_Concat;
  begin
    Test('ANSI.Concat(array)').Expect(ANSI.Concat(['a', 'bee', 'c'], ',')).Equals('a,bee,c');
    Test('ANSI.Concat(array)').Expect(ANSI.Concat(['a', '', 'c'], ',')).Equals('a,,c');
  end;


  procedure TANSIFnTests.fn_Embrace;
  begin
    Test.Expect(ANSI.Embrace('abc')).Equals('(abc)');
    Test.Expect(ANSI.Embrace('abc', '(')).Equals('(abc)');
    Test.Expect(ANSI.Embrace('abc', '[')).Equals('[abc]');
    Test.Expect(ANSI.Embrace('abc', '{')).Equals('{abc}');
    Test.Expect(ANSI.Embrace('abc', '<')).Equals('<abc>');
    Test.Expect(ANSI.Embrace('abc', '#')).Equals('#abc#');
  end;


  procedure TANSIFnTests.fn_Enquote;
  begin
    Test('ANSI.Enquote(''Some Mothers Do ''Ave ''Em'')').Expect(ANSI.Enquote('Some Mothers Do ''Ave ''Em')).Equals('''Some Mothers Do ''''Ave ''''Em''');
    Test('ANSI.Enquote(''Some Mothers Do ''Ave ''Em'', ''"'')').Expect(ANSI.Enquote('Some Mothers Do ''Ave ''Em', '"')).Equals('"Some Mothers Do ''Ave ''Em"');
    Test('ANSI.Enquote(''Mother knows best'')').Expect(ANSI.Enquote('Mother knows best')).Equals('''Mother knows best''');
    Test('ANSI.Enquote(''Mother knows best'', ''"'')').Expect(ANSI.Enquote('Mother knows best', '"')).Equals('"Mother knows best"');
  end;


  procedure TANSIFnTests.fn_IsLowercase;
  begin
    Test('ANSI.IsLowercase(a)').Expect(ANSI.IsLowercase('a')).IsTRUE;
    Test('ANSI.IsLowercase(A)').Expect(ANSI.IsLowercase('A')).IsFALSE;
    Test('ANSI.IsLowercase(1)').Expect(ANSI.IsLowercase('1')).IsFALSE;
    Test('ANSI.IsLowercase(?)').Expect(ANSI.IsLowercase('?')).IsFALSE;
    Test('ANSI.IsLowercase(™)').Expect(ANSI.IsLowercase('™')).IsFALSE;
  end;


  procedure TANSIFnTests.fn_IsUppercase;
  begin
    Test('ANSI.IsUppercase(a)').Expect(ANSI.IsUppercase('a')).IsFALSE;
    Test('ANSI.IsUppercase(A)').Expect(ANSI.IsUppercase('A')).IsTRUE;
    Test('ANSI.IsUppercase(1)').Expect(ANSI.IsUppercase('1')).IsFALSE;
    Test('ANSI.IsUppercase(?)').Expect(ANSI.IsUppercase('?')).IsFALSE;
    Test('ANSI.IsUppercase(™)').Expect(ANSI.IsUppercase('™')).IsFALSE;
  end;


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


  procedure TANSIFnTests.fn_Lowercase;
  begin
    Test('ANSI.Lowercase(a)').Expect(ANSI.Lowercase('a')).Equals('a');
    Test('ANSI.Lowercase(A)').Expect(ANSI.Lowercase('A')).Equals('a');
    Test('ANSI.Lowercase(1)').Expect(ANSI.Lowercase('1')).Equals('1');
    Test('ANSI.Lowercase(?)').Expect(ANSI.Lowercase('?')).Equals('?');
    Test('ANSI.Lowercase(â„¢)').Expect(ANSI.Lowercase('â„¢')).Equals('â„¢');
  end;


  procedure TANSIFnTests.fn_PadLeft;
  begin
    Test('ANSI.PadLeft(''foo'', 4, '' '')').Expect(ANSI.PadLeft('foo', 4, ' ')).Equals('    foo');
    Test('ANSI.PadLeft(''foo'', 3, '' '')').Expect(ANSI.PadLeft('foo', 3, ' ')).Equals('   foo');
    Test('ANSI.PadLeft(''foo'', 2, '' '')').Expect(ANSI.PadLeft('foo', 2, ' ')).Equals('  foo');
  end;


  procedure TANSIFnTests.fn_PadRight;
  begin
    Test('ANSI.PadRight(''foo'', 4, '' '')').Expect(ANSI.PadRight('foo', 4, ' ')).Equals('foo    ');
    Test('ANSI.PadRight(''foo'', 3, '' '')').Expect(ANSI.PadRight('foo', 3, ' ')).Equals('foo   ');
    Test('ANSI.PadRight(''foo'', 2, '' '')').Expect(ANSI.PadRight('foo', 2, ' ')).Equals('foo  ');
  end;


  procedure TANSIFnTests.fn_PadToLengthLeft;
  begin
    Test('ANSI.PadToLengthLeft(''foo'', 4, '' '')').Expect(ANSI.PadToLengthLeft('foo', 4, ' ')).Equals(' foo');
    Test('ANSI.PadToLengthLeft(''foo'', 3, '' '')').Expect(ANSI.PadToLengthLeft('foo', 3, ' ')).Equals('foo');
    Test('ANSI.PadToLengthLeft(''foo'', 2, '' '')').Expect(ANSI.PadToLengthLeft('foo', 2, ' ')).Equals('foo');
  end;


  procedure TANSIFnTests.fn_PadToLengthRight;
  begin
    Test('ANSI.PadToLengthRight(''foo'', 4, '' '')').Expect(ANSI.PadToLengthRight('foo', 4, ' ')).Equals('foo ');
    Test('ANSI.PadToLengthRight(''foo'', 3, '' '')').Expect(ANSI.PadToLengthRight('foo', 3, ' ')).Equals('foo');
    Test('ANSI.PadToLengthRight(''foo'', 2, '' '')').Expect(ANSI.PadToLengthRight('foo', 2, ' ')).Equals('foo');
  end;


  procedure TANSIFnTests.fn_RepeatString;
  begin
    Test('ANSI.RepeatString(''foo'', 0)').Expect(ANSI.RepeatString('foo', 0)).Equals('');
    Test('ANSI.RepeatString(''foo'', 3)').Expect(ANSI.RepeatString('foo', 3)).Equals('foofoofoo');
  end;


  procedure TANSIFnTests.fn_SameText;
  begin
    Test('ANSI.SameText(a, A)').Expect(ANSI.SameText('a', 'A')).IsTRUE;
    Test('ANSI.SameText(A, a)').Expect(ANSI.SameText('A', 'a')).IsTRUE;
    Test('ANSI.SameText(a, a)').Expect(ANSI.SameText('a', 'a')).IsTRUE;
  end;


  procedure TANSIFnTests.fn_StringOfChar;
  begin
    Test('ANSI.StringOfChar(''foo'', 0)').Expect(ANSI.StringOfChar('*', 0)).Equals('');
    Test('ANSI.StringOfChar(''foo'', 3)').Expect(ANSI.StringOfChar('*', 3)).Equals('***');
  end;


  procedure TANSIFnTests.fn_TrimLeft;
  begin
    Test('ANSI.TrimLeft(''   foo'')').Expect(ANSI.TrimLeft('   foo')).Equals('foo');
    Test('ANSI.TrimLeft(''   foo'', ''.'')').Expect(ANSI.TrimLeft('   foo', '.')).Equals('   foo');
    Test('ANSI.TrimLeft(''   foo'', 1)').Expect(ANSI.TrimLeft('   foo', 1)).Equals('  foo');
    Test('ANSI.TrimLeft(''   foo'', 4)').Expect(ANSI.TrimLeft('   foo', 4)).Equals('oo');
  end;


  procedure TANSIFnTests.fn_TrimRight;
  begin
    Test('ANSI.TrimRight(''foo   '')').Expect(ANSI.TrimRight('foo   ')).Equals('foo');
    Test('ANSI.TrimRight(''foo   '', ''.'')').Expect(ANSI.TrimRight('foo   ', '.')).Equals('foo   ');
    Test('ANSI.TrimRight(''foo   '', 1)').Expect(ANSI.TrimRight('foo   ', 1)).Equals('foo  ');
    Test('ANSI.TrimRight(''foo   '', 4)').Expect(ANSI.TrimRight('foo   ', 4)).Equals('fo');
  end;


  procedure TANSIFnTests.fn_Unbrace;
  begin
    Test('ANSI.Unbrace(''(abc)'')').Expect(ANSI.Unbrace('(abc)')).Equals('abc');
    Test('ANSI.Unbrace(''(abc)'')').Expect(ANSI.Unbrace('(abc)')).Equals('abc');
    Test('ANSI.Unbrace(''[abc]'')').Expect(ANSI.Unbrace('[abc]')).Equals('abc');
    Test('ANSI.Unbrace(''{abc}'')').Expect(ANSI.Unbrace('{abc}')).Equals('abc');
    Test('ANSI.Unbrace(''<abc>'')').Expect(ANSI.Unbrace('<abc>')).Equals('abc');
    Test('ANSI.Unbrace(''#abc#'')').Expect(ANSI.Unbrace('#abc#')).Equals('abc');
    Test('ANSI.Unbrace(''?abc?'')').Expect(ANSI.Unbrace('?abc?')).Equals('abc');
  end;


  procedure TANSIFnTests.fn_Unquote;
  begin
    Test('ANSI.Unquote(''Some Mothers Do ''''Ave ''''Em'')').Expect(ANSI.Unquote('''Some Mothers Do ''''Ave ''''Em''')).Equals('Some Mothers Do ''Ave ''Em');
    Test('ANSI.Unquote("Some Mothers Do ''Ave ''Em")').Expect(ANSI.Unquote('"Some Mothers Do ''Ave ''Em"')).Equals('Some Mothers Do ''Ave ''Em');
    Test('ANSI.Unquote(''Mother knows best'')').Expect(ANSI.Unquote('''Mother knows best''')).Equals('Mother knows best');
    Test('ANSI.Unquote("Mother knows best")').Expect(ANSI.Unquote('"Mother knows best"')).Equals('Mother knows best');
  end;


  procedure TANSIFnTests.fn_Uppercase;
  begin
    Test('ANSI.Uppercase(a)').Expect(ANSI.Uppercase('a')).Equals('A');
    Test('ANSI.Uppercase(A)').Expect(ANSI.Uppercase('A')).Equals('A');
    Test('ANSI.Uppercase(1)').Expect(ANSI.Uppercase('1')).Equals('1');
    Test('ANSI.Uppercase(?)').Expect(ANSI.Uppercase('?')).Equals('?');
    Test('ANSI.Uppercase(™)').Expect(ANSI.Uppercase('™')).Equals('™');
  end;









{ TANSITests ------------------------------------------------------------------------------------- }

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

    for i := (NUM_LT + NUM_EQ) to Pred(Length(CASES)) do
      Test(WIDE.FromANSI(CASES[i].A + ' > ' + CASES[i].B + '!'))
        .Expect(ANSI(CASES[i].A).CompareWith(CASES[i].B)).Equals(1);
  end;


  procedure TANSITests.fn_Contains;
  const
    STR: ANSIString = 'The quick fox!';
  begin
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
  end;


  procedure TANSITests.fn_ContainsText;
  const
    STR: ANSIString = 'The quick fox!';
  begin
    Test('contains ''T''').Expect(ANSI(STR).ContainsText('T')).IsTRUE;
    Test('contains ''t''').Expect(ANSI(STR).ContainsText('t')).IsTRUE;
    Test('contains ''!''').Expect(ANSI(STR).ContainsText('!')).IsTRUE;
    Test('contains ''Q''').Expect(ANSI(STR).ContainsText('Q')).IsTRUE;
    Test('contains ''q''').Expect(ANSI(STR).ContainsText('q')).IsTRUE;
    Test('contains ''Z''').Expect(ANSI(STR).ContainsText('Z')).IsFALSE;

    Test('contains ''the''').Expect(ANSI(STR).ContainsText('the')).IsTRUE;
    Test('contains ''Fox!''').Expect(ANSI(STR).ContainsText('Fox!')).IsTRUE;
    Test('contains ''QUICK''').Expect(ANSI(STR).ContainsText('QUICK')).IsTRUE;
    Test('contains ''brown''').Expect(ANSI(STR).ContainsText('brown')).IsFALSE;
  end;


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
    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A + ' same text as ' + CASES[i].B + '!')
        .Expect(ANSI(CASES[i].A).EqualsText(CASES[i].B)).IsTRUE;
  end;


  procedure TANSITests.fn_Find;
  const            // 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!â„¢';
  var
    pa: TCharIndexArray;
  begin
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
  end;


  procedure TANSITests.fn_FindText;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
    pa: TCharIndexArray;
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

    ANSI(STR).FindText('T', pa); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                 Test('First ''T''').Expect(pa[0]).Equals(1);
                                 Test('Second ''T''').Expect(pa[1]).Equals(32);

    ANSI(STR).FindText('!', pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                 Test('First ''!''').Expect(pa[0]).Equals(21);
                                 Test('Second ''!''').Expect(pa[1]).Equals(45);

    ANSI(STR).FindText('q', pa); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                 Test('First ''q''').Expect(pa[0]).Equals(5);
                                 Test('Second ''q''').Expect(pa[1]).Equals(12);
                                 Test('Third ''q''').Expect(pa[2]).Equals(36);

    ANSI(STR).FindText('z', pa); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


  procedure TANSITests.fn_FindFirst;
  const            // 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!â„¢';
  var
    p: Integer;
  begin
    ANSI(STR).FindFirst('â„¢', p);      Test('FirstPos of ''â„¢''').Expect(p).Equals(46);

    ANSI(STR).FindFirst('T', p);      Test('FirstPos of ''T''').Expect(p).Equals(1);
    ANSI(STR).FindFirst('!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    ANSI(STR).FindFirst('q', p);      Test('FirstPos of ''q''').Expect(p).Equals(5);
    ANSI(STR).FindFirst('Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    ANSI(STR).FindFirst('The', p);    Test('FirstPos of ''The''').Expect(p).Equals(1);
    ANSI(STR).FindFirst('fox!', p);   Test('FirstPos of ''fox!''').Expect(p).Equals(18);
    ANSI(STR).FindFirst('quick', p);  Test('FirstPos of ''quick''').Expect(p).Equals(5);
    ANSI(STR).FindFirst('brown', p);  Test('FirstPos of ''brown''').Expect(p).Equals(0);
  end;


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


  procedure TANSITests.fn_Replace;
  begin
    Test('ANSI.Replace(''Food of the Gods'', ''o'', '''')').Expect(ANSI.Replace('Food of the Gods', 'o', '', [rfReplaceAll])).Equals('Fd f the Gds');
    Test('ANSI.Replace(''Food of the Gods'', ''od'', '''')').Expect(ANSI.Replace('Food of the Gods', 'od', '', [rfReplaceAll])).Equals('Fo of the Gs');
    Test('ANSI.Replace(''Waiting for Godo'', ''o'', '''')').Expect(ANSI.Replace('Waiting for Godo', 'o', '', [rfReplaceAll])).Equals('Waiting fr Gd');

    Test('ANSI.Replace(''Food of the Gods'', ''o'', ''oo'')').Expect(ANSI.Replace('Food of the Gods', 'o', 'oo', [rfReplaceAll])).Equals('Fooood oof the Goods');

    Test('ANSI.Replace(''Food of the Gods'', ''o'', '''')').Expect(ANSI.Replace('Food of the Gods', 'o', '', [])).Equals('Fod of the Gods');
  end;


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
