
  unit Test.Strings.WIDE;

interface

  uses
    Deltics.Smoketest;


  type
    TWIDETests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Len;
      procedure fn_Pos;
      procedure fn_PosText;
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



{ TWIDETests ------------------------------------------------------------------------------------- }

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


  procedure TWIDETests.fn_Pos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    WIDE(STR).FindFirst('i', p);      Test('FirstPos of ''i''').Expect(p).Equals(7);
    WIDE(STR).FindFirst('I', p);      Test('FirstPos of ''I''').Expect(p).Equals(24);

    WIDE(STR).FindFirst('T', p);      Test('FirstPos of ''T''').Expect(p).Equals(1);
    WIDE(STR).FindFirst('!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    WIDE(STR).FindFirst('q', p);      Test('FirstPos of ''q''').Expect(p).Equals(5);
    WIDE(STR).FindFirst('Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    WIDE(STR).FindFirst('The', p);    Test('FirstPos of ''The''').Expect(p).Equals(1);
    WIDE(STR).FindFirst('fox!', p);   Test('FirstPos of ''fox!''').Expect(p).Equals(18);
    WIDE(STR).FindFirst('quick', p);  Test('FirstPos of ''quick''').Expect(p).Equals(5);
    WIDE(STR).FindFirst('brown', p);  Test('FirstPos of ''brown''').Expect(p).Equals(0);

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
  end;


  procedure TWIDETests.fn_PosText;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    WIDE(STR).FindFirstText('i', p);      Test('FirstPos of ''i''').Expect(p).Equals(7);
    WIDE(STR).FindFirstText('I', p);      Test('FirstPos of ''I''').Expect(p).Equals(7);

    WIDE(STR).FindFirstText('t', p);      Test('FirstPos of ''t''').Expect(p).Equals(1);
    WIDE(STR).FindFirstText('!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    WIDE(STR).FindFirstText('Q', p);      Test('FirstPos of ''Q''').Expect(p).Equals(5);
    WIDE(STR).FindFirstText('Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    WIDE(STR).FindFirstText('THE', p);    Test('FirstPos of ''THE''').Expect(p).Equals(1);
    WIDE(STR).FindFirstText('FOX!', p);   Test('FirstPos of ''FOX!''').Expect(p).Equals(18);
    WIDE(STR).FindFirstText('QUICK', p);  Test('FirstPos of ''QUICK''').Expect(p).Equals(5);
    WIDE(STR).FindFirstText('BROWN', p);  Test('FirstPos of ''BROWN''').Expect(p).Equals(0);

    WIDE(STR).FindText('T', pa); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                 Test('First ''T''').Expect(pa[0]).Equals(1);
                                 Test('Second ''T''').Expect(pa[1]).Equals(32);

    WIDE(STR).FindText('!', pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                 Test('First ''!''').Expect(pa[0]).Equals(21);
                                 Test('Second ''!''').Expect(pa[1]).Equals(45);

    WIDE(STR).FindText('q', pa); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                 Test('First ''q''').Expect(pa[0]).Equals(5);
                                 Test('Second ''q''').Expect(pa[1]).Equals(12);
                                 Test('Third ''q''').Expect(pa[2]).Equals(36);

    WIDE(STR).FindText('z', pa); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


  procedure TWIDETests.fn_NPos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
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
  end;


  procedure TWIDETests.fn_RPos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    WIDE(STR).FindLast('T', p);       Test('LastPos of ''T''').Expect(p).Equals(32);
    WIDE(STR).FindLast('!', p);       Test('LastPos of ''!''').Expect(p).Equals(45);
    WIDE(STR).FindLast('q', p);       Test('LastPos of ''q''').Expect(p).Equals(36);
    WIDE(STR).FindLast('Z', p);       Test('LastPos of ''Z''').Expect(p).Equals(0);

    WIDE(STR).FindLast('The', p);     Test('LastPos of ''The''').Expect(p).Equals(32);
    WIDE(STR).FindLast('fox!', p);    Test('LastPos of ''fox!''').Expect(p).Equals(42);
    WIDE(STR).FindLast('quick', p);   Test('LastPos of ''quick''').Expect(p).Equals(36);
    WIDE(STR).FindLast('brown', p);   Test('LastPos of ''brown''').Expect(p).Equals(0);
  end;


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


  procedure TWIDETests.fn_ContainsText;
  const
    STR: UnicodeString = 'The quick fox!';
  begin
    Test('contains ''t''').Expect(WIDE(STR).ContainsText('t')).IsTRUE;
    Test('contains ''!''').Expect(WIDE(STR).ContainsText('!')).IsTRUE;
    Test('contains ''Q''').Expect(WIDE(STR).ContainsText('Q')).IsTRUE;
    Test('contains ''Z''').Expect(WIDE(STR).ContainsText('Z')).IsFALSE;

    Test('contains ''the''').Expect(WIDE(STR).ContainsText('the')).IsTRUE;
    Test('contains ''Fox!''').Expect(WIDE(STR).ContainsText('Fox!')).IsTRUE;
    Test('contains ''QUICK''').Expect(WIDE(STR).ContainsText('QUICK')).IsTRUE;
    Test('contains ''brown''').Expect(WIDE(STR).ContainsText('brown')).IsFALSE;
  end;


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


  procedure TWIDETests.fn_Len;
  var
    s: UnicodeString;
  begin
    s := #0;
    Test('Zero length').Expect(WIDE.Len(PWideChar(s))).Equals(0);
    s := 'short';
    Test('Non-zero length').Expect(WIDE.Len(PWideChar(s))).Equals(5);
  end;


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




  procedure TWIDETests.fn_Embrace;
  begin
    Test.Expect(WIDE.Embrace('abc')).Equals('(abc)');
    Test.Expect(WIDE.Embrace('abc', '(')).Equals('(abc)');
    Test.Expect(WIDE.Embrace('abc', '[')).Equals('[abc]');
    Test.Expect(WIDE.Embrace('abc', '{')).Equals('{abc}');
    Test.Expect(WIDE.Embrace('abc', '<')).Equals('<abc>');
    Test.Expect(WIDE.Embrace('abc', '#')).Equals('#abc#');
  end;


  procedure TWIDETests.fn_Enquote;
  begin
    Test.Expect(WIDE.Enquote('abc')).Equals('''abc''');
    Test.Expect(WIDE.Enquote('abc', '"')).Equals('"abc"');
    Test.Expect(WIDE.Enquote('Harry ''Flash'' Gordon')).Equals('''Harry ''''Flash'''' Gordon''');
    Test.Expect(WIDE.Enquote('Harry ''Flash'' Gordon', '"')).Equals('"Harry ''Flash'' Gordon"');
  end;


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


  procedure TWIDETests.fn_Unquote;
  begin
    Test.Expect(WIDE.Unquote('''abc''')).Equals('abc');
    Test.Expect(WIDE.Unquote('"abc"')).Equals('abc');
    Test.Expect(WIDE.Unquote('''Harry ''''Flash'''' Gordon''')).Equals('Harry ''Flash'' Gordon');
    Test.Expect(WIDE.Unquote('"Harry ''Flash'' Gordon"')).Equals('Harry ''Flash'' Gordon');
  end;





  procedure TWIDETests.fn_Replace;
  begin
    Test('WIDE.Replace(''Food of the Gods'', ''o'', '''')').Expect(WIDE.Replace('Food of the Gods', 'o', '', [rfReplaceAll])).Equals('Fd f the Gds');
    Test('WIDE.Replace(''Food of the Gods'', ''od'', '''')').Expect(WIDE.Replace('Food of the Gods', 'od', '', [rfReplaceAll])).Equals('Fo of the Gs');
    Test('WIDE.Replace(''Waiting for Godo'', ''o'', '''')').Expect(WIDE.Replace('Waiting for Godo', 'o', '', [rfReplaceAll])).Equals('Waiting fr Gd');

    Test('WIDE.Replace(''Food of the Gods'', ''o'', ''oo'')').Expect(WIDE.Replace('Food of the Gods', 'o', 'oo', [rfReplaceAll])).Equals('Fooood oof the Goods');

    Test('WIDE.Replace(''Food of the Gods'', ''o'', '''')').Expect(WIDE.Replace('Food of the Gods', 'o', '', [])).Equals('Fod of the Gods');
  end;





end.

