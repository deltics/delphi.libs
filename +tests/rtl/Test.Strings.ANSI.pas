
  unit Test.Strings.ANSI;

interface

  uses
    Deltics.Smoketest;


  type
    TANSITests = class(TTestCase)
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
      procedure fn_SameText;
      procedure fn_Lowercase;
      procedure fn_Uppercase;
    end;



implementation

  uses
    Math,
    Windows,
    Deltics.Strings,
    Test.Strings;




{ TANSITests ------------------------------------------------------------------------------------- }

  procedure TANSITests.Transcoding;
  begin
    Test('ANSI.Encode!').Expect(ANSI.Encode(SRCS)).Equals(SRCA);
    Test('ANSI.Decode!').Expect(ANSI.Decode(SRCA)).Equals(SRCS);

    Test('FromUTF8!').Expect(ANSI.FromUTF8(SRCU)).Equals(SRCA);
    Test('FromWide!').Expect(ANSI.FromWide(SRCW)).Equals(SRCA);
  end;


  procedure TANSITests.fn_Pos;
  const            // 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!™';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    ANSI.Pos(STR, '™', p);      Test('FirstPos of ''™''').Expect(p).Equals(46);

    ANSI.Pos(STR, 'T', p);      Test('FirstPos of ''T''').Expect(p).Equals(1);
    ANSI.Pos(STR, '!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    ANSI.Pos(STR, 'q', p);      Test('FirstPos of ''q''').Expect(p).Equals(5);
    ANSI.Pos(STR, 'Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    ANSI.Pos(STR, 'The', p);    Test('FirstPos of ''The''').Expect(p).Equals(1);
    ANSI.Pos(STR, 'fox!', p);   Test('FirstPos of ''fox!''').Expect(p).Equals(18);
    ANSI.Pos(STR, 'quick', p);  Test('FirstPos of ''quick''').Expect(p).Equals(5);
    ANSI.Pos(STR, 'brown', p);  Test('FirstPos of ''brown''').Expect(p).Equals(0);

    ANSI.Pos(STR, 'T', pa); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                    Test('First ''T''').Expect(pa[0]).Equals(1);
                                    Test('Second ''T''').Expect(pa[1]).Equals(32);

    ANSI.Pos(STR, '!', pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                    Test('First ''!''').Expect(pa[0]).Equals(21);
                                    Test('Second ''!''').Expect(pa[1]).Equals(45);

    ANSI.Pos(STR, 'q', pa); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                    Test('First ''q''').Expect(pa[0]).Equals(5);
                                    Test('Second ''q''').Expect(pa[1]).Equals(12);
                                    Test('Third ''q''').Expect(pa[2]).Equals(36);

    ANSI.Pos(STR, 'z', pa); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


  procedure TANSITests.fn_NPos;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    ANSI.Pos(STR, 'T', p);      Test('FirstPos of ''T''').Expect(p).Equals(1);
    ANSI.NPos(STR, 'T', p);       Test('NextPos of ''T''').Expect(p).Equals(32);
    ANSI.NPos(STR, 'T', p);       Test('NextPos of ''T''').Expect(p).Equals(0);

    ANSI.Pos(STR, '!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    ANSI.NPos(STR, '!', p);       Test('NextPos of ''!''').Expect(p).Equals(45);
    ANSI.NPos(STR, '!', p);       Test('NextPos of ''!''').Expect(p).Equals(0);

    ANSI.Pos(STR, 'q', p);      Test('FirstPos of ''q''').Expect(p).Equals(5);
    ANSI.NPos(STR, 'q', p);       Test('NextPos of ''q''').Expect(p).Equals(12);
    ANSI.NPos(STR, 'q', p);       Test('NextPos of ''q''').Expect(p).Equals(36);
    ANSI.NPos(STR, 'q', p);       Test('NextPos of ''q''').Expect(p).Equals(0);

    ANSI.Pos(STR, 'z', p);      Test('FirstPos of ''z''').Expect(p).Equals(0);
    ANSI.NPos(STR, 'z', p);       Test('NextPos of ''z''').Expect(p).Equals(0);
  end;


  procedure TANSITests.fn_RPos;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    ANSI.RPos(STR, 'T', p);       Test('LastPos of ''T''').Expect(p).Equals(32);
    ANSI.RPos(STR, '!', p);       Test('LastPos of ''!''').Expect(p).Equals(45);
    ANSI.RPos(STR, 'q', p);       Test('LastPos of ''q''').Expect(p).Equals(36);
    ANSI.RPos(STR, 'Z', p);       Test('LastPos of ''Z''').Expect(p).Equals(0);

    ANSI.RPos(STR, 'The', p);     Test('LastPos of ''The''').Expect(p).Equals(32);
    ANSI.RPos(STR, 'fox!', p);    Test('LastPos of ''fox!''').Expect(p).Equals(42);
    ANSI.RPos(STR, 'quick', p);   Test('LastPos of ''quick''').Expect(p).Equals(36);
    ANSI.RPos(STR, 'brown', p);   Test('LastPos of ''brown''').Expect(p).Equals(0);
  end;


  procedure TANSITests.fn_Split;
  const
    STAR: ANSIChar = '*';
  var
    s: ANSIString;
    left, right: ANSIString;
    parts: TANSIStringArray;
  begin
    Test('Split('''', ''*'')').Expect(ANSI.Split('', STAR, left, right)).IsFALSE;
    Test('')['left'].Expect(left).Equals('');
    Test('')['right'].Expect(right).Equals('');

    Test('Split(''left'', ''*'')').Expect(ANSI.Split('left', STAR, left, right)).IsFALSE;
    Test('left')['left'].Expect(left).Equals('left');
    Test('left')['right'].Expect(right).Equals('');

    Test('Split(''*right'', ''*'')').Expect(ANSI.Split('*right', STAR, left, right)).IsTRUE;
    Test('*right')['left'].Expect(left).Equals('');
    Test('*right')['right'].Expect(right).Equals('right');

    Test('Split(''left*right'', ''*'')').Expect(ANSI.Split('left*right', STAR, left, right)).IsTRUE;
    Test('left*right')['left'].Expect(left).Equals('left');
    Test('left*right')['right'].Expect(right).Equals('right');

    s := 'left*mid-left*middle*mid-right*right';
    Test('Split(''%s'', ''*'')', [s]).Expect(ANSI.Split(s, STAR, parts)).IsTRUE;
    Test('Split(''%s'', ''*'')', [s])['no. of parts'].Expect(Length(parts)).Equals(5);
    Test('part')[0].Expect(parts[0]).Equals('left');
    Test('part')[1].Expect(parts[1]).Equals('mid-left');
    Test('part')[2].Expect(parts[2]).Equals('middle');
    Test('part')[3].Expect(parts[3]).Equals('mid-right');
    Test('part')[4].Expect(parts[4]).Equals('right');
  end;


  procedure TANSITests.fn_PosText;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    ANSI.PosText(STR, 'i', p);      Test('FirstPos of ''i''').Expect(p).Equals(7);
    ANSI.PosText(STR, 'I', p);      Test('FirstPos of ''I''').Expect(p).Equals(7);

    ANSI.PosText(STR, 't', p);      Test('FirstPos of ''t''').Expect(p).Equals(1);
    ANSI.PosText(STR, '!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    ANSI.PosText(STR, 'Q', p);      Test('FirstPos of ''Q''').Expect(p).Equals(5);
    ANSI.PosText(STR, 'Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    ANSI.PosText(STR, 'THE', p);    Test('FirstPos of ''THE''').Expect(p).Equals(1);
    ANSI.PosText(STR, 'FOX!', p);   Test('FirstPos of ''FOX!''').Expect(p).Equals(18);
    ANSI.PosText(STR, 'QUICK', p);  Test('FirstPos of ''QUICK''').Expect(p).Equals(5);
    ANSI.PosText(STR, 'BROWN', p);  Test('FirstPos of ''BROWN''').Expect(p).Equals(0);

    ANSI.PosText(STR, 'T', pa); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                Test('First ''T''').Expect(pa[0]).Equals(1);
                                Test('Second ''T''').Expect(pa[1]).Equals(32);

    ANSI.PosText(STR, '!', pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                Test('First ''!''').Expect(pa[0]).Equals(21);
                                Test('Second ''!''').Expect(pa[1]).Equals(45);

    ANSI.PosText(STR, 'q', pa); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                Test('First ''q''').Expect(pa[0]).Equals(5);
                                Test('Second ''q''').Expect(pa[1]).Equals(12);
                                Test('Third ''q''').Expect(pa[2]).Equals(36);

    ANSI.PosText(STR, 'z', pa); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
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
        .Expect(ANSI.Compare(CASES[i].A, CASES[i].B)).Equals(-1);

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(WIDE.FromANSI(CASES[i].A + ' = ' + CASES[i].B + '!'))
        .Expect(ANSI.Compare(CASES[i].A, CASES[i].B)).Equals(0);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(CASES)) do
      Test(WIDE.FromANSI(CASES[i].A + ' > ' + CASES[i].B + '!'))
        .Expect(ANSI.Compare(CASES[i].A, CASES[i].B)).Equals(1);
  end;


  procedure TANSITests.fn_Contains;
  const
    STR: ANSIString = 'The quick fox!';
  begin
    Test('contains ''T''').Expect(ANSI.Contains(STR, 'T')).IsTRUE;
    Test('contains ''t''').Expect(ANSI.Contains(STR, 't')).IsFALSE;
    Test('contains ''!''').Expect(ANSI.Contains(STR, '!')).IsTRUE;
    Test('contains ''Q''').Expect(ANSI.Contains(STR, 'Q')).IsFALSE;
    Test('contains ''q''').Expect(ANSI.Contains(STR, 'q')).IsTRUE;
    Test('contains ''Z''').Expect(ANSI.Contains(STR, 'Z')).IsFALSE;

    Test('contains ''The''').Expect(ANSI.Contains(STR, 'The')).IsTRUE;
    Test('contains ''fox!''').Expect(ANSI.Contains(STR, 'fox!')).IsTRUE;
    Test('contains ''quick''').Expect(ANSI.Contains(STR, 'quick')).IsTRUE;
    Test('contains ''brown''').Expect(ANSI.Contains(STR, 'brown')).IsFALSE;
  end;


  procedure TANSITests.fn_ContainsText;
  const
    STR: ANSIString = 'The quick fox!';
  begin
    Test('contains ''T''').Expect(ANSI.ContainsText(STR, 'T')).IsTRUE;
    Test('contains ''t''').Expect(ANSI.ContainsText(STR, 't')).IsTRUE;
    Test('contains ''!''').Expect(ANSI.ContainsText(STR, '!')).IsTRUE;
    Test('contains ''Q''').Expect(ANSI.ContainsText(STR, 'Q')).IsTRUE;
    Test('contains ''q''').Expect(ANSI.ContainsText(STR, 'q')).IsTRUE;
    Test('contains ''Z''').Expect(ANSI.ContainsText(STR, 'Z')).IsFALSE;

    Test('contains ''the''').Expect(ANSI.ContainsText(STR, 'the')).IsTRUE;
    Test('contains ''Fox!''').Expect(ANSI.ContainsText(STR, 'Fox!')).IsTRUE;
    Test('contains ''QUICK''').Expect(ANSI.ContainsText(STR, 'QUICK')).IsTRUE;
    Test('contains ''brown''').Expect(ANSI.ContainsText(STR, 'brown')).IsFALSE;
  end;


  procedure TANSITests.fn_IsLowercase;
  const
    CASES: array[0..2] of TANSIStringAB = (
                                       (A: 'LowerCase';           B: 'lowercase'),
                                       (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                       (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                      );
  var
    i: Integer;
  begin
    Test('Empty String is NOT considered lowercase!').Expect(ANSI.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A).Expect(ANSI.IsLowercase(CASES[i].A)).IsFALSE;

    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].B).Expect(ANSI.IsLowercase(CASES[i].B)).IsTRUE;
  end;


  procedure TANSITests.fn_IsUppercase;
  const
    CASES: array[0..2] of TANSIStringAB = (
                                       (A: 'UpperCase';           B: 'UPPERCASE'),
                                       (A: '*NOT lowercase*';     B: '*NOT LOWERCASE*'),
                                       (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                      );
  var
    i: Integer;
  begin
    Test('Empty String is NOT considered uppercase!').Expect(ANSI.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A).Expect(ANSI.IsUppercase(CASES[i].A)).IsFALSE;

    for i := 1 to Pred(Length(CASES)) do
      Test(CASES[i].B).Expect(ANSI.IsUppercase(CASES[i].B)).IsTRUE;
  end;


  procedure TANSITests.fn_SameText;
  const
    CASES: array[0..3] of TANSIStringAB = (
                                       (A: '';                    B: ''),
                                       (A: 'LowerCase';           B: 'lowercase'),
                                       (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                       (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                      );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A + ' same text as ' + CASES[i].B + '!')
        .Expect(ANSI.SameText(CASES[i].A, CASES[i].B)).IsTRUE;
  end;


  procedure TANSITests.fn_Len;
  var
    s: ANSIString;
  begin
    s := #0;
    Test('Zero length').Expect(ANSI.Len(PANSIChar(s))).Equals(0);

    s := 'short';
    Test('Non-zero length').Expect(ANSI.Len(PANSIChar(s))).Equals(5);

    s := 'Windows™';
    Test('With MBCS Char').Expect(ANSI.Len(PANSIChar(s))).Equals(8);
  end;


  procedure TANSITests.fn_Lowercase;
  const
    CASES: array[0..3] of TANSIStringAB = (
                                       (A: '';                    B: ''),
                                       (A: 'LowerCase';           B: 'lowercase'),
                                       (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                       (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                      );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A).Expect(ANSI.Lowercase(CASES[i].A)).Equals(CASES[i].B);
  end;


  procedure TANSITests.fn_Uppercase;
  const
    CASES: array[0..3] of TANSIStringAB = (
                                       (A: '';                    B: ''),
                                       (A: 'UpperCase';           B: 'UPPERCASE'),
                                       (A: '*NOT LOWERCASE*';     B: '*NOT LOWERCASE*'),
                                       (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                      );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A).Expect(ANSI.Uppercase(CASES[i].A)).Equals(CASES[i].B);
  end;






end.
