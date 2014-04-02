
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
      procedure fn_SameText;
      procedure fn_Lowercase;
      procedure fn_Uppercase;
    end;



implementation

  uses
    Math,
    Deltics.Strings,
    Test.Strings;



{ TWIDETests ------------------------------------------------------------------------------------- }

  procedure TWIDETests.Transcoding;
  begin
    Test('WIDE.Encode!').Expect(WIDE.Encode(SRCS)).Equals(SRCW);
    Test('WIDE.Decode!').Expect(WIDE.Decode(SRCW)).Equals(SRCS);

    Test('WIDE.FromANSI!').Expect(WIDE.FromANSI(SRCA)).Equals(SRCW);
    Test('WIDE.FromUTF8!').Expect(WIDE.FromUTF8(SRCU)).Equals(SRCW);
  end;


  procedure TWIDETests.fn_Pos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    WIDE.Pos(STR, 'i', p);      Test('FirstPos of ''i''').Expect(p).Equals(7);
    WIDE.Pos(STR, 'I', p);      Test('FirstPos of ''I''').Expect(p).Equals(24);

    WIDE.Pos(STR, 'T', p);      Test('FirstPos of ''T''').Expect(p).Equals(1);
    WIDE.Pos(STR, '!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    WIDE.Pos(STR, 'q', p);      Test('FirstPos of ''q''').Expect(p).Equals(5);
    WIDE.Pos(STR, 'Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    WIDE.Pos(STR, 'The', p);    Test('FirstPos of ''The''').Expect(p).Equals(1);
    WIDE.Pos(STR, 'fox!', p);   Test('FirstPos of ''fox!''').Expect(p).Equals(18);
    WIDE.Pos(STR, 'quick', p);  Test('FirstPos of ''quick''').Expect(p).Equals(5);
    WIDE.Pos(STR, 'brown', p);  Test('FirstPos of ''brown''').Expect(p).Equals(0);

    WIDE.Pos(STR, 'T', pa); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                            Test('First ''T''').Expect(pa[0]).Equals(1);
                            Test('Second ''T''').Expect(pa[1]).Equals(32);

    WIDE.Pos(STR, '!', pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                            Test('First ''!''').Expect(pa[0]).Equals(21);
                            Test('Second ''!''').Expect(pa[1]).Equals(45);

    WIDE.Pos(STR, 'q', pa); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                            Test('First ''q''').Expect(pa[0]).Equals(5);
                            Test('Second ''q''').Expect(pa[1]).Equals(12);
                            Test('Third ''q''').Expect(pa[2]).Equals(36);

    WIDE.Pos(STR, 'z', pa); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


  procedure TWIDETests.fn_PosText;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    WIDE.PosText(STR, 'i', p);      Test('FirstPos of ''i''').Expect(p).Equals(7);
    WIDE.PosText(STR, 'I', p);      Test('FirstPos of ''I''').Expect(p).Equals(7);

    WIDE.PosText(STR, 't', p);      Test('FirstPos of ''t''').Expect(p).Equals(1);
    WIDE.PosText(STR, '!', p);      Test('FirstPos of ''!''').Expect(p).Equals(21);
    WIDE.PosText(STR, 'Q', p);      Test('FirstPos of ''Q''').Expect(p).Equals(5);
    WIDE.PosText(STR, 'Z', p);      Test('FirstPos of ''Z''').Expect(p).Equals(0);

    WIDE.PosText(STR, 'THE', p);    Test('FirstPos of ''THE''').Expect(p).Equals(1);
    WIDE.PosText(STR, 'FOX!', p);   Test('FirstPos of ''FOX!''').Expect(p).Equals(18);
    WIDE.PosText(STR, 'QUICK', p);  Test('FirstPos of ''QUICK''').Expect(p).Equals(5);
    WIDE.PosText(STR, 'BROWN', p);  Test('FirstPos of ''BROWN''').Expect(p).Equals(0);

    WIDE.PosText(STR, 'T', pa); Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                Test('First ''T''').Expect(pa[0]).Equals(1);
                                Test('Second ''T''').Expect(pa[1]).Equals(32);

    WIDE.PosText(STR, '!', pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                Test('First ''!''').Expect(pa[0]).Equals(21);
                                Test('Second ''!''').Expect(pa[1]).Equals(45);

    WIDE.PosText(STR, 'q', pa); Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                Test('First ''q''').Expect(pa[0]).Equals(5);
                                Test('Second ''q''').Expect(pa[1]).Equals(12);
                                Test('Third ''q''').Expect(pa[2]).Equals(36);

    WIDE.PosText(STR, 'z', pa); Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


  procedure TWIDETests.fn_NPos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    p := 0;
    WIDE.NPos(STR, 'T', p);   Test('FirstPos of ''T''').Expect(p).Equals(1);
    WIDE.NPos(STR, 'T', p);   Test('NextPos of ''T''').Expect(p).Equals(32);
    WIDE.NPos(STR, 'T', p);   Test('NextPos of ''T''').Expect(p).Equals(0);

    p := 0;
    WIDE.NPos(STR, '!', p);   Test('FirstPos of ''!''').Expect(p).Equals(21);
    WIDE.NPos(STR, '!', p);   Test('NextPos of ''!''').Expect(p).Equals(45);
    WIDE.NPos(STR, '!', p);   Test('NextPos of ''!''').Expect(p).Equals(0);

    p := 0;
    WIDE.NPos(STR, 'q', p);   Test('FirstPos of ''q''').Expect(p).Equals(5);
    WIDE.NPos(STR, 'q', p);   Test('NextPos of ''q''').Expect(p).Equals(12);
    WIDE.NPos(STR, 'q', p);   Test('NextPos of ''q''').Expect(p).Equals(36);
    WIDE.NPos(STR, 'q', p);   Test('NextPos of ''q''').Expect(p).Equals(0);

    p := 0;
    WIDE.NPos(STR, 'z', p);   Test('FirstPos of ''z''').Expect(p).Equals(0);
    WIDE.NPos(STR, 'z', p);   Test('NextPos of ''z''').Expect(p).Equals(0);
  end;


  procedure TWIDETests.fn_RPos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    WIDE.RPos(STR, 'T', p);       Test('LastPos of ''T''').Expect(p).Equals(32);
    WIDE.RPos(STR, '!', p);       Test('LastPos of ''!''').Expect(p).Equals(45);
    WIDE.RPos(STR, 'q', p);       Test('LastPos of ''q''').Expect(p).Equals(36);
    WIDE.RPos(STR, 'Z', p);       Test('LastPos of ''Z''').Expect(p).Equals(0);

    WIDE.RPos(STR, 'The', p);     Test('LastPos of ''The''').Expect(p).Equals(32);
    WIDE.RPos(STR, 'fox!', p);    Test('LastPos of ''fox!''').Expect(p).Equals(42);
    WIDE.RPos(STR, 'quick', p);   Test('LastPos of ''quick''').Expect(p).Equals(36);
    WIDE.RPos(STR, 'brown', p);   Test('LastPos of ''brown''').Expect(p).Equals(0);
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

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(VECTOR[i].A + ' = ' + VECTOR[i].B + '!')
        .Expect(WIDE.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(0);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A + ' > ' + VECTOR[i].B + '!')
        .Expect(WIDE.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(1);
  end;


  procedure TWIDETests.fn_Contains;
  const
    STR: UnicodeString = 'The quick fox!';
  begin
    Test('contains ''T''').Expect(WIDE.Contains(STR, 'T')).IsTRUE;
    Test('contains ''f''').Expect(WIDE.Contains(STR, '!')).IsTRUE;
    Test('contains ''q''').Expect(WIDE.Contains(STR, 'q')).IsTRUE;
    Test('contains ''Z''').Expect(WIDE.Contains(STR, 'Z')).IsFALSE;

    Test('contains ''The''').Expect(WIDE.Contains(STR, 'The')).IsTRUE;
    Test('contains ''fox!''').Expect(WIDE.Contains(STR, 'fox!')).IsTRUE;
    Test('contains ''quick''').Expect(WIDE.Contains(STR, 'quick')).IsTRUE;
    Test('contains ''brown''').Expect(WIDE.Contains(STR, 'brown')).IsFALSE;
  end;


  procedure TWIDETests.fn_ContainsText;
  const
    STR: UnicodeString = 'The quick fox!';
  begin
    Test('contains ''t''').Expect(WIDE.ContainsText(STR, 't')).IsTRUE;
    Test('contains ''!''').Expect(WIDE.ContainsText(STR, '!')).IsTRUE;
    Test('contains ''Q''').Expect(WIDE.ContainsText(STR, 'Q')).IsTRUE;
    Test('contains ''Z''').Expect(WIDE.ContainsText(STR, 'Z')).IsFALSE;

    Test('contains ''the''').Expect(WIDE.ContainsText(STR, 'the')).IsTRUE;
    Test('contains ''Fox!''').Expect(WIDE.ContainsText(STR, 'Fox!')).IsTRUE;
    Test('contains ''QUICK''').Expect(WIDE.ContainsText(STR, 'QUICK')).IsTRUE;
    Test('contains ''brown''').Expect(WIDE.ContainsText(STR, 'brown')).IsFALSE;
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
    Note('Tests for IsLowercase(UnicodeString)...');

    Test('Empty String is NOT considered lowercase!').Expect(WIDE.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].A).Expect(WIDE.IsLowercase(STR_VECTOR[i].A)).IsFALSE;

    for i := 0 to Pred(Length(STR_VECTOR)) do
      Test(STR_VECTOR[i].B).Expect(WIDE.IsLowercase(STR_VECTOR[i].B)).IsTRUE;

    Note('Tests for IsLowercase(WideChar)...');

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].A).Expect(WIDE.IsLowercase(CHAR_VECTOR[i].A)).IsFALSE;

    for i := 0 to Pred(Length(CHAR_VECTOR)) do
      Test(CHAR_VECTOR[i].B).Expect(WIDE.IsLowercase(CHAR_VECTOR[i].B)).Equals(i = 1);
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
                                            (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                           );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A + ' same text as ' + VECTOR[i].B + '!')
        .Expect(WIDE.SameText(VECTOR[i].A, VECTOR[i].B)).IsTRUE;
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
    Test('Split('''', ''*'')').Expect(WIDE.Split('', STAR, left, right)).IsFALSE;
    Test('')['left'].Expect(left).Equals('');
    Test('')['right'].Expect(right).Equals('');

    Test('Split(''left'', ''*'')').Expect(WIDE.Split('left', STAR, left, right)).IsFALSE;
    Test('left')['left'].Expect(left).Equals('left');
    Test('left')['right'].Expect(right).Equals('');

    Test('Split(''*right'', ''*'')').Expect(WIDE.Split('*right', STAR, left, right)).IsTRUE;
    Test('*right')['left'].Expect(left).Equals('');
    Test('*right')['right'].Expect(right).Equals('right');

    Test('Split(''left*right'', ''*'')').Expect(WIDE.Split('left*right', STAR, left, right)).IsTRUE;
    Test('left*right')['left'].Expect(left).Equals('left');
    Test('left*right')['right'].Expect(right).Equals('right');

    s := 'left*mid-left*middle*mid-right*right';
    Test('Split(''%s'', ''*'')', [s]).Expect(WIDE.Split(s, STAR, parts)).IsTRUE;
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







end.

