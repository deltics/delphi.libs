
  unit Test.Strings.UTF8;

interface

  uses
    Deltics.Smoketest;


  type
    TUTF8Tests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Find;
      procedure fn_FindFirst;
      procedure fn_FindNext;
      procedure fn_FindLast;
      procedure fn_Compare;
      procedure fn_Contains;
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


  var
    STR: UTF8String;
    SUB_THE: UTF8String;
    SUB_FOX: UTF8String;
    SUB_QUICK: UTF8String;
    SUB_BROWN: UTF8String;
    CHAR_T: UTF8Char;
    CHAR_BANG: UTF8Char;
    CHAR_Q: UTF8Char;
    CHAR_Z: UTF8Char;

  type
    TVECTORS = array of TUTF8StringAB;


  procedure PrepareVectors(const SRC: array of TWIDEStringAB; var VEC: TVECTORS);
  var
    i: Integer;
  begin
    SetLength(VEC, Length(SRC));
    for i := 0 to Pred(Length(SRC)) do
    begin
      VEC[i].A := UTF8.FromWide(SRC[i].A);
      VEC[i].B := UTF8.FromWide(SRC[i].B);
    end;
  end;




{ TUTF8Tests ------------------------------------------------------------------------------------- }

  procedure TUTF8Tests.Transcoding;
  const
    ENCODED: array [0..2] of Byte = (Byte('â'), Byte('„'), Byte('¢'));
  var
    tm: UTF8String;
  begin
    tm := UTF8.Encode('™');

    Test('UTF8.Encode(''™'')').Expect(@tm[1]).Equals(@ENCODED[0], 3);

    Test('ANSI.FromUTF8()').Expect(ANSI.FromUTF8(tm)).Equals('™');
    Test('WIDE.FromUTF8()').Expect(WIDE.FromUTF8(tm)).Equals('™');

    TestUTF8('UTF8.Encode!').Expect(UTF8.Encode(SRCS)).Equals(SRCU);
    Test('UTF8.Decode!').Expect(UTF8.Decode(SRCU)).Equals(SRCS);
    Test('STR.FromUTF8!').Expect(Deltics.Strings.STR.FromUTF8(SRCU)).Equals(SRCS);

    TestUTF8('FromANSI!').Expect(UTF8.FromANSI(SRCA)).Equals(SRCU);
    TestUTF8('FromWide!').Expect(UTF8.FromWide(SRCW)).Equals(SRCU);
  end;


  procedure TUTF8Tests.fn_Find;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    s: IUTF8String;
    pa: TCharIndexArray;
  begin
    s := UTF8FromSTR(STR);
    s.Find(UTF8.Encode('t'), pa);   Test('No positions of ''t''').Expect(Length(pa)).Equals(0);
    s.Find(UTF8.Encode('T'), pa);   Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                    Test('First ''T''').Expect(pa[0]).Equals(1);
                                    Test('Second ''T''').Expect(pa[1]).Equals(32);

    s.Find(UTF8.Encode('!'), pa);   Test('2 positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                    Test('First ''!''').Expect(pa[0]).Equals(21);
                                    Test('Second ''!''').Expect(pa[1]).Equals(45);

    s.Find(UTF8.Encode('q'), pa);   Test('3 positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                    Test('First ''q''').Expect(pa[0]).Equals(5);
                                    Test('Second ''q''').Expect(pa[1]).Equals(12);
                                    Test('Third ''q''').Expect(pa[2]).Equals(36);

    s.Find(UTF8.Encode('z'), pa);   Test('No positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


  procedure TUTF8Tests.fn_FindFirst;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    s: IUTF8String;
    p: Integer;
  begin
    s := UTF8FromSTR(STR);
    s.FindFirst(UTF8.Encode('t'), p);    Test('FirstPos of ''t''').Expect(p).Equals(0);
    s.FindFirst(UTF8.Encode('T'), p);    Test('FirstPos of ''T''').Expect(p).Equals(1);
    s.FindFirst(UTF8.Encode('!'), p);    Test('FirstPos of ''!''').Expect(p).Equals(21);
    s.FindFirst(UTF8.Encode('q'), p);    Test('FirstPos of ''q''').Expect(p).Equals(5);
    s.FindFirst(UTF8.Encode('Z'), p);    Test('FirstPos of ''Z''').Expect(p).Equals(0);

    s.FindFirst(UTF8.Encode('the'), p);    Test('FirstPos of ''the''').Expect(p).Equals(0);
    s.FindFirst(UTF8.Encode('fox'), p);    Test('FirstPos of ''fox!''').Expect(p).Equals(18);
    s.FindFirst(UTF8.Encode('quick'), p);  Test('FirstPos of ''quick''').Expect(p).Equals(5);
    s.FindFirst(UTF8.Encode('brown'), p);  Test('FirstPos of ''brown''').Expect(p).Equals(0);
  end;


  procedure TUTF8Tests.fn_FindNext;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    UTF8(STR).FindFirst('T', p);  Test('FirstPos of ''T''').Expect(p).Equals(1);
    UTF8(STR).FindNext('T', p);   Test('NextPos of ''T''').Expect(p).Equals(32);
    UTF8(STR).FindNext('T', p);   Test('NextPos of ''T''').Expect(p).Equals(0);

    UTF8(STR).FindFirst('!', p);  Test('FirstPos of ''!''').Expect(p).Equals(21);
    UTF8(STR).FindNext('!', p);   Test('NextPos of ''!''').Expect(p).Equals(45);
    UTF8(STR).FindNext('!', p);   Test('NextPos of ''!''').Expect(p).Equals(0);

    UTF8(STR).FindFirst('q', p);  Test('FirstPos of ''q''').Expect(p).Equals(5);
    UTF8(STR).FindNext('q', p);   Test('NextPos of ''q''').Expect(p).Equals(12);
    UTF8(STR).FindNext('q', p);   Test('NextPos of ''q''').Expect(p).Equals(36);
    UTF8(STR).FindNext('q', p);   Test('NextPos of ''q''').Expect(p).Equals(0);

    UTF8(STR).FindFirst('z', p);  Test('FirstPos of ''z''').Expect(p).Equals(0);
    UTF8(STR).FindNext('z', p);   Test('NextPos of ''z''').Expect(p).Equals(0);
  end;


  procedure TUTF8Tests.fn_FindLast;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    UTF8(STR).FindLast('T', p);       Test('LastPos of ''T''').Expect(p).Equals(32);
    UTF8(STR).FindLast('!', p);       Test('LastPos of ''!''').Expect(p).Equals(45);
    UTF8(STR).FindLast('q', p);       Test('LastPos of ''q''').Expect(p).Equals(36);
    UTF8(STR).FindLast('Z', p);       Test('LastPos of ''Z''').Expect(p).Equals(0);

    UTF8(STR).FindLast('The', p);     Test('LastPos of ''The''').Expect(p).Equals(32);
    UTF8(STR).FindLast('fox!', p);    Test('LastPos of ''fox!''').Expect(p).Equals(42);
    UTF8(STR).FindLast('quick', p);   Test('LastPos of ''quick''').Expect(p).Equals(36);
    UTF8(STR).FindLast('brown', p);   Test('LastPos of ''brown''').Expect(p).Equals(0);
  end;


  procedure TUTF8Tests.fn_Compare;
  const
    SRC: array[0..8] of TWIDEStringAB = (
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
    VECTOR: TVECTORS;
    i: Integer;
  begin
    PrepareVectors(SRC, VECTOR);

    for i := 0 to Pred(NUM_LT) do
      Test(VECTOR[i].A + ' < ' + VECTOR[i].B + '!')
        .Expect(UTF8.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(-1);

    for i := 0 to Pred(NUM_LT) do
      Test(VECTOR[i].A + ' < ' + VECTOR[i].B + '!')
        .Expect(UTF8(VECTOR[i].A).CompareWith(VECTOR[i].B)).Equals(-1);

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(VECTOR[i].A + ' = ' + VECTOR[i].B + '!')
        .Expect(UTF8.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(0);

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(VECTOR[i].A + ' = ' + VECTOR[i].B + '!')
        .Expect(UTF8(VECTOR[i].A).CompareWith(VECTOR[i].B)).Equals(0);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A + ' > ' + VECTOR[i].B + '!')
        .Expect(UTF8.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(1);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A + ' > ' + VECTOR[i].B + '!')
        .Expect(UTF8(VECTOR[i].A).CompareWith(VECTOR[i].B)).Equals(1);
  end;


  procedure TUTF8Tests.fn_Contains;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  begin
    Test('contains ''T''').Expect(UTF8(STR).Contains(CHAR_T)).IsTRUE;
    Test('contains ''f''').Expect(UTF8(STR).Contains(CHAR_BANG)).IsTRUE;
    Test('contains ''q''').Expect(UTF8(STR).Contains(CHAR_Q)).IsTRUE;
    Test('contains ''Z''').Expect(UTF8(STR).Contains(CHAR_Z)).IsFALSE;

    Test('contains ''The''').Expect(UTF8(STR).Contains(SUB_The)).IsTRUE;
    Test('contains ''fox!''').Expect(UTF8(STR).Contains(SUB_FOX)).IsTRUE;
    Test('contains ''quick''').Expect(UTF8(STR).Contains(SUB_QUICK)).IsTRUE;
    Test('contains ''brown''').Expect(UTF8(STR).Contains(SUB_BROWN)).IsFALSE;
  end;


  procedure TUTF8Tests.fn_IsLowercase;
  const
    SRC: array[0..2] of TWIDEStringAB = (
                                         (A: 'LowerCase';           B: 'lowercase'),
                                         (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                         (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                        );
  var
    VECTOR: TVECTORS;
    i: Integer;
  begin
    PrepareVectors(SRC, VECTOR);

    Test('Empty String is NOT considered lowercase!').Expect(UTF8('').IsLowercase).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].A)).Expect(UTF8(VECTOR[i].A).IsLowercase).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].B)).Expect(UTF8(VECTOR[i].B).IsLowercase).IsTRUE;
  end;


  procedure TUTF8Tests.fn_IsUppercase;
  const
    SRC: array[0..2] of TWIDEStringAB = (
                                         (A: 'UpperCase';           B: 'UPPERCASE'),
                                         (A: '*NOT lowercase*';     B: '*NOT LOWERCASE*'),
                                         (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                        );
  var
    VECTOR: TVECTORS;
    i: Integer;
  begin
    PrepareVectors(SRC, VECTOR);

    Test('Empty String is NOT considered uppercase!').Expect(UTF8('').IsUppercase).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].A)).Expect(UTF8(VECTOR[i].A).IsUppercase).IsFALSE;

    for i := 1 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].B)).Expect(UTF8(VECTOR[i].B).IsUppercase).IsTRUE;

//      Test(WIDE.FromUTF8(VECTOR[i].B)).Expect(UTF8(VECTOR[i].B).IsUppercase).IsTRUE;
  end;


  procedure TUTF8Tests.fn_SameText;
  const
    SRC: array[0..3] of TWIDEStringAB = (
                                         (A: '';                    B: ''),
                                         (A: 'LowerCase';           B: 'lowercase'),
                                         (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                         (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                        );
  var
    VECTOR: TVECTORS;
    i: Integer;
  begin
    PrepareVectors(SRC, VECTOR);

    for i := 0 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].A + ' same text as ' + VECTOR[i].B + '!'))
        .Expect(UTF8.SameText(VECTOR[i].A, VECTOR[i].B)).IsTRUE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].A + ' same text as ' + VECTOR[i].B + '!'))
        .Expect(UTF8(VECTOR[i].A).EqualsText(VECTOR[i].B)).IsTRUE;
  end;


  procedure TUTF8Tests.fn_Lowercase;
  const
    SRC: array[0..3] of TWIDEStringAB = (
                                         (A: '';                    B: ''),
                                         (A: 'LowerCase';           B: 'lowercase'),
                                         (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                         (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                        );
  var
    VECTOR: TVECTORS;
    i: Integer;
  begin
    PrepareVectors(SRC, VECTOR);

    for i := 0 to Pred(Length(VECTOR)) do
      TestUTF8.Expect(UTF8(VECTOR[i].A).Lowercase).Equals(VECTOR[i].B);
  end;


  procedure TUTF8Tests.fn_Uppercase;
  const
    SRC: array[0..3] of TWIDEStringAB = (
                                       (A: '';                    B: ''),
                                       (A: 'UpperCase';           B: 'UPPERCASE'),
                                       (A: '*NOT LOWERCASE*';     B: '*NOT LOWERCASE*'),
                                       (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                      );
  var
    VECTOR: TVECTORS;
    i: Integer;
  begin
    PrepareVectors(SRC, VECTOR);

    for i := 0 to Pred(Length(VECTOR)) do
      TestUTF8.Expect(UTF8(VECTOR[i].A).Uppercase).Equals(VECTOR[i].B);
  end;




initialization
  STR := UTF8.Encode('The quick, quick fox!  I said: The quick fox!');

  SUB_THE     := UTF8.Encode('The');
  SUB_FOX     := UTF8.Encode('fox!');
  SUB_QUICK   := UTF8.Encode('quick');
  SUB_BROWN   := UTF8.Encode('brown');

  CHAR_T      := UTF8.Encode('T')[1];
  CHAR_BANG   := UTF8.Encode('!')[1];
  CHAR_q      := UTF8.Encode('q')[1];
  CHAR_Z      := UTF8.Encode('Z')[1];

end.

