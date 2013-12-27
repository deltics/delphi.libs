
  unit Test.Strings.UTF8;

interface

  uses
    Deltics.Smoketest;


  type
    TUTF8Tests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Pos;
      procedure fn_NPos;
      procedure fn_RPos;
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
  begin
    Test('UTF8 -> ANSI').Expect(ANSI.FromUTF8(UTF8.Encode('™'))).Equals('™');
    Test('UTF8 -> WIDE').Expect(WIDE.FromUTF8(UTF8.Encode('™'))).Equals('™');

    TestUTF8('UTF8.Encode!').Expect(UTF8.Encode(SRCS)).Equals(SRCU);
    Test('UTF8.Decode!').Expect(UTF8.Decode(SRCU)).Equals(SRCS);

    TestUTF8('FromANSI!').Expect(UTF8.FromANSI(SRCA)).Equals(SRCU);
    TestUTF8('FromWide!').Expect(UTF8.FromWide(SRCW)).Equals(SRCU);
  end;


  procedure TUTF8Tests.fn_Pos;
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
    UTF8.Pos(STR, CHAR_T, p);     Test('FirstPos of ''T''').Expect(p).Equals(1);
    UTF8.Pos(STR, CHAR_BANG, p);  Test('FirstPos of ''!''').Expect(p).Equals(21);
    UTF8.Pos(STR, CHAR_Q, p);     Test('FirstPos of ''q''').Expect(p).Equals(5);
    UTF8.Pos(STR, CHAR_Z, p);     Test('FirstPos of ''Z''').Expect(p).Equals(0);

    UTF8.Pos(STR, SUB_THE, p);    Test('FirstPos of ''The''').Expect(p).Equals(1);
    UTF8.Pos(STR, SUB_FOX, p);    Test('FirstPos of ''fox!''').Expect(p).Equals(18);
    UTF8.Pos(STR, SUB_QUICK, p);  Test('FirstPos of ''quick''').Expect(p).Equals(5);
    UTF8.Pos(STR, SUB_BROWN, p);  Test('FirstPos of ''brown''').Expect(p).Equals(0);

    UTF8.Pos(STR, CHAR_T, pa);  Test('2 Positions of ''T''').Expect(Length(pa)).Equals(2).IsRequired;
                                Test('First ''T''').Expect(pa[0]).Equals(1);
                                Test('Second ''T''').Expect(pa[1]).Equals(32);

    UTF8.Pos(STR, CHAR_BANG, pa); Test('2 Positions of ''!''').Expect(Length(pa)).Equals(2).IsRequired;
                                  Test('First ''!''').Expect(pa[0]).Equals(21);
                                  Test('Second ''!''').Expect(pa[1]).Equals(45);

    UTF8.Pos(STR, CHAR_Q, pa);  Test('3 Positions of ''q''').Expect(Length(pa)).Equals(3).IsRequired;
                                Test('First ''q''').Expect(pa[0]).Equals(5);
                                Test('Second ''q''').Expect(pa[1]).Equals(12);
                                Test('Third ''q''').Expect(pa[2]).Equals(36);

    UTF8.Pos(STR, CHAR_Z, pa);  Test('No Positions of ''z''').Expect(Length(pa)).Equals(0);
  end;


  procedure TUTF8Tests.fn_NPos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    p := 0;
    UTF8.NPos(STR, 'T', p);   Test('FirstPos of ''T''').Expect(p).Equals(1);
    UTF8.NPos(STR, 'T', p);   Test('NextPos of ''T''').Expect(p).Equals(32);
    UTF8.NPos(STR, 'T', p);   Test('NextPos of ''T''').Expect(p).Equals(0);

    p := 0;
    UTF8.NPos(STR, '!', p);   Test('FirstPos of ''!''').Expect(p).Equals(21);
    UTF8.NPos(STR, '!', p);   Test('NextPos of ''!''').Expect(p).Equals(45);
    UTF8.NPos(STR, '!', p);   Test('NextPos of ''!''').Expect(p).Equals(0);

    p := 0;
    UTF8.NPos(STR, 'q', p);   Test('FirstPos of ''q''').Expect(p).Equals(5);
    UTF8.NPos(STR, 'q', p);   Test('NextPos of ''q''').Expect(p).Equals(12);
    UTF8.NPos(STR, 'q', p);   Test('NextPos of ''q''').Expect(p).Equals(36);
    UTF8.NPos(STR, 'q', p);   Test('NextPos of ''q''').Expect(p).Equals(0);

    p := 0;
    UTF8.NPos(STR, 'z', p);   Test('FirstPos of ''z''').Expect(p).Equals(0);
    UTF8.NPos(STR, 'z', p);   Test('NextPos of ''z''').Expect(p).Equals(0);
  end;


  procedure TUTF8Tests.fn_RPos;
  const// 0         1         2         3         4
    STR = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
  begin
    UTF8.RPos(STR, 'T', p);       Test('LastPos of ''T''').Expect(p).Equals(32);
    UTF8.RPos(STR, '!', p);       Test('LastPos of ''!''').Expect(p).Equals(45);
    UTF8.RPos(STR, 'q', p);       Test('LastPos of ''q''').Expect(p).Equals(36);
    UTF8.RPos(STR, 'Z', p);       Test('LastPos of ''Z''').Expect(p).Equals(0);

    UTF8.RPos(STR, 'The', p);     Test('LastPos of ''The''').Expect(p).Equals(32);
    UTF8.RPos(STR, 'fox!', p);    Test('LastPos of ''fox!''').Expect(p).Equals(42);
    UTF8.RPos(STR, 'quick', p);   Test('LastPos of ''quick''').Expect(p).Equals(36);
    UTF8.RPos(STR, 'brown', p);   Test('LastPos of ''brown''').Expect(p).Equals(0);
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

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(VECTOR[i].A + ' = ' + VECTOR[i].B + '!')
        .Expect(UTF8.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(0);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A + ' > ' + VECTOR[i].B + '!')
        .Expect(UTF8.Compare(VECTOR[i].A, VECTOR[i].B)).Equals(1);
  end;


  procedure TUTF8Tests.fn_Contains;
  begin
    Test('contains ''T''').Expect(UTF8.Contains(STR, CHAR_T)).IsTRUE;
    Test('contains ''f''').Expect(UTF8.Contains(STR, CHAR_BANG)).IsTRUE;
    Test('contains ''q''').Expect(UTF8.Contains(STR, CHAR_Q)).IsTRUE;
    Test('contains ''Z''').Expect(UTF8.Contains(STR, CHAR_Z)).IsFALSE;

    Test('contains ''The''').Expect(UTF8.Contains(STR, SUB_The)).IsTRUE;
    Test('contains ''fox!''').Expect(UTF8.Contains(STR, SUB_FOX)).IsTRUE;
    Test('contains ''quick''').Expect(UTF8.Contains(STR, SUB_QUICK)).IsTRUE;
    Test('contains ''brown''').Expect(UTF8.Contains(STR, SUB_BROWN)).IsFALSE;
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

    Test('Empty String is NOT considered lowercase!').Expect(UTF8.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].A)).Expect(UTF8.IsLowercase(VECTOR[i].A)).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].B)).Expect(UTF8.IsLowercase(VECTOR[i].B)).IsTRUE;
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

    Test('Empty String is NOT considered uppercase!').Expect(UTF8.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].A)).Expect(UTF8.IsUppercase(VECTOR[i].A)).IsFALSE;

    for i := 1 to Pred(Length(VECTOR)) do
      Test(WIDE.FromUTF8(VECTOR[i].B)).Expect(UTF8.IsUppercase(VECTOR[i].B)).IsTRUE;
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
      TestUTF8.Expect(UTF8.Lowercase(VECTOR[i].A)).Equals(VECTOR[i].B);
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
      TestUTF8.Expect(UTF8.Uppercase(VECTOR[i].A)).Equals(VECTOR[i].B);
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

