
  unit Test.Strings.ANSI;

interface

  uses
    Deltics.Smoketest;


  type
    TANSITests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Pos;
      procedure fn_NPos;
      procedure fn_RPos;
      procedure fn_Compare;
      procedure fn_IsLowercase;
      procedure fn_IsUppercase;
      procedure fn_SameText;
      procedure fn_Lowercase;
      procedure fn_Uppercase;
    end;

    TANSIPerformance = class(TPerformanceCase, ICompareResults)
    private // IComparePerformance
      procedure DefineComparisons(const aCompare: IDefineComparisons);

    published
      procedure SystemPosChar;
      procedure SystemPosStr;
      procedure PosChar;
      procedure NPosChar;
      procedure RPosChar;
      procedure PosStr;
      procedure NPosStr;
      procedure RPosStr;
    end;



implementation

  uses
    Math,
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
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
  var
    p: Integer;
    pa: TCharIndexArray;
  begin
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



  procedure TANSIPerformance.DefineComparisons(const aCompare: IDefineComparisons);
  begin
    aCompare.CompilerVersions;
    aCompare.WithCase('WIDEPerformance');
    aCompare.PreviousResults;
  end;




{ TANSIPerformance }

  procedure TANSIPerformance.SystemPosChar;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  begin
    Pos(FOX, STR);
  end;

  procedure TANSIPerformance.SystemPosStr;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIString = 'fox';
  begin
    Pos(FOX, STR);
  end;

  procedure TANSIPerformance.PosChar;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  var
    p: Integer;
  begin
    ANSI.Pos(STR, FOX, p);
  end;


  procedure TANSIPerformance.NPosChar;
  const               // 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  var
    p: Integer;
  begin
    p := 18;
    ANSI.NPos(STR, FOX, p);
  end;

  procedure TANSIPerformance.RPosChar;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  var
    p: Integer;
  begin
    ANSI.RPos(STR, FOX, p);
  end;

  procedure TANSIPerformance.PosStr;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIString = 'fox';
  var
    p: Integer;
  begin
    ANSI.Pos(STR, FOX, p);
  end;

  procedure TANSIPerformance.NPosStr;
  const               // 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIString = 'fox';
  var
    p: Integer;
  begin
    p := 18;
    ANSI.NPos(STR, FOX, p);
  end;

  procedure TANSIPerformance.RPosStr;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIString = 'fox';
  var
    p: Integer;
  begin
    ANSI.RPos(STR, FOX, p);
  end;


end.
