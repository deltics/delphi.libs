
  unit Test.Strings.ANSI;

interface

  uses
    Deltics.Smoketest;


  type
    TANSITests = class(TTestCase)
      procedure Transcoding;
      procedure fn_Compare;
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


{ TANSITests ------------------------------------------------------------------------------------- }

  procedure TANSITests.Transcoding;
  begin
    Test('ANSI.Encode!').Expect(ANSI.Encode(SRCS)).Equals(SRCA);
    Test('ANSI.Decode!').Expect(ANSI.Decode(SRCA)).Equals(SRCS);

    Test('FromUTF8!').Expect(ANSI.FromUTF8(SRCU)).Equals(SRCA);
    Test('FromWide!').Expect(ANSI.FromWide(SRCW)).Equals(SRCA);
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







end.
