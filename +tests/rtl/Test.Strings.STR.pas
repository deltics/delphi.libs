
  unit Test.Strings.STR;

interface

  uses
    Deltics.Smoketest;


  type
    TSTRTests = class(TTestCase)
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


{ TSTRTests }

  procedure TSTRTests.Transcoding;
  begin
    Test('FromANSI!').Expect(STR.FromANSI(SRCA)).Equals(SRCW);
    Test('FromUTF8!').Expect(STR.FromUTF8(SRCU)).Equals(SRCW);
    Test('FromWide!').Expect(STR.FromWide(SRCW)).Equals(SRCW);
  end;


  procedure TSTRTests.fn_Compare;
  const
    CASES: array[0..8] of TStringAB = (
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
      Test(CASES[i].A + ' < ' + CASES[i].B + '!')
        .Expect(STR.Compare(CASES[i].A, CASES[i].B)).Equals(-1);

    for i := NUM_LT to Pred(NUM_LT + NUM_EQ) do
      Test(CASES[i].A + ' = ' + CASES[i].B + '!')
        .Expect(STR.Compare(CASES[i].A, CASES[i].B)).Equals(0);

    for i := (NUM_LT + NUM_EQ) to Pred(Length(CASES)) do
      Test(CASES[i].A + ' > ' + CASES[i].B + '!')
        .Expect(STR.Compare(CASES[i].A, CASES[i].B)).Equals(1);
  end;


  procedure TSTRTests.fn_IsLowercase;
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

    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A).Expect(STR.IsLowercase(CASES[i].A)).IsFALSE;

    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].B).Expect(STR.IsLowercase(CASES[i].B)).IsTRUE;
  end;


  procedure TSTRTests.fn_IsUppercase;
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

    for i := 0 to Pred(Length(CASES)) do
      Test(CASES[i].A).Expect(STR.IsUppercase(CASES[i].A)).IsFALSE;

    for i := 1 to Pred(Length(CASES)) do
      Test(CASES[i].B).Expect(STR.IsUppercase(CASES[i].B)).IsTRUE;
  end;


  procedure TSTRTests.fn_SameText;
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
      Test(CASES[i].A + ' same text as ' + CASES[i].B + '!')
        .Expect(STR.SameText(CASES[i].A, CASES[i].B)).IsTRUE;
  end;


  procedure TSTRTests.fn_Lowercase;
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


  procedure TSTRTests.fn_Uppercase;
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







end.
