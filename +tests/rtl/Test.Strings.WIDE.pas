
  unit Test.Strings.WIDE;

interface

  uses
    Deltics.Smoketest;


  type
    TWIDETests = class(TTestCase)
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



{ TWIDETests ------------------------------------------------------------------------------------- }

  procedure TWIDETests.Transcoding;
  begin
    Test('WIDE.Encode!').Expect(WIDE.Encode(SRCS)).Equals(SRCW);
    Test('WIDE.Decode!').Expect(WIDE.Decode(SRCW)).Equals(SRCS);

    Test('WIDE.FromANSI!').Expect(WIDE.FromANSI(SRCA)).Equals(SRCW);
    Test('WIDE.FromUTF8!').Expect(WIDE.FromUTF8(SRCU)).Equals(SRCW);
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


  procedure TWIDETests.fn_IsLowercase;
  const
    VECTOR: array[0..2] of TWIDEStringAB = (
                                            (A: 'LowerCase';           B: 'lowercase'),
                                            (A: '*NOT UPPERCASE*';     B: '*not uppercase*'),
                                            (A: 'Microsoft Windows™';  B: 'microsoft windows™')
                                           );
  var
    i: Integer;
  begin
    Test('Empty String is NOT considered lowercase!').Expect(WIDE.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A).Expect(WIDE.IsLowercase(VECTOR[i].A)).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(VECTOR[i].B).Expect(WIDE.IsLowercase(VECTOR[i].B)).IsTRUE;
  end;


  procedure TWIDETests.fn_IsUppercase;
  const
    VECTOR: array[0..2] of TWIDEStringAB = (
                                            (A: 'UpperCase';           B: 'UPPERCASE'),
                                            (A: '*NOT lowercase*';     B: '*NOT LOWERCASE*'),
                                            (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                           );
  var
    i: Integer;
  begin
    Test('Empty String is NOT considered uppercase!').Expect(WIDE.IsLowercase('')).IsFALSE;

    for i := 0 to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A).Expect(WIDE.IsUppercase(VECTOR[i].A)).IsFALSE;

    for i := 1 to Pred(Length(VECTOR)) do
      Test(VECTOR[i].B).Expect(WIDE.IsUppercase(VECTOR[i].B)).IsTRUE;
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


  procedure TWIDETests.fn_Lowercase;
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
      Test(VECTOR[i].A).Expect(WIDE.Lowercase(VECTOR[i].A)).Equals(VECTOR[i].B);
  end;


  procedure TWIDETests.fn_Uppercase;
  const
    VECTOR: array[0..3] of TWIDEStringAB = (
                                            (A: '';                    B: ''),
                                            (A: 'UpperCase';           B: 'UPPERCASE'),
                                            (A: '*NOT LOWERCASE*';     B: '*NOT LOWERCASE*'),
                                            (A: 'Microsoft Windows™';  B: 'MICROSOFT WINDOWS™')
                                           );
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(VECTOR)) do
      Test(VECTOR[i].A).Expect(WIDE.Uppercase(VECTOR[i].A)).Equals(VECTOR[i].B);
  end;







end.

