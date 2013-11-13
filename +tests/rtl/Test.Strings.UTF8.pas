
  unit Test.Strings.UTF8;

interface

  uses
    Deltics.Smoketest;


  type
    TUTF8Tests = class(TTestCase)
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







end.

