

  unit SelfTest;


interface

  uses
    Deltics.Smoketest;


  type
    TSelfTest = class(TTestCase)
      procedure FailedSelfTests;
      procedure DynamicTestExecution;
      procedure DisabledTests;
      procedure BooleanTests;
      procedure BooleanTests_ShouldFail;
      procedure DoubleTests_Precise;
      procedure DoubleTests_Precise_ShouldFail;
      procedure DoubleTests_To2DPs;
      procedure DoubleTests_To2DPs_ShouldFail;
      procedure EnumTests;
      procedure IntegerTests;
      procedure IntegerTests_ShouldFail;
      procedure StringTests;
      procedure StringTests_ShouldFail;
      procedure StringTests_CaseSensitive;
      procedure StringTests_CaseSensitive_ShouldFail;
      procedure StringTests_CaseInsensitive;
      procedure StringTests_CaseInsensitive_ShouldFail;
      procedure ComplexNumberTests;
      procedure Emit;
    end;

    TTimerTest = class(TPerformanceCase)
      procedure RunsFor1Second;
      procedure RunsFor2Second;
    end;


    TComplexNumber = record
      Real: Double;
      Imaginary: Double;
    end;


    ComplexNumberExpectations = interface
    ['{54770E74-20EA-48FE-8BF7-CC5B55582330}']
      function Equals(aValue: TComplexNumber): TExpectation;
    end;

    ComplexNumberTest = interface
    ['{D344DE41-8B6E-48D2-A615-AB6183D593D4}']
      function Expect(aValue: TComplexNumber): ComplexNumberExpectations;
    end;




implementation

  uses
  { vcl: }
    Classes,
    Graphics,
    SysUtils,
  { deltics: }
    Deltics.StrUtils;


  const
    TEST_STR  : String     = 'The quick brown fox jumped over the lazy dog';
    TEST_ANSI : ANSIString = 'The quick brown fox jumped over the lazy dog';



{ TSelfTest -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.FailedSelfTests;
  {
    These self tests have inverted expectations - tests that should clearly
     pass are stated as expected to fail and vice versa.

    This is to exercise the self-test result analysis and ensure correct
     failure detection.
  }
  begin
    Test('TRUE').Expect(TRUE).Equals(TRUE).Test.ShouldFail;
    Test('TRUE').Expect(TRUE).Equals(FALSE).Test.ShouldPass;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.DynamicTestExecution;
  {
    Disables the "DisabledTests" test case.
  }
  var
    disabled: TTestDelegate;
  begin
    disabled := TestCase['disabledtests'];

    disabled.Enabled := FALSE;
    Test('DisabledTest.Enabled').Expect(disabled.EffectivelyEnabled).IsFALSE;
  end;


  procedure TSelfTest.DisabledTests;
  {
    Tests disabling test for a given test run.  Will abort the test run if
     performed.
  }
  begin
    Abort('This test should be disabled before running the self-tests');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.DoubleTests_Precise;
  var
    d: Double;
  begin
    d := Pi;
    Test('Pi').Expect(Pi).Equals(Pi);
    Test('d').Expect(d).Equals(Pi);

    d := 3.9823;
    Test.Expect(d).Equals(3.9823);
    Test.Expect(d).Between(3.981, 3.983);
    Test.Expect(d).InRange(3.9823, 3.983);
    Test.Expect(d).GreaterThan(3.9822);
    Test.Expect(d).LessThan(3.9824);
    Test.Expect(d).NotGreaterThan(3.9823);
    Test.Expect(d).NotGreaterThan(3.99);
    Test.Expect(d).NotLessThan(3.9823);
    Test.Expect(d).NotLessThan(3.98);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.DoubleTests_Precise_ShouldFail;
  var
    d: Double;
  begin
    Test['22/7'].Expect(22/7).Equals(Pi).Test.ShouldFail;

    d := 3.9823;
    Test.Expect(d).Equals(3.98).Test.ShouldFail;
    Test.Expect(d).Equals(3.982).Test.ShouldFail;
    Test.Expect(d).Equals(3.9829).Test.ShouldFail;
    Test.Expect(d).GreaterThan(3.9823).Test.ShouldFail;
    Test.Expect(d).GreaterThan(3.983).Test.ShouldFail;
    Test.Expect(d).LessThan(3.9823).Test.ShouldFail;
    Test.Expect(d).LessThan(3.98).Test.ShouldFail;
    Test.Expect(d).Between(3.983, 3.990).Test.ShouldFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.DoubleTests_To2DPs;
  var
    d: Double;
  begin
    d := Pi;
    Test('Pi').Expect(22/7).ToDPs(2).Equals(Pi);
    Test('Pi').Expect(Pi).ToDPs(2).Equals(Pi);
    Test('d ({actual})').Expect(d).ToDPs(2).Equals(Pi);

    d := 3.9823;
    Test.Expect(d).ToDPs(0).Equals(3);
    Test.Expect(d).ToDPs(1).Equals(3.9);
    Test.Expect(d).ToDPs(2).Equals(3.98);
    Test.Expect(d).ToDPs(3).Equals(3.982);
    Test.Expect(d).ToDPs(4).Equals(3.9823);

    Test.Expect(d).ToDPs(2).Equals(3.984);
    Test.Expect(d).ToDPs(2).Equals(3.989);

    Test.Expect(d).ToDPs(2).NotGreaterThan(3.98);
    Test.Expect(d).ToDPs(2).NotLessThan(3.98);
    Test.Expect(d).ToDPs(2).Between(3.97, 3.99);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.DoubleTests_To2DPs_ShouldFail;
  var
    d: Double;
  begin
    d := 3.9823;
    Test.Expect(d).ToDPs(0).Equals(4).Test.ShouldFail;
    Test.Expect(d).ToDPs(1).Equals(3.8).Test.ShouldFail;
    Test.Expect(d).ToDPs(2).Equals(3.99).Test.ShouldFail;
    Test.Expect(d).ToDPs(3).Equals(3.983).Test.ShouldFail;
    Test.Expect(d).ToDPs(4).Equals(3.9822).Test.ShouldFail;

    Test.Expect(d).ToDPs(2).GreaterThan(3.98).Test.ShouldFail;
    Test.Expect(d).ToDPs(2).GreaterThan(3.983).Test.ShouldFail;
    Test.Expect(d).ToDPs(2).LessThan(3.98).Test.ShouldFail;
    Test.Expect(d).ToDPs(2).Between(3.983, 3.990).Test.ShouldFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.BooleanTests;
  begin
    Test.Expect(TRUE).IsTRUE;
    Test.Expect(FALSE).IsFALSE;
    Test.Expect(TRUE).Equals(TRUE);
    Test.Expect(FALSE).Equals(FALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.BooleanTests_ShouldFail;
  begin
    Test.Expect(TRUE).IsFALSE.Test.ShouldFail;
    Test.Expect(FALSE).IsTRUE.Test.ShouldFail;
    Test.Expect(TRUE).Equals(FALSE).Test.ShouldFail;
    Test.Expect(FALSE).Equals(TRUE).Test.ShouldFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.EnumTests;
  begin
    (Test('Colour') as ColorTest).Expect(clGreen).Equals(clGreen);
    (Test('Brush Style') as EnumTest).ForEnum(TypeInfo(TBrushStyle)).Expect(Ord(bsSolid)).Equals(Ord(bsSolid));

    Note('The following test exercises the reporting of a failed equality test');
    (Test('Brush Style') as EnumTest).ForEnum(TypeInfo(TBrushStyle)).Expect(Ord(bsSolid)).Equals(Ord(bsClear)).Test.ShouldFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.IntegerTests;
  begin
    Test.Expect(10).Equals(10);

    Test.Expect(10).Between(5, 15);
    Test.Expect(10).InRange(10, 15);

    Test.Expect(10).GreaterThan(5);
    Test.Expect(10).LessThan(15);
    Test.Expect(10).NotGreaterThan(10);
    Test.Expect(10).NotGreaterThan(15);
    Test.Expect(10).NotLessThan(5);
    Test.Expect(10).NotLessThan(10);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.IntegerTests_ShouldFail;
  begin
    Test.Expect(10).Equals(11).Test.ShouldFail;

    Test.Expect(10).Between(5, 10).Test.ShouldFail;
    Test.Expect(10).Between(10, 15).Test.ShouldFail;
    Test.Expect(10).Between(15, 20).Test.ShouldFail;
    Test.Expect(10).InRange(15, 20).Test.ShouldFail;

    Test.Expect(10).GreaterThan(10).Test.ShouldFail;
    Test.Expect(10).GreaterThan(15).Test.ShouldFail;
    Test.Expect(10).LessThan(10).Test.ShouldFail;
    Test.Expect(10).LessThan(5).Test.ShouldFail;
    Test.Expect(10).NotGreaterThan(5).Test.ShouldFail;
    Test.Expect(10).NotLessThan(15).Test.ShouldFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.StringTests_CaseSensitive;
  begin
    Test.Expect(TEST_STR).BeginsWith('The quick');
    Test.Expect(TEST_STR).EndsWith('lazy dog');
    Test.Expect(TEST_STR).Contains('brown fox');

  {$ifdef UNICODE}
    Test.Expect(TEST_ANSI).Equals(TEST_ANSI);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.StringTests_CaseSensitive_ShouldFail;
  begin
    Test.Expect(TEST_STR).Equals('').Test.ShouldFail;
    Test.Expect(TEST_STR).Equals(Lowercase(TEST_STR)).Test.ShouldFail;

    Test.Expect(TEST_STR).BeginsWith('the quick').Test.ShouldFail;
    Test.Expect(TEST_STR).BeginsWith(TEST_STR + ' again').Test.ShouldFail;

    Test.Expect(TEST_STR).EndsWith('Lazy Dog').Test.ShouldFail;
    Test.Expect(TEST_STR).EndsWith('Once again ' + TEST_STR).Test.ShouldFail;
    Test.Expect(TEST_STR).Contains('the quick').Test.ShouldFail;

  {$ifdef UNICODE}
    Test.Expect(TEST_ANSI).Equals('').Test.ShouldFail;
    Test.Expect(TEST_ANSI).Equals(Lowercase(TEST_ANSI)).Test.ShouldFail;
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.StringTests;
  begin
    Test.Expect('abc').IsLowercase;
    Test.Expect('ABC').IsUppercase;
    Test.Expect('abc!def').IsLowercase;
    Test.Expect('ABC!DEF').IsUppercase;
    Test.Expect(TEST_STR).Length.Equals(Length(TEST_STR));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.StringTests_ShouldFail;
  begin
    Test.Expect('abc').IsUppercase.Test.ShouldFail;
    Test.Expect('ABC').IsLowercase.Test.ShouldFail;
    Test.Expect('abcDEF').IsUppercase.Test.ShouldFail;
    Test.Expect('abcDEF').IsLowercase.Test.ShouldFail;
    Test.Expect('abc!def').IsUppercase.Test.ShouldFail;
    Test.Expect('ABC!DEF').IsLowercase.Test.ShouldFail;
    Test.Expect(TEST_STR).Length.Equals(0).Test.ShouldFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.StringTests_CaseInsensitive;
  begin
    Test.Expect(TEST_STR).CaseInsensitive.Equals(TEST_STR);
    Test.Expect(TEST_STR).CaseInsensitive.Equals(Lowercase(TEST_STR));

    Test.Expect(TEST_STR).CaseInsensitive.BeginsWith('THE QUICK');
    Test.Expect(TEST_STR).CaseInsensitive.BeginsWith('The quick');
    Test.Expect(TEST_STR).CaseInsensitive.BeginsWith('the quick');

    Test.Expect(TEST_STR).CaseInsensitive.Contains('THE QUICK');
    Test.Expect(TEST_STR).CaseInsensitive.Contains('the quick');
    Test.Expect(TEST_STR).CaseInsensitive.Contains('The Quick');

    Test.Expect(TEST_STR).CaseInsensitive.EndsWith('LAZY DOG');
    Test.Expect(TEST_STR).CaseInsensitive.EndsWith('Lazy Dog');
    Test.Expect(TEST_STR).CaseInsensitive.EndsWith('lazy dog');

  {$ifdef UNICODE}
    Test.Expect(TEST_ANSI).CaseInsensitive.Equals(Lowercase(TEST_ANSI)).Test.ShouldPass;
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.StringTests_CaseInsensitive_ShouldFail;
  begin
    Test.Expect(TEST_STR).CaseInsensitive.Equals('').Test.ShouldFail;
    Test.Expect(TEST_STR).CaseInsensitive.BeginsWith(TEST_STR + ' again').Test.ShouldFail;
    Test.Expect(TEST_STR).CaseInsensitive.EndsWith('Once again ' + TEST_STR).Test.ShouldFail;
  end;




  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.ComplexNumberTests;
  var
    a, b: TComplexNumber;
  begin
    a.Real      := 1;
    a.Imaginary := 4;

    b := a;
    (Test as ComplexNumberTest).Expect(a).Equals(b);

    b.Real      := 1;
    b.Imaginary := 3;
    (Test as ComplexNumberTest).Expect(a).Equals(b).Test.ShouldFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSelfTest.Emit;
  var
    sl: TStringList;
  begin
    Inspect['Emitted message'].Emit;
    Inspect['Emitted message with %s'].Emit(['args']);
    Inspect['String'].Emit('A String');
    Inspect['Integer'].Emit(42);
    Inspect['Boolean'].Emit(TRUE);

    sl := TStringList.Create;
    sl.Add('First item');
    sl.Add('Second item');
    sl.Add('Third item');

    Inspect['Stringlist'].Emit(sl);

    sl.Clear;
    Inspect['Empty Stringlist'].Emit(sl);

    sl.Free;
  end;





  procedure TTimerTest.RunsFor1Second;
  begin
    Sleep(1000);
  end;


  procedure TTimerTest.RunsFor2Second;
  begin
    Sleep(2000);
  end;









{ TComplexNumberTest }

  type
    TComplexNumberTest = class(TExpectation, ComplexNumberTest,
                                             ComplexNumberExpectations)
    private
      fValue: TComplexNumber;
      function ToString(const aValue: TComplexNumber): String;
      property Value: TComplexNumber read fValue;
    public
      function Expect(aValue: TComplexNumber): ComplexNumberExpectations;
      function Equals(aExpected: TComplexNumber): TExpectation; reintroduce;
    end;


  function TComplexNumberTest.ToString(const aValue: TComplexNumber): String;
  begin
    result := Format('[%f + %fi]', [aValue.Real, aValue.Imaginary]);
  end;


  function TComplexNumberTest.Equals(aExpected: TComplexNumber): TExpectation;
  begin
    result := self;

    Expected    := ToString(aExpected);
    Description := '= {expected}';

//    OK := Subject['Real part'].Expect(Value.Real).Equals(aExpected.Real).OK;
//    OK := Subject['Imaginary part'].Expect(Value.Imaginary).Equals(aExpected.Imaginary).OK and OK;

    OK := (Value.Real = aExpected.Real)
      and (Value.Imaginary = aExpected.Imaginary);
  end;


  function TComplexNumberTest.Expect(aValue: TComplexNumber): ComplexNumberExpectations;
  begin
    result := self;
    fValue := aValue;
    Actual := ToString(aValue);
  end;


initialization
  Smoketest.RegisterExtension(ComplexNumberTest, TComplexNumberTest);

end.
