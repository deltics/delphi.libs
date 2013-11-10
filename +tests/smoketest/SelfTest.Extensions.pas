

  unit SelfTest.Extensions;

{$i selftest.inc}

interface

  uses
    Deltics.Smoketest;


  type
    TTestCustomExtensions = class(TTestCase)
      procedure ComplexNumberTests;
      procedure ComplexNumberTests_ShouldFail;
      procedure ComplexNumberTests_Inspection;
    end;


    TComplexNumber = record
      Real: Double;
      Imaginary: Double;
    end;


    ComplexNumberInspector = interface
    ['{614CD588-718D-4950-8A5C-7F8252613487}']
//      function get_Part(aName: Variant): ComplexNumberInspector;
      procedure Real(aValue: TComplexNumber);
      procedure Imaginary(aValue: TComplexNumber);
      procedure Value(aValue: TComplexNumber);
//      property Part[aPart: Variant]: ComplexNumberInspector read get_Part; default;
    end;


    ComplexNumberExpectation = interface
    ['{54770E74-20EA-48FE-8BF7-CC5B55582330}']
      function Equals(aValue: TComplexNumber): Evaluation;
    end;

    ComplexNumberTest = interface
    ['{D344DE41-8B6E-48D2-A615-AB6183D593D4}']
      function Expect(aValue: TComplexNumber): ComplexNumberExpectation;
    end;




implementation

  uses
  { vcl: }
    SysUtils;



{ TTestExtensions -------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCustomExtensions.ComplexNumberTests;
  var
    a, b: TComplexNumber;
  begin
    a.Real      := 1;
    a.Imaginary := 4;

    b := a;
    (Test as ComplexNumberTest).Expect(a).Equals(b);
    (Test('Test value') as ComplexNumberTest).Expect(a).Equals(b);
    (Test('Test value!') as ComplexNumberTest).Expect(a).Equals(b);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCustomExtensions.ComplexNumberTests_ShouldFail;
  var
    a, b: TComplexNumber;
  begin
    a.Real      := 1;
    a.Imaginary := 4;

    b.Real      := 1;
    b.Imaginary := 3;

    (Test as ComplexNumberTest).Expect(a).Equals(b).IsExpectedToFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCustomExtensions.ComplexNumberTests_Inspection;
  var
    a: TComplexNumber;
  begin
    a.Real      := 1;
    a.Imaginary := 4;

    (Inspect('Complex Number') as ComplexNumberInspector).Value(a);
    (Inspect('Complex Number')['real'] as ComplexNumberInspector).Real(a);
    (Inspect('Complex Number')['imaginary'] as ComplexNumberInspector).Imaginary(a);
  end;










{ TComplexNumberTest ----------------------------------------------------------------------------- }

  type
    TComplexNumberInspector = class(TInspector, ComplexNumberInspector)
    public
      function get_Part(aPart: Variant): ComplexNumberInspector;
      procedure Real(aValue: TComplexNumber);
      procedure Imaginary(aValue: TComplexNumber);
      procedure Value(aValue: TComplexNumber);
    end;


    TComplexNumberTest = class(TExpectation, ComplexNumberTest,
                                             ComplexNumberExpectation)
    private
      fValue: TComplexNumber;
      property Value: TComplexNumber read fValue;

    public // ComplexNumberTest
      function Expect(aValue: TComplexNumber): ComplexNumberExpectation;

    public // ComplexNumberExpectation
      function Equals(aExpected: TComplexNumber): Evaluation; reintroduce;
    end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function ComplexNumberToString(const aValue: TComplexNumber): String;
  begin
    result := Format('[%f + %fi]', [aValue.Real, aValue.Imaginary]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TComplexNumberInspector.get_Part(aPart: Variant): ComplexNumberInspector;
  begin
    result := inherited get_Part(aPart) as ComplexNumberInspector;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TComplexNumberInspector.Real(aValue: TComplexNumber);
  begin
    Emit(FloatToStr(aValue.Real));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TComplexNumberInspector.Imaginary(aValue: TComplexNumber);
  begin
    Emit(FloatToStr(aValue.Imaginary));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TComplexNumberInspector.Value(aValue: TComplexNumber);
  begin
    Emit(ComplexNumberToString(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TComplexNumberTest.Equals(aExpected: TComplexNumber): Evaluation;
  begin
    result := self;

    Description := '= {expected}';
    Expected    := ComplexNumberToString(aExpected);

    OK := (Value.Real = aExpected.Real)
      and (Value.Imaginary = aExpected.Imaginary);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TComplexNumberTest.Expect(aValue: TComplexNumber): ComplexNumberExpectation;
  begin
    result := self;
    fValue := aValue;
    Actual := ComplexNumberToString(aValue);
  end;









initialization
  Smoketest.RegisterExtension(ComplexNumberTest, TComplexNumberTest);
  Smoketest.RegisterExtension(ComplexNumberInspector, TComplexNumberInspector);

  Smoketest.Add([TTestCustomExtensions]);

end.
