

  unit SelfTest.Failures;

{$i selftest.inc}

interface

  uses
    Deltics.Smoketest,
    SelfTest.Consts;


  type
    TTestFailures = class(TTestCase)
      procedure SingleTests_Precise;
      procedure SingleTests_Approximate;
      procedure DoubleTests_Precise;
      procedure DoubleTests_Approximate;
      procedure ExtendedTests_Precise;
      procedure ExtendedTests_Approximate;
      procedure CurrencyTests_Precise;
      procedure CurrencyTests_Approximate;
      procedure BooleanTests;
      procedure CardinalTests;
      procedure IntegerTests;
      procedure StringTests;
      procedure StringTests_CaseSensitive;
      procedure StringTests_CaseInsensitive;
      procedure GUIDTests;
      procedure EnumTests;
    end;




implementation

  uses
  { vcl: }
    ActiveX,
    Classes,
    Graphics,
    SysUtils,
  { deltics: }
    Deltics.Strings,
    Deltics.StrUtils,
    Deltics.SysUtils;




{ TSelfTest -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.SingleTests_Precise;
  var
    n: Single;
  begin
    TheseTests.AreExpectedToFail;

    (Test('ISingleExpectation').Expect(3.9823) as IExpectation).Supports(SingleExpectation);
    (Test('ISingleExpectation').Expect(22/7) as IExpectation).Supports(SingleExpectation);
    (Test('ISingleExpectation').Expect(Pi) as IExpectation).Supports(SingleExpectation);

    n := 3.9823;
    Test.Expect(n).Equals(3.98);
    Test.Expect(n).Equals(3.982);
    Test.Expect(n).Equals(3.9829);
    Test.Expect(n).GreaterThan(3.9823);
    Test.Expect(n).GreaterThan(3.983);
    Test.Expect(n).LessThan(3.9823);
    Test.Expect(n).LessThan(3.98);
    Test.Expect(n).Between(3.983, 3.990);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.SingleTests_Approximate;
  var
    n: Single;
  begin
    TheseTests.AreExpectedToFail;

    n := 3.9823;
    Test.Expect(n).ToDPs(0).Equals(4);
    Test.Expect(n).ToDPs(1).Equals(3.8);
    Test.Expect(n).ToDPs(2).Equals(3.99);
    Test.Expect(n).ToDPs(3).Equals(3.983);
    Test.Expect(n).ToDPs(4).Equals(3.9822);

    Test.Expect(n).ToDPs(2).GreaterThan(3.98);
    Test.Expect(n).ToDPs(2).GreaterThan(3.983);
    Test.Expect(n).ToDPs(2).LessThan(3.98);
    Test.Expect(n).ToDPs(2).Between(3.983, 3.990);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.DoubleTests_Precise;
  var
    n: Double;
  begin
    TheseTests.AreExpectedToFail;

  {$ifdef EnhancedOverloads}
    (Test('DoubleExpectation').Expect(Pi) as IExpectation).Supports(DoubleExpectation);
    (Test('DoubleExpectation').Expect(22/7) as IExpectation).Supports(DoubleExpectation);
  {$else}
    (Test('DoubleExpectation').Expect(Pi) as IExpectation).Supports(DoubleExpectation);
    (Test('DoubleExpectation').Expect(22/7) as IExpectation).Supports(DoubleExpectation);
  {$endif}

    n := 3.9823;
  {$ifdef EnhancedOverloads}
    Test.Expect(n).Equals(3.98);
    Test.Expect(n).Equals(3.982);
    Test.Expect(n).Equals(3.9829);
    Test.Expect(n).GreaterThan(3.9823);
    Test.Expect(n).GreaterThan(3.983);
    Test.Expect(n).LessThan(3.9823);
    Test.Expect(n).LessThan(3.98);
    Test.Expect(n).Between(3.983, 3.990);
  {$else}
    (Test as DoubleTest).Expect(n).Equals(3.98);
    (Test as DoubleTest).Expect(n).Equals(3.982);
    (Test as DoubleTest).Expect(n).Equals(3.9829);
    (Test as DoubleTest).Expect(n).GreaterThan(3.9823);
    (Test as DoubleTest).Expect(n).GreaterThan(3.983);
    (Test as DoubleTest).Expect(n).LessThan(3.9823);
    (Test as DoubleTest).Expect(n).LessThan(3.98);
    (Test as DoubleTest).Expect(n).Between(3.983, 3.990);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.DoubleTests_Approximate;
  var
    n: Double;
  begin
    TheseTests.AreExpectedToFail;

    n := 3.9823;
  {$ifdef EnhancedOverloads}
    Test.Expect(n).ToDPs(0).Equals(4);
    Test.Expect(n).ToDPs(1).Equals(3.8);
    Test.Expect(n).ToDPs(2).Equals(3.99);
    Test.Expect(n).ToDPs(3).Equals(3.983);
    Test.Expect(n).ToDPs(4).Equals(3.9822);

    Test.Expect(n).ToDPs(2).GreaterThan(3.98);
    Test.Expect(n).ToDPs(2).GreaterThan(3.983);
    Test.Expect(n).ToDPs(2).LessThan(3.98);
    Test.Expect(n).ToDPs(2).Between(3.983, 3.990);
  {$else}
    (Test as DoubleTest).Expect(n).ToDPs(0).Equals(4);
    (Test as DoubleTest).Expect(n).ToDPs(1).Equals(3.8);
    (Test as DoubleTest).Expect(n).ToDPs(2).Equals(3.99);
    (Test as DoubleTest).Expect(n).ToDPs(3).Equals(3.983);
    (Test as DoubleTest).Expect(n).ToDPs(4).Equals(3.9822);

    (Test as DoubleTest).Expect(n).ToDPs(2).GreaterThan(3.98);
    (Test as DoubleTest).Expect(n).ToDPs(2).GreaterThan(3.983);
    (Test as DoubleTest).Expect(n).ToDPs(2).LessThan(3.98);
    (Test as DoubleTest).Expect(n).ToDPs(2).Between(3.983, 3.990);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.ExtendedTests_Precise;
  var
    n: Extended;
  begin
    TheseTests.AreExpectedToFail;

    Test['22/7'].Expect(22/7).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).Equals(3.98);
    Test.Expect(n).Equals(3.982);
    Test.Expect(n).Equals(3.9829);
    Test.Expect(n).GreaterThan(3.9823);
    Test.Expect(n).GreaterThan(3.983);
    Test.Expect(n).LessThan(3.9823);
    Test.Expect(n).LessThan(3.98);
    Test.Expect(n).Between(3.983, 3.990);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.ExtendedTests_Approximate;
  var
    n: Extended;
  begin
    TheseTests.AreExpectedToFail;

    n := 3.9823;
    Test.Expect(n).ToDPs(0).Equals(4);
    Test.Expect(n).ToDPs(1).Equals(3.8);
    Test.Expect(n).ToDPs(2).Equals(3.99);
    Test.Expect(n).ToDPs(3).Equals(3.983);
    Test.Expect(n).ToDPs(4).Equals(3.9822);

    Test.Expect(n).ToDPs(2).GreaterThan(3.98);
    Test.Expect(n).ToDPs(2).GreaterThan(3.983);
    Test.Expect(n).ToDPs(2).LessThan(3.98);
    Test.Expect(n).ToDPs(2).Between(3.983, 3.990);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.CurrencyTests_Precise;
  var
    n: Currency;
  begin
    TheseTests.AreExpectedToFail;

  {$ifdef EnhancedOverloads}
    Test['22/7'].Expect(22/7).Equals(Pi);
  {$else}
  {$endif}

    n := 3.9823;
  {$ifdef EnhancedOverloads}
    Test.Expect(n).Equals(3.98);
    Test.Expect(n).Equals(3.982);
    Test.Expect(n).Equals(3.9829);
    Test.Expect(n).GreaterThan(3.9823);
    Test.Expect(n).GreaterThan(3.983);
    Test.Expect(n).LessThan(3.9823);
    Test.Expect(n).LessThan(3.98);
    Test.Expect(n).Between(3.983, 3.990);
  {$else}
    (Test as CurrencyTest).Expect(n).Equals(3.98);
    (Test as CurrencyTest).Expect(n).Equals(3.982);
    (Test as CurrencyTest).Expect(n).Equals(3.9829);
    (Test as CurrencyTest).Expect(n).GreaterThan(3.9823);
    (Test as CurrencyTest).Expect(n).GreaterThan(3.983);
    (Test as CurrencyTest).Expect(n).LessThan(3.9823);
    (Test as CurrencyTest).Expect(n).LessThan(3.98);
    (Test as CurrencyTest).Expect(n).Between(3.983, 3.990);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.CurrencyTests_Approximate;
  var
    n: Currency;
  begin
    TheseTests.AreExpectedToFail;

    n := 3.9823;
  {$ifdef EnhancedOverloads}
    Test.Expect(n).ToDPs(0).Equals(4);
    Test.Expect(n).ToDPs(1).Equals(3.8);
    Test.Expect(n).ToDPs(2).Equals(3.99);
    Test.Expect(n).ToDPs(3).Equals(3.983);
    Test.Expect(n).ToDPs(4).Equals(3.9822);

    Test.Expect(n).ToDPs(2).GreaterThan(3.98);
    Test.Expect(n).ToDPs(2).GreaterThan(3.983);
    Test.Expect(n).ToDPs(2).LessThan(3.98);
    Test.Expect(n).ToDPs(2).Between(3.983, 3.990);
  {$else}
    (Test as CurrencyTest).Expect(n).ToDPs(0).Equals(4);
    (Test as CurrencyTest).Expect(n).ToDPs(1).Equals(3.8);
    (Test as CurrencyTest).Expect(n).ToDPs(2).Equals(3.99);
    (Test as CurrencyTest).Expect(n).ToDPs(3).Equals(3.983);
    (Test as CurrencyTest).Expect(n).ToDPs(4).Equals(3.9822);

    (Test as CurrencyTest).Expect(n).ToDPs(2).GreaterThan(3.98);
    (Test as CurrencyTest).Expect(n).ToDPs(2).GreaterThan(3.983);
    (Test as CurrencyTest).Expect(n).ToDPs(2).LessThan(3.98);
    (Test as CurrencyTest).Expect(n).ToDPs(2).Between(3.983, 3.990);
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.BooleanTests;
  begin
    TheseTests.AreExpectedToFail;

    Test.Expect(TRUE).IsFALSE;
    Test.Expect(FALSE).IsTRUE;
    Test.Expect(TRUE).Equals(FALSE);
    Test.Expect(FALSE).Equals(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.CardinalTests;
  var
    c: Cardinal;
  begin
    TheseTests.AreExpectedToFail;

    c := MaxInt;

    Test.Expect(c).Equals(MaxInt - 1);
    Test.Expect(c).Between(0, MaxInt);
    Test.Expect(c).InRange(0, MaxInt - 1);
    Test.Expect(c).GreaterThan(MaxInt);
    Test.Expect(c).LessThan(MaxInt);
    Test.Expect(c).NotGreaterThan(MaxInt - 1);
    Test.Expect(c).NotLessThan(Cardinal(MaxInt) + 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.IntegerTests;
  begin
    TheseTests.AreExpectedToFail;

    Test.Expect(10).Equals(11);

    Test.Expect(10).Between(5, 10);
    Test.Expect(10).Between(10, 15);
    Test.Expect(10).Between(15, 20);
    Test.Expect(10).InRange(15, 20);

    Test.Expect(10).GreaterThan(10);
    Test.Expect(10).GreaterThan(15);
    Test.Expect(10).LessThan(10);
    Test.Expect(10).LessThan(5);
    Test.Expect(10).NotGreaterThan(5);
    Test.Expect(10).NotLessThan(15);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.StringTests;
  begin
    TheseTests.AreExpectedToFail;

    Test.Expect('abc').IsUppercase;
    Test.Expect('ABC').IsLowercase;
    Test.Expect('abcDEF').IsUppercase;
    Test.Expect('abcDEF').IsLowercase;
    Test.Expect('abc!def').IsUppercase;
    Test.Expect('ABC!DEF').IsLowercase;
    Test.Expect(TEST_STR).Length.Equals(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.StringTests_CaseSensitive;
  begin
    TheseTests.AreExpectedToFail;

    Test.Expect(TEST_STR).Equals('');
    Test.Expect(TEST_STR).Equals(Lowercase(TEST_STR));

    Test.Expect(TEST_STR).BeginsWith('the quick');
    Test.Expect(TEST_STR).BeginsWith(TEST_STR + ' again');

    Test.Expect(TEST_STR).EndsWith('Lazy Dog');
    Test.Expect(TEST_STR).EndsWith('Once again ' + TEST_STR);
    Test.Expect(TEST_STR).Contains('the quick');

    Test.Expect(TEST_ANSI).Equals('');
    Test.Expect(TEST_ANSI).Equals(ANSI.Lowercase(TEST_ANSI));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.StringTests_CaseInsensitive;
  begin
    TheseTests.AreExpectedToFail;

    Test.Expect(TEST_STR).CaseInsensitive.Equals('');
    Test.Expect(TEST_STR).CaseInsensitive.BeginsWith(TEST_STR + ' again');
    Test.Expect(TEST_STR).CaseInsensitive.EndsWith('Once again ' + TEST_STR);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.GUIDTests;
  var
    g: TGUID;
  begin
    TheseTests.AreExpectedToFail;

    CoCreateGUID(g);

    Test.Expect(g).Equals(NullGUID);
    Test.Expect(g).IsNull;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFailures.EnumTests;
  begin
    TheseTests.AreExpectedToFail;

    (Test('Colour') as ColorTest).Expect(clGreen).Equals(clWhite);
//    (Test('Brush Style') as EnumTest).ForEnum(TypeInfo(TBrushStyle)).Expect(Ord(bsSolid)).Equals(Ord(bsClear));
  end;




initialization
  Smoketest.Add([TTestFailures]);

end.
