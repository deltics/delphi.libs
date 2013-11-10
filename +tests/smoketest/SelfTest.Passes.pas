
{$i selftest.inc}

  unit SelfTest.Passes;


interface

  uses
    Deltics.Smoketest,
    SelfTest.Consts;


  type
    TTestPasses = class(TTestCase)
      procedure BuiltInExpectations;
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
      procedure DateTests;
      procedure TimeTests;
      procedure DatetimeTests;
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



{ TTestPasses ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.BuiltInExpectations;
  var
    d: Double;
    sg: Single;
    e: Extended;
    cy: Currency;
    cd: Cardinal;
    i: Integer;
    i64: Int64;
    b: Boolean;
    str: String;
    g: TGUID;
    intf: IUnknown;
    p: Pointer;
    o: TObject;
  begin
    d   := 0;
    sg  := 0;
    e   := 0;
    cy  := 0;
    cd  := 0;
    i   := 0;
    i64 := 0;
    b   := FALSE;

    str   := '';
    g     := NullGUID;
    intf  := NIL;
    p     := NIL;
    o     := NIL;

    // Fixed/floating point tests

  {$ifdef EnhancedOverloads}
    Note('Verify additional overload syntax supported in this version of Delphi');

    (Test('Currency({actual})').Expect(cy) as IExpectation).Supports(CurrencyExpectation, 'CurrencyExpectation');
    (Test('Double({actual})').Expect(d) as IExpectation).Supports(DoubleExpectation, 'DoubleExpectation');
  {$else}
    Note('Overload syntax for DoubleTests not supported in this version of Delphi');
  {$endif}

    // Extension syntax for Double and Currency tests is supported in all versions
    //  to allow for tests that may need to be portable between Delphi 7 and later
    //  compilers.

    ((Test('Currency({actual}) (using extension syntax)') as CurrencyTest).Expect(cy) as IExpectation).Supports(CurrencyExpectation, 'CurrencyExpectation');
    ((Test('Double({actual}) (using extension syntax)') as DoubleTest).Expect(d) as IExpectation).Supports(DoubleExpectation, 'DoubleExpectation');

    (Test('Single({actual})').Expect(sg) as IExpectation).Supports(SingleExpectation, 'SingleExpectation');
    (Test('Extended({actual})').Expect(e) as IExpectation).Supports(ExtendedExpectation, 'ExtendedExpectation');
    (Test('Literal({actual})').Expect(1.0) as IExpectation).Supports(ExtendedExpectation, 'ExtendedExpectation');

    // Cardinal and Integer tests

    (Test('Cardinal({actual})').Expect(cd) as IExpectation).Supports(CardinalExpectation, 'CardinalExpectation');
    (Test('Integer({actual})').Expect(i) as IExpectation).Supports(IntegerExpectation, 'IntegerExpectation');
    (Test('Int64({actual})').Expect(i64) as IExpectation).Supports(IntegerExpectation, 'IntegerExpectation');

    // Other types with 'built-in' tests

    (Test('Boolean({actual})').Expect(b) as IExpectation).Supports(BooleanExpectation, 'BooleanExpectation');
    (Test('String({actual})').Expect(str) as IExpectation).Supports(StringExpectation, 'StringExpectation');
    (Test('TGUID({actual})').Expect(g) as IExpectation).Supports(GUIDExpectation, 'GUIDExpectation');
    (Test('Interface({actual})').Expect(intf) as IExpectation).Supports(InterfaceExpectation, 'InterfaceExpectation');
    (Test('Pointer({actual})').Expect(p) as IExpectation).Supports(PointerExpectation, 'PointerExpectation');
    (Test('TObject({actual})').Expect(o) as IExpectation).Supports(ObjectExpectation, 'ObjectExpectation');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.SingleTests_Precise;
  var
    n: Single;
  begin
    n := Pi;
    Test.Expect(n).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).Equals(3.9823);
    Test.Expect(n).Between(3.981, 3.983);
    Test.Expect(n).InRange(3.9823, 3.983);
    Test.Expect(n).GreaterThan(3.9822);
    Test.Expect(n).LessThan(3.9824);
    Test.Expect(n).NotGreaterThan(3.9823);
    Test.Expect(n).NotGreaterThan(3.99);
    Test.Expect(n).NotLessThan(3.9823);
    Test.Expect(n).NotLessThan(3.98);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.SingleTests_Approximate;
  var
    n: Single;
  begin
    n := Pi;
    Test.Expect(n).ToDPs(2).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).ToDPs(0).Equals(3);
    Test.Expect(n).ToDPs(1).Equals(3.9);
    Test.Expect(n).ToDPs(2).Equals(3.98);
    Test.Expect(n).ToDPs(3).Equals(3.982);
    Test.Expect(n).ToDPs(4).Equals(3.9823);

    Test.Expect(n).ToDPs(2).Equals(3.984);
    Test.Expect(n).ToDPs(2).Equals(3.989);

    Test.Expect(n).ToDPs(2).GreaterThan(3.97);
    Test.Expect(n).ToDPs(2).LessThan(3.99);

    Test.Expect(n).ToDPs(2).NotGreaterThan(3.98);
    Test.Expect(n).ToDPs(2).NotLessThan(3.98);

    Test.Expect(n).ToDPs(2).Between(3.97, 3.99);
    Test.Expect(n).ToDPs(2).InRange(3.97, 3.98);
    Test.Expect(n).ToDPs(2).InRange(3.98, 3.99);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.DoubleTests_Precise;
  var
    n: Double;
  begin
  {$ifdef EnhancedOverloads}
    Note('Tests performed using the overload syntax');

    n := Pi;
    Test.Expect(n).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).Equals(3.9823);
    Test.Expect(n).Between(3.981, 3.983);
    Test.Expect(n).InRange(3.9823, 3.983);
    Test.Expect(n).InRange(3.98, 3.9823);

    Test.Expect(n).GreaterThan(3.9822);
    Test.Expect(n).LessThan(3.9824);

    Test.Expect(n).NotGreaterThan(3.9823);
    Test.Expect(n).NotGreaterThan(3.99);
    Test.Expect(n).NotLessThan(3.9823);
    Test.Expect(n).NotLessThan(3.98);

    Note('Tests will now be repeated using the Extension Syntax for DoubleTest');
  {$else}
    Note('All tests performed using the Extension Syntax for DoubleTest');
    Note('Overload syntax for DoubleTests not supported in this version of Delphi');
  {$endif}

    n := Pi;
    (Test as DoubleTest).Expect(n).Equals(Pi);

    n := 3.9823;
    (Test as DoubleTest).Expect(n).Equals(3.9823);
    (Test as DoubleTest).Expect(n).Between(3.981, 3.983);
    (Test as DoubleTest).Expect(n).InRange(3.9823, 3.983);
    (Test as DoubleTest).Expect(n).GreaterThan(3.9822);
    (Test as DoubleTest).Expect(n).LessThan(3.9824);
    (Test as DoubleTest).Expect(n).NotGreaterThan(3.9823);
    (Test as DoubleTest).Expect(n).NotGreaterThan(3.99);
    (Test as DoubleTest).Expect(n).NotLessThan(3.9823);
    (Test as DoubleTest).Expect(n).NotLessThan(3.98);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.DateTests;
  var
    dt: TDateTime;
    y, m, d: Word;
  begin
    dt := Now;
    DecodeDate(Now, y, m, d);

    (Test as DateTest).Expect(dt).Year.Equals(y);
    (Test as DateTest).Expect(dt).Month.Equals(m);
    (Test as DateTest).Expect(dt).DayOfMonth.Equals(d);

    (Test as DateTest).Expect(dt).Equals(dt);
    (Test as DateTest).Expect(dt).Equals(y, m, d);
    (Test as DateTest).Expect(dt).IsBefore(Now + 1);
    (Test as DateTest).Expect(dt).IsAfter(Now - 1);
    (Test as DateTest).Expect(dt).IsBetween(Now - 1, Now + 1);

    (Test as DateTest).Expect(dt).IsNotBefore(Now - 1);
    (Test as DateTest).Expect(dt).IsNotAfter(Now + 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.TimeTests;
  var
    dt: TDateTime;
    h, n, s, z: Word;
  begin
    dt := Now;
    DecodeTime(Now, h, n, s, z);

    (Test as TimeTest).Expect(dt).Hour.Equals(h);
    (Test as TimeTest).Expect(dt).Minute.Equals(n);
    (Test as TimeTest).Expect(dt).Second.Equals(s);
    (Test as TimeTest).Expect(dt).Millisecond.Equals(z);

    (Test as TimeTest).Expect(dt).Equals(dt);
    (Test as TimeTest).Expect(dt).Equals(h, n);
    (Test as TimeTest).Expect(dt).Equals(h, n, s);
    (Test as TimeTest).Expect(dt).Equals(h, n, s, z);
    (Test as TimeTest).Expect(dt).IsBetween(Now - 0.1, Now + 0.1);

    (Test as TimeTest).Expect(dt).IsAfter(Now - 0.1);
    (Test as TimeTest).Expect(dt).IsBefore(Now + 0.1);
    (Test as TimeTest).Expect(dt).IsNotAfter(Now + 0.1);
    (Test as TimeTest).Expect(dt).IsNotBefore(Now - 0.1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.DatetimeTests;
  var
    dt: TDateTime;
    y, m, d: Word;
    h, n, s, z: Word;
  begin
    dt := Now;
    DecodeDate(Now, y, m, d);
    DecodeTime(Now, h, n, s, z);

    Note('Perform TDatetime tests using the TestDatetime test method:');

    TestDatetime.Expect(dt).Year.Equals(y);
    TestDatetime.Expect(dt).Month.Equals(m);
    TestDatetime.Expect(dt).DayOfMonth.Equals(d);

    TestDatetime.Expect(dt).Hour.Equals(h);
    TestDatetime.Expect(dt).Minute.Equals(n);
    TestDatetime.Expect(dt).Second.Equals(s);
    TestDatetime.Expect(dt).Millisecond.Equals(z);

    TestDatetime.Expect(dt).Equals(dt);

    TestDatetime.Expect(dt).IsBetween(Now - 0.5, Now + 0.5);

    TestDatetime.Expect(dt).IsAfter(Now - 0.5);
    TestDatetime.Expect(dt).IsBefore(Now + 0.5);
    TestDatetime.Expect(dt).IsNotAfter(Now + 0.5);
    TestDatetime.Expect(dt).IsNotBefore(Now - 0.5);

    TestDatetime.Expect(dt).HasDate(y, m, d);
    TestDatetime.Expect(dt).HasTime(h, n, s);
    TestDatetime.Expect(dt).HasTime(h, n, s, z);

    Note('Repeat tests using "Test as DatetimeTest" extension syntax');

    (Test as DateTimeTest).Expect(dt).Year.Equals(y);
    (Test as DateTimeTest).Expect(dt).Month.Equals(m);
    (Test as DateTimeTest).Expect(dt).DayOfMonth.Equals(d);

    (Test as DateTimeTest).Expect(dt).Hour.Equals(h);
    (Test as DateTimeTest).Expect(dt).Minute.Equals(n);
    (Test as DateTimeTest).Expect(dt).Second.Equals(s);
    (Test as DateTimeTest).Expect(dt).Millisecond.Equals(z);

    (Test as DateTimeTest).Expect(dt).Equals(dt);

    (Test as DateTimeTest).Expect(dt).IsBetween(Now - 0.5, Now + 0.5);

    (Test as DateTimeTest).Expect(dt).IsAfter(Now - 0.5);
    (Test as DateTimeTest).Expect(dt).IsBefore(Now + 0.5);
    (Test as DateTimeTest).Expect(dt).IsNotAfter(Now + 0.5);
    (Test as DateTimeTest).Expect(dt).IsNotBefore(Now - 0.5);

    (Test as DateTimeTest).Expect(dt).HasDate(y, m, d);
    (Test as DateTimeTest).Expect(dt).HasTime(h, n, s);
    (Test as DateTimeTest).Expect(dt).HasTime(h, n, s, z);

  {$ifdef DELPHI2009_OR_LATER}
    Note('TDatetime values can also be tested using the Expect(TDatetime) overload.  Run one test to make sure');
    Test.Expect(dt).Year.Equals(y);
  {$else}
    Note('Expect(TDatetime) overload not supported in this version of Delphi');
  {$endif}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.DoubleTests_Approximate;
  var
    n: Double;
  begin
  {$ifdef EnhancedOverloads}
    Note('Tests performed using the overload syntax');

    n := Pi;
    Test('Pi ({actual})').Expect(22/7).ToDPs(2).Equals(Pi);
    Test('Pi ({actual})').Expect(Pi).ToDPs(2).Equals(Pi);
    Test('n ({actual})').Expect(n).ToDPs(2).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).ToDPs(0).Equals(3);
    Test.Expect(n).ToDPs(1).Equals(3.9);
    Test.Expect(n).ToDPs(2).Equals(3.98);
    Test.Expect(n).ToDPs(3).Equals(3.982);
    Test.Expect(n).ToDPs(4).Equals(3.9823);

    Test.Expect(n).ToDPs(2).Equals(3.984);
    Test.Expect(n).ToDPs(2).Equals(3.989);

    Test.Expect(n).ToDPs(2).NotGreaterThan(3.98);
    Test.Expect(n).ToDPs(2).NotLessThan(3.98);
    Test.Expect(n).ToDPs(2).Between(3.97, 3.99);

    Note('Tests will now be repeated using the Extension Syntax for DoubleTest');
  {$else}
    Note('All tests performed using the Extension Syntax for DoubleTest');
    Note('Overload syntax for DoubleTests not supported in this version of Delphi');
  {$endif}

    n := Pi;
    (Test('Pi ({actual})') as DoubleTest).Expect(22/7).ToDPs(2).Equals(Pi);
    (Test('Pi ({actual})') as DoubleTest).Expect(Pi).ToDPs(2).Equals(Pi);
    (Test('n ({actual})') as DoubleTest).Expect(n).ToDPs(2).Equals(Pi);

    n := 3.9823;
    (Test as DoubleTest).Expect(n).ToDPs(0).Equals(3);
    (Test as DoubleTest).Expect(n).ToDPs(1).Equals(3.9);
    (Test as DoubleTest).Expect(n).ToDPs(2).Equals(3.98);
    (Test as DoubleTest).Expect(n).ToDPs(3).Equals(3.982);
    (Test as DoubleTest).Expect(n).ToDPs(4).Equals(3.9823);

    (Test as DoubleTest).Expect(n).ToDPs(2).Equals(3.984);
    (Test as DoubleTest).Expect(n).ToDPs(2).Equals(3.989);

    (Test as DoubleTest).Expect(n).ToDPs(2).NotGreaterThan(3.98);
    (Test as DoubleTest).Expect(n).ToDPs(2).NotLessThan(3.98);
    (Test as DoubleTest).Expect(n).ToDPs(2).Between(3.97, 3.99);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.ExtendedTests_Precise;
  var
    n: Extended;
  begin
    n := Pi;
    Test.Expect(Pi).Equals(Pi);
    Test.Expect(n).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).Equals(3.9823);
    Test.Expect(n).Between(3.981, 3.983);
    Test.Expect(n).InRange(3.9823, 3.983);
    Test.Expect(n).GreaterThan(3.9822);
    Test.Expect(n).LessThan(3.9824);
    Test.Expect(n).NotGreaterThan(3.9823);
    Test.Expect(n).NotGreaterThan(3.99);
    Test.Expect(n).NotLessThan(3.9823);
    Test.Expect(n).NotLessThan(3.98);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.ExtendedTests_Approximate;
  var
    n: Extended;
  begin
    n := Pi;
    Test('Pi').Expect(22/7).ToDPs(2).Equals(Pi);
    Test('Pi').Expect(Pi).ToDPs(2).Equals(Pi);
    Test('n ({actual})').Expect(n).ToDPs(2).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).ToDPs(0).Equals(3);
    Test.Expect(n).ToDPs(1).Equals(3.9);
    Test.Expect(n).ToDPs(2).Equals(3.98);
    Test.Expect(n).ToDPs(3).Equals(3.982);
    Test.Expect(n).ToDPs(4).Equals(3.9823);

    Test.Expect(n).ToDPs(2).Equals(3.984);
    Test.Expect(n).ToDPs(2).Equals(3.989);

    Test.Expect(n).ToDPs(2).NotGreaterThan(3.98);
    Test.Expect(n).ToDPs(2).NotLessThan(3.98);
    Test.Expect(n).ToDPs(2).Between(3.97, 3.99);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.CurrencyTests_Precise;
  var
    n: Currency;
  begin
  {$ifdef EnhancedOverloads}
    Note('Tests performed using the overload syntax');

    n := Pi;
    Test('Pi').Expect(Pi).Equals(Pi);
    Test('n').Expect(n).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).Equals(3.9823);
    Test.Expect(n).Between(3.981, 3.983);
    Test.Expect(n).InRange(3.9823, 3.983);
    Test.Expect(n).GreaterThan(3.9822);
    Test.Expect(n).LessThan(3.9824);
    Test.Expect(n).NotGreaterThan(3.9823);
    Test.Expect(n).NotGreaterThan(3.99);
    Test.Expect(n).NotLessThan(3.9823);
    Test.Expect(n).NotLessThan(3.98);

    Note('Tests will now be repeated using the Extension Syntax for DoubleTest');
  {$else}
    Note('All tests performed using the Extension Syntax for DoubleTest');
    Note('Overload syntax for DoubleTests not supported in this version of Delphi');
  {$endif}

    n := Pi;
    (Test('Pi using extension syntax') as CurrencyTest).Expect(Pi).Equals(Pi);
    (Test('n using extension syntax') as CurrencyTest).Expect(n).Equals(Pi);

    n := 3.9823;
    (Test as CurrencyTest).Expect(n).Equals(3.9823);
    (Test as CurrencyTest).Expect(n).Between(3.981, 3.983);
    (Test as CurrencyTest).Expect(n).InRange(3.9823, 3.983);
    (Test as CurrencyTest).Expect(n).GreaterThan(3.9822);
    (Test as CurrencyTest).Expect(n).LessThan(3.9824);
    (Test as CurrencyTest).Expect(n).NotGreaterThan(3.9823);
    (Test as CurrencyTest).Expect(n).NotGreaterThan(3.99);
    (Test as CurrencyTest).Expect(n).NotLessThan(3.9823);
    (Test as CurrencyTest).Expect(n).NotLessThan(3.98);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.CurrencyTests_Approximate;
  var
    n: Currency;
  begin
  {$ifdef EnhancedOverloads}
    Note('Tests performed using the overload syntax');

    n := Pi;
    Test('Pi').Expect(22/7).ToDPs(2).Equals(Pi);
    Test('Pi').Expect(Pi).ToDPs(2).Equals(Pi);
    Test('n ({actual})').Expect(n).ToDPs(2).Equals(Pi);

    n := 3.9823;
    Test.Expect(n).ToDPs(0).Equals(3);
    Test.Expect(n).ToDPs(1).Equals(3.9);
    Test.Expect(n).ToDPs(2).Equals(3.98);
    Test.Expect(n).ToDPs(3).Equals(3.982);
    Test.Expect(n).ToDPs(4).Equals(3.9823);

    Test.Expect(n).ToDPs(2).Equals(3.984);
    Test.Expect(n).ToDPs(2).Equals(3.989);

    Test.Expect(n).ToDPs(2).NotGreaterThan(3.98);
    Test.Expect(n).ToDPs(2).NotLessThan(3.98);
    Test.Expect(n).ToDPs(2).Between(3.97, 3.99);

    Note('Tests will now be repeated using the Extension Syntax for DoubleTest');
  {$else}
    Note('All tests performed using the Extension Syntax for DoubleTest');
    Note('Overload syntax for DoubleTests not supported in this version of Delphi');
  {$endif}

    n := Pi;
    (Test('Pi') as CurrencyTest).Expect(22/7).ToDPs(2).Equals(Pi);
    (Test('Pi') as CurrencyTest).Expect(Pi).ToDPs(2).Equals(Pi);
    (Test('n {actual})') as CurrencyTest).Expect(n).ToDPs(2).Equals(Pi);

    n := 3.9823;
    (Test as CurrencyTest).Expect(n).ToDPs(0).Equals(3);
    (Test as CurrencyTest).Expect(n).ToDPs(1).Equals(3.9);
    (Test as CurrencyTest).Expect(n).ToDPs(2).Equals(3.98);
    (Test as CurrencyTest).Expect(n).ToDPs(3).Equals(3.982);
    (Test as CurrencyTest).Expect(n).ToDPs(4).Equals(3.9823);

    (Test as CurrencyTest).Expect(n).ToDPs(2).Equals(3.984);
    (Test as CurrencyTest).Expect(n).ToDPs(2).Equals(3.989);

    (Test as CurrencyTest).Expect(n).ToDPs(2).NotGreaterThan(3.98);
    (Test as CurrencyTest).Expect(n).ToDPs(2).NotLessThan(3.98);
    (Test as CurrencyTest).Expect(n).ToDPs(2).Between(3.97, 3.99);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.BooleanTests;
  begin
    Test('IsTRUE').Expect(TRUE).IsTRUE;
    Test('IsFALSE').Expect(FALSE).IsFALSE;
    Test('Equals({actual})').Expect(TRUE).Equals(TRUE);
    Test('Equals({actual})').Expect(FALSE).Equals(FALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.CardinalTests;
  var
    c: Cardinal;
  begin
    c := MaxInt;

    Test.Expect(c).Equals(MaxInt);
    Test.Expect(c).Between(MaxInt - 1, Cardinal(MaxInt) + 1);
    Test.Expect(c).InRange(0, MaxInt);
    Test.Expect(c).GreaterThan(MaxInt - 1);
    Test.Expect(c).LessThan(Cardinal(MaxInt) + 1);
    Test.Expect(c).NotGreaterThan(MaxInt);
    Test.Expect(c).NotLessThan(MaxInt);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.IntegerTests;
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
  procedure TTestPasses.StringTests;
  begin
    Test.Expect('abc').IsLowercase;
    Test.Expect('ABC').IsUppercase;
    Test.Expect('abc!def').IsLowercase;
    Test.Expect('ABC!DEF').IsUppercase;
    Test.Expect(TEST_STR).Length.Equals(Length(TEST_STR));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.StringTests_CaseSensitive;
  begin
    Test.Expect(TEST_STR).BeginsWith('The quick');
    Test.Expect(TEST_STR).EndsWith('lazy dog');
    Test.Expect(TEST_STR).Contains('brown fox');

    Test.Expect(TEST_ANSI).Equals(TEST_ANSI);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.StringTests_CaseInsensitive;
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

    Test.Expect(TEST_ANSI).CaseInsensitive.Equals(ANSI.Lowercase(TEST_ANSI));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.GUIDTests;
  var
    g: TGUID;
  begin
    CoCreateGUID(g);

    Test.Expect(g).Equals(g);
    Test.Expect(NullGUID).IsNull;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestPasses.EnumTests;
  begin
    (Test('Colour') as ColorTest).Expect(clGreen).Equals(clGreen);
//    (Test('Brush Style') as EnumTest).ForEnum(TypeInfo(TBrushStyle)).Expect(Ord(bsSolid)).Equals(Ord(bsSolid));
  end;




initialization
  Smoketest.Add([TTestPasses]);

end.
