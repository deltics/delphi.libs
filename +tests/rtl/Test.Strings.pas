
  unit Test.Strings;

interface

  uses
    Deltics.Smoketest,
    Deltics.Strings;


  type
    TTestStrings = class(TTestCase, INameCase)
    private
      function NameForCase: UnicodeString;
    published
      procedure SizeOfChar;
    end;


    TANSIStringAB = record
      A: ANSIString;
      B: ANSIString;
    end;

    TWIDEStringAB = record
      A: UnicodeString;
      B: UnicodeString;
    end;

    TStringAB = record
      A: String;
      B: String;
    end;

    TUTF8StringAB = record
      A: UTF8String;
      B: UTF8String;
    end;


  const
    SRCS: String        = 'Unicode™';
    SRCA: ANSIString    = 'Unicode™';
    SRCW: UnicodeString = 'Unicode™';
  var
    SRCU: UTF8String;



implementation

  uses
    Test.Strings.ANSI,
    Test.Strings.WIDE,
    Test.Strings.UTF8,
    Test.Strings.STR;


{ TTestStrings ----------------------------------------------------------------------------------- }

  function TTestStrings.NameForCase: UnicodeString;
  begin
    result := 'Deltics.Strings';
  end;


  procedure TTestStrings.SizeOfChar;
  begin
  {$ifdef UNICODE}
    Test('sizeof(Char)').Expect(sizeof(Char)).Equals(2);
  {$else}
    Test('sizeof(Char)').Expect(sizeof(Char)).Equals(1);
  {$endif}

    Test('sizeof(UTF8Char)').Expect(sizeof(UTF8Char)).Equals(1);
    Test('sizeof(ANSIChar)').Expect(sizeof(ANSIChar)).Equals(1);
    Test('sizeof(WideChar)').Expect(sizeof(WideChar)).Equals(2);
  end;




initialization
  SRCU := UTF8.Encode('Unicode™');

  Smoketest.Add(TTestStrings, [TANSITests, TWIDETests, TSTRTests, TUTF8Tests]);
  Smoketest.AverageTime([TANSIPerformance], 5).RunningFor(200).Iterations;
  Smoketest.AverageTime([TWIDEPerformance], 5).RunningFor(200).Iterations;
//  Smoketest.AverageTime([TUTF8Performance], 5).RunningFor(200).Iterations;
end.
