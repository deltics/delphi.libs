
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

    TANSICharAB = record
      A: ANSIChar;
      B: ANSIChar;
    end;

    TWIDEStringAB = record
      A: UnicodeString;
      B: UnicodeString;
    end;

    TWIDECharAB = record
      A: WideChar;
      B: WideChar;
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
    Test.Strings.STR,
    Test.Strings.UTF8,
    Test.Strings.WIDE;


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

  Smoketest.Add(TTestStrings, [TANSIFnTests, TANSITests, TWIDETests, TSTRTests, TUTF8Tests]);
end.
