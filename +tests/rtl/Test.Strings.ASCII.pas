
  unit Test.Strings.ASCII;

interface

  uses
    Deltics.Smoketest;


  type
    TASCIIFnTests = class(TTestCase)
      procedure Transcoding;
    end;



implementation

  uses
    Deltics.Strings,
    Test.Strings;





{ TANSIFnTests }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TASCIIFnTests.Transcoding;
  begin
    Test('ASCII.Encode(%s)', [SRCS]).Expect(ASCII.Encode(SRCS)).Equals(SRCASCII);
  end;



end.
