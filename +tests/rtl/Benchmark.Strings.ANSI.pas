
  unit Benchmark.Strings.ANSI;

interface

  uses
    Deltics.Smoketest,
    Benchmark.Strings;


  type
    TANSIPerformance = class(TStringPerformanceCase, INameCase)
    private
      function NameForCase: UnicodeString;
    published
      procedure Pos_Char;
      procedure Pos_Str;
      procedure Pos_NextChar;
      procedure Pos_NextStr;
    end;



implementation

  uses
    Deltics.Strings;


  const             // 0         1         2         3         4
    STR : ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    F   : ANSIChar   = 'f';
    FOX : ANSIString = 'fox';


{ TANSIPerformance ------------------------------------------------------------------------------- }

  function TANSIPerformance.NameForCase: UnicodeString;
  begin
    result := 'Deltics.Strings (ANSI)';
  end;


  procedure TANSIPerformance.Pos_Char;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  var
    p: Integer;
  begin
    ANSI.Find(STR, FOX, p);
  end;


  procedure TANSIPerformance.Pos_NextChar;
  var
    p: Integer;
  begin
    p := 21;
    ANSI.FindNext(STR, F, p);
  end;


  procedure TANSIPerformance.Pos_Str;
  var
    p: Integer;
  begin
    ANSI.Find(STR, FOX, p);
  end;


  procedure TANSIPerformance.Pos_NextStr;
  var
    p: Integer;
  begin
    p := 21;
    ANSI.FindNext(STR, FOX, p);
  end;





end.
