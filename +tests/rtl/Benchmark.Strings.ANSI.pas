
  unit Benchmark.Strings.ANSI;

interface

  uses
    Deltics.Smoketest,
    Benchmark.Strings;


  type
    TANSIPerformance = class(TStringPerformanceCase)
    published
      procedure SystemPosChar;
      procedure SystemPosStr;
      procedure PosChar;
      procedure NPosChar;
      procedure RPosChar;
      procedure PosStr;
      procedure NPosStr;
      procedure RPosStr;
    end;



implementation

  uses
    Deltics.Strings;




{ TANSIPerformance ------------------------------------------------------------------------------- }

  procedure TANSIPerformance.SystemPosChar;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  begin
    Pos(FOX, STR);
  end;

  procedure TANSIPerformance.SystemPosStr;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIString = 'fox';
  begin
    Pos(FOX, STR);
  end;

  procedure TANSIPerformance.PosChar;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  var
    p: Integer;
  begin
    ANSI.Pos(STR, FOX, p);
  end;


  procedure TANSIPerformance.NPosChar;
  const               // 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  var
    p: Integer;
  begin
    p := 18;
    ANSI.NPos(STR, FOX, p);
  end;

  procedure TANSIPerformance.RPosChar;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIChar = 'f';
  var
    p: Integer;
  begin
    ANSI.RPos(STR, FOX, p);
  end;

  procedure TANSIPerformance.PosStr;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIString = 'fox';
  var
    p: Integer;
  begin
    ANSI.Pos(STR, FOX, p);
  end;

  procedure TANSIPerformance.NPosStr;
  const               // 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIString = 'fox';
  var
    p: Integer;
  begin
    p := 18;
    ANSI.NPos(STR, FOX, p);
  end;

  procedure TANSIPerformance.RPosStr;
  const// 0         1         2         3         4
    STR: ANSIString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: ANSIString = 'fox';
  var
    p: Integer;
  begin
    ANSI.RPos(STR, FOX, p);
  end;


end.
