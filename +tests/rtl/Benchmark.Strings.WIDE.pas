
  unit Benchmark.Strings.WIDE;

interface

  uses
    Deltics.Smoketest,
    Benchmark.Strings;


  type
    TWIDEPerformance = class(TStringPerformanceCase)
      procedure SystemPosChar;
      procedure SystemPosStr;
      procedure PosChar;
      procedure NPosChar;
      procedure RPosChar;
      procedure PosStr;
      procedure NPosStr;
      procedure RPosStr;
      procedure PosCharText;
      procedure PosText;
    end;



implementation

  uses
    Deltics.Strings;



{ TWIDEPerformance ------------------------------------------------------------------------------- }

  procedure TWIDEPerformance.SystemPosChar;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: WideChar = 'f';
  begin
    Pos(FOX, STR);
  end;

  procedure TWIDEPerformance.SystemPosStr;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  begin
    Pos(FOX, STR);
  end;

  procedure TWIDEPerformance.PosChar;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: WideChar = 'f';
  var
    p: Integer;
  begin
    WIDE.Pos(STR, FOX, p);
  end;

  procedure TWIDEPerformance.PosStr;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  var
    p: Integer;
  begin
    WIDE.Pos(STR, FOX, p);
  end;

  procedure TWIDEPerformance.PosCharText;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'F';
  var
    p: Integer;
  begin
    WIDE.PosText(STR, FOX, p);
  end;

  procedure TWIDEPerformance.PosText;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  var
    p: Integer;
  begin
    WIDE.PosText(STR, FOX, p);
  end;

  procedure TWIDEPerformance.NPosChar;
  const               // 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: WideChar = 'f';
  var
    p: Integer;
  begin
    p := 18;
    WIDE.NPos(STR, FOX, p);
  end;

  procedure TWIDEPerformance.RPosChar;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: WideChar = 'f';
  var
    p: Integer;
  begin
    WIDE.RPos(STR, FOX, p);
  end;

  procedure TWIDEPerformance.NPosStr;
  const               // 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  var
    p: Integer;
  begin
    p := 18;
    WIDE.NPos(STR, FOX, p);
  end;

  procedure TWIDEPerformance.RPosStr;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  var
    p: Integer;
  begin
    WIDE.RPos(STR, FOX, p);
  end;





end.

