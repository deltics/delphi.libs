
  unit Benchmark.Strings.WIDE;

interface

  uses
    Deltics.Smoketest,
    Benchmark.Strings;


  type
    TWIDEPerformance = class(TStringPerformanceCase)
      procedure SystemPosChar;
      procedure SystemPosStr;
      procedure FindFirstChar;
      procedure FindFirstStr;
      procedure FindFirstText;
      procedure FindNextChar;
      procedure FindNextStr;
      procedure FindLastChar;
      procedure FindLastStr;
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

  procedure TWIDEPerformance.FindFirstChar;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: WideChar = 'f';
  var
    p: Integer;
  begin
    WIDE(STR).FindFirst(FOX, p);
  end;

  procedure TWIDEPerformance.FindFirstStr;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  var
    p: Integer;
  begin
    WIDE(STR).FindFirst(FOX, p);
  end;

  procedure TWIDEPerformance.FindFirstText;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'FOX';
  var
    p: Integer;
  begin
    WIDE(STR).FindFirstText(FOX, p);
  end;


  procedure TWIDEPerformance.FindLastChar;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: WideChar = 'f';
  var
    p: Integer;
  begin
    WIDE(STR).FindLast(FOX, p);
  end;


  procedure TWIDEPerformance.FindLastStr;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  var
    p: Integer;
  begin
    WIDE(STR).FindLast(FOX, p);
  end;


  procedure TWIDEPerformance.FindNextChar;
  const               // 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: WideChar = 'f';
  var
    p: Integer;
  begin
    p := 18;
    WIDE(STR).FindNext(FOX, p);
  end;


  procedure TWIDEPerformance.FindNextStr;
  const               // 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  var
    p: Integer;
  begin
    p := 18;
    WIDE(STR).FindNext(FOX, p);
  end;




end.

