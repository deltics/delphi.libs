
  unit Benchmark.Strings.WIDE;

interface

  uses
    Deltics.Smoketest,
    Benchmark.Strings;


  type
    TWIDEPerformance = class(TStringPerformanceCase, INameCase)
    private
      function NameForCase: UnicodeString;
    published
      procedure Pos_Char;
      procedure Pos_Str;
      procedure Pos_NextChar;
      procedure Pos_NextStr;
(*
      procedure FindFirstText;
      procedure FindNextChar;
      procedure FindNextStr;
      procedure FindLastChar;
      procedure FindLastStr;
*)
    end;



implementation

  uses
    Deltics.Strings;


  const                // 0         1         2         3         4
    STR : UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    F   : WIDEChar      = 'f';
    FOX : UnicodeString = 'fox';



{ TWIDEPerformance ------------------------------------------------------------------------------- }

  function TWIDEPerformance.NameForCase: UnicodeString;
  begin
    result := 'Deltics.Strings (WIDE)';
  end;


  procedure TWIDEPerformance.Pos_Char;
  var
    p: Integer;
  begin
    WIDE.Find(STR, FOX, p);
  end;


  procedure TWIDEPerformance.Pos_NextChar;
  var
    p: Integer;
  begin
    WIDE.FindNext(STR, FOX, p);
  end;


  procedure TWIDEPerformance.Pos_Str;
  var
    p: Integer;
  begin
    WIDE.Find(STR, FOX, p);
  end;


  procedure TWIDEPerformance.Pos_NextStr;
  var
    p: Integer;
  begin
    WIDE.FindNext(STR, FOX, p);
  end;


(*
  procedure TWIDEPerformance.FindFirstText;
  const// 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'FOX';
  var
    p: Integer;
  begin
    WIDE(STR).Find(FOX, p);
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
    WIDE(STR).Find(FOX, p);
  end;


  procedure TWIDEPerformance.FindNextStr;
  const               // 0         1         2         3         4
    STR: UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    FOX: UnicodeString = 'fox';
  var
    p: Integer;
  begin
    p := 18;
    WIDE(STR).Find(FOX, p);
  end;
*)



end.

