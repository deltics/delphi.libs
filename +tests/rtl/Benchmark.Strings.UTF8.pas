
  unit Benchmark.Strings.UTF8;

interface

  uses
    Deltics.Smoketest,
    Benchmark.Strings;


  type
    TUTF8Performance = class(TStringPerformanceCase)
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





{ TUTF8Performance ------------------------------------------------------------------------------- }

  procedure TUTF8Performance.NPosChar;
  begin

  end;

  procedure TUTF8Performance.NPosStr;
  begin

  end;

  procedure TUTF8Performance.PosChar;
  begin

  end;

  procedure TUTF8Performance.PosStr;
  begin

  end;

  procedure TUTF8Performance.RPosChar;
  begin

  end;

  procedure TUTF8Performance.RPosStr;
  begin

  end;

  procedure TUTF8Performance.SystemPosChar;
  begin

  end;

  procedure TUTF8Performance.SystemPosStr;
  begin

  end;


end.

