
  unit Benchmark.Strings.STR;

interface

  uses
    Deltics.Smoketest,
    Benchmark.Strings;


  type
    TSTRPerformance = class(TStringPerformanceCase)
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


{ TSTRPerformance -------------------------------------------------------------------------------- }

  procedure TSTRPerformance.NPosChar;
  begin

  end;

  procedure TSTRPerformance.NPosStr;
  begin

  end;

  procedure TSTRPerformance.PosChar;
  begin

  end;

  procedure TSTRPerformance.PosStr;
  begin

  end;

  procedure TSTRPerformance.RPosChar;
  begin

  end;

  procedure TSTRPerformance.RPosStr;
  begin

  end;

  procedure TSTRPerformance.SystemPosChar;
  begin

  end;

  procedure TSTRPerformance.SystemPosStr;
  begin

  end;

end.
