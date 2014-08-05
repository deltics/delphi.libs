
  unit Benchmark.Strings;

interface

  uses
    Deltics.Smoketest;


  type
    TStringPerformanceCase = class(TPerformanceCase, ICompareResults)
    private // ICompareResults
      procedure DefineComparisons(const aCompare: IDefineComparisons);
    end;


implementation

  uses
//    Benchmark.Strings.ANSI,
//    Benchmark.Strings.STR,
//    Benchmark.Strings.UTF8,
    Benchmark.Strings.WIDE;


{ TTestStrings ----------------------------------------------------------------------------------- }

  procedure TStringPerformanceCase.DefineComparisons(const aCompare: IDefineComparisons);
  begin
    aCompare.CompilerVersions('stringperformance.json');
    aCompare.WithResult('baseline');
  end;


initialization
  Smoketest.Compare([//TANSIPerformance,
                     //TSTRPerformance,
                     //TUTF8Performance,
                     TWIDEPerformance]).RunningFor(5).RunsOf(200).Iterations;
end.
