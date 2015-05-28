
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
    Benchmark.Strings.RTL,
    Benchmark.Strings.ANSI,
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
  Smoketest.Compare([TRTLPerformanceW,
                     TWIDEPerformance]).RunningFor(5).RunsOf(200).Iterations;

  Smoketest.Compare([TRTLPerformanceA,
                    {$ifdef FASTSTRINGS}
                     TFastStringsPerformance,
                    {$endif}
                     TANSIPerformance]).RunningFor(5).RunsOf(200).Iterations;
end.
