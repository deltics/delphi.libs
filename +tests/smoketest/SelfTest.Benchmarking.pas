

  unit SelfTest.Benchmarking;

{$i selftest.inc}

interface

  uses
    Deltics.Smoketest;


  type
    TTimerTest = class(TPerformanceCase)
      procedure RunsFor10thOfASecond;
      procedure RunsFor1Second;
      procedure RunsFor2Second;
    end;




implementation

  uses
  { vcl: }
    Windows;


{ TTimerTest ------------------------------------------------------------------------------------- }
















  procedure TTimerTest.RunsFor2Second;
  begin










  Smoketest.Time([TTimerTest]).RunningFor(5).Iterations;

end.
