

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

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTimerTest.RunsFor10thOfASecond;
  begin
    Sleep(100);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTimerTest.RunsFor1Second;
  begin
    Sleep(1000);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTimerTest.RunsFor2Second;
  begin
    Sleep(2000);
  end;






initialization
  Smoketest.Time([TTimerTest]).RunningFor(5).Seconds;
  Smoketest.Time([TTimerTest]).RunningFor(5).Iterations;

end.
