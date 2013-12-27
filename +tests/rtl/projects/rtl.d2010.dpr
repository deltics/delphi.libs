
  program rtl.d2010;

uses
  FastMM4,
  Deltics.Smoketest,
  Test.JSON in '..\Test.JSON.pas',
  Test.Strings.ANSI in '..\Test.Strings.ANSI.pas',
  Test.Strings in '..\Test.Strings.pas',
  Test.Strings.STR in '..\Test.Strings.STR.pas',
  Test.Strings.UTF8 in '..\Test.Strings.UTF8.pas',
  Test.Strings.WIDE in '..\Test.Strings.WIDE.pas',
  Benchmark.Strings in '..\Benchmark.Strings.pas',
  Benchmark.Strings.ANSI in '..\Benchmark.Strings.ANSI.pas',
  Benchmark.Strings.UTF8 in '..\Benchmark.Strings.UTF8.pas',
  Benchmark.Strings.WIDE in '..\Benchmark.Strings.WIDE.pas',
  Benchmark.Strings.STR in '..\Benchmark.Strings.STR.pas';

begin
  Smoketest.Ready;
end.
