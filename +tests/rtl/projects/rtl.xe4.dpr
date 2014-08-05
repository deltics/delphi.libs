
  program rtl.xe4;

uses
  FastMM4,
  Deltics.Smoketest,
  Test.JSON in '..\Test.JSON.pas',
  Test.Strings in '..\Test.Strings.pas',
  Test.Strings.ANSI in '..\Test.Strings.ANSI.pas',
  Test.Strings.UTF8 in '..\Test.Strings.UTF8.pas',
  Test.Strings.STR in '..\Test.Strings.STR.pas',
  Test.Strings.WIDE in '..\Test.Strings.WIDE.pas',
  Test.StringTemplates in '..\Test.StringTemplates.pas',
  Benchmark.Strings in '..\Benchmark.Strings.pas',
  Benchmark.Strings.WIDE in '..\Benchmark.Strings.WIDE.pas',
  Deltics.Strings in '..\..\..\rtl\Deltics.Strings.pas',
  Deltics.Strings.ANSI in '..\..\..\rtl\Deltics.Strings.ANSI.pas',
  Deltics.Strings.UTF8 in '..\..\..\rtl\Deltics.Strings.UTF8.pas',
  Deltics.Strings.WIDE in '..\..\..\rtl\Deltics.Strings.WIDE.pas',
  Deltics.Strings.WideStringList in '..\..\..\rtl\Deltics.Strings.WideStringList.pas',
  Deltics.StringTemplates in '..\..\..\rtl\Deltics.StringTemplates.pas';

begin
  Smoketest.Ready;
end.
