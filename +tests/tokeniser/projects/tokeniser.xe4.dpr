
  program tokeniser.xe4;

uses
  FastMM4,
  Deltics.Smoketest,
  Deltics.Tokeniser in '..\..\..\tokeniser\Deltics.Tokeniser.pas',
  Deltics.Tokeniser.Consts in '..\..\..\tokeniser\Deltics.Tokeniser.Consts.pas',
  Deltics.Tokeniser.Dictionary in '..\..\..\tokeniser\Deltics.Tokeniser.Dictionary.pas',
  Deltics.Tokeniser.Dictionary.Pascal in '..\..\..\tokeniser\Deltics.Tokeniser.Dictionary.Pascal.pas',
  Deltics.Tokeniser.TokenReader in '..\..\..\tokeniser\Deltics.Tokeniser.TokenReader.pas',
  Deltics.Tokeniser.Tokens in '..\..\..\tokeniser\Deltics.Tokeniser.Tokens.pas',
  Pascal.Tests in '..\Pascal.Tests.pas';

begin
  Smoketest.Ready;
end.
