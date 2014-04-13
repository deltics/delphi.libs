
  unit Deltics.Tokeniser.Dictionary.Text;

interface

uses
  Deltics.Tokeniser.Dictionary;


var
  LexText: TTokenDictionary = NIL;


const
  CTXT_Whitespace   = 1;
  CTXT_AlphaNumeric = 2;
  CTXT_DateTime     = 3;
  CTXT_Symbols      = 10;

  KTXT_Alpha        = 110;
  KTXT_Numeric      = 111;
  KTXT_AlphaNumeric = 112;

  KTXT_Date         = 120;
  KTXT_Time         = 121;


implementation

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Types;


initialization
  LexText := TTokenDictionary.Create;

  with LexText do
  begin
    AddCategory(tcWhitespace, 'Whitespace');
    AddToken(tkSpace);
    AddToken(tkTab);
    AddToken(tkCR);
    AddToken(tkLF);

    AddCategory(CTXT_AlphaNumeric, 'AlphaNumeric');
    AddCharSet(KTXT_Alpha,            'Alpha',        ['a'..'z']);
    AddCharSet(KTXT_AlphaNumeric,     'AlphaNumeric', ['a'..'z', '0'..'9']);
    AddQualifiedCharSet(KTXT_Numeric, 'Numeric',      ['-', '+', '0'..'9'], ['0'..'9', '.', '-', '+', 'e']);

    AddCategory(CTXT_DateTime, 'Date/Time');
    AddQualifiedCharSet(KTXT_Date, 'Date', ['a','d','f','j','m','n','o','s','0'..'9'],
                                           ['a'..'j','l'..'p','r'..'v','y','0'..'9','/','-',',']);
    AddQualifiedCharSet(KTXT_Time, 'Time', ['0'..'9'], ['0'..'9',':','a','m','p']);

    AddCategory(CTXT_Symbols, 'Symbols');
    AddToken(tkExclamationMark);
    AddToken(tkQuote);
    AddToken(tkHash);
    AddToken(tkDollar);
    AddToken(tkPercent);
    AddToken(tkAmpersand);
    AddToken(tkApostrophe);
    AddToken(tkLeftParenthesis);
    AddToken(tkRightParenthesis);
    AddToken(tkAsterisk);
    AddToken(tkPlus);
    AddToken(tkComma);
    AddToken(tkDash);
    AddToken(tkPeriod);
    AddToken(tkForwardSlash);
    AddToken(tkColon);
    AddToken(tkSemiColon);
    AddToken(tkLeftAngleBrace);
    AddToken(tkEquals);
    AddToken(tkRightAngleBrace);
    AddToken(tkQuestionMark);
    AddToken(tkAtSign);
    AddToken(tkLeftSquareBrace);
    AddToken(tkBackslash);
    AddToken(tkRightSquareBrace);
    AddToken(tkCircumflex);
    AddToken(tkUnderscore);
    AddToken(tkTypographicApostrophe);
    AddToken(tkLeftCurlyBrace);
    AddToken(tkPipe);
    AddToken(tkRightCurlyBrace);
    AddToken(tkTilde);
  end;

finalization
  LexText.Free;
end.
