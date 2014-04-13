
  unit Deltics.Tokeniser.Dictionary.JSON;

interface

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;


  var
    LexText: TTokenDictionary = NIL;


  const
    CTXT_Whitespace   = tcWhitespace;
    CTXT_Values       = 2;
    CTXT_Delimiters   = 3;

    KTXT_String       = 110;
    KTXT_Integer      = 112;
    KTXT_Number       = 113;
    KTXT_True         = 114;
    KTXT_False        = 115;
    KTXT_Null         = 116;

    KTXT_ArrayStart     = tkLeftSquareBrace;
    KTXT_ArrayEnd       = tkRightSquareBrace;

    KTXT_ObjectStart    = tkLeftCurlyBrace;
    KTXT_ObjectEnd      = tkRightCurlyBrace;

    KTXT_ValueSeparator = tkColon;


implementation

  uses
    Deltics.Tokeniser.Types;


initialization
  LexText := TTokenDictionary.Create('JSON');

  with LexText do
  begin
    AddCategory(tcWhitespace, 'Whitespace');
    AddToken(tkSpace);
    AddToken(tkTab);
    AddToken(tkCR);
    AddToken(tkLF);

    AddCategory(CTXT_Delimiters, 'Delimiters');
    AddToken(tkColon);
    AddToken(tkLeftSquareBrace);
    AddToken(tkRightquareBrace);
    AddToken(tkLeftCurlyBrace);
    AddToken(tkRightCurlyBrace);

    AddCategory(CTXT_Values, 'Values');
    Add
    AddCharSet(KTXT_StringAlpha,      'Alpha',        ['a'..'z']);
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
