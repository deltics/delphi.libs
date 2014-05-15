

  unit Deltics.Tokeniser.Dictionary.SQL;


interface

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;


  var
    CoreSQL: TTokenDictionary = NIL;


  const
    ttComment         = #3;
    ttReservedWord    = #4;
    ttIdentifier      = #5;
    ttLiteral         = #6;
    ttSymbol          = #7;
    ttIsolationLevel  = #8;

    sqlWhitespace         = 100;
    sqlEOL                = 101;

    sqlBegin              = 601;
    sqlCursor             = 602;
    sqlDeclare            = 603;
    sqlDELETE             = 604;
    sqlINSERT             = 608;
    sqlJOIN               = 609;
    sqlFROM               = 610;
    sqlINTO               = 611;
    sqlPrepare            = 612;
    sqlSection            = 613;
    sqlSET                = 614;
    sqlSELECT             = 615;
    sqlUPDATE             = 616;
    sqlWHERE              = 617;
    sqlLEFT               = 618;
    sqlINNER              = 619;
    sqlRIGHT              = 620;
    sqlOUTER              = 621;
    sqlFOR                = 622;
    sqlOF                 = 623;
    sqlON                 = 624;
    sqlIS                 = 625;
    sqlNOT                = 626;
    sqlNULL               = 627;
    sqlINDEX              = 628;
    sqlCREATE             = 629;
    sqlALTER              = 630;
    sqlGRANT              = 631;
    sqlCALL               = 632;
    sqlCOMMENT            = 633;
    sqlWITH               = 634;
    sqlOLD                = 635;
    sqlNEW                = 636;
    sqlFINAL              = 637;
    sqlTABLE              = 638;
    sqlAS                 = 639;
    sqlREPLACE            = 640;
    sqlIsolation_CS       = 641;
    sqlIsolation_RR       = 642;
    sqlIsolation_RS       = 643;
    sqlIsolation_UR       = 644;
    sqlNO                 = 645;
    sqlLOG                = 646;
    sqlHOLD               = 647;
    sqlRETURN             = 648;

    sqlQuotedIdentifier   = 700;
    sqlIdentifier         = 701;
    sqlInteger            = 702;
    sqlFloat              = 703;
    sqlString             = 704;

    sqlPeriod             = tkPeriod;
    sqlComma              = tkComma;
    sqlEquals             = tkEquals;
    sqlLessThan           = tkLeftAngleBrace;
    sqlGreaterThan        = tkRightAngleBrace;
    sqlOpenParenthesis    = tkLeftParenthesis;
    sqlCloseParenthesis   = tkRightParenthesis;


  type
    TSQLDictionary = class(TTokenDictionary)
      procedure Initialise; override;
    end;



implementation

{ TSQLDictionary }

  procedure TSQLDictionary.Initialise;
  var
    charSets: TArrayOfANSICharSet;
  begin
    SetCaseSensitivity(FALSE);
    SetName('ANSI(?) SQL');

    TokenType := ttWhitespace;
    AddCharSet(sqlWhitespace, '[whitespace]', [' ', #9]);
    AddString(sqlEOL, '[CR]',    #13);
    AddString(sqlEOL, '[LF]',    #10);
    AddString(sqlEOL, '[CRLF]',  #13#10);

    TokenType := ttIdentifier;
    AddCharSet(sqlIdentifier,         'identifier', ['a'..'z', '0'..'9',  '_']);
    AddDelimited(sqlQuotedIdentifier, 'quoted identifier', '"', '"');

    TokenType := ttLiteral;
    AddDelimited(sqlString,   'string', '''', '''');

    SetLength(charSets, 2);
    charSets[0] := ['-', '+', '0'..'9'];
    charSets[1] := ['0'..'9'];
    AddCharSet(sqlInteger, 'Integer', charSets, 1);

    SetLength(charSets, 7);
    charSets[0] := ['-', '+', '0'..'9'];
    charSets[1] := ['0'..'9'];
    charSets[2] := ['.'];
    charSets[3] := ['0'..'9'];
    charSets[4] := ['e'];
    charSets[5] := ['+', '-', '0'..'9'];
    charSets[6] := ['0'..'9'];
    AddCharSet(sqlFloat, 'Float', charSets, [3,6]);

    TokenType := ttReservedWord;
    AddString(sqlBegin,    'begin');
    AddString(sqlDeclare,  'declare');
    AddString(sqlSection,  'section');
    AddString(sqlFROM,     'from');
    AddString(sqlINTO,     'into');
    AddString(sqlJOIN,     'join');
    AddString(sqlSELECT,   'select');
    AddString(sqlSET,      'set');
    AddString(sqlINSERT,   'insert');
    AddString(sqlUPDATE,   'update');
    AddString(sqlDELETE,   'delete');
    AddString(sqlLEFT,     'left');
    AddString(sqlINNER,    'inner');
    AddString(sqlRIGHT,    'right');
    AddString(sqlOUTER,    'outer');
    AddString(sqlWHERE,    'where');
    AddString(sqlFOR,      'for');
    AddString(sqlOF,       'of');
    AddString(sqlON,       'on');
    AddString(sqlIS,       'is');
    AddString(sqlNOT,      'not');
    AddString(sqlNULL,     'null');
    AddString(sqlCREATE,   'create');
    AddString(sqlINDEX,    'index');
    AddString(sqlALTER,    'alter');
    AddString(sqlGRANT,    'grant');
    AddString(sqlCALL,     'call');
    AddString(sqlCOMMENT,  'comment');
    AddString(sqlWITH,     'with');
    AddString(sqlOLD,      'old');
    AddString(sqlNEW,      'new');
    AddString(sqlFINAL,    'final');
    AddString(sqlTABLE,    'table');
    AddString(sqlAS,       'as');
    AddString(sqlREPLACE,  'replace');
    AddString(sqlNO,       'no');
    AddString(sqlLOG,      'log');
    AddString(sqlHOLD,     'hold');
    AddString(sqlRETURN,   'return');

    TokenType := ttIsolationLevel;
    AddString(sqlIsolation_CS,  'cs');
    AddString(sqlIsolation_RR,  'rr');
    AddString(sqlIsolation_RS,  'rs');
    AddString(sqlIsolation_UR,  'ur');

    TokenType := ttSymbol;
    AddASCII(sqlComma);
    AddASCII(sqlPeriod);
    AddASCII(sqlOpenParenthesis);
    AddASCII(sqlCloseParenthesis);
    AddASCII(sqlEquals);
    AddASCII(sqlLessThan);
    AddASCII(sqlGreaterThan);
  end;



initialization
  CoreSQL  := TSQLDictionary.Create;

finalization
  CoreSQL.Free;

end.
