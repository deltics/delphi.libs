

  unit Deltics.Tokeniser.Dictionary.SQL92;


interface

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;


  var
    SQL89: TTokenDictionary = NIL;
    SQL92: TTokenDictionary = NIL;


  const
    ttComment         = #3;
    ttReservedWord    = #4;
    ttIdentifier      = #5;
    ttLiteral         = #6;
    ttSymbol          = #7;
    ttParameter       = #8;

    sqlWhitespace         = 100;
    sqlEOL                = 101;

    // SQL-86 reserved words
    //  - data types ?
    sqlAND                = 600;
    sqlASC                = 600;
    sqlBETWEEN            = 600;
    sqlBY                 = 600;
    sqlDELETE             = 600;
    sqlDESC               = 600;
    sqlDISTINCT           = 600;
    sqlFROM               = 600;
    sqlGROUP              = 600;
    sqlINSERT             = 600;
    sqlINTO               = 600;
    sqlNOT                = 600;
    sqlNULL               = 600;
    sqlORDER              = 600;
    sqlSELECT             = 600;
    sqlUPDATE             = 600;
    sqlVALUES             = 600;
    sqlWHERE              = 600;

    // SQL-89 reserved words
    //  - data types
    sqlCHAR               = 600;
    sqlCHARACTER          = 600;
    sqlDECIMAL            = 600;
    sqlDOUBLE             = 600;
    sqlFLOAT              = 600;
    sqlINT                = 600;
    sqlINTEGER            = 600;
    sqlSMALLINT           = 600;
    sqlNUMERIC            = 600;
    sqlPRECISION          = 600;
    sqlREAL               = 600;

    sqlADD                = 600;
    sqlALL                = 600;
    sqlAVG                = 600;
    sqlCOLUMN             = 600;
    sqlCONSTRAINT         = 600;
    sqlCOUNT              = 600;
    sqlCREATE             = 600;
    sqlDISALLOW           = 600;
    sqlFOREIGN            = 600;
    sqlGRANT              = 600;
    sqlHAVING             = 600;
    sqlIGNORE             = 600;
    sqlIN                 = 600;
    sqlINDEX              = 600;
    sqlINNER              = 600;
    sqlIS                 = 600;
    sqlJOIN               = 600;
    sqlKEY                = 600;
    sqlLEFT               = 600;
    sqlLIKE               = 600;
    sqlMAX                = 600;
    sqlMIN                = 600;
    sqlON                 = 600;
    sqlOR                 = 600;
    sqlPRIMARY            = 600;
    sqlREFERENCES         = 600;
    sqlRIGHT              = 600;
    sqlSET                = 600;
    sqlSTDEV              = 600;
    sqlSTDEVP             = 600;
    sqlSUM                = 600;
    sqlTABLE              = 600;
    sqlUNIQUE             = 600;
    sqlVAR                = 600;
    sqlVARP               = 600;
    sqlWITH               = 600;

    sqlIdentifier         = 600;

    // SQL-92 reserved words
    //  - Additional data types
    sqlBIT                = 600;
    sqlDATE               = 600;
    sqlINTERVAL           = 600;
    sqlNATIONAL           = 600;
    sqlNCHAR              = 600;
    sqlNVARCHAR           = 600;
    sqlTIME               = 600;
    sqlTIMESTAMP          = 600;
    sqlTIMESTAMPTZ        = 600;
    sqlTIMEZONE           = 600;
    sqlVARCHAR            = 600;
    sqlVARYING            = 600;
    sqlZONE               = 600;
    //  - Additional DDL
    sqlALTER              = 600;
    sqlDROP               = 600;
    sqlREVOKE             = 600;


    // SQL-99 reserved words
    sqlBOOLEAN            = 600;


    sqlDelimitedIdentifier  = 600;
    sqlParameter            = 800;

    sqlLiteralFloat         = 900;
    sqlLiteralInteger       = 900;
    sqlLiteralString        = 900;

    sqlNotEqual           = 1000;
    sqlPeriod             = tkPeriod;
    sqlComma              = tkComma;
    sqlEquals             = tkEquals;
    sqlLessThan           = tkLeftAngleBrace;
    sqlGreaterThan        = tkRightAngleBrace;
    sqlOpenParenthesis    = tkLeftParenthesis;
    sqlCloseParenthesis   = tkRightParenthesis;


  type
    TANSISQL = class(TTokenDictionary)
      procedure Initialise; override;
    end;

    TSQL89 = class(TANSISQL)
      procedure Initialise; override;
    end;

    TSQL92 = class(TSQL89)
      procedure Initialise; override;
    end;



implementation

{ TSQL86 }

  procedure TANSISQL.Initialise;
  var
    sequence: TArrayOfANSICharSet;
  begin
    SetCaseSensitivity(FALSE);
    SetName('SQL/86');

    TokenType := ttReservedWord;
    AddString(sqlAND,       'AND');
    AddString(sqlASC,       'ASC');
    AddString(sqlBETWEEN,   'BETWEEN');
    AddString(sqlBY,        'BY');
    AddString(sqlDELETE,    'DELETE');
    AddString(sqlDESC,      'DESC');
    AddString(sqlDISTINCT,  'DISTINCT');
    AddString(sqlFROM,      'FROM');
    AddString(sqlGROUP,     'GROUP');
    AddString(sqlINSERT,    'INSERT');
    AddString(sqlINTO,      'INTO');
    AddString(sqlNOT,       'NOT');
    AddString(sqlNULL,      'NULL');
    AddString(sqlORDER,     'ORDER');
    AddString(sqlSELECT,    'SELECT');
    AddString(sqlUPDATE,    'UPDATE');
    AddString(sqlVALUES,    'VALUES');
    AddString(sqlWHERE,     'WHERE');

    TokenType := ttLiteral;
    AddDelimited(sqlLiteralString,  'string', '''', '''');

    TokenType := ttSymbol;
    AddASCII(sqlComma);
    AddASCII(sqlPeriod);
    AddASCII(sqlOpenParenthesis);
    AddASCII(sqlCloseParenthesis);
    AddASCII(sqlEquals);
    AddASCII(sqlLessThan);
    AddASCII(sqlGreaterThan);
    AddASCII(sqlNotEqual);

    TokenType := ttWhitespace;
    AddCharSet(sqlWhitespace, '[whitespace]', [' ', #9]);
    AddString(sqlEOL, '[CR]',    #13);
    AddString(sqlEOL, '[LF]',    #10);
    AddString(sqlEOL, '[CRLF]',  #13#10);

    TokenType := ttLiteral;
    SetLength(sequence, 2);
    sequence[0] := ['-', '+', '0'..'9'];
    sequence[1] := ['0'..'9'];
    AddCharSet(sqlLiteralInteger, 'integer', sequence, 1);

    SetLength(sequence, 7);
    sequence[0] := ['-', '+', '0'..'9'];
    sequence[1] := ['0'..'9'];
    sequence[2] := ['.'];
    sequence[3] := ['0'..'9'];
    sequence[4] := ['e'];
    sequence[5] := ['+', '-', '0'..'9'];
    sequence[6] := ['0'..'9'];
    AddCharSet(sqlLiteralFloat, 'float', sequence, [3, 6]);
  end;


  procedure TSQL89.Initialise;
  begin
    inherited Initialise;
    SetName('SQL/89');

    TokenType := ttIdentifier;
    AddCharSet(sqlIdentifier, 'identifier', ['a'..'z'], ['a'..'z', '0'..'9',  '_']);

    TokenType := ttReservedWord;
    AddString(sqlCHAR,      'CHAR');
    AddString(sqlCHARACTER, 'CHARACTER');
    AddString(sqlDECIMAL,   'DECIMAL');
    AddString(sqlDOUBLE,    'DOUBLE');
    AddString(sqlFLOAT,     'FLOAT');
    AddString(sqlINT,       'INT');
    AddString(sqlINTEGER,   'INTEGER');
    AddString(sqlNUMERIC,   'NUMERIC');
    AddString(sqlPRECISION, 'PRECISION');
    AddString(sqlREAL,      'REAL');
    AddString(sqlSMALLINT,  'SMALLINT');

    TokenType := ttParameter;
    AddCharSet(sqlParameter, 'parameter', [':'], ['a'..'z', '0'..'9', '_', '.', '-']);
  end;


  procedure TSQL92.Initialise;
  begin
    inherited Initialise;
    SetName('SQL/92');

    TokenType := ttReservedWord;
    AddString(sqlBIT,         'BIT');
    AddString(sqlDATE,        'DATE');
    AddString(sqlINTERVAL,    'INTERVAL');
    AddString(sqlNATIONAL,    'NATIONAL');
    AddString(sqlNCHAR,       'NCHAR');
    AddString(sqlNVARCHAR,    'NVARCHAR');
    AddString(sqlTIME,        'TIME');
    AddString(sqlTIMESTAMP,   'TIMESTAMP');
    AddString(sqlTIMESTAMPTZ, 'TIMESTAMPTZ');
    AddString(sqlTIMEZONE,    'TIMEZONE');
    AddString(sqlVARCHAR,     'VARCHAR');
    AddString(sqlVARYING,     'VARYING');
    AddString(sqlZONE,        'ZONE');

    TokenType := ttIdentifier;
    AddDelimited(sqlDelimitedIdentifier, 'delimited identifier', '"', '"');
  end;



initialization
  SQL89  := TSQL89.Create;
  SQL92  := TSQL92.Create;

finalization
  SQL92.Free;
  SQL89.Free;

end.
