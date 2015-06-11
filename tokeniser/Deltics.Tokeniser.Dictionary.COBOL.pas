{
  * X11 (MIT) LICENSE *

  Copyright © 2014 Jolyon Smith

  Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
   so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.


  * GPL and Other Licenses *

  The FSF deem this license to be compatible with version 3 of the GPL.
   Compatability with other licenses should be verified by reference to those
   other license terms.


  * Contact Details *

  Original author : Jolyon Smith
  skype           : deltics
  e-mail          : <EXTLINK mailto: jsmith@deltics.co.nz>jsmith@deltics.co.nz</EXTLINK>
  website         : <EXTLINK http://www.deltics.co.nz>www.deltics.co.nz</EXTLINK>
}


  unit Deltics.Tokeniser.Dictionary.COBOL;


interface

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary,
    Deltics.Tokeniser.Dictionary.SQL;


  var
    COBOLLanguage: TTokenDictionary = NIL;
    EXECInterfaces: TTokenDictionary = NIL;
    COBOLEmbeddedSQL: TTokenDictionary = NIL;


  const
    // Symbolic Constants for Token Types - these may be used in constant expressions
    ttComment       = #3;
    ttReservedWord  = #4;
    ttDirective     = #5;
    ttIdentifier    = #6;
    ttOperator      = #7;
    ttLiteral       = #8;
    ttSymbol        = #9;
    ttEmbeddedSQL   = #10;

    // Simple synonyms for built-in ASCII char tokens
    cblLeftParenthesis    = tkLeftParenthesis;
    cblRightParenthesis   = tkRightParenthesis;
    cblLeftSquareBrace    = tkLeftSquareBrace;
    cblRightSquareBrace   = tkRightSquareBrace;
    cblColon              = tkColon;
    cblSemiColon          = tkSemiColon;
    cblPeriod             = tkDot;
    cblComma              = tkComma;
    cblCircumflex         = tkCircumflex;

    // Symbolic Constants for Token IDs - these may be used in constant expressions
    cblEOL                = 100;
    cblWhitespace         = 1000;

    cblLineNo             = 140;
    cblComment            = 150;
    cblFormFeed           = 151;
    cblContinuation       = 152;
    cblDebug              = 153;
    cblDirective          = 154;
    cblEmbeddedSQL        = 155;

    cblLineComment        = 160;


    // KEYWORDS -----------------------------------------------------
    //
    // Whether a keyword is a RESERVED word or not is dependent upon
    //  the dialect defined for the dictionary
    // --------------------------------------------------------------

    {#DI 200}
    cblAccept                   = 200;
    cblAdd                      = 201;
    cblAfter                    = 202;
    cblAll                      = 203;
    cblAlphanumeric             = 204;
    cblAre                      = 205;
    cblAs                       = 206;
    cblAssign                   = 207;
    cblAt                       = 208;
    cblAuthor                   = 209;
    cblBlock                    = 210;
    cblBy                       = 211;
    cblCall                     = 212;
    cblClose                    = 213;
    cblCommon                   = 214;
    cblComp                     = 215;
    cblComp3                    = 216;
    cblComp5                    = 217;
    cblCompute                  = 218;
    cblConfiguration            = 219;
    cblContains                 = 220;
    cblCopy                     = 221;
    cblCorresponding            = 222;
    cblData                     = 223;
    cblDelimited                = 224;
    cblDisplay                  = 225;
    cblDivision                 = 225;
    cblElse                     = 226;
    cblEnd                      = 227;
    cblEndEvaluate              = 228;
    cblEndExec                  = 229;
    cblEndIf                    = 230;
    cblEndPerform               = 231;
    cblEnvironment              = 232;
    cblEvaluate                 = 233;
    cblEXEC                     = 234;
    cblExit                     = 235;
    cblFalse                    = 236;
    cblFD                       = 237;
    cblFile                     = 238;
    cblFileControl              = 239;
    cblFiller                   = 240;
    cblFrom                     = 241;
    cblGo                       = 242;
    cblIdentification           = 243;
    cblIf                       = 244;
    cblIndexed                  = 245;
    cblInitial                  = 246;
    cblInitialize               = 247;
    cblInput                    = 248;
    cblInputOutput              = 249;
    cblInspect                  = 250;
    cblInto                     = 251;
    cblIs                       = 252;
    cblLabel                    = 253;
    cblLeading                  = 254;
    cblLine                     = 255;
    cblLinkage                  = 256;
    cblLocalStorage             = 257;
    cblMode                     = 258;
    cblMove                     = 259;
    cblNot                      = 260;
    cblObjectComputer           = 261;
    cblOccurs                   = 262;
    cblOf                       = 263;
    cblOpen                     = 264;
    cblOrganization             = 265;
    cblOther                    = 266;
    cblOutput                   = 267;
    cblPerform                  = 268;
    cblPIC                      = 269;
    cblProcedure                = 270;
    cblProgram                  = 271;
    cblProgramID                = 272;
    cblRead                     = 273;
    cblRedefines                = 274;
    cblRecording                = 275;
    cblRecords                  = 276;
    cblReplacing                = 277;
    cblRounded                  = 278;
    cblRun                      = 279;
    cblSection                  = 280;
    cblSelect                   = 281;
    cblSequential               = 282;
    cblSet                      = 283;
    cblSize                     = 284;
    cblSpace                    = 285;
    cblSpaces                   = 286;
    cblSourceComputer           = 287;
    cblStandard                 = 288;
    cblStop                     = 289;
    cblSubtract                 = 290;
    cblTimes                    = 291;
    cblTo                       = 292;
    cblTrue                     = 293;
    cblUntil                    = 294;
    cblUsing                    = 295;
    cblValue                    = 296;
    cblWhen                     = 297;
    cblWorkingStorage           = 298;
    cblWrite                    = 299;
    cblZero                     = 300;
    cblZeroes                   = 301;
    cblZeros                    = 302;
    {#ENDDI}

    // Literal tokens
    cblString                   = 600;
    cblDBCSLiteral              = 601;
    cblNationalLiteral          = 602;
    cblNullTerminatedString     = 603;
    cblInteger                  = 604;
    cblFloat                    = 605;
    cblHexLiteral               = 606;
    cblPictureString            = 607;

    // Identifier tokens
    cblIdentifier               = 700;
    cblHostIdentifier           = 701;


    // Operators - some of which are synomyms for ASCII chars

    cblOpAddition         = tkPlus;
    cblOpSubtraction      = tkMinus;
    cblOpDivision         = tkForwardSlash;
    cblOpMultiplication   = tkAsterisk;
    cblOpEquality         = 800;
    cblOpNotEqual         = 801;
    cblOpLessThan         = 802;
    cblOpGreaterThan      = 803;
    cblOpNotLessThan      = 804;
    cblOpNotGreaterThan   = 805;
    cblOpMod              = 806;
    cblOpNot              = 807;
    cblOpAnd              = 808;
    cblOpOr               = 809;


    cblEXECSQL            = 200;

    // COBOL SQL extensions

    sqlEndExec                    = 700;
    sqlExec                       = 701;
    sqlInclude                    = 702;
    sqlHostVariable               = 703;
    sqlHostVariableWithIndicator  = 704;
    sqlParagraph                  = 705;

    sqlString                     = Deltics.Tokeniser.Dictionary.SQL.sqlString;

    cdCOBOL2002 = 202;



implementation

  uses
    Deltics.Tokeniser.Tokens;


  type
    TCOBOLLanguage = class(TTokenDictionary)
      procedure Initialise; override;
    end;

    TEXECInterfaces = class(TTokenDictionary)
      procedure Initialise; override;
    end;

    TCOBOLEmbeddedSQL = class(TSQLDictionary)
      procedure Initialise; override;
    end;


  procedure TCOBOLLanguage.Initialise;
  var
    charSets: TArrayOfANSICharSet;
    charStrs: TArrayOfString;
  begin
    SetCaseSensitivity(FALSE);
    SetName('COBOL Language');
    SetTerminal(72);

    TokenType := ttWhitespace;
    AddCharSet(cblWhitespace, '[whitespace]', [' ', #9]);
    AddString(cblEOL, '[CR]',    #13);
    AddString(cblEOL, '[LF]',    #10);
    AddString(cblEOL, '[CRLF]',  #13#10);

    TokenType := ttComment;
    AddRange(cblLineNo,        'Line No', 1, 6);
    AddLineEnd(cblComment,     'Comment',      '*', 7);
    AddLineEnd(cblLineComment, 'Line Comment', '*>',  [cdCOBOL2002]);
    AddString(cblFormFeed,     'Form Feed',               '/', 7);
    AddLineEnd(cblFormFeed,    'Form Feed with Comment',  '/', 7);

    TokenType := ttDirective;
    AddString(cblDebug,         'DEBUG',          'D', 7);
    AddString(cblContinuation,  'Continuation',   '-', 7);
    AddLineEnd(cblDirective,    'Directive',      '$', 7);

    TokenType := ttEmbeddedSQL;
    AddDelimited(cblEXEC, 'EXEC Interface', 'exec', 'end-exec', TRUE);
    SetSubDictionary(cblEXEC, EXECInterfaces);

    TokenType := ttReservedWord;
    AddString(cblAccept,                  'accept');
    AddString(cblAdd,                     'add');
    AddString(cblAfter,                   'after');
    AddString(cblAll,                     'all');
    AddString(cblAlphanumeric,            'alphanumeric');
    AddString(cblAre,                     'are');
    AddString(cblAs,                      'as');
    AddString(cblAssign,                  'assign');
    AddString(cblAt,                      'at');
    AddString(cblAuthor,                  'author');
    AddString(cblBlock,                   'block');
    AddString(cblBy,                      'by');
    AddString(cblCall,                    'call');
    AddString(cblClose,                   'close');
    AddString(cblCommon,                  'common');
    AddString(cblComp,                    'comp');
    AddString(cblComp3,                   'comp-3');
    AddString(cblComp5,                   'comp-5');
    AddString(cblCompute,                 'compute');
    AddString(cblConfiguration,           'configuration');
    AddString(cblContains,                'contains');
    AddString(cblCopy,                    'copy');
    AddString(cblCorresponding,           'corresponding');
    AddString(cblData,                    'data');
    AddString(cblDelimited,               'delimited');
    AddString(cblDisplay,                 'display');
    AddString(cblDivision,                'division');
    AddString(cblElse,                    'else');
    AddString(cblEnd,                     'end');
    AddString(cblEndEvaluate,             'end-evaluate');
    AddString(cblEndIf,                   'end-if');
    AddString(cblEndPerform,              'end-perform');
    AddString(cblEnvironment,             'environment');
    AddString(cblEvaluate,                'evaluate');
    AddString(cblExit,                    'exit');
    AddString(cblFD,                      'fd');
    AddString(cblFalse,                   'false');
    AddString(cblFile,                    'file');
    AddString(cblFileControl,             'file-control');
    AddString(cblFiller,                  'filler');
    AddString(cblFrom,                    'from');
    AddString(cblGo,                      'go');
    AddString(cblIdentification,          'identification');
    AddString(cblIf,                      'if');
    AddString(cblIndexed,                 'indexed');
    AddString(cblInitial,                 'initial');
    AddString(cblInitialize,              'initialize');
    AddString(cblInspect,                 'inspect');
    AddString(cblInput,                   'input');
    AddString(cblInputOutput,             'input-output');
    AddString(cblInto,                    'into');
    AddString(cblIs,                      'is');
    AddString(cblLabel,                   'label');
    AddString(cblLeading,                 'leading');
    AddString(cblLine,                    'line');
    AddString(cblLinkage,                 'linkage');
    AddString(cblLocalStorage,            'local-storage');
    AddString(cblMode,                    'mode');
    AddString(cblMove,                    'move');
    AddString(cblNot,                     'not');
    AddString(cblObjectComputer,          'object-computer');
    AddString(cblOccurs,                  'occurs');
    AddString(cblOf,                      'of');
    AddString(cblOpen,                    'open');
    AddString(cblOrganization,            'organization');
    AddString(cblOther,                   'other');
    AddString(cblOutput,                  'output');
    AddString(cblPerform,                 'perform');
    AddString(cblPIC,                     'pic');
    AddString(cblPIC,                     'picture');
    AddString(cblProcedure,               'procedure');
    AddString(cblProgram,                 'program');
    AddString(cblProgramID,               'program-id');
    AddString(cblRead,                    'read');
    AddString(cblRecording,               'recording');
    AddString(cblRecords,                 'records');
    AddString(cblRedefines,               'redefines');
    AddString(cblReplacing,               'replacing');
    AddString(cblRounded,                 'rounded');
    AddString(cblRun,                     'run');
    AddString(cblSection,                 'section');
    AddString(cblSelect,                  'select');
    AddString(cblSequential,              'sequential');
    AddString(cblSet,                     'set');
    AddString(cblSize,                    'size');
    AddString(cblSourceComputer,          'source-computer');
    AddString(cblSpace,                   'space');
    AddString(cblSpaces,                  'spaces');
    AddString(cblStandard,                'standard');
    AddString(cblStop,                    'stop');
    AddString(cblString,                  'string');
    AddString(cblSubtract,                'subtract');
    AddString(cblTimes,                   'times');
    AddString(cblTo,                      'to');
    AddString(cblTrue,                    'true');
    AddString(cblUntil,                   'until');
    AddString(cblUsing,                   'using');
    AddString(cblValue,                   'value');
    AddString(cblWhen,                    'when');
    AddString(cblWorkingStorage,          'working-storage');
    AddString(cblWrite,                   'write');
    AddString(cblZero,                    'zero');
    AddString(cblZeroes,                  'zeroes');
    AddString(cblZeros,                   'zeros');

//    AddString(cblExec,    'exec');
//    AddString(cblEndExec, 'end-exec');

    TokenType := ttLiteral;
    AddDelimited(cblString,               'String',           '''',   '''');
    AddDelimited(cblString,               'String',           '"',    '"');
    AddDelimited(cblDBCSLiteral,          'DBCS Literal',     'G''',  '''');
    AddDelimited(cblDBCSLiteral,          'DBCS Literal',     'G"',   '"');
    AddDelimited(cblNationalLiteral,      'National Literal', 'N''',  '''');
    AddDelimited(cblNationalLiteral,      'National Literal', 'N"',   '"');
    AddDelimited(cblNullTerminatedString, 'Null-terminated',  'Z''',  '''');
    AddDelimited(cblNullTerminatedString, 'Null-terminated',  'Z"',   '"');
    AddDelimitedCharSet(cblHexLiteral,    'Hex Literal', 'X''', '''', ['a'..'f', '0'..'9']);
    AddDelimitedCharSet(cblHexLiteral,    'Hex Literal', 'X"',  '"',  ['a'..'f', '0'..'9']);

    SetLength(charSets, 2);
    charSets[0] := ['-', '+', '0'..'9'];
    charSets[1] := ['0'..'9'];
    AddCharSet(cblInteger, 'Integer', charSets, [1]);

    SetLength(charSets, 7);
    charSets[0] := ['-', '+', '0'..'9'];
    charSets[1] := ['0'..'9'];
    charSets[2] := ['.'];
    charSets[3] := ['0'..'9'];
    charSets[4] := ['e'];
    charSets[5] := ['+', '-', '0'..'9'];
    charSets[6] := ['0'..'9'];
    AddCharSet(cblFloat, 'Float', charSets, [3,6]);

    SetLength(charStrs, 5);
    charStrs[0] := 'ABGNPSXZ9/,+-*$';
    charStrs[1] := 'ABGNPXZ0123456789/,+-*$ESV.CRD()';
    charStrs[2] := ',.V';
    charStrs[3] := 'ABGNPXZ0123456789/,+-*$ESCRD()';
    charStrs[4] := 'ABGNPXZ9/*$ESRD';
    AddCharSet(cblPictureString, 'Picture', charStrs, [0,1,4]);

    TokenType := ttIdentifier;
    AddQualifiedCharSet(cblIdentifier,      'identifier',       ['0'..'9', 'a'..'z'], ['a'..'z', '0'..'9', '-', '_'], uaNowhere);

    SetLength(charSets, 3);
    charSets[0] := [':'];
    charSets[1] := ['0'..'9', 'a'..'z'];
    charSets[2] := ['0'..'9', 'a'..'z', '-'];
    AddCharSet(cblHostIdentifier,  'host identifier',  charSets, 2);

    TokenType := ttOperator;
    AddString(cblOpAddition,        '+');
    AddString(cblOpSubtraction,     '-');
    AddString(cblOpDivision,        '/');
    AddString(cblOpMultiplication,  '*');
    AddString(cblOpEquality,        '=');
    AddString(cblOpNotEqual,        '<>');
    AddString(cblOpLessThan,        '<');
    AddString(cblOpGreaterThan,     '>');
    AddString(cblOpNotLessThan,     '>=');
    AddString(cblOpNotGreaterThan,  '<=');
    AddString(cblOpMod,             'mod');
    AddString(cblOpNot,             'not');
    AddString(cblOpAnd,             'and');
    AddString(cblOpOr,              'or');

    TokenType := ttSymbol;
    AddASCII(tkLeftParenthesis);
    AddASCII(tkRightParenthesis);
    AddASCII(tkLeftSquareBrace);
    AddASCII(tkRightSquareBrace);
    AddASCII(tkColon);
    AddASCII(tkSemiColon);
    AddASCII(tkDot);
    AddASCII(tkComma);
    AddASCII(tkCircumflex);
  end;



{ TEXECInterfaces }

  procedure TEXECInterfaces.Initialise;
  begin
    SetCaseSensitivity(FALSE);
    SetName('EXEC Interface');

    TokenType := ttWhitespace;
    AddCharSet(cblWhitespace, '[whitespace]', [' ', #9]);
    AddString(cblEOL, '[CR]',    #13);
    AddString(cblEOL, '[LF]',    #10);
    AddString(cblEOL, '[CRLF]',  #13#10);

    TokenType := ttLiteral;
    AddString(cblEXEC,        'exec');
    AddDelimited(cblEXECSQL,  'SQL', 'sql', 'end-exec', TRUE);
    SetSubDictionary(cblEXECSQL, COBOLEmbeddedSQL);
  end;



{ TEmbeddedSQL }

  procedure TCOBOLEmbeddedSQL.Initialise;
  var
    charSets: TArrayOfANSICharSet;
  begin
    inherited;
    SetName('COBOL Embedded SQL');

    TokenType := ttComment;
    AddRange(cblLineNo,        'Line No', 1, 6);
    AddLineEnd(cblComment,     'Comment',      '*', 7);
    AddLineEnd(cblLineComment, 'Line Comment', '*>',  [cdCOBOL2002]);
    AddString(cblFormFeed,     'Form Feed',               '/', 7);
    AddLineEnd(cblFormFeed,    'Form Feed with Comment',  '/', 7);
    AddRange(sqlParagraph,     'Paragraph', 8, 72);

    TokenType := ttIdentifier;
    SetLength(charSets, 5);
    charSets[0] := [':'];
    charSets[1] := ['0'..'9', 'a'..'z'];
    charSets[2] := ['0'..'9', 'a'..'z', '-'];
    charSets[3] := ['.'];
    charSets[4] := ['0'..'9', 'a'..'z', '-'];
    AddCharSet(sqlHostVariable,  'host variable',  charSets, [2, 4]);

    SetLength(charSets, 6);
    charSets[0] := [':'];
    charSets[1] := ['0'..'9', 'a'..'z'];
    charSets[2] := ['0'..'9', 'a'..'z', '-'];
    charSets[3] := [':'];
    charSets[4] := ['0'..'9', 'a'..'z'];
    charSets[5] := ['0'..'9', 'a'..'z', '-'];
    AddCharSet(sqlHostVariableWithIndicator,  'host variable + ind',  charSets, [4, 5]);

    TokenType := ttReservedWord;
    AddString(cblEXECSQL,  'sql');
    AddString(sqlEndExec,  'end-exec');
    AddString(sqlInclude,  'include');
  end;




initialization
  COBOLEmbeddedSQL  := TCOBOLEmbeddedSQL.Create;
  EXECInterfaces    := TEXECInterfaces.Create;
  COBOLLanguage     := TCOBOLLanguage.Create;

finalization
  COBOLLanguage.Free;
  EXECInterfaces.Free;
  COBOLEmbeddedSQL.Free;
end.
