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
    Deltics.Tokeniser.Dictionary;


  var
    COBOLLanguage: TTokenDictionary = NIL;


  const
    // Symbolic Constants for Token Types - these may be used in constant expressions
    ttComment       = #3;
    ttReservedWord  = #4;
    ttDirective     = #5;
    ttIdentifier    = #6;
    ttOperator      = #7;
    ttLiteral       = #8;
    ttSymbol        = #9;

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
//    cblSymbol             = 100;
    cblEOL                = 100;
    cblWhitespace         = 1000;

    cblComment            = 150;
    cblFormFeed           = 151;
    cblContinuation       = 152;
    cblDebug              = 153;
    cblDirective          = 154;

    cblLineComment        = 160;


    // KEYWORDS -----------------------------------------------------
    //
    // Whether a keyword is a RESERVED word or not is dependent upon
    //  the dialect defined for the dictionary
    // --------------------------------------------------------------

    cblAs                       = 200;
    cblAssign                   = 201;
    cblAuthor                   = 202;
    cblCall                     = 204;
    cblCommon                   = 205;
    cblCompute                  = 206;
    cblConfiguration            = 207;
    cblCopy                     = 208;
    cblData                     = 209;
    cblDisplay                  = 210;
    cblDivision                 = 211;
    cblEndExec                  = 212;
    cblEnvironment              = 213;
    cblExec                     = 214;
    cblFD                       = 215;
    cblFile                     = 216;
    cblFileControl              = 217;
    cblIdentificationDivision   = 218;
    cblInitial                  = 219;
    cblInputOutput              = 220;
    cblIs                       = 221;
    cblLinkage                  = 222;
    cblLocalStorage             = 223;
    cblMove                     = 224;
    cblObjectComputer           = 225;
    cblPIC                      = 226;
    cblProcedure                = 227;
    cblProgram                  = 228;
    cblProgramID                = 229;
    cblSection                  = 230;
    cblSelect                   = 231;
    cblSet                      = 232;
    cblSourceComputer           = 233;
    cblTo                       = 234;
    cblUsing                    = 235;
    cblValue                    = 236;
    cblWorkingStorage           = 237;

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



    cdCOBOL2002 = 202;



implementation

  type
    TCOBOLLanguage = class(TTokenDictionary)
      procedure Initialise; override;
    end;



  procedure TCOBOLLanguage.Initialise;
  var
    charSets: TArrayOfANSICharSet;
  begin
    SetCaseSensitivity(FALSE);
    SetName('COBOL Language');

    TokenType := ttWhitespace;
    AddCharSet(cblWhitespace, '[whitespace]', [' ', #9]);
    AddString(cblEOL, '[CR]',    #13);
    AddString(cblEOL, '[LF]',    #10);
    AddString(cblEOL, '[CRLF]',  #13#10);

    TokenType := ttComment;
    AddLineEnd(cblComment,     'Comment',           '*');
    AddLineEnd(cblFormFeed,    'Form Feed Comment', '/');
    AddLineEnd(cblLineComment, 'Line Comment',      '*>',  [cdCOBOL2002]);

    TokenType := ttDirective;
    AddCharset(cblDebug,            'DEBUG',          ['D','d']);
    AddLineEnd(cblDirective,        'Directive',      '$');
//    AddPositional(cblContinuation,  'Continuation',   '-', 7);

    TokenType := ttReservedWord;
    AddString(cblAs,                      'as');
    AddString(cblAssign,                  'assign');
    AddString(cblAuthor,                  'author');
    AddString(cblCall,                    'call');
    AddString(cblCommon,                  'common');
    AddString(cblCompute,                 'compute');
    AddString(cblConfiguration,           'configuration');
    AddString(cblCopy,                    'copy');
    AddString(cblData,                    'data');
    AddString(cblDisplay,                 'display');
    AddString(cblDivision,                'division');
    AddString(cblEnvironment,             'environment');
    AddString(cblFD,                      'fd');
    AddString(cblFile,                    'file');
    AddString(cblFileControl,             'file-control');
    AddString(cblIdentificationDivision,  'identification');
    AddString(cblInitial,                 'initial');
    AddString(cblInputOutput,             'input-output');
    AddString(cblIs,                      'is');
    AddString(cblLinkage,                 'linkage');
    AddString(cblLocalStorage,            'local-storage');
    AddString(cblMove,                    'move');
    AddString(cblObjectComputer,          'object-computer');
    AddString(cblPIC,                     'pic');
    AddString(cblPIC,                     'picture');
    AddString(cblProcedure,               'procedure');
    AddString(cblProgram,                 'program');
    AddString(cblProgramID,               'program-id');
    AddString(cblSection,                 'section');
    AddString(cblSelect,                  'select');
    AddString(cblSet,                     'set');
    AddString(cblSourceComputer,          'source-computer');
    AddString(cblTo,                      'to');
    AddString(cblUsing,                   'using');
    AddString(cblValue,                   'value');
    AddString(cblWorkingStorage,          'working-storage');

    AddString(cblExec,    'exec');
    AddString(cblEndExec, 'end-exec');


    TokenType := ttLiteral;
    AddDelimited(cblString,               'String',           '''',   '''');
    AddDelimited(cblString,               'String',           '"',    '"');
    AddDelimited(cblDBCSLiteral,          'DBCS Literal',     'G''',  '''');
    AddDelimited(cblDBCSLiteral,          'DBCS Literal',     'G"',   '"');
    AddDelimited(cblNationalLiteral,      'National Literal', 'N''',  '''');
    AddDelimited(cblNationalLiteral,      'National Literal', 'N"',   '"');
    AddDelimited(cblNullTerminatedString, 'Null-terminated',  'Z''',  '''');
    AddDelimited(cblNullTerminatedString, 'Null-terminated',  'Z"',   '"');
//    AddQualifiedCharSet(cblFloat,         'Float',       ['-', '+', '0'..'9'], ['0'..'9', '.', '-', '+', 'e']);
    AddDelimitedCharSet(cblHexLiteral,    'Hex Literal', 'X''', '''', ['a'..'f', '0'..'9']);
    AddDelimitedCharSet(cblHexLiteral,    'Hex Literal', 'X"',  '"',  ['a'..'f', '0'..'9']);

    SetLength(charSets, 2);
    charSets[0] := ['-', '+', '0'..'9'];
    charSets[1] := ['0'..'9'];
    AddCharSet(cblInteger, 'Integer', charSets, 1);

    SetLength(charSets, 7);
    charSets[0] := ['-', '+', '0'..'9'];
    charSets[1] := ['0'..'9'];
    charSets[2] := ['.'];
    charSets[3] := ['0'..'9'];
    charSets[4] := ['e'];
    charSets[5] := ['+', '-', '0'..'9'];
    charSets[6] := ['0'..'9'];
    AddCharSet(cblFloat, 'Float', charSets, 3);

    SetLength(charSets, 2);
    charSets[0] := ['A', 'B', 'G', 'N', 'P', 'X', 'Z', '9',
                    '/', ',', '+', '–', '*', '$',
                    'E', 'S', 'V', '.', 'C', 'R', 'D'];
    charSets[1] := ['A', 'B', 'G', 'N', 'P', 'X', 'Z', '0'..'9',
                    '/', ',', '+', '–', '*', '$',
                    'E', 'S', 'V', '.', 'C', 'R', 'D',
                    '(', ')'];
    AddCharSet(cblPictureString, 'Picture', charSets);


    TokenType := ttIdentifier;
    AddQualifiedCharSet(cblIdentifier,      'identifier',       ['0'..'9', 'a'..'z'], ['a'..'z', '0'..'9', '-'], ['a'..'z'], uaNowhere);
    AddQualifiedCharSet(cblHostIdentifier,  'host identifier',  [':'], ['a'..'z', '0'..'9', '-'], uaNowhere);

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



initialization
  COBOLLanguage := TCOBOLLanguage.Create;


finalization
  COBOLLanguage.Free;
end.
