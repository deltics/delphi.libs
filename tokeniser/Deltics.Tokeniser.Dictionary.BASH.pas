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


  unit Deltics.Tokeniser.Dictionary.BASH;


interface

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;


  var
    BASHLanguage: TTokenDictionary = NIL;


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
    bashLeftParenthesis    = tkLeftParenthesis;
    bashRightParenthesis   = tkRightParenthesis;
    bashLeftSquareBrace    = tkLeftSquareBrace;
    bashRightSquareBrace   = tkRightSquareBrace;
    bashSemiColon          = tkSemiColon;
    cblPeriod             = tkDot;
    cblComma              = tkComma;
    cblCircumflex         = tkCircumflex;

    // Symbolic Constants for Token IDs - these may be used in constant expressions
//    cblSymbol             = 100;
    bashEOL               = 100;
    bashWhitespace        = 1000;

    bashComment           = 150;


    // KEYWORDS -----------------------------------------------------
    //
    // Whether a keyword is a RESERVED word or not is dependent upon
    //  the dialect defined for the dictionary
    // --------------------------------------------------------------

    {#DI 200}
    bashCat                     = 200;
    bashEcho                    = 201;
    bashEndIf                   = 202;
    bashEval                    = 203;
    bashExitStatus              = 204;
    bashExport                  = 205;
    bashIf                      = 206;
    bashProcessID               = 207;
    bashRM                      = 208;
    bashSyncsort                = 209;
    {#ENDDI}

    // Literal tokens
    bashString                  = 600;
    bashInteger                 = 601;
    bashBackticks               = 602;

    // Identifier tokens
    bashRelativePath            = 700;
    bashIdentifier              = 701;
    bashVariable                = 702;


    // Operators - some of which are synomyms for ASCII chars

    bashEquals            = tkEquals;
    bashLessThan          = 800;
    bashGreaterThan       = 801;





implementation

  uses
    Deltics.Tokeniser.Tokens;

  type
    TBASHLanguage = class(TTokenDictionary)
      procedure Initialise; override;
    end;


  procedure TBASHLanguage.Initialise;
  const
    FILENAMECHARS : TANSICharSet = ['.', '/', '$', '{', '}', '_', '-', 'a'..'z', '0'..'9'];
  begin
    SetCaseSensitivity(FALSE);
    SetName('BASH');

    TokenType := ttWhitespace;
    AddCharSet(bashWhitespace, '[whitespace]', [' ', #9]);
    AddString(bashEOL, '[CR]',    #13);
    AddString(bashEOL, '[LF]',    #10);
    AddString(bashEOL, '[CRLF]',  #13#10);

    TokenType := ttComment;
    AddLineEnd(bashComment, 'Comment', '#');

    TokenType := ttReservedWord;
    AddString(bashCat,        'cat');
    AddString(bashEcho,       'echo');
    AddString(bashEndIf,      'fi');
    AddString(bashEval,       'eval');
    AddString(bashExport,     'export');
    AddString(bashIf,         'if');
    AddString(bashRM,         'rm');
    AddString(bashSyncsort,   'syncsort');
    AddString(bashExitStatus, '$?');
    AddString(bashProcessID,  '$$');

    TokenType := ttLiteral;
    AddDelimited(bashBackticks, 'Backticks',  '`', '`');
    SetInnerDictionary(bashBackticks, self);

    AddDelimited(bashString,    'String',     '''',   '''');
    AddDelimited(bashString,    'String',     '"',    '"');
    AddCharSet(bashInteger,     'Integer', ['0'..'9']);

    TokenType := ttIdentifier;
    AddQualifiedCharSet(bashIdentifier,   'identifier', ['0'..'9', 'a'..'z'], ['a'..'z', '0'..'9', '-', '_']);
    AddQualifiedCharSet(bashVariable,     'variable',   ['$'], ['0'..'9', 'a'..'z']);
    AddDelimitedCharSet(bashVariable,     'variable',   '${', '}', ['0'..'9', 'a'..'z'], FALSE);
    AddQualifiedCharSet(bashRelativePath, 'relative path', ['.'], FILENAMECHARS);

    TokenType := ttOperator;
    AddString(bashLessThan,        '-lt');
    AddString(bashGreaterThan,     '-gt');

    TokenType := ttSymbol;
    AddASCII(tkLeftParenthesis);
    AddASCII(tkRightParenthesis);
    AddASCII(tkLeftSquareBrace);
    AddASCII(tkRightSquareBrace);
    AddASCII(tkEquals);
    AddASCII(tkColon);
    AddASCII(tkSemiColon);
    AddASCII(tkDot);
    AddASCII(tkComma);
    AddASCII(tkCircumflex);
  end;





initialization
  BASHLanguage := TBASHLanguage.Create;

finalization
  BASHLanguage.Free;
end.
