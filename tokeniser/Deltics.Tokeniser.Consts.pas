{
  * X11 (MIT) LICENSE *

  Copyright © 2006 Jolyon Smith

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

  ----------------------------------------------------------------------------------------

    Common and core Types and Constants used by the Deltics.Tokeniser
     implementation.

  ----------------------------------------------------------------------------------------
}

{$i deltics.tokeniser.inc}

{$ifdef deltics_tokeniser}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.Tokeniser.Consts;


interface

  type
    TDialectID  = 0..127;
    TTokenType  = #0..#127;
    TTokenID    = Integer;

    TDialects   = set of TDialectID;


  type
    TTokeniserOption = (
                          toCaseSensitive,
                          toConsumeWhitespace,
                          toNormaliseKeywords,
                          toNormaliseCase,
                          toRawStream
                         );
    TTokeniserOptions = set of TTokeniserOption;


  const
    ttNotSpecified = #0;
    ttWhitespace   = #1;

    // Built in token kind IDs for use with token navigation methods
    tkUnknown    = -1;
    tkAny        = -2;
    tkSameID     = -3;
    tkSameType   = -4;
    tkIdentical  = -5;

    // 'Built-in' token IDs for the ASCII character set (excluding letters
    //  and numerals)
    //
    // NOTE: These IDs are the ASCII/Char value of the corresponding
    //        ASCII char.  Some ASCII characters have multiple token IDs
    //        for other common names by which those symbols may be referred
    //
    //       e.g   tkMinus is also defined as tkDash and tkHyphen
    //
    // These are symbolic constants that may be used in constant expressions.
    //
    tkTab                     =  9;
    tkLF                      = 10;
    tkCR                      = 13;
    tkSpace                   = 32;
    tkExclamationMark         = 33;
    tkQuote                   = 34;
    tkHash                    = 35;
    tkDollar                  = 36;
    tkPercent                 = 37;
    tkAmpersand               = 38;
    tkApostrophe              = 39;
    tkLeftParenthesis         = 40;
    tkRightParenthesis        = 41;
    tkAsterisk                = 42;
    tkPlus                    = 43;
    tkComma                   = 44;
    tkMinus                   = 45;
    tkPeriod                  = 46;
    tkForwardSlash            = 47;
    { numeral digits occupy 48-57 ---------- }
    tkColon                   = 58;
    tkSemiColon               = 59;
    tkLessThan                = 60;
    tkEquals                  = 61;
    tkGreaterThan             = 62;
    tkQuestionMark            = 63;
    tkAtSign                  = 64;  // @ - Boring
    { uppercase letters occupy 65-90 ------- }
    tkLeftSquareBrace         = 91;
    tkBackslash               = 92;
    tkRightSquareBrace        = 93;
    tkCircumflex              = 94;
    tkUnderscore              = 95;
    tkTypographicApostrophe   = 96;
    { lowercase letters occupy 97-122 ------ }
    tkLeftCurlyBrace          = 123;
    tkPipe                    = 124;
    tkRightCurlyBrace         = 125;
    tkTilde                   = 126;

    // Additional  'Built-in' token ID's for common ASCII symbol sequences
    tkCRLF                    = 256;

    // Synonmys
    tkGate                    = tkHash;
    tkPound                   = tkHash;

    tkDash                    = tkMinus;
    tkHyphen                  = tkMinus;

    tkDot                     = tkPeriod;
    tkFullStop                = tkPeriod;

    tkLeftAngleBrace          = tkLessThan;
    tkRightAngleBrace         = tkGreaterThan;

    tkAmpersAt                = tkAtSign;  // @ - Variation on Ampersand
    tkApeStaart               = tkAtSign;  // @ - Dutch - 'Monkeys Tail' included because it made me smile


implementation

  { This unit has no implementation code }

end.
