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
}


  unit Deltics.Tokeniser.Dictionary.Pascal;


interface

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;


  type
    TPascalDialect = (
                      pdTurboPascal,
                      pdFreePascal,
                      pdObjectPascal
                     );

  var
    DFMContent: TTokenDictionary = NIL;
    PascalLanguage: TTokenDictionary = NIL;
    PascalCompilerDirectives: TTokenDictionary = NIL;


  const
    // Symbolic Constants for Token Types - these may be used in constant expressions
    ttCompilerDirective = #2;
    ttInlineAssembler   = #10;

    ttComment       = #3;
    ttReservedWord  = #4;
    ttDirective     = #5;
    ttIdentifier    = #6;
    ttOperator      = #7;
    ttLiteral       = #8;
    ttSymbol        = #9;

    // Symbolic Constants for Token IDs - these may be used in constant expressions
    tkIdentifier        = 110;
    tkSymbol            = 120;
    tkEOL               = 130;
    tkInlineAssembler   = 140;

    // KEYWORDS -----------------------------------------------------
    //
    // Whether a keyword is a RESERVED word or not is dependent upon
    //  the dialect defined for the dictionary
    // --------------------------------------------------------------

    tk_whitespace       = 1000;

    tk_program          = 200;
    tk_library          = 201;
    tk_unit             = 202;
    tk_interface        = 203;
    tk_implementation   = 204;
    tk_initialization   = 205;
    tk_finalization     = 206;

    tk_asm              = 210;
    tk_file             = 211;
    tk_uses             = 212;
    tk_var              = 215;
    tk_type             = 216;
    tk_const            = 217;
    tk_absolute         = 218;
    tk_begin            = 219;
    tk_end              = 220;
    tk_class            = 221;
    tk_object           = 222;
    tk_record           = 223;
    tk_packed           = 224;
    tk_set              = 225;
    tk_implements       = 226;
    tk_private          = 227;
    tk_protected        = 228;
    tk_public           = 229;
    tk_published        = 230;
    tk_property         = 231;
    tk_read             = 232;
    tk_write            = 233;
    tk_default          = 234;
    tk_stored           = 235;
    tk_reintroduce      = 236;
    tk_virtual          = 237;
    tk_dynamic          = 238;
    tk_override         = 239;
    tk_overload         = 240;
    tk_in               = 241;
    tk_out              = 242;
    tk_automated        = 243;
    tk_deprecated       = 244;
    tk_raise            = 245;
    tk_try              = 246;
    tk_finally          = 247;
    tk_except           = 248;
    tk_procedure        = 249;
    tk_function         = 250;
    tk_inherited        = 251;
    tk_message          = 252;
    tk_dispid           = 253;
    tk_dispinterface    = 254;
    tk_resourcestring   = 255;
    tk_array            = 256;
    tk_range            = 257;
    tk_forward          = 258;
    tk_constructor      = 259;
    tk_destructor       = 260;
    tk_index            = 261;
    tk_threadvar        = 262;
    tk_inline           = 263;
    tk_abstract         = 264;
    tk_external         = 265;
    tk_reference        = 266;
    tk_resident         = 267;
    tk_readonly         = 268;
    tk_static           = 269;
    tk_writeonly        = 270;
    tk_nodefault        = 271;
    tk_platform         = 272;
    tk_delayed          = 273;
    tk_contains         = 274;
    tk_requires         = 275;

    tk_final            = 280;
    tk_helper           = 281;
    tk_operator         = 282;
    tk_sealed           = 283;
    tk_strict           = 284;

    tk_cdecl            = 291;
    tk_export           = 292;
    tk_far              = 293;
    tk_near             = 294;
    tk_pascal           = 295;
    tk_register         = 296;
    tk_safecall         = 297;
    tk_stdcall          = 298;
    tk_winapi           = 299;

    tk_with             = 300;
    tk_for              = 301;
    tk_while            = 302;
    tk_repeat           = 303;
    tk_do               = 304;
    tk_until            = 305;
    tk_case             = 306;
    tk_of               = 307;
    tk_if               = 308;
    tk_then             = 309;
    tk_else             = 320;
    tk_label            = 321;
    tk_goto             = 322;
    tk_to               = 323;
    tk_downto           = 324;
    tk_on               = 325;
    tk_is               = 326;
    tk_as               = 327;
    tk_at               = 328;

    tk_break            = 400;
    tk_exit             = 401;
    tk_continue         = 402;
    tk_assert           = 403;
    tk_halt             = 404;
    tk_abort            = 405;

    tkAssignment        = 500;
    tkAddition          = 501;
    tkSubtraction       = 502;
    tkDivision          = 503;
    tkMultiplication    = 504;
    tk_mod              = 505;
    tk_div              = 506;
    tk_shl              = 507;
    tk_shr              = 508;
    tk_not              = 509;
    tk_and              = 510;
    tk_or               = 511;
    tk_xor              = 512;

    tkAddressOf         = 520;
    tkEquality          = 521;
    tkNotEqual          = 522;
    tkLessThan          = 523;
    tkGreaterThan       = 524;
    tkNotLessThan       = 525;
    tkNotGreaterThan    = 526;

    tkString            = 600;
    tkInteger           = 601;
    tkFloat             = 602;
    tkHexadecimal       = 603;
    tkChar              = 604;

    tkDirective         = 700;

    tkLineComment       = 800;
    tkBlockComment      = 801;

    pdDelphi1           = 1;
    pdDelphi2           = 2;
    pdDelphi3           = 3;
    pdDelphi4           = 4;
    pdDelphi5           = 5;
    pdDelphi6           = 6;
    pdDelphi7           = 7;

    pdDelphi8           = 8;
    pdDelphi2005        = 9;
    pdDelphi2006        = 10;
    pdDelphi2007        = 11;
    pdDelphi2009        = 12;
    pdDelphi2010        = 14;
    pdDelphiXE          = 15;
    pdDelphiXE2         = 16;
    pdDelphiXE3         = 17;
    pdDelphiXE4         = 18;
    pdDelphiXE5         = 19;

    AllDelphiVersions   = [pdDelphi1,
                           pdDelphi2, pdDelphi3, pdDelphi4, pdDelphi5, pdDelphi6, pdDelphi7,
                           pdDelphi8, pdDelphi2005,
                           pdDelphi2006, pdDelphi2007, pdDelphi2009, pdDelphi2010,
                           pdDelphiXE, pdDelphiXE2, pdDelphiXE3, pdDelphiXE4, pdDelphiXE5];

    pdDelphi3AndLater       = AllDelphiVersions - [pdDelphi1, pdDelphi2];
    pdDelphi2006AndLater    = [pdDelphi2006, pdDelphi2007, pdDelphi2009, pdDelphi2010, pdDelphiXE, pdDelphiXE2, pdDelphiXE3, pdDelphiXE4, pdDelphiXE5];
    pdDelphi2007AndLater    = [pdDelphi2007, pdDelphi2009, pdDelphi2010, pdDelphiXE, pdDelphiXE2, pdDelphiXE3, pdDelphiXE4, pdDelphiXE5];
    pdDelphi2009AndLater    = [pdDelphi2009, pdDelphi2010, pdDelphiXE, pdDelphiXE2, pdDelphiXE3, pdDelphiXE4, pdDelphiXE5];





  const
    ttcd_Delimiters     =  #1;
    ttcd_Conditional    =  #2;
    ttcd_Directives     =  #3;
    ttcd_Options        =  #4;
    ttcd_Imports        =  #5;
    ttcd_ReservedWords  =  #6;
    ttcd_Symbols        =  #7;

    tkcd_BraceStar       = 1;
    tkcd_StarBrace       = 2;

    tkcd_ifdef         = 100;
    tkcd_ifndef        = 101;
    tkcd_ifopt         = 102;
    tkcd_else          = 103;
    tkcd_endif         = 104;
    tkcd_define        = 105;
    tkcd_undef         = 106;

    tkcd_include       = 300;
    tkcd_resource      = 301;
    tkcd_link          = 302;

    tkcd_on            = 400;
    tkcd_off           = 401;

    tkcd_Symbol        = 500;
    tkcd_Filename      = 501;




  const
    dfmObject           = 200;
    dfmEnd              = 201;
    dfmItem             = 202;
    dfmCollection       = 203;
    dfmString           = 204;
    dfmIdentifier       = 205;
    dfmBeginStringList  = 206;
    dfmEndStringList    = 207;
    dfmFalse            = 208;
    dfmTrue             = 209;
    dfmBeginSet         = 210;
    dfmEndSet           = 211;
    dfmInteger          = 212;
    dfmFloat            = 213;
    dfmTStoredProc      = 214;
    dfmStoredProcName   = 215;
    dfmSQLStrings       = 216;

    dfmConcat           = tkPlus;




implementation

  type
    TDFMContent = class(TTokenDictionary)
      procedure Initialise; override;
    end;


    TPascalLanguage = class(TTokenDictionary)
      procedure Initialise; override;
    end;

    TPascalCompilerDirectives = class(TTokenDictionary)
      procedure Initialise; override;
    end;



  procedure TDFMContent.Initialise;
  begin
    SetCaseSensitivity(FALSE);
    SetName('DFM Content');

    TokenType := ttWhitespace;
    AddCharSet(tk_whitespace, '[whitespace]', [' ', #9]);
    AddString(tkEOL, '[CR]',    #13);
    AddString(tkEOL, '[LF]',    #10);
    AddString(tkEOL, '[CRLF]',  #13#10);

    TokenType := ttReservedWord;
    AddString(dfmObject,          'object');
    AddString(dfmEnd,             'end');
    AddString(dfmItem,            'item');
    AddString(dfmFalse,           'false');
    AddString(dfmTrue,            'true');
    AddString(dfmTStoredProc,     'tstoredproc');
    AddString(dfmStoredProcName,  'storedprocname');
    AddString(dfmSQLStrings,      'SQL.Strings = (');
    AddString(dfmSQLStrings,      'ModifySQL.Strings = (');
    AddString(dfmSQLStrings,      'InsertSQL.Strings = (');
    AddString(dfmSQLStrings,      'DeleteSQL.Strings = (');

    TokenType := ttLiteral;
    AddDelimited(dfmString,         'String',   '''', '''');
    AddQualifiedCharSet(dfmInteger, 'Integer',  ['-', '+', '0'..'9'], ['0'..'9']);
    AddQualifiedCharSet(dfmFloat,   'Float',    ['-', '+', '0'..'9'], ['0'..'9', '.', '-', '+', 'e']);

    TokenType := ttIdentifier;
    AddQualifiedCharSet(dfmIdentifier, 'identifier', ['_', 'a'..'z'], ['a'..'z', '0'..'9', '_', '.'], uaAnywhere);

    TokenType := ttOperator;
    AddString(tkEquals,         '=');

    TokenType := ttSymbol;
    AddString(dfmBeginStringList, '(');
    AddString(dfmEndStringList,   ')');
    AddString(dfmBeginSet,        '[');
    AddString(dfmEndSet,          ']');
    AddASCII(tkColon);
    AddASCII(tkPlus);

    SetCompoundable([dfmString]);
  end;


  procedure TPascalLanguage.Initialise;
  begin
    SetCaseSensitivity(FALSE);
    SetName('Pascal Language');

    TokenType := ttWhitespace;
    AddCharSet(tk_whitespace, '[whitespace]', [' ', #9]);
    AddString(tkEOL, '[CR]',    #13);
    AddString(tkEOL, '[LF]',    #10);
    AddString(tkEOL, '[CRLF]',  #13#10);

    TokenType := ttDirective;
    AddDelimited(tkDirective, 'Compiler Directive', '{$',   '}');
    AddDelimited(tkDirective, 'Compiler Directive', '(*$',  '*)');
    SetSubDictionary(tkDirective, PascalCompilerDirectives);

    TokenType := ttComment;
    AddLineEnd(tkLineComment,   'Line Comment',  '//');
    AddDelimited(tkBlockComment,  'Block Comment', '{', '}',    TRUE);
    AddDelimited(tkBlockComment,  'Block Comment', '(*', '*)',  TRUE);

    TokenType := ttInlineAssembler;
    AddDelimited(tkInlineAssembler,  'Inline Assembler', 'asm', 'end',  TRUE);

    TokenType := ttReservedWord;
    AddString(tk_unit,             'unit');
    AddString(tk_interface,        'interface');
    AddString(tk_implementation,   'implementation');
    AddString(tk_uses,             'uses');
    AddString(tk_initialization,   'initialization');
    AddString(tk_finalization,     'finalization');

    AddString(tk_program, 'program');
    AddString(tk_library, 'library');

    AddString(tk_absolute,        'absolute');
    AddString(tk_abstract,        'abstract');
    AddString(tk_array,           'array');
    AddString(tk_automated,       'automated');
    AddString(tk_begin,           'begin');
    AddString(tk_cdecl,           'cdecl');
    AddString(tk_class,           'class');
    AddString(tk_const,           'const');
    AddString(tk_constructor,     'constructor');
    AddString(tk_contains,        'contains');
    AddString(tk_default,         'default');
    AddString(tk_delayed,         'delayed');
    AddString(tk_deprecated,      'deprecated');
    AddString(tk_destructor,      'destructor');
    AddString(tk_dispid,          'dispid');
    AddString(tk_dynamic,         'dynamic');
    AddString(tk_end,             'end');
    AddString(tk_except,          'except');
    AddString(tk_export,          'export');
    AddString(tk_external,        'external');
    AddString(tk_far,             'far');
    AddString(tk_file,            'file');
    AddString(tk_finalization,    'finalization');
    AddString(tk_finally,         'finally');
    AddString(tk_forward,         'forward');
    AddString(tk_function,        'function');
    AddString(tk_implementation,  'implementation');
    AddString(tk_implements,      'implements');
    AddString(tk_in,              'in');
    AddString(tk_index,           'index');
    AddString(tk_inherited,       'inherited');
    AddString(tk_initialization,  'initialization');
    AddString(tk_inline,          'inline',           pdDelphi2006AndLater);
    AddString(tk_interface,       'interface',        pdDelphi3AndLater);
    AddString(tk_message,         'message');
    AddString(tk_near,            'near');
    AddString(tk_nodefault,       'nodefault');
    AddString(tk_object,          'object');
    AddString(tk_out,             'out');
    AddString(tk_overload,        'overload');
    AddString(tk_override,        'override');
    AddString(tk_packed,          'packed');
    AddString(tk_pascal,          'pascal');
    AddString(tk_platform,        'platform');
    AddString(tk_private,         'private');
    AddString(tk_procedure,       'procedure');
    AddString(tk_property,        'property');
    AddString(tk_protected,       'protected');
    AddString(tk_public,          'public');
    AddString(tk_published,       'published');
    AddString(tk_raise,           'raise');
    AddString(tk_read,            'read');
    AddString(tk_readonly,        'readonly');
    AddString(tk_record,          'record');
    AddString(tk_reference,       'reference');
    AddString(tk_register,        'register');
    AddString(tk_reintroduce,     'reintroduce');
    AddString(tk_requires,        'requires');
    AddString(tk_resident,        'resident');
    AddString(tk_resourcestring,  'resourcestring');
    AddString(tk_safecall,        'safecall');
    AddString(tk_set,             'set');
    AddString(tk_static,          'static');
    AddString(tk_stdcall,         'stdcall');
    AddString(tk_stored,          'stored');
    AddString(tk_threadvar,       'threadvar');
    AddString(tk_try,             'try');
    AddString(tk_type,            'type');
    AddString(tk_var,             'var');
    AddString(tk_virtual,         'virtual');
    AddString(tk_winapi,          'winapi');
    AddString(tk_write,           'write');
    AddString(tk_writeonly,       'writeonly');

    AddString(tk_strict,          'strict',         pdDelphi2006AndLater);

    AddString(tk_helper,          'helper',         pdDelphi2007AndLater);
    AddString(tk_operator,        'operator',       pdDelphi2007AndLater);

    AddString(tk_final,           'final',          pdDelphi2009AndLater);
    AddString(tk_sealed,          'sealed',         pdDelphi2009AndLater);

    AddString(tk_as,     'as');
    AddString(tk_at,     'at');
    AddString(tk_case,   'case');
    AddString(tk_do,     'do');
    AddString(tk_downto, 'downto');
    AddString(tk_else,   'else');
    AddString(tk_for,    'for');
    AddString(tk_goto,   'goto');
    AddString(tk_if,     'if');
    AddString(tk_is,     'is');
    AddString(tk_label,  'label');
    AddString(tk_of,     'of');
    AddString(tk_on,     'on');
    AddString(tk_repeat, 'repeat');
    AddString(tk_then,   'then');
    AddString(tk_to,     'to');
    AddString(tk_until,  'until');
    AddString(tk_while,  'while');
    AddString(tk_with,   'with');
    AddString(tk_range,  '..');

    AddString(tk_abort,    'abort');
    AddString(tk_assert,   'assert');
    AddString(tk_break,    'break');
    AddString(tk_continue, 'continue');
    AddString(tk_exit,     'exit');
    AddString(tk_halt,     'halt');

    TokenType := ttLiteral;
    AddDelimited(tkString,              'String',      '''', '''');
    AddQualifiedCharSet(tkChar,         'Char',        ['#'], ['0'..'9']);
    AddQualifiedCharSet(tkInteger,      'Integer',     ['-', '+', '0'..'9'], ['0'..'9']);
    AddQualifiedCharSet(tkHexadecimal,  'Hexadecimal', ['$'], ['a'..'f', '0'..'9']);
    AddQualifiedCharSet(tkFloat,        'Float',       ['-', '+', '0'..'9'], ['0'..'9', '.', '-', '+', 'e']);

    TokenType := ttIdentifier;
    AddQualifiedCharSet(tkIdentifier,   'identifier',  ['_', 'a'..'z'], ['a'..'z', '0'..'9', '_'], uaAnywhere);
    AddQualifiedCharSet(tkIdentifier,   'identifier',  ['&'], ['a'..'z'], uaAnywhere);

    TokenType := ttOperator;
    AddString(tkAssignment,     ':=');
    AddString(tkAddition,       '+');
    AddString(tkSubtraction,    '-');
    AddString(tkDivision,       '/');
    AddString(tkMultiplication, '*');
    AddString(tkAddressOf,      '@');
    AddString(tkEquality,       '=');
    AddString(tkNotEqual,       '<>');
    AddString(tkLessThan,       '<');
    AddString(tkGreaterThan,    '>');
    AddString(tkNotLessThan,    '>=');
    AddString(tkNotGreaterThan, '<=');
    AddString(tk_mod,           'mod');
    AddString(tk_div,           'div');
    AddString(tk_shl,           'shl');
    AddString(tk_shr,           'shr');
    AddString(tk_not,           'not');
    AddString(tk_and,           'and');
    AddString(tk_or,            'or');
    AddString(tk_xor,           'xor');

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

    // Legacy Pascal notation which treats (. and .) as equivalent to square braces:
    AddString(tkLeftSquareBrace,  '[', '(.');
    AddString(tkRightSquareBrace, ']', '.)');

    // Indicate which token types are compoundable.  That is, contiguous sequences of
    //  tokens of that type are combined to form a single token of that type.
    //
    // For Pascal, the only such token is tkString (string literals).

    SetCompoundable([tkString])
  end;


  procedure TPascalCompilerDirectives.Initialise;
  begin
    SetName('Pascal Compiler Directives');
    TokenType := ttWhitespace;
    AddASCII([tkSpace, tkTab]);

    TokenType := ttcd_Delimiters;
    AddASCII([tkComma, tkLeftCurlyBrace, tkRightCurlyBrace]);
    AddString(tkcd_BraceStar, '(*');
    AddString(tkcd_StarBrace, '*)');

    TokenType := ttcd_Conditional;
    AddString(tkcd_ifdef,           '$ifdef');
    AddString(tkcd_ifndef,          '$ifndef');
    AddString(tkcd_ifopt,           '$ifopt');
    AddString(tkcd_else,            '$else');
    AddString(tkcd_endif,           '$endif');
    AddString(tkcd_define,          '$define');
    AddString(tkcd_undef,           '$undef');

    TokenType := ttcd_Directives;

    TokenType := ttcd_Options;

    TokenType := ttcd_Imports;
    AddString(tkcd_include,     '$i');
    AddString(tkcd_include,     '$include');
    AddString(tkcd_resource,    '$r');
    AddString(tkcd_resource,    '$res');
    AddString(tkcd_link,        '$l');
    AddString(tkcd_link,        '$link');

    TokenType := ttcd_ReservedWords;
    AddString(tkcd_on,  'on');
    AddString(tkcd_off, 'off');

    TokenType := ttcd_Symbols;
    AddCharSet(tkcd_Symbol,      'Symbol',   ['a'..'z', '_', '0'..'9']);
    AddCharSet(tkcd_Filename,    'Filename', ['a'..'z', '_', '0'..'9', '*', '.', ':', '\', '-', '(', ')', '~', '!']);
    AddDelimited(tkcd_Filename,  'Filename', '''', '''');
  end;



{ TDFMContent }


initialization
  DFMContent                := TDFMContent.Create;
  PascalLanguage            := TPascalLanguage.Create;
  PascalCompilerDirectives  := TPascalCompilerDirectives.Create;


finalization
  DFMContent.Free;
  PascalLanguage.Free;
  PascalCompilerDirectives.Free;
end.
