
  unit Deltics.Tokeniser.Dictionary.CSharp;

interface

uses
  Deltics.Tokeniser.Dictionary;


var
  LexCSharp: TTokenDictionary = NIL;
  LexCSharpCD: TTokenDictionary = NIL;


const
  CCS_Whitespace   = 1;
  CCS_ReservedWord = 2;
  CCS_Directive    = 3;
  CCS_Comment      = 4;
  CCS_Operator     = 5;
  CCS_Literal      = 6;
  CCS_Identifier   = 7;

  KCS_Identifier   = 110;
  KCS_Symbol       = 120;

  // TODO: CSharp reserved words
  KCS_program        = 200;
  KCS_library        = 201;
  KCS_unit           = 202;
  KCS_initialization = 203;
  KCS_finalization   = 204;

  KCS_interface      = 210;
  KCS_implementation = 211;
  KCS_uses           = 212;
  KCS_var            = 215;
  KCS_type           = 216;
  KCS_const          = 217;
  KCS_absolute       = 218;
  KCS_begin          = 219;
  KCS_end            = 220;
  KCS_class          = 221;
  KCS_object         = 222;
  KCS_record         = 223;
  KCS_packed         = 224;
  KCS_set            = 225;
  KCS_implements     = 226;
  KCS_private        = 227;
  KCS_protected      = 228;
  KCS_public         = 229;
  KCS_published      = 230;
  KCS_property       = 231;
  KCS_read           = 232;
  KCS_write          = 233;
  KCS_default        = 234;
  KCS_stored         = 235;
  KCS_reintroduce    = 236;
  KCS_virtual        = 237;
  KCS_dynamic        = 238;
  KCS_override       = 239;
  KCS_overload       = 240;
  KCS_in             = 241;
  KCS_out            = 242;
  KCS_automated      = 243;
  KCS_deprecated     = 244;
  KCS_raise          = 245;
  KCS_try            = 246;
  KCS_finally        = 247;
  KCS_except         = 248;
  KCS_procedure      = 249;
  KCS_function       = 250;
  KCS_inherited      = 251;
  KCS_message        = 252;
  KCS_dispid         = 253;
  KCS_resourcestring = 254;

  KCS_with           = 300;
  KCS_for            = 301;
  KCS_while          = 302;
  KCS_repeat         = 303;
  KCS_do             = 304;
  KCS_until          = 305;
  KCS_case           = 306;
  KCS_of             = 307;
  KCS_if             = 308;
  KCS_then           = 309;
  KCS_else           = 320;
  KCS_label          = 321;
  KCS_goto           = 322;
  KCS_to             = 323;
  KCS_downto         = 324;
  KCS_on             = 325;
  KCS_is             = 326;
  KCS_as             = 327;

  KCS_break          = 400;
  KCS_exit           = 401;
  KCS_continue       = 402;
  KCS_assert         = 403;
  KCS_halt           = 404;
  KCS_abort          = 405;

  KCS_Assignment      = 500;
  KCS_Addition        = 501;
  KCS_Subtraction     = 502;
  KCS_Division        = 503;
  KCS_Multiplication  = 504;
  KCS_mod             = 505;
  KCS_div             = 506;
  KCS_shl             = 507;
  KCS_shr             = 508;
  KCS_not             = 509;
  KCS_and             = 510;
  KCS_or              = 511;
  KCS_xor             = 512;

  KCS_AddressOf       = 520;
  KCS_Equality        = 521;
  KCS_NotEqual        = 522;
  KCS_LessThan        = 523;
  KCS_GreaterThan     = 524;
  KCS_NotLessThan     = 525;
  KCS_NotGreaterThan  = 526;

  KCS_String          = 600;
  KCS_Integer         = 601;
  KCS_Float           = 602;
  KCS_Hexadecimal     = 603;

  KCS_Directive       = 700;

  KCS_LineComment     = 800;
  KCS_BlockComment    = 801;


const
  CCSCD_Delimiters     = 1;
  CCSCD_Conditional    = 2;
  CCSCD_Directives     = 3;
  CCSCD_Options        = 4;
  CCSCD_Imports        = 5;
  CCSCD_ReservedWords  = 6;
  CCSCD_Symbols        = 7;

  KCSCD_SlashStar      = 1;
  KCSCD_StarSlash      = 2;

  KCSCD_if              = 100;
  KCSCD_else            = 101;
  KCSCD_elif            = 102;
  KCSCD_endif           = 103;
  KCSCD_define          = 104;
  KCSCD_undef           = 105;
  KCSCD_warning         = 106;
  KCSCD_error           = 107;
  KCSCD_line            = 108;
  KCSCD_region          = 109;
  KCSCD_endregion       = 110;

  KCSCD_include      = 300;
  KCSCD_resource     = 301;
  KCSCD_link         = 302;

  KCSCD_on           = 400;
  KCSCD_off          = 401;

  KCSCD_Symbol       = 500;
  KCSCD_Filename     = 501;


implementation

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Types;


initialization
  LexCSharp := TTokenDictionary.Create('C#', TRUE);
  LexCSharpCD := TTokenDictionary.Create('C# Compiler Directives');

  with LexCSharpCD do
  begin
    AddCategory(tcWhitespace, 'Whitespace');
    AddToken(tkSpace);
    AddToken(tkTab);

    AddCategory(CCSCD_Delimiters, 'Delimiters');
    AddToken(tkComma);
    AddKeyword(KCSCD_SlashStar, '/*');
    AddKeyword(KCSCD_StarSlash, '*/');

    AddCategory(CCSCD_Directives, 'Directives');
    AddDelimited(KCSCD_if,        '#if',        '#if',        #0);
    AddDelimited(KCSCD_else,      '#else',      '#else',      #0);
    AddDelimited(KCSCD_elif,      '#elif',      '#elif',      #0);
    AddDelimited(KCSCD_endif,     '#endif',     '#endif',     #0);
    AddDelimited(KCSCD_define,    '#define',    '#define',    #0);
    AddDelimited(KCSCD_undef,     '#undef',     '#undef',     #0);
    AddDelimited(KCSCD_warning,   '#warning',   '#warning',   #0);
    AddDelimited(KCSCD_error,     '#error',     '#error',     #0);
    AddDelimited(KCSCD_line,      '#line',      '#line',      #0);
    AddDelimited(KCSCD_region,    '#region',    '#region',    #0);
    AddDelimited(KCSCD_endregion, '#endregion', '#endregion', #0);

    AddCategory(CCSCD_Options, 'Compiler Options');

    AddCategory(CCSCD_Imports, 'Includes and Imports');

    AddCategory(CCSCD_ReservedWords, 'Reserved Words');
    AddKeyword(KCSCD_on,  'on');
    AddKeyword(KCSCD_off, 'off');

    AddCategory(CCSCD_Symbols, 'Symbols');
    AddCharSet(KCSCD_Symbol,      'Symbol',   ['a'..'z', '_', '0'..'9']);
    AddCharSet(KCSCD_Filename,    'Filename', ['a'..'z', '_', '0'..'9', '*', '.', ':', '\', '-', '(', ')', '~', '!']);
    AddDelimited(KCSCD_Filename,  'Filename', '''', '''');
  end;

  with LexCSharp do
  begin
    AddCategory(tcWhitespace, 'Whitespace');
    AddToken(tkSpace);
    AddToken(tkTab);
    AddToken(tkCR);
    AddToken(tkLF);

    AddCategory(CCS_Directive, 'Compiler Directives');
//    AddDelimited(KCS_Directive,     'Compiler Directive', '{$', '}',   FALSE, [], TRUE, LexPascalCD);
//    AddDelimited(KCS_Directive,     'Compiler Directive', '(*$', '*)', FALSE, [], TRUE, LexPascalCD);

    AddCategory(CCS_Comment,   'Comment');
    AddDelimited(KCS_LineComment,   'Line Comment',  '//', #0);
    AddDelimited(KCS_BlockComment,  'Block Comment', '/*', '*/', TRUE);

(*
    AddCategory(CCS_ReservedWord, 'Reserved Words');
    AddKeyword(KCS_unit,             'unit');
    AddKeyword(KCS_interface,        'interface');
    AddKeyword(KCS_implementation,   'implementation');
    AddKeyword(KCS_uses,             'uses');
    AddKeyword(KCS_initialization,   'initialization');
    AddKeyword(KCS_finalization,     'finalization');

    AddKeyword(KCS_program, 'program');
    AddKeyword(KCS_library, 'library');
    AddKeyword(KCS_unit,    'unit');

    AddKeyword(KCS_interface,      'interface');
    AddKeyword(KCS_implementation, 'implementation');
    AddKeyword(KCS_uses,           'uses');
    AddKeyword(KCS_var,            'var');
    AddKeyword(KCS_type,           'type');
    AddKeyword(KCS_const,          'const');
    AddKeyword(KCS_absolute,       'absolute');
    AddKeyword(KCS_begin,          'begin');
    AddKeyword(KCS_end,            'end');
    AddKeyword(KCS_class,          'class');
    AddKeyword(KCS_object,         'object');
    AddKeyword(KCS_record,         'record');
    AddKeyword(KCS_packed,         'packed');
    AddKeyword(KCS_set,            'set');
    AddKeyword(KCS_implements,     'implements');
    AddKeyword(KCS_private,        'private');
    AddKeyword(KCS_protected,      'protected');
    AddKeyword(KCS_public,         'public');
    AddKeyword(KCS_published,      'published');
    AddKeyword(KCS_property,       'property');
    AddKeyword(KCS_read,           'read');
    AddKeyword(KCS_write,          'write');
    AddKeyword(KCS_default,        'default');
    AddKeyword(KCS_stored,         'stored');
    AddKeyword(KCS_reintroduce,    'reintroduce');
    AddKeyword(KCS_virtual,        'virtual');
    AddKeyword(KCS_dynamic,        'dynamic');
    AddKeyword(KCS_override,       'override');
    AddKeyword(KCS_overload,       'overload');
    AddKeyword(KCS_in,             'in');
    AddKeyword(KCS_out,            'out');
    AddKeyword(KCS_automated,      'automated');
    AddKeyword(KCS_deprecated,     'deprecated');
    AddKeyword(KCS_raise,          'raise');
    AddKeyword(KCS_try,            'try');
    AddKeyword(KCS_finally,        'finally');
    AddKeyword(KCS_except,         'except');
    AddKeyword(KCS_procedure,      'procedure');
    AddKeyword(KCS_function,       'function');
    AddKeyword(KCS_inherited,      'inherited');
    AddKeyword(KCS_message,        'message');
    AddKeyword(KCS_dispid,         'dispid');
    AddKeyword(KCS_resourcestring, 'resourcestring');

    AddKeyword(KCS_with,   'with');
    AddKeyword(KCS_for,    'for');
    AddKeyword(KCS_while,  'while');
    AddKeyword(KCS_repeat, 'repeat');
    AddKeyword(KCS_do,     'do');
    AddKeyword(KCS_until,  'until');
    AddKeyword(KCS_case,   'case');
    AddKeyword(KCS_of,     'of');
    AddKeyword(KCS_if,     'if');
    AddKeyword(KCS_then,   'then');
    AddKeyword(KCS_else,   'else');
    AddKeyword(KCS_label,  'label');
    AddKeyword(KCS_goto,   'goto');
    AddKeyword(KCS_to,     'to');
    AddKeyword(KCS_downto, 'downto');
    AddKeyword(KCS_on,     'on');
    AddKeyword(KCS_is,     'is');
    AddKeyword(KCS_as,     'as');

    AddKeyword(KCS_break,    'break');
    AddKeyword(KCS_exit,     'exit');
    AddKeyword(KCS_continue, 'continue');
    AddKeyword(KCS_assert,   'assert');
    AddKeyword(KCS_halt,     'halt');
    AddKeyword(KCS_abort,    'abort');
*)
    AddCategory(CCS_Literal, 'Literal');
    AddDelimited(KCS_String,               'String',      '"', '"');
    AddQualifiedCharSet(KCS_Integer,       'Char',        ['#'], ['0'..'9']);
    AddQualifiedCharSet(KCS_Integer,       'Integer',     ['-', '+', '0'..'9'], ['0'..'9']);
    AddQualifiedCharSet(KCS_Hexadecimal,   'Hexadecimal', ['$'], ['a'..'f', '0'..'9']);
    AddQualifiedCharSet(KCS_Float,         'Float',       ['-', '+', '0'..'9'], ['0'..'9', '.', '-', '+', 'e']);

    AddCategory(CCS_Identifier,  'Identifier');
    AddQualifiedCharSet(KCS_Identifier,    'identifier',  ['_', 'a'..'z'], ['a'..'z', '0'..'9', '_']);

    AddCategory(CCS_Operator,     'Operator');
    AddKeyword(KCS_Assignment,     ':=');
    AddKeyword(KCS_Addition,       '+');
    AddKeyword(KCS_Subtraction,    '-');
    AddKeyword(KCS_Division,       '/');
    AddKeyword(KCS_Multiplication, '*');
    AddKeyword(KCS_AddressOf,      '@');
    AddKeyword(KCS_Equality,       '=');
    AddKeyword(KCS_NotEqual,       '!=');
    AddKeyword(KCS_LessThan,       '<');
    AddKeyword(KCS_GreaterThan,    '>');
    AddKeyword(KCS_NotLessThan,    '>=');
    AddKeyword(KCS_NotGreaterThan, '<=');
    AddKeyword(KCS_mod,            'mod');
    AddKeyword(KCS_div,            'div');
    AddKeyword(KCS_shl,            'shl');
    AddKeyword(KCS_shr,            'shr');
    AddKeyword(KCS_not,            'not');
    AddKeyword(KCS_and,            'and');
    AddKeyword(KCS_or,             'or');
    AddKeyword(KCS_xor,            'xor');

    AddToken(tkLeftParenthesis);
    AddToken(tkRightParenthesis);
    AddToken(tkLeftSquareBrace);
    AddToken(tkRightSquareBrace);
    AddToken(tkColon);
    AddToken(tkSemiColon);
    AddToken(tkDot);
    AddToken(tkComma);
    AddToken(tkCircumflex);
  end;

finalization
  LexCSharp.Free;
  LexCSharpCD.Free;
end.
