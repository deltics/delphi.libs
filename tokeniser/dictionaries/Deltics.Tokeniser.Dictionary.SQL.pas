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

{$i Deltics.Tokeniser.inc}


  unit Deltics.Tokeniser.Dictionary.SQL;


interface

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;



  var
    SQL92: TTokenDictionary = NIL;


const
  _ttSQLLiteral        = #1;
  _ttSQLIdentifier     = #2;
  _ttSQLVariable       = #3;
  _ttSQLOperator       = #10;
  _ttSQLReservedWord   = #20;
  _ttSQLSymbol         = #30;

  ttSQLLiteral        : TTokenType = _ttSQLIdentifier;
  ttSQLIdentifier     : TTokenType = _ttSQLIdentifier;
  ttSQLVariable       : TTokenType = _ttSQLVariable;
  ttSQLOperator       : TTokenType = _ttSQLOperator;
  ttSQLReservedWord   : TTokenType = _ttSQLReservedWord;
  ttSQLSymbol         : TTokenType = _ttSQLSymbol;

  KSQL_FIRST_LITERAL          = 100;
  KSQL_FIRST_OPERATOR         = 200;
  KSQL_FIRST_RESERVED_WORD    = 300;
  KSQL_FIRST_NONRESERVED_WORD = 500;

  KSQL_Identifier             = 1000;
  KSQL_Variable               = 1001;


  // SQL-92 Literal Constants and Values
  KSQL_Literal_Integer    = KSQL_FIRST_LITERAL + 1;
  KSQL_Literal_Float      = KSQL_FIRST_LITERAL + 2;
  KSQL_Literal_String     = KSQL_FIRST_LITERAL + 3;
  KSQL_Literal_BitString  = KSQL_FIRST_LITERAL + 4;
  KSQL_Literal_HexString  = KSQL_FIRST_LITERAL + 5;
  KSQL_Literal_Date       = KSQL_FIRST_LITERAL + 6;
  KSQL_Literal_Time       = KSQL_FIRST_LITERAL + 7;


  // SQL-92 Reserved Words
  KSQL_Absolute           = KSQL_FIRST_RESERVED_WORD +   1;
  KSQL_Action             = KSQL_FIRST_RESERVED_WORD +   2;
  KSQL_Add                = KSQL_FIRST_RESERVED_WORD +   3;
  KSQL_All                = KSQL_FIRST_RESERVED_WORD +   4;
  KSQL_Allocate           = KSQL_FIRST_RESERVED_WORD +   5;
  KSQL_Alter              = KSQL_FIRST_RESERVED_WORD +   6;
  KSQL_And                = KSQL_FIRST_RESERVED_WORD +   7;
  KSQL_Any                = KSQL_FIRST_RESERVED_WORD +   8;
  KSQL_Are                = KSQL_FIRST_RESERVED_WORD +   9;
  KSQL_As                 = KSQL_FIRST_RESERVED_WORD +  10;
  KSQL_Asc                = KSQL_FIRST_RESERVED_WORD +  11;
  KSQL_Assertion          = KSQL_FIRST_RESERVED_WORD +  12;
  KSQL_At                 = KSQL_FIRST_RESERVED_WORD +  13;
  KSQL_Authorization      = KSQL_FIRST_RESERVED_WORD +  14;
  KSQL_Avg                = KSQL_FIRST_RESERVED_WORD +  15;
  KSQL_Begin              = KSQL_FIRST_RESERVED_WORD +  16;
  KSQL_Between            = KSQL_FIRST_RESERVED_WORD +  17;
  KSQL_Bit                = KSQL_FIRST_RESERVED_WORD +  18;
  KSQL_Bit_Length         = KSQL_FIRST_RESERVED_WORD +  19;
  KSQL_Both               = KSQL_FIRST_RESERVED_WORD +  20;
  KSQL_By                 = KSQL_FIRST_RESERVED_WORD +  21;
  KSQL_Cascade            = KSQL_FIRST_RESERVED_WORD +  22;
  KSQL_Cascaded           = KSQL_FIRST_RESERVED_WORD +  23;
  KSQL_Case               = KSQL_FIRST_RESERVED_WORD +  24;
  KSQL_Cast               = KSQL_FIRST_RESERVED_WORD +  25;
  KSQL_Catalog            = KSQL_FIRST_RESERVED_WORD +  26;
  KSQL_Char               = KSQL_FIRST_RESERVED_WORD +  27;
  KSQL_Character          = KSQL_FIRST_RESERVED_WORD +  29;
  KSQL_Character_Length   = KSQL_FIRST_RESERVED_WORD +  30;
  KSQL_Char_Length        = KSQL_FIRST_RESERVED_WORD +  31;
  KSQL_Check              = KSQL_FIRST_RESERVED_WORD +  32;
  KSQL_Close              = KSQL_FIRST_RESERVED_WORD +  33;
  KSQL_Coalesce           = KSQL_FIRST_RESERVED_WORD +  34;
  KSQL_Collate            = KSQL_FIRST_RESERVED_WORD +  35;
  KSQL_Collation          = KSQL_FIRST_RESERVED_WORD +  36;
  KSQL_Column             = KSQL_FIRST_RESERVED_WORD +  37;
  KSQL_Commit             = KSQL_FIRST_RESERVED_WORD +  38;
  KSQL_Connect            = KSQL_FIRST_RESERVED_WORD +  39;
  KSQL_Connection         = KSQL_FIRST_RESERVED_WORD +  40;
  KSQL_Constraint         = KSQL_FIRST_RESERVED_WORD +  41;
  KSQL_Constraints        = KSQL_FIRST_RESERVED_WORD +  42;
  KSQL_Continue           = KSQL_FIRST_RESERVED_WORD +  43;
  KSQL_Convert            = KSQL_FIRST_RESERVED_WORD +  44;
  KSQL_Corresponding      = KSQL_FIRST_RESERVED_WORD +  45;
  KSQL_Create             = KSQL_FIRST_RESERVED_WORD +  46;
  KSQL_Cross              = KSQL_FIRST_RESERVED_WORD +  47;
  KSQL_Current            = KSQL_FIRST_RESERVED_WORD +  48;
  KSQL_Current_Date       = KSQL_FIRST_RESERVED_WORD +  49;
  KSQL_Current_Time       = KSQL_FIRST_RESERVED_WORD +  50;
  KSQL_Current_Timestamp  = KSQL_FIRST_RESERVED_WORD +  51;
  KSQL_Current_User       = KSQL_FIRST_RESERVED_WORD +  52;
  KSQL_Cursor             = KSQL_FIRST_RESERVED_WORD +  53;
  KSQL_Date               = KSQL_FIRST_RESERVED_WORD +  54;
  KSQL_Day                = KSQL_FIRST_RESERVED_WORD +  55;
  KSQL_Deallocate         = KSQL_FIRST_RESERVED_WORD +  56;
  KSQL_Dec                = KSQL_FIRST_RESERVED_WORD +  57;
  KSQL_Decimal            = KSQL_FIRST_RESERVED_WORD +  58;
  KSQL_Declare            = KSQL_FIRST_RESERVED_WORD +  59;
  KSQL_Default            = KSQL_FIRST_RESERVED_WORD +  60;
  KSQL_Deferrable         = KSQL_FIRST_RESERVED_WORD +  61;
  KSQL_Deferred           = KSQL_FIRST_RESERVED_WORD +  62;
  KSQL_Delete             = KSQL_FIRST_RESERVED_WORD +  63;
  KSQL_Desc               = KSQL_FIRST_RESERVED_WORD +  64;
  KSQL_Describe           = KSQL_FIRST_RESERVED_WORD +  65;
  KSQL_Descriptor         = KSQL_FIRST_RESERVED_WORD +  66;
  KSQL_Diagnostics        = KSQL_FIRST_RESERVED_WORD +  67;
  KSQL_Disconnect         = KSQL_FIRST_RESERVED_WORD +  68;
  KSQL_Distinct           = KSQL_FIRST_RESERVED_WORD +  69;
  KSQL_Domain             = KSQL_FIRST_RESERVED_WORD +  70;
  KSQL_Double             = KSQL_FIRST_RESERVED_WORD +  71;
  KSQL_Drop               = KSQL_FIRST_RESERVED_WORD +  72;
  KSQL_Else               = KSQL_FIRST_RESERVED_WORD +  73;
  KSQL_End                = KSQL_FIRST_RESERVED_WORD +  74;
  KSQL_End_Exec           = KSQL_FIRST_RESERVED_WORD +  75;
  KSQL_Escape             = KSQL_FIRST_RESERVED_WORD +  76;
  KSQL_Except             = KSQL_FIRST_RESERVED_WORD +  77;
  KSQL_Exception          = KSQL_FIRST_RESERVED_WORD +  78;
  KSQL_Exec               = KSQL_FIRST_RESERVED_WORD +  79;
  KSQL_Execute            = KSQL_FIRST_RESERVED_WORD +  80;
  KSQL_Exists             = KSQL_FIRST_RESERVED_WORD +  81;
  KSQL_External           = KSQL_FIRST_RESERVED_WORD +  82;
  KSQL_Extract            = KSQL_FIRST_RESERVED_WORD +  83;
  KSQL_False              = KSQL_FIRST_RESERVED_WORD +  84;
  KSQL_Fetch              = KSQL_FIRST_RESERVED_WORD +  85;
  KSQL_First              = KSQL_FIRST_RESERVED_WORD +  86;
  KSQL_Float              = KSQL_FIRST_RESERVED_WORD +  87;
  KSQL_For                = KSQL_FIRST_RESERVED_WORD +  88;
  KSQL_Foreign            = KSQL_FIRST_RESERVED_WORD +  89;
  KSQL_Found              = KSQL_FIRST_RESERVED_WORD +  90;
  KSQL_From               = KSQL_FIRST_RESERVED_WORD +  91;
  KSQL_Full               = KSQL_FIRST_RESERVED_WORD +  92;
  KSQL_Get                = KSQL_FIRST_RESERVED_WORD +  93;
  KSQL_Global             = KSQL_FIRST_RESERVED_WORD +  94;
  KSQL_Go                 = KSQL_FIRST_RESERVED_WORD +  95;
  KSQL_Goto               = KSQL_FIRST_RESERVED_WORD +  96;
  KSQL_Grant              = KSQL_FIRST_RESERVED_WORD +  97;
  KSQL_Group              = KSQL_FIRST_RESERVED_WORD +  98;
  KSQL_Having             = KSQL_FIRST_RESERVED_WORD +  99;
  KSQL_Hour               = KSQL_FIRST_RESERVED_WORD + 100;
  KSQL_Identity           = KSQL_FIRST_RESERVED_WORD + 101;
  KSQL_Immediate          = KSQL_FIRST_RESERVED_WORD + 102;
  KSQL_In                 = KSQL_FIRST_RESERVED_WORD + 103;
  KSQL_Indicator          = KSQL_FIRST_RESERVED_WORD + 104;
  KSQL_Initially          = KSQL_FIRST_RESERVED_WORD + 105;
  KSQL_Inner              = KSQL_FIRST_RESERVED_WORD + 106;
  KSQL_Input              = KSQL_FIRST_RESERVED_WORD + 107;
  KSQL_Insensitive        = KSQL_FIRST_RESERVED_WORD + 108;
  KSQL_Insert             = KSQL_FIRST_RESERVED_WORD + 109;
  KSQL_Int                = KSQL_FIRST_RESERVED_WORD + 110;
  KSQL_Integer            = KSQL_FIRST_RESERVED_WORD + 111;
  KSQL_Intersect          = KSQL_FIRST_RESERVED_WORD + 112;
  KSQL_Interval           = KSQL_FIRST_RESERVED_WORD + 113;
  KSQL_Into               = KSQL_FIRST_RESERVED_WORD + 114;
  KSQL_Is                 = KSQL_FIRST_RESERVED_WORD + 115;
  KSQL_Isolation          = KSQL_FIRST_RESERVED_WORD + 116;
  KSQL_Join               = KSQL_FIRST_RESERVED_WORD + 117;
  KSQL_Key                = KSQL_FIRST_RESERVED_WORD + 118;
  KSQL_Language           = KSQL_FIRST_RESERVED_WORD + 119;
  KSQL_Last               = KSQL_FIRST_RESERVED_WORD + 120;
  KSQL_Leading            = KSQL_FIRST_RESERVED_WORD + 121;
  KSQL_Left               = KSQL_FIRST_RESERVED_WORD + 122;
  KSQL_Level              = KSQL_FIRST_RESERVED_WORD + 123;
  KSQL_Like               = KSQL_FIRST_RESERVED_WORD + 124;
  KSQL_Local              = KSQL_FIRST_RESERVED_WORD + 125;
  KSQL_Lower              = KSQL_FIRST_RESERVED_WORD + 126;
  KSQL_Match              = KSQL_FIRST_RESERVED_WORD + 127;
  KSQL_Max                = KSQL_FIRST_RESERVED_WORD + 128;
  KSQL_Min                = KSQL_FIRST_RESERVED_WORD + 129;
  KSQL_Minute             = KSQL_FIRST_RESERVED_WORD + 130;
  KSQL_Module             = KSQL_FIRST_RESERVED_WORD + 131;
  KSQL_Month              = KSQL_FIRST_RESERVED_WORD + 132;
  KSQL_Names              = KSQL_FIRST_RESERVED_WORD + 133;
  KSQL_National           = KSQL_FIRST_RESERVED_WORD + 134;
  KSQL_Natural            = KSQL_FIRST_RESERVED_WORD + 135;
  KSQL_Nchar              = KSQL_FIRST_RESERVED_WORD + 136;
  KSQL_Next               = KSQL_FIRST_RESERVED_WORD + 137;
  KSQL_No                 = KSQL_FIRST_RESERVED_WORD + 138;
  KSQL_Not                = KSQL_FIRST_RESERVED_WORD + 139;
  KSQL_Null               = KSQL_FIRST_RESERVED_WORD + 140;
  KSQL_Nullif             = KSQL_FIRST_RESERVED_WORD + 141;
  KSQL_Numeric            = KSQL_FIRST_RESERVED_WORD + 142;
  KSQL_Octet_Length       = KSQL_FIRST_RESERVED_WORD + 143;
  KSQL_Of                 = KSQL_FIRST_RESERVED_WORD + 144;
  KSQL_On                 = KSQL_FIRST_RESERVED_WORD + 145;
  KSQL_Only               = KSQL_FIRST_RESERVED_WORD + 146;
  KSQL_Open               = KSQL_FIRST_RESERVED_WORD + 147;
  KSQL_Option             = KSQL_FIRST_RESERVED_WORD + 148;
  KSQL_Or                 = KSQL_FIRST_RESERVED_WORD + 149;
  KSQL_Order              = KSQL_FIRST_RESERVED_WORD + 150;
  KSQL_Outer              = KSQL_FIRST_RESERVED_WORD + 151;
  KSQL_Output             = KSQL_FIRST_RESERVED_WORD + 152;
  KSQL_Overlaps           = KSQL_FIRST_RESERVED_WORD + 153;
  KSQL_Pad                = KSQL_FIRST_RESERVED_WORD + 154;
  KSQL_Partial            = KSQL_FIRST_RESERVED_WORD + 155;
  KSQL_Position           = KSQL_FIRST_RESERVED_WORD + 156;
  KSQL_Precision          = KSQL_FIRST_RESERVED_WORD + 157;
  KSQL_Prepare            = KSQL_FIRST_RESERVED_WORD + 158;
  KSQL_Preserve           = KSQL_FIRST_RESERVED_WORD + 159;
  KSQL_Primary            = KSQL_FIRST_RESERVED_WORD + 160;
  KSQL_Prior              = KSQL_FIRST_RESERVED_WORD + 161;
  KSQL_Privileges         = KSQL_FIRST_RESERVED_WORD + 162;
  KSQL_Procedure          = KSQL_FIRST_RESERVED_WORD + 163;
  KSQL_Public             = KSQL_FIRST_RESERVED_WORD + 164;
  KSQL_Read               = KSQL_FIRST_RESERVED_WORD + 165;
  KSQL_Real               = KSQL_FIRST_RESERVED_WORD + 166;
  KSQL_References         = KSQL_FIRST_RESERVED_WORD + 167;
  KSQL_Relative           = KSQL_FIRST_RESERVED_WORD + 168;
  KSQL_Restrict           = KSQL_FIRST_RESERVED_WORD + 169;
  KSQL_Revoke             = KSQL_FIRST_RESERVED_WORD + 170;
  KSQL_Right              = KSQL_FIRST_RESERVED_WORD + 171;
  KSQL_Rollback           = KSQL_FIRST_RESERVED_WORD + 172;
  KSQL_Rows               = KSQL_FIRST_RESERVED_WORD + 173;
  KSQL_Schema             = KSQL_FIRST_RESERVED_WORD + 174;
  KSQL_Scroll             = KSQL_FIRST_RESERVED_WORD + 175;
  KSQL_Second             = KSQL_FIRST_RESERVED_WORD + 176;
  KSQL_Section            = KSQL_FIRST_RESERVED_WORD + 177;
  KSQL_Select             = KSQL_FIRST_RESERVED_WORD + 178;
  KSQL_Session            = KSQL_FIRST_RESERVED_WORD + 179;
  KSQL_Session_User       = KSQL_FIRST_RESERVED_WORD + 180;
  KSQL_Set                = KSQL_FIRST_RESERVED_WORD + 181;
  KSQL_Size               = KSQL_FIRST_RESERVED_WORD + 182;
  KSQL_Smallint           = KSQL_FIRST_RESERVED_WORD + 183;
  KSQL_Some               = KSQL_FIRST_RESERVED_WORD + 184;
  KSQL_Space              = KSQL_FIRST_RESERVED_WORD + 185;
  KSQL_Sql                = KSQL_FIRST_RESERVED_WORD + 186;
  KSQL_Sqlcode            = KSQL_FIRST_RESERVED_WORD + 187;
  KSQL_Sqlerror           = KSQL_FIRST_RESERVED_WORD + 188;
  KSQL_Sqlstate           = KSQL_FIRST_RESERVED_WORD + 189;
  KSQL_Substring          = KSQL_FIRST_RESERVED_WORD + 190;
  KSQL_Sum                = KSQL_FIRST_RESERVED_WORD + 191;
  KSQL_System_user        = KSQL_FIRST_RESERVED_WORD + 192;
  KSQL_Table              = KSQL_FIRST_RESERVED_WORD + 193;
  KSQL_Temporary          = KSQL_FIRST_RESERVED_WORD + 194;
  KSQL_Then               = KSQL_FIRST_RESERVED_WORD + 195;
  KSQL_Time               = KSQL_FIRST_RESERVED_WORD + 196;
  KSQL_Timestamp          = KSQL_FIRST_RESERVED_WORD + 197;
  KSQL_Timezone_Hour      = KSQL_FIRST_RESERVED_WORD + 198;
  KSQL_Timezone_Minute    = KSQL_FIRST_RESERVED_WORD + 199;
  KSQL_To                 = KSQL_FIRST_RESERVED_WORD + 200;
  KSQL_Trailing           = KSQL_FIRST_RESERVED_WORD + 201;
  KSQL_Transaction        = KSQL_FIRST_RESERVED_WORD + 202;
  KSQL_Translate          = KSQL_FIRST_RESERVED_WORD + 203;
  KSQL_Translation        = KSQL_FIRST_RESERVED_WORD + 204;
  KSQL_Trim               = KSQL_FIRST_RESERVED_WORD + 205;
  KSQL_True               = KSQL_FIRST_RESERVED_WORD + 206;
  KSQL_Union              = KSQL_FIRST_RESERVED_WORD + 207;
  KSQL_Unique             = KSQL_FIRST_RESERVED_WORD + 208;
  KSQL_Unknown            = KSQL_FIRST_RESERVED_WORD + 209;
  KSQL_Update             = KSQL_FIRST_RESERVED_WORD + 210;
  KSQL_Upper              = KSQL_FIRST_RESERVED_WORD + 211;
  KSQL_Usage              = KSQL_FIRST_RESERVED_WORD + 212;
  KSQL_User               = KSQL_FIRST_RESERVED_WORD + 213;
  KSQL_Using              = KSQL_FIRST_RESERVED_WORD + 214;
  KSQL_Value              = KSQL_FIRST_RESERVED_WORD + 215;
  KSQL_Values             = KSQL_FIRST_RESERVED_WORD + 216;
  KSQL_Varchar            = KSQL_FIRST_RESERVED_WORD + 217;
  KSQL_Varying            = KSQL_FIRST_RESERVED_WORD + 218;
  KSQL_View               = KSQL_FIRST_RESERVED_WORD + 219;
  KSQL_When               = KSQL_FIRST_RESERVED_WORD + 220;
  KSQL_Whenever           = KSQL_FIRST_RESERVED_WORD + 221;
  KSQL_Where              = KSQL_FIRST_RESERVED_WORD + 222;
  KSQL_With               = KSQL_FIRST_RESERVED_WORD + 223;
  KSQL_Work               = KSQL_FIRST_RESERVED_WORD + 224;
  KSQL_Write              = KSQL_FIRST_RESERVED_WORD + 225;
  KSQL_Year               = KSQL_FIRST_RESERVED_WORD + 226;
  KSQL_Zone               = KSQL_FIRST_RESERVED_WORD + 227;

  // Special tokens
  KSQL_Percentage         = KSQL_FIRST_NONRESERVED_WORD + 1;
  KSQL_KiloByte           = KSQL_FIRST_NONRESERVED_WORD + 2;
  KSQL_MegaByte           = KSQL_FIRST_NONRESERVED_WORD + 3;
  KSQL_GigaByte           = KSQL_FIRST_NONRESERVED_WORD + 4;

  // SQL-92 Operators
  KSQL_Addition       = KSQL_FIRST_OPERATOR +   1;
  KSQL_Subtraction    = KSQL_FIRST_OPERATOR +   2;
  KSQL_Division       = KSQL_FIRST_OPERATOR +   3;
  KSQL_Multiplication = KSQL_FIRST_OPERATOR +   4;
  KSQL_Equality       = KSQL_FIRST_OPERATOR +   5;
  KSQL_NotEqual       = KSQL_FIRST_OPERATOR +   6;
  KSQL_LessThan       = KSQL_FIRST_OPERATOR +   7;
  KSQL_GreaterThan    = KSQL_FIRST_OPERATOR +   8;
  KSQL_NotLessThan    = KSQL_FIRST_OPERATOR +   9;
  KSQL_NotGreaterThan = KSQL_FIRST_OPERATOR +  10;



implementation

  uses
    Deltics.Tokeniser.Types;


  type
    TSQLDictionary = class(TTokenDictionary)
      procedure Initialise; override;
    end;

  procedure TSQLDictionary.Initialise;
  begin
    TokenType := ttWhitespace;
    AddASCII([tkSpace, tkTab, tkCR, tkLF, tkCRLF]);

    TokenType := ttSQLIdentifier;
    AddQualifiedCharset(KSQL_Identifier,  'Identifier', ['_', 'a'..'z'], ['a'..'z', '0'..'9', '_']);
    AddDelimited(KSQL_Identifier,         'Identifier', '"', '"');
    AddDelimited(KSQL_Identifier,         'Identifier', '[', ']');
    AddQualifiedCharset(KSQL_Variable,    'Variable', [':'], ['a'..'z', '0'..'9', '_']);

    TokenType := ttSQLLiteral;
    AddDelimited(KSQL_Literal_String,    'String',      '''', '''');
    AddQualifiedCharset(KSQL_Literal_Integer,   'Integer',     ['-', '+', '0'..'9'], ['0'..'9']);
    AddQualifiedCharset(KSQL_Literal_Float,     'Float',       ['.', '-', '+', '0'..'9'], ['0'..'9', '.', '-', '+', 'e']);
    AddQualifiedCharset(KSQL_Literal_BitString, 'Bit String',  ['b'], ['''', '0', '1']);
    AddQualifiedCharset(KSQL_Literal_HexString, 'Hex String',  ['x'], ['''', '0'..'9', 'a'..'f']);
    AddQualifiedCharset(KSQL_Literal_HexString, 'Hex String',  ['0'], ['x', '''', '0'..'9', 'a'..'f']);
    AddQualifiedCharset(KSQL_Literal_Date,      'Date',        ['0'..'9'], ['0'..'9', '-']);
    AddQualifiedCharset(KSQL_Literal_Time,      'Time',        ['0'..'9'], ['0'..'9', ':']);

    TokenType := ttSQLReservedWord;
    AddString(KSQL_Absolute,          'absolute');
    AddString(KSQL_Action,            'action');
    AddString(KSQL_Add,               'add');
    AddString(KSQL_All,               'all');
    AddString(KSQL_Allocate,          'allocate');
    AddString(KSQL_Alter,             'alter');
    AddString(KSQL_And,               'and');
    AddString(KSQL_Any,               'any');
    AddString(KSQL_Are,               'are');
    AddString(KSQL_As,                'as');
    AddString(KSQL_Asc,               'asc');
    AddString(KSQL_Assertion,         'assertion');
    AddString(KSQL_At,                'at');
    AddString(KSQL_Authorization,     'authorization');
    AddString(KSQL_Avg,               'avg');
    AddString(KSQL_Begin,             'begin');
    AddString(KSQL_Between,           'between');
    AddString(KSQL_Bit,               'bit');
    AddString(KSQL_Bit_Length,        'bit_Length');
    AddString(KSQL_Both,              'both');
    AddString(KSQL_By,                'by');
    AddString(KSQL_Cascade,           'cascade');
    AddString(KSQL_Cascaded,          'cascaded');
    AddString(KSQL_Case,              'case');
    AddString(KSQL_Cast,              'cast');
    AddString(KSQL_Catalog,           'catalog');
    AddString(KSQL_Char,              'char');
    AddString(KSQL_Character,         'character');
    AddString(KSQL_Character_Length,  'character_length');
    AddString(KSQL_Char_Length,       'char_length');
    AddString(KSQL_Check,             'check');
    AddString(KSQL_Close,             'close');
    AddString(KSQL_Coalesce,          'coalesce');
    AddString(KSQL_Collate,           'collate');
    AddString(KSQL_Collation,         'collation');
    AddString(KSQL_Column,            'column');
    AddString(KSQL_Commit,            'commit');
    AddString(KSQL_Connect,           'connect');
    AddString(KSQL_Connection,        'connection');
    AddString(KSQL_Constraint,        'constraint');
    AddString(KSQL_Constraints,       'constraints');
    AddString(KSQL_Continue,          'continue');
    AddString(KSQL_Convert,           'convert');
    AddString(KSQL_Corresponding,     'corresponding');
    AddString(KSQL_Create,            'create');
    AddString(KSQL_Cross,             'cross');
    AddString(KSQL_Current,           'current');
    AddString(KSQL_Current_Date,      'current_date');
    AddString(KSQL_Current_Time,      'current_time');
    AddString(KSQL_Current_Timestamp, 'current_timestamp');
    AddString(KSQL_Current_User,      'current_user');
    AddString(KSQL_Cursor,            'cursor');
    AddString(KSQL_Date,              'date');
    AddString(KSQL_Day,               'day');
    AddString(KSQL_Deallocate,        'deallocate');
    AddString(KSQL_Dec,               'dec');
    AddString(KSQL_Decimal,           'decimal');
    AddString(KSQL_Declare,           'declare');
    AddString(KSQL_Default,           'default');
    AddString(KSQL_Deferrable,        'deferrable');
    AddString(KSQL_Deferred,          'deferred');
    AddString(KSQL_Delete,            'delete');
    AddString(KSQL_Desc,              'desc');
    AddString(KSQL_Describe,          'describe');
    AddString(KSQL_Descriptor,        'descriptor');
    AddString(KSQL_Diagnostics,       'diagnostics');
    AddString(KSQL_Disconnect,        'disconnect');
    AddString(KSQL_Distinct,          'distinct');
    AddString(KSQL_Domain,            'domain');
    AddString(KSQL_Double,            'double');
    AddString(KSQL_Drop,              'drop');
    AddString(KSQL_Else,              'else');
    AddString(KSQL_End,               'end');
    AddString(KSQL_End_Exec,          'end-exec');
    AddString(KSQL_Escape,            'escape');
    AddString(KSQL_Except,            'except');
    AddString(KSQL_Exception,         'exception');
    AddString(KSQL_Exec,              'exec');
    AddString(KSQL_Execute,           'execute');
    AddString(KSQL_Exists,            'exists');
    AddString(KSQL_External,          'external');
    AddString(KSQL_Extract,           'extract');
    AddString(KSQL_False,             'false');
    AddString(KSQL_Fetch,             'fetch');
    AddString(KSQL_First,             'first');
    AddString(KSQL_Float,             'float');
    AddString(KSQL_For,               'for');
    AddString(KSQL_Foreign,           'foreign');
    AddString(KSQL_Found,             'found');
    AddString(KSQL_From,              'from');
    AddString(KSQL_Full,              'full');
    AddString(KSQL_Get,               'get');
    AddString(KSQL_Global,            'global');
    AddString(KSQL_Go,                'go');
    AddString(KSQL_Goto,              'goto');
    AddString(KSQL_Grant,             'grant');
    AddString(KSQL_Group,             'group');
    AddString(KSQL_Having,            'having');
    AddString(KSQL_Hour,              'hour');
    AddString(KSQL_Identity,          'identity');
    AddString(KSQL_Immediate,         'immediate');
    AddString(KSQL_In,                'in');
    AddString(KSQL_Indicator,         'indicator');
    AddString(KSQL_Initially,         'initially');
    AddString(KSQL_Inner,             'inner');
    AddString(KSQL_Input,             'input');
    AddString(KSQL_Insensitive,       'insensitive');
    AddString(KSQL_Insert,            'insert');
    AddString(KSQL_Int,               'int');
    AddString(KSQL_Integer,           'integer');
    AddString(KSQL_Intersect,         'intersect');
    AddString(KSQL_Interval,          'interval');
    AddString(KSQL_Into,              'into');
    AddString(KSQL_Is,                'is');
    AddString(KSQL_Isolation,         'isolation');
    AddString(KSQL_Join,              'join');
    AddString(KSQL_Key,               'key');
    AddString(KSQL_Language,          'language');
    AddString(KSQL_Last,              'last');
    AddString(KSQL_Leading,           'leading');
    AddString(KSQL_Left,              'left');
    AddString(KSQL_Level,             'level');
    AddString(KSQL_Like,              'like');
    AddString(KSQL_Local,             'local');
    AddString(KSQL_Lower,             'lower');
    AddString(KSQL_Match,             'match');
    AddString(KSQL_Max,               'max');
    AddString(KSQL_Min,               'min');
    AddString(KSQL_Minute,            'minute');
    AddString(KSQL_Module,            'module');
    AddString(KSQL_Month,             'month');
    AddString(KSQL_Names,             'names');
    AddString(KSQL_National,          'national');
    AddString(KSQL_Natural,           'natural');
    AddString(KSQL_Nchar,             'nchar');
    AddString(KSQL_Next,              'next');
    AddString(KSQL_No,                'no');
    AddString(KSQL_Not,               'not');
    AddString(KSQL_Null,              'null');
    AddString(KSQL_Nullif,            'nullif');
    AddString(KSQL_Numeric,           'numeric');
    AddString(KSQL_Octet_Length,      'octet_length');
    AddString(KSQL_Of,                'of');
    AddString(KSQL_On,                'on');
    AddString(KSQL_Only,              'only');
    AddString(KSQL_Open,              'open');
    AddString(KSQL_Option,            'option');
    AddString(KSQL_Or,                'or');
    AddString(KSQL_Order,             'order');
    AddString(KSQL_Outer,             'outer');
    AddString(KSQL_Output,            'output');
    AddString(KSQL_Overlaps,          'overlaps');
    AddString(KSQL_Pad,               'pad');
    AddString(KSQL_Partial,           'partial');
    AddString(KSQL_Position,          'position');
    AddString(KSQL_Precision,         'precision');
    AddString(KSQL_Prepare,           'prepare');
    AddString(KSQL_Preserve,          'preserve');
    AddString(KSQL_Primary,           'primary');
    AddString(KSQL_Prior,             'prior');
    AddString(KSQL_Privileges,        'privileges');
    AddString(KSQL_Procedure,         'procedure');
    AddString(KSQL_Public,            'public');
    AddString(KSQL_Read,              'read');
    AddString(KSQL_Real,              'real');
    AddString(KSQL_References,        'references');
    AddString(KSQL_Relative,          'relative');
    AddString(KSQL_Restrict,          'restrict');
    AddString(KSQL_Revoke,            'revoke');
    AddString(KSQL_Right,             'right');
    AddString(KSQL_Rollback,          'rollback');
    AddString(KSQL_Rows,              'rows');
    AddString(KSQL_Schema,            'schema');
    AddString(KSQL_Scroll,            'scroll');
    AddString(KSQL_Second,            'second');
    AddString(KSQL_Section,           'section');
    AddString(KSQL_Select,            'select');
    AddString(KSQL_Session,           'session');
    AddString(KSQL_Session_User,      'session_user');
    AddString(KSQL_Set,               'set');
    AddString(KSQL_Size,              'size');
    AddString(KSQL_Smallint,          'smallint');
    AddString(KSQL_Some,              'some');
    AddString(KSQL_Space,             'space');
    AddString(KSQL_Sql,               'sql');
    AddString(KSQL_Sqlcode,           'sqlcode');
    AddString(KSQL_Sqlerror,          'sqlerror');
    AddString(KSQL_Sqlstate,          'sqlstate');
    AddString(KSQL_Substring,         'substring');
    AddString(KSQL_Sum,               'sum');
    AddString(KSQL_System_user,       'system_user');
    AddString(KSQL_Table,             'table');
    AddString(KSQL_Temporary,         'temporary');
    AddString(KSQL_Then,              'then');
    AddString(KSQL_Time,              'time');
    AddString(KSQL_Timestamp,         'timestamp');
    AddString(KSQL_Timezone_Hour,     'timezone_hour');
    AddString(KSQL_Timezone_Minute,   'timezone_minute');
    AddString(KSQL_To,                'to');
    AddString(KSQL_Trailing,          'trailing');
    AddString(KSQL_Transaction,       'transaction');
    AddString(KSQL_Translate,         'translate');
    AddString(KSQL_Translation,       'translation');
    AddString(KSQL_Trim,              'trim');
    AddString(KSQL_True,              'true');
    AddString(KSQL_Union,             'union');
    AddString(KSQL_Unique,            'unique');
    AddString(KSQL_Unknown,           'unknown');
    AddString(KSQL_Update,            'update');
    AddString(KSQL_Upper,             'upper');
    AddString(KSQL_Usage,             'usage');
    AddString(KSQL_User,              'user');
    AddString(KSQL_Using,             'using');
    AddString(KSQL_Value,             'value');
    AddString(KSQL_Values,            'values');
    AddString(KSQL_Varchar,           'varchar');
    AddString(KSQL_Varying,           'varying');
    AddString(KSQL_View,              'view');
    AddString(KSQL_When,              'when');
    AddString(KSQL_Whenever,          'whenever');
    AddString(KSQL_Where,             'where');
    AddString(KSQL_With,              'with');
    AddString(KSQL_Work,              'work');
    AddString(KSQL_Write,             'write');
    AddString(KSQL_Year,              'year');
    AddString(KSQL_Zone,              'zone');

    TokenType := ttSQLSymbol;
    AddASCII(tkLeftParenthesis);
    AddASCII(tkRightParenthesis);
    AddASCII(tkDot);
    AddASCII(tkComma);
    AddASCII(tkAsterisk);
    AddASCII(tkPercent);
    AddString(KSQL_KiloByte,  'KB');
    AddString(KSQL_MegaByte,  'MB');
    AddString(KSQL_GigaByte,  'GB');

    TokenType := ttSQLOperator;
    AddString(KSQL_Addition,       '+');
    AddString(KSQL_Subtraction,    '-');
    AddString(KSQL_Division,       '/');
    AddString(KSQL_Multiplication, '*');
    AddString(KSQL_Equality,       '=');
    AddString(KSQL_NotEqual,       '<>');
    AddString(KSQL_LessThan,       '<');
    AddString(KSQL_GreaterThan,    '>');
    AddString(KSQL_NotLessThan,    '>=');
    AddString(KSQL_NotGreaterThan, '<=');
  end;



initialization
  SQL92 := TSQLDictionary.Create;

finalization
  SQL92.Free;
end.
