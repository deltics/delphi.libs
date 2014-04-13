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

{$i deltics.tokeniser.inc}

{$ifdef deltics_tokeniser}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.Tokeniser.Dictionary;


interface

  uses
  { vcl: }
    Classes,
    Contnrs,
  { deltics: }
    Deltics.Strings,
  { deltics.tokeniser: }
    Deltics.Tokeniser.Consts;


type
  TTokenDictionary = class;
  TTokenDefinition = class;
  TTokenDefinitions = class;

  TDefinitionClassID = (
                        dcCharacterSet,
                        dcString,
                        dcPrefixed,
                        dcLineEnd,
                        dcDelimited
                       );

  TUnicodeAllowed = (
                     uaNowhere,
                     uaInitial,
                     uaNonInitial,
                     uaAnywhere
                    );

  TANSICharSet        = set of ANSIChar;
  TArrayOfANSICharSet = array of TANSICharSet;

  WideCharArray     = array of WideChar;

  _WideCharArray     = array[0..65535] of WideChar;
  PWideCharArray    = ^_WideCharArray;


  {
    --------------------------------------------------------------------------
     TTokenDictionary

     Defines a class that stores the dictionary of token definitions to
      be applied by a tokeniser.
    --------------------------------------------------------------------------
  }
  TTokenDictionary = class
  private
    fName: String;
    fIsCaseSensitive: Boolean;
    fTokenType: TTokenType;

    fInitialDefinitions: array[WideChar] of TTokenDefinitions;
    fItems: TTokenDefinitions;
    fEmptyDefinitionList: TTokenDefinitions;

//    fDialects: array[TDialectID] of String;
//    fTokenTypes: array[TTokenType] of String;
//    fUnknown: TTokenDefinition;

    function get_Items(const aIndex: Integer): TTokenDefinition;
    function get_ItemCount: Integer;

  protected
    procedure Initialise; virtual; abstract;
    procedure Prepare;
    procedure SetName(const aName: String);
    property TokenType: TTokenType read fTokenType write fTokenType;

    procedure AddASCII(const aID: TTokenID;
                       const aDialects: TDialects = []); overload;
    procedure AddASCII(const aIDs: array of TTokenID;
                       const aDialects: TDialects = []); overload;
    procedure AddCharSet(const aID: TTokenID;
                         const aName: String;
                         const aValidChars: TANSICharSet;
                         const aDialects: TDialects = []); overload;
    procedure AddCharSet(const aID: TTokenID;
                         const aName: String;
                         const aCharSets: TArrayOfANSICharSet;
                         const aRequiredSequences: Integer = 0;
                         const aDialects: TDialects = []); overload;
    procedure AddLineEnd(const aID: TTokenID;
                         const aName: String;
                         const aPrefix: String;
                         const aDialects: TDialects = []);
    procedure AddDelimited(const aID: TTokenID;
                           const aName: String;
                           const aPrefix: UnicodeString;
                           const aSuffix: UnicodeString;
                           const aMultiline: Boolean = FALSE;
                           const aDialects: TDialects = []);
    procedure AddDelimitedCharSet(const aID: TTokenID;
                                  const aName: String;
                                  const aPrefix: UnicodeString;
                                  const aSuffix: UnicodeString;
                                  const aValidChars: TANSICharSet;
                                  const aUnicodeAllowed: Boolean = FALSE;
                                  const aDialects: TDialects = []);
    procedure AddQualifiedCharSet(const aID: TTokenID;
                                  const aName: String;
                                  const aInitialChars: TANSICharSet;
                                  const aValidChars: TANSICharSet;
                                  const aUnicodeAllowed: TUnicodeAllowed = uaNowhere;
                                  const aDialects: TDialects = []); overload;
    procedure AddQualifiedCharSet(const aID: TTokenID;
                                  const aName: String;
                                  const aInitialChars: TANSICharSet;
                                  const aValidChars: TANSICharSet;
                                  const aRequiredChars: TANSICharSet;
                                  const aUnicodeAllowed: TUnicodeAllowed = uaNowhere;
                                  const aDialects: TDialects = []); overload;
    procedure AddString(const aID: TTokenID;
                        const aName: String;
                        const aDialects: TDialects = []); overload;
    procedure AddString(const aID: TTokenID;
                        const aName: String;
                        const aText: UnicodeString;
                        const aDialects: TDialects = []); overload;
    procedure SetCaseSensitivity(const aIsSensitive: Boolean);
    procedure SetCompoundable(const aIDs: array of TTokenID);
    procedure SetSubDictionary(const aID: TTokenID;
                               const aDictionary: TTokenDictionary);
  public
    constructor Create;
    destructor Destroy; override;

    procedure FilterDefinitions(const aList: TTokenDefinitions;
                                const aBuffer: PWideCharArray;
                                const aLength: Integer);
    function GetDefinitions(const aInitialChar: WideChar): TTokenDefinitions;
    function MostCompatible(const aDefinitions: TTokenDefinitions; const aBuffer: PWideCharArray; const aTokenLength: Integer): TTokenDefinition; virtual;

    property IsCaseSensitive: Boolean read fIsCaseSensitive;
    property Items[const aIndex: Integer]: TTokenDefinition read get_Items;
    property ItemCount: Integer read get_ItemCount;
    property Name: String read fName;
  end;


  TTokenDefinition = class
  private
    fDialects: TDialects;
    fDictionary: TTokenDictionary;
    fID: TTokenID;
    fIsCompoundable: Boolean;
    fLength: Integer;
    fMultiLine: Boolean;
    fName: String;
    fSubDictionary: TTokenDictionary;
    fTokenType: TTokenType;
    fClassID: TDefinitionClassID;
    fMaxStartPos: Integer;
    fMinStartPos: Integer;
  protected
    constructor Create(const aDictionary: TTokenDictionary;
                       const aID: Integer;
                       const aName: String;
                       const aMultiLine: Boolean;
                       const aDialects: TDialects);
    function get_InitialChars: WideCharArray; virtual; abstract;
    property InitialChars: WideCharArray read get_InitialChars;
    property SetIsCompoundable: Boolean write fIsCompoundable;
    property SetLength: Integer write fLength;
    property SetSubDictionary: TTokenDictionary write fSubDictionary;
  public
    function IsCompatible(const aBuffer: PWideCharArray;
                          const aLength: Integer): Boolean; virtual; abstract;
    function IsComplete(const aBuffer: PWideCharArray;
                        const aLength: Integer): Boolean; virtual;
    property ClassID: TDefinitionClassID read fClassID;
    property Dialects: TDialects read fDialects;
    property Dictionary: TTokenDictionary read fDictionary;
    property ID: TTokenID read fID;
    property IsCompoundable: Boolean read fIsCompoundable;
    property Length: Integer read fLength;
    property MultiLine: Boolean read fMultiLine;
    property Name: String read fName;
    property SubDictionary: TTokenDictionary read fSubDictionary;
    property TokenType: TTokenType read fTokenType;
  end;


  TStringToken = class(TTokenDefinition)
  private
    fText: UnicodeString;
    function get_Text: String;
  protected
    constructor Create(const aDictionary: TTokenDictionary;
                       const aID: Integer;
                       const aName: String;
                       const aText: String;
                       const aDialects: TDialects);
    function get_InitialChars: WideCharArray; override;
  public
    function IsCompatible(const aBuffer: PWideCharArray;
                          const aLength: Integer): Boolean; override;
    function IsComplete(const aBuffer: PWideCharArray;
                        const aLength: Integer): Boolean; override;
    property Text: String read get_Text;
  end;


  TCharSetToken = class(TTokenDefinition)
  private
    fInitialChars: WideCharArray;
    fChars: WideCharArray;
    fCharSet: set of ANSIChar;
  protected
    function get_InitialChars: WideCharArray; override;
    procedure AddChar(const aChar: WideChar);
    procedure AddInitialChar(const aChar: WideChar);
  public
    property InitialChars;
    property Chars: WideCharArray read fChars;
  end;


    TDelimitedCharSetToken = class(TCharSetToken)
    private
      fUnicode: Boolean;
    protected
      constructor Create(const aDictionary: TTokenDictionary;
                         const aID: Integer;
                         const aName: String;
                         const aPrefix: UnicodeString;
                         const aSuffix: UnicodeString;
                         const aChars: TANSICharSet;
                         const aUnicodeAllowed: Boolean;
                         const aDialects: TDialects);
    public
      function IsCompatible(const aBuffer: PWideCharArray;
                            const aLength: Integer): Boolean; override;
    end;


    TQualifiedCharSetToken = class(TCharSetToken)
    private
      fUnicode: TUnicodeAllowed;
    public
      constructor Create(const aDictionary: TTokenDictionary;
                         const aID: Integer;
                         const aName: String;
                         const aInitialChars: TANSICharSet;
                         const aChars: TANSICharSet;
                         const aRequiredChars: TANSICharSet;
                         const aUnicodeAllowed: TUnicodeAllowed;
                         const aDialects: TDialects);
      function IsCompatible(const aBuffer: PWideCharArray;
                            const aLength: Integer): Boolean; override;
    end;


    TCharSetSequenceToken = class(TCharSetToken)
    private
      fCharSets: TArrayOfANSICharSet;
      fRequired: Integer;
      fCurrSeq: Integer;
    public
      constructor Create(const aDictionary: TTokenDictionary;
                         const aID: Integer;
                         const aName: String;
                         const aCharSets: TArrayOfANSICharSet;
                         const aRequiredSequences: Integer;
                         const aDialects: TDialects);
      function IsCompatible(const aBuffer: PWideCharArray;
                            const aLength: Integer): Boolean; override;
      function IsComplete(const aBuffer: PWideCharArray; const aLength: Integer): Boolean; override;
    end;


  TPrefixedToken = class(TTokenDefinition)
  private
    fPrefix: UnicodeString;
    fPrefixLength: Integer;
  protected
    constructor Create(const aDictionary: TTokenDictionary;
                       const aID: Integer;
                       const aName: String;
                       const aPrefix: UnicodeString;
                       const aMultiLine: Boolean;
                       const aDialects: TDialects);
    function get_InitialChars: WideCharArray; override;
    property PrefixLength: Integer read fPrefixLength;
  public
    function IsCompatible(const aBuffer: PWideCharArray;
                          const aLength: Integer): Boolean; override;
    property Prefix: UnicodeString read fPrefix;
  end;


    TDelimitedToken = class(TPrefixedToken)
    private
      fSuffix: UnicodeString;
      fSuffixLength: Integer;
    protected
      constructor Create(const aDictionary: TTokenDictionary;
                         const aID: Integer;
                         const aName: String;
                         const aPrefix: UnicodeString;
                         const aSuffix: UnicodeString;
                         const aMultiLine: Boolean;
                         const aDialects: TDialects);
      property SuffixLength: Integer read fSuffixLength;
    public
      function IsCompatible(const aBuffer: PWideCharArray;
                            const aLength: Integer): Boolean; override;
      function IsComplete(const aBuffer: PWideCharArray;
                          const aLength: Integer): Boolean; override;
      property Suffix: String read fSuffix;
    end;


    TLineEndToken = class(TPrefixedToken)
    protected
      constructor Create(const aDictionary: TTokenDictionary;
                         const aID: Integer;
                         const aName: String;
                         const aPrefix: UnicodeString;
                         const aDialects: TDialects);
    public
      function IsComplete(const aBuffer: PWideCharArray;
                          const aLength: Integer): Boolean; override;
    end;


  TTokenDefinitions = class(TObjectList)
  private
    function get_Item(const aIndex: Integer): TTokenDefinition;
  protected
    procedure Add(const aDefinition: TTokenDefinition);
  public
    property Items[const aIndex: Integer]: TTokenDefinition read get_Item; default;
  end;





implementation

  uses
  { deltics: }
    Deltics.SysUtils;



{ TTokenDictionary ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenDictionary.Create;
  {
    Constructor for a Dictionary object.  Creates list objects to hold
     token definitions and registers the built-in tkUnknown token kind.
  }
  begin
    inherited Create;

    fName   := ClassName;
    fItems  := TTokenDefinitions.Create(TRUE);

    Initialise;
    Prepare;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTokenDictionary.Destroy;
  {
    Destructor for Dictionary class.  Destroys all lists used to maintain
     token registration information (lists own their entries so all
     objects in those lists are also free'd).
  }
  var
    c: WideChar;
  begin
    for c := Low(fInitialDefinitions) to High(fInitialDefinitions) do
      if fInitialDefinitions[c] = fEmptyDefinitionList then
        fInitialDefinitions[c] := NIL
      else
        FreeAndNIL(fInitialDefinitions[c]);

    FreeAndNIL([@fItems,
                @fEmptyDefinitionList]);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenDictionary.get_Items(const aIndex: Integer): TTokenDefinition;
  begin
    result := TTokenDefinition(fItems[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenDictionary.get_ItemCount: Integer;
  begin
    result := fItems.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddASCII(const aID: TTokenID;
                                    const aDialects: TDialects);
  begin
    case aID of
      tkTab  : AddString(tkTab,  '[tab]',  Char(aID), aDialects);
      tkCR   : AddString(tkCR,   '[CR]',   Char(aID), aDialects);
      tkLF   : AddString(tkLF,   '[LF]',   Char(aID), aDialects);
      tkCRLF : AddString(tkCRLF, '[CR+LF]', #13#10, aDialects);
    else
      AddString(aID, Char(aID), aDialects);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddASCII(const aIDs: array of TTokenID;
                                    const aDialects: TDialects);
  var
    i: Integer;
  begin
    for i := Low(aIDs) to High(aIDs) do
      AddASCII(aIDs[i], aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddCharSet(const aID: TTokenID;
                                      const aName: String;
                                      const aValidChars: TANSICharSet;
                                      const aDialects: TDialects);
  begin
    AddQualifiedCharSet(aID, aName, aValidChars, aValidChars, uaNowhere, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddCharSet(const aID: TTokenID;
                                        const aName: String;
                                        const aCharSets: TArrayOfANSICharSet;
                                        const aRequiredSequences: Integer;
                                        const aDialects: TDialects);
  begin
    TCharSetSequenceToken.Create(self, aID, aName, aCharSets, aRequiredSequences, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddLineEnd(const aID: TTokenID;
                                        const aName: String;
                                        const aPrefix: String;
                                        const aDialects: TDialects);
  begin
    TLineEndToken.Create(self, aID, aName, aPrefix, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddDelimited(const aID: TTokenID;
                                          const aName: String;
                                          const aPrefix: String;
                                          const aSuffix: String;
                                          const aMultiline: Boolean;
                                          const aDialects: TDialects);
  begin
    TDelimitedToken.Create(self, aID, aName, aPrefix, aSuffix, aMultiLine, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddDelimitedCharSet(const aID: TTokenID;
                                                 const aName: String;
                                                 const aPrefix: UnicodeString;
                                                 const aSuffix: UnicodeString;
                                                 const aValidChars: TANSICharSet;
                                                 const aUnicodeAllowed: Boolean;
                                                 const aDialects: TDialects);
  begin
    TDelimitedCharSetToken.Create(self, aID, aName, aPrefix, aSuffix, aValidChars, aUnicodeAllowed, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddQualifiedCharSet(const aID: TTokenID;
                                                 const aName: String;
                                                 const aInitialChars: TANSICharSet;
                                                 const aValidChars: TANSICharSet;
                                                 const aUnicodeAllowed: TUnicodeAllowed;
                                                 const aDialects: TDialects);
  begin
    TQualifiedCharSetToken.Create(self, aID, aName, aInitialChars, aValidChars, [], aUnicodeAllowed, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddQualifiedCharSet(const aID: TTokenID;
                                                 const aName: String;
                                                 const aInitialChars: TANSICharSet;
                                                 const aValidChars: TANSICharSet;
                                                 const aRequiredChars: TANSICharSet;
                                                 const aUnicodeAllowed: TUnicodeAllowed;
                                                 const aDialects: TDialects);
  begin
    TQualifiedCharSetToken.Create(self, aID, aName, aInitialChars, aValidChars, aRequiredChars, aUnicodeAllowed, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddString(const aID: TTokenID;
                                       const aName: String;
                                       const aDialects: TDialects);
  begin
    AddString(aID, aName, aName, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.AddString(const aID: TTokenID;
                                       const aName: String;
                                       const aText: UnicodeString;
                                       const aDialects: TDialects);
  begin
    TStringToken.Create(self, aID, aName, aText, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.FilterDefinitions(const aList: TTokenDefinitions;
                                               const aBuffer: PWideCharArray;
                                               const aLength: Integer);
  var
    i: Integer;
  begin
    for i := Pred(aList.Count) downto 0 do
      if NOT aList[i].IsCompatible(aBuffer, aLength) then
        aList.Delete(i);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenDictionary.MostCompatible(const aDefinitions: TTokenDefinitions;
                                           const aBuffer: PWideCharArray;
                                           const aTokenLength: Integer): TTokenDefinition;
  {
    Determines which, of the specified list of token Candidates is the most compatible with the
     current contents of the token buffer.  NOTE: All Candidates in the list are compatible - the
     purpose of this exercise is to decide which is the most appropriate single kind for the
     current token buffer.

    There are two phases - the first phase tests regular tokens and character set tokens - any
     delimited Candidates are saved for the second phase of tests.

    Of the non-delimited Candidates
    Once all other Candidates have been checked for compatibility, the
     list of delimited Candidates is then checked.  Since a delimited kind
     is explicitly terminated, if a delimited kind matches the token
     buffer, this is a better match than a non-delimited kind.

    Finally, of all the delimited Candidates, the one with the least
     amount of delimiter information is the best.
  }
  var
    i: Integer;
    def: TTokenDefinition;
    prefixed: TList;
  begin
    result := NIL;

    // Create a list to hold any delimited Candidates we find - to avoid
    //  memory reallocations set the capacity the same as the original
    //  list (which is the maximum capacity likely to be required, i.e if
    //  all candidate kinds are delimited kinds)
    prefixed := TList.Create;
    try
      prefixed.Capacity := aDefinitions.Count;

      for i := Pred(aDefinitions.Count) downto 0 do
      begin
        def := TTokenDefinition(aDefinitions[i]);

        // Try and match to a character set or string definition

        case def.ClassID of
          dcCharacterSet  : if NOT Assigned(result)
                             or (def.Length < result.Length) then
                              result := def;

          dcString        : if (def.Length = aTokenLength) then
                            begin
                              result := def;
                              BREAK;
                            end;

          dcPrefixed,
          dcDelimited     : prefixed.Add(def);

          dcLineEnd       : begin
                              result := def;
                              BREAK;
                            end;
        end;
      end;

      // If we have a result or have no delimited definitions to test, our
      //  work is done

      if Assigned(result) or (prefixed.Count = 0) then
        EXIT;

      // Find the best match from these (best match = MOST delimiter
      //  information required)

      result := TTokenDefinition(prefixed[0]);

      for i := Pred(prefixed.Count) downto 1 do
      begin
        def := TTokenDefinition(prefixed[i]);
        if (def.Length > result.Length) then
          result := def;
      end;
    finally
      prefixed.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.Prepare;

    procedure AddInitial(const aChar: WideChar;
                         const aDef: TTokenDefinition);
    begin
      if NOT Assigned(fInitialDefinitions[aChar]) then
        fInitialDefinitions[aChar] := TTokenDefinitions.Create(FALSE);

      fInitialDefinitions[aChar].Add(aDef);
    end;

  var
    i, j: Integer;
    c: WideChar;
    def: TTokenDefinition;
    initialChars: WideCharArray;
    unicodeDefs: TList;
  begin
    if Assigned(fEmptyDefinitionList) then
      EXIT;

    unicodeDefs := TList.Create;
    try
      for i := 0 to Pred(ItemCount) do
      begin
        def := Items[i];
        initialChars := def.InitialChars;

        for j := 0 to Pred(Length(initialChars)) do
        begin
          c := initialChars[j];

          AddInitial(c, def);

          if NOT fIsCaseSensitive then
          begin
            if WIDE.IsUppercase(c) then
              AddInitial(WIDE.Lowercase(c), def)
            else if WIDE.IsLowercase(c) then
              AddInitial(WIDE.Uppercase(c), def);
          end;
        end;

        if (def is TQualifiedCharSetToken)
         and (TQualifiedCharSetToken(def).fUnicode in [uaInitial, uaAnywhere]) then
          unicodeDefs.Add(def);
      end;

      for i := 0 to Pred(unicodeDefs.Count) do
        for c := WideChar(128) to #65535 do
          AddInitial(c, TTokenDefinition(unicodeDefs[i]));

    finally
      unicodeDefs.Free;
    end;

    fEmptyDefinitionList := TTokenDefinitions.Create(FALSE);

    for c := Low(fInitialDefinitions) to High(fInitialDefinitions) do
      if NOT Assigned(fInitialDefinitions[c]) then
        fInitialDefinitions[c] := fEmptyDefinitionList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.SetName(const aName: String);
  begin
    fName := aName;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenDictionary.GetDefinitions(const aInitialChar: WideChar): TTokenDefinitions;
  var
    i: Integer;
  begin
    result := TTokenDefinitions(fInitialDefinitions[aInitialChar]);
    if NOT Assigned(result) then
      result := fEmptyDefinitionList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.SetCaseSensitivity(const aIsSensitive: Boolean);
  begin
    fIsCaseSensitive := aIsSensitive;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.SetCompoundable(const aIDs: array of TTokenID);
  var
    i, j: Integer;
  begin
    for i := 0 to Pred(ItemCount) do
      for j := 0 to Pred(Length(aIDs)) do
        if (Items[i].ID = aIDs[j]) then
          Items[i].SetIsCompoundable := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDictionary.SetSubDictionary(const aID: TTokenID;
                                              const aDictionary: TTokenDictionary);
  var
    i: Integer;
  begin
    for i := 0 to Pred(ItemCount) do
      if (Items[i].ID = aID) then
        Items[i].SetSubDictionary := aDictionary;
  end;







{ TDefinition ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenDefinition.Create(const aDictionary: TTokenDictionary;
                                      const aID: Integer;
                                      const aName: String;
                                      const aMultiLine: Boolean;
                                      const aDialects: TDialects);
  begin
    inherited Create;

    fID         := aID;
    fDialects   := aDialects;
    fMultiLine  := aMultiLine;
    fName       := aName;

    fDictionary := aDictionary;
    fTokenType  := aDictionary.TokenType;

    if (self is TStringToken) then
      fClassID := dcString
    else if (self is TDelimitedToken) then
      fClassID := dcDelimited
    else if (self is TLineEndToken) then
      fClassID := dcLineEnd
    else if (self is TPrefixedToken) then
      fClassID := dcPrefixed
    else if (self is TCharSetToken) then
      fClassID := dcCharacterSet
    else
      raise Exception.Create('Unknown/unsupported token definition class');

      fDictionary.fItems.Add(self);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenDefinition.IsComplete(const aBuffer: PWideCharArray;
                                       const aLength: Integer): Boolean;
  begin
    result := TRUE;
  end;







{ TStringDefinition ------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TStringToken.Create(const aDictionary: TTokenDictionary;
                                  const aID: Integer;
                                  const aName: String;
                                  const aText: String;
                                  const aDialects: TDialects);
  begin
    inherited Create(aDictionary, aID, aName, FALSE, aDialects);

    SetLength := System.Length(aText);

    fText := aText;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringToken.get_InitialChars: WideCharArray;
  var
    uc, lc: Char;
  begin
    uc := WIDE.Uppercase(fText[1]);
    lc := WIDE.Lowercase(fText[1]);

    if NOT Dictionary.IsCaseSensitive and (uc <> lc) then
    begin
      System.SetLength(result, 2);
      result[0] := uc;
      result[1] := lc;
    end
    else
    begin
      System.SetLength(result, 1);
      result[0] := fText[1];
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringToken.get_Text: String;
  begin
    result := fText;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringToken.IsCompatible(const aBuffer: PWideCharArray;
                                     const aLength: Integer): Boolean;
  begin
    result := (aLength <= Length) and (aBuffer^[Pred(aLength)] = fText[aLength]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringToken.IsComplete(const aBuffer: PWideCharArray;
                                   const aLength: Integer): Boolean;
  begin
    result := (aLength = Length);
  end;





{ TDtxCharSetDefinition -------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCharSetToken.get_InitialChars: WideCharArray;
  begin
    result := fInitialChars;
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TQualifiedCharSetToken.Create(const aDictionary: TTokenDictionary;
                                            const aID: Integer;
                                            const aName: String;
                                            const aInitialChars: TANSICharSet;
                                            const aChars: TANSICharSet;
                                            const aRequiredChars: TANSICharSet;
                                            const aUnicodeAllowed: TUnicodeAllowed;
                                            const aDialects: TDialects);

    function CountValidChars: Integer;
    var
      c: ANSIChar;
    begin
      result := 0;

      for c := #0 to #255 do
      begin
        if (c in aInitialChars) then
          AddInitialChar(WideChar(c));

        if (c in aChars) then
          AddChar(WideChar(c));

        if (c in aInitialChars) or (c in aChars) then
          Inc(result);
      end;
    end;

  begin
    inherited Create(aDictionary, aID, aName, FALSE, aDialects);

    fCharSet  := aChars;
    fUnicode  := aUnicodeAllowed;

    SetLength := CountValidChars;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TQualifiedCharSetToken.IsCompatible(const aBuffer: PWideCharArray;
                                               const aLength: Integer): Boolean;
  var
    cp: Word;
    c: ANSIChar absolute cp;
  begin
    cp := Ord(aBuffer[Pred(aLength)]);

    result := (cp <= 127) and (c in fCharSet);

    if NOT result and (cp > 127) then
    begin
      case fUnicode of
        uaAnywhere    : result := TRUE;
        uaInitial     : result := (aLength = 1);
        uaNonInitial  : result := (aLength > 1);
      else
        {uaNowhere} result := FALSE;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCharSetToken.AddChar(const aChar: WideChar);
  var
    len: Integer;
    uc, lc: Char;
  begin
    uc := WIDE.Uppercase(aChar);
    lc := WIDE.Lowercase(aChar);

    len := System.Length(fChars);

    if NOT Dictionary.IsCaseSensitive and (uc <> lc) then
    begin
      System.SetLength(fChars, len + 2);
      fChars[len]     := uc;
      fChars[len + 1] := lc;
    end
    else
    begin
      System.SetLength(fChars, len + 1);
      fChars[len] := aChar;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCharSetToken.AddInitialChar(const aChar: WideChar);
  var
    len: Integer;
    uc, lc: Char;
  begin
    uc := WIDE.Uppercase(aChar);
    lc := WIDE.Lowercase(aChar);

    len := System.Length(fInitialChars);

    if NOT Dictionary.IsCaseSensitive and (uc <> lc) then
    begin
      System.SetLength(fInitialChars, len + 2);
      fInitialChars[len]     := uc;
      fInitialChars[len + 1] := lc;
    end
    else
    begin
      System.SetLength(fInitialChars, len + 1);
      fInitialChars[len] := aChar;
    end;
  end;








  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDelimitedCharSetToken.Create(const aDictionary: TTokenDictionary;
                                            const aID: Integer;
                                            const aName: String;
                                            const aPrefix: UnicodeString;
                                            const aSuffix: UnicodeString;
                                            const aChars: TANSICharSet;
                                            const aUnicodeAllowed: Boolean;
                                            const aDialects: TDialects);
  begin
    inherited Create(aDictionary, aID, aName, FALSE, aDialects);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDelimitedCharSetToken.IsCompatible(const aBuffer: PWideCharArray;
                                               const aLength: Integer): Boolean;
  begin
  end;






  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCharSetSequenceToken.Create(const aDictionary: TTokenDictionary;
                                           const aID: Integer;
                                           const aName: String;
                                           const aCharSets: TArrayOfANSICharSet;
                                           const aRequiredSequences: Integer;
                                           const aDialects: TDialects);

    procedure AddInitialChars;
    var
      c: ANSIChar;
    begin
      for c := #0 to #255 do
        if (c in aCharSets[0]) then
          AddInitialChar(WideChar(c));
    end;

  begin
    inherited Create(aDictionary, aID, aName, FALSE, aDialects);

    fCharSets := aCharSets;
    fRequired := aRequiredSequences;

    AddInitialChars;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCharSetSequenceToken.IsCompatible(const aBuffer: PWideCharArray;
                                              const aLength: Integer): Boolean;
  var
    c: ANSIChar;
  begin
    c := ANSIChar(Ord(aBuffer[Pred(aLength)]));

    if (aLength = 2) then
      fCurrSeq := 0;

    result := c in fCharSets[fCurrSeq];
    if result then
    begin
      while (fCurrSeq < High(fCharSets)) and (c in fCharSets[Succ(fCurrSeq)]) do
        Inc(fCurrSeq);

      EXIT;
    end;

    if fCurrSeq < High(fCharSets) then
    begin
      Inc(fCurrSeq);
      result := c in fCharSets[fCurrSeq];
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCharSetSequenceToken.IsComplete(const aBuffer: PWideCharArray;
                                            const aLength: Integer): Boolean;
  var
    c: ANSIChar;
  begin
    c := ANSIChar(Ord(aBuffer[Pred(aLength)]));

    result := (fRequired = 0) or
              ((fCurrSeq = fRequired) and (c in fCharSets[fCurrSeq]));
  end;







{ TPrefixedDefinition ---------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TPrefixedToken.Create(const aDictionary: TTokenDictionary;
                                    const aID: Integer;
                                    const aName: String;
                                    const aPrefix: String;
                                    const aMultiLine: Boolean;
                                    const aDialects: TDialects);
  begin
    inherited Create(aDictionary, aID, aName, aMultiLine, aDialects);

    fPrefix       := aPrefix;
    fPrefixLength := System.Length(aPrefix);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPrefixedToken.get_InitialChars: WideCharArray;
  begin
    System.SetLength(result, 1);
    result[0] := fPrefix[1];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPrefixedToken.IsCompatible(const aBuffer: PWideCharArray;
                                       const aLength: Integer): Boolean;
  {
    A LineEnd definition is compatible if the buffer is greater than the length
     of the Prefix (for this to occur the prefix itself must be compatible)
     or if the last read char in the buffer matches the corresponding char
     in the Prefix.
  }
  var
    c: WideChar;
  begin
    c := aBuffer^[Pred(aLength)];

    result := ((aLength > PrefixLength) or (c = fPrefix[aLength]))
              and ((c <> #13) and (c <> #10));
  end;







{ TDelimitedDefinition --------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDelimitedToken.Create(const aDictionary: TTokenDictionary;
                                     const aID: Integer;
                                     const aName: String;
                                     const aPrefix: String;
                                     const aSuffix: String;
                                     const aMultiLine: Boolean;
                                     const aDialects: TDialects);
  begin
    inherited Create(aDictionary, aID, aName, aPrefix, aMultiLine, aDialects);

    fSuffix       := aSuffix;
    fSuffixLength := System.Length(aSuffix);

    SetLength := PrefixLength + SuffixLength;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDelimitedToken.IsCompatible(const aBuffer: PWideCharArray;
                                        const aLength: Integer): Boolean;
  begin
    if (aLength <= PrefixLength) then
      result := (aBuffer^[Pred(aLength)] = fPrefix[aLength])
    else
      result := NOT IsComplete(aBuffer, aLength);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDelimitedToken.IsComplete(const aBuffer: PWideCharArray;
                                      const aLength: Integer): Boolean;
  var
    i: Integer;
  begin
    result := (aLength >= Length);

    if result then
    begin
      for i := SuffixLength downto 1 do
      begin
        result := fSuffix[i] = aBuffer^[aLength - (SuffixLength - i) - 1];
        if NOT result then
          EXIT;
      end;
    end
  end;






{ TTokenDefinitions ------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenDefinitions.Add(const aDefinition: TTokenDefinition);
  begin
    inherited Add(aDefinition);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenDefinitions.get_Item(const aIndex: Integer): TTokenDefinition;
  begin
    result := TTokenDefinition(inherited Items[aIndex]);
  end;








{ TLineEndToken }

  constructor TLineEndToken.Create(const aDictionary: TTokenDictionary;
                                   const aID: Integer;
                                   const aName: String;
                                   const aPrefix: UnicodeString;
                                   const aDialects: TDialects);
  begin
    inherited Create(aDictionary, aID, aName, aPrefix, FALSE, aDialects);
  end;


  function TLineEndToken.IsComplete(const aBuffer: PWideCharArray;
                                    const aLength: Integer): Boolean;
  var
    c: WideChar;
  begin
    c := aBuffer^[Pred(aLength)];
    result := (c = #13) or (c = #10);
  end;




end.
