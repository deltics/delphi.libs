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


  unit Deltics.Tokeniser.TokenReader;


interface

  uses
    Classes,
    Contnrs,
    Deltics.Streams,
    Deltics.Strings,
    Deltics.Tokeniser,
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary,
    Deltics.Tokeniser.Tokens;


  type
    TCharStack = class;
    TReadCharProc = procedure(var aChar: WideChar) of object;


    TTokenReader = class
    private
      fCandidateTokens: TTokenDefinitions;
      fInvalidTokens: TTokenDefinitions;
      fDictionary: TTokenDictionary;

      fEOF: Boolean;
      fReplay: TCharStack;
      fRewind: TCharStack;
      fNextCharReady: Boolean;
      fNextDefinition: TTokenDefinition;
      fNextToken: IToken;

      fCaseSensitive: Boolean;
      fConsumeWhitespace: Boolean;
      fNormaliseCase: Boolean;
      fNormaliseKeywords: Boolean;

      fCharPos: Integer;
      fStartLine: Integer;
      fStartPos: Integer;
      fCompareBuffer: PWideCharArray;
      fTokenBuffer: PWideCharArray;
      fMaxTokenLength: Integer;
      fTokenLength: Integer;
      fLineNo: Integer;

      fOwnsStream: Boolean;
      fStream: TUnicodeStream;

      ReadChar: TReadCharProc;

      procedure UnreadChar; overload;
      procedure UnreadChar(const aList: TTokenDefinitions); overload;

      function get_EOF: Boolean;
      function get_Stream: TStream;
      procedure set_Stream(const aValue: TStream);

      procedure AllocTokenBuffers(const aBufSize: Integer);
      procedure FreeTokenBuffers;

      procedure ASCIIReadChar(var aChar: WideChar);
      procedure ASCIIReadCharNoCase(var aChar: WideChar);

      function CreateToken(const aDefinition: TTokenDefinition;
                           const aText: UnicodeString): TToken;
      function NextTokenDefinition: TTokenDefinition;
      function ReadToken: IToken;
      function TokenText(const aDefinition: TTokenDefinition): UnicodeString;

      property CaseSensitive: Boolean read fCaseSensitive;
      property ConsumeWhitespace: Boolean read fConsumeWhitespace;
      property NormaliseCase: Boolean read fNormaliseCase;
      property NormaliseKeywords: Boolean read fNormaliseKeywords;

    public
      constructor Create(const aStream: TStream;
                         const aDictionary: TTokenDictionary;
                         const aOptions: TTokeniserOptions = [toConsumeWhitespace]); overload;
      destructor Destroy; override;
      property LineNo: Integer read fLineNo;
      function Next: IToken;
      property EOF: Boolean read get_EOF;
      property Dictionary: TTokenDictionary read fDictionary;
      property Stream: TStream read get_Stream write set_Stream;
    end;


    PCharInfo = ^TCharInfo;
    TCharInfo = record
      Char: WideChar;
      LineNo: Integer;
      Pos: Integer;
      Candidates: TTokenDefinitions;
    end;


    TCharStack = class
    private
      fCapacity: Integer;
      fCount: Integer;
      fIsEmpty: Boolean;
      fItems: array of TCharInfo;
      fMark: Integer;
      fMarked: Boolean;
      procedure SetCapacity(const aCapacity: Integer);
      function get_IsMarked: Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure Peek(const aList: TTokenDefinitions);
      procedure Poke(const aList: TTokenDefinitions);
      procedure Mark;
//      procedure Pop(var aChar: WideChar; var aLineNo, aCharPos: Integer; const aList: TTokenDefinitions); overload;
      function Pop(var aChar: WideChar; var aLineNo, aCharPos: Integer): Boolean; overload;
      procedure Push(const aChar: WideChar; const aLineNo, aCharPos: Integer); overload;
//      procedure Push(const aChar: WideChar; const aLineNo, aCharPos: Integer; const aList: TTokenDefinitions); overload;
      property IsEmpty: Boolean read fIsEmpty;
      property IsMarked: Boolean read get_IsMarked;
    end;



implementation

  uses
    Deltics.SysUtils;


  constructor TCharStack.Create;
  begin
    inherited Create;

    fIsEmpty := TRUE;

    SetCapacity(64);
  end;


  destructor TCharStack.Destroy;
  var
    i: Integer;
  begin
    for i := 0 to Pred(fCapacity) do
      fItems[i].Candidates.Free;

    inherited;
  end;


  function TCharStack.get_IsMarked: Boolean;
  begin
    fMarked := fMarked and (fCount > fMark);
    result  := fMarked;
  end;


  procedure TCharStack.Mark;
  begin
    fMark   := fCount;
    fMarked := TRUE;
  end;


  procedure TCharStack.Clear;
  var
    i: Integer;
  begin
    for i := Pred(fCount) downto 0 do
    begin
      fItems[i].Char    := #0;
      fItems[i].LineNo  := 0;
      fItems[i].Pos     := 0;
    end;

    fCount    := 0;
    fIsEmpty  := TRUE;
    fMarked   := FALSE;
    fMark     := 0;
  end;


  procedure TCharStack.Peek(const aList: TTokenDefinitions);
  begin
    CloneList(fItems[fCount - 1].Candidates, aList);
  end;


  procedure TCharStack.Poke(const aList: TTokenDefinitions);
  begin
    CloneList(aList, fItems[fCount - 1].Candidates);
  end;


  function TCharStack.Pop(var aChar: WideChar; var aLineNo, aCharPos: Integer): Boolean;
  var
    ci: PCharInfo;
  begin
    ASSERT(NOT fIsEmpty);

    ci := @fItems[fCount - 1];

    aChar     := ci.Char;
    aLineNo   := ci.LineNo;
    aCharPos  := ci.Pos;

    Dec(fCount);

    fIsEmpty := (fCount = 0);
    result   := (fCount > 0);
  end;


  procedure TCharStack.Push(const aChar: WideChar;
                            const aLineNo, aCharPos: Integer);
  var
    ci: PCharInfo;
  begin
    if fCount = fCapacity - 1 then
      SetCapacity(2 * fCapacity);

    ci := @fItems[fCount];
    ci.Char   := aChar;
    ci.LineNo := aLineNo;
    ci.Pos    := aCharPos;

    Inc(fCount);

    fIsEmpty := FALSE;
  end;


  procedure TCharStack.SetCapacity(const aCapacity: Integer);
  var
    i: Integer;
  begin
    SetLength(fItems, aCapacity);

    for i := fCapacity to Pred(aCapacity) do
      fItems[i].Candidates := TTokenDefinitions.Create(FALSE);

    fCapacity := aCapacity;
  end;




const
    BUFSIZE = 1024;


  type
    TTokenHelper = class(TToken);



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenReader.Create(const aStream: TStream;
                                  const aDictionary: TTokenDictionary;
                                  const aOptions: TTokeniserOptions);
  begin
    inherited Create;

    fRewind   := TCharStack.Create;
    fReplay   := TCharStack.Create;
    fCharPos  := 0;
    fLineNo   := 1;

    fCaseSensitive      := toCaseSensitive in aOptions;
    fConsumeWhitespace  := toConsumeWhitespace in aOptions;
    fNormaliseCase      := toNormaliseCase in aOptions;
    fNormaliseKeywords  := toNormaliseKeywords in aOptions;

    fDictionary := aDictionary;
    Stream      := aStream;

    AllocTokenBuffers(BUFSIZE);

    if CaseSensitive then
      ReadChar := ASCIIReadChar
    else
      ReadChar := ASCIIReadCharNoCase;

    fCandidateTokens  := TTokenDefinitions.Create(FALSE);
    fInvalidTokens    := TTokenDefinitions.Create(FALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTokenReader.Destroy;
  begin
    Stream := NIL;

    FreeAndNIL(fInvalidTokens);
    FreeAndNIL(fCandidateTokens);

    FreeTokenBuffers;

    fRewind.Free;
    fReplay.Free;

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.CreateToken(const aDefinition: TTokenDefinition;
                                    const aText: UnicodeString): TToken;
  {
    Adds a token to the token list of the specified kind.  A flag indicates whether or not the
     next character (the first character of the next token) is ready for processing (TRUE) or has
     yet to be read (FALSE).
  }
  var
    aDelimited: TDelimitedToken absolute aDefinition;
    token: TTokenHelper absolute result;
    sub: TTokenReader;
    strm: TReadMemoryStream;
  begin
    result := NIL;

    // The current character position identifies the END of the
    //  token being added.  If the NextCharReady flag is TRUE,
    //  we have read beyond the end of the token and are
    //  positioned on the first character of the next token, so
    //  in that case the true end of the current token is the
    //  last character but one.

    if NOT Assigned(aDefinition) then
    begin
      result := TTokenHelper.CreateUnknown(WideString(fTokenBuffer^),
                                           fStartPos,
                                           fTokenLength,
                                           fStartLine,
                                           fLineNo);
      EXIT;
    end;

    // If we can add a token of the specified kind, then create a
    //  token.  If the token is a delimited token that needs to be
    //  tokenised itself, then we do that here before adding the
    //  token to the token list and firing the appropriate
    //  notification event.

    if (aDefinition.ClassID = dcDelimited) and Assigned(aDefinition.SubDictionary) then
    begin
      if aDefinition.SubDictionarySubstitution then
      begin
        // TODO: Figure out how token substitution can/should work...

        result := TTokenHelper.Create(aDefinition, aText, fStartPos, fTokenLength, fStartLine, fLineNo);
      end
      else
        result := TTokenHelper.Create(aDefinition, aText, fStartPos, fTokenLength, fStartLine, fLineNo);

      // If the subdictionary is an 'Inner' dictionary then we create a memory stream to read
      //  only the part of the token that is WITHIN the identified delimiters, otherwise
      //  the subdictionary will also take account of the delimiters (which is sometimes
      //  desirable or even necessary!)

      if aDefinition.InnerDictionary then
        strm  := TReadMemoryStream.Create(@fTokenBuffer[Length(aDelimited.Prefix)],
                                           (fTokenLength - (Length(aDelimited.Prefix) + Length(aDelimited.Suffix)) + 1) * 2)
      else
        strm  := TReadMemoryStream.Create(fTokenBuffer, fTokenLength * 2);

      sub := TTokenReader.Create(strm, aDefinition.SubDictionary);
      try
        sub.fLineNo   := fStartLine;
        sub.fCharPos  := fStartPos;

        while NOT sub.EOF do
          token.Add(sub.Next);

        TTokenHelper(result).SetLineTo(sub.LineNo);

      finally
        sub.Free;
        strm.Free;
      end;
    end
    else
      result := TTokenHelper.Create(aDefinition, aText, fStartPos, fTokenLength, fStartLine, fLineNo);
  end ;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenReader.ASCIIReadChar(var aChar: WideChar);
  var
    c: WideChar absolute aChar;
  begin
    if NOT fNextCharReady then
    begin
      fStream.ReadChar(c);
      Inc(fCharPos);

      fRewind.Push(aChar, fLineNo, fCharPos);

      if (fTokenLength = fMaxTokenLength) then
        AllocTokenBuffers(fMaxTokenLength + BUFSIZE);

      fTokenBuffer^[fTokenLength] := aChar;

      fEOF := fStream.EOF;
    end
    else
    begin
      fNextCharReady := fReplay.Pop(aChar, fLineNo, fCharPos);
      fRewind.Push(aChar, fLineNo, fCharPos);
      fTokenBuffer^[fTokenLength] := aChar;
    end;

    if (aChar = #10) then
    begin
      Inc(fLineNo);
      fCharPos := 0;
    end;

    Inc(fTokenLength);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenReader.ASCIIReadCharNoCase(var aChar: WideChar);
  begin
    ASCIIReadChar(aChar);

    if WIDE.IsUppercase(aChar) then
      fCompareBuffer^[fTokenLength - 1] := WIDE.Lowercase(aChar)
    else
      fCompareBuffer^[fTokenLength - 1] := aChar;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenReader.AllocTokenBuffers(const aBufSize: Integer);
  {
    Allocates or increases the size of the buffers used to hold the
     text for a token as it is read from the input stream.

    If the parser lexicon is case sensitive then the buffer used for
     comparison is just a pointer to the token buffer.  If the lexicon
     is not case sensitive, then a separate buffer is maintained which
     is entirely lower case.

    This function can only INCREASE the size of the token buffers.  An
     attempt to reduce the buffer size will be ignored.
  }
  begin
    if (aBufSize <= fMaxTokenLength) then
      EXIT;

    ReallocMem(fTokenBuffer, aBufSize * sizeof(WideChar));

    if CaseSensitive then
      fCompareBuffer := fTokenBuffer
    else
      ReallocMem(fCompareBuffer, aBufSize * sizeof(WideChar));

    fMaxTokenLength := aBufSize;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenReader.FreeTokenBuffers;
  {
    Returns all memory in use by the token buffers to the system.
  }
  begin
    ReallocMem(fTokenBuffer, 0);

    if CaseSensitive then
      fCompareBuffer := NIL
    else
      ReallocMem(fCompareBuffer, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.Next: IToken;
  begin
    if Assigned(fNextToken) then
    begin
      result := fNextToken;
      fNextToken := NIL;
    end
    else
      result := ReadToken;

    // To handle whitespace at the end of the file, we now need to look-ahead to the
    //  next valid token.  This ensures that when we are consuming whitespace we correctly
    //  indicate EOF after reading the last NON-whitespace token as this look-ahead
    //  will consume all of the whitespace that follows it.

    repeat
      fNextToken := ReadToken;
    until NOT Assigned(fNextToken) or NOT ConsumeWhitespace or NOT fNextToken.IsWhitespace;

    if Assigned(fNextToken) and ConsumeWhitespace and fNextToken.IsWhitespace then
      fNextToken := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.ReadToken: IToken;
  var
    def: TTokenDefinition;
    part: TTokenDefinition;
    s: UnicodeString;
  begin
    result := NIL;

    if Assigned(fNextDefinition) then
    begin
      def := fNextDefinition;
      fNextDefinition := NIL;
    end
    else
      repeat
        def := NextTokenDefinition;
      until EOF or NOT Assigned(def) or NOT ConsumeWhitespace or (def.TokenType <> ttWhitespace);

    if Assigned(def) then
    begin
      if EOF and ConsumeWhitespace and (def.TokenType = ttWhitespace) then
        EXIT;

      s := TokenText(def);

      if def.IsCompoundable then
      begin
        part := NextTokenDefinition;
        while (NOT EOF) and (part = def) do
        begin
          s := s + TokenText(def);
          part := NextTokenDefinition;
        end;

        fNextDefinition := part;
      end;

      result := CreateToken(def, s);
    end
    else if NOT EOF then
    begin
      // We expected to have a token definition but didn't - create an 'unknown' token

      s := TokenText(def);
      result := CreateToken(def, s);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.NextTokenDefinition: TTokenDefinition;

    function CompleteToken(const aDef: TTokenDefinition): Boolean;
    var
      chr: WideChar;
    begin
      result := FALSE;

      fRewind.Mark;
      try
        case aDef.ClassID of
          dcString        : begin
                              while NOT EOF and (fTokenLength < aDef.Length) do
                                ReadChar(chr);

                              result := (fTokenLength = aDef.Length);
                            end;

          dcLineEnd       : while NOT EOF do
                            begin
                              ReadChar(chr);

                              if aDef.IsComplete(fCompareBuffer, fTokenLength) then
                              begin
                                UnreadChar;
                                result := TRUE;
                                BREAK;
                              end;

                              if NOT aDef.IsCompatible(fCompareBuffer, fTokenLength) then
                              begin
                                UnreadChar;
                                result := FALSE;
                                BREAK;
                              end;
                            end;

          dcPrefixed      : while NOT EOF do
                            begin
                              ReadChar(chr);

                              if NOT aDef.IsCompatible(fCompareBuffer, fTokenLength) then
                              begin
                                UnreadChar;
                                result := TRUE;
                                BREAK;
                              end;
                            end;

          dcDelimited     : while NOT EOF do
                            begin
                              ReadChar(chr);

                              if aDef.IsComplete(fCompareBuffer, fTokenLength) then
                              begin
                                result := TRUE;
                                BREAK;
                              end;

                              if NOT aDef.IsCompatible(fCompareBuffer, fTokenLength) then
                              begin
                                result := FALSE;
                                UnreadChar;
                                BREAK;
                              end;
                            end;

          dcCharacterSet,
          dcRange         : begin
                              if NOT EOF then
                              begin
                                while NOT EOF do
                                begin
                                  ReadChar(chr);

                                  if NOT aDef.IsCompatible(fCompareBuffer, fTokenLength) then
                                    BREAK;
                                end;

                                repeat
                                  UnreadChar;
                                  result := aDef.IsComplete(fCompareBuffer, fTokenLength);
                                until result or (NOT fRewind.IsMarked);
                              end
                              else
                                result := aDef.IsComplete(fCompareBuffer, fTokenLength);
                            end;
        else
          ASSERT(FALSE, 'WTF?!');
        end;

      finally
        if NOT result then
          while fRewind.IsMarked do
            UnreadChar(fCandidateTokens);
      end;
    end;

  var
    i: Integer;
    chr: WideChar;
    def: TTokenDefinition;
  begin
    result        := NIL;
    fTokenLength  := 0;
    ReadChar(chr);

    fStartPos   := fCharPos;
    fStartLine  := fLineNo;

    // Get initial list of candidate token kinds
    CloneList(Dictionary.GetDefinitions(fStartPos, chr), fCandidateTokens);
    fRewind.Poke(fCandidateTokens);

    // Now try and whittle down the fCandidateTokens by building up the
    //  token one char at a time, re-applying the dictionary to filter
    //  the candidate definitions as we do so...

    while NOT EOF and (fCandidateTokens.Count > 1) do
    begin
      ReadChar(chr);

      // Ask the dictionary to remove any definitions that are no longer compatible
      //  with the contents of the compare buffer

      Dictionary.FilterDefinitions(fCandidateTokens, fCompareBuffer, fTokenLength);
      fRewind.Poke(fCandidateTokens);
    end;

    // Too far?  If we have no candidate definitions but have built up a token
    //  then we need to back track

    repeat
      case fCandidateTokens.Count of
        0 : begin
              if fTokenLength = 1 then
                BREAK;

              UnreadChar(fCandidateTokens);
            end;

        1 : begin
              def := fCandidateTokens[0];

              if NOT CompleteToken(def) then
              begin
                fInvalidTokens.Add(def);
                fCandidateTokens.Delete(0);
              end
              else
                result := def;
            end;

      else
        for i := Pred(fCandidateTokens.Count) downto 0 do
        begin
          def := fCandidateTokens[i];
          if fInvalidTokens.IndexOf(def) <> -1 then
            fCandidateTokens.Delete(i)
          else if NOT def.IsComplete(fCompareBuffer, fTokenLength) then
          begin
            fInvalidTokens.Add(def);
            fCandidateTokens.Delete(i);
          end;
        end;

        while fCandidateTokens.Count > 1 do
        begin
          def := Dictionary.MostCompatible(fCandidateTokens, fCompareBuffer, fTokenLength);
          if NOT CompleteToken(def) then
          begin
            fInvalidTokens.Add(def);
            fCandidateTokens.Remove(def);
          end
          else
          begin
            result := def;
            BREAK;
          end;
        end;
      end;
    until Assigned(result) or EOF;

    fRewind.Clear;
    fInvalidTokens.Clear;

    // We have a candidate token definition, but the token itself may not yet
    //  be complete.  What determines whether and when the token is compeleted
    //  depends on the type of token definition we are dealing with...

    fCompareBuffer^[fTokenLength] := WideChar(0);
    fTokenBuffer^[fTokenLength]   := WideChar(0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.get_EOF: Boolean;
  begin
    result := fEOF and (NOT fNextCharReady) and NOT Assigned(fNextToken);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.get_Stream: TStream;
  begin
    result := fStream;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenReader.set_Stream(const aValue: TStream);
  begin
    if fOwnsStream then
      FreeAndNIL(fStream);

    if Assigned(aValue) then
    begin
      if (aValue is TUnicodeStream) then
        fStream := TUnicodeStream(aValue)
      else
        fStream := TUnicodeStream.Create(aValue);
    end
    else
      fStream := NIL;

    fOwnsStream := Assigned(fStream) and (fStream <> aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.TokenText(const aDefinition: TTokenDefinition): UnicodeString;
  begin
    if NOT Assigned(aDefinition) then
      result := WideString(fTokenBuffer^)
    else if NormaliseKeywords and (aDefinition.ClassID = dcString) then
      result := TStringToken(aDefinition).Text
    else if NormaliseCase and NOT (aDefinition.ClassID = dcDelimited) then
      result := WideString(fCompareBuffer^)
    else
      result := WideString(fTokenBuffer^);
  end;





  procedure TTokenReader.UnreadChar(const aList: TTokenDefinitions);
  begin
    UnreadChar;
    if NOT fRewind.IsEmpty then
      fRewind.Peek(aList)
    else
      aList.Clear;
  end;


  procedure TTokenReader.UnreadChar;
  var
    c: WideChar;
    l, p: Integer;
  begin
    Dec(fTokenLength);

    fRewind.Pop(c, l, p);
    fReplay.Push(c, l, p);

    fNextCharReady := TRUE;
  end;




end.
