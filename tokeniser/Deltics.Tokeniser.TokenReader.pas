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
    Deltics.Streams,
    Deltics.Strings,
    Deltics.Tokeniser,
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary,
    Deltics.Tokeniser.Tokens;


  type
    TReadCharProc = procedure(var aChar: WideChar) of object;


    TTokenReader = class
    private
      fCandidateTokens: TTokenDefinitions;
      fPreviousCandidateTokens: TTokenDefinitions;
      fDictionary: TTokenDictionary;

      fEOF: Boolean;
      fNextChar: WideChar;
      fNextCharReady: Boolean;
      fNextDefinition: TTokenDefinition;

      fCaseSensitive: Boolean;
      fConsumeWhitespace: Boolean;
      fNormaliseCase: Boolean;
      fNormaliseKeywords: Boolean;

      fNewLine: Boolean;
      fCharPos: Integer;
      fPrevCharPos: Integer;
      fStartPos: Integer;
      fCompareBuffer: PWideCharArray;
      fTokenBuffer: PWideCharArray;
      fMaxTokenLength: Integer;
      fTokenLength: Integer;
      fLineNo: Integer;

      fOwnsStream: Boolean;
      fStream: TUnicodeStream;

      ReadChar: TReadCharProc;

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
      function TokenText(const aDefinition: TTokenDefinition): UnicodeString;

      property NextCharReady: Boolean read fNextCharReady;

      property CaseSensitive: Boolean read fCaseSensitive;
      property ConsumeWhitespace: Boolean read fConsumeWhitespace;
      property NormaliseCase: Boolean read fNormaliseCase;
      property NormaliseKeywords: Boolean read fNormaliseKeywords;

    public
      constructor Create(const aStream: TStream;
                         const aDictionary: TTokenDictionary;
                         const aOptions: TTokeniserOptions = [toConsumeWhitespace]);
      destructor Destroy; override;
      property LineNo: Integer read fLineNo;
      function Next: TToken; overload;
      property EOF: Boolean read get_EOF;
      property Dictionary: TTokenDictionary read fDictionary;
      property Stream: TStream read get_Stream write set_Stream;
    end;


implementation

  uses
    Deltics.SysUtils;


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

    fNewLine      := TRUE;

    fCharPos      := 0;
    fPrevCharPos  := 0;
    fStartPos     := 1;
    fLineNo       := 1;

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

    fCandidateTokens          := TTokenDefinitions.Create(FALSE);
    fPreviousCandidateTokens  := TTokenDefinitions.Create(FALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTokenReader.Destroy;
  begin
    Stream := NIL;

    FreeAndNIL([@fCandidateTokens,
                @fPreviousCandidateTokens]);

    FreeTokenBuffers;

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
    endpos: Integer;
  begin
    result := NIL;

    // TODO: Optimisation: Find a way to avoid position tracking calculations
    //        for situations when position information isn't required

    // The current character position identifies the END of the
    //  token being added.  If the NextCharReady flag is TRUE,
    //  we have read beyond the end of the token and are
    //  positioned on the first character of the next token, so
    //  in that case the true end of the current token is the
    //  last character but one.
    if NextCharReady then
      endpos := fPrevCharPos
    else
      endpos := fCharPos;

    try
      if NOT Assigned(aDefinition) then
      begin
        result := TTokenHelper.CreateUnknown(WideString(fTokenBuffer^),
                                             fStartPos,
                                             fTokenLength,
                                             fLineNo,
                                             fNewLine);
        fNewLine := FALSE;
        EXIT;
      end;

      // If we can add a token of the specified kind, then create a
      //  token.  If the token is a delimited token that needs to be
      //  tokenised itself, then we do that here before adding the
      //  token to the token list and firing the appropriate
      //  notification event.

      if NOT (ConsumeWhitespace and (aDefinition.TokenType = ttWhitespace)) then
      begin
        if (aDefinition.ID = tkLF) then
          result := TTokenHelper.Create(aDefinition, aText, fStartPos, fTokenLength, fLineNo - 1, fNewLine)
        else
          result := TTokenHelper.Create(aDefinition, aText, fStartPos, fTokenLength, fLineNo, fNewLine);

        if (aDefinition.ClassID = dcDelimited) then
        begin
          // TODO: Tokenise tokens with subdictionaries...

  //          if TDtxDelimitedToken(TokenDefinition).Tokenise then
  //            token.Tokenise;

  //          if TDelimitedTokenKind(kind).CanSpanLines then
  //            Inc(fLineNo, token.LineSpan);
        end;

        fNewLine := FALSE;
      end;

    finally
      if Assigned(aDefinition) and (aDefinition.ID = tkLF) then
        fStartPos := fCharPos
      else
        fStartPos := endpos;
    end;
  end ;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenReader.ASCIIReadChar(var aChar: WideChar);
  var
    c: WideChar absolute aChar;
  begin
    if NOT NextCharReady then
    begin
      fPrevCharPos := fCharPos;
      Inc(fCharPos);

      fStream.ReadChar(c);
    end
    else
      aChar := fNextChar;

    // If the token is a line feed then we increment the line count
    //  and reset the character position, being careful again to
    //  take account of whether or not the first character of the
    //  next token has already been read.  If it has then the new
    //  character position is already 1 (one).  If it hasn't then
    //  the new character position is 0 (zero), i.e. the start of
    //  the new line.

    if (aChar = #10) then
    begin
      Inc(fLineNo);
      fCharPos  := 1;
      fNewLine  := TRUE;
    end;

    fNextCharReady := FALSE;

    if (fTokenLength = fMaxTokenLength) then
      AllocTokenBuffers(fMaxTokenLength + BUFSIZE);

    fTokenBuffer^[fTokenLength] := aChar;

    Inc(fTokenLength);

    fEOF := fStream.EOF;
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
  function TTokenReader.Next: TToken;
  var
    def: TTokenDefinition;
    part: TTokenDefinition;
    s: UnicodeString;
  begin
    repeat
      def := NextTokenDefinition;
    until NOT Assigned(def) or NOT (ConsumeWhitespace and (def.TokenType = ttWhitespace)) or EOF;

    if Assigned(def) then
    begin
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

      result  := CreateToken(def, s);
    end
    else
    begin
      s := TokenText(def);
      result  := CreateToken(def, s);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.NextTokenDefinition: TTokenDefinition;
  var
    i: Integer;
    chr: WideChar;
    countLines: Boolean;
    defs: TTokenDefinitions;
  begin
    if Assigned(fNextDefinition) then
    begin
      result := fNextDefinition;
      fNextDefinition := NIL;
      EXIT;
    end;

    result := NIL;

(*
    while NOT Assigned(result) and (NOT EOF or NextCharReady) do
    begin
*)
//      if (fPreviousCandidateTokens.Count > 0) then
        fPreviousCandidateTokens.Clear;

      fTokenLength := 0;
      ReadChar(chr);

      // Get initial list of candidate token kinds
      CloneList(Dictionary.GetDefinitions(chr), fCandidateTokens);

      // Now try and whittle down the fCandidateTokens by building up the
      //  token one char at a time, re-applying the dictionary to filter
      //  the candidate definitions as we do so...

      while NOT EOF and (fCandidateTokens.Count > 1) do
      begin
        ReadChar(chr);

        // If the list has changed (number of entries has reduced), store the new list of
        //  compatible kinds to avoid having to look them up again later....

        if (fCandidateTokens.Count <> fPreviousCandidateTokens.Count) then
          CloneList(fCandidateTokens, fPreviousCandidateTokens);

        // Ask the dictionary to remove any definitions that are no longer compatible
        //  with the contents of the compare buffer

        Dictionary.FilterDefinitions(fCandidateTokens, fCompareBuffer, fTokenLength);
      end;

      // Too far?  If we have no candidate definitions but have built up a token
      //  then we need to back track 1 character

      if (fCandidateTokens.Count = 0) and (fTokenLength > 1) then
      begin
        Dec(fTokenLength);

        if (chr = #10) then
        begin
          Dec(fLineNo);
          fCharPos := fPrevCharPos;
        end;

        fNextChar      := chr;
        fNextCharReady := TRUE;

        // We ran out of candidate definitions so we use the previously saved list
        //  of candidates to find a most compatible match ...
        //
        // (the previous candidate list is the list compatible with the token before
        //  we back tracked, removing the most recently added char)

        defs := fPreviousCandidateTokens;
      end
      else
        defs := fCandidateTokens;

      for i := Pred(defs.Count) downto 0 do
      begin
        if defs[i].ClassID <> dcCharacterSet then
          CONTINUE;

        if NOT defs[i].IsComplete(fCompareBuffer, fTokenLength) then
          defs.Delete(i);
      end;

      case defs.Count of
        0: { NO-OP};
        1: result := defs[0];
      else
        result := Dictionary.MostCompatible(defs, fCompareBuffer, fTokenLength);
      end;

      // We have a candidate token definition, but the token itself may not yet
      //  be complete.  What determines whether and when the token is compeleted
      //  depends on the type of token definition we are dealing with...

      if Assigned(result) then
      begin
        case result.ClassID of
          dcString        : { NO-OP };

          dcLineEnd       : while NOT EOF do
                            begin
                              ReadChar(chr);

                              if result.IsComplete(fCompareBuffer, fTokenLength) then
                              begin
                                if (chr = #10) then
                                begin
                                  Dec(fLineNo);
                                  fCharPos := fPrevCharPos;
                                end;

                                fNextChar       := chr;
                                fNextCharReady  := TRUE;
                                fEOF            := FALSE; // ?
                                BREAK;
                              end;

                              if NOT result.IsCompatible(fCompareBuffer, fTokenLength) then
                              begin
                                Dec(fTokenLength);

                                if (chr = #10) then
                                begin
                                  Dec(fLineNo);
                                  fCharPos := fPrevCharPos;
                                end;

                                fNextChar       := chr;
                                fNextCharReady  := TRUE;
                                fEOF            := FALSE; // ?
                                result          := NIL;
                                BREAK;
                              end;
                            end;

          dcPrefixed,
          dcDelimited     : while NOT EOF do
                            begin
                              ReadChar(chr);

                              if result.IsComplete(fCompareBuffer, fTokenLength) then
                              begin
                                fNextCharReady := FALSE;
                                BREAK;
                              end;

                              if NOT result.IsCompatible(fCompareBuffer, fTokenLength) then
                              begin
                                Dec(fTokenLength);

                                if (chr = #10) then
                                begin
                                  Dec(fLineNo);
                                  fCharPos := fPrevCharPos;
                                end;

                                fNextChar       := chr;
                                fNextCharReady  := TRUE;
                                fEOF            := FALSE; // ?
                                result          := NIL;
                                BREAK;
                              end;
                            end;

          dcCharacterSet  : if NOT NextCharReady then
                              while NOT EOF do
                              begin
                                ReadChar(chr);

                                if NOT result.IsCompatible(fCompareBuffer, fTokenLength) then
                                begin
                                  Dec(fTokenLength);

                                  if (chr = #10) then
                                  begin
                                    Dec(fLineNo);
                                    fCharPos := fPrevCharPos;
                                  end;

                                  fNextChar       := chr;
                                  fNextCharReady  := TRUE;
                                  fEOF            := FALSE;
                                  BREAK;
                                end;
                              end;

        else
          if (result.Length <> fTokenLength) then
          begin
            while NOT EOF do
            begin
              ReadChar(chr);

              if NOT result.IsCompatible(fCompareBuffer, fTokenLength) then
              begin
                Dec(fTokenLength);

                if (chr = #10) then
                begin
                  Dec(fLineNo);
                  fCharPos := fPrevCharPos;
                end;

                fNextChar       := chr;
                fNextCharReady  := TRUE;
                fEOF            := FALSE;
                BREAK;
              end;
            end;
          end;
        end;
      end;

      fCompareBuffer^[fTokenLength] := WideChar(0);
      fTokenBuffer^[fTokenLength]   := WideChar(0);

      fPreviousCandidateTokens.Clear;

//    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReader.get_EOF: Boolean;
  begin
    result := fEOF and (NOT fNextCharReady);
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





end.
