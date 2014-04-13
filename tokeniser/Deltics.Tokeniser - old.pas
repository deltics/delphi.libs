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

   Implements a dictionary driven tokeniser, designed to accept an input
    string, stream or file and tokenise that input using an externally
    specified lexicon (dictionary), consisting of token definitions that
    may be, reserved words, delimited values or symbols composited from a
    set of valid characters, optionally with a different set of valid
    initial characters.

   - Change History

    5-Jul-06   Renamed as Deltics.Tokeniser.pas

   24-May-06   Renamed as DictParser.pas

   25-Apr-06   Newly optimised version.
   -
      Mar-06   Optimised version lost in server crash.  Non-optimised
                version restored
   -
      Jan-06   Initial version

  ----------------------------------------------------------------------------------------
}

{$i Deltics.Tokeniser.inc}

{$debuginfo ON}

  unit Deltics.Tokeniser;


interface

  uses
    Classes,
    Contnrs,
    Deltics.MultiCast,
    Deltics.Types,
    Deltics.Tokeniser.Dictionary,
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Formatter,
    Deltics.Tokeniser.Tokens;


type
  TDtxTokeniser = class;
  TTokeniserOption = (toOwnsTokens,
                      toDiscardWhitespace,
                      toNormaliseKeywords,
                      toNormaliseCase);
  TTokeniserOptions = set of TTokeniserOption;


  // Event type raised when a new token is added
  //
  // ToDo: Raising events 'inline' with tokenising incurs significant
  //        overhead.  Investigate replacing event mechanism with message
  //        based notification
  TTokenAdded = procedure(const Sender: TDtxTokeniser; const aToken: TDtxToken) of object;


  TDtxTokeniser = class
  private
    fDictionary: TTokenDictionary;
    fLineCount: Integer;

    eOnTokenAdded: TTokenAdded;

    fOptions: TTokeniserOptions;

//    function get_Progress: Integer;

    procedure DoTokenAdded;

    function get_CaseSensitive: Boolean;
    function get_DiscardWhitespace: Boolean;
    function get_LineCount: Integer;

  protected
    function CanAddToken(const aDefinition: TTokenDefinition): Boolean; virtual;

  public
    constructor Create(const aDictionary: TTokenDictionary;
                       const aOptions: TTokeniserOptions = [toOwnsTokens]);
//    destructor Destroy; override;

    procedure ChangeDictionary(const aDictionary: TTokenDictionary);
    procedure ChangeOptions(const aOptions: TTokeniserOptions);

//    function First(const aKindID: Integer): TDtxToken;
//    function Last(const aKindID: Integer): TDtxToken;

//    procedure Emit(const aStream: TStream;
//                   const aFormatter: TFormatter = NIL);

    function Tokenise(const aStream: TStream): TDtxTokenList; overload;
    function Tokenise(const aString: String): TDtxTokenList; overload;
    function TokeniseFile(const aFileName: String): TDtxTokenList;
    procedure TokeniseFiles(const aFileSpec: String; const aFileList: TStrings);

    property CaseSensitive: Boolean read get_CaseSensitive;
    property DiscardWhitespace: Boolean read get_DiscardWhitespace;
    property Dictionary: TTokenDictionary read fDictionary;
    property LineCount: Integer read get_LineCount;
    property Options: TTokeniserOptions read fOptions;
//    property Progress: Integer read get_Progress;

    property OnTokenAdded: TTokenAdded read eOnTokenAdded write eOnTokenAdded;
  end;





implementation

  uses
  {$ifdef UNICODE}
    Character,
  {$endif}
    Math,
    StrUtils,
    SysUtils,
    Windows,
    Deltics.Streams,
    Deltics.SysUtils,
    Deltics.Unicode;


  const
    BUFSIZE = 1024;

  type
    TDefinitionHelper = class(TTokenDefinition);
    TDictionaryHelper = class(TTokenDictionary);
    TTokenHelper = class(TDtxToken);
    TReadCharProc = procedure(var aChar: WideChar) of object;


    TProcessor = class
    private
      done: Boolean;
      gotNextChar: Boolean;
      isComplete: Boolean;

      fNewLine: Boolean;
      fCharPos: Integer;
      fPrevCharPos: Integer;
      fStartPos: Integer;
      fTabWidth: Integer;
      fCompareBuffer: PWideCharArray;
      fTokenBuffer: PWideCharArray;
      fMaxTokenLength: Integer;
      fTokenLength: Integer;
      fLineNo: Integer;

      fDictionary: TDictionaryHelper;
      fTokenDefinition: TTokenDefinition;
//      fStream: IBufferedStreamReader;
      fStream: TUnicodeStream;
      fTokeniser: TDtxTokeniser;
      fTokens: TDtxTokenList;

      ReadChar: TReadCharProc;

      procedure AllocTokenBuffers(const aBufSize: Integer);
      procedure FreeTokenBuffers;

      procedure AddToken;
      procedure ExtractTokens(var aLineCount: Integer);
      procedure ASCIIReadChar(var aChar: WideChar);
      procedure ASCIIReadCharNoCase(var aChar: WideChar);

      class function Execute(const aTokeniser: TDtxTokeniser;
                             const aStream: TUnicodeStream;
                             var aLineCount: Integer): TDtxTokenList;

      property Dictionary: TDictionaryHelper read fDictionary;
//      property Stream: IBufferedStreamReader read fStream;
      property Stream: TUnicodeStream read fStream;
      property TabWidth: Integer read fTabWidth;
      property TokenDefinition: TTokenDefinition read fTokenDefinition;
      property Tokeniser: TDtxTokeniser read fTokeniser;
      property Tokens: TDtxTokenList read fTokens;

    public
      constructor Create(const aTokeniser: TDtxTokeniser;
                         const aStream: TUnicodeStream;
                         const aList: TDtxTokenList);
      destructor Destroy; override;
    end;



  constructor TProcessor.Create(const aTokeniser: TDtxTokeniser;
                                const aStream: TUnicodeStream;
                                const aList: TDtxTokenList);
  begin
    inherited Create;

    fDictionary := TDictionaryHelper(aTokeniser.Dictionary);
//    fStream     := TBufferedStream.CreateReader(aStream, 4096);
    fStream     := aStream;
    fTokeniser  := aTokeniser;
    fTokens     := aList;

    AllocTokenBuffers(BUFSIZE);

    if Tokeniser.CaseSensitive then
      ReadChar := ASCIIReadChar
    else
      ReadChar := ASCIIReadCharNoCase;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TProcessor.Destroy;
  begin
    FreeTokenBuffers;

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TProcessor.AddToken;
  {
    Adds a token to the token list of the specified kind.  A flag indicates whether or not the
     next character (the first character of the next token) is ready for processing (TRUE) or has
     yet to be read (FALSE).
  }
  var
    TABWIDTH: Integer;
    i: Integer;
    ch: WideChar;
    token: TDtxToken;
    endpos: Integer;
    s: WideString;
  begin
    // TODO: Optimisation: Find a way to avoid position tracking calculations
    //        for situations when position information isn't required

    TABWIDTH  := 4;
    try
      // The current character position identifies the END of the
      //  token being added.  If the NextCharReady flag is TRUE,
      //  we have read beyond the end of the token and are
      //  positioned on the first character of the next token, so
      //  in that case the true end of the current token is the
      //  last character but one.
      if gotNextChar then
        endpos := fPrevCharPos
      else
        endpos := fCharPos;

      if NOT Assigned(TokenDefinition) then
      begin
        TTokenHelper.CreateUnknown(Tokens,
                                   WideString(fTokenBuffer),
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
      if Tokeniser.CanAddToken(TokenDefinition) then
      begin
        // TODO: This stuff can surely be optimised ?  e.g. cache prevDefinition

        token := NIL;

        if (toNormaliseKeywords in Tokeniser.Options) and (TokenDefinition.ClassID = dcString) then
          s := TDtxStringToken(TokenDefinition).Text
        else if (toNormaliseCase in Tokeniser.Options) and NOT (TokenDefinition.ClassID = dcDelimited) then
          s := WideString(fCompareBuffer^)
        else
          s := WideString(fTokenBuffer^);

        if (TokenDefinition.IsCompoundable and (fTokens.Count > 0)) then
        begin
          token := fTokens[Pred(fTokens.Count)];
          if (token.Definition = TokenDefinition) then
            TTokenHelper(token).Append(s, fTokenLength)
          else
            token := NIL;
        end;

        if NOT Assigned(token) then
          token := TTokenHelper.Create(Tokens, TokenDefinition, s, fStartPos, fTokenLength, fLineNo, fNewLine);

        if (TokenDefinition.ClassID = dcDelimited) then
        begin
//          if TDtxDelimitedToken(TokenDefinition).Tokenise then
//            token.Tokenise;

//          if TDelimitedTokenKind(kind).CanSpanLines then
//            Inc(fLineNo, token.LineSpan);
        end;

        fNewLine := FALSE;
      end;

      // If the token is a line feed then we increment the line count
      //  and reset the character position, being careful again to
      //  take account of whether or not the first character of the
      //  next token has already been read.  If it has then the new
      //  character position is already 1 (one).  If it hasn't then
      //  the new character position is 0 (zero), i.e. the start of
      //  the new line.
      if (TokenDefinition.ID = tkLF) then
      begin
        Inc(fLineNo);

        // fCharPos  := IfThen(gotNextChar, IfThen(chr = #9, TABWIDTH, 1), 0);
        if gotNextChar then
(*
          if (chr = #9) then
            fCharPos := TABWIDTH
          else
            fCharPos := 1
*)
        else
          fCharPos := 0;

        fNewLine  := TRUE;
      end;
    finally
      if Assigned(TokenDefinition) and (TokenDefinition.ID = tkLF) then
        fStartPos := fCharPos
      else if Assigned(TokenDefinition) and (TokenDefinition.ID = tkTab) then
        fStartPos := ((fStartPos div TABWIDTH) * TABWIDTH) + (TABWIDTH * fTokenLength)
      else
        // fStartPos := IfThen(gotNextChar, IfThen((chr = #9), (endpos div TABWIDTH) * TABWIDTH, endpos), endpos);

(*
      if gotNextChar and (chr = #9) then
        fStartPos := (endpos div TABWIDTH) * TABWIDTH
      else
        fStartPos := endpos;
*)
      Inc(fStartPos);
    end;
  end ;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TProcessor.ASCIIReadChar(var aChar: WideChar);
  var
    c: WideChar absolute aChar;
  begin
    if NOT gotNextChar then
    begin
      fPrevCharPos := fCharPos;
      Inc(fCharPos);

      Stream.ReadChar(c);

      if ANSIChar(c) = #9 then
        fCharPos := Succ(fCharPos div TabWidth) * TabWidth;

//      MultiByteToWideChar(CP_ACP, 0, @c, 1, @aChar, 1);
    end;

    gotNextChar := FALSE;

    if (fTokenLength = fMaxTokenLength) then
      AllocTokenBuffers(fMaxTokenLength + BUFSIZE);

    fTokenBuffer^[fTokenLength] := aChar;

    Inc(fTokenLength);

    done := Stream.EOF;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TProcessor.ASCIIReadCharNoCase(var aChar: WideChar);
  begin
    ASCIIReadChar(aChar);

    if IsUpper(aChar) then
      fCompareBuffer^[fTokenLength - 1] := CharLower(aChar)
    else
      fCompareBuffer^[fTokenLength - 1] := aChar;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TProcessor.AllocTokenBuffers(const aBufSize: Integer);
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

    if Tokeniser.CaseSensitive then
      fCompareBuffer := fTokenBuffer
    else
      ReallocMem(fCompareBuffer, aBufSize * sizeof(WideChar));

    fMaxTokenLength := aBufSize;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TProcessor.FreeTokenBuffers;
  {
    Returns all memory in use by the token buffers to the system.
  }
  begin
    ReallocMem(fTokenBuffer, 0);

    if Tokeniser.CaseSensitive then
      fCompareBuffer := NIL
    else
      ReallocMem(fCompareBuffer, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TProcessor.Execute(const aTokeniser: TDtxTokeniser;
                                    const aStream: TUnicodeStream;
                                    var aLineCount: Integer): TDtxTokenList;
  var
    proc: TProcessor;
  begin
    aLineCount  := 0;
    result      := TDtxTokenList.Create(TRUE);

    if (aStream.Size = 0) then
      EXIT;

    proc := TProcessor.Create(aTokeniser, aStream, result);
    try
      proc.ExtractTokens(aLineCount);
    finally
      proc.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TProcessor.ExtractTokens(var aLineCount: Integer);
  var
    i: Integer;
    chr: WideChar;
    candidates: TTokenDefinitions;
    prevCandidates: TTokenDefinitions;
  begin
    done        := FALSE;
    gotNextChar := FALSE;
    fNewLine    := TRUE;


    candidates     := NIL;
    prevCandidates := NIL;

//    fTabWidth     := Tokeniser.Dictionary.TabWidth;
    fTabWidth     := 4;
    fCharPos      := 0;
    fPrevCharPos  := 0;
    fStartPos     := 1;
    fLineNo       := 1;
    try
      candidates     := TTokenDefinitions.Create(FALSE);
      prevCandidates := TTokenDefinitions.Create(FALSE);

      while NOT done or gotNextChar do
      begin
        fTokenDefinition := NIL;

        if (prevCandidates.Count > 0) then
          prevCandidates.Clear;

        fTokenLength := 0;
        ReadChar(chr);

        // Get initial list of candidate token kinds
        CloneList(Dictionary.GetDefinitions(chr), candidates);

        // Now try and whittle down the candidates by building up the token
        while NOT done and (Candidates.Count > 1) do
        begin
          ReadChar(chr);

          // If the list has changed (number of entries has reduced), store the new list of
          //  compatible kinds to avoid having to look them up again later....

          if (candidates.Count <> prevCandidates.Count) then
            CloneList(candidates, prevCandidates);

          // Ask the dictionary to remove any definitions that are no longer compatible
          //  with the contents of the compare buffer

          Dictionary.FilterDefinitions(candidates, fCompareBuffer, fTokenLength);
        end;

        // Too far?  If we have no candidates but have built up a token,
        //  then we need to back track 1 character

        if (candidates.Count = 0) and (fTokenLength > 1) then
        begin
          Dec(fTokenLength);
          gotNextChar := TRUE;

          // Use the previously saved list of compatible kinds to find a
          //  most compatible match - if possible.
          //
          // (the last stored list is the list compatible with str before
          //  we lopped the last char off)

          if (prevCandidates.Count > 1) then
            fTokenDefinition := Dictionary.MostCompatible(prevCandidates, fTokenLength);
        end
        else if (candidates.Count = 1) then
          fTokenDefinition := Candidates[0]
        else if (candidates.Count > 1) then
          fTokenDefinition := Dictionary.MostCompatible(candidates, fTokenLength);


        // We have a candidate token definition, but the token itself may not yet
        //  be complete.  What determines whether and when the token is compeleted
        //  depends on the type of token definition we are dealing with...

        if Assigned(fTokenDefinition) then
        begin
          case fTokenDefinition.ClassID of
            dcPrefixed,
            dcDelimited     : while NOT done do
                              begin
                                ReadChar(chr);

                                if TDefinitionHelper(fTokenDefinition).IsComplete(fCompareBuffer, fTokenLength) then
                                begin
                                  gotNextChar := FALSE;
                                  BREAK;
                                end;

                                if NOT TDefinitionHelper(fTokenDefinition).IsCompatible(fCompareBuffer, fTokenLength) then
                                begin
                                  Dec(fTokenLength);
                                  gotNextChar := TRUE;
                                  fTokenDefinition := NIL;
                                  BREAK;
                                end;
                              end;

            dcCharacterSet  : if NOT gotNextChar then
                                while NOT done do
                                begin
                                  ReadChar(chr);

                                  if NOT TDefinitionHelper(TokenDefinition).IsCompatible(fCompareBuffer, fTokenLength) then
                                  begin
                                    Dec(fTokenLength);
                                    gotNextChar := TRUE;
                                    done := FALSE;
                                    BREAK;
                                  end;
                                end;

          else
            if (fTokenDefinition.Length <> fTokenLength) then
            begin
              while NOT done do
              begin
                ReadChar(chr);

                if NOT TDefinitionHelper(TokenDefinition).IsCompatible(fCompareBuffer, fTokenLength) then
                begin
                  Dec(fTokenLength);
                  gotNextChar := TRUE;
                  done := FALSE;
                  BREAK;
                end;
              end;
            end;
          end;
        end;

        fCompareBuffer^[fTokenLength] := WideChar(0);
        fTokenBuffer^[fTokenLength]   := WideChar(0);

        AddToken;
      end;
    finally
      FreeAndNIL([@prevCandidates,
                  @Candidates]);
    end;

    Inc(aLineCount, fLineNo);
  end;








  // ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
  //  TTokeniser
  // ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDtxTokeniser.Create(const aDictionary: TTokenDictionary;
                                   const aOptions: TTokeniserOptions);
  {
    Creates a tokeniser that will use the specified dictionary when tokenising and will own
     the tokens it identifies if required.
  }
  begin
    inherited Create;

    fDictionary := aDictionary;
    fOptions    := aOptions;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.get_CaseSensitive: Boolean;
  begin
    result := FALSE;
//    result := NOT Assigned(Dictionary) or Dictionary.CaseSensitive;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.get_DiscardWhitespace: Boolean;
  begin
    result := (toDiscardWhitespace in Options);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.get_LineCount: Integer;
  begin
    result := fLineCount;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
(*
  function TDtxTokeniser.get_Progress: Integer;
  {
    Returns the %age of the stream that has been tokenised so far.
  }
  begin
    if Assigned(Stream) then
      result := Ceil(100 * (Stream.Position / Stream.Size))
    else
      result := 0;
  end;
*)


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDtxTokeniser.DoTokenAdded;
  begin
    if Assigned(OnTokenAdded) then
//      OnTokenAdded(self, Items[Pred(Count)]);
  end;


(*
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.First(const aKindID: Integer): TDtxToken;
  var
    i: Integer;
  begin
    if (aKindID = tkAny) then
      result := Items[0]
    else
    begin
      result := NIL;

      for i := 0 to Pred(Count)do
      begin
        if (Items[i].TokenKind.ID = aKindID) then
        begin
          result := Items[i];
          BREAK;
        end;
      end;
    end;
  end;
*)

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.CanAddToken(const aDefinition: TTokenDefinition): Boolean;
  begin
    if DiscardWhitespace then
      result := (aDefinition.TokenType <> ttWhitespace)
    else
      result := TRUE;
  end;


(*
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.Last(const aKindID: Integer): TDtxToken;
  var
    i: Integer;
  begin
    if (aKindID = tkAny) then
      result := Items[Pred(Count)]
    else
    begin
      result := NIL;

      for i := Pred(Count) downto 0 do
      begin
        if (Items[i].TokenKind.ID = aKindID) then
        begin
          result := Items[i];
          BREAK;
        end;
      end;
    end;
  end;
*)

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.Tokenise(const aStream: TStream): TDtxTokenList;
  var
    unicode: TUnicodeStream;
  begin
    result := NIL;

    if (aStream is TUnicodeStream) then
      unicode := TUnicodeStream(aStream)
    else
      unicode := TUnicodeStream.Create(aStream);

    try
      result := TProcessor.Execute(self, unicode, fLineCount);
    finally
      if unicode <> aStream then
        FreeAndNIL(unicode);

    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.Tokenise(const aString: String): TDtxTokenList;
  var
    strm: TUnicodeStream;
  begin
    result := NIL;

    strm := TUnicodeStream.Create(aString);
    try
      result := Tokenise(strm);
    finally
      strm.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDtxTokeniser.TokeniseFile(const aFileName: String): TDtxTokenList;
  var
    strm: TFileStream;
  begin
    result := NIL;

    strm := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
    try
      strm.Position := 0;
      result := Tokenise(strm);
    finally
      strm.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDtxTokeniser.TokeniseFiles(const aFileSpec: String;
                                        const aFileList: TStrings);
  var
    i: Integer;
    filepath: String;
    files: TSearchRec;
    tokeniser: TDtxTokeniser;
  begin
    filepath := ExtractFilePath(aFileSpec);

    aFileList.Clear;
    try
      if (FindFirst(aFileSpec, 0, files) = 0) then
      repeat
        aFileList.AddObject(files.Name, Pointer(files.Size));
      until (FindNext(files) <> 0);
    finally
      SysUtils.FindClose(files);
    end;


    for i := 0 to Pred(aFileList.Count) do
      try
        aFileList.Objects[i] := TokeniseFile(filepath + aFileList[i]);
      except
        on e: Exception do
        begin
{
          e.Message := 'Error on line ' + IntToStr(TTokeniser(aFileList.Objects[i]).LineCount)
                     + ' in ' + filepath + aFileList[i] + #13#13 + e.Message;
}
          raise e;
        end;
      end;
  end;


(*
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDtxTokeniser.Emit(const aStream: TStream;
                            const aFormatter: TFormatter);
  const
    CRLF: array[0..1] of ANSIChar = (#13, #10);
    SPACE: array[0..0] of ANSIChar = (' ');
  var
    i: Integer;
    token: TDtxToken;
  begin
    if Count = 0 then
      EXIT;

    if DiscardWhitespace or Assigned(aFormatter) then
    begin
      if Assigned(aFormatter) then
      begin
        aFormatter.Stream := aStream;
      end
      else
      begin
        token := First(tkAny);
        while Assigned(token) and (token.Next(tkAny) <> NIL) do
        begin
          aStream.Write(token.TokenText[1], token.Length);

          if (token.LineNo <> token.Next(tkAny).LineNo) then
            aStream.Write(CRLF, 2)
          else
            aStream.Write(SPACE, 1);

          token := token.Next(tkAny);
        end;

        aStream.Write(token.TokenText[1], token.Length);
      end;
    end
    else
    begin
      for i := 0 to Pred(Count) do
        aStream.Write(Items[i].TokenText[1], Items[i].Length);
    end;
  end;
*)

  procedure TDtxTokeniser.ChangeDictionary(const aDictionary: TTokenDictionary);
  begin
    if (Dictionary = aDictionary) then
      EXIT;

    fDictionary := aDictionary;
//    Initialise;
  end;


  procedure TDtxTokeniser.ChangeOptions(const aOptions: TTokeniserOptions);
  begin
    if (Options = aOptions) then
      EXIT;

    fOptions := aOptions;
//    Initialise;
  end;






end.
