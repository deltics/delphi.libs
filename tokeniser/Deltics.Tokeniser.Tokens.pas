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


  unit Deltics.Tokeniser.Tokens;


interface

  uses
    Classes,
    Contnrs,
    Deltics.Classes,
    Deltics.Types,
    Deltics.Strings,
    Deltics.Tokeniser,
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;


  type
    TToken = class;
//    TTokenList = class;

//    ITokenCursor = interface;
//    ITokenList = interface;



    TToken = class(TCOMInterfacedObject, IToken)
    private
      fLength: Integer;
      fLines: TStringList;
      fLineSpan: Integer;
      fDefinition: TTokenDefinition;
      fSource: TTokenSource;
      fText: UnicodeString;
      fTokens: TObjectList;

      function get_Count: Integer;
      function get_Token(const aIndex: Integer): TToken;

      procedure DetermineLineSpan;
//      function get_Line(const aIndex: Integer): UnicodeString;
      function get_Lines: Classes.TStringList;
      function get_Dictionary: TTokenDictionary;

    protected
      constructor Create(const aDefinition: TTokenDefinition;
                         const aString: UnicodeString;
                         const aStartPos: Integer;
                         const aLength: Integer;
                         const aLineNo: Integer;
                         const aLineHead: Boolean); overload;

      constructor Create(const aDefinition: TTokenDefinition;
                         const aChar: WideChar;
                         const aPos: Integer;
                         const aLineNo: Integer;
                         const aLineHead: Boolean); overload;

      constructor CreateUnknown(const aString: UnicodeString;
                                const aStartPos: Integer;
                                const aLength: Integer;
                                const aLineNo: Integer;
                                const aLineHead: Boolean);

      procedure Append(const aText: String; const aLength: Integer);
      property Lines: Classes.TStringList read get_Lines;

    public
      destructor Destroy; override;

(*
      function Likeness(const aOther: TToken): Integer;
      function LikenessEx(const aOther: TToken;
                          var Map: ANSIString;
                          var OtherMap: ANSIString;
                          var CharsTheSame: Integer;
                          var CharsTotal: Integer): Integer;
*)      procedure Tokenise;

      property Count: Integer read get_Count;
      property Dictionary: TTokenDictionary read get_Dictionary;
//      property Index: Integer read fIndex;
//      property Line[const aIndex: Integer]: UnicodeString read get_Line;
//      property LineHead: Boolean read fLineHead;
//      property LineNo: Integer read fLineNo;
//      property LineSpan: Integer read fLineSpan;
//      property StartPos: Integer read fStartPos;
      property Tokens[const aIndex: Integer]: TToken read get_Token;

    protected // IToken
      function get_Definition: TTokenDefinition;
      function get_Length: Integer;
      function get_Source: TTokenSource;
      function get_Text: UnicodeString;
    public
      property Definition: TTokenDefinition read fDefinition;
      property Length: Integer read fLength;
      property Source: TTokenSource read fSource;
      property Text: UnicodeString read fText;
    end;



(*
    TLineHeadTokens = class(TTokenList)
    private
      fIncludeBlankLines: Boolean;
      function get_Text(const aIndex: Integer): String;
    protected
      procedure Initialise; override;
    public
      constructor Create(const aSource: TTokenList;
                         const aIncludeBlankLines: Boolean); reintroduce;
      property Text[const aIndex: Integer]: String read get_Text;
    end;
*)





implementation

  uses
  {$ifdef Deltics_Progress}
    Deltics.Progress,
  {$endif}
    Deltics.StrUtils,
    Deltics.SysUtils,
    // VCL/RTL
    Math,
    SysUtils,
    Windows;


  type
//    TTokeniserHelper = class(Deltics.Tokeniser.TTokeniser);
    TTokenType    = Deltics.Tokeniser.Consts.TTokenType;
    TTokenSource  = Deltics.Tokeniser.TTokenSource;

    TTokenSourceHelper = class(TTokenSource);



{ TToken ----------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TToken.Create(const aDefinition: TTokenDefinition;
                            const aString: UnicodeString;
                            const aStartPos: Integer;
                            const aLength: Integer;
                            const aLineNo: Integer;
                            const aLineHead: Boolean);
  begin
    inherited Create;

    fSource := TTokenSource.Create(aLineNo, aStartPos);

    fLength     := aLength;
    fDefinition := aDefinition;

    fText := aString;

    if (aDefinition is TDelimitedToken) then
    begin
      if aDefinition.MultiLine then
        DetermineLineSpan;

//      if TDelimitedTokenKind(aTokenKind).Tokenise then
//        fTokens := TObjectList.Create(TRUE);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TToken.Create(const aDefinition: TTokenDefinition;
                            const aChar: WideChar;
                            const aPos: Integer;
                            const aLineNo: Integer;
                            const aLineHead: Boolean);
  begin
    Create(aDefinition, UnicodeString(aChar), 1, aPos, aLineNo, aLineHead);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TToken.CreateUnknown(const aString: UnicodeString;
                                   const aStartPos: Integer;
                                   const aLength: Integer;
                                   const aLineNo: Integer;
                                   const aLineHead: Boolean);
  begin
    Create(NIL, aString, aStartPos, aLength, aLineNo, aLineHead);

    DetermineLineSpan;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TToken.Destroy;
  begin
    FreeAndNIL(fLines);
    FreeAndNIL(fSource);
    FreeAndNIL(fTokens);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TToken.Append(const aText: String; const aLength: Integer);
  begin
    fText := fText + Copy(aText, 1, aLength);
    Inc(fLength, aLength);
  end;


(*
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.Likeness(const aOther: TToken): Integer;
  const
    SAMEKIND: array[FALSE..TRUE] of Integer = (0, 100);
  begin
    result := SAMEKIND[Definition = aOther.Definition];

    if (result = 0)
//     or Definition.IsKeyword
     or (Text = aOther.Text) then
      EXIT;

    result := Deltics.StrUtils.Likeness(Text, aOther.Text);
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.LikenessEx(const aOther: TToken;
                             var Map: ANSIString;
                             var OtherMap: ANSIString;
                             var CharsTheSame: Integer;
                             var CharsTotal: Integer): Integer;
  const
    SAMEKIND: array[FALSE..TRUE] of Integer = (0, 100);
  begin
    result := SAMEKIND[Definition = aOther.Definition];

//    if (result = 0) or Definition.IsKeyword then
    begin
      CharsTheSame := 0;
      CharsTotal   := Length + aOther.Length;
      EXIT;
    end;

    if (Text = aOther.Text) then
    begin
      Map      := Text;
      OtherMap := Text;
      CharsTheSame := System.Length(Text);
      CharsTotal   := CharsTheSame * 2;
      EXIT;
    end;

    result := Deltics.StrUtils.LikenessEx(Text, aOther.Text, Map, OtherMap, CharsTheSame, CharsTotal);
  end;
*)



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TToken.DetermineLineSpan;
  var
    i: Integer;
    ch: PWideChar;
  begin
    ch := PWideChar(Text);
    for i := 1 to System.Length(Text) do
    begin
      if (ch^ = #10) then
        Inc(fLineSpan);
      Inc(ch);
    end;

    TTokenSourceHelper(Source).SetLineSpan(fLineSpan);

//    if (LineSpan > 0) then
//      Lines;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Count: Integer;
  begin
    if Assigned(fTokens) then
      result := fTokens.Count
    else
      result := 0;
  end;


(*
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Line(const aIndex: Integer): UnicodeString;
  begin
    result := Lines[aIndex];

//    if NOT {Tokeniser}.DiscardWhitespace then
//      EXIT;

    result := Trim(result);

    result := StringReplace(result, #9, ' ', [rfReplaceAll]);
    while Pos('  ', result) > 0 do
      result := StringReplace(result, '  ', ' ', [rfReplaceAll]);
  end;
*)

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Length: Integer;
  begin
    result := fLength;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Lines: Classes.TStringList;
  begin
    if {(LineSpan > 0) and} NOT Assigned(fLines) then
    begin
      fLines := TStringList.Create;
      fLines.Text := Text;
    end;

    result := fLines;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Definition: TTokenDefinition;
  begin
    result := fDefinition;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Source: TTokenSource;
  begin
    result := fSource;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Text: UnicodeString;
  begin
    result := fText;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Token(const aIndex: Integer): TToken;
  begin
    ASSERT(Assigned(fTokens));
    result := TToken(fTokens[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TToken.Tokenise;
//  var
//    inner: TTokeniser;
  begin
    ASSERT((Definition is TDelimitedToken), 'Can only tokenise delimited tokens');

//    inner := NIL;
    try
      // The tokens created by tokenising the token will be owned by the
      //  original tokeniser of that token (if it owns the tokens), not by
      //  the tokenise we are creating for the purpose

(*
      if Assigned(Definition.SubDictionary) then
        inner := TTokeniser.Create(Definition.SubDictionary, TTokeniser(Owner).Options - [toOwnsTokens])
      else
        inner := TTokeniser.Create(Definition.SubDictionary, TTokeniser(Owner).Options - [toOwnsTokens]) ;
*)

      {$ifdef Deltics_Progress}
      ProgressMeter.CreateProcess(0, Text, System.Length(Text));
      {$endif}

//      inner.Tokenise(Text);

//      fLineSpan := inner.LineCount - 1;

//      CloneList(TDtxTokeniserHelper(inner).List, fTokens);
    finally
//      inner.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TToken.get_Dictionary: TTokenDictionary;
  begin
    result := Definition.Dictionary;

    ASSERT(Assigned(result));
  end;









(*
{ TLineHeadTokens -------------------------------------------------------------------------------- }

  constructor TLineHeadTokens.Create(const aSource: TTokenList;
                                     const aIncludeBlankLines: Boolean);
  begin
    fIncludeBlankLines := aIncludeBlankLines;

    inherited Create(aSource);
  end;


  function TLineHeadTokens.get_Text(const aIndex: Integer): String;
  var
    token: TDtxToken;
  begin
    result  := '';
    token   := Items[aIndex];

    if Assigned(token) and (token.LineSpan > 0) then
      result := token.Line[(aIndex + 1) - token.LineNo]
    else
    begin
      while Assigned(token) and (token.LineNo = Succ(aIndex))
       and (token.Definition.ID <> tkCR) and (token.Definition.ID <> tkLF) do
      begin
        result := result + token.Text + ' ';
        token  := token.Next(tkAny);
      end;

      if (Length(result) > 0) then
        SetLength(result, Length(result) - 1);
    end;
  end;


  procedure TLineHeadTokens.Initialise;
  var
    i, j: Integer;
    prevLineNo: Integer;
    token: TDtxToken;
  begin
    prevLineNo := 0;

    for i := 0 to Pred(Source.Count) do
    begin
      token := Source[i];
//      if (token.LineNo <> prevLineNo) then
      if (token.LineHead) then
      begin
        if fIncludeBlankLines then
        begin
          while (token.LineNo - prevLineNo) > 1 do
          begin
            Add(NIL);
            Inc(prevLineNo);
          end;
        end;

        for j := 0 to token.LineSpan do
          Add(token);

        prevLineNo := token.LineNo + token.LineSpan;
      end;
    end;
  end;
*)









end.
