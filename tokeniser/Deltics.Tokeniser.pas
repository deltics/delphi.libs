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

      Apr-14   Massively re-written and updated for Unicode support and reference
                counted token lists, streams etc.

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

{$i deltics.tokeniser.inc}

{.$ifdef deltics_tokeniser}
  {$debuginfo ON}
{.$else}
  {.$debuginfo OFF}
{.$endif}


  unit Deltics.Tokeniser;


interface

  uses
  { vcl: }
    Classes,
    SysUtils,
  { deltics: }
    Deltics.Classes,
    Deltics.Strings,
    Deltics.Unicode,
  { deltics.tokeniser: }
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;


  type
    ITokenList = interface;
    ITokenStream = interface;


    TTokenSource = class
    private
      fLineFrom: Integer;
      fLineTo: Integer;
      fStartPos: Integer;
      function get_LineSpan: Integer;
    protected
      procedure SetLineTo(const aLineNo: Integer);
    public
      constructor Create(const aLine, aStart: Integer);
      property Line: Integer read fLineFrom;
      property LineFrom: Integer read fLineFrom;
      property LineTo: Integer read fLineTo;
      property LineSpan: Integer read get_LineSpan;
      property StartPos: Integer read fStartPos;
    end;


    IToken = interface
    ['{9DF5B974-DD3C-4CCF-A017-D15585663408}']
      function get_Definition: TTokenDefinition;
      function get_ID: TTokenID;
      function get_IsWhitespace: Boolean;
      function get_Length: Integer;
      function get_Source: TTokenSource;
      function get_Text: UnicodeString;
      function get_TokenCount: Integer;
      function get_Token(const aIndex: Integer): IToken;

      procedure GetTokenList(var aList: ITokenList);

      property Definition: TTokenDefinition read get_Definition;
      property ID: TTokenID read get_ID;
      property IsWhitespace: Boolean read get_IsWhitespace;
      property TokenCount: Integer read get_TokenCount;
      property Tokens[const aIndex: Integer]: IToken read get_Token;
      property Length: Integer read get_Length;
      property Source: TTokenSource read get_Source;
      property Text: UnicodeString read get_Text;
    end;


    ITokenCursor = interface
    ['{C4696924-5820-4CBB-A331-1BC615C93D44}']
      function get_EOF: Boolean;
      function get_Token: IToken;

      function CreateList(const aFrom, aTo: IToken; const aInclusive: Boolean = TRUE): ITokenList;
      function CreateParentheticalList: ITokenList;
      function First: IToken; overload;
      function First(const aID: TTokenID): IToken; overload;
      function First(const aID: TTokenID; const aText: UnicodeString): IToken; overload;
      function Last: IToken; overload;
      function Locate(const aToken: IToken): Boolean;
      function Next: IToken; overload;
      function Next(const aID: TTokenID): IToken; overload;
      function Next(const aID: TTokenID; const aText: UnicodeString): IToken; overload;
      function Prev: IToken; overload;
      function Prev(const aID: TTokenID): IToken; overload;

      procedure Restore;
      function SavePoint: IToken;

      function Clone: ITokenCursor;

      property EOF: Boolean read get_EOF;
      property Token: IToken read get_Token;
    end;


    ITokenList = interface
    ['{218D0AF0-8D13-40BD-B6CD-AACD005F2C83}']
      function get_Count: Integer;
      function get_First: IToken;
      function get_Item(const aIndex: Integer): IToken;
      function get_Last: IToken;

      procedure Add(const aText: UnicodeString);
      function IndexOf(const aToken: IToken): Integer;
      procedure Insert(const aIndex: Integer; const aText: UnicodeString);
      procedure Replace(const aToken: IToken; const aText: String);
      function Slice(const aFrom, aTo: Integer): ITokenList;

      property Count: Integer read get_Count;
      property First: IToken read get_First;
      property Item[const aIndex: Integer]: IToken read get_Item; default;
      property Last: IToken read get_Last;
    end;


    ITokenStream = interface
    ['{E73877DA-F97C-48F3-A8D4-FE60D6E29EF8}']
      function get_EOF: Boolean;
      function get_Token: IToken;

      function Read: IToken;

      property EOF: Boolean read get_EOF;
      property Token: IToken read get_Token;
    end;


    TTokenListFileProc = procedure(const aFilename: UnicodeString; const aTokenList: ITokenList; const aLines: Integer) of object;


    TTokenList = class(TFlexInterfacedObject, ITokenList)
    private
      fItems: TInterfaceList;
      procedure CopyFrom(const aSource: ITokenList);
    public
      constructor Create; overload;
      constructor CreateClone(const aSource: ITokenList);
      constructor CreateManaged;
      constructor CreateManagedClone(const aSource: ITokenList);
      destructor Destroy; override;

      class function Filter(const aSource: ITokenList; const aID: TTokenID): ITokenList; overload;
      class function Create(const aSource: UnicodeString; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = [toConsumeWhitespace]): ITokenList; overload;
      class function Create(const aSource: IUnicodeReader; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = [toConsumeWhitespace]): ITokenList; overload;
      class function Create(const aSource: IUnicodeReader; var aLines: Integer; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = [toConsumeWhitespace]): ITokenList; overload;
      class procedure ForEachFile(const aFilePattern: UnicodeString; const aSubDirs: Boolean; const aCallback: TTokenListFileProc; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = []);
      class function LoadFromFile(const aFilename: UnicodeString; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = [toConsumeWhitespace]): ITokenList; overload;
      class function LoadFromFile(const aFilename: UnicodeString; const aEncoding: TEncoding; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = [toConsumeWhitespace]): ITokenList; overload;
      class function LoadFromFile(const aFilename: UnicodeString; var aLines: Integer; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = [toConsumeWhitespace]): ITokenList; overload;
      class function LoadFromFile(const aFilename: UnicodeString; const aEncoding: TEncoding; var aLines: Integer; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = [toConsumeWhitespace]): ITokenList; overload;

      procedure Clear;
      procedure Delete(const aIndex: Integer);
      procedure Insert(const aIndex: Integer; const aToken: IToken); overload;
      procedure Remove(const aToken: IToken);

    protected // ITokenList ---------------------------------------------------
      function get_Count: Integer;
      function get_First: IToken;
      function get_Item(const aIndex: Integer): IToken;
      function get_Last: IToken;
    public
      procedure Add(const aToken: IToken); overload;
      procedure Add(const aText: UnicodeString); overload;
      procedure Add(const aTokenList: ITokenList); overload;
      procedure Insert(const aIndex: Integer; const aText: UnicodeString); overload;
      function IndexOf(const aToken: IToken): Integer;
      procedure Replace(const aToken: IToken; const aText: String);
      function Slice(const aFrom, aTo: Integer): ITokenList;
    public
      property Count: Integer read get_Count;
      property Items[const aIndex: Integer]: IToken read get_Item; default;
      property First: IToken read get_First;
      property Last: IToken read get_Last;
    end;


    TTokenStream = class(TCOMInterfacedObject, ITokenStream)
    protected
      fToken: IToken;
    public
      class function Create(const aSource: ITokenList; const aInitialIndex: Integer = 0): ITokenStream; overload;
      class function Create(const aSource: UnicodeString; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = []): ITokenStream; overload;
      class function Create(const aSource: IUnicodeReader; const aOwnsStream: Boolean; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = []): ITokenStream; overload;
      class function LoadFromFile(const aFilename: UnicodeString; const aDictionary: TTokenDictionary; const aOptions: TTokeniserOptions = []): ITokenStream;

    protected // ITokenStream -------------------------------------------------
      function get_EOF: Boolean; virtual; abstract;
      function get_Token: IToken;
      function Read: IToken; virtual; abstract;
    end;


    TTokenCursor = class(TCOMInterfacedObject, ITokenCursor)
    private
      fEOFIndex: Integer;
      fIgnoreWhitespace: Boolean;
      fIndex: Integer;
      fList: ITokenList;
      fSavePoints: array of Integer;
      fToken: IToken;
    public
      constructor Create(const aToken: IToken); overload;
      constructor Create(const aList: ITokenList); overload;
      constructor Create(const aList: ITokenList; const aInitialIndex: Integer); overload;

    protected // ITokenCursor -------------------------------------------------
      function get_EOF: Boolean;
      function get_Token: IToken;

      function CreateList(const aFrom, aTo: IToken; const aInclusive: Boolean): ITokenList;
      function CreateParentheticalList: ITokenList;
      function First: IToken; overload;
      function First(const aID: TTokenID): IToken; overload;
      function First(const aID: TTokenID; const aText: UnicodeString): IToken; overload;
      function Last: IToken; overload;
      function Locate(const aToken: IToken): Boolean;
      function Next: IToken; overload;
      function Next(const aID: TTokenID): IToken; overload;
      function Next(const aID: TTokenID; const aText: UnicodeString): IToken; overload;
      function Prev: IToken; overload;
      function Prev(const aID: TTokenID): IToken; overload;
      procedure Restore;
      function SavePoint: IToken;

      function Clone: ITokenCursor;
      property IgnoreWhitespace: Boolean read fIgnoreWhitespace write fIgnoreWhitespace;
    end;



implementation

  uses
  { deltics: }
    Deltics.SysUtils,
  { deltics.tokeniser: }
    Deltics.Tokeniser.TokenReader,
    Deltics.Tokeniser.Tokens;



  type
    TTokenHelper = class(TToken);


  { Concrete stream implementations -------------------------------------------------------------- }
  type
    TTokenListStream = class(TTokenStream)
    private
      fEOFIndex: Integer;
      fIndex: Integer;
      fList: ITokenList;
    protected
      constructor Create(const aList: ITokenList; const aIndex: Integer);

    protected // ITokenStream
      function get_EOF: Boolean; override;
      function Read: IToken; override;
    end;


    TTokenReaderStream = class(TTokenStream)
    private
      fReader: TTokenReader;
      fSource: IUnicodeReader;
    protected
      constructor Create(const aReader: TTokenReader; const aSource: IUnicodeReader);
    public
      destructor Destroy; override;

    protected // ITokenStream
      function get_EOF: Boolean; override;
      function Read: IToken; override;
    end;






{ TTokenList ------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenList.Create(const aSource: UnicodeString;
                                   const aDictionary: TTokenDictionary;
                                   const aOptions: TTokeniserOptions): ITokenList;
  var
    reader: IUnicodeReader;
  begin
    reader := TUnicodeReader.OfWIDE(aSource);
    result := Create(reader, aDictionary, aOptions);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenList.Create(const aSource: IUnicodeReader;
                                   const aDictionary: TTokenDictionary;
                                   const aOptions: TTokeniserOptions): ITokenList;
  var
    notUsed: Integer;
  begin
    result := Create(aSource, notUsed, aDictionary, aOptions);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenList.Create(const aSource: IUnicodeReader;
                                   var aLines: Integer;
                                   const aDictionary: TTokenDictionary;
                                   const aOptions: TTokeniserOptions): ITokenList;
  var
    list: TTokenList;
    reader: TTokenReader;
  begin
    list   := TTokenList.CreateManaged;
    result := list;

    reader := TTokenReader.Create(aSource, aDictionary, aOptions);
    try
      while NOT reader.EOF do
        list.Add(reader.Next);

      aLines := reader.LineNo;

    finally
      reader.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenList.LoadFromFile(const aFilename: UnicodeString;
                                         const aDictionary: TTokenDictionary;
                                         const aOptions: TTokeniserOptions): ITokenList;
  var
    notUsed: Integer;
  begin
    result := LoadFromFile(aFilename, TEncoding.UTF8, notUsed, aDictionary, aOptions);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenList.LoadFromFile(const aFilename: UnicodeString;
                                         const aEncoding: TEncoding;
                                         const aDictionary: TTokenDictionary;
                                         const aOptions: TTokeniserOptions): ITokenList;
  var
    notUsed: Integer;
  begin
    result := LoadFromFile(aFilename, aEncoding, notUsed, aDictionary, aOptions);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenList.LoadFromFile(const aFilename: UnicodeString;
                                         var aLines: Integer;
                                         const aDictionary: TTokenDictionary;
                                         const aOptions: TTokeniserOptions): ITokenList;
  begin
    result := LoadFromFile(aFilename, TEncoding.UTF8, aLines, aDictionary, aOptions);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenList.LoadFromFile(const aFilename: UnicodeString;
                                         const aEncoding: TEncoding;
                                         var aLines: Integer;
                                         const aDictionary: TTokenDictionary;
                                         const aOptions: TTokeniserOptions): ITokenList;
  var
    stream: TFileStream;
    reader: IUnicodeReader;
  begin
    stream := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyWrite);
    reader := TUnicodeReader.AsOwnerOfStream(stream, aEncoding);

    result := Create(reader, aLines, aDictionary, aOptions);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenList.Filter(const aSource: ITokenList; const aID: TTokenID): ITokenList;
  var
    i: Integer;
    list: TTokenList;
    token: IToken;
  begin
    list    := TTokenList.CreateManaged;
    result  := list;

    for i := 0 to Pred(aSource.Count) do
    begin
      token := aSource[i];
      if token.Definition.ID = aID then
        list.Add(token);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TTokenList.ForEachFile(const aFilePattern: UnicodeString;
                                         const aSubDirs: Boolean;
                                         const aCallback: TTokenListFileProc;
                                         const aDictionary: TTokenDictionary;
                                         const aOptions: TTokeniserOptions = []);
  var
    rec: TSearchRec;
    path: UnicodeString;
    filename: UnicodeString;
    tokens: ITokenList;
    lines: Integer;
  begin
    path      := ExtractFilePath(aFilePattern);
    filename  := ExtractFileName(aFilePattern);

    if FindFirst(aFilePattern, faAnyfile - faDirectory, rec) = 0 then
    try
      repeat
        tokens := LoadFromFile(path + rec.Name, lines, aDictionary, aOptions);
        aCallback(path + rec.Name, tokens, lines);

      until FindNext(rec) <> 0;

    finally
      FindClose(rec);
    end;

    if NOT aSubDirs then
      EXIT;

    if FindFirst(path + '\*.*', faDirectory, rec) = 0 then
    try
      repeat
        if (rec.Name = '.') or (rec.Name = '..') then
          CONTINUE;

        ForEachFile(path + rec.Name + '\' + filename, aSubDirs, aCallback, aDictionary, aOptions);
      until FindNext(rec) <> 0;

    finally
      FindClose(rec);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenList.Create;
  begin
    DisableRefCount;
    CreateManaged;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenList.CreateClone(const aSource: ITokenList);
  begin
    Create;
    CopyFrom(aSource);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenList.CreateManaged;
  begin
    inherited Create;

    fItems := TInterfaceList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenList.CreateManagedClone(const aSource: ITokenList);
  begin
    CreateManaged;
    CopyFrom(aSource);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTokenList.Destroy;
  begin
    FreeAndNIL(fItems);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenList.get_Count: Integer;
  begin
    result := fItems.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenList.get_First: IToken;
  begin
    result := fItems[0] as IToken;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenList.get_Item(const aIndex: Integer): IToken;
  begin
    result := fItems[aIndex] as IToken;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenList.get_Last: IToken;
  begin
    result := fItems[Count - 1] as IToken;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Add(const aToken: IToken);
  begin
    if Assigned(aToken) then
      fItems.Add(aToken as IUnknown);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Add(const aText: UnicodeString);
  begin
    Add(TTokenHelper.Create(NIL, aText, 0, 0, 0, 0));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Add(const aTokenList: ITokenList);
  var
    i: Integer;
  begin
    for i := 0 to Pred(aTokenList.Count) do
      Add(aTokenList[i]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Insert(const aIndex: Integer;
                              const aText: UnicodeString);
  begin
    Insert(aIndex, TTokenHelper.Create(NIL, aText, 0, 0, 0, 0));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Insert(const aIndex: Integer;
                              const aToken: IToken);
  begin
    fItems.Insert(aIndex, aToken as IUnknown);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Clear;
  begin
    fItems.Clear;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.CopyFrom(const aSource: ITokenList);
  var
    i: Integer;
  begin
    fItems.Clear;

    if NOT Assigned(aSource) then
      EXIT;

    fItems.Capacity := aSource.Count;

    for i := 0 to Pred(aSource.Count) do
      fItems.Add(aSource[i]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Delete(const aIndex: Integer);
  begin
    fItems.Delete(aIndex);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenList.IndexOf(const aToken: IToken): Integer;
  begin
    result := fItems.IndexOf(aToken as IUnknown);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Remove(const aToken: IToken);
  var
    idx: Integer;
  begin
    idx := fItems.IndexOf(aToken as IUnknown);
    if idx <> -1 then
      Delete(idx);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTokenList.Replace(const aToken: IToken; const aText: String);
  var
    idx: Integer;
  begin
    idx := fItems.IndexOf(aToken as IUnknown);
    if idx <> -1 then
      fItems[idx] := TTokenHelper.Create(NIL, aText, 0, 0, 0, 0) as IUnknown;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenList.Slice(const aFrom, aTo: Integer): ITokenList;
  var
    i: Integer;
    list: TTokenList;
  begin
    list := TTokenList.CreateManaged;

    list.fItems.Capacity := (aTo - aFrom) + 1;

    for i := aFrom to aTo do
      list.fItems.Add(fItems[i]);

    result := list;
  end;





{ TTokenStream ----------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenStream.Create(const aSource: ITokenList;
                                     const aInitialIndex: Integer): ITokenStream;
  begin
    result := TTokenListStream.Create(aSource, aInitialIndex);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenStream.Create(const aSource: UnicodeString;
                                     const aDictionary: TTokenDictionary;
                                     const aOptions: TTokeniserOptions): ITokenStream;
  var
    reader: IUnicodeReader;
  begin
    reader := TUnicodeReader.OfWIDE(aSource);
    result := Create(reader, TRUE, aDictionary, aOptions);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenStream.Create(const aSource: IUnicodeReader;
                                     const aOwnsStream: Boolean;
                                     const aDictionary: TTokenDictionary;
                                     const aOptions: TTokeniserOptions): ITokenStream;
  var
    reader: TTokenReader;
  begin
    reader := TTokenReader.Create(aSource, aDictionary, aOptions);

    if aOwnsStream then
      result := TTokenReaderStream.Create(reader, aSource)
    else
      result := TTokenReaderStream.Create(reader, NIL);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TTokenStream.LoadFromFile(const aFilename: UnicodeString;
                                           const aDictionary: TTokenDictionary;
                                           const aOptions: TTokeniserOptions): ITokenStream;
  var
    stream: TFileStream;
    source: IUnicodeReader;
    reader: TTokenReader;
  begin
    stream := TFileStream.Create(aFilename, fmOpenRead or fmShareDenyWrite);
    source := TUnicodeReader.AsOwnerOfStream(stream, TEncoding.UTF8);
    reader := TTokenReader.Create(source, aDictionary, aOptions);

    result := TTokenReaderStream.Create(reader, source);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenStream.get_Token: IToken;
  begin
    result := fToken;
  end;











{ TTokenListStream ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenListStream.Create(const aList: ITokenList;
                                      const aIndex: Integer);
  begin
    inherited Create;

    fEOFIndex := aList.Count - 1;
    fList     := aList;
    fIndex    := Min(aIndex - 1, fEOFIndex);

    Read;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenListStream.get_EOF: Boolean;
  begin
    result := (fIndex = fEOFIndex);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenListStream.Read: IToken;
  begin
    if (fIndex < fEOFIndex) then
    begin
      Inc(fIndex);
      fToken := fList[fIndex] as IToken;
    end
    else
      fToken := NIL;

    result := fToken;
  end;










{ TTokenReaderStream ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTokenReaderStream.Create(const aReader: TTokenReader; const aSource: IUnicodeReader);
  begin
    inherited Create;

    fReader := aReader;
    fSource := aSource;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTokenReaderStream.Destroy;
  begin
    FreeAndNIL(fReader);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReaderStream.get_EOF: Boolean;
  begin
    result := fReader.EOF;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTokenReaderStream.Read: IToken;
  begin
    fToken := fReader.Next;
    result := fToken;
  end;








{ TTokenSource }

  constructor TTokenSource.Create(const aLine, aStart: Integer);
  begin
    inherited Create;

    fLineFrom := aLine;
    fStartPos := aStart;
  end;


  function TTokenSource.get_LineSpan: Integer;
  begin
    result := (fLineTo - fLineFrom) + 1;
  end;


  procedure TTokenSource.SetLineTo(const aLineNo: Integer);
  begin
    fLineTo := aLineNo;
  end;







{ TTokenCursor }

  constructor TTokenCursor.Create(const aToken: IToken);
  var
    list: ITokenList;
  begin
    aToken.GetTokenList(list);
    Create(list);
  end;


  constructor TTokenCursor.Create(const aList: ITokenList);
  begin
    inherited Create;

    fIndex    := -1;
    fEOFIndex := aList.Count;
    fList     := aList;

    if (fEOFIndex > 0) then
      First
    else
      fIndex := 0;
  end;


  constructor TTokenCursor.Create(const aList: ITokenList;
                                  const aInitialIndex: Integer);
  begin
    Create(aList);

    fIndex := aInitialIndex;
  end;


  function TTokenCursor.CreateList(const aFrom, aTo: IToken; const aInclusive: Boolean): ITokenList;
  var
    list: TTokenList;
  begin
    list := TTokenList.CreateManaged;

    if Locate(aFrom) then
    begin
      if aInclusive then
        list.Add(aFrom);

      while (NOT get_EOF) and (fToken <> aTo) do
      begin
        list.Add(fToken);
        Next;
      end;

      if aInclusive and (fToken = aTo) then
        list.Add(aTo);
    end;

    result := list;
  end;


  function TTokenCursor.CreateParentheticalList: ITokenList;
  var
    list: TTokenList;
    nest: Integer;
  begin
    result := NIL;

    if (fToken.ID <> tkLeftParenthesis) then
      raise Exception.Create('Cursor is not positioned on a parenthetical token');

    list := TTokenList.CreateManaged;

    nest := 0;
    repeat
      case fToken.ID of
        tkLeftParenthesis : Inc(nest);
        tkRightParenthesis: Dec(nest);
      end;

      list.Add(fToken);
      if NOT get_EOF then
        self.Next;

    until (get_EOF) or (nest = 0);

    // Remove parentheticals from the resulting token list

    list.Delete(0);
    list.Delete(list.Count - 1);

    result := list;
  end;


  function TTokenCursor.Clone: ITokenCursor;
  begin
    result := TTokenCursor.Create(fList, fIndex);
  end;


  function TTokenCursor.First: IToken;
  begin
    if (fList.Count > 0) then
    begin
      fIndex := 0;
      fToken := fList[0] as IToken;
    end
    else
      fToken := NIL;

    result := fToken;
  end;


  function TTokenCursor.First(const aID: TTokenID): IToken;
  begin
    First;
    result := Next(aID);
  end;


  function TTokenCursor.First(const aID: TTokenID;
                              const aText: UnicodeString): IToken;
  begin
    First;
    result := Next(aID, aText);
  end;


  function TTokenCursor.get_EOF: Boolean;
  begin
    result := (fIndex = fEOFIndex);
  end;


  function TTokenCursor.get_Token: IToken;
  begin
    result := fToken;
  end;


  function TTokenCursor.Last: IToken;
  begin
    fIndex := Pred(fEOFIndex);

    if (fIndex > -1) then
      fToken := fList[fIndex] as IToken
    else
      fToken := NIL;

    result := fToken;
  end;


  function TTokenCursor.Locate(const aToken: IToken): Boolean;
  begin
    First;
    while NOT get_EOF and (fToken <> aToken) do
      Next;

    result := NOT get_EOF;
  end;


  function TTokenCursor.Next: IToken;
  begin
    if NOT get_EOF then
      Inc(fIndex);

    if fIndex < fEOFIndex then
      fToken := fList[fIndex]
    else
      fToken := NIL;

    result := fToken;
  end;


  function TTokenCursor.Next(const aID: TTokenID): IToken;
  var
    i: Integer;
  begin
    for i := Succ(fIndex) to Pred(fList.Count) do
    begin
      result := fList[i];
      if Assigned(result.Definition) and (result.Definition.ID = aID) then
      begin
        fIndex := i;
        fToken := result;
        EXIT;
      end;
    end;

    fIndex := fEOFIndex;
    fToken := NIL;
    result := NIL;
  end;


  function TTokenCursor.Next(const aID: TTokenID; const aText: UnicodeString): IToken;
  var
    i: Integer;
  begin
    for i := Succ(fIndex) to Pred(fList.Count) do
    begin
      result := fList[i];
      if Assigned(result.Definition) and (result.Definition.ID = aID)
       and (result.Text = aText) then
      begin
        fIndex := i;
        fToken := result;
        EXIT;
      end;
    end;

    fIndex := fEOFIndex;
    fToken := NIL;
    result := NIL;
  end;


  function TTokenCursor.Prev: IToken;
  begin
    if (fIndex = 0) then
      raise Exception.Create('No more tokens');

    Dec(fIndex);
    if (fIndex >= 0) then
      fToken := fList[fIndex]
    else
      fToken := NIL;

    result := fToken;
  end;


  function TTokenCursor.Prev(const aID: TTokenID): IToken;
  var
    i: Integer;
  begin
    for i := Pred(fIndex) downto 0 do
    begin
      result := fList[i];
      if Assigned(result.Definition) and (result.Definition.ID = aID) then
      begin
        fIndex := i;
        fToken := result;
        EXIT;
      end;
    end;

    fIndex := fEOFIndex;
    fToken := NIL;
    result := NIL;
  end;


  procedure TTokenCursor.Restore;
  var
    idx: Integer;
  begin
    idx := Length(fSavePoints) - 1;

    if (idx = -1) then
      raise Exception.Create('No savepoint to restore in cursor');

    fIndex := fSavePoints[idx];
    fToken := fList[fIndex];
    SetLength(fSavePoints, idx);
  end;


  function TTokenCursor.SavePoint: IToken;
  var
    idx: Integer;
  begin
    result := fToken;

    idx := Length(fSavePoints);
    SetLength(fSavePoints, idx + 1);
    fSavePoints[idx] := fIndex;
  end;




end.


