{
  * X11 (MIT) LICENSE *

  Copyright © 2008 Jolyon Smith

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

{$i deltics.rtl.inc}

{$ifdef deltics_streams}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Streams;


interface

  uses
  { vcl: }
    Classes,
    SysUtils,
  { deltics: }
    Deltics.Memento,
    Deltics.Strings;


  type
    TStreamDecorator = class(TStream)
    private
      fOwnsStream: Boolean;
      fStream: TStream;
      function get_EOF: Boolean;
    protected
      function GetSize: Int64; override;
      function GetStream: TStream; virtual;
      procedure SetSize(const aValue: Int64); override;
      procedure AcquireStream(const aStream: TStream; const aIsOwn: Boolean); virtual;
      procedure ReleaseStream; virtual;
      property EOF: Boolean read get_EOF;
      property Stream: TStream read GetStream;
    public
      constructor Create(const aStream: TStream);
      destructor Destroy; override;
      function Read(var aBuffer; aCount: Integer): Integer; override;
      function Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64; override;
      function Write(const aBuffer; aCount: Integer): Integer; override;
      property Size: Int64 read GetSize write SetSize;
    end;


  {$ifNdef DELPHI2009_OR_LATER}
    TStringStream = class(Classes.TStringStream)
    public
      constructor Create; reintroduce; overload;
      procedure SaveToFile(const aFilename: String);
    end;
  {$endif}


  type
    TBufferedStream = class;
    TBufferedStreamReader = class;
    TBufferedStreamWriter = class;

    EBufferedStream = class(Exception);


    IBufferedStream = interface
    ['{E7B89208-5C2C-4330-B12C-19540F05A5A8}']
    end;


    IBufferedStreamReader = interface(IBufferedStream)
    ['{8E65DAC5-24DF-4D1C-B070-58F0C459952B}']
      function get_EOF: Boolean;
      function Read(var aBuffer; aCount: Integer): Integer;
      property EOF: Boolean read get_EOF;
    end;


    IBufferedStreamWriter = interface(IBufferedStream)
    ['{8B92CCF9-F963-4512-AAEB-B2A7DD9EE8B8}']
      function Write(const aBuffer; aCount: Integer): Integer;
    end;


    TBufferedStream = class(TStreamDecorator, IUnknown)
    private
      fRefCount: Integer;
      function QueryInterface(const aIID: TGUID; out aObj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    protected
      fBuffer: PByte;
      fBufCurr: PByte;
      fBufSize: Integer;
    protected
      function GetSize: Int64; override;
      procedure SetSize(const aValue: Int64); override;
      constructor Create(const aStream: TStream; const aBufSize: Integer);
    public
      class function CreateReader(const aStream: TStream;
                                  const aBufSize: Integer = 4096): IBufferedStreamReader;
      class function CreateWriter(const aStream: TStream;
                                  const aBufSize: Integer = 4096): IBufferedStreamWriter;
      destructor Destroy; override;
      function Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64; override;
    end;


    TBufferedStreamReader = class(TBufferedStream, IBufferedStreamReader)
    private
      fBufEnd: PByte;
      function FillBuffer: Integer; {$if CompilerVersion > 18} inline; {$ifend}
    protected
      function get_EOF: Boolean;
      procedure AcquireStream(const aStream: TStream; const aIsOwn: Boolean); override;
    public
      constructor Create(const aStream: TStream; const aBufSize: Integer);
      function Read(var aBuffer; aCount: Integer): Integer; override;
      function Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64; override;
      function Write(const aBuffer; aCount: Integer): Integer; override;
    end;


    TBufferedStreamWriter = class(TBufferedStream, IBufferedStreamWriter)
    protected
      procedure AcquireStream(const aStream: TStream; const aIsOwn: Boolean); override;
      function GetSize: Int64; override;
    public
      constructor Create(const aStream: TStream; const aBufSize: Integer);
      procedure Flush; {$if CompilerVersion > 18} inline; {$ifend}
      function Read(var aBuffer; aCount: Integer): Integer; override;
      function Write(const aBuffer; aCount: Integer): Integer; override;
    end;


    TReadMemoryStream = class(TCustomMemoryStream)
    public
      constructor Create(const aBase: Pointer;
                         const aBytes: Int64);
      function Write(const Buffer; Count: Longint): Longint; override;
    end;


    IStreamPositionMemento = interface(IMemento)
    ['{33F5550E-47E1-492B-8CCE-9254F7389FB4}']
      function get_Position: Int64;
      procedure set_Position(const aValue: Int64);
      property Position: Int64 read get_Position write set_Position;
    end;


  function StreamPositionMemento(const aStream: TStream): IStreamPositionMemento;


  type
    ECharEncoding = class(Exception);

    TCharEncoding = (
                     ceASCII,
                     ceANSI,
                     ceUTF8,
                     ceUTF16,
                     ceUTF16LE,
                     ceUTF16BE,
                     ceUTF32,
                     ceUTF32BE,
                     ceUTF32LE
                    );
    TCharEncodings = set of TCharEncoding;


    TEncodingEnforcement = (
                            encConfirm,
                            encOverride
                           );


    TUnicodeStream = class(TStreamDecorator)
    private
      fBOMPresent: Boolean;
      fEncoding: TCharEncoding;
      fCodepoint: Cardinal;
      fNeedCodepoint: Boolean;
      constructor Create(const aBuffer; const aBufferSize: Integer); overload;
    protected
      procedure Initialise(const aDefaultEncoding: TCharEncoding);
    public
      constructor Create(const aEncoding: TCharEncoding = ceUTF8); overload;
      constructor Create(const aStream: TStream;
                         const aDefaultEncoding: TCharEncoding = ceUTF8); overload;
      constructor Create(const aString: String); overload;
      constructor Create(const aString: WideString); overload;
      constructor CreateUTF8(const aString: UTF8String);
      procedure CheckEncoding(const aEncoding: TCharEncoding);
      function DetectEncoding(const aDefault: TCharEncoding): TCharEncoding;
      procedure LoadFromFile(const aFileName: String;
                             const aDefaultEncoding: TCharEncoding = ceUTF8);
      function ReadChar(var aChar: ANSIChar): Boolean; overload;
      function ReadChar(var aChar: WideChar): Boolean; overload;
      function ReadLine: String;
      function ReadString: String; overload;
      function ReadString(const aMaxChars: Integer): String; overload;
      procedure Reset;
      property BOMPresent: Boolean read fBOMPresent;
      property Encoding: TCharEncoding read fEncoding write fEncoding;
      property EOF;
    end;


    function CharLower(const aChar: WideChar): WideChar;
    function CharUpper(const aChar: WideChar): WideChar;
    function IsCharLower(const aChar: ANSIChar): Boolean; overload;
    function IsCharLower(const aChar: WideChar): Boolean; overload;
    function IsCharUpper(const aChar: ANSIChar): Boolean; overload;
    function IsCharUpper(const aChar: WideChar): Boolean; overload;


    function ReadCodePoint(const aStream: TStream;
                           var aCodePoint: Cardinal;
                           const aEncoding: TCharEncoding): Boolean; {$if CompilerVersion > 18} inline; {$ifend}





implementation

  uses
  { vcl: }
  {$ifdef DELPHI2009_OR_LATER}
    Character,
  {$endif}
    Math,
    TypInfo,
    Windows,
  { deltics: }
    Deltics.SysUtils;



  procedure ByteMove(const Source; var Dest; Count: Integer);
{$ifdef WIN64}
  begin
    CopyMemory(@Dest, @Source, Count);
  end;
{$else}
  asm
                      // ECX = Count
                      // EAX = Const Source
                      // EDX = Var Dest
                      // If there are no bytes to copy, just quit
                      // altogether; there's no point pushing registers.
    Cmp   ECX,0
    Je    @JustQuit
                      // Preserve the critical Delphi registers.
    push  ESI
    push  EDI
                      // Move Source into ESI (SOURCE register).
                      // Move Dest into EDI (DEST register).
                      // This might not actually be necessary, as I'm not using MOVsb etc.
                      // I might be able to just use EAX and EDX;
                      // there could be a penalty for not using ESI, EDI, but I doubt it.
                      // This is another thing worth trying!
    Mov   ESI, EAX
    Mov   EDI, EDX
                      // The following loop is the same as repNZ MovSB, but oddly quicker!
  @Loop:
    Mov   AL, [ESI]   // get a source byte
    Inc   ESI         // bump source address
    Mov   [EDI], AL   // Put it into the destination
    Inc   EDI         // bump destination address
    Dec   ECX         // Dec ECX to note how many we have left to copy
    Jnz   @Loop       // If ECX <> 0, then loop.
                      // Pop the critical Delphi registers that we've altered.
    pop   EDI
    pop   ESI
  @JustQuit:
  end;
{$endif}



{$ifNdef DELPHI2009_OR_LATER}
  constructor TStringStream.Create;
  begin
    inherited Create('');
  end;


  procedure TStringStream.SaveToFile(const aFilename: String);
  var
    s: String;
    strm: TStream;
  begin
    strm := TFileStream.Create(aFileName, fmCreate);
    try
      s := DataString;
      strm.Write(s[1], Length(s) * SizeOf(Char));

    finally
      strm.Free;
    end;
  end;
{$endif}



{ TStreamDecorator ------------------------------------------------------------------------------- }

  constructor TStreamDecorator.Create(const aStream: TStream);
  begin
    inherited Create;

    AcquireStream(aStream, FALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TStreamDecorator.Destroy;
  begin
    ReleaseStream;

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStreamDecorator.get_EOF: Boolean;
  begin
    result := (Position = Size);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TStreamDecorator.AcquireStream(const aStream: TStream; const aIsOwn: Boolean);
  begin
    if Assigned(fStream) then
      ReleaseStream;

    fStream := aStream;

    fOwnsStream := aIsOwn and Assigned(fStream);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStreamDecorator.GetSize: Int64;
  begin
    result := Stream.Size;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStreamDecorator.GetStream: TStream;
  begin
    result := fStream;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStreamDecorator.Read(var aBuffer; aCount: Integer): Integer;
  begin
    result := Stream.Read(aBuffer, aCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TStreamDecorator.ReleaseStream;
  begin
    if fOwnsStream then
      fStream.Free;

    fStream := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStreamDecorator.Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64;
  begin
    result := Stream.Seek(aOffset, aOrigin);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TStreamDecorator.SetSize(const aValue: Int64);
  begin
    Stream.Size:= aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStreamDecorator.Write(const aBuffer; aCount: Integer): Integer;
  begin
    result := Stream.Write(aBuffer, aCount);
  end;








{ TBufferedStream -------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TBufferedStream.CreateReader(const aStream: TStream;
                                              const aBufSize: Integer): IBufferedStreamReader;
  begin
    result := TBufferedStreamReader.Create(aStream, aBufSize) as IBufferedStreamReader;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TBufferedStream.CreateWriter(const aStream: TStream;
                                              const aBufSize: Integer): IBufferedStreamWriter;
  begin
    result := TBufferedStreamWriter.Create(aStream, aBufSize) as IBufferedStreamWriter;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TBufferedStream.Create(const aStream: TStream;
                                     const aBufSize: Integer);
  begin
    GetMem(fBuffer, aBufSize);
    fBufSize := aBufSize;

    inherited Create(aStream);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TBufferedStream.Destroy;
  begin
    FreeMem(fBuffer);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStream.QueryInterface(const aIID: TGUID; out aObj): HResult;
  begin
    if GetInterface(aIID, aObj) then
      result := 0
    else
      result := E_NOINTERFACE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStream._AddRef: Integer;
  begin
    result := InterlockedIncrement(fRefCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStream._Release: Integer;
  begin
    result := InterlockedDecrement(fRefCount);

    if (fRefCount = 0) then
      Free;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStream.GetSize: Int64;
  begin
    result := fStream.Size;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStream.Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64;
  resourcestring
    rsfSeekNotSupported = 'Seek operations are not supported by %s';
  begin
    raise EBufferedStream.CreateFmt(rsfSeekNotSupported, [ClassName]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TBufferedStream.SetSize(const aValue: Int64);
  begin
    fStream.Size := aValue;
  end;








{ TBufferedStreamReader -------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TBufferedStreamReader.AcquireStream(const aStream: TStream;
                                                const aIsOwn: Boolean);
  begin
    inherited;
    FillBuffer;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TBufferedStreamReader.Create(const aStream: TStream; const aBufSize: Integer);
  begin
    inherited;
  end;


  function TBufferedStreamReader.FillBuffer: Integer;
  begin
    result := fStream.Read(fBuffer^, fBufSize);

    fBufEnd   := PByte(Integer(fBuffer) + result);
    fBufCurr  := fBuffer;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStreamReader.get_EOF: Boolean;
  begin
    result := (fBufCurr = fBufEnd) and (fStream.Position = fStream.Size);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStreamReader.Read(var aBuffer; aCount: Integer): Integer;
  var
    bytesLeft: Integer;
  begin
    result := 0;

    if (fBufCurr = fBufEnd) then
      if (FillBuffer = 0) then
        EXIT;

    bytesLeft := Math.Min(aCount, Integer(fBufEnd) - Integer(fBufCurr));

    while (aCount > 0) do
    begin
      ByteMove(fBufCurr^, aBuffer, bytesLeft);
      Inc(fBufCurr, bytesLeft);
      Inc(result, bytesLeft);
      Dec(aCount, bytesLeft);

      if (aCount > 0) then
        bytesLeft := FillBuffer;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStreamReader.Seek(const aOffset: Int64;
                                            aOrigin: TSeekOrigin): Int64;
  begin
    if (aOffset = 0) and (aOrigin = soCurrent) then
      result := fStream.Position - (Integer(fBufEnd) - Integer(fBufCurr))
    else
      result := inherited Seek(aOffset, aOrigin);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStreamReader.Write(const aBuffer; aCount: Integer): Integer;
  resourcestring
    rsfWriteNotSupported = 'Write operations are not supported by %s';
  begin
    raise EBufferedStream.CreateFmt(rsfWriteNotSupported, [ClassName]);
  end;








{ TBufferedStreamWriter -------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TBufferedStreamWriter.AcquireStream(const aStream: TStream;
                                                const aIsOwn: Boolean);
  begin
    inherited;
    fBufCurr := fBuffer;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TBufferedStreamWriter.Create(const aStream: TStream; const aBufSize: Integer);
  begin
    inherited;
  end;


  procedure TBufferedStreamWriter.Flush;
  begin
    if (fBufCurr = fBuffer) then
      EXIT;

    fStream.Write(fBuffer^, Integer(fBufCurr) - Integer(fBuffer));
    fBufCurr := fBuffer;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStreamWriter.GetSize: Int64;
  begin
    result := fStream.Size;
    if (fStream.Position = fStream.Size) then
      Inc(result, Integer(fBufCurr) - Integer(fBuffer));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStreamWriter.Read(var aBuffer; aCount: Integer): Integer;
  resourcestring
    rsfReadNotSupported = 'Read operations are not supported by %s';
  begin
    raise EBufferedStream.CreateFmt(rsfReadNotSupported, [ClassName]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBufferedStreamWriter.Write(const aBuffer; aCount: Integer): Integer;
  begin
    if (aCount >= fBufSize) then
    begin
      Flush;
      result := fStream.Write(aBuffer, aCount);
      EXIT;
    end;

    if (aCount >= (fBufSize - (Integer(fBufCurr) - Integer(fBuffer)))) then
      Flush;

    ByteMove(aBuffer, fBufCurr^, aCount);
    Inc(fBufCurr, aCount);
    result := aCount;
  end;







{ TStreamMemento --------------------------------------------------------------------------------- }

  type
    TStreamPositionMemento = class(TMemento, IStreamPositionMemento)
    private
      fStream: TStream;
      fPosition: Int64;
    protected
      constructor Create(const aStream: TStream);
      procedure DoRecall; override;
    { IStreamMemento - - - - - - - - - - - - - - - - - - - - }
    private
      function get_Position: Int64;
      procedure set_Position(const aValue: Int64);
    end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TStreamPositionMemento.Create(const aStream: TStream);
  begin
    inherited Create;

    fStream   := aStream;
    fPosition := fStream.Position;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStreamPositionMemento.get_Position: Int64;
  begin
    result := fPosition;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TStreamPositionMemento.set_Position(const aValue: Int64);
  begin
    fPosition := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TStreamPositionMemento.DoRecall;
  begin
    fStream.Position := fPosition;
  end;



{ StreamPositionMemento factory ------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function StreamPositionMemento(const aStream: TStream): IStreamPositionMemento;
  begin
    result := TStreamPositionMemento.Create(aStream);
  end;







{ TReadMemoryStream }

  constructor TReadMemoryStream.Create(const aBase: Pointer; const aBytes: Int64);
  begin
    inherited Create;
    SetPointer(aBase, aBytes);
  end;


  function TReadMemoryStream.Write(const Buffer; Count: Integer): Longint;
  begin
    raise Exception.Create('Cannot write to a TReadMemoryStream');
  end;







  const
    BOM_UTF8      : array[0..2] of Byte = ($EF, $BB, $BF);
    BOM_UTF16BE   : array[0..1] of Byte = ($FE, $FF);
    BOM_UTF16LE   : array[0..1] of Byte = ($FF, $FE);
    BOM_UTF32BE   : array[0..3] of Byte = ($00, $00, $FE, $FF);
    BOM_UTF32LE   : array[0..3] of Byte = ($FF, $FE, $00, $00);



  function CharLower(const aChar: WideChar): WideChar;
  var
    wc: array[0..1] of WideChar;
  begin
    wc[0] := aChar;
    wc[1] := WideChar(0);

    result := Windows.CharLowerW(@wc[0])^;
  end;


  function CharUpper(const aChar: WideChar): WideChar;
  var
    wc: array[0..1] of WideChar;
  begin
    wc[0] := aChar;
    wc[1] := WideChar(0);

    result := Windows.CharUpperW(@wc[0])^;
  end;


  function IsCharLower(const aChar: ANSIChar): Boolean;
  begin
    result := Windows.IsCharLowerA(aChar);
  end;


  function IsCharLower(const aChar: WideChar): Boolean;
  begin
    result := Windows.IsCharLowerW(aChar);
  end;


  function IsCharUpper(const aChar: ANSIChar): Boolean;
  begin
    result := Windows.IsCharUpperA(aChar);
  end;


  function IsCharUpper(const aChar: WideChar): Boolean;
  begin
    result := Windows.IsCharUpperW(aChar);
  end;







{ TUnicodeStream --------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TUnicodeStream.Create(const aEncoding: TCharEncoding);
  begin
    inherited Create(NIL);
    AcquireStream(TMemoryStream.Create, TRUE);
    fEncoding := aEncoding;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TUnicodeStream.Create(const aStream: TStream;
                                    const aDefaultEncoding: TCharEncoding);
  begin
    inherited Create(aStream);
    Initialise(aDefaultEncoding);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TUnicodeStream.Create(const aBuffer;
                                    const aBufferSize: Integer);
  var
    bytes: TMemoryStream;
  begin
    inherited Create(NIL);

    bytes := TMemoryStream.Create;
    bytes.Write(aBuffer, aBufferSize);
    bytes.Position := 0;

    AcquireStream(bytes, TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TUnicodeStream.Create(const aString: String);
  var
    size: Integer;
    encoding: TCharEncoding;
  begin
  {$ifdef UNICODE}
    size := Length(aString) * StringElementSize(aString);
  {$else}
    size := Length(aString);
  {$endif}

    Create(aString[1], size);

  {$ifdef UNICODE}
    case StringCodePage(aString) of
      CP_UTF7   : encoding := ceUTF8; // Not really, but does it matter ?
      CP_UTF8   : encoding := ceUTF8;
      1200      : encoding := ceUTF16LE;
      1201      : encoding := ceUTF16BE;
      12000     : encoding := ceUTF32LE;
      12001     : encoding := ceUTF32BE;
    else
      encoding := ceANSI;
    end;
  {$else}
    encoding := ceANSI;
  {$endif}

    Initialise(encoding);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TUnicodeStream.Create(const aString: WideString);
  begin
    Create(aString[1], Length(aString) * 2);
    Initialise(ceUTF16);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TUnicodeStream.CreateUTF8(const aString: UTF8String);
  begin
    Create(aString[1], Length(aString));
    Initialise(ceUTF8);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnicodeStream.LoadFromFile(const aFileName: String;
                                        const aDefaultEncoding: TCharEncoding);
  begin
    AcquireStream(TMemoryStream.Create, TRUE);
    TMemoryStream(Stream).LoadFromFile(aFileName);
    Initialise(aDefaultEncoding);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnicodeStream.CheckEncoding(const aEncoding: TCharEncoding);
  begin
    if (Encoding = aEncoding) then
      EXIT;

    case aEncoding of
      ceASCII,
      ceANSI  : if (Encoding = ceUTF8) and NOT BOMPresent then
                begin
                  fEncoding := aEncoding;
                  EXIT;
                end;

      ceUTF16 : if (Encoding in [ceUTF16BE, ceUTF16LE]) then
                  EXIT;
      ceUTF32 : if (Encoding in [ceUTF32BE, ceUTF32LE]) then
                  EXIT;
    end;

    raise ECharEncoding.Create('Unexpected file encoding (' + GetEnumName(TypeInfo(TCharEncoding), Ord(Encoding)) + ')');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TUnicodeStream.DetectEncoding(const aDefault: TCharEncoding): TCharEncoding;
  var
    bom: array[0..3] of Byte;
    oldPos: IStreamPositionMemento;
    bytesRead: Integer;
    detected: TCharEncoding;

    function TestBOM(const aBOM: array of Byte;
                     const aEncoding: TCharEncoding): Boolean;
    begin
      if (bytesRead < Length(aBOM)) then
        result := FALSE
      else
        result := CompareMem(@bom, @aBOM, Length(aBOM));

      if result then
      begin
        detected := aEncoding;
        oldPos.Position := oldPos.Position + Length(aBOM);

        fBOMPresent := TRUE;
      end;
    end;

  begin
    result      := fEncoding;
    detected    := ceANSI;
    fBOMPresent := FALSE;

    // If the stream is too small to contain a BOM (smallest possible
    //  encoding header is 2 bytes) we cannot do any detection

    if ((Size - Position) < 2) then
      EXIT;

    // ... with the longest possible being 4 bytes
    oldPos    := StreamPositionMemento(self);
    bytesRead := Read(bom, 4);
    if NOT TestBOM(BOM_UTF32BE, ceUTF32BE) then
      if NOT TestBOM(BOM_UTF32LE, ceUTF32LE) then
        if NOT TestBOM(BOM_UTF16BE, ceUTF16BE) then
          if NOT TestBOM(BOM_UTF16LE, ceUTF16LE) then
            if NOT TestBOM(BOM_UTF8, ceUTF8) then
              oldPos.Recall;

    if (detected = ceANSI) then
    begin
      if (bytesRead >= 2) and (bom[1] = 0) then
        detected := ceUTF16LE
      else
        detected := aDefault;
    end;

    result := detected;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnicodeStream.Initialise(const aDefaultEncoding: TCharEncoding);
  begin
    StreamPositionMemento(self);
    fEncoding       := DetectEncoding(aDefaultEncoding);
    fNeedCodepoint  := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TUnicodeStream.ReadChar(var aChar: ANSIChar): Boolean;
  begin
    case Encoding of
      ceANSI: result := (Stream.Read(aChar, 1) = 1);

    else
      result := FALSE;
      ASSERT(FALSE, 'Not yet implemented');

      {
        TODO:

          if need codepoint then

            ReadCodePoint
            if < $128 then

              return ANSIChar(codepoint)

            else
              if (codepoint >= $10000 <= $10FFFF) then
                transcode surrogate pair, convert to fANSIChars[]
              else
                convert to fANSIChars[]

              set ANSI char index = 1
              set need codepoint = FALSE

              return fANSIChars[1]

          else
            Inc fANSICharIndex
            return fANSIChars[fANSICharIndex]

            if last char then
              set need codepoint = TRUE
      }
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TUnicodeStream.ReadChar(var aChar: WideChar): Boolean;
  begin
    case Encoding of
      ceUTF16LE : result := self.Read(aChar, 2) = 2;

      ceUTF16,
      ceUTF16BE : begin
                    result := self.Read(aChar, 2) = 2;
                    aChar := WideChar(ReverseBytes(Word(aChar)));
                  end;
    else
      if fNeedCodePoint then
      begin
        result  := ReadCodePoint(self, fCodePoint, Encoding);
        if (fCodepoint >= $10000) and (fCodepoint <= $10FFFF) then
        begin
          aChar          := WideChar(((fCodePoint - $10000) div $400) + $D800);
          fNeedCodePoint := FALSE;
        end
        else
          aChar := WideChar(fCodePoint);
      end
      else
      begin
        aChar   := WideChar(((fCodePoint - $10000) mod $400) + $DC00);
        result  := TRUE;
        fNeedCodePoint := TRUE;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TUnicodeStream.ReadLine: String;
  var
    c: Char;
    oldPos: Int64;
  begin
    result := '';

    while ReadChar(c) and (c <> #13) and (c <> #10) do
      result := result + c;

    // If we ended on a carriage return and the next character is a line feed
    //  we skip it (which we will have achieved by simply having read it
    //  already!), but having read it to test it, if it is NOT a line feed then
    //  we have to rewind to where we are right now

    if (c = #13) then
    begin
      oldPos := Stream.Position;
      if ReadChar(c) and (c <> #10) then
        Stream.Position := oldPos;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TUnicodeStream.ReadString: String;
  var
    i: Integer;
    c: Char;
    oldEncoding: TCharEncoding;
  begin
    oldEncoding := Encoding;
    try
      Encoding := DetectEncoding(Encoding);

      SetLength(result, Size - Position);
      i := 1;
      while ReadChar(c) and (c <> #0) do
      begin
        result[i] := c;
        Inc(i);
      end;

      SetLength(result, i - 1);
    finally
      Encoding := oldEncoding;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TUnicodeStream.ReadString(const aMaxChars: Integer): String;
  var
    i: Integer;
    c: Char;
    oldEncoding: TCharEncoding;
  begin
    oldEncoding := Encoding;
    try
      Encoding := DetectEncoding(Encoding);
      result  := '';

      i := aMaxChars;
      while (i > 0) and ReadChar(c) and (c <> #0) do
      begin
        result := result + c;
        Dec(i);
      end;
    finally
      Encoding := oldEncoding;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnicodeStream.Reset;
  begin
    Position := 0;
    Initialise(Encoding);
  end;










{ Library Routines ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function ReadCodePoint(const aStream: TStream;
                         var aCodepoint: Cardinal;
                         const aEncoding: TCharEncoding): Boolean;
  var
    b: array[1..6] of Byte;
    wc: Word;
    surrogate: Word;
  begin
    result := FALSE;

    case aEncoding of
      ceASCII,
      ceANSI    : begin
                    // TODO: handle/support MBCS

                    result      := aStream.Read(b[1], 1) = 1;
                    aCodepoint  := Cardinal(b[1]);
                  end;

      ceUTF8    : begin
                    result := aStream.Read(b[1], 1) = 1;

                    case b[1] of
                        0..127  : aCodepoint := Cardinal(b[1]);

                      192..223  : begin
                                    result      := aStream.Read(b[2], 1) = 1;
                                    aCodepoint  := ((b[1] - 192) * 64)
                                                 +  (b[2] - 128);
                                  end;

                      224..239  : begin
                                    result      := aStream.Read(b[2], 2) = 2;
                                    aCodepoint  := ((b[1] - 224) * 4096)
                                                 + ((b[2] - 128) * 64)
                                                 +  (b[3] - 128);
                                  end;

                      240..247  : begin
                                    result      := aStream.Read(b[2], 3) = 3;
                                    aCodepoint  := ((b[1] - 240) * 262144)
                                                 + ((b[2] - 224) * 4096)
                                                 + ((b[3] - 128) * 64)
                                                 +  (b[4] - 128);
                                  end;

                      248..251  : begin
                                    result      := aStream.Read(b[2], 4) = 4;
                                    aCodepoint  := ((b[1] - 248) * 16777216)
                                                 + ((b[1] - 240) * 262144)
                                                 + ((b[2] - 224) * 4096)
                                                 + ((b[3] - 128) * 64)
                                                 +  (b[4] - 128);
                                  end;

                      252..253  : begin
                                    result      := aStream.Read(b[2], 5) = 5;
                                    aCodepoint  := ((b[1] - 252) * 1073741824)
                                                 + ((b[2] - 248) * 16777216)
                                                 + ((b[3] - 240) * 262144)
                                                 + ((b[4] - 224) * 4096)
                                                 + ((b[5] - 128) * 64)
                                                 +  (b[6] - 128);
                                  end;

                      254..255  : // BOM Characters - should not encounter these in an actual encoding!
                                  raise ECharEncoding.Create('Invalid UTF8 stream');
                    end;
                  end;

      ceUTF16,
      ceUTF16BE : begin
                    // INTEL is Little-Endian so we need to re-order the bytes in each of the
                    //  words as we read them.  WORD ORDER (in UTF16) is not affected by BYTE order

                    result  := aStream.Read(wc, 2) = 2;
                    wc      := WORD(Byte(wc) or (wc shr 8));

                    case wc of
                      $D800..$DFFF  : begin
                                        result      := aStream.Read(surrogate, 2) = 2;
                                        surrogate   := WORD(Byte(surrogate) or (surrogate shr 8));
                                        aCodepoint  := ((wc or $D800) shl 10) + (surrogate or $DC00);
                                      end;
                    else
                      aCodepoint := Cardinal(wc);
                    end;
                  end;

      ceUTF16LE : begin
                    result := aStream.Read(wc, 2) = 2;
                    case wc of
                      $D800..$DFFF  : begin
                                        result      := aStream.Read(surrogate, 2) = 2;
                                        aCodepoint  := ((wc or $D800) shl 10) + (surrogate or $DC00);
                                      end;
                    else
                      aCodepoint := Cardinal(wc);
                    end;
                  end;

      ceUTF32,
      ceUTF32BE : begin
                    // INTEL is Little-Endian so we need to re-order the bytes in the longword

                    result      := aStream.Read(b, 4) = 4;
                    aCodepoint  := Cardinal((b[4] shl 24) + (b[3] shl 16) + (b[2] shl 8) + b[1]);
                  end;

      ceUTF32LE : result := aStream.Read(aCodepoint, 4) = 4;
    end;
  end;




end.
