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
    Deltics.Classes,
    Deltics.Memento,
    Deltics.Strings,
    Deltics.Unicode;


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
      procedure ResetBuffer;
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
      constructor Create; overload;
      constructor Create(const aBase: Pointer; const aBytes: Int64); overload;
      procedure Overlay(const aBase: Pointer; const aBytes: Int64); overload;
      function Write(const Buffer; Count: Longint): Longint; override;
    end;


    IStreamPositionMemento = interface(IMemento)
    ['{33F5550E-47E1-492B-8CCE-9254F7389FB4}']
      function get_Position: Int64;
      procedure set_Position(const aValue: Int64);
      property Position: Int64 read get_Position write set_Position;
    end;


  function StreamPositionMemento(const aStream: TStream): IStreamPositionMemento;




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
  procedure TBufferedStreamReader.ResetBuffer;
  begin
    fBufCurr  := NIL;
    fBufEnd   := NIL;
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
  var
    oldPos: Int64;
  begin
    if (aOffset = 0) and (aOrigin = soCurrent) then
      result := fStream.Position - (Integer(fBufEnd) - Integer(fBufCurr))
    else
    begin
      oldPos := fStream.Position;
      fStream.Seek(aOffset, aOrigin);

      if fStream.Position <> oldPos then
        Resetbuffer;
    end;
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

  constructor TReadMemoryStream.Create;
  begin
    inherited Create;
  end;


  constructor TReadMemoryStream.Create(const aBase: Pointer; const aBytes: Int64);
  begin
    inherited Create;
    SetPointer(aBase, aBytes);
  end;


  procedure TReadMemoryStream.Overlay(const aBase: Pointer; const aBytes: Int64);
  begin
    SetPointer(aBase, aBytes);
  end;

  function TReadMemoryStream.Write(const Buffer; Count: Integer): Longint;
  begin
    raise Exception.Create('Cannot write to a TReadMemoryStream');
  end;







end.
