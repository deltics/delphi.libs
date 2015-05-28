
  unit Deltics.Unicode;

interface

  uses
    Classes,
    SysUtils,
    Deltics.Strings;


  type
    IUnicodeReader = interface
    ['{EC71F82C-C696-4421-96AD-9B6D1318AFFD}']
      function get_EOF: Boolean;
      function get_Encoding: TEncoding;
      function ReadChar(var aChar: WideChar): Boolean;

      property Encoding: TEncoding read get_Encoding;
      property EOF: Boolean read get_EOF;
    end;


    TUnicodeReader = class
      class function AsOwnerOfWideChars(const aBuffer: PWideChar; const aBufferLength: Integer): IUnicodeReader; overload;
      class function AsOwnerOfStream(const aStream: TStream; const aEncoding: TEncoding; const aBufferKB: Word = 4): IUnicodeReader; overload;

      class function OfStream(const aStream: TStream; const aEncoding: TEncoding; const aBufferKB: Word = 4): IUnicodeReader; overload;
      class function OfANSI(const aString: ANSIString): IUnicodeReader;
      class function OfWIDE(const aString: UnicodeString): IUnicodeReader;
      class function OfUTF8(const aString: UTF8String): IUnicodeReader;
      class function OfWideChars(const aBuffer: PWideChar; const aBufferLength: Integer): IUnicodeReader; overload;
    end;


  const
    BOM_UTF8      : array[0..2] of Byte = ($EF, $BB, $BF);
    BOM_UTF16BE   : array[0..1] of Byte = ($FE, $FF);
    BOM_UTF16LE   : array[0..1] of Byte = ($FF, $FE);
    BOM_UTF32BE   : array[0..3] of Byte = ($00, $00, $FE, $FF);
    BOM_UTF32LE   : array[0..3] of Byte = ($FF, $FE, $00, $00);



implementation

  uses
    Windows,
    Deltics.Classes,
    Deltics.Readers,
    Deltics.Strings.Encoding,
    Deltics.SysUtils;




  function IdentifyEncoding(const aBOM: TBOM;
                            var aEncoding: TEncoding): Boolean;
  var
    default: TEncoding;

    function HasBOM(const aTestEncoding: TEncoding): Boolean;
    var
      encBOM: TBOM;
    begin
      encBOM := TEncodingImplementation(aTestEncoding).BOM;

      if (Length(encBOM) < Length(aBOM)) then
        result := FALSE
      else
        result := CompareMem(@aBOM, @encBOM, Length(aBOM));

      if result then
        aEncoding := aTestEncoding;
    end;

  begin
    default   := TEncoding(aEncoding);
    result    := FALSE;

    // If the stream is too small to contain a BOM (smallest possible
    //  encoding header is 2 bytes) we cannot do any detection

    if (Length(aBOM) < 2) then
      EXIT;

    result := TEncoding.Identify(aBOM, aEncoding);

    if NOT result then
      aEncoding := default;
  end;


  type
    TUnicodeMemoryReader = class
    public
      class function Create(const aAddress: Pointer; const aLength: Integer; const aDefaultEncoding: TEncoding): IUnicodeReader; overload;
    end;


    TUnicodeStringReader = class(TCOMInterfacedObject, IUnicodeReader)
    private
      fPos: Integer;
      fString: UnicodeString;
    public
      constructor Create(const aString: ANSIString); overload;
      constructor Create(const aString: UnicodeString); overload;

    private // IUnicodeReader
      function get_Encoding: TEncoding;
      function get_EOF: Boolean;
    public
      function ReadChar(var aChar: WIDEChar): Boolean;
      property EOF: Boolean read get_EOF;
    end;


    TUnicodeStreamReader = class(TCOMInterfacedObject, IUnicodeReader)
    private
      fBuffer: PWideChar;
      fBufferLength: Integer;
      fBufferPos: Integer;
      fBufferSize: Integer;
      fEncoding: TEncodingImplementation;
      fEOF: Boolean;
      fOwnsStream: Boolean;
      fStream: TStream;
      procedure DetectEncoding(const aStream: TStream; const aDefault: TEncoding);
      procedure FillBuffer;
    public
      constructor Create(const aStream: TStream;
                         const aEncoding: TEncoding;
                         const aBufferKB: Word); overload;
      constructor CreateAsOwnerOf(const aStream: TStream;
                                  const aEncoding: TEncoding;
                                  const aBufferKB: Word); overload;
      destructor Destroy; override;

    private // IUnicodeReader
      function get_Encoding: TEncoding;
      function get_EOF: Boolean;
    public
      function ReadChar(var aChar: WideChar): Boolean;
      property Encoding: TEncoding read get_Encoding;
      property EOF: Boolean read get_EOF;
    end;


    TUnicodeWideCharReader = class(TCOMInterfacedObject, IUnicodeReader)
    private
      fBuffer: PWideChar;
      fBufferLength: Integer;
      fBufferPos: Integer;
      fOwnsBuffer: Boolean;
    public
      constructor Create(const aBuffer: PWideChar; const aBufferLength: Integer);
      constructor CreateAsOwnerOf(const aBuffer: PWideChar; const aBufferLength: Integer);

    private // IUnicodeReader
      function get_Encoding: TEncoding;
      function get_EOF: Boolean;
    public
      function ReadChar(var aChar: WideChar): Boolean;
      property EOF: Boolean read get_EOF;
    end;




{ TUnicodeStringReader }

  constructor TUnicodeStringReader.Create(const aString: ANSIString);
  begin
    inherited Create;

    fString   := ANSI(aString).ToWIDE;
    fPos      := 0;
  end;


  constructor TUnicodeStringReader.Create(const aString: UnicodeString);
  begin
    inherited Create;

    fString   := aString;
    fPos      := 0;
  end;


  function TUnicodeStringReader.ReadChar(var aChar: WIDEChar): Boolean;
  begin
    Inc(fPos);
    result := fPos <= Length(fString);

    if result then
      aChar := fString[fPos];
  end;


  function TUnicodeStringReader.get_Encoding: TEncoding;
  begin
    result := TEncoding.UTF16;
  end;


  function TUnicodeStringReader.get_EOF: Boolean;
  begin
    result := (fPos = Length(fString));
  end;











{ TUnicodeReader }
  class function TUnicodeReader.OfStream(const aStream: TStream;
                                         const aEncoding: TEncoding;
                                         const aBufferKB: Word): IUnicodeReader;
  begin
    result := TUnicodeStreamReader.Create(aStream, aEncoding, aBufferKB);
  end;


  class function TUnicodeReader.AsOwnerOfStream(const aStream: TStream;
                                                const aEncoding: TEncoding;
                                                const aBufferKB: Word): IUnicodeReader;
  begin
    result := TUnicodeStreamReader.CreateAsOwnerOf(aStream, aEncoding, aBufferKB);
  end;


  class function TUnicodeReader.AsOwnerOfWideChars(const aBuffer: PWideChar;
                                                   const aBufferLength: Integer): IUnicodeReader;
  begin
    result := TUnicodeWideCharReader.CreateAsOwnerOf(aBuffer, aBufferLength);
  end;


  class function TUnicodeReader.OfANSI(const aString: ANSIString): IUnicodeReader;
  begin
    result := TUnicodeStringReader.Create(aString);
  end;


  class function TUnicodeReader.OfUTF8(const aString: UTF8String): IUnicodeReader;
  begin
    // TODO: Implement memory reader

    // Temporarily simply convert to WIDE and use Unicode string reader

    result := TUnicodeStringReader.Create(WIDE.FromUTF8(aString));
  end;


  class function TUnicodeReader.OfWIDE(const aString: UnicodeString): IUnicodeReader;
  begin
    result := OfWideChars(@aString[1], Length(aString));
  end;


  class function TUnicodeReader.OfWideChars(const aBuffer: PWideChar;
                                            const aBufferLength: Integer): IUnicodeReader;
  begin
    result := TUnicodeWideCharReader.Create(aBuffer, aBufferLength);
  end;






{ TUnicodeWideCharReader }

  constructor TUnicodeWideCharReader.Create(const aBuffer: PWideChar;
                                            const aBufferLength: Integer);
  begin
    inherited Create;

    fBuffer       := aBuffer;
    fBufferLength := aBufferLength;
  end;


  constructor TUnicodeWideCharReader.CreateAsOwnerOf(const aBuffer: PWideChar;
                                                     const aBufferLength: Integer);
  begin
    Create(aBuffer, aBufferLength);

    fOwnsBuffer := TRUE;
  end;


  function TUnicodeWideCharReader.get_Encoding: TEncoding;
  begin
    result := TEncoding.UTF16;
  end;


  function TUnicodeWideCharReader.get_EOF: Boolean;
  begin
    result := (fBufferPos = fBufferLength);
  end;


  function TUnicodeWideCharReader.ReadChar(var aChar: WideChar): Boolean;
  begin
    result := (fBufferPos < fBufferLength);

    if result then
    begin
      aChar := fBuffer[fBufferPos];
      Inc(fBufferPos);
    end;
  end;






  type
    TEncodingHelper = class(TEncoding);




  constructor TUnicodeStreamReader.Create(const aStream: TStream;
                                          const aEncoding: TEncoding;
                                          const aBufferKB: Word);
  begin
    DetectEncoding(aStream, aEncoding);

    inherited Create;

    fStream     := aStream;
    fBufferSize := aBufferKB * 1024;
  end;


  constructor TUnicodeStreamReader.CreateAsOwnerOf(const aStream: TStream;
                                                   const aEncoding: TEncoding;
                                                   const aBufferKB: Word);
  begin
    Create(aStream, aEncoding, aBufferKB);

    fOwnsStream := TRUE;
  end;


  destructor TUnicodeStreamReader.Destroy;
  begin
    if Assigned(fBuffer) then
    begin
      FreeMem(fBuffer);
      fBuffer := NIL;
    end;

    if fOwnsStream then
    begin
      fStream.Free;
      fStream := NIL;
    end;

    inherited;
  end;


  procedure TUnicodeStreamReader.DetectEncoding(const aStream: TStream;
                                                const aDefault: TEncoding);
  begin
    fEncoding := aDefault as TEncodingImplementation;

    TEncoding.Identify(aStream, TEncoding(fEncoding));
  end;


  procedure TUnicodeStreamReader.FillBuffer;
  var
    src: array of Byte;
    bytesRead: Integer;
    charBytes: Integer;
    extraBytes: Integer;
  begin
    SetLength(src, fBufferSize);

    bytesRead := fStream.Read(src[0], fBufferSize);
    if (bytesRead < fBufferSize) then
      SetLength(src, bytesRead);

    fBufferLength := fEncoding.GetCharCount(@src[0], bytesRead);
    ReallocMem(fBuffer, fBufferLength * 2);

    fEncoding.Decode(@src[0], bytesRead, fBuffer, fBufferLength);
    fBufferPos := 0;

    // Calculate any extra bytes read, being any bytes that were read which were
    //  not part of the converted buffer:
    //
    //    bytes read     :   [............................................]
    //    buffer chars   :   [-----------------------------------------]
    //    extra bytes    :                                             ^^^
    //
    // If this is non-zero then we need to rewind the stream by that number of bytes

    charBytes := fEncoding.GetByteCount(fBuffer, fBufferLength);

    extraBytes := bytesRead - charBytes;
    if extraBytes > 0 then
      fStream.Seek(-extraBytes, soCurrent);

    // Set our EOF marker if we have exhausted the stream

    fEOF := (fStream.Position = fStream.Size);
  end;


  function TUnicodeStreamReader.get_Encoding: TEncoding;
  begin
    result := fEncoding;
  end;


  function TUnicodeStreamReader.get_EOF: Boolean;
  begin
    // Only EOF if the stream is exhausted AND we have reached the
    //  end of the buffer

    result := fEOF and (fBufferPos = fBufferLength);
  end;


  function TUnicodeStreamReader.ReadChar(var aChar: WideChar): Boolean;
  begin
    if (fBufferPos = fBufferLength) then
      FillBuffer;

    result := (fBufferPos < fBufferLength);

    if result then
    begin
      aChar := fBuffer[fBufferPos];
      Inc(fBufferPos);
    end;
  end;






{ TUnicodeMemoryReader }

  class function TUnicodeMemoryReader.Create(const aAddress: Pointer;
                                             const aLength: Integer;
                                             const aDefaultEncoding: TEncoding): IUnicodeReader;
  begin
    raise ENotImplemented.Create(TUnicodeMemoryReader, 'Create');
  end;

end.
