
  unit Deltics.Strings.Encoding;

  {$i deltics.inc}

interface

  uses
    Classes,
    SysUtils;


  const
    CP_UTF32LE  = 12000;
    CP_UTF32    = 12001;
    CP_UTF16LE  = 1200;
    CP_UTF16    = 1201;
    CP_ASCII    = 20217;
    CP_UTF8     = 65001;

  const
    BOM_UTF8    : array[0..2] of Byte = ($EF, $BB, $BF);
//    BOM_UTF16   : array[0..1] of Byte = ($FE, $FF);
    BOM_UTF16BE : array[0..1] of Byte = ($FE, $FF);
    BOM_UTF16LE : array[0..1] of Byte = ($FF, $FE);
//    BOM_UTF32   : array[0..3] of Byte = ($00, $00, $FE, $FF);
    BOM_UTF32BE : array[0..3] of Byte = ($00, $00, $FE, $FF);
    BOM_UTF32LE : array[0..3] of Byte = ($FF, $FE, $00, $00);

    BOMCHAR_UTF16LE = WIDEChar($FEFF);
    BOMCHAR_UTF16BE = WIDEChar($FFFE);


  type
    TEncoding = class;
    TEncodingImplementation = class;
    TEncodingClass = class of TEncoding;


    TBOM = array of Byte;


    TEncoding = class
    public
      class function ASCII: TEncoding;
      class function UTF8: TEncoding;
      class function UTF16: TEncoding;
      class function UTF16BE: TEncoding;
      class function UTF16LE: TEncoding;
      class function UTF32: TEncoding;
      class function UTF32BE: TEncoding;
      class function UTF32LE: TEncoding;
      class function Identify(const aBOM: TBOM; var aEncoding: TEncoding): Boolean; overload;
      class function Identify(const aStream: TStream; var aEncoding: TEncoding): Boolean; overload;
      class function Identify(const aBytes: PByte; const aCount: Integer; var aEncoding: TEncoding): Boolean; overload;
    end;


    TEncodingImplementation = class(TEncoding)
    private
      fCodePage: Word;
      class function Instance(var aEncoding: TEncoding; const aClass: TEncodingClass): TEncoding; // TODO: Check this:  static;
    protected
      constructor Create; overload; virtual; abstract;
      constructor Create(const aCodePage: Word); overload;
      function get_BOM: TBOM; virtual;
    public
      function GetByteCount(const aChars: PWIDEChar; const aCount: Integer): Integer; overload; virtual; abstract;
      function GetCharCount(const aBytes: PByte; const aCount: Integer): Integer; overload; virtual; abstract;
      function Decode(const aBytes: PByte; const aByteCount: Integer; const aChars: PWIDEChar; const aCharCount: Integer): Integer; overload; virtual; abstract;
      function Encode(const aChars: PWIDEChar; const aCharCount: Integer; const aBytes: PByte; const aByteCount: Integer): Integer; overload; virtual; abstract;
      property BOM: TBOM read get_BOM;
    end;


    TMultiByteEncoding = class(TEncodingImplementation)
    public
      function GetByteCount(const aChars: PWIDEChar; const aCount: Integer): Integer; override;
      function GetCharCount(const aBytes: PByte; const aCount: Integer): Integer; override;
      function Decode(const aBytes: PByte; const aByteCount: Integer; const aChars: PWIDEChar; const aCharCount: Integer): Integer; override;
      function Encode(const aChars: PWIDEChar; const aCharCount: Integer; const aBytes: PByte; const aByteCount: Integer): Integer; override;
    end;




implementation

  uses
    Windows,
    Deltics.Strings.Encoding.ASCII,
    Deltics.Strings.Encoding.UTF8,
    Deltics.Strings.Encoding.UTF16,
    Deltics.Strings.Encoding.UTF32;


  type
    PEncoding = ^TEncoding;

  var
    _ASCII    : TEncoding = NIL;
    _UTF8     : TEncoding = NIL;
    _UTF16    : TEncoding = NIL;
    _UTF16LE  : TEncoding = NIL;
    _UTF32    : TEncoding = NIL;
    _UTF32LE  : TEncoding = NIL;


  function InterlockedCompareExchangePointer(var Destination; const Exchange, Comperand: Pointer): Pointer;
  asm
    xchg ecx, eax
    lock cmpxchg [ecx], edx
  end;


  class function TEncodingImplementation.Instance(var aEncoding: TEncoding;
                                                  const aClass: TEncodingClass): TEncoding;
  var
    enc: TEncoding;
  begin
    if NOT Assigned(aEncoding) then
    begin
      enc   := aClass.Create;
      if InterlockedCompareExchangePointer(aEncoding, enc, NIL) <> NIL then
        enc.Free;
    end;

    result := aEncoding;
  end;


  class function TEncoding.ASCII: TEncoding;
  begin
    result := TEncodingImplementation.Instance(_ASCII, TASCIIEncoding);
  end;

  class function TEncoding.UTF8: TEncoding;
  begin
    result := TEncodingImplementation.Instance(_UTF8, TUTF8Encoding);
  end;

  class function TEncoding.UTF16: TEncoding;
  begin
    result := TEncodingImplementation.Instance(_UTF16, TUTF16BEEncoding);
  end;

  class function TEncoding.UTF16BE: TEncoding;
  begin
    result := TEncodingImplementation.Instance(_UTF16, TUTF16BEEncoding);
  end;

  class function TEncoding.UTF16LE: TEncoding;
  begin
    result := TEncodingImplementation.Instance(_UTF16LE, TUTF16LEEncoding);
  end;

  class function TEncoding.UTF32: TEncoding;
  begin
    result := TEncodingImplementation.Instance(_UTF32, TUTF32BEEncoding);
  end;

  class function TEncoding.UTF32BE: TEncoding;
  begin
    result := TEncodingImplementation.Instance(_UTF32, TUTF32BEEncoding);
  end;

  class function TEncoding.UTF32LE: TEncoding;
  begin
    result := TEncodingImplementation.Instance(_UTF32LE, TUTF32LEEncoding);
  end;


  class function TEncoding.Identify(const aBOM: TBOM;
                                    var aEncoding: TEncoding): Boolean;

    function HasBOM(const aTestEncoding: TEncoding): Boolean;
    var
      encBOM: TBOM;
    begin
      encBOM := TEncodingImplementation(aTestEncoding).BOM;

      if (Length(aBOM) < Length(encBOM)) then
        result := FALSE
      else
        result := CompareMem(@aBOM, @encBOM, Length(encBOM));

      if result then
        aEncoding := aTestEncoding;
    end;

  begin
    result := FALSE;

    // A BOM has a minimum size, so if the BOM passed to us is not
    //  big enough then we know we will not be able to identify any
    //  encoding from it

    if (Length(aBOM) < 2) then
      EXIT;

    result := TRUE;

    if NOT HasBOM(TEncoding.UTF32) then
      if NOT HasBOM(TEncoding.UTF32LE) then
        if NOT HasBOM(TEncoding.UTF16) then
          if NOT HasBOM(TEncoding.UTF16LE) then
            if NOT HasBOM(TEncoding.UTF8) then
            begin
              // We failed to identify a recognisable BOM.  There is one last
              //  heuristic... if the BOM passed was at least two bytes of which
              //  one is non-zero (null), then we can infer a UTF16 encoding with
              //  the position of the zero (null) determining the endianness

              if (Length(aBOM) >= 2) then
              begin
                if (aBOM[0] <> 0) and (aBOM[1] = 0) then
                  aEncoding := TEncoding.UTF16
                else if (aBOM[0] = 0) and (aBOM[1] <> 0) then
                  aEncoding := TEncoding.UTF16LE;
              end;

              result := FALSE;
            end;
  end;


  class function TEncoding.Identify(const aStream: TStream;
                                    var aEncoding: TEncoding): Boolean;
  var
    restorePos: Int64;
    bom: TBOM;
    bytesRead: Integer;
  begin
    restorePos := aStream.Position;

    SetLength(bom, 4);
    bytesRead := aStream.Read(bom[0], 4);
    try
      if bytesRead < 4 then
        SetLength(bom, bytesRead);

      result := Identify(bom, aEncoding);
      if result then
      begin
        // The encoding was identified by a recognizable BOM so
        //  we adjust the restorePos so that we will re-position
        //  the stream on the first byte immediately following
        //  the BOM

        bom         := TEncodingImplementation(aEncoding).BOM;
        restorePos  := restorePos + Length(bom);
      end;

    finally
      aStream.Position := restorePos;
    end;
  end;


  class function TEncoding.Identify(const aBytes: PByte;
                                    const aCount: Integer;
                                    var aEncoding: TEncoding): Boolean;
  var
    bom: TBOM;
  begin
    result := (aCount >= 2);

    if NOT result then
      EXIT;

    if aCount > 4 then
      SetLength(bom, 4)
    else
      SetLength(bom, aCount);

    CopyMemory(@bom, aBytes, Length(bom));

    result := Identify(bom, aEncoding);
  end;







  constructor TEncodingImplementation.Create(const aCodePage: Word);
  begin
    inherited Create;

    fCodePage := aCodePage;
  end;




  function TEncodingImplementation.get_BOM: TBOM;
  begin
    SetLength(result, 0);
  end;







{ TMultiByteEncoding }

  function TMultiByteEncoding.GetByteCount(const aChars: PWIDEChar;
                                           const aCount: Integer): Integer;
  begin
    result := WideCharToMultiByte(fCodePage, 0, aChars, aCount, NIL, 0, NIL, NIL);
  end;


  function TMultiByteEncoding.GetCharCount(const aBytes: PByte;
                                           const aCount: Integer): Integer;
  begin
    result := MultiByteToWideChar(fCodePage, 0, PANSIChar(aBytes), aCount, NIL, 0);
  end;


  function TMultiByteEncoding.Encode(const aChars: PWIDEChar;
                                     const aCharCount: Integer;
                                     const aBytes: PByte;
                                     const aByteCount: Integer): Integer;
  begin
    result := WideCharToMultiByte(fCodePage, 0, aChars, aCharCount, PANSIChar(aBytes), aByteCount, NIL, NIL);
  end;


  function TMultiByteEncoding.Decode(const aBytes: PByte;
                                     const aByteCount: Integer;
                                     const aChars: PWIDEChar;
                                     const aCharCount: Integer): Integer;
  begin
    result := MultiByteToWideChar(fCodePage, 0, PANSIChar(aBytes), aByteCount, aChars, aCharCount);
  end;



initialization

finalization
  _ASCII.Free;
  _UTF8.Free;
  _UTF16.Free;
  _UTF16LE.Free;
  _UTF32.Free;
  _UTF32LE.Free;

  _ASCII    := NIL;
  _UTF8     := NIL;
  _UTF16    := NIL;
  _UTF16LE  := NIL;
  _UTF32    := NIL;
  _UTF32LE  := NIL;

end.
