
  unit Deltics.Strings.Encoding.UTF16;

interface

  uses
    Deltics.Strings.Encoding;


  type
    TUTF16LEEncoding = class(TEncodingImplementation)
    protected
      constructor Create; override;
      function get_BOM: TBOM; override;
    public
      function GetByteCount(const aChars: PWIDEChar; const aCount: Integer): Integer; override;
      function GetCharCount(const aBytes: PByte; const aCount: Integer): Integer; override;
      function Decode(const aBytes: PByte; const aByteCount: Integer; const aChars: PWIDEChar; const aCharCount: Integer): Integer; override;
      function Encode(const aChars: PWIDEChar; const aCharCount: Integer; const aBytes: PByte; const aByteCount: Integer): Integer; override;
    end;


    TUTF16BEEncoding = class(TUTF16LEEncoding)
    protected
      constructor Create; override;
      function get_BOM: TBOM; override;
    public
      function Decode(const aBytes: PByte; const aByteCount: Integer; const aChars: PWIDEChar; const aCharCount: Integer): Integer; override;
      function Encode(const aChars: PWIDEChar; const aCharCount: Integer; const aBytes: PByte; const aByteCount: Integer): Integer; override;
    end;


implementation

  uses
    Windows,
    Deltics.SysUtils;


{ TUTF16LEEncoding }

  constructor TUTF16LEEncoding.Create;
  begin
    inherited Create(CP_UTF16LE);
  end;


  function TUTF16LEEncoding.get_BOM: TBOM;
  begin
    SetLength(result, 2);
    result[0] := BOM_UTF16LE[0];
    result[1] := BOM_UTF16LE[1];
  end;


  function TUTF16LEEncoding.GetByteCount(const aChars: PWIDEChar;
                                         const aCount: Integer): Integer;
  begin
    result := aCount * 2;
  end;


  function TUTF16LEEncoding.GetCharCount(const aBytes: PByte;
                                         const aCount: Integer): Integer;
  begin
    result := aCount div 2;
  end;


  function TUTF16LEEncoding.Decode(const aBytes: PByte;
                                   const aByteCount: Integer;
                                   const aChars: PWIDEChar;
                                   const aCharCount: Integer): Integer;
  begin
    CopyMemory(aBytes, aChars, aByteCount);
    result := aByteCount;
  end;


  function TUTF16LEEncoding.Encode(const aChars: PWIDEChar;
                                   const aCharCount: Integer;
                                   const aBytes: PByte;
                                   const aByteCount: Integer): Integer;
  begin
    CopyMemory(aChars, aBytes, aByteCount);
    result := aByteCount;
  end;






{ TUTF16BEEncoding }

  constructor TUTF16BEEncoding.Create;
  begin
    inherited Create(CP_UTF16);
  end;


  function TUTF16BEEncoding.get_BOM: TBOM;
  begin
    SetLength(result, 2);
    result[0] := BOM_UTF16BE[0];
    result[1] := BOM_UTF16BE[1];
  end;


  function TUTF16BEEncoding.Decode(const aBytes: PByte;
                                   const aByteCount: Integer;
                                   const aChars: PWIDEChar;
                                   const aCharCount: Integer): Integer;
  begin
    result := inherited Decode(aBytes, aByteCount, aChars, aCharCount);
    ReverseBytes(System.PWord(aChars), aCharCount);
  end;


  function TUTF16BEEncoding.Encode(const aChars: PWIDEChar;
                                   const aCharCount: Integer;
                                   const aBytes: PByte;
                                   const aByteCount: Integer): Integer;
  begin
    result := inherited Encode(aChars, aCharCount, aBytes, aByteCount);
    ReverseBytes(System.PWord(aBytes), aCharCount);
  end;





end.
