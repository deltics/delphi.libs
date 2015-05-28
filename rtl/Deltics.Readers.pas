

  unit Deltics.Readers;


interface

  uses
    Classes,
    Deltics.Classes;


  type
    IReader = interface
    ['{1F0A8E5D-2284-4D9C-B8E3-44C9D304C7D3}']
      function get_EOF: Boolean;
      function Read(var aBytes; aBytesToRead: Integer): Integer;
      function ReadByte(var aByte: Byte): Boolean;

      property EOF: Boolean read get_EOF;
    end;


    TStreamReaderEOFFn = function: Boolean of object;
    TStreamReaderReadFn = function(var aBytes; aBytesToRead: Integer): Integer of object;
    TStreamReaderReadByteFn = function(var aByte: Byte): Boolean of object;


    TStreamReader = class(TCOMInterfacedObject, IReader)
    private
      fOwnsStream: Boolean;
      fStream: TStream;
      fBuffer: PByte;
      fBufferLength: Cardinal;
      fBufferSize: Word;
      fBufferPos: Cardinal;
      fEOF_fn: TStreamReaderEOFFn;
      fRead_fn: TStreamReaderReadFn;
      fReadByte_fn: TStreamReaderReadByteFn;
      function EOFBuffered: Boolean;
      function EOFUnbuffered: Boolean;
      function ReadByteBuffered(var aByte: Byte): Boolean;
      function ReadByteUnbuffered(var aByte: Byte): Boolean;
      function ReadBuffered(var aBytes; aBytesToRead: Integer): Integer;
      function ReadUnbuffered(var aBytes; aBytesToRead: Integer): Integer;
    public
      constructor Create(const aStream: TStream; const aBufferSizeKB: Word = 4);
      constructor CreateAsOwnerOf(const aStream: TStream; const aBufferSizeKB: Word = 4);
      destructor Destroy; override;

    private // IReader
      function get_EOF: Boolean;
    public
      function Read(var aBytes; aBytesToRead: Integer): Integer;
      function ReadByte(var aByte: Byte): Boolean;
      property EOF: Boolean read get_EOF;
    end;


    TBufferReader = class(TCOMInterfacedObject, IReader)
    private
      fBuffer: PByte;
      fBufferLength: Cardinal;
      fBufferPos: Cardinal;
      fOwnsBuffer: Boolean;
    public
      constructor Create(const aBuffer: Pointer; const aBufferLength: Cardinal);
      constructor CreateAsOwnerOf(const aBuffer: Pointer; const aBufferLength: Cardinal);
      destructor Destroy; override;

    private // IReader
      function get_EOF: Boolean;
    public
      function Read(var aBytes; aBytesToRead: Integer): Integer;
      function ReadByte(var aByte: Byte): Boolean;
      property EOF: Boolean read get_EOF;
    end;



implementation

  uses
    Windows;


{ TStreamReader }

  constructor TStreamReader.Create(const aStream: TStream;
                                   const aBufferSizeKB: Word);
  begin
    inherited Create;

    fStream     := aStream;
    fBufferSize := aBufferSizeKB * 1024;

    if (fBufferSize > 0) then
    begin
      fEOF_fn       := EOFBuffered;
      fRead_fn      := ReadBuffered;
      fReadByte_fn  := ReadByteBuffered;

      GetMem(fBuffer, fBufferSize);

      fBufferLength := fStream.Read(fBuffer^, fBufferSize);
    end
    else
    begin
      fEOF_fn       := EOFUnbuffered;
      fRead_fn      := ReadUnbuffered;
      fReadByte_fn  := ReadByteUnbuffered;
    end;
  end;


  constructor TStreamReader.CreateAsOwnerOf(const aStream: TStream;
                                            const aBufferSizeKB: Word);
  begin
    Create(aStream, aBufferSizeKB);

    fOwnsStream := TRUE;
  end;


  destructor TStreamReader.Destroy;
  begin
    if fOwnsStream then
    begin
      fStream.Free;
      fStream := NIL;
    end;

    if Assigned(fBuffer) then
    begin
//      FreeMem(fBuffer);
      fBuffer := NIL;
    end;

    inherited;
  end;


  function TStreamReader.EOFBuffered: Boolean;
  begin
    result := (fStream.Position = fStream.Size) and (fBufferPos = fBufferLength);
  end;


  function TStreamReader.EOFUnbuffered: Boolean;
  begin
    result := (fStream.Position = fStream.Size);
  end;


  function TStreamReader.get_EOF: Boolean;
  begin
    result := fEOF_fn()
  end;


  function TStreamReader.Read(var aBytes; aBytesToRead: Integer): Integer;
  begin
    result := fRead_fn(aBytes, aBytesToRead);
  end;


  function TStreamReader.ReadByte(var aByte: Byte): Boolean;
  begin
    result := fReadByte_fn(aByte);
  end;


  function TStreamReader.ReadBuffered(var aBytes; aBytesToRead: Integer): Integer;
  var
    dest: PByte;
    bytesLeft: Integer;
  begin
    if ((fBufferPos + Cardinal(aBytesToRead)) <= fBufferLength) then
    begin
      case aBytesToRead of
        1 : Byte(aBytes) := PByte(Cardinal(fBuffer) + fBufferPos)^;
      else
        CopyMemory(@aBytes, Pointer(Cardinal(fBuffer) + fBufferPos), aBytesToRead);
      end;

      Inc(fBufferPos, aBytesToRead);
      result := aBytesToRead;
    end
    else
    begin
      bytesLeft := fBufferLength - fBufferPos;

      if (bytesLeft > 0) then
      begin
        CopyMemory(@aBytes, Pointer(Cardinal(fBuffer) + fBufferPos), bytesLeft);

        Dec(aBytesToRead, bytesLeft);
        Inc(fBufferPos, bytesLeft);
      end;

      result := bytesLeft;

      while (NOT EOF) and (result < aBytesToRead) do
      begin
        fBufferPos    := 0;
        fBufferLength := fStream.Read(fBuffer^, fBufferSize);

        bytesLeft := fBufferLength - fBufferPos;
        if aBytesToRead < bytesLeft then
          bytesLeft := aBytesToRead;

        dest := PByte(@aBytes);
        CopyMemory(Pointer(Cardinal(dest) + Cardinal(result)), fBuffer, bytesLeft);

        Dec(aBytesToRead, bytesLeft);
        Inc(fBufferPos, bytesLeft);

        result := result + bytesLeft;
      end;
    end;
  end;


  function TStreamReader.ReadByteBuffered(var aByte: Byte): Boolean;
  begin
    result := FALSE;

    if (fBufferPos = fBufferLength) then
    begin
      fBufferPos    := 0;
      fBufferLength := fStream.Read(fBuffer^, fBufferSize);

      if fBufferLength = 0 then
        EXIT;
    end;

    aByte := PByte(Cardinal(fBuffer) + fBufferPos)^;
    Inc(fBufferPos);

    result := TRUE;
  end;


  function TStreamReader.ReadByteUnbuffered(var aByte: Byte): Boolean;
  begin
    result := fStream.Read(aByte, 1) = 1;
  end;


  function TStreamReader.ReadUnbuffered(var aBytes; aBytesToRead: Integer): Integer;
  begin
    result := fStream.Read(aBytes, aBytesToRead);
  end;





  constructor TBufferReader.Create(const aBuffer: Pointer;
                                   const aBufferLength: Cardinal);
  begin
    inherited Create;

    fBuffer       := aBuffer;
    fBufferLength := aBufferLength;
  end;


  constructor TBufferReader.CreateAsOwnerOf(const aBuffer: Pointer;
                                            const aBufferLength: Cardinal);
  begin
    Create(aBuffer, aBufferLength);

    fOwnsBuffer := TRUE;
  end;


  destructor TBufferReader.Destroy;
  begin
    if fOwnsBuffer then
    begin
      FreeMem(fBuffer);
      fBuffer := NIL;
    end;

    inherited;
  end;


  function TBufferReader.get_EOF: Boolean;
  begin
    result := (fBufferPos = fBufferLength);
  end;



  function TBufferReader.Read(var aBytes; aBytesToRead: Integer): Integer;
  var
    bytesLeft: Integer;
  begin
    bytesLeft := (fBufferLength - fBufferPos);
    if aBytesToRead > bytesLeft then
      aBytesToRead := bytesLeft;

    CopyMemory(@aBytes, Pointer(Cardinal(fBuffer) + fBufferPos), aBytesToRead);
    result := aBytesToRead;

    Inc(fBufferPos, result);
  end;


  function TBufferReader.ReadByte(var aByte: Byte): Boolean;
  begin
    result := fBufferPos < fBufferLength;
    if result then
    begin
      aByte := PByte(Cardinal(fBuffer) + fBufferPos)^;
      Inc(fBufferPos);
    end;
  end;






end.
