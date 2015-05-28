
  unit Deltics.BitField;

interface

  uses
    Classes;

type
  TBitBytes = array of Byte;

  TBitField = class
  {
    An encapsulation of a field of individually switchable bits.

    To use this class, create an instance, specifying the number of BITS
     in the field.  To set or clear these bits use the Bit[] array property,
     using the appropriate bit number.

    When a bit field is created, all bits are initially OFF (FALSE).

    NOTE: Bits are numbered from 1, not zero.  i.e. in a 16-bit field, the
     individual bits would be accessed as follows:

      Bit[]  16 15 14 13 12 11 10 9  8  7  6  5  4  3  2  1

             0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0

    So, to set the 7th and 14th bits in a 16 bit field:

             bits := TBitField,Create(16);
             bits[7] := TRUE;
             bits[14] := TRUE;

    Would give the result:

      Bit[]  16 15 14 13 12 11 10 9  8  7  6  5  4  3  2  1

             0  0  1  0  0  0  0  0  0  1  0  0  0  0  0  0


    Bit fields are represented internally as a byte array.  If the number
     of bits in the field do not fully occupy the final bits in the storage
     the storage is unused.  The bit field implementation always uses the
     minimum number of bytes required to represent the specified number
     of bits.

    Examples:

        Number of Bits    Bytes Used    Unused Bits

          4                 1             4 (5 thru 8)
          8                 1             0
          9                 2             7 (10 thru 16)
         16                 2             0

    Note that although internally memory is allocated for these unused
     bits, attempting to address bits outside the range specified when
     a bit field is created will result in an exception.

    For debugging purposes, a bit field supports an AsString property
     which yields a binary string representation of the bit field.  The
     string describes the bits in the bit field Data, representing
     unused bits as '.' characters.  Used bits are represented in the
     string with a '1' or '0', according to whether that bit is currently
     set or cleared.

    E.g. a 12 bit field with bits 1 thru 6 set and 7 thru 12 cleared
     is described by the AsString property as:

        ....000000111111


    Finally, bit fields support streaming via the usual LoadFromStream
     and SaveToStream methods.
  }
  private
    fCount: Integer;      // The number of bits in the field
    fData: TBitBytes;     // Byte array to store the field data
    fDataBase: PByte;     // Pointer to byte data (may or may NOT be a pointer to fData)
    fDataSize: Integer;   // Size (in bytes) of the field data

    function get_AsString: String;
    function get_Bit(const aIndex: Integer): Boolean;
    procedure set_Bit(const aIndex: Integer;
                      const aValue: Boolean);

    procedure CheckIndex(const aBitIndex: Integer);

    procedure GetByteWithBit(const aBitIndex: Integer;
                             var aByte: Byte);
    procedure PutByteWithBit(const aBitIndex: Integer;
                             const aByte: Byte);
    function BitOffsetIntoByte(const aBitIndex: Integer): Integer;

  public
    constructor Create(const aBitCount: Integer); overload;
    constructor Create(const aData: Pointer; const aBytes: Integer); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStream(const aStream: TStream);
    procedure SaveToStream(const aStream: TStream);

    property AsString: String read get_AsString;
    property Bit[const aIndex: Integer]: Boolean read get_Bit write set_Bit; default;
    property Count: Integer read fCount;
  end;



implementation

  uses
    SysUtils,
    Windows;


{ -----------------------------------------------------------------------------}
constructor TBitField.Create(const aBitCount: Integer);
{
  Determines the minimum number of bytes required to store the
   specified number of bits (must be > 1), and allocates and
   initialises that storage.
}
begin
  ASSERT(aBitCount > 1, 'Bit fields must contain at least two bits');

  inherited Create;

  fCount := aBitCount;

  if ((Count mod 8) = 0) then
    fDataSize := (Count div 8)
  else
    fDataSize := (Count div 8) + 1;

  SetLength(fData, fDataSize);
  fDataBase := @fData[0];

  Clear;
end;


{ -----------------------------------------------------------------------------}
constructor TBitField.Create(const aData: Pointer; const aBytes: Integer);
begin
  Create(aBytes * 8);

  fDataBase := PByte(aData);
  fDataSize := aBytes;

  SetLength(fData, 0);
end;


{ -----------------------------------------------------------------------------}
destructor TBitField.Destroy;
{
  Returns to the system the memory allocated for the internal
   storage of the bit field.
}
begin
  SetLength(fData, 0);

  inherited;
end;


{ -----------------------------------------------------------------------------}
function TBitField.get_AsString: String;
{
  Property getter for the AsString property.

  Returns a string representation of the bit field.  Bits are
   represented by 1's or 0's (right-to-left, first-to-last)
   according to whether they are currently set (1) or cleared (0).

  Any unused bits are represented by '.' characters, and appear
   to the left (at the end) of the string.
}
const
  {@stb}
  BINARY_DIGIT : array[FALSE..TRUE] of Char = ('0', '1');
  {@ste}
var
  i: Integer;
begin
  // Initialise a string with all '.' chars for all bits in the
  //  field
  result := StringOfChar('.', (fDataSize * 8));

  // Now overwrite the chars that represent used bits with the
  //  appropriate char (1 = set, 0 = cleared)
  for i := 1 to Count do
    result[(Count - i) + 1] := BINARY_DIGIT[Bit[i]];

  for i := Length(result) downto 1 do
    if i mod 8 = 0 then
      result := Copy(result, 1, i) + ' ' + Copy(result, i + 1, Length(result) - i);
end;


{ -----------------------------------------------------------------------------}
function TBitField.get_Bit(const aIndex: Integer): Boolean;
{
  Property getter for the Bit array property.

  Returns the current state of a specified bit (TRUE = set, FALSE = cleared).

  NOTE: Bit indices are 1 based.  Indexing errors are detected and
   handled by calling CheckIndex before proceeding which will raise an
   exception if the index is invalid.
}
var
  dataByte: Byte;
  testByte: Byte;
begin
  CheckIndex(aIndex);

  // Create a mask byte consisting of all bits cleared with the
  //  exception of the bit representing the bit index specified
  testByte := 1 shl BitOffsetIntoByte(aIndex);

  // Get the byte from internal storage containing the specified
  //  bit and test it against the mask we just created
  GetByteWithBit(aIndex, dataByte);

  result := (dataByte and testByte) = testByte;
end;


{ -----------------------------------------------------------------------------}
procedure TBitField.set_Bit(const aIndex: Integer;
                            const aValue: Boolean);
{
  Property setter for the Bit array property.

  Changes the state of the specified bit (TRUE = set, FALSE = clear).

  NOTE: Bit indices are 1 based.  Indexing errors are detected and
   handled by the fact that the current setting of the specified Bit
   is first tested to see if it needs to be changed.
}
var
  dataByte: Byte;
  updateByte: Byte;
begin
  if (Bit[aIndex] = aValue) then
    EXIT;

  // Create a mask byte consisting of all bits cleared with the
  //  exception of the bit representing the bit index specified
  updateByte := 1 shl BitOffsetIntoByte(aIndex);

  // To update the single bit, we get the byte containing that bit,
  //  modify the relevant bit and then replace the entire byte
  GetByteWithBit(aIndex, dataByte);

  if NOT aValue then
    dataByte := (dataByte xor updateByte)   // Clear the bit
  else
    dataByte := (dataByte or updateByte);   // Set the bit

  PutByteWithBit(aIndex, dataByte);
end;


{ -----------------------------------------------------------------------------}
procedure TBitField.CheckIndex(const aBitIndex: Integer);
{
  Tests the specified bit index to ensure that it is valid and in
   range for this bit field.

  Raises an EInvalidOperation exception if the index is not valid.
}
begin
  if (aBitIndex < 1) or (aBitIndex > Count) then
    {@stb}
    raise EInvalidOp.CreateFmt('%d is not a valid bit index', [aBitIndex]);
    {@ste}
end;


{ -----------------------------------------------------------------------------}
procedure TBitField.Clear;
begin
  ZeroMemory(fDataBase, fDataSize);
end;


{ -----------------------------------------------------------------------------}
function TBitField.BitOffsetIntoByte(const aBitIndex: Integer): Integer;
{
  Returns the offset into the relevant data byte of the bit with the
   specified index.  The actual byte # is not relevant to the result of
   this function but is included in the examples below for clarity:

  Examples:      Bit #   (Byte #)  Offset

                   1        1        0
                   4        1        3
                   9        2        0
                  10        2        1

}
begin
  result := ((aBitIndex - 1) mod 8);
end;


{ -----------------------------------------------------------------------------}
procedure TBitField.GetByteWithBit(const aBitIndex: Integer;
                                   var aByte: Byte);
{
  Obtains the byte from the internal storage that contains the specified
   bit, and returns the entire byte in the specified var param.
}
begin
  aByte := fDataBase[(aBitIndex - 1) div 8];
end;


{ -----------------------------------------------------------------------------}
procedure TBitField.PutByteWithBit(const aBitIndex: Integer;
                                   const aByte: Byte);
{
  Replaces the byte in internal storage that contains the specified
   bit, with the data in the specified byte.
}
begin
  fDataBase[(aBitIndex - 1) div 8] := aByte;
end;


{ -----------------------------------------------------------------------------}
procedure TBitField.LoadFromStream(const aStream: TStream);
{
  Reads internal storage data from the specified stream.
}
begin
  aStream.Read(fDataBase^, fDataSize);
end;


{ -----------------------------------------------------------------------------}
procedure TBitField.SaveToStream(const aStream: TStream);
{
  Writes the internal storage data to the specified stream.
}
begin
  aStream.Write(fDataBase^, fDataSize);
end;



end.
