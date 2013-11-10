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

{$ifdef deltics_types}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Types;


interface

  uses
  { vcl: }
    Classes,
    SysUtils,
  { deltics: }
    Deltics.Strings;


  type
  {$ifdef DELPHIXE_OR_EARLIER}
    // -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
    // Prior to Delphi XE For Delphi Version XE2 onward there may be platform specific variations
    //  for the various platform types but we use a common, portable definition
    //  where-ever possible

    NativeUInt  = Cardinal;
    NativeInt   = Integer;
  {$endif}


    TCustomType = class;

    PObject = ^TObject;

    TStdCallProcedure = procedure; stdcall;
    TStdCallBooleanFunction = function: Boolean; stdcall;

    TArrayOfPointer = array of Pointer;
    TArrayOfCustomType = array of TCustomType;
    TArrayOfANSIString = array of ANSIString;
    TArrayOfString = array of String;
    TArrayOfUnicodeString = array of UnicodeString;

    TCompareResult = (crLesser,   // Comparison identifies left side < right side
                      crEqual,    // Comparison identifies left side = right side
                      crGreater,  // Comparison identifies left side > right side
                      crInvalid   // Left and right sides cannot be compared
                     );
    TCompareFunction = function(const DataA, DataB): TCompareResult;

    TCustomType = class
    private
      fData: array of Byte;
      fSize: Word;

      function get_Byte(const aIndex: Word): Byte;
      function get_Data: Pointer;
      procedure set_Byte(const aIndex: Word; const aValue: Byte);

      function get_AsByte: Byte;
      function get_AsCardinal: Cardinal;
      function get_AsHex: String;
      function get_AsInt64: Int64;
      function get_AsInteger: Integer;
      function get_AsShortInt: ShortInt;
      function get_AsSmallInt: SmallInt;
      function get_AsWord: Word;
      procedure set_AsByte(const aValue: Byte);
      procedure set_AsCardinal(const aValue: Cardinal);
      procedure set_AsInt64(const aValue: Int64);
      procedure set_AsInteger(const aValue: Integer);
      procedure set_AsShortInt(const aValue: ShortInt);
      procedure set_AsSmallInt(const aValue: SmallInt);
      procedure set_AsWord(const aValue: Word);

    protected
      function getAsString: String;
      procedure setSize(const aSize: Word);

    public
      constructor Create(const aSize: Word);
      destructor Destroy; override;

      procedure LoadFromStream(const aStream: TStream); virtual;
      procedure SaveToStream(const aStream: TStream); virtual;

    public
      procedure Assign(const aSource: TCustomType); virtual;
      function Clone: TCustomType;
      procedure Initialise; virtual;
      function IsEqual(const aOther: TCustomType): Boolean;

      property AsByte: Byte read get_AsByte write set_AsByte;
      property AsCardinal: Cardinal read get_AsCardinal write set_AsCardinal;
      property AsHex: String read get_AsHex;
      property AsInt64: Int64 read get_AsInt64 write set_AsInt64;
      property AsInteger: Integer read get_AsInteger write set_AsInteger;
      property AsShortInt: ShortInt read get_AsShortInt write set_AsShortInt;
      property AsSmallInt: SmallInt read get_AsSmallInt write set_AsSmallInt;
      property AsString: String read getAsString;
      property AsWord: Word read get_AsWord write set_AsWord;

      property Bytes[const aIndex: Word]: Byte read get_Byte write set_Byte;
      property Data: Pointer read get_Data;
      property Size: Word read fSize;
    end;
    TCustomTypeClass = class of TCustomType;


    ECustomType = class(Exception);



    TMilliseconds = type Cardinal;
    TSeconds = type Cardinal;



implementation

  uses
  { deltics: }
    Deltics.SysUtils;



  constructor TCustomType.Create(const aSize: Word);
  begin
    inherited Create;

    setSize(aSize);
  end;


  destructor TCustomType.Destroy;
  begin
    setSize(0);

    inherited;
  end;


  function TCustomType.get_Byte(const aIndex: Word): Byte;
  begin
    result := fData[aIndex];
  end;


  function TCustomType.get_Data: Pointer;
  begin
    if (Size > 0) then
      result := @(fData[0])
    else
      result := NIL;
  end;


  procedure TCustomType.set_Byte(const aIndex: Word; const aValue: Byte);
  begin
    fData[aIndex] := aValue;
  end;


  procedure TCustomType.setSize(const aSize: Word);
  begin
    if (Size = aSize) then
      EXIT;

    fSize := aSize;
    SetLength(fData, Size);

    if (aSize > 0) then
      Initialise;
  end;


  procedure TCustomType.LoadFromStream(const aStream: TStream);
  begin
    aStream.Read(fData[0], fSize);
  end;


  procedure TCustomType.SaveToStream(const aStream: TStream);
  begin
    aStream.Write(fData[0], fSize);
  end;


  function TCustomType.get_AsHex: String;
  const
    HEX: array[0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                                 'A', 'B', 'C', 'D', 'E', 'F');
  var
    i: Word;
  begin
    result := '';

    for i := Pred(Size) downto 0 do
      result := result + HEX[(fData[i] and $f0) shr 4] + HEX[fData[i] and $0f];
  end;


  function TCustomType.getAsString: String;
  begin
    result := AsHex;
  end;


  procedure TCustomType.Assign(const aSource: TCustomType);
  begin
    if NOT Assigned(aSource) then
      raise ECustomType.CreateFmt('Cannot assign NIL to a %s', [ClassName]);

    if (ClassType <> aSource.ClassType) then
      raise ECustomType.CreateFmt('Cannot assign a %s to a %s', [aSource.ClassName, ClassName]);

    setSize(aSource.Size);

    if (Size > 0) then
      Move(aSource.Data^, Data^, Size);
  end;


  function TCustomType.Clone: TCustomType;
  begin
    result := TCustomTypeClass(ClassType).Create(Size);
    result.Assign(self);
  end;


  procedure TCustomType.Initialise;
  begin
    if (Size > 0) then
      FillZero(Data^, Size);
  end;


  function TCustomType.IsEqual(const aOther: TCustomType): Boolean;
  begin
    result := (Size = aOther.Size) and CompareMem(Data, aOther.Data, Size);
  end;


  function TCustomType.get_AsCardinal: Cardinal;
  begin
    case Size of
      1: result := Cardinal(AsByte);
      2: result := Cardinal(AsWord);
      4: result := Cardinal(Data^);
    else
      raise ECustomType.CreateFmt('AsCardinal is not valid for %s', [ClassName]);
    end;
  end;


  function TCustomType.get_AsInt64: Int64;
  begin
    case Size of
      1: result := Int64(AsShortInt);
      2: result := Int64(AsSmallInt);
      4: result := Int64(AsInteger);
      8: result := Int64(Data^);
    else
      raise ECustomType.CreateFmt('AsInt64 is not valid for %s', [ClassName]);
    end;
  end;


  procedure TCustomType.set_AsInt64(const aValue: Int64);
  begin
    if (Size <> sizeof(Int64)) then
      raise ECustomType.CreateFmt('AsInt64 is not valid for %s', [ClassName]);

    Int64(Data^) := aValue;
  end;


  function TCustomType.get_AsInteger: Integer;
  begin
    case Size of
      1: result := Integer(AsShortInt);
      2: result := Integer(AsSmallInt);
      4: result := Integer(Data^);
    else
      raise ECustomType.CreateFmt('AsInteger is not valid for %s', [ClassName]);
    end;
  end;


  procedure TCustomType.set_AsCardinal(const aValue: Cardinal);
  begin
    if (Size <> sizeof(Cardinal)) then
      raise ECustomType.CreateFmt('AsCardinal is not valid for %s', [ClassName]);

    Cardinal(Data^) := aValue;
  end;


  procedure TCustomType.set_AsInteger(const aValue: Integer);
  begin
    if (Size <> sizeof(Integer)) then
      raise ECustomType.CreateFmt('AsInteger is not valid for %s', [ClassName]);

    Integer(Data^) := aValue;
  end;


  function TCustomType.get_AsWord: Word;
  begin
    case Size of
      1: result := Word(AsByte);
      2: result := Word(Data^);
    else
      raise ECustomType.CreateFmt('AsWord is not valid for %s', [ClassName]);
    end;
  end;


  procedure TCustomType.set_AsWord(const aValue: Word);
  begin
    if (Size <> sizeof(Word)) then
      raise ECustomType.CreateFmt('AsWord is not valid for %s', [ClassName]);

    Word(Data^) := aValue;
  end;



  function TCustomType.get_AsByte: Byte;
  begin
    case Size of
      1: result := Byte(Data^);
    else
      raise ECustomType.CreateFmt('AsByte is not valid for %s', [ClassName]);
    end;
  end;


  function TCustomType.get_AsShortInt: ShortInt;
  begin
    case Size of
      1: result := ShortInt(Data^);
    else
      raise ECustomType.CreateFmt('AsShortInt is not valid for %s', [ClassName]);
    end;
  end;

  function TCustomType.get_AsSmallInt: SmallInt;
  begin
    case Size of
      1: result := SmallInt(AsShortInt);
      2: result := SmallInt(Data^);
    else
      raise ECustomType.CreateFmt('AsSmalInt is not valid for %s', [ClassName]);
    end;

  end;


  procedure TCustomType.set_AsByte(const aValue: Byte);
  begin
    if (Size > sizeof(Byte)) then
      raise ECustomType.CreateFmt('AsByte is not valid for %s', [ClassName]);

    Byte(Data^) := aValue;
  end;


  procedure TCustomType.set_AsShortInt(const aValue: ShortInt);
  begin
    if (Size > sizeof(ShortInt)) then
      raise ECustomType.CreateFmt('AsShortInt is not valid for %s', [ClassName]);

    ShortInt(Data^) := aValue;
  end;


  procedure TCustomType.set_AsSmallInt(const aValue: SmallInt);
  begin
    if (Size > sizeof(SmallInt)) then
      raise ECustomType.CreateFmt('AsSmallInt is not valid for %s', [ClassName]);

    SmallInt(Data^) := aValue;
  end;




end.

