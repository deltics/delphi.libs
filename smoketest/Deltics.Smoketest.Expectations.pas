{
  * X11 (MIT) LICENSE *

  Copyright © Jolyon Smith

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

{$i deltics.smoketest.inc}

{$ifdef deltics_smoketest_expectations}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Smoketest.Expectations;


interface

  uses
  { vcl: }
    SysUtils,
  { deltics: }
    Deltics.Classes,
    Deltics.DateUtils,
    Deltics.Strings,
  { smoketest: }
    Deltics.Smoketest;


  type
    TBooleanExpectation = class(TExpectation, BooleanExpectation)
    private
      fValue: Boolean;
    public
      constructor Create(const aSubject: TTest; const aValue: Boolean);
      property Value: Boolean read fValue;
    public  // IBooleanExpecation
      function Equals(const aExpected: Boolean): Evaluation; reintroduce;
      function IsFALSE: Evaluation;
      function IsTRUE: Evaluation;
    end;


    TApproximateExpectation = class(TExpectation, ApproximateExpectation)
    private
      fDPs: Integer;
      fMaximum: Currency;
      fMinimum: Currency;
      fOriginal: Currency;
      fValue: Currency;
    public
      constructor Create(const aSubject: TTest;
                         const aValue: Currency;
                         const aDPs: Integer);
      function ToRequiredDPs(const aValue: Currency): Currency;
      function ToString(const aValue: Currency): UnicodeString; reintroduce;
      property Value: Currency read fValue;
    protected
      function DoReplaceTokens(var aString: UnicodeString): Boolean; override;
    public // ApproximateExpectation
      function Between(const aMin, aMax: Currency): Evaluation;
      function Equals(const aExpected: Currency): Evaluation; reintroduce;
      function GreaterThan(const aValue: Currency): Evaluation;
      function InRange(const aMin, aMax: Currency): Evaluation;
      function LessThan(const aValue: Currency): Evaluation;
      function NotGreaterThan(const aValue: Currency): Evaluation;
      function NotLessThan(const aValue: Currency): Evaluation;
    end;


    TSingleExpectation = class(TExpectation, SingleExpectation)
    private
      fMinimum: Single;
      fMaximum: Single;
      fValue: Single;
    public
      constructor Create(const aSubject: TTest;
                         const aValue: Single);
      property Value: Single read fValue;
    protected
      function DoReplaceTokens(var aString: UnicodeString): Boolean; override;
    public // SingleExpectation
      function ToDPs(const aDPs: Integer): ApproximateExpectation;
      function Between(const aMin, aMax: Single): Evaluation;
      function Equals(const aExpected: Single): Evaluation; reintroduce;
      function GreaterThan(const aValue: Single): Evaluation;
      function InRange(const aMin, aMax: Single): Evaluation;
      function LessThan(const aValue: Single): Evaluation;
      function NotGreaterThan(const aValue: Single): Evaluation;
      function NotLessThan(const aValue: Single): Evaluation;
    end;


    TDoubleExpectation = class(TExpectation, DoubleTest,
                                             DoubleExpectation)
    private
      fMinimum: Double;
      fMaximum: Double;
      fValue: Double;
    public
      constructor Create(const aSubject: TTest;
                         const aValue: Double);
      property Value: Double read fValue;
    protected
      function DoReplaceTokens(var aString: UnicodeString): Boolean; override;
    public // DoubleTest
      function Expect(aValue: Double): DoubleExpectation;
    public // DoubleExpectation
      function ToDPs(const aDPs: Integer): ApproximateExpectation;
      function Between(const aMin, aMax: Double): Evaluation;
      function Equals(const aExpected: Double): Evaluation; reintroduce;
      function GreaterThan(const aValue: Double): Evaluation;
      function InRange(const aMin, aMax: Double): Evaluation;
      function LessThan(const aValue: Double): Evaluation;
      function NotGreaterThan(const aValue: Double): Evaluation;
      function NotLessThan(const aValue: Double): Evaluation;
    end;


    TExtendedExpectation = class(TExpectation, ExtendedExpectation)
    private
      fMinimum: Extended;
      fMaximum: Extended;
      fValue: Extended;
    protected
      function DoReplaceTokens(var aString: UnicodeString): Boolean; override;
    public
      constructor Create(const aSubject: TTest;
                         const aValue: Extended);
      property Value: Extended read fValue;
    public // ExtendedExpectation
      function ToDPs(const aDPs: Integer): ApproximateExpectation;
      function Between(const aMin, aMax: Extended): Evaluation;
      function Equals(const aExpected: Extended): Evaluation; reintroduce;
      function GreaterThan(const aValue: Extended): Evaluation;
      function InRange(const aMin, aMax: Extended): Evaluation;
      function LessThan(const aValue: Extended): Evaluation;
      function NotGreaterThan(const aValue: Extended): Evaluation;
      function NotLessThan(const aValue: Extended): Evaluation;
    end;


    TCurrencyExpectation = class(TExpectation, CurrencyTest,
                                               CurrencyExpectation)
    private
      fMinimum: Currency;
      fMaximum: Currency;
      fValue: Currency;
    protected
      function DoReplaceTokens(var aString: UnicodeString): Boolean; override;
    public
      constructor Create(const aSubject: TTest;
                         const aValue: Currency);
      property Value: Currency read fValue;
    public // CurrencyTest
      function Expect(aValue: Currency): CurrencyExpectation;
    public // CurrencyExpectation
      function ToDPs(const aDPs: Integer): ApproximateExpectation;
      function Between(const aMin, aMax: Currency): Evaluation;
      function Equals(const aExpected: Currency): Evaluation; reintroduce;
      function GreaterThan(const aValue: Currency): Evaluation;
      function InRange(const aMin, aMax: Currency): Evaluation;
      function LessThan(const aValue: Currency): Evaluation;
      function NotGreaterThan(const aValue: Currency): Evaluation;
      function NotLessThan(const aValue: Currency): Evaluation;
    end;


    TCardinalExpectation = class(TExpectation, CardinalExpectation)
    private
      fMaximum: Cardinal;
      fMinimum: Cardinal;
      fValue: Cardinal;
    protected
      function DoReplaceTokens(var aString: UnicodeString): Boolean; override;
    public
      property Value: Cardinal read fValue;
      constructor Create(const aSubject: TTest;
                         const aValue: Cardinal);
    public // CardinalExpectation
      function Between(const aMin, aMax: Cardinal): Evaluation;
      function Equals(const aExpected: Cardinal): Evaluation; reintroduce;
      function GreaterThan(const aValue: Cardinal): Evaluation;
      function InRange(const aMin, aMax: Cardinal): Evaluation;
      function LessThan(const aValue: Cardinal): Evaluation;
      function NotGreaterThan(const aValue: Cardinal): Evaluation;
      function NotLessThan(const aValue: Cardinal): Evaluation;
    end;


    TIntegerExpectation = class(TExpectation, IntegerExpectation)
    private
      fMaximum: Int64;
      fMinimum: Int64;
      fValue: Int64;
    protected
      function DoReplaceTokens(var aString: UnicodeString): Boolean; override;
    public
      property Value: Int64 read fValue;
      constructor Create(const aSubject: TTest;
                         const aValue: Int64);
    public // IntegerExpectation
      function Between(const aMin, aMax: Int64): Evaluation;
      function Equals(const aExpected: Int64): Evaluation; reintroduce;
      function GreaterThan(const aValue: Int64): Evaluation;
      function InRange(const aMin, aMax: Int64): Evaluation;
      function LessThan(const aValue: Int64): Evaluation;
      function NotGreaterThan(const aValue: Int64): Evaluation;
      function NotLessThan(const aValue: Int64): Evaluation;
    end;


    TTestException = class(TExpectation, ExceptionExpectation)
    private
      fException: Exception;
      fExceptionClass: TExceptionClass;
    public
      constructor CreateExpecting(const aSubject: TTest;
                                  const aExceptionClass: TExceptionClass); overload;
      constructor CreateUnexpected(const aSubject: TTest); overload;
      destructor Destroy; override;
    public // ExceptionExpectation
      function get_Exception: Exception;
      function get_ExceptionClass: TExceptionClass;
//      property Exception: Exception read get_Exception;
//      property ExceptionClass: TExceptionClass read get_ExceptionClass;
    end;


    TInterfaceExpectation = class(TExpectation, InterfaceExpectation)
    private
      fValue: TWeakInterface;
      function get_Value: IInterface;
    public
      constructor Create(const aSubject: TTest;
                         const aValue: IInterface);
      destructor Destroy; override;
      property Value: IInterface read get_Value;
    public // InterfaceExpectation
      function Equals(const aExpected: IInterface): Evaluation; reintroduce;
      function IsAssigned: Evaluation;
      function IsNIL: Evaluation;
      function Supports(const aIID: TGUID; const aName: UnicodeString = ''): Evaluation;
    end;


    TGUIDExpectation = class(TExpectation, GUIDExpectation)
    private
      fValue: TGUID;
    public
      constructor Create(const aSubject: TTest;
                         const aValue: TGUID);
      property Value: TGUID read fValue;
    public // GUIDExpectation
      function Equals(const aValue: TGUID): Evaluation; reintroduce; overload;
      function IsNull: Evaluation;
    end;


    TPointerExpectation = class(TExpectation, PointerExpectation)
    private
      fValue: Pointer;
    public
      constructor Create(const aSubject: TTest;
                         const aValue: Pointer);
      property Value: Pointer read fValue;
    public // PointerExpectation
      function Equals(const aPointer: Pointer): Evaluation; reintroduce; overload;
      function Equals(const aPointer: Pointer; const aBytes: Integer): Evaluation; reintroduce; overload;
      function IsAssigned: Evaluation;
      function IsNIL: Evaluation;
    end;


      TObjectExpectation = class(TPointerExpectation, ObjectExpectation)
      private
        function get_Value: TObject;
      public
        property Value: TObject read get_Value;
        constructor Create(const aSubject: TTest;
                           const aValue: TObject);
      public // ObjectExpectation
        function Equals(const aRef: TObject): Evaluation; reintroduce;
        function IsAssigned: Evaluation;
        function IsNIL: Evaluation;
        function IsInstanceOf(const aClass: TClass): Evaluation;
        function Supports(const aIID: TGUID; const aName: UnicodeString = ''): Evaluation;
      end;



    TStringExpectation = class(TExpectation, UTF8Test,
                                             StringExpectation,
                                             UTF8Expectation)
    {
      Implements case-sensitive (and case-irrelevant) UnicodeString tests.
    }
    private
      fValue: UnicodeString;
    protected
      property Value: UnicodeString read fValue;
    public
      constructor Create(const aSubject: TTest; const aValue: UnicodeString);

    public // UTF8Test
      function Expect(aValue: UTF8String): UTF8Expectation;

    public // StringExpectation + UTF8Expectation
      function IsEmpty: Evaluation;
      function IsLowercase: Evaluation;
      function IsUppercase: Evaluation;
      function Length: IntegerExpectation;

    public // StringExpectation
      function CaseInsensitive: TextExpectation;
      function BeginsWith(const aString: ANSIString): Evaluation; overload;
      function Contains(const aSubString: ANSIString): Evaluation; overload;
      function EndsWith(const aString: ANSIString): Evaluation; overload;
      function Equals(const aExpected: ANSIString): Evaluation; reintroduce; overload;

      function BeginsWith(const aString: UnicodeString): Evaluation; overload;
      function Contains(const aSubString: UnicodeString): Evaluation; overload;
      function EndsWith(const aString: UnicodeString): Evaluation; overload;
      function Equals(const aExpected: UnicodeString): Evaluation; reintroduce; overload;

    public // UTF8Expectation
      function UTF8Expectation.CaseInsensitive = UTF8CaseInsensitive;
      function UTF8CaseInsensitive: UTF8TextExpectation;
      function BeginsWith(const aString: UTF8String): Evaluation; overload;
      function Contains(const aSubString: UTF8String): Evaluation; overload;
      function EndsWith(const aString: UTF8String): Evaluation; overload;
      function Equals(const aExpected: UTF8String): Evaluation; reintroduce; overload;
    end;


    TTextExpectation = class(TExpectation, TextExpectation,
                                           UTF8TextExpectation)
    {
      Implements case-insensitive UnicodeString tests.
    }
    private
      fValue: UnicodeString;
    public
      constructor Create(const aSubject: TTest; const aValue: UnicodeString);
      property Value: UnicodeString read fValue;

    public // TextExpectation
      function BeginsWith(const aString: ANSIString): Evaluation; overload;
      function Contains(const aSubString: ANSIString): Evaluation; overload;
      function EndsWith(const aString: ANSIString): Evaluation; overload;
      function Equals(const aExpected: ANSIString): Evaluation; reintroduce; overload;

      function BeginsWith(const aString: UnicodeString): Evaluation; overload;
      function Contains(const aSubString: UnicodeString): Evaluation; overload;
      function EndsWith(const aString: UnicodeString): Evaluation; overload;
      function Equals(const aExpected: UnicodeString): Evaluation; reintroduce; overload;

    public // UTF8Expectation
      function BeginsWith(const aString: UTF8String): Evaluation; overload;
      function Contains(const aSubString: UTF8String): Evaluation; overload;
      function EndsWith(const aString: UTF8String): Evaluation; overload;
      function Equals(const aExpected: UTF8String): Evaluation; reintroduce; overload;
    end;



  type
    TDatetimeExpectation = class(TExpectation, DatetimeTest,
                                               DatetimeExpectation)
    private
      fValue: TDateTime;
      function PartTest(const aSelector: TDateTimeUnit): TCardinalExpectation;
      property Value: TDateTime read fValue;

    public  // DateTimeTest
      function Expect(aValue: TDatetime): DateTimeExpectation;

    public  // DateTimeExpectations
      function Year: CardinalExpectation;
      function Month: CardinalExpectation;
      function Day: CardinalExpectation;
      function DayOfMonth: CardinalExpectation;
      function DayOfWeek: CardinalExpectation;
      function DayOfYear: CardinalExpectation;
      function WeekOfMonth: CardinalExpectation;
      function WeekOfYear: CardinalExpectation;
      function Hour: CardinalExpectation;
      function Minute: CardinalExpectation;
      function Second: CardinalExpectation;
      function Millisecond: CardinalExpectation;
      function LocalTime: DateTimeExpectation;
      function UTC: DateTimeExpectation;
      function Equals(const aValue: TDateTime): Evaluation; reintroduce;
      function HasDate(const aYear, aMonth, aDay: Word): Evaluation;
      function HasTime(const aHour, aMinute, aSecond, aMillisecond: Word): Evaluation;
      function IsBetween(const aEarliest, aLatest: TDateTime): Evaluation;
      function IsInRange(const aEarliest, aLatest: TDateTime): Evaluation;
      function IsAfter(const aValue: TDateTime): Evaluation;
      function IsBefore(const aValue: TDateTime): Evaluation;
      function IsNotAfter(const aValue: TDateTime): Evaluation;
      function IsNotBefore(const aValue: TDateTime): Evaluation;
    end;



    TDateExpectation = class(TDatetimeExpectation, DateTest,
                                                   DateExpectation)
    public // DateTest
      function Expect(aValue: TDate): DateExpectation;

    public // DateTestExpectations
      function Equals(const aValue: TDate): Evaluation; reintroduce; overload;
      function Equals(const  aYear, aMonth, aDay: Word): Evaluation; reintroduce; overload;
      function IsBetween(const aEarliest, aLatest: TDate): Evaluation;
      function IsInRange(const aEarliest, aLatest: TDate): Evaluation;
      function IsAfter(const aValue: TDate): Evaluation;
      function IsBefore(const aValue: TDate): Evaluation;
      function IsNotAfter(const aValue: TDate): Evaluation;
      function IsNotBefore(const aValue: TDate): Evaluation;
    end;


    TTimeExpectation = class(TDatetimeExpectation, TimeTest,
                                                   TimeExpectation)
    public // TimeTest
      function Expect(aValue: TTime): TimeExpectation;

    public // ITimeExpectations
      function Equals(const aValue: TTime): Evaluation; reintroduce; overload;
      function Equals(const aHour, aMinute: Word; const aSecond: Word = NO_SECONDS; const aMillisecond: Word = NO_MILLISECONDS): Evaluation; reintroduce; overload;
      function IsBetween(const aEarliest, aLatest: TTime): Evaluation;
      function IsInRange(const aEarliest, aLatest: TTime): Evaluation;
      function IsAfter(const aValue: TTime): Evaluation;
      function IsBefore(const aValue: TTime): Evaluation;
      function IsNotAfter(const aValue: TTime): Evaluation;
      function IsNotBefore(const aValue: TTime): Evaluation;
    end;







implementation

  uses
  { vcl: }
    Graphics,
    Math,
    TypInfo,
    Windows,
  { deltics: }
    DateUtils,
    Deltics.SysUtils;




  type
    TDoubleInspector = class(TInspector, DoubleInspector)
    public // IDoubleInspector
      procedure Value(aValue: Double);
    end;


    TCurrencyInspector = class(TInspector, CurrencyInspector)
    public // CurrencyInspector
      procedure Value(aValue: Currency);
    end;




  function AddrToStr(const aPointer: Pointer): UnicodeString;
  begin
    result := IfThen(Assigned(aPointer), '$' + IntToHex(NativeInt(aPointer), sizeof(Pointer) * 2),
                                         'NIL');
  end;





{ TColorTest ------------------------------------------------------------------------------------- }

  type
    TColorTest = class(TExpectation, ColorTest,
                                     ColorExpectation)
    private
      fValue: TColor;
      property Value: TColor read fValue;

    public  // ColorTest
      function Expect(aValue: TColor): ColorExpectation;

    public  // ColorExpectations
      function Equals(aExpected: TColor): Evaluation; reintroduce;
    end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TColorTest.Equals(aExpected: TColor): Evaluation;
  begin
    result := self;

    Expected    := ColorToString(aExpected);
    Actual      := ColorToString(Value);

    Description := Description + ' is ' + Expected;

    OK := (Value = aExpected)
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TColorTest.Expect(aValue: TColor): ColorExpectation;
  begin
    fValue := aValue;
    result := self;
  end;








{ TDatetimeExpectation --------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.PartTest(const aSelector: TDateTimeUnit): TCardinalExpectation;
  const
    PART: array[TDateTimeUnit] of UnicodeString = ('year', 'month', 'NOT USED (week)', 'day',
                                            'hour', 'minute', 'second', 'millisecond');
  var
    y, m, d: Word;
    h, s, z: Word;
    v: Word;
  begin
    v := 0;

    if aSelector in [dtYear, dtMonth, dtDay] then
      DecodeDate(Value, y, m, d)
    else
      DecodeTime(Value, h, m, s, z);

    case aSelector of
      dtYear        : v := y;
      dtMonth       : v := m;
      dtDay         : v := d;
      dtHour        : v := h;
      dtMinute      : v := m;
      dtSecond      : v := s;
      dtMillisecond : v := z;
    else
      ASSERT(FALSE, 'Unexpected/unknown date part specified');
    end;

    result := TCardinalExpectation.Create(Subject, v);
    result.SubjectName := SubjectName + ' (' + PART[aSelector] + ')';

    ReplaceInSubject(result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.Expect(aValue: TDateTime): DateTimeExpectation;
  begin
    fValue  := aValue;
    result  := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.Year: CardinalExpectation;        begin result := PartTest(dtYear); end;
  function TDatetimeExpectation.Month: CardinalExpectation;       begin result := PartTest(dtMonth); end;
  function TDatetimeExpectation.Day: CardinalExpectation;         begin result := PartTest(dtDay); end;
  function TDatetimeExpectation.DayOfMonth: CardinalExpectation;  begin result := PartTest(dtDay); end;
  function TDatetimeExpectation.Hour: CardinalExpectation;        begin result := PartTest(dtHour); end;
  function TDatetimeExpectation.Minute: CardinalExpectation;      begin result := PartTest(dtMinute); end;
  function TDatetimeExpectation.Second: CardinalExpectation;      begin result := PartTest(dtSecond); end;
  function TDatetimeExpectation.Millisecond: CardinalExpectation; begin result := PartTest(dtMillisecond); end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.WeekOfMonth: CardinalExpectation;
  var
    new: TCardinalExpectation;
  begin
    new := TCardinalExpectation.Create(Subject, WeekOfTheMonth(Value));
    new.SubjectName := SubjectName + ' (week of the month)';

    ReplaceInSubject(new);

    result := new;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.WeekOfYear: CardinalExpectation;
  var
    new: TCardinalExpectation;
  begin
    new := TCardinalExpectation.Create(Subject, WeekOfTheYear(Value));
    new.SubjectName := SubjectName + ' (week of the year)';

    ReplaceInSubject(new);

    result := new;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.DayOfWeek: CardinalExpectation;
  var
    replacement: TCardinalExpectation;
  begin
    replacement := TCardinalExpectation.Create(Subject, DayOfTheWeek(Value)); // NOTE: DayOfWeek is NOT IS0-8601 compliant
    replacement.SubjectName := SubjectName + ' (day of the week)';

    ReplaceInSubject(replacement);

    result := replacement;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.DayOfYear: CardinalExpectation;
  var
    replacement: TCardinalExpectation;
  begin
    replacement := TCardinalExpectation.Create(Subject, DayOfTheYear(Value));
    replacement.SubjectName := SubjectName + ' (day of the year)';

    ReplaceInSubject(replacement);

    result := replacement;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.IsBetween(const aEarliest, aLatest: TDateTime): Evaluation;
  var
    min, max: TDateTime;
  begin
    result := self;

    min := aEarliest;
    max := aLatest;

    Description := Description + ' is between ' + DateTimeToStr(min) + ' and ' + DateTimeToStr(max);
    Actual      := DateTimeToStr(fValue);

    OK := (fValue > min) and (fValue < max);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.IsInRange(const aEarliest, aLatest: TDateTime): Evaluation;
  var
    min, max: TDateTime;
  begin
    result := self;

    min := aEarliest;
    max := aLatest;

    Description := Description + ' is between ' + DateTimeToStr(min) + ' and ' + DateTimeToStr(max) + ' (inclusive)';
    Actual      := DateTimeToStr(fValue);

    OK := (fValue >= min) and (fValue <= max);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.IsBefore(const aValue: TDateTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is before ' + DateTimeToStr(aValue);
    Actual      := DateTimeToStr(fValue);

    OK := (fValue < aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.Equals(const aValue: TDateTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is ' + DateTimeToStr(aValue);
    Expected    := DateTimeToStr(aValue);
    Actual      := DateTimeToStr(fValue);

    OK := Abs(DateDiff(fValue, aValue)) < (1 / SecsPerDay);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.HasDate(const aYear, aMonth, aDay: Word): Evaluation;
  var
    y, m, d: Word;
  begin
    result := self;

    Description := Description + ' has date {expected}';
    Expected    := DateToStr(EncodeDate(aYear, aMonth, aDay));
    Actual      := DateToStr(fValue);

    DecodeDate(fValue, y, m, d);

    OK := (aYear = y) and (aMonth = m) and (aDay = d);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.HasTime(const aHour, aMinute, aSecond, aMillisecond: Word): Evaluation;
  var
    h, m, s, z: Word;
    fmt: TFormatSettings;
    noMilliseconds: Boolean;
  begin
    result := self;

    GetLocaleFormatSettings(GetThreadLocale, fmt);

    z := aMillisecond;
    noMilliseconds := (z = NO_MILLISECONDS);
    if noMilliseconds then
    begin
      z := 0;
      fmt.LongTimeFormat := 'HH:MM:SS';
    end
    else
      fmt.LongTimeFormat := 'HH:MM:SS.ZZZ';

    Description := Description + ' has time {expected}';
    Expected    := TimeToStr(EncodeTime(aHour, aMinute, aSecond, z), fmt);
    Actual      := TimeToStr(fValue, fmt);

    DecodeTime(fValue, h, m, s, z);

    OK := (aHour =  h) and (aMinute = m) and (aSecond = s)
       and (noMilliseconds or (aMillisecond = z));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.IsAfter(const aValue: TDateTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is after ' + DateTimeToStr(aValue);
    Actual      := DateTimeToStr(fValue);

    OK := (fValue > aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.LocalTime: DateTimeExpectation;
  begin
    fValue := UTCToLocal(fValue);
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.UTC: DateTimeExpectation;
  begin
    fValue := LocalToUTC(fValue);
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.IsNotBefore(const aValue: TDateTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is not before ' + DateTimeToStr(aValue);
    Actual      := DateTimeToStr(fValue);

    OK := (fValue >= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDatetimeExpectation.IsNotAfter(const aValue: TDateTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is not after ' + DateTimeToStr(aValue);
    Actual      := DateTimeToStr(fValue);

    OK := (fValue <= aValue);
  end;













{ TDateExpectation ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.Expect(aValue: TDate): DateExpectation;
  begin
    fValue := aValue;
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.Equals(const aValue: TDate): Evaluation;
  begin
    result := self;

    Description := Description + ' is ' + DateToStr(aValue);
    Expected    := DateToStr(aValue);
    Actual      := DateToStr(fValue);

    OK := Trunc(fValue) = Trunc(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.Equals(const aYear, aMonth, aDay: Word): Evaluation;
  begin
    result  := Equals(TDate(EncodeDate(aYear, aMonth, aDay)));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.IsBetween(const aEarliest, aLatest: TDate): Evaluation;
  begin
    result := self;

    Description := Description + ' is between ' + DateToStr(aEarliest) + ' and ' + DateToStr(aLatest);
    Actual      := DateToStr(fValue);

    OK := (Trunc(fValue) > Trunc(aEarliest))
      and (Trunc(fValue) < Trunc(aLatest));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.IsInRange(const aEarliest, aLatest: TDate): Evaluation;
  begin
    result := self;

    Description := Description + ' is between ' + DateToStr(aEarliest) + ' and ' + DateToStr(aLatest) + ' (inclusive)';
    Actual      := DateToStr(fValue);

    OK := (Trunc(fValue) >= Trunc(aEarliest))
      and (Trunc(fValue) <= Trunc(aLatest));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.IsAfter(const aValue: TDate): Evaluation;
  begin
    result := self;

    Description := Description + ' is after ' + DateToStr(aValue);
    Actual      := DateToStr(fValue);

    OK := (Trunc(fValue) > Trunc(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.IsBefore(const aValue: TDate): Evaluation;
  begin
    result := self;

    Description := Description + ' is before ' + DateToStr(aValue);
    Actual      := DateToStr(fValue);

    OK := (Trunc(fValue) < Trunc(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.IsNotAfter(const aValue: TDate): Evaluation;
  begin
    result := self;

    Description := Description + ' is not after ' + DateToStr(aValue);
    Actual      := DateToStr(fValue);

    OK := (Trunc(fValue) <= Trunc(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDateExpectation.IsNotBefore(const aValue: TDate): Evaluation;
  begin
    result := self;

    Description := Description + ' is not before ' + DateToStr(aValue);
    Actual      := DateToStr(fValue);

    OK := (Trunc(fValue) >= Trunc(aValue));
  end;









{ TTimeExpectation -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.Expect(aValue: TTime): TimeExpectation;
  begin
    fValue := aValue;
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.Equals(const aValue: TTime): Evaluation;
  var
    fh, fm, fs, fz: Word;
    vh, vm, vs, vz: Word;
  begin
    result := self;

    Description := Description + ' is ' + TimeToStr(aValue);
    Expected    := TimeToStr(aValue);
    Actual      := TimeToStr(fValue);

    DecodeTime(fValue, fh, fm, fs, fz);
    DecodeTime(aValue, vh, vm, vs, vz);

    OK := (fh = vh) and (fm = vm) and (fs = vs) and (fz = vz);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.Equals(const aHour, aMinute, aSecond, aMillisecond: Word): Evaluation;
  var
    h, m, s, ms: Word;
  begin
    result := self;

    DecodeTime(fValue, h, m, s, ms);
    if (aSecond = NO_SECONDS) then
    begin
      Description := Description + ' is ' + Format('%.2d:%2.d', [aHour, aMinute]);
      Actual      := Format('%.2d:%2.d', [h, m]);
      OK := (h = aHour) and (m = aMinute);
    end
    else if (aMilliSecond = NO_SECONDS) then
    begin
      Description := Description + ' is ' + Format('%.2d:%2.d:%.2d', [aHour, aMinute, aSecond]);
      Actual      := Format('%.2d:%2.d:%.2d', [h, m, s]);
      OK := (h = aHour) and (m = aMinute) and (s = aSecond);
    end
    else
      result := Equals(TTime(EncodeTime(aHour, aMinute, aSecond, aMillisecond)));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.IsBetween(const aEarliest, aLatest: TTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is between ' + TimeToStr(aEarliest) + ' and ' + TimeToStr(aLatest);
    Actual      := TimeToStr(fValue);

    OK := (Frac(fValue) > Frac(aEarliest))
      and (Frac(fValue) < Frac(aLatest));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.IsInRange(const aEarliest, aLatest: TTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is between ' + TimeToStr(aEarliest) + ' and ' + TimeToStr(aLatest) + ' (inclusive)';
    Actual      := TimeToStr(fValue);

    OK := (Frac(fValue) >= Frac(aEarliest))
      and (Frac(fValue) <= Frac(aLatest));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.IsAfter(const aValue: TTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is after ' + TimeToStr(aValue);
    Actual      := TimeToStr(fValue);

    OK := (Frac(fValue) > Frac(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.IsBefore(const aValue: TTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is before ' + TimeToStr(aValue);
    Actual      := TimeToStr(fValue);

    OK := (Frac(fValue) < Frac(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.IsNotAfter(const aValue: TTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is not after ' + TimeToStr(aValue);
    Actual      := TimeToStr(fValue);

    OK := (Frac(fValue) <= Frac(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTimeExpectation.IsNotBefore(const aValue: TTime): Evaluation;
  begin
    result := self;

    Description := Description + ' is not before ' + TimeToStr(aValue);
    Actual      := TimeToStr(fValue);

    OK := (Frac(fValue) >= Frac(aValue));
  end;










{ TEnumTest -------------------------------------------------------------------------------------- }

  type
    TEnumTest = class(TExpectation, EnumTest,
                                    SpecificEnumTest,
                                    EnumExpectations)
    private
      fEnum: PTypeInfo;
      fValue: Cardinal;
      property Value: Cardinal read fValue;

    public // EnumTest
      function ForEnum(aTypeInfo: PTypeInfo): SpecificEnumTest;

    public // SpecificEnumTest
//      function Expect(aValue: Cardinal): EnumExpectations;
      function Expect(const aValue): EnumExpectations;

    public // EnumExpectations
      function Equals(aExpected: Cardinal): Evaluation; reintroduce;
    end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TEnumTest.ForEnum(aTypeInfo: PTypeInfo): SpecificEnumTest;
  begin
    fEnum   := aTypeInfo;
    result  := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TEnumTest.Expect(const aValue): EnumExpectations;
  begin
    fValue := Cardinal(aValue);
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TEnumTest.Equals(aExpected: Cardinal): Evaluation;
  begin
    result := self;

    Expected    := GetEnumName(fEnum, Cardinal(aExpected));
    Actual      := GetEnumName(fEnum, Value);

    Description := Description + ' is ' + Expected;

    OK := (Value = Cardinal(aExpected))
  end;











{ TBooleanExpectation ---------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TBooleanExpectation.Create(const aSubject: TTest;
                                         const aValue: Boolean);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := IfThen(Value, 'TRUE', 'FALSE');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBooleanExpectation.Equals(const aExpected: Boolean): Evaluation;
  begin
    result := self;

    Expected    := IfThen(aExpected,  'TRUE', 'FALSE');
    Description := 'is ' + Expected;

    OK := (fValue = aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBooleanExpectation.IsFALSE: Evaluation;
  begin
    result := Equals(FALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBooleanExpectation.IsTRUE: Evaluation;
  begin
    result := Equals(TRUE);
  end;






{ TApproximateExpectation ------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TApproximateExpectation.Create(const aSubject: TTest;
                                             const aValue: Currency;
                                             const aDPs: Integer);
  begin
    inherited Create(aSubject);

    fDPs      := aDPs;
    fOriginal := aValue;
    fValue    := ToRequiredDPs(aValue);

    Actual    := ToString(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.ToRequiredDPs(const aValue: Currency): Currency;
  var
    m: Extended;
    v: Extended;
  begin
    m := Power(10, fDPs);
    v := Trunc(aValue * m) / m;

    result  := Trunc(v * m) / m;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.ToString(const aValue: Currency): UnicodeString;
  begin
    result := FloatToStrF(ToRequiredDPs(aValue), ffFixed, MaxInt, fDPs);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.Between(const aMin, aMax: Currency): Evaluation;
  var
    mi, ma: Currency;
  begin
    result := self;

    mi := ToRequiredDPs(aMin);
    ma := ToRequiredDPs(aMax);

    fMinimum := Min(mi, ma);
    fMaximum := Max(mi, ma);

    Expected      := '{minimum}..{maximum}';
    Description   := 'between {expected}';

    OK := (Value > fMinimum) and (Value < fMaximum);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.Equals(const aExpected: Currency): Evaluation;
  begin
    result := self;

    Expected    := ToString(aExpected);
    Description := '= {expected}';

    OK := (Value = ToRequiredDPs(aExpected));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.GreaterThan(const aValue: Currency): Evaluation;
  begin
    result := self;

    Expected    := ToString(aValue);
    Description := '> {expected}';

    OK := (Value > ToRequiredDPs(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.InRange(const aMin, aMax: Currency): Evaluation;
  var
    mi, ma: Currency;
  begin
    result := self;

    mi := ToRequiredDPs(aMin);
    ma := ToRequiredDPs(aMax);

    fMinimum := Min(mi, ma);
    fMaximum := Max(mi, ma);

    Expected      := '{minimum}..{maximum}';
    Description   := 'between {expected} (inclusive)';

    OK := (Value >= fMinimum) and (Value <= fMaximum);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.LessThan(const aValue: Currency): Evaluation;
  begin
    result := self;

    Expected    := ToString(aValue);
    Description := '< {expected}';

    OK := (Value < ToRequiredDPs(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.NotGreaterThan(const aValue: Currency): Evaluation;
  begin
    result := self;

    Expected    := ToString(aValue);
    Description := '<= {expected}';

    OK := (Value <= ToRequiredDPs(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.NotLessThan(const aValue: Currency): Evaluation;
  begin
    result := self;

    Expected    := ToString(aValue);
    Description := '>= {expected}';

    OK := (Value >= ToRequiredDPs(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApproximateExpectation.DoReplaceTokens(var aString: UnicodeString): Boolean;
  begin
    result := inherited DoReplaceTokens(aString);
    result := DoReplaceToken(aString, '{minimum}', ToString(fMinimum)) or result;
    result := DoReplaceToken(aString, '{maximum}', ToString(fMaximum)) or result;
  end;










{ TSingleExpectation ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TSingleExpectation.Create(const aSubject: TTest;
                                        const aValue: Single);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := FloatToStrF(aValue, ffGeneral, MaxInt, MaxInt);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.Between(const aMin, aMax: Single): Evaluation;
  begin
    result := self;

    fMinimum  := Min(aMin, aMax);
    fMaximum  := Max(aMin, aMax);

    Expected    := '{minimum}..{maximum}';
    Description := 'between {expected}';

    OK := (Value > aMin) and (Value < aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.Equals(const aExpected: Single): Evaluation;
  begin
    result := self;

    Description := '= {expected}';
    Expected    := FloatToStr(aExpected);

    OK := (Value = aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.GreaterThan(const aValue: Single): Evaluation;
  begin
    result := self;

    Description := '> {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value > aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.InRange(const aMin, aMax: Single): Evaluation;
  begin
    result := self;

    fMinimum  := Min(aMin, aMax);
    fMaximum  := Max(aMin, aMax);

    Expected    := '{minimum}..{maximum}';
    Description := 'between {expected} (inclusive)';

    OK := (Value >= aMin) and (Value <= aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.LessThan(const aValue: Single): Evaluation;
  begin
    result := self;

    Description := '< {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value < aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.NotGreaterThan(const aValue: Single): Evaluation;
  begin
    result := self;

    Description := '<= {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value <= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.NotLessThan(const aValue: Single): Evaluation;
  begin
    result := self;

    Description := '>= {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value >= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.DoReplaceTokens(var aString: UnicodeString): Boolean;
  begin
    result := inherited DoReplaceTokens(aString);
    result := DoReplaceToken(aString, '{minimum}', FloatToStr(fMinimum)) or result;
    result := DoReplaceToken(aString, '{maximum}', FloatToStr(fMaximum)) or result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSingleExpectation.ToDPs(const aDPs: Integer): ApproximateExpectation;
  {
    Replaces the current SingleExpectation with an ApproximateExpectation.
  }
  var
    approx: TApproximateExpectation;
  begin
    approx := TApproximateExpectation.Create(Subject, Value, aDPs);
    approx.SubjectName := SubjectName + ' (to ' + IntToStr(aDPs) + ' dps)';

    ReplaceInSubject(approx);

    result := approx;
  end;




{ TDoubleExpectation ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDoubleExpectation.Create(const aSubject: TTest;
                                        const aValue: Double);
  begin
    inherited Create(aSubject);
    Expect(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.Expect(aValue: Double): DoubleExpectation;
  begin
    result  := self;
    fValue  := aValue;
    Actual  := FloatToStrF(aValue, ffGeneral, MaxInt, MaxInt);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.Between(const aMin, aMax: Double): Evaluation;
  begin
    result := self;

    fMinimum  := Min(aMin, aMax);
    fMaximum  := Max(aMin, aMax);

    Expected    := '{minimum}..{maximum}';
    Description := 'between {expected}';

    OK := (Value > aMin) and (Value < aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.Equals(const aExpected: Double): Evaluation;
  begin
    result := self;

    Description := '= {expected}';
    Expected    := FloatToStr(aExpected);

    OK := (Value = aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.GreaterThan(const aValue: Double): Evaluation;
  begin
    result := self;

    Description := '> {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value > aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.InRange(const aMin, aMax: Double): Evaluation;
  begin
    result := self;

    fMinimum  := Min(aMin, aMax);
    fMaximum  := Max(aMin, aMax);

    Expected    := '{minimum}..{maximum}';
    Description := 'between {expected} (inclusive)';

    OK := (Value >= aMin) and (Value <= aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.LessThan(const aValue: Double): Evaluation;
  begin
    result := self;

    Description := '< {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value < aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.NotGreaterThan(const aValue: Double): Evaluation;
  begin
    result := self;

    Description := '<= {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value <= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.NotLessThan(const aValue: Double): Evaluation;
  begin
    result := self;

    Description := '>= {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value >= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.DoReplaceTokens(var aString: UnicodeString): Boolean;
  begin
    result := inherited DoReplaceTokens(aString);
    result := DoReplaceToken(aString, '{minimum}', FloatToStr(fMinimum)) or result;
    result := DoReplaceToken(aString, '{maximum}', FloatToStr(fMaximum)) or result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDoubleExpectation.ToDPs(const aDPs: Integer): ApproximateExpectation;
  {
    Replaces the current DoubleExpectation with an ApproximateExpectation.
  }
  var
    approx: TApproximateExpectation;
  begin
    approx := TApproximateExpectation.Create(Subject, Value, aDPs);
    approx.SubjectName := SubjectName + ' (to ' + IntToStr(aDPs) + ' dps)';

    ReplaceInSubject(approx);

    result := approx;
  end;





{ TExtendedExpectation ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TExtendedExpectation.Create(const aSubject: TTest;
                                        const aValue: Extended);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := FloatToStrF(aValue, ffGeneral, MaxInt, MaxInt);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.Between(const aMin, aMax: Extended): Evaluation;
  begin
    result := self;

    fMinimum  := Min(aMin, aMax);
    fMaximum  := Max(aMin, aMax);

    Expected    := '{minimum}..{maximum}';
    Description := 'between {expected}';

    OK := (Value > aMin) and (Value < aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.Equals(const aExpected: Extended): Evaluation;
  begin
    result := self;

    Description := '= {expected}';
    Expected    := FloatToStr(aExpected);

    OK := (Value = aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.GreaterThan(const aValue: Extended): Evaluation;
  begin
    result := self;

    Description := '> {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value > aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.InRange(const aMin, aMax: Extended): Evaluation;
  begin
    result := self;

    fMinimum  := Min(aMin, aMax);
    fMaximum  := Max(aMin, aMax);

    Expected    := '{minimum}..{maximum}';
    Description := 'between {expected} (inclusive)';

    OK := (Value >= aMin) and (Value <= aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.LessThan(const aValue: Extended): Evaluation;
  begin
    result := self;

    Description := '< {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value < aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.NotGreaterThan(const aValue: Extended): Evaluation;
  begin
    result := self;

    Description := '<= {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value <= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.NotLessThan(const aValue: Extended): Evaluation;
  begin
    result := self;

    Description := '>= {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value >= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.DoReplaceTokens(var aString: UnicodeString): Boolean;
  begin
    result := inherited DoReplaceTokens(aString);
    result := DoReplaceToken(aString, '{minimum}', FloatToStr(fMinimum)) or result;
    result := DoReplaceToken(aString, '{maximum}', FloatToStr(fMaximum)) or result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExtendedExpectation.ToDPs(const aDPs: Integer): ApproximateExpectation;
  {
    Replaces the current ExtendedExpectation with an ApproximateExpectation.
  }
  var
    approx: TApproximateExpectation;
  begin
    approx := TApproximateExpectation.Create(Subject, Value, aDPs);
    approx.SubjectName := SubjectName + ' (to ' + IntToStr(aDPs) + ' dps)';

    ReplaceInSubject(approx);

    result := approx;
  end;









{ TCurrencyExpectation ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCurrencyExpectation.Create(const aSubject: TTest;
                                          const aValue: Currency);
  begin
    inherited Create(aSubject);
    Expect(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.Expect(aValue: Currency): CurrencyExpectation;
  begin
    result  := self;
    fValue  := aValue;
    Actual  := FloatToStrF(aValue, ffGeneral, MaxInt, MaxInt);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.Between(const aMin, aMax: Currency): Evaluation;
  begin
    result := self;

    fMinimum  := Min(aMin, aMax);
    fMaximum  := Max(aMin, aMax);

    Expected    := '{minimum}..{maximum}';
    Description := 'between {expected}';

    OK := (Value > aMin) and (Value < aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.Equals(const aExpected: Currency): Evaluation;
  begin
    result := self;

    Description := '= {expected}';
    Expected    := FloatToStr(aExpected);

    OK := (Value = aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.GreaterThan(const aValue: Currency): Evaluation;
  begin
    result := self;

    Description := '> {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value > aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.InRange(const aMin, aMax: Currency): Evaluation;
  begin
    result := self;

    fMinimum  := Min(aMin, aMax);
    fMaximum  := Max(aMin, aMax);

    Expected    := '{minimum}..{maximum}';
    Description := 'between {expected} (inclusive)';

    OK := (Value >= aMin) and (Value <= aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.LessThan(const aValue: Currency): Evaluation;
  begin
    result := self;

    Description := '< {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value < aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.NotGreaterThan(const aValue: Currency): Evaluation;
  begin
    result := self;

    Description := '<= {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value <= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.NotLessThan(const aValue: Currency): Evaluation;
  begin
    result := self;

    Description := '>= {expected}';
    Expected    := FloatToStr(aValue);

    OK := (Value >= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.DoReplaceTokens(var aString: UnicodeString): Boolean;
  begin
    result := inherited DoReplaceTokens(aString);
    result := DoReplaceToken(aString, '{minimum}', FloatToStr(fMinimum)) or result;
    result := DoReplaceToken(aString, '{maximum}', FloatToStr(fMaximum)) or result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCurrencyExpectation.ToDPs(const aDPs: Integer): ApproximateExpectation;
  {
    Replaces the current CurrencyExpectation with an ApproximateExpectation.
  }
  var
    approx: TApproximateExpectation;
  begin
    approx := TApproximateExpectation.Create(Subject, Value, aDPs);
    approx.SubjectName := SubjectName + ' (to ' + IntToStr(aDPs) + ' dps)';

    ReplaceInSubject(approx);

    result := approx;
  end;








{ TCardinalExpectation ---------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCardinalExpectation.Create(const aSubject: TTest;
                                         const aValue: Cardinal);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := IntToStr(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCardinalExpectation.DoReplaceTokens(var aString: UnicodeString): Boolean;
  begin
    result := inherited DoReplaceTokens(aString);
    result := DoReplaceToken(aString, '{minimum}', IntToStr(fMinimum)) or result;
    result := DoReplaceToken(aString, '{maximum}', IntToStr(fMaximum)) or result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCardinalExpectation.Between(const aMin, aMax: Cardinal): Evaluation;
  begin
    result := self;

    fMinimum  := Deltics.SysUtils.Min(aMin, aMax);
    fMaximum  := Deltics.SysUtils.Max(aMin, aMax);

    Description := 'between {expected}';
    Expected    := '{minimum}..{maximum}';

    OK := (Value > aMin) and (Value < aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCardinalExpectation.Equals(const aExpected: Cardinal): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aExpected);
    Description := '= {expected}';

    OK := (Value = aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCardinalExpectation.GreaterThan(const aValue: Cardinal): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aValue);
    Description := '> {expected}';

    OK := (Value > aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCardinalExpectation.InRange(const aMin, aMax: Cardinal): Evaluation;
  begin
    result := self;

    fMinimum  := Deltics.SysUtils.Min(aMin, aMax);
    fMaximum  := Deltics.SysUtils.Max(aMin, aMax);

    Description := 'between {expected} (inclusive)';
    Expected    := '{minimum}..{maximum}';

    OK := (Value >= aMin) and (Value <= aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCardinalExpectation.LessThan(const aValue: Cardinal): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aValue);
    Description := '< {expected}';

    OK := (Value < aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCardinalExpectation.NotGreaterThan(const aValue: Cardinal): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aValue);
    Description := '<= {expected}';

    OK := (Value <= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCardinalExpectation.NotLessThan(const aValue: Cardinal): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aValue);
    Description := '>= {expected}';

    OK := (Value >= aValue);
  end;











{ TIntegerExpectation ---------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TIntegerExpectation.Create(const aSubject: TTest;
                                         const aValue: Int64);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := IntToStr(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TIntegerExpectation.DoReplaceTokens(var aString: UnicodeString): Boolean;
  begin
    result := inherited DoReplaceTokens(aString);
    result := DoReplaceToken(aString, '{minimum}', IntToStr(fMinimum)) or result;
    result := DoReplaceToken(aString, '{maximum}', IntToStr(fMaximum)) or result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TIntegerExpectation.Between(const aMin, aMax: Int64): Evaluation;
  begin
    result := self;

  {$ifdef DELPHI2006_OR_LATER}
    fMinimum  := Min(aMin, aMax) + 1;
    fMaximum  := Max(aMin, aMax) - 1;
  {$else}
    fMinimum  := Min64(aMin, aMax) + 1;
    fMaximum  := Max64(aMin, aMax) - 1;
  {$endif}

    Description := 'between {expected}';
    Expected    := '{minimum}..{maximum}';

    OK := (Value > aMin) and (Value < aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TIntegerExpectation.Equals(const aExpected: Int64): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aExpected);
    Description := '= {expected}';

    OK := (Value = aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TIntegerExpectation.GreaterThan(const aValue: Int64): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aValue);
    Description := '> {expected}';

    OK := (Value > aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TIntegerExpectation.InRange(const aMin, aMax: Int64): Evaluation;
  begin
    result := self;

    fMinimum  := Min64(aMin, aMax);
    fMaximum  := Max64(aMin, aMax);

    Description := 'between {expected} (inclusive)';
    Expected    := '{minimum}..{maximum}';

    OK := (Value >= aMin) and (Value <= aMax);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TIntegerExpectation.LessThan(const aValue: Int64): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aValue);
    Description := '< {expected}';

    OK := (Value < aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TIntegerExpectation.NotGreaterThan(const aValue: Int64): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aValue);
    Description := '<= {expected}';

    OK := (Value <= aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TIntegerExpectation.NotLessThan(const aValue: Int64): Evaluation;
  begin
    result := self;

    Expected    := IntToStr(aValue);
    Description := '>= {expected}';

    OK := (Value >= aValue);
  end;









{ TInterfaceExpectation -------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TInterfaceExpectation.Create(const aSubject: TTest;
                                           const aValue: IInterface);
  begin
    inherited Create(aSubject);

    fValue  := TWeakInterface.Create(aValue);
    Actual  := IfThen(Assigned(aValue), 'ASSIGNED', 'NIL');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TInterfaceExpectation.Destroy;
  begin
    fValue.Free;

    inherited Destroy;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfaceExpectation.Equals(const aExpected: IInterface): Evaluation;
  begin
    result := self;

    Description := '= {expected}';
    Expected    := AddrToStr(Pointer(aExpected));

    OK := ((Value as IUnknown) = (aExpected as IUnknown));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfaceExpectation.get_Value: IInterface;
  begin
    result := fValue as IInterface;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfaceExpectation.IsAssigned: Evaluation;
  begin
    result := self;

    Description := 'is assigned';
    Expected    := 'ASSIGNED';

    OK := Assigned(Value);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfaceExpectation.IsNIL: Evaluation;
  begin
    result := self;

    Description := 'is not assigned';
    Expected    := 'NIL';

    OK := NOT Assigned(Value);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInterfaceExpectation.Supports(const aIID: TGUID;
                                          const aName: UnicodeString): Evaluation;
  begin
    result := self;

    Description := 'interface supports ' + IfThen(aName = '', GUIDToString(aIID), aName);

    OK := Assigned(Value) and SysUtils.Supports(Value, aIID);
  end;





{ TGUIDExpectation ---------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TGUIDExpectation.Create(const aSubject: TTest;
                                      const aValue: TGUID);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := GUIDToString(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TGUIDExpectation.Equals(const aValue: TGUID): Evaluation;
  begin
    result := self;

    Expected    := GUIDToString(aValue);
    Description := '= {expected}';

    OK := SameGUID(aValue, Value);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TGUIDExpectation.IsNull: Evaluation;
  begin
    result := self;

    Description := 'is null';
    Expected    := GUIDToString(NullGUID);

    OK := SameGUID(Value, NullGUID);
  end;











{ TPointerExpectation ---------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TPointerExpectation.Create(const aSubject: TTest;
                                         const aValue: Pointer);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := AddrToStr(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPointerExpectation.Equals(const aPointer: Pointer): Evaluation;
  begin
    result := self;

    Expected    := AddrToStr(aPointer);
    Description := '= {expected}';

    OK := (aPointer = Value);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPointerExpectation.Equals(const aPointer: Pointer;
                                      const aBytes: Integer): Evaluation;
  begin
    result := self;

    Description := 'points to same ' + IntToStr(aBytes) + ' bytes at ' + AddrToStr(aPointer);

    if Assigned(Value) then
      OK := CompareMem(Value, aPointer, aBytes);
    { TODO: If not assigned...}
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPointerExpectation.IsAssigned: Evaluation;
  begin
    result := self;

    Description := 'is {expected}';
    Expected    := 'assigned';

    OK := Assigned(Value);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPointerExpectation.IsNIL: Evaluation;
  begin
    result := self;

    Description := 'is {expected}';
    Expected    := 'NIL';

    OK := NOT Assigned(Value);
  end;







{ TObjectExpectation ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TObjectExpectation.Create(const aSubject: TTest;
                                        const aValue: TObject);
  begin
    inherited Create(aSubject, Pointer(aValue));

    fValue  := aValue;

    if Assigned(Value) then
      Actual  := Value.ClassName + '(' + AddrToStr(aValue) + ')'
    else
      Actual := 'NIL';
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TObjectExpectation.Equals(const aRef: TObject): Evaluation;
  begin
    result := self;

    OK := (Value = aRef);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TObjectExpectation.get_Value: TObject;
  begin
    result := TObject(inherited Value);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TObjectExpectation.Supports(const aIID: TGUID;
                                       const aName: UnicodeString): Evaluation;
  begin
    result := self;

    if (aName = '') then
      Description := 'implements IID ' + GUIDToString(aIID)
    else
      Description := 'implements ' + aName;

    OK := Assigned(Value) and SysUtils.Supports(Value, aIID);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TObjectExpectation.IsAssigned: Evaluation;
  begin
    result := self;

    Description := 'is {expected}';
    Expected    := 'assigned';

    OK := Assigned(Value);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TObjectExpectation.IsInstanceOf(const aClass: TClass): Evaluation;
  begin
    result := self;

    Description := 'is instance of {expected}';
    Expected    := aClass.ClassName;

    if Assigned(Value) then
      Actual  := Value.ClassName
    else
      Actual  := 'NIL';

    OK := Assigned(Value) and (Value.ClassType = aClass);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TObjectExpectation.IsNIL: Evaluation;
  begin
    result := self;

    Description := 'is {expected}';
    Expected    := 'NIL';

    OK := NOT Assigned(Value);
  end;










{ TStringExpectation ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TStringExpectation.Create(const aSubject: TTest;
                                        const aValue: UnicodeString);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := '''' + Value + '''';
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.Expect(aValue: UTF8String): UTF8Expectation;
  begin
    fValue := WIDE.FromUTF8(aValue);
    Actual := '''' + Value + '''';
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.CaseInsensitive: TextExpectation;
  {
    Replaces the current StringExpectation (case sensitive) with a
     TextExpectation (case INsensitive).
  }
  var
    txt: TTextExpectation;
  begin
    txt := TTextExpectation.Create(Subject, Value);
    ReplaceInSubject(txt);

    result := txt;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.Length: IntegerExpectation;
  var
    int: TIntegerExpectation;
  begin
    int := TIntegerExpectation.Create(Subject, System.Length(Value));
    int.SubjectName := SubjectName + ' (length)';

    ReplaceInSubject(int);

    result := int;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.IsEmpty: Evaluation;
  begin
    result := self;

    Description := 'is empty';
    Expected    := '''''';

    OK := (Value = '');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.IsLowercase: Evaluation;
  var
    i: Integer;
    bOK: Boolean;
  begin
    result := self;

    Description := 'is lowercase';
    Expected    := '''' + Lowercase(Value) + '''';

    bOK := TRUE;
    for i := 1 to Pred(System.Length(Value)) do
    begin
      if NOT IsCharAlphaW(Value[i]) then
        CONTINUE;

      bOK := IsCharLowerW(Value[i]);
      if NOT bOK then
        BREAK;
    end;

    OK := bOK;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.IsUppercase: Evaluation;
  var
    i: Integer;
    bOK: Boolean;
  begin
    result := self;

    Description := 'is uppercase';
    Expected    := '''' + Uppercase(Value) + '''';

    bOK := TRUE;
    for i := 1 to Pred(System.Length(Value)) do
    begin
      if NOT IsCharAlphaW(Value[i]) then
        CONTINUE;

      bOK := IsCharUpperW(Value[i]);
      if NOT bOK then
        BREAK;
    end;

    OK := bOK;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.BeginsWith(const aString: ANSIString): Evaluation;
  begin
    result := BeginsWith(WIDE.FromANSI(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.Contains(const aSubString: ANSIString): Evaluation;
  begin
    result := Contains(WIDE.FromANSI(aSubString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.EndsWith(const aString: ANSIString): Evaluation;
  begin
    result := EndsWith(WIDE.FromANSI(aString));
  end;


  {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.Equals(const aExpected: ANSIString): Evaluation;
  begin
    result := Equals(WIDE.FromANSI(aExpected));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.BeginsWith(const aString: UnicodeString): Evaluation;
  var
    lead: UnicodeString;
  begin
    result := self;

    Description := 'begins with {expected}';
    Expected    := '''' + aString + '...''';

    if System.Length(Value) > System.Length(aString) then
      Actual := '''' + Copy(Value, 1, System.Length(aString)) + '...''';

    if (System.Length(Value) >= System.Length(aString)) then
    begin
      lead  := Copy(Value, 1, System.Length(aString));

      OK := (lead = aString);
    end
    else
      OK := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.Contains(const aSubString: UnicodeString): Evaluation;
  begin
    result := self;

    Description := 'contains {expected}';
    Expected    := '''...' + aSubString + '...''' ;

    OK := (Pos(aSubString, Value) > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.EndsWith(const aString: UnicodeString): Evaluation;
  var
    tail: UnicodeString;
  begin
    result := self;

    Description := 'ends with {expected}';
    Expected    := '''...' + aString + '''';

    if System.Length(aString) <= System.Length(Value) then
      Actual := '''...' + Copy(Value, System.Length(Value) - System.Length(aString) + 1, System.Length(aString)) + '''';

    if (System.Length(Value) >= System.Length(aString)) then
    begin
      tail := Copy(Value, (System.Length(Value) - System.Length(aString)) + 1, System.Length(aString));

      OK := (tail = aString);
    end
    else
      OK := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.Equals(const aExpected: UnicodeString): Evaluation;
  begin
    result := self;

    Description := '= {expected}';
    Expected    := '''' + aExpected + '''';

    OK := (Value = aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.UTF8CaseInsensitive: UTF8TextExpectation;
  begin
    result := CaseInsensitive as UTF8TextExpectation;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.BeginsWith(const aString: UTF8String): Evaluation;
  begin
    result := BeginsWith(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.Contains(const aSubString: UTF8String): Evaluation;
  begin
    result := Contains(WIDE.FromUTF8(aSubString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.EndsWith(const aString: UTF8String): Evaluation;
  begin
    result := EndsWith(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringExpectation.Equals(const aExpected: UTF8String): Evaluation;
  begin
    result := Equals(WIDE.FromUTF8(aExpected));
  end;












{ TTextExpectation ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTextExpectation.Create(const aSubject: TTest;
                                      const aValue: UnicodeString);
  begin
    inherited Create(aSubject);

    fValue  := aValue;
    Actual  := '''' + aValue + '''';
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.BeginsWith(const aString: ANSIString): Evaluation;
  begin
    result := BeginsWith(WIDE.FromANSI(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.Contains(const aSubString: ANSIString): Evaluation;
  begin
    result := Contains(WIDE.FromANSI(aSubString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.EndsWith(const aString: ANSIString): Evaluation;
  begin
    result := EndsWith(WIDE.FromANSI(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.Equals(const aExpected: ANSIString): Evaluation;
  begin
    result := Equals(WIDE.FromANSI(aExpected));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.BeginsWith(const aString: UnicodeString): Evaluation;
  const
    ELIDE: array[FALSE..TRUE] of String = ('', '...');
  begin
    result := self;

    Description := 'begins with text {expected}';
    Expected    := '''' + aString + '...''';

    Actual := '''' + Copy(Value, 1, Length(aString)) + ELIDE[Length(Value) > Length(aString)] + '''';

    OK := WIDE.BeginsWithText(Value, aString);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.Contains(const aSubString: UnicodeString): Evaluation;
  begin
    result := self;

    Description := 'contains text {expected}';
    Expected    := '''...' + aSubString + '...''';

    OK := (Pos(ANSILowerCase(aSubString), ANSILowerCase(Value)) > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.EndsWith(const aString: UnicodeString): Evaluation;
  var
    tail: UnicodeString;
  begin
    result := self;

    Description := 'ends with text {expected}';
    Expected    := '''...' + aString + '''';

    if Length(aString) <= Length(Value) then
      Actual := '''...' + Copy(Value, Length(Value) - Length(aString) + 1, Length(aString)) + ''''
    else
      Actual := '''' + Value + '''';

    if (Length(Value) >= Length(aString)) then
    begin
      tail := Copy(Value, (Length(Value) - Length(aString)) + 1, Length(aString));

      OK := ANSISameText(tail, aString);
    end
    else
      OK := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.Equals(const aExpected: UnicodeString): Evaluation;
  begin
    result := self;

    Description := '= text {expected}';
    Expected    := '''' + aExpected + '''';

    OK := ANSISameText(Value, aExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.BeginsWith(const aString: UTF8String): Evaluation;
  begin
    result := BeginsWith(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.Contains(const aSubString: UTF8String): Evaluation;
  begin
    result := Contains(WIDE.FromUTF8(aSubString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.EndsWith(const aString: UTF8String): Evaluation;
  begin
    result := EndsWith(WIDE.FromUTF8(aString));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTextExpectation.Equals(const aExpected: UTF8String): Evaluation;
  begin
    result := Equals(WIDE.FromUTF8(aExpected));
  end;














{ TTestException --------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  constructor TTestException.CreateExpecting(const aSubject: TTest;
                                             const aExceptionClass: TExceptionClass);
  begin
    inherited Create(aSubject);

    fException      := AcquireExceptionObject;
    fExceptionClass := aExceptionClass;

    Description := 'Expected exception: ' + aExceptionClass.ClassName;
    Expected    := aExceptionClass.ClassName + ' exception expected';

    if NOT Assigned(fException) then
      Actual := 'No exception raised'
    else if NOT (fException is aExceptionClass) then
      Actual := aExceptionClass.ClassName + ' exception raised instead'
    else
      Expected := Expected + ' (' + fException.ClassName + ': ' + fException.Message + ')';

    OK := Assigned(fException) and (fException is aExceptionClass);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  constructor TTestException.CreateUnexpected(const aSubject: TTest);
  begin
    inherited Create(aSubject);

    fException := AcquireExceptionObject;

    Expected  := 'No exception expected';

    if Assigned(fException) then
      Actual := fException.ClassName + ' was raised, with message: ' + fException.Message;

    OK := NOT Assigned(fException);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  destructor TTestException.Destroy;
  begin
    FreeAndNIL(fException);

    inherited;
  end;


  function TTestException.get_Exception: Exception;
  begin
    result := fException;
  end;


  function TTestException.get_ExceptionClass: TExceptionClass;
  begin
    result := TExceptionClass(fException.ClassType);
  end;





{ TDoubleInspector }

  procedure TDoubleInspector.Value(aValue: Double);
  begin
    Emit(FloatToStr(aValue));
  end;



{ TCurrencyInspector }

  procedure TCurrencyInspector.Value(aValue: Currency);
  begin
    Emit(CurrToStr(aValue));
  end;









initialization
  Smoketest.RegisterExtension(DoubleTest,   TDoubleExpectation);
  Smoketest.RegisterExtension(CurrencyTest, TCurrencyExpectation);
  Smoketest.RegisterExtension(ColorTest,    TColorTest);
  Smoketest.RegisterExtension(DateTest,     TDateExpectation);
  Smoketest.RegisterExtension(TimeTest,     TTimeExpectation);
  Smoketest.RegisterExtension(DateTimeTest, TDatetimeExpectation);
  Smoketest.RegisterExtension(UTF8Test,     TStringExpectation);
  Smoketest.RegisterExtension(EnumTest,     TEnumTest);

  Smoketest.RegisterExtension(DoubleInspector,   TDoubleInspector);
  Smoketest.RegisterExtension(CurrencyInspector, TCurrencyInspector);
end.
