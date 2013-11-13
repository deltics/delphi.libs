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

{$i deltics.smoketest.inc}

{$ifdef deltics_smoketest}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Smoketest;

// TODO: Provide command line support for "diagnostics" and "thread isolation" options
//        (search: "options" to find affected areas)


interface

  uses
  { vcl: }
    Classes,
    Contnrs,
    Graphics,
    SysUtils,
    TypInfo,
    Variants,
  { deltics: }
    Deltics.Classes,
    Deltics.CommandLine,
    Deltics.DateUtils,
    Deltics.MultiCast,
    Deltics.StateList,
    Deltics.Strings,
    Deltics.SysUtils,
    Deltics.Threads.Storage,
    Deltics.Threads.Worker,
    Deltics.Types;


  type
    TTestArticleType = (
                        atSmoketest,
                        atTestCase,
                        atPerformanceCase,
                        atTestDelegate,
                        atPerformanceDelegate
                       );

    TDelegateType = (
                     dtDefault,
                     dtFunctionTest,
                     dtFailTest           // A delegate of this type is EXPECTED to fail
                    );

    TPerformanceMode = (
                        pmIterations,
                        pmSeconds
                       );


  const
    NO_DAYS         = Word(-1);
    NO_SECONDS      = Word(-1);
    NO_MILLISECONDS = Word(-1);

  type
    ESmoketest = class(Exception);
    EAbortTest = class(ESmoketest);
    EAbortCase = class(ESmoketest);

    IPerformanceCaseAddition = interface;
    IPerformanceCaseDuration = interface;
    Evaluation   = interface;
    IExpectation  = interface;
    IReason       = interface;

    ITestArticle          = interface;
    ITestCase             = interface;
    IPerformanceCase      = interface;
    ITestMethod           = interface;
    IPerformanceMethod    = interface;
    IPerformanceSample    = interface;

    ColorTest     = interface;
    CurrencyTest  = interface;
    DatetimeTest  = interface;
    DateTest      = interface;
    TimeTest      = interface;
    DoubleTest    = interface;
    UTF8Test      = interface;

    ApproximateExpectation  = interface;
    BooleanExpectation      = interface;
    CardinalExpectation     = interface;
    ColorExpectation        = interface;
    CurrencyExpectation     = interface;
    DateExpectation         = interface;
    DatetimeExpectation     = interface;
    DoubleExpectation       = interface;
    ExceptionExpectation    = interface;
    ExtendedExpectation     = interface;
    GUIDExpectation         = interface;
    IntegerExpectation      = interface;
    InterfaceExpectation    = interface;
    ObjectExpectation       = interface;
    PointerExpectation      = interface;
    SingleExpectation       = interface;
    StringExpectation       = interface;
    TextExpectation         = interface;
    UTF8Expectation         = interface;
    UTF8TextExpectation     = interface;
    TimeExpectation         = interface;

    TTestArticle = class;
    TSmoketest  = class;
    TSmoketestCommandLine = class;
    {$typeinfo ON}
    TCase = class;
    TTestCase = class;
    TPerformanceCase = class;
    {$typeinfo OFF}
    TTestCaseClass = class of TTestCase;
    TPerformanceCaseClass = class of TPerformanceCase;

    TCaseClass = class of TCase;

    TDelegate = class;
    TTestDelegate = class;
    TPerformanceDelegate = class;
    TInspector = class;
    TInspectorClass = class of TInspector;
    TTest = class;
    TTestRun = class;

    TOutput = class;
    TOutputClass = class of TOutput;
    TAlert = class;
    TInformation = class;
    TInspection = class;
    TExpectation = class;
    TExpectationClass = class of TExpectation;
    TTestResult = class;

    TExceptionClass = class of Exception;

    TDelegateMethod = procedure of object;


    IExpectation = interface
    ['{A5C6A9EC-D6B8-4BD8-8BFA-43E79F71374C}']
      function Supports(const aIID: TGUID; const aName: UnicodeString = ''): Evaluation;
    end;


    IInspectorMethods = interface
    ['{FBA6C926-3550-42F3-94B9-6906C6C96904}']
      function get_Part(aPart: Variant): IInspectorMethods;
    {$ifdef EnhancedOverloads}
      procedure Value(aValue: Currency); overload;
      procedure Value(aValue: Double); overload;
      procedure Value(aValue: TDatetime); overload;
      procedure Value(aValue: TDate); overload;
      procedure Value(aValue: TTime); overload;
      procedure Value(aValue: UTF8String); overload;
    {$endif}
      procedure Value(aValue: ANSIString); overload;
      procedure Value(aValue: UnicodeString); overload;
      procedure Value(aValue: Single); overload;
      procedure Value(aValue: Extended); overload;
      procedure Value(aValue: Boolean); overload;
      procedure Value(aValue: Int64); overload;
      procedure Value(aValue: TGUID); overload;
      procedure Value(aValue: TStrings); overload;
      procedure Value(aBuffer: Pointer; aSize: Integer); overload;
      procedure Values(const aArgs: array of const);
      property Part[aPart: Variant]: IInspectorMethods read get_Part; default;
    end;


    IInspector = interface(IInspectorMethods)
    ['{B39B2D1D-EDD7-4716-A94A-668170A212B7}']
      function get_MonoSpaced: IInspectorMethods;
      property MonoSpaced: IInspectorMethods read get_MonoSpaced;
    end;


    ITest = interface
    ['{902B3635-AABF-48F2-9E9F-EAD884C78D57}']
      function get_Part(aPart: Variant): ITest;
    {$ifdef EnhancedOverloads}
      function Expect(aValue: Currency): CurrencyExpectation; overload;
      function Expect(aValue: Double): DoubleExpectation; overload;
      function Expect(aValue: TDatetime): DatetimeExpectation; overload;
      function Expect(aValue: TDate): DateExpectation; overload;
      function Expect(aValue: TTime): TimeExpectation; overload;
      function Expect(aValue: UTF8String): UTF8Expectation; overload;
    {$endif}
      function Expect(aValue: Boolean): BooleanExpectation; overload;
      function Expect(aValue: Single): SingleExpectation; overload;
      function Expect(aValue: Extended): ExtendedExpectation; overload;
      function Expect(aValue: Cardinal): CardinalExpectation; overload;
      function Expect(aValue: Int64): IntegerExpectation; overload;
      function Expect(aValue: IInterface): InterfaceExpectation; overload;
      function Expect(aValue: TObject): ObjectExpectation; overload;
      function Expect(aValue: TGUID): GUIDExpectation; overload;
      function Expect(aValue: Pointer): PointerExpectation; overload;
      function Expect(aValue: ANSIString): StringExpectation; overload;
      function Expect(aValue: UnicodeString): StringExpectation; overload;
      function Expecting(aClass: TExceptionClass; aMessage: UnicodeString = ''): ExceptionExpectation;
      function UnexpectedException: ExceptionExpectation;

      property Part[aPart: Variant]: ITest read get_Part; default;
    end;

    IOutput = interface
    ['{30385B42-6CAB-406C-BBC2-2933AD9E7788}']
      function get_Article: ITestArticle;
      property Article: ITestArticle read get_Article;
    end;


    Evaluation = interface
    ['{F5F6DD44-B07E-4584-8ED1-6B062A5697C2}']
      function get_OK: Boolean;
      function IsCritical: IReason;
      function IsExpectedToFail: IReason;
      function IsRequired: IReason;
      procedure IsShowStopper;
      property OK: Boolean read get_OK;
    end;


    IReason = interface
    ['{5826E6E6-D9E8-4EDD-8ACD-FF74C13F937A}']
      procedure Because(const aExplanation: UnicodeString);
    end;


    ITheseTests = interface
    ['{21F0C6CF-01DE-48FF-B7F4-27A236C8931C}']
      procedure AreExpectedToFail;
    end;

    INextTest = interface
    ['{E33CB759-AD0C-4844-BEF0-20A886F01B4B}']
      procedure IsExpectedToFail;
    end;

    INextNTests = interface
    ['{59395DDD-8A9F-4C55-A464-7539560DD17E}']
      function Tests: ITheseTests;
    end;


    IHaveChildCases   = interface ['{E5518D96-A989-4E6F-8CB7-B24D83548FEE}'] procedure AddCases; end;
    INameCase         = interface ['{BDAA53B3-DC49-486B-BB75-B6A098B0E45F}'] function NameForCase: UnicodeString; end;
    ISetupSmoketest   = interface ['{C6ABF342-11F2-4D89-96D9-E47E09EBA6DC}'] procedure SetupSmoketest; end;
    ICleanupSmoketest = interface ['{4E4A026F-EACD-4CAC-BB2E-32F3CFE6E465}'] procedure CleanupSmoketest; end;
    ISetupTestRun     = interface ['{41AD1AC7-488E-4344-BEBF-3082F7010DB5}'] procedure SetupTestRun; end;
    ICleanupTestRun   = interface ['{77D3B727-8F6B-4C89-AA01-56B2A37E3E38}'] procedure CleanupTestRun; end;
    ISetupTestCase    = interface ['{9799BC59-9E89-4100-B92B-939346C678A8}'] procedure Setup; end;
    ICleanupTestCase  = interface ['{214DFC59-2FF5-4DA3-97B1-85FA7AFAC006}'] procedure Cleanup; end;
    ISetupTest        = interface ['{A792944A-165C-41FB-A67B-6E3F104A50A7}'] procedure SetupTest(const aTest: TDelegate); end;
    ICleanupTest      = interface ['{1016CB7A-CD07-4640-9C96-B9F466103654}'] procedure CleanupTest(const aTest: TDelegate); end;


    ISmoketestSetup = interface
    ['{7B39E5F4-89FF-43DA-82C9-AB6287AE4615}']
      procedure Ready;
      procedure RegisterExtension(const aIID: TGUID; const aTest: TExpectationClass); overload;
      procedure RegisterExtension(const aIID: TGUID; const aInspector: TInspectorClass); overload;
      procedure Add(const aCases: array of TTestCaseClass); overload;
      procedure Add(const aParentCase: TTestCaseClass; const aCases: array of TTestCaseClass); overload;
      function AverageTime(const aCases: array of TPerformanceCaseClass; const aRepeats: Integer): IPerformanceCaseAddition; overload;
      function Time(const aCases: array of TPerformanceCaseClass): IPerformanceCaseAddition; overload;
    end;


    ITestArticle = interface
    ['{B50DD146-2463-48A5-8EA1-CEC9E46F1886}']
      function get_Child(const aIndex: Integer): ITestArticle;
      function get_ArticleType: TTestArticleType;
      function get_Count: Integer;
      function get_DisplayName: UnicodeString;
      function get_EffectivelyEnabled: Boolean;
      function get_Elapsed: Int64;
      function get_Enabled: Boolean;
      function get_HasFailures: Boolean;
      function get_HasErrors: Boolean;
      function get_ID: Int64;
      function get_IsRunning: Boolean;
      function get_Name: UnicodeString;
      function get_Parent: ITestArticle;
      function get_Reference: UnicodeString;
      procedure set_Enabled(const aValue: Boolean);

      function AsObject(var aRef; const aClass: TClass = NIL): Boolean;

      property Child[const aIndex: Integer]: ITestArticle read get_Child; default;
      property ArticleType: TTestArticleType read get_ArticleType;
      property Count: Integer read get_Count;
      property DisplayName: UnicodeString read get_DisplayName;
      property EffectivelyEnabled: Boolean read get_EffectivelyEnabled;
      property Elapsed: Int64 read get_Elapsed;
      property Enabled: Boolean read get_Enabled write set_Enabled;
      property HasErrors: Boolean read get_HasErrors;
      property HasFailures: Boolean read get_HasFailures;
      property ID: Int64 read get_ID;
      property IsRunning: Boolean read get_IsRunning;
      property Name: UnicodeString read get_Name;
      property Parent: ITestArticle read get_Parent;
      property Reference: UnicodeString read get_Reference;
    end;


    ISmoketestMetadata = interface(ITestArticle)
    ['{F56A2057-CC31-4A0A-914B-0169EBDC1B82}']
      function get_Case(const aIndex: Integer): TCase;
      function get_CaseCount: Integer;

      function FindCase(const aNamePath: UnicodeString): ITestCase;
      function FindMethod(const aNamePath: UnicodeString): ITestMethod;

      property Cases[const aIndex: Integer]: TCase read get_Case;
      property CaseCount: Integer read get_CaseCount;
    end;


    ISmoketestRuntime = interface(ISmoketestMetadata)
    ['{B58A6B94-B4C6-4C8F-8431-31D965B6A190}']
      function get_CommandLine: TSmoketestCommandLine;
      function get_TestRun: TTestRun;
      function get_Thread: TWorkerThread;
      function get_On_Finished: TMultiCastNotify;
      function get_On_Started: TMultiCastNotify;
      function get_On_Update: TMultiCastNotify;

      procedure Abort;
      procedure Run;

      property CommandLine: TSmoketestCommandLine read get_CommandLine;
      property TestRun: TTestRun read get_TestRun;
      property Thread: TWorkerThread read get_Thread;
      property On_Finished: TMultiCastNotify read get_On_Finished;
      property On_Started: TMultiCastNotify read get_On_Started;
      property On_Update: TMultiCastNotify read get_On_Update;
    end;


    ITestCase = interface(ITestArticle)
    ['{42108727-60B8-4B87-ADAD-24CA723E8030}']
      function get_CaseByIndex(const aIndex: Integer): ITestCase;
      function get_CaseByName(const aName: UnicodeString): ITestCase;
      function get_CaseCount: Integer;
      function get_HasChildCases: Boolean;
      function get_IsAborted: Boolean;
      function get_MethodByIndex(const aIndex: Integer): ITestMethod;
      function get_MethodByName(const aName: UnicodeString): ITestMethod;
      function get_MethodCount: Integer;
      function get_StartTime: TDateTime;

      property CaseByIndex[const aIndex: Integer]: ITestCase read get_CaseByIndex;
      property CaseByName[const aName: UnicodeString]: ITestCase read get_CaseByName;
      property CaseCount: Integer read get_CaseCount;
      property HasChildCases: Boolean read get_HasChildCases;
      property IsAborted: Boolean read get_IsAborted;
      property MethodByIndex[const aIndex: Integer]: ITestMethod read get_MethodByIndex;
      property MethodByName[const aName: UnicodeString]: ITestMethod read get_MethodByName;
      property MethodCount: Integer read get_MethodCount;
      property StartTime: TDateTime read get_StartTime;
    end;


    IPerformanceCase = interface(ITestArticle)
    ['{4D1618BC-2B9F-4AF8-965B-27CDAA1206FA}']
      function get_MethodByIndex(const aIndex: Integer): IPerformanceMethod;
      function get_MethodByName(const aName: UnicodeString): IPerformanceMethod;
      function get_MethodCount: Integer;
      function get_Samples: Integer;
      function get_Sampling: Integer;
      function get_StartTime: TDateTime;

      property MethodByIndex[const aIndex: Integer]: IPerformanceMethod read get_MethodByIndex;
      property MethodByName[const aName: UnicodeString]: IPerformanceMethod read get_MethodByName;
      property MethodCount: Integer read get_MethodCount;
      property Samples: Integer read get_Samples;
      property Sampling: Integer read get_Sampling;
      property StartTime: TDateTime read get_StartTime;
    end;


    ITestMethod = interface(ITestArticle)
    ['{4A9BF2DE-BCB7-4C99-82AF-E905D1C4A00E}']
      function get_Aborted: Boolean;
      function get_Failures: Integer;
      function get_HasFailures: Boolean;
      function get_HasPasses: Boolean;
      function get_NotImplemented: Boolean;
      function get_Passes: Integer;
      function get_StartTime: TDateTime;
      function get_TestCase: ITestCase;

      property Aborted: Boolean read get_Aborted;
      property Failures: Integer read get_Failures;
      property HasFailures: Boolean read get_HasFailures;
      property HasPasses: Boolean read get_HasPasses;
      property NotImplemented: Boolean read get_NotImplemented;
      property Passes: Integer read get_Passes;
      property StartTime: TDateTime read get_StartTime;
      property TestCase: ITestCase read get_TestCase;
    end;


    IPerformanceMethod = interface(ITestArticle)
    ['{D4048DE2-C2B5-4B52-874C-5ADD4167351A}']
      function get_AverageRuntime: Double;
      function get_ExecutionsPerSecond: Double;
      function get_Fastest: Double;
      function get_Slowest: Double;
      function get_Sample(const aIndex: Integer): IPerformanceSample;
      function get_SampleCount: Integer;
      function get_Sampling: Integer;
      function get_TestCase: IPerformanceCase;

      property AverageRuntime: Double read get_AverageRuntime;
      property ExecutionsPerSecond: Double read get_ExecutionsPerSecond;
      property Fastest: Double read get_Fastest;
      property Slowest: Double read get_Slowest;
      property Samples[const aIndex: Integer]: IPerformanceSample read get_Sample;
      property SampleCount: Integer read get_SampleCount;
      property Sampling: Integer read get_Sampling;
      property TestCase: IPerformanceCase read get_TestCase;
    end;


    IPerformanceSample = interface
    ['{1AC9FF5D-4627-42A0-A3EC-C3ED01DD0CC1}']
      function get_AverageRuntime: Double;
      function get_ExecutionsPerSecond: Double;
      function get_Fastest: Double;
      function get_Slowest: Double;
      function get_TotalRuntime: Double;

      property AverageRuntime: Double read get_AverageRuntime;
      property ExecutionsPerSecond: Double read get_ExecutionsPerSecond;
      property Fastest: Double read get_Fastest;
      property Slowest: Double read get_Slowest;
      property TotalRuntime: Double read get_TotalRunTime;
    end;



    IAlert = interface
    ['{A1A6A0EB-5205-4F05-B303-303F2821EE90}']
      function get_Text: UnicodeString;

      property Text: UnicodeString read get_Text;
    end;


    IInformation = interface
    ['{076F14E4-B90B-4236-9598-91A82DB857D0}']
      function get_Text: UnicodeString;

      property Text: UnicodeString read get_Text;
    end;


    IInspection = interface
    ['{B1A70FDC-A88E-4436-B2FA-98272990656F}']
      function get_ItemCount: Integer;
      function get_ItemLabel(const aIndex: Integer): UnicodeString;
      function get_ItemValue(const aIndex: Integer): UnicodeString;
      function get_MonoSpaced: Boolean;
      function get_Subject: UnicodeString;
      function get_Value: UnicodeString;

      property ItemCount: Integer read get_ItemCount;
      property ItemLabels[const aIndex: Integer]: UnicodeString read get_ItemLabel;
      property ItemValues[const aIndex: Integer]: UnicodeString read get_ItemValue;
      property MonoSpaced: Boolean read get_MonoSpaced;
      property Subject: UnicodeString read get_Subject;
      property Value: UnicodeString read get_Value;
    end;


    IResult = interface
    ['{CDB0BA13-B460-4065-97FA-82EC2F2B64E9}']
      function get_Actual: UnicodeString;
      function get_Description: UnicodeString;
      function get_Evaluated: Boolean;
      function get_Expected: UnicodeString;
      function get_ExpectedToFail: Boolean;
      function get_Explanation: UnicodeString;
      function get_OK: Boolean;
      function get_SubjectName: UnicodeString;
      function get_TestCase: ITestCase;
      function get_TestMethod: ITestMethod;

      property Actual: UnicodeString read get_Actual;
      property Description: UnicodeString read get_Description;
      property Evaluated: Boolean read get_Evaluated;
      property Expected: UnicodeString read get_Expected;
      property ExpectedToFail: Boolean read get_ExpectedToFail;
      property Explanation: UnicodeString read get_Explanation;
      property OK: Boolean read get_OK;
      property SubjectName: UnicodeString read get_SubjectName;
      property TestCase: ITestCase read get_TestCase;
      property TestMethod: ITestMethod read get_TestMethod;
    end;


    TTestArticle = class(TInterfacedObject, IAsObject,
                                            ITestArticle,
                                            IOn_Destroy)
    private
      fArticleType: TTestArticleType;
      fDisplayName: UnicodeString;
      fElapsed: Int64;
      fEnabled: Boolean;
      fName: UnicodeString;
      fOwner: TTestArticle;
      fState: TStateList;
      fOn_Destroy: IOn_Destroy;
      procedure OnObjectDestroyed(Sender: TObject);
    // PRIVATE but still virtual/overridable - only framework classes
    //          are intended to override these methods, hence they are
    //          PRIVATE, to prevent them 'leaking' into the "user experience"
    //          for people creating tests and test cases
      function get_Count: Integer; virtual; abstract;
      function get_ID: Int64;
      function get_Child(const aIndex: Integer): ITestArticle; virtual; abstract;
      procedure Add(const aObject: TTestArticle); overload; virtual;
      procedure AddElapsed(const aValue: Cardinal); virtual;
      function NameForConsole: UnicodeString;
      function PlainReference: UnicodeString;
      function Find(const aPath: UnicodeString): TTestArticle;
      function IndexOf(const aChild: TTestArticle): Integer;
      procedure NotifyChange;
      procedure OnStateChanged(aSender: TObject; const aStateID: TStateID);
      procedure Remove(const aObject: TTestArticle); virtual; abstract;
      function SetDisplayName: UnicodeString; virtual;
      constructor CreateEx; virtual;
      constructor Create(const aName: UnicodeString = ''); overload;
      constructor Create(const aOwner: TTestArticle;
                         const aName: UnicodeString = ''); overload;
    public
      destructor Destroy; override;
    private
      property Owner: TTestArticle read fOwner;
      property State: TStateList read fState;
      property On_Destroy: IOn_Destroy read fOn_Destroy implements IOn_Destroy;

    private // IAsObject
      function get_AsObject: TObject;

    private // ITestArticle
      function get_ArticleType: TTestArticleType;
      function get_DisplayName: UnicodeString;
      function get_Enabled: Boolean;
      function get_Elapsed: Int64; virtual;
      function get_EffectivelyEnabled: Boolean;
      function get_HasErrors: Boolean;
      function get_HasFailures: Boolean;
      function get_IsRunning: Boolean;
      function get_Name: UnicodeString;
      function get_Parent: ITestArticle;
      function get_Reference: UnicodeString;
      procedure set_Enabled(const aValue: Boolean);
    public
      function AsObject(var aRef; const aClass: TClass = NIL): Boolean;

      property Child[const aIndex: Integer]: ITestArticle read get_Child; default;
      property ArticleType: TTestArticleType read get_ArticleType;
      property Count: Integer read get_Count;
      property DisplayName: UnicodeString read get_DisplayName;
      property EffectivelyEnabled: Boolean read get_EffectivelyEnabled;
      property Elapsed: Int64 read get_Elapsed;
      property Enabled: Boolean read get_Enabled write set_Enabled;
      property HasErrors: Boolean read get_HasErrors;
      property HasFailures: Boolean read get_HasFailures;
      property ID: Int64 read get_ID;
      property IsRunning: Boolean read get_IsRunning;
      property Name: UnicodeString read get_Name;
      property Parent: ITestArticle read get_Parent;
      property Reference: UnicodeString read get_Reference;
    end;


    IPerformanceCaseAddition = interface
    ['{BDB38494-97F7-47F3-83D5-871ABE82C8B4}']
      function RunningFor(const aNumber: Integer): IPerformanceCaseDuration;
    end;

    IPerformanceCaseDuration = interface
    ['{44AA8CB7-869B-4C1A-A0A5-4B7D17D2B22B}']
      procedure Iterations;
      procedure Seconds;
    end;


    TSmoketestCommandLine = class(TCommandLine)
    private
      fDisableList: TStringList;
      fRunList: TStringList;
    protected
      procedure Define; override;
    private
      function get_DisableList: TStringList;
      function get_RunList: TStringList;

      function get_AutoRun: Boolean;
      function get_DisableCases: Boolean;
      function get_DisablePerformanceCases: Boolean;
      function get_NoOutput: Boolean;
      function get_OutputFilename: UnicodeString;
      function get_OutputToConsole: Boolean;
      function get_OutputJSON: Boolean;
      function get_OutputPlainText: Boolean;
      function get_OutputXML: Boolean;
      function get_SilentRunning: Boolean;
      function get_ThreadIsolation: Boolean;
      function get_VerboseOutput: Boolean;
    public
      destructor Destroy; override;

      property DisableList: TStringList read get_DisableList;
      property RunList: TStringList read get_RunList;

      property AutoRun: Boolean read get_AutoRun;
      property DisableCases: Boolean read get_DisableCases;
      property DisablePerformanceCases: Boolean read get_DisablePerformanceCases;
      property NoOutput: Boolean read get_NoOutput;
      property OutputFilename: UnicodeString read get_OutputFilename;
      property OutputToConsole: Boolean read get_OutputToConsole;
      property OutputJSON: Boolean read get_OutputJSON;
      property OutputPlainText: Boolean read get_OutputPlainText;
      property OutputXML: Boolean read get_OutputXML;
      property SilentRunning: Boolean read get_SilentRunning;
      property ThreadIsolation: Boolean read get_ThreadIsolation;
      property VerboseOutput: Boolean read get_VerboseOutput;
    end;


    TSmoketest = class(TTestArticle, ISmoketestMetadata,
                                     ISmoketestRuntime,
                                     ISmoketestSetup)
    private
      fActiveCase: TPerThreadObjectStack;
      fCases: TObjectList;
      fCmdLine: TSmoketestCommandLine;
      fName: UnicodeString;
      fTestRun: TTestRun;
      fThread: TWorkerThread;
      fOn_Started: TMultiCastNotify;
      fOn_Finished: TMultiCastNotify;
      fOn_Update: TMultiCastNotify;
      constructor Create;
      function get_Child(const aIndex: Integer): ITestArticle; override;
      function get_ActiveCase: TCase;
      function get_Count: Integer; override;
      function get_IsInitialised: Boolean;
      procedure set_ActiveCase(const aValue: TCase);
      function FindExtension(const aIID: TGUID; var aClass: TExpectationClass): Boolean; overload;
      function FindExtension(const aIID: TGUID; var aClass: TInspectorClass): Boolean; overload;
      procedure OutputResults;
      procedure OnThreadStateChange(Sender: TObject; const aStateID: TStateID);
      property ActiveCase: TCase read get_ActiveCase write set_ActiveCase;
      procedure Add(const aArticle: TTestArticle); overload; override;
      procedure Remove(const aArticle: TTestArticle); override;
      procedure Execute;
      procedure Initialise;
      procedure Startup;
      procedure Shutdown;
      procedure EndRun;
      procedure StartRun;

      property On_Finished: TMultiCastNotify read fOn_Finished;
      property On_Started: TMultiCastNotify read fOn_Started;
      property On_Update: TMultiCastNotify read fOn_Update;
    public
      procedure AfterConstruction; override;
      destructor Destroy; override;
      procedure Initialize;
      function Find(const aCase: TCaseClass): TCase;
      function FindArticle(const aPath: UnicodeString): TTestArticle;
      property IsInitialised: Boolean read get_IsInitialised;

    private // ITestArticle
      function get_Name: UnicodeString;
    public
      property Name: UnicodeString read get_Name;

    private // ISmoketestMetadata
      function get_Case(const aIndex: Integer): TCase;
      function get_CaseCount: Integer;
      function FindCase(const aNamePath: UnicodeString): ITestCase;
      function FindMethod(const aNamePath: UnicodeString): ITestMethod;
      property Cases[const aIndex: Integer]: TCase read get_Case;
      property CaseCount: Integer read get_CaseCount;

    private // ISmoketestRuntime
      function get_CommandLine: TSmoketestCommandLine;
      function get_TestRun: TTestRun;
      function get_Thread: TWorkerThread;
      function get_On_Finished: TMultiCastNotify;
      function get_On_Started: TMultiCastNotify;
      function get_On_Update: TMultiCastNotify;
      procedure Abort;
      procedure Run;
    public
      property CommandLine: TSmoketestCommandLine read get_CommandLine;
      property TestRun: TTestRun read get_TestRun;
      property Thread: TWorkerThread read get_Thread;

    private // ISmoketestSetup
      procedure Ready;
      procedure RegisterExtension(const aIID: TGUID; const aTest: TExpectationClass); overload;
      procedure RegisterExtension(const aIID: TGUID; const aInspector: TInspectorClass); overload;
      procedure Add(const aCases: array of TTestCaseClass); overload;
      procedure Add(const aParent: TTestCaseClass;
                    const aCases: array of TTestCaseClass); overload;
      function AverageTime(const aCases: array of TPerformanceCaseClass; const aRepeats: Integer): IPerformanceCaseAddition; overload;
      function Time(const aCases: array of TPerformanceCaseClass): IPerformanceCaseAddition; overload;
    end;


    TCase = class(TTestArticle)
    private
      fRunCount: Integer;
      fStartTime: TDateTime;
      constructor Create(const aParentCase: TCase = NIL); virtual;
      function get_Delegate(const aIndex: Integer): TDelegate; virtual; abstract;
      function get_DelegateCount: Integer; virtual; abstract;
      function get_StartTime: TDateTime;
      procedure DoExecute; virtual; abstract;
      procedure DoInitialise; virtual;
      procedure DoShutdown; virtual;
      procedure DoStartup; virtual;
      procedure Execute;
      procedure Initialise;
      procedure Shutdown;
      procedure Startup;
      function SetDisplayName: UnicodeString; override;
    public
      property Delegate[const aIndex: Integer]: TDelegate read get_Delegate;
      property DelegateCount: Integer read get_DelegateCount;
      property RunCount: Integer read fRunCount;
      property StartTime: TDateTime read fStartTime;
    end;


    TTestCase = class(TCase, ITestCase)
    private
      fActiveDelegate: TTestDelegate;
      fCases: TObjectList;
      fInitialDelegates: TObjectList;
      fInspector: TInspector;
      fFinalDelegates: TObjectList;
      fIncidents: TObjectList;
//      function get_TestCount: Integer;
//      function get_TestByIndex(const aIndex: Integer): TTestDelegate;
//      function get_TestByName(const aName: UnicodeString): TTestDelegate;
      constructor Create(const aParent: TTestCase); reintroduce; overload;
      constructor CreateEx; override;
      function get_Child(const aIndex: Integer): ITestArticle; override;
      function get_Count: Integer; override;
      function get_Delegate(const aIndex: Integer): TDelegate; override;
      function get_DelegateCount: Integer; override;
      function get_Smoketest: ISmoketestMetadata;
      procedure Add(const aObject: TTestArticle); override;
      procedure DoExecute; override;
      procedure DoInitialise; override;
      procedure DoStartup; override;
      procedure DoShutdown; override;
      procedure Error(const aMessage: UnicodeString);
      function Passed: Boolean;
      procedure Remove(const aObject: TTestArticle); override;
      procedure ChildEnterState(aSender: TObject; const aStateID: TStateID);
      procedure ChildLeaveState(aSender: TObject; const aStateID: TStateID);
    protected
      procedure Abort(const aMessage: UnicodeString = ''); overload;
      procedure Abort(const aMessage: UnicodeString; const aArgs: array of const); overload;
      procedure AbortCase(const aMessage: UnicodeString = ''); overload;
      procedure AbortCase(const aMessage: UnicodeString; const aArgs: array of const); overload;
    public
      constructor Create; reintroduce; overload;
      procedure AfterConstruction; override;
      destructor Destroy; override;
    protected
      // NOTE: String methods are not explicitly implemented - the compiler will map such
      //        declarations onto either the ANSIString or UnicodeString declared implementations
      //        according to whether the built-in 'String' type in the compiler version is
      //        ANSI or Unicode
      function Inspect(aName: String): IInspector; overload;
      function Inspect(aName: String; aArgs: array of const): IInspector; overload;
      function Test(aName: String): ITest; overload;
      function Test(aName: String; aArgs: array of const): ITest; overload;
    {$ifdef UNICODE}
      function Inspect(aName: ANSIString): IInspector; overload;
      function Inspect(aName: ANSIString; aArgs: array of const): IInspector; overload;
      function Inspect(aName: UTF8String): IInspector; overload;
      function Inspect(aName: UTF8String; aArgs: array of const): IInspector; overload;
      function Test(aName: ANSIString): ITest; overload;
      function Test(aName: ANSIString; aArgs: array of const): ITest; overload;
      function Test(aName: UTF8String): ITest; overload;
      function Test(aName: UTF8String; aArgs: array of const): ITest; overload;
    {$else}
      function Inspect(aName: UnicodeString): IInspector; overload;
      function Inspect(aName: UnicodeString; aArgs: array of const): IInspector; overload;
      function Test(aName: UnicodeString): ITest; overload;
      function Test(aName: UnicodeString; aArgs: array of const): ITest; overload;
    {$endif}
      function TestDatetime: DatetimeTest; overload;
      function TestDatetime(aName: UnicodeString): DatetimeTest; overload;
      function TestDatetime(aName: UnicodeString; aArgs: array of const): DatetimeTest; overload;
      function TestUTF8: UTF8Test; overload;
      function TestUTF8(aName: UnicodeString): UTF8Test; overload;
      function TestUTF8(aName: UnicodeString; aArgs: array of const): UTF8Test; overload;

      procedure Alert(aMessage: UnicodeString);
      procedure Note(aMessage: UnicodeString); overload;
      procedure Note(aMessage: UnicodeString; const aArgs: array of const); overload;
      function Test: ITest; overload;

      // The Deltics.Smoketest unit has a Smoketest reference which provides access
      //  to the SmoketestSetup interface.  In the context of a TestCase method, a
      //  reference ot the SmoketestMetaData is more appropriate so we introducing a
      //  property with the same name as the unit variable so as to hide it and replace
      //  it with the more appropriate interface reference.  The setup interface can still
      //  be obtained using a unit name qualification if necessary.
      property Smoketest: ISmoketestMetadata read get_Smoketest;

    protected
      function Metadata: ITestCase;
      function TheseTests: ITheseTests;
      function TheNextTest: INextTest;
      function TheNext(const aNumber: Integer): INextNTests;

    private // ITestCase ------------------------------------------------------
      function get_HasChildCases: Boolean;
    private
      function get_CaseByIndex(const aIndex: Integer): ITestCase;
      function get_CaseByName(const aName: UnicodeString): ITestCase;
      function get_CaseCount: Integer;
      function get_IsAborted: Boolean;
      function get_MethodByIndex(const aIndex: Integer): ITestMethod;
      function get_MethodByName(const aName: UnicodeString): ITestMethod;
      function get_MethodCount: Integer;
    end;


    TPerformanceCase = class(TCase, IPerformanceCase)
    private
      fDelegates: TObjectList;
      fN: Integer;
      fMode: TPerformanceMode;
      fSamples: Integer;
      fSampling: Integer;
      function SetDisplayName: UnicodeString; override;
      function get_Count: Integer; override;
      function get_Child(const aIndex: Integer): ITestArticle; override;
      function get_Delegate(const aIndex: Integer): TDelegate; override;
      function get_DelegateCount: Integer; override;
      procedure Add(const aObject: TTestArticle); override;
      procedure Remove(const aObject: TTestArticle); override;
      procedure DoExecute; override;
      constructor Create(const N: Integer;
                         const aMode: TPerformanceMode;
                         const aSamples: Integer); reintroduce;
      property Mode: TPerformanceMode read fMode;
    public
      procedure AfterConstruction; override;
      destructor Destroy; override;
      property N: Integer read fN;

    private // IPerformanceCase
      function get_MethodByIndex(const aIndex: Integer): IPerformanceMethod;
      function get_MethodByName(const aName: UnicodeString): IPerformanceMethod;
      function get_MethodCount: Integer;
      function get_Samples: Integer;
      function get_Sampling: Integer;
    end;


    TDelegate = class(TTestArticle)
    private
      fMethod: TDelegateMethod;
      fRunCount: Integer;
      fStartTime: TDateTime;
      function SetDisplayName: UnicodeString; override;
    protected
      constructor Create(const aCase: TCase;
                         const aName: UnicodeString;
                         const aMethod: Pointer); reintroduce;
      function get_Count: Integer; override;
      function get_Child(const aIndex: Integer): ITestArticle; override;
      procedure DoExecute; virtual; abstract;
      procedure Execute;
      procedure SetStartTime; virtual;
      property Method: TDelegateMethod read fMethod;
    public
      property RunCount: Integer read fRunCount;

    private // Inherited by/for interface implementations on derived classes
      function get_StartTime: TDateTime;
      function get_TestCase: TCase;
    public
      property StartTime: TDateTime read fStartTime;
      property TestCase: TCase read get_TestCase;
    end;
    TDelegateClass = class of TDelegate;


      TTestDelegate = class(TDelegate, ITestMethod)
      private
        fDelegateType: TDelegateType;
        fExpectedFailures: Integer;
        function get_Failed: Boolean;
        function get_Passed: Boolean;
        function get_TestCase: ITestCase;
        function SetDisplayName: UnicodeString; override;
      protected
        procedure DoExecute; override;
      protected
        property DelegateType: TDelegateType read fDelegateType;
        property Failed: Boolean read get_Failed;
        property Passed: Boolean read get_Passed;

      private // ITestMethod
        function get_Aborted: Boolean;
        function get_Failures: Integer;
        function get_HasPasses: Boolean;
        function get_NotImplemented: Boolean;
        function get_Passes: Integer;
      public
        property Aborted: Boolean read get_Aborted;
        property Failures: Integer read get_Failures;
        property HasPasses: Boolean read get_HasPasses;
        property Passes: Integer read get_Passes;
      end;


      TPerformanceDelegate = class(TDelegate, IPerformanceMethod)
      private
        fSample: Integer;
        fElapsed: array of Int64;
        fFastest: array of Cardinal;
        fSlowest: array of Cardinal;
        fIteration: Cardinal;
      private
        function get_Elapsed: Int64; override;
        function get_TestCase: IPerformanceCase;
        procedure AddElapsed(const aValue: Cardinal); override;
        property Iteration: Cardinal read fIteration;
      protected
        procedure Prepare(const aSamples: Integer);
        procedure Execute(const aSample: Integer);
        procedure Finalise;
        procedure DoExecute; override;
        procedure SetStartTime; override;

      private // IPerformanceMethod
        function get_Sample(const aIndex: Integer): IPerformanceSample;
        function get_SampleCount: Integer;
        function get_Sampling: Integer;

      private // IPerformanceSample - returns current sample data when running, otherwise the average over all samples
        function get_AverageRuntime: Double;
        function get_ExecutionsPerSecond: Double;
        function get_Fastest: Double;
        function get_Slowest: Double;
      public
        property AverageRuntime: Double read get_AverageRuntime;
        property ExecutionsPerSecond: Double read get_ExecutionsPerSecond;
        property Fastest: Double read get_Fastest;
        property Slowest: Double read get_Slowest;
      end;


      TPerformanceSample = class(TCOMInterfacedObject, IPerformanceSample)
      private
        fAverageRunTime: Cardinal;
        fTotalRunTime: Cardinal;
        fFastest: Cardinal;
        fSlowest: Cardinal;
        fIterations: Cardinal;
        constructor Create(const aDelegate: TPerformanceDelegate; const aSample: Integer);

      private // IPerformanceSample
        function get_AverageRuntime: Double;
        function get_ExecutionsPerSecond: Double;
        function get_Fastest: Double;
        function get_Slowest: Double;
        function get_TotalRuntime: Double;
      end;


    TIncident = class(TInterfacedObject)
    {
      An Incident is something that can result in a test delegate method creating
       some relevant output.  The two primary classes of Incident are:

      - Inspection  : inspecting some value or simply emitting some text from a test
                       delegate

      - Test        : some expectation that certain conditions will be met at the point
                       in the test delegate that the incident occurs
    }
    private
      fName: UnicodeString;
      fTest: TTestDelegate;
      fTestCase: TTestCase;
    protected
      constructor Create(const aCase: TTestCase;
                         const aName: UnicodeString = '');
      property Test: TTestDelegate read fTest;
      property TestCase: TTestCase read fTestCase;
      property Name: UnicodeString read fName;
    end;


      TInspector = class(TIncident, IUnknown,
                                    IInspectorMethods,
                                    IInspector)
      {
        Inspections allow for tests to emit documentation or to output values for
         information purposes.
      }
      private
        fInspection: TInspection;
        fMonospaced: Boolean;
        fPartName: UnicodeString;
        procedure Output(const aString: UnicodeString);
      protected
        class function GetInspector(const aCase: TTestCase; const aName: UnicodeString = ''): TInspector;
        procedure Emit(const aString: UnicodeString); overload;
        procedure Emit(const aString: UnicodeString; const aArgs: array of const); overload;
        procedure Emit(aBuffer: Pointer); overload;
        procedure Emit(aBuffer: Pointer; aSize: Integer); overload;

      private // IUnknown
        function QueryInterface(const aIID: TGUID; out aObj): HRESULT; stdcall;

      protected // IInspectorMethods
        function get_Part(aPart: Variant): IInspectorMethods;
      private
      {$ifdef EnhancedOverloads}
        procedure Value(aValue: Currency); overload;
        procedure Value(aValue: Double); overload;
        procedure Value(aValue: TDatetime); overload;
        procedure Value(aValue: TDate); overload;
        procedure Value(aValue: TTime); overload;
        procedure Value(aValue: UTF8String); overload;
      {$endif}
        procedure Value(aValue: ANSIString); overload;
        procedure Value(aValue: UnicodeString); overload;
        procedure Value(aValue: Single); overload;
        procedure Value(aValue: Extended); overload;
        procedure Value(aValue: Boolean); overload;
        procedure Value(aValue: Int64); overload;
        procedure Value(aValue: TGUID); overload;
        procedure Value(aValue: TStrings); overload;
        procedure Value(aBuffer: Pointer; aSize: Integer); overload;
        procedure Values(const aArgs: array of const);
        property Part[aPart: Variant]: IInspectorMethods read get_Part; default;

      private // IInspector
        function get_MonoSpaced: IInspectorMethods;
      end;


      TTest = class(TIncident, IUnknown,
                               ITest)
      {
        Tests capture and evaluate actual conditions against expected conditions
         during the execution of a test delegate.
      }
      protected
        function QueryInterface(const aIID: TGUID; out aObj): HRESULT; stdcall;
        procedure Replace(const aOld, aNew: TExpectation);

      private // ITest
        function get_Part(aPart: Variant): ITest;
    {$ifdef EnhancedOverloads}
        function Expect(aValue: Currency): CurrencyExpectation; overload;
        function Expect(aValue: Double): DoubleExpectation; overload;
        function Expect(aValue: TDatetime): DatetimeExpectation; overload;
        function Expect(aValue: TDate): DateExpectation; overload;
        function Expect(aValue: TTime): TimeExpectation; overload;
        function Expect(aValue: UTF8String): UTF8Expectation; overload;
    {$endif}
        function Expect(aValue: Boolean): BooleanExpectation; overload;
        function Expect(aValue: Single): SingleExpectation; overload;
        function Expect(aValue: Extended): ExtendedExpectation; overload;
        function Expect(aValue: Cardinal): CardinalExpectation; overload;
        function Expect(aValue: Int64): IntegerExpectation; overload;
        function Expect(aValue: IInterface): InterfaceExpectation; overload;
        function Expect(aValue: TObject): ObjectExpectation; overload;
        function Expect(aValue: TGUID): GUIDExpectation; overload;
        function Expect(aValue: Pointer): PointerExpectation; overload;
        function Expect(aValue: ANSIString): StringExpectation; overload;
        function Expect(aValue: UnicodeString): StringExpectation; overload;
        function Expecting(aClass: TExceptionClass; aMessage: UnicodeString = ''): ExceptionExpectation;
        function UnexpectedException: ExceptionExpectation;
      end;



    TOutput = class(TInterfacedObject, IOutput)
    private
      fArticle: TTestArticle;
      fText: UnicodeString;
      fTime: TDateTime;
    protected
      constructor Create(const aDelegate: TTestDelegate; const aText: UnicodeString);
    public
      destructor Destroy; override;
      property Text: UnicodeString read fText;
      property Time: TDateTime read fTime;

    private // IOutput
      function get_Article: ITestArticle;
    public
      property Article: ITestArticle read get_Article;
    end;


    TOutputList = class
    private
      fItems: TObjectList;
      fReplaced: TObjectList;
      function get_Count: Integer;
      function get_Item(const aIndex: Integer): IOutput;
    protected
      procedure Add(const aOutput: TOutput);
      procedure Replace(const aOld, aNew: TOutput);
    public
      constructor Create;
      destructor Destroy; override;
      property Count: Integer read get_Count;
      property Items[const aIndex: Integer]: IOutput read get_Item; default;
    end;


    TAlert = class(TOutput, IAlert)
    private // IAlert
      function get_Text: UnicodeString;
    public
      property Text: UnicodeString read get_Text;
    end;


    TInformation = class(TOutput, IInformation)
    private // IInformation
      function get_Text: UnicodeString;
    public
      property Text: UnicodeString read get_Text;
    end;


    TInspection = class(TOutput, IInspection)
    private
      fMonoSpaced: Boolean;
      fSubject: UnicodeString;
      fItemLabels: array of UnicodeString;
      fItemValues: array of UnicodeString;
      constructor Create(const aDelegate: TTestDelegate; const aSubject, aValue: UnicodeString; const aMonoSpaced: Boolean = FALSE);
      procedure AddItem(const aName, aValue: UnicodeString);

    private // IInspection
      function get_ItemCount: Integer;
      function get_ItemLabel(const aIndex: Integer): UnicodeString;
      function get_ItemValue(const aIndex: Integer): UnicodeString;
      function get_MonoSpaced: Boolean;
      function get_Subject: UnicodeString;
      function get_Value: UnicodeString;
    public
      property ItemCount: Integer read get_ItemCount;
      property ItemLabels[const aIndex: Integer]: UnicodeString read get_ItemLabel;
      property ItemValues[const aIndex: Integer]: UnicodeString read get_ItemValue;
      property MonoSpaced: Boolean read get_MonoSpaced;
      property Subject: UnicodeString read get_Subject;
      property Value: UnicodeString read get_Value;
    end;


    TTestResult = class(TOutput)
    private
      fOK: Boolean;
    protected
      constructor Create(const aIncident: TIncident;
                         const aMessage: UnicodeString;
                         const aOK: Boolean = TRUE);
      function get_OK: Boolean; virtual;
      procedure set_OK(const aValue: Boolean); virtual;
    public
      property OK: Boolean read get_OK write set_OK;
    end;


    TExpectation = class(TTestResult, IUnknown,
                                      IExpectation,
                                      Evaluation,
                                      IReason,
                                      IResult)
    private
      fActual: UnicodeString;
      fDescription: UnicodeString;
      fEvaluated: Boolean;
      fExpected: UnicodeString;
      fExplanation: UnicodeString;
      fInterfaceName: UnicodeString;
      fExpectedToFail: Boolean;
      fSubject: TTest;        // NOTE: Only valid during execution of the test case
      fSubjectName: UnicodeString;
      function get_SubjectName: UnicodeString;
    protected
      constructor Create(const aSubject: TTest);
      procedure set_OK(const aValue: Boolean); override;
      function DoReplaceTokens(var aString: UnicodeString): Boolean; virtual;
      function DoReplaceToken(var aString: UnicodeString; const aToken, aValue: UnicodeString): Boolean;
      procedure ReplaceInSubject(const aNew: TExpectation);
      function ReplaceToken(const aString: UnicodeString; const aToken, aValue: UnicodeString): UnicodeString;
      function ReplaceTokens(const aString: UnicodeString): UnicodeString;

    protected // Evaluation
      function get_OK: Boolean; override;
      procedure Because(const aExplanation: UnicodeString);
      function IsCritical: IReason;
      function IsRequired: IReason;
      procedure IsShowStopper;
      property OK: Boolean read get_OK write set_OK;
    protected // IExpectation
      function Supports(const aIID: TGUID; const aName: UnicodeString = ''): Evaluation;
    protected // IReason
      // procedure Because(const aExplanation: UnicodeString); // <-- already declared as part of Evaluation above
    protected // ISelfTest
      function IsExpectedToFail: IReason;

    private // IResult
      function get_Actual: UnicodeString;
      function get_Description: UnicodeString;
      function get_Evaluated: Boolean;
      function get_Expected: UnicodeString;
      function get_ExpectedToFail: Boolean;
      function get_Explanation: UnicodeString;
      function get_TestCase: ITestCase;
      function get_TestMethod: ITestMethod;
    protected
      property Actual: UnicodeString read get_Actual write fActual;
      property Description: UnicodeString read fDescription write fDescription;
      property Expected: UnicodeString read get_Expected write fExpected;
      property Subject: TTest read fSubject;
      property SubjectName: UnicodeString read get_SubjectName write fSubjectName;
    public
      property Explanation: UnicodeString read fExplanation;
      property Evaluated: Boolean read fEvaluated;
      property ExpectedToFail: Boolean read fExpectedToFail;
    end;



    TTestRun = class
    private
      fName: UnicodeString;
      fStartTime: TDateTime;
      fEndTime: TDateTime;
      fOutput: TOutputList;
      function get_Name: UnicodeString;
      procedure set_EndTime(const aValue: TDateTime);
      procedure set_Name(const aValue: UnicodeString);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      property Name: UnicodeString read get_Name write set_Name;
      property StartTime: TDateTime read fStartTime;
      property EndTime: TDateTime read fEndTime write set_EndTime;
      property Output: TOutputList read fOutput;
    end;



    DoubleInspector = interface
    ['{70BE01B5-34FF-4602-8A4D-0D707CCE0044}']
      procedure Value(aValue: Double);
    end;


    CurrencyInspector = interface
    ['{EA01AFC3-E89D-40E3-BE3C-DD66401EE9FC}']
      procedure Value(aValue: Currency);
    end;


    DoubleTest = interface
    ['{A5F51704-AAD5-4A33-BF7B-76226FA60A7D}']
      function Expect(aValue: Double): DoubleExpectation;
    end;

    CurrencyTest = interface
    ['{20093DEE-6EA8-4CD4-8624-3FA6E419E9A8}']
      function Expect(aValue: Currency): CurrencyExpectation;
    end;

    ColorTest = interface
    ['{4D7E5C81-BF25-44D7-91F8-A412B50D7C60}']
      function Expect(aValue: TColor): ColorExpectation;
    end;


    ColorExpectation = interface
    ['{94A59F39-8BAE-4078-9BCE-30F60FED9816}']
      function Equals(aExpected: TColor): Evaluation;
    end;

    DateExpectation = interface
    ['{106167D2-9197-4EA8-9577-E7D3C12FC74C}']
      function Year: CardinalExpectation;
      function Month: CardinalExpectation;
      function Day: CardinalExpectation;
      function DayOfMonth: CardinalExpectation;
      function DayOfWeek: CardinalExpectation;
      function DayOfYear: CardinalExpectation;
      function WeekOfMonth: CardinalExpectation;
      function WeekOfYear: CardinalExpectation;
      function Equals(const aValue: TDate): Evaluation; overload;
      function Equals(const aYear, aMonth: Word; const aDay: Word = NO_DAYS): Evaluation; overload;
      function IsBetween(const aEarliest, aLatest: TDate): Evaluation;
      function IsInRange(const aEarliest, aLatest: TDate): Evaluation;
      function IsAfter(const aValue: TDate): Evaluation;
      function IsBefore(const aValue: TDate): Evaluation;
      function IsNotAfter(const aValue: TDate): Evaluation;
      function IsNotBefore(const aValue: TDate): Evaluation;
    end;

    TimeExpectation = interface
    ['{A6B5E44F-04BE-4687-A7DC-19E133F1A690}']
      function Hour: CardinalExpectation;
      function Minute: CardinalExpectation;
      function Second: CardinalExpectation;
      function Millisecond: CardinalExpectation;
      function Equals(const aValue: TTime): Evaluation; overload;
      function Equals(const aHour, aMinute: Word; const aSecond: Word = NO_SECONDS; const aMillisecond: Word = NO_MILLISECONDS): Evaluation; overload;
      function IsBetween(const aEarliest, aLatest: TTime): Evaluation;
      function IsInRange(const aEarliest, aLatest: TTime): Evaluation;
      function IsAfter(const aValue: TTime): Evaluation;
      function IsBefore(const aValue: TTime): Evaluation;
      function IsNotAfter(const aValue: TTime): Evaluation;
      function IsNotBefore(const aValue: TTime): Evaluation;
    end;

    DateTimeExpectation = interface
    ['{64A0E575-95CE-40CE-8DE4-52071F37B897}']
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
      function Equals(const aValue: TDateTime): Evaluation;
      function HasDate(const aYear, aMonth, aDay: Word): Evaluation;
      function HasTime(const aHour, aMinute, aSecond: Word; const aMillisecond: Word = NO_MILLISECONDS): Evaluation;
      function IsBetween(const aEarliest, aLatest: TDateTime): Evaluation;
      function IsInRange(const aEarliest, aLatest: TDateTime): Evaluation;
      function IsAfter(const aValue: TDateTime): Evaluation;
      function IsBefore(const aValue: TDateTime): Evaluation;
      function IsNotAfter(const aValue: TDateTime): Evaluation;
      function IsNotBefore(const aValue: TDateTime): Evaluation;
    end;

    BooleanExpectation = interface
    ['{7994B0AD-E585-4E6B-B553-45BF68FC9673}']
      function Equals(const aExpected: Boolean): Evaluation;
      function IsFALSE: Evaluation;
      function IsTRUE: Evaluation;
    end;

    SingleExpectation = interface
    ['{59431CA7-74C0-4CC5-AF55-C6993E01B404}']
      function ToDPs(const aDPs: Integer): ApproximateExpectation;
      function Between(const aMin, aMax: Single): Evaluation;
      function Equals(const aExpected: Single): Evaluation;
      function GreaterThan(const aValue: Single): Evaluation;
      function InRange(const aMin, aMax: Single): Evaluation;
      function LessThan(const aValue: Single): Evaluation;
      function NotGreaterThan(const aValue: Single): Evaluation;
      function NotLessThan(const aValue: Single): Evaluation;
    end;

    DoubleExpectation = interface
    ['{6694AE20-A4F2-4109-936D-D1441D2D84F1}']
      function ToDPs(const aDPs: Integer): ApproximateExpectation;
      function Between(const aMin, aMax: Double): Evaluation;
      function Equals(const aExpected: Double): Evaluation;
      function GreaterThan(const aValue: Double): Evaluation;
      function InRange(const aMin, aMax: Double): Evaluation;
      function LessThan(const aValue: Double): Evaluation;
      function NotGreaterThan(const aValue: Double): Evaluation;
      function NotLessThan(const aValue: Double): Evaluation;
    end;

    ExtendedExpectation = interface
    ['{649B91FD-3C11-4CAC-9B32-930EF43799CA}']
      function ToDPs(const aDPs: Integer): ApproximateExpectation;
      function Between(const aMin, aMax: Extended): Evaluation;
      function Equals(const aExpected: Extended): Evaluation;
      function GreaterThan(const aValue: Extended): Evaluation;
      function InRange(const aMin, aMax: Extended): Evaluation;
      function LessThan(const aValue: Extended): Evaluation;
      function NotGreaterThan(const aValue: Extended): Evaluation;
      function NotLessThan(const aValue: Extended): Evaluation;
    end;

    CurrencyExpectation = interface
    ['{A8A63852-E546-4803-9C9C-DD3C32E7B0F4}']
      function ToDPs(const aDPs: Integer): ApproximateExpectation;
      function Between(const aMin, aMax: Currency): Evaluation;
      function Equals(const aExpected: Currency): Evaluation;
      function GreaterThan(const aValue: Currency): Evaluation;
      function InRange(const aMin, aMax: Currency): Evaluation;
      function LessThan(const aValue: Currency): Evaluation;
      function NotGreaterThan(const aValue: Currency): Evaluation;
      function NotLessThan(const aValue: Currency): Evaluation;
    end;

    ApproximateExpectation = interface
    ['{263F3DF3-2FAF-4F7E-8928-9BEB6EF02993}']
      function Between(const aMin, aMax: Currency): Evaluation;
      function Equals(const aExpected: Currency): Evaluation;
      function GreaterThan(const aValue: Currency): Evaluation;
      function InRange(const aMin, aMax: Currency): Evaluation;
      function LessThan(const aValue: Currency): Evaluation;
      function NotGreaterThan(const aValue: Currency): Evaluation;
      function NotLessThan(const aValue: Currency): Evaluation;
    end;

    CardinalExpectation = interface
    ['{20D6EB9E-5A1A-45F1-874B-A8E5C45920BF}']
      function Between(const aMin, aMax: Cardinal): Evaluation;
      function Equals(const aExpected: Cardinal): Evaluation;
      function GreaterThan(const aValue: Cardinal): Evaluation;
      function InRange(const aMin, aMax: Cardinal): Evaluation;
      function LessThan(const aValue: Cardinal): Evaluation;
      function NotGreaterThan(const aValue: Cardinal): Evaluation;
      function NotLessThan(const aValue: Cardinal): Evaluation;
    end;

    IntegerExpectation = interface
    ['{9C6AD58F-66FF-45EF-BDFD-422F5E48B0DE}']
      function Between(const aMin, aMax: Int64): Evaluation;
      function Equals(const aExpected: Int64): Evaluation;
      function GreaterThan(const aValue: Int64): Evaluation;
      function InRange(const aMin, aMax: Int64): Evaluation;
      function LessThan(const aValue: Int64): Evaluation;
      function NotGreaterThan(const aValue: Int64): Evaluation;
      function NotLessThan(const aValue: Int64): Evaluation;
    end;

    InterfaceExpectation = interface
    ['{33580CD0-F7D6-44B5-ADDA-6D91BBD1FC29}']
      function Equals(const aExpected: IInterface): Evaluation;
      function IsAssigned: Evaluation;
      function IsNIL: Evaluation;
      function Supports(const aIID: TGUID; const aName: UnicodeString = ''): Evaluation;
    end;

    ObjectExpectation = interface
    ['{25B328D3-56ED-4AEA-8BD1-9FAC858972E0}']
      function Equals(const aRef: TObject): Evaluation;
      function IsAssigned: Evaluation;
      function IsNIL: Evaluation;
      function IsInstanceOf(const aClass: TClass): Evaluation;
      function Supports(const aIID: TGUID; const aName: UnicodeString = ''): Evaluation;
    end;

    PointerExpectation = interface
    ['{1A976BAD-55E7-4C59-B460-44EB15ED66F0}']
      function Equals(const aPointer: Pointer): Evaluation; overload;
      function Equals(const aPointer: Pointer; const aBytes: Integer): Evaluation; overload;
      function IsAssigned: Evaluation;
      function IsNIL: Evaluation;
    end;

    GUIDExpectation = interface
    ['{44736B59-A50C-4274-A3CF-D2A65663F8E5}']
      function Equals(const aValue: TGUID): Evaluation;
      function IsNull: Evaluation;
    end;

    TextExpectation = interface
    ['{71DA4178-A71B-451B-B5EE-5D5B29C92FA7}']
      function BeginsWith(const aString: String): Evaluation; overload;
      function Contains(const aSubString: String): Evaluation; overload;
      function EndsWith(const aString: String): Evaluation; overload;
      function Equals(const aExpected: String): Evaluation; overload;
    {$ifdef UNICODE}
      function BeginsWith(const aString: ANSIString): Evaluation; overload;
      function Contains(const aSubString: ANSIString): Evaluation; overload;
      function EndsWith(const aString: ANSIString): Evaluation; overload;
      function Equals(const aExpected: ANSIString): Evaluation; overload;
    {$else}
      function BeginsWith(const aString: UnicodeString): Evaluation; overload;
      function Contains(const aSubString: UnicodeString): Evaluation; overload;
      function EndsWith(const aString: UnicodeString): Evaluation; overload;
      function Equals(const aExpected: UnicodeString): Evaluation; overload;
    {$endif}
    end;

    StringExpectation = interface(TextExpectation)
    ['{6BDDE890-3AB1-4364-BB66-57711107F7B7}']
      function CaseInsensitive: TextExpectation;
      function IsEmpty: Evaluation;
      function IsLowercase: Evaluation;
      function IsUppercase: Evaluation;
      function Length: IntegerExpectation;
    end;

    UTF8TextExpectation = interface
    ['{EF242137-A5C7-4209-85C1-153BCCDC3D3F}']
      function BeginsWith(const aString: UTF8String): Evaluation; overload;
      function Contains(const aSubString: UTF8String): Evaluation; overload;
      function EndsWith(const aString: UTF8String): Evaluation; overload;
      function Equals(const aExpected: UTF8String): Evaluation; overload;

      function BeginsWith(const aString: UnicodeString): Evaluation; overload;
      function Contains(const aSubString: UnicodeString): Evaluation; overload;
      function EndsWith(const aString: UnicodeString): Evaluation; overload;
      function Equals(const aExpected: UnicodeString): Evaluation; overload;
    end;

    UTF8Expectation = interface(UTF8TextExpectation)
    ['{3556D2CE-D49A-41C4-96FC-E4BA209CB10E}']
      function CaseInsensitive: UTF8TextExpectation;
      function IsEmpty: Evaluation;
      function IsLowercase: Evaluation;
      function IsUppercase: Evaluation;
      function Length: IntegerExpectation;
    end;

    ExceptionExpectation = interface
    ['{55134E9C-2B14-41C3-BFA8-FB95B7159408}']
      function get_Exception: Exception;
      function get_ExceptionClass: TExceptionClass;
      property Exception: Exception read get_Exception;
      property ExceptionClass: TExceptionClass read get_ExceptionClass;
    end;

    DateTest = interface
    ['{908BBD0F-E493-4468-BABD-2D09C2DB9218}']
      function Expect(aValue: TDate): DateExpectation;
    end;

    TimeTest = interface
    ['{6672B519-52AB-46BE-A1CA-1ADAE233D3EE}']
      function Expect(aValue: TTime): TimeExpectation;
    end;

    DateTimeTest = interface
    ['{2CE43D88-5A59-4996-BFF3-A8CA46921C65}']
      function Expect(aValue: TDateTime): DateTimeExpectation;
    end;

    UTF8Test = interface
    ['{9E03FACF-CF44-472D-91E6-103123B3BE65}']
      function Expect(aValue: UTF8String): UTF8Expectation;
    end;

    EnumExpectations = interface
    ['{48C6B934-DFED-46F3-8217-76B60E9C8487}']
//      function Equals(const aValue): Evaluation; overload;
      function Equals(aValue: Cardinal): Evaluation; overload;
    end;

    SpecificEnumTest = interface
    ['{118FFC65-97E6-46DE-B9E2-6DD2AF8C9D42}']
//      function Expect(aValue: Cardinal): EnumExpectations;
      function Expect(const aValue): EnumExpectations;
    end;

    EnumTest = interface
    ['{E6C50FDF-2085-4746-901B-579CFE603443}']
      function ForEnum(aTypeInfo: PTypeInfo): SpecificEnumTest;
    end;




  const
    // States supported by all test objects
    tsInitialising  : TStateID = 'tsInitialising';
    tsRunning       : TStateID = 'tsRunning';
    tsError         : TStateID = 'tsError';
    tsAborted       : TStateID = 'tsAborted';

    // Additional states supported by test cases
    tsDisabled        : TStateID = 'tsDisabled';
    tsPassed          : TStateID = 'tsPassed';
    tsFailed          : TStateID = 'tsFailed';
    tsNotImplemented  : TStateID = 'tsNotImplemented';


  function Smoketest: ISmoketestSetup;


implementation

{$undef LeakReport} // Just until I get around to implementing a proper leak report mechanism

  uses
  {$ifdef LeakReport}
    FastMM4,
  {$endif}
  { vcl: }
    ActiveX,
    ComObj,
    Forms,
    Math,
    Messages,
    Types,
    Windows,
  { deltics: }
    Deltics.Forms,
    Deltics.HPC,
    Deltics.MessageHandler,
//    Deltics.Progress,
    Deltics.RTTI,
    Deltics.StrUtils,
    Deltics.Threads,
  { smoketest: }
    Deltics.Smoketest.Console,
    Deltics.Smoketest.Expectations,
    Deltics.Smoketest.FileWriter,
    Deltics.Smoketest.SplashScreen;


  const
    CMDLINE_AutoRun                 = '-r';
    CMDLINE_DisableCases            = '-d';
    CMDLINE_DisablePerformanceCases = '-dp';
    CMDLINE_NoOutput                = '-no';
    CMDLINE_OutputToFile            = '-f';
    CMDLINE_OutputJSON              = '-json';
    CMDLINE_OutputPlainText         = '-text';
    CMDLINE_OutputXML               = '-xml';
    CMDLINE_SilentRunning           = '-s';
    CMDLINE_ThreadIsolation         = '-ti';
    CMDLINE_VerboseOutput           = '-v';

  const
    ALL_TESTS = -1;
    NOT_FOUND = -1;

    STM_START   = WM_USER;
    STM_FINISH  = WM_USER + 1;
    STM_UPDATE  = WM_USER + 2;



  type
    TTestExtension = class
    private
      IID: TGUID;
      InspectorClass: TInspectorClass;
      TestClass: TExpectationClass
    end;

  var
    _InitCase: TTestCase;

    _Extensions: TObjectList = NIL;
    _Suite: TSmoketest = NIL;


  type
    TVCLNotifier = class(TMessageHandler)
      procedure SendStartNotification;
      procedure SendFinishNotification;
      procedure SendUpdateNotification(const aArticle: TTestArticle);

      procedure STMStart(var aMessage: TMessage); message STM_START;
      procedure STMFinish(var aMessage: TMessage); message STM_FINISH;
      procedure STMUpdate(var aMessage: TMessage); message STM_UPDATE;
    end;


  procedure TVCLNotifier.SendStartNotification;
  begin
    SendMessage(STM_START, 0, 0);
  end;


  procedure TVCLNotifier.SendFinishNotification;
  begin
    SendMessage(STM_FINISH, 0, 0);
  end;


  procedure TVCLNotifier.SendUpdateNotification(const aArticle: TTestArticle);
  begin
    SendMessage(STM_UPDATE, WParam(aArticle), 0);
  end;


  procedure TVCLNotifier.STMStart(var aMessage: TMessage);
  begin
    _Suite.On_Started.DoEvent;
  end;


  procedure TVCLNotifier.STMFinish(var aMessage: TMessage);
  begin
    _Suite.On_Finished.DoEvent;
  end;


  procedure TVCLNotifier.STMUpdate(var aMessage: TMessage);
  begin
    _Suite.On_Update.DoEventFor(TTestArticle(aMessage.WParam));
  end;


  var
    _VCL: TVCLNotifier = NIL;


  type
    TExpectedFailureRegistrar = class(TCOMInterfacedObject, INextTest,
                                                            INextNTests,
                                                            ITheseTests)
    private
      fDelegate: TTestDelegate;
      fN: Integer;
    public
      constructor Create(const aDelegate: TTestDelegate; const aNumber: Integer);

    public  // INextTest
      procedure IsExpectedToFail;
    protected // INextNTests
      function Tests: ITheseTests;
    protected // ITheseTests
      procedure AreExpectedToFail;
    end;


  constructor TExpectedFailureRegistrar.Create(const aDelegate: TTestDelegate;
                                               const aNumber: Integer);
  begin
    inherited Create;
    fDelegate := aDelegate;
    fN        := aNumber;
  end;


  procedure TExpectedFailureRegistrar.IsExpectedToFail;
  begin
    if (fDelegate.fExpectedFailures) = ALL_TESTS then
      fDelegate.fExpectedFailures := 0;

    Inc(fDelegate.fExpectedFailures);
  end;


  function TExpectedFailureRegistrar.Tests: ITheseTests;
  begin
    result := self;
  end;


  procedure TExpectedFailureRegistrar.AreExpectedToFail;
  begin
    if (fDelegate.fExpectedFailures) = ALL_TESTS then
      fDelegate.fExpectedFailures := 0;

    fDelegate.fExpectedFailures := fN;
  end;


  type
    TPerformanceAdder = class(TCOMInterfacedObject, IPerformanceCaseAddition,
                                                    IPerformanceCaseDuration)
    private
      fHasBeenAdded: Boolean;
      fCases: array of TPerformanceCaseClass;
      fN: Integer;
      fRepeats: Integer;
    public // IPerformanceCaseAddition
      function RunningFor(const aN: Integer): IPerformanceCaseDuration;
    public // IPerformanceCaseDuration
      procedure Seconds;
      procedure Iterations;
    public
      constructor Create(const aCases: array of TPerformanceCaseClass; const aRepeats: Integer = 1);
      destructor Destroy; override;
    end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TPerformanceAdder.Create(const aCases: array of TPerformanceCaseClass;
                                       const aRepeats: Integer);
  var
    i: Integer;
  begin
    inherited Create;

    fRepeats := aRepeats;
    SetLength(fCases, Length(aCases));

    for i := 0 to Pred(Length(aCases)) do
      fCases[i] := aCases[i];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TPerformanceAdder.Destroy;
  begin
    if NOT fHasBeenAdded then
      Iterations;

    inherited Destroy;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceAdder.RunningFor(const aN: Integer): IPerformanceCaseDuration;
  begin
    fN := aN;
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceAdder.Seconds;
  var
    i: Integer;
  begin
    for i := Low(fCases) to High(fCases) do
      fCases[i].Create(fN, pmSeconds, fRepeats);

    fHasBeenAdded := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceAdder.Iterations;
  var
    i: Integer;
  begin
    for i := Low(fCases) to High(fCases) do
      fCases[i].Create(fN, pmIterations, fRepeats);

    fHasBeenAdded := TRUE;
  end;




{ ------------------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function Smoketest: ISmoketestSetup;
  begin
    if NOT _Suite.IsInitialised then
      _Suite.Initialize{([])};

    result := _Suite;
  end;



{ ------------------------------------------------------------------------------------------------ }

  type
    TRunThread = class(TWorkerThread)
    private
//      fRootObject: TTestArticle;
//      procedure set_RootObject(const aValue: TTestArticle);
    protected
      procedure Execute; override;
      procedure OnTerminate; override;
//      property RootObject: TTestArticle read fRootObject write set_RootObject;
    end;


(*
  procedure TRunThread.set_RootObject(const aValue: TTestArticle);
  begin
    ASSERT(NOT Assigned(fRootObject));
    fRootObject := aValue;
  end;
*)


  procedure TRunThread.Execute;

    function TotalTestObjects(const aObject: ITestArticle): Integer;
    var
      i: Integer;
    begin
      result := 1;
      for i := 0 to Pred(aObject.Count) do
        result := result + TotalTestObjects(aObject[i]);
    end;

  begin
    CoInitializeEx(NIL, COINIT_MULTITHREADED);
    try
      _Suite.StartRun;
      try
        _Suite.Initialise;
        _Suite.Execute;

      finally
        _Suite.EndRun;
      end;

    finally
      CoUninitialize;
    end;
  end;



  procedure TRunThread.OnTerminate;
  begin
    inherited;
//    fRootObject := NIL;
  end;








{ TTestArticle -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestArticle.CreateEx;
  begin
    inherited Create;

    if self is TSmoketest then
      fArticleType := atSmoketest
    else if self is TTestCase then
      fArticleType := atTestCase
    else if self is TPerformanceCase then
      fArticleType := atPerformanceCase
    else if self is TTestDelegate then
      fArticleType := atTestDelegate
    else if self is TPerformanceDelegate then
      fArticleType := atPerformanceDelegate;

    fOn_Destroy := TOnDestroy.Create(self);

    fState := TStateList.Create(self, [tsInitialising,
                                       tsRunning,
                                       tsError,
                                       tsAborted]);
    fState.On_Change.Add(OnStateChanged);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestArticle.Create(const aName: UnicodeString = '');
  begin
    Create(TTestArticle(NIL), aName);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestArticle.Create(const aOwner: TTestArticle;
                                  const aName: UnicodeString);
  begin
    CreateEx;

    fName   := aName;
    fOwner  := aOwner;

    if NOT Assigned(Owner) and (self <> _Suite) then
      fOwner := _Suite;

    if Assigned(Owner) then
      Owner.Add(self);

    fEnabled  := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTestArticle.Destroy;
  begin
    FreeAndNIL(fState);
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.Find(const aPath: UnicodeString): TTestArticle;
  var
    i: Integer;
    delimPos: Integer;
    name: UnicodeString;
    childName: UnicodeString;
    remainingName: UnicodeString;
    candidate: TTestArticle;
  begin
    result := NIL;

    name      := aPath;
    delimPos  := Pos('\', name);
    if delimPos <> 0 then
    begin
      childName     := Copy(aPath, 1, delimPos - 1);
      remainingName := Copy(aPath, delimPos + 1, Length(aPath) - delimPos);
    end
    else
      childName := name;

    for i := 0 to Pred(Count) do
    begin
      Child[i].AsObject(candidate);
      if (candidate.Reference = childName)
       or ANSISameText(candidate.Name, childName)
       or ANSISameText(candidate.DisplayName, childName) then
      begin
        result := candidate;
        BREAK;
      end;
    end;

    if Assigned(result) and (remainingName <> '') then
      result := result.Find(remainingName);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_ArticleType: TTestArticleType;
  begin
    result := fArticleType;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_AsObject: TObject;
  begin
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_DisplayName: UnicodeString;
  begin
    if (fDisplayName = '') then
      fDisplayName := SetDisplayName;

    result := fDisplayName;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_EffectivelyEnabled: Boolean;
  begin
    result := fEnabled and (NOT Assigned(Owner) or Owner.EffectivelyEnabled);

    if (fArticleType in [atPerformanceCase, atTestCase]) then
      result := result and (Count > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_Elapsed: Int64;
  begin
    result := fElapsed;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_Enabled: Boolean;
  begin
    result := fEnabled;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_HasErrors: Boolean;
  begin
    result := State.Supports(tsError) and State.InState[tsError];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_HasFailures: Boolean;
  begin
    result := State.Supports(tsFailed) and State.InState[tsFailed];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_ID: Int64;
  begin
    result := Int64(self);
  end;

  
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_IsRunning: Boolean;
  begin
    result := State.InState[tsRunning];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_Name: UnicodeString;
  begin
    if (fName = '') then
    begin
      fName := ClassName;
      if (fName[1] = 'T') and WIDE.IsUppercase(fName[2]) then
        Delete(fName, 1, 1);

      fName := CamelCapsToWords(fName);
    end;

    result := fName;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_Parent: ITestArticle;
  begin
    result := fOwner as ITestArticle;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.get_Reference: UnicodeString;
  begin
    result := '';
    if NOT Assigned(self) or NOT Assigned(Owner) then
      EXIT;

    result := Owner.Reference;
    if (result <> '') then
      result := result + '.';

    result := result + IntToStr(Owner.IndexOf(self) + 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.IndexOf(const aChild: TTestArticle): Integer;
  begin
    for result := 0 to Pred(Count) do
      if (Child[result].ID = aChild.ID) then
        EXIT;

    result := -1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.NameForConsole: UnicodeString;
  var
    i: Integer;
    o: TTestArticle;
  begin
    i := 0;
    o := Owner;
    while Assigned(o) do
    begin
      Inc(i);
      o := o.Owner;
    end;

    result := StringOfChar(' ', i);
    result := result + result;

    case ArticleType of
      atTestCase        : result := '?- ' + result + PlainReference + ' ' + DisplayName;
      atPerformanceCase : result := '%- ' + result + PlainReference + ' ' + DisplayName;
    else
      result := result + ' > ' + PlainReference + ' ' + DisplayName;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestArticle.NotifyChange;
  begin
    if Assigned(_VCL) then
      _VCL.SendUpdateNotification(self);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestArticle.Add(const aObject: TTestArticle);
  begin
    aObject.On_Destroy.Add(OnObjectDestroyed);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestArticle.AddElapsed(const aValue: Cardinal);
  begin
    if (aValue = 0) then
      EXIT;

    Inc(fElapsed, aValue);
    NotifyChange;

    if Assigned(Owner) then
      Owner.AddElapsed(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestArticle.OnObjectDestroyed(Sender: TObject);
  begin
    Remove(TTestArticle(Sender));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestArticle.OnStateChanged(aSender: TObject;
                                        const aStateID: TStateID);
  begin
    NotifyChange;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.PlainReference: UnicodeString;
  begin
    result := '';
    if NOT Assigned(self) or NOT Assigned(Owner) then
      EXIT;

    if Assigned(Owner.Owner) then
      result := IntToStr(Owner.Owner.IndexOf(Owner) + 1);

    if (result <> '') then
      result := result + '.';

    result := result + IntToStr(Owner.IndexOf(self) + 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.SetDisplayName: UnicodeString;
  begin
    result := fName;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestArticle.set_Enabled(const aValue: Boolean);
  begin
    if fEnabled = aValue then
      EXIT;

    fEnabled := aValue;

    case Enabled of
      TRUE  : State.Leave(tsDisabled);
      FALSE : State.Enter(tsDisabled);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestArticle.AsObject(var aRef; const aClass: TClass): Boolean;
  var
    this: TObject absolute aRef;
  begin
    if (aClass = NIL)
     or (self.ClassType = aClass)
     or (self.InheritsFrom(aClass)) then
      this := self
    else
      this := NIL;

    result := Assigned(this);
  end;












{ TSmoketestCommandLine -------------------------------------------------------------------------  }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TSmoketestCommandLine.Destroy;
  begin
    FreeAndNIL(fDisableList);
    FreeAndNIL(fRunList);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketestCommandLine.Define;
  begin
    // These have (optional) lists of case names/references
    DefineSwitch(CMDLINE_AutoRun);
    DefineSwitch(CMDLINE_DisableCases);

    // This is a switch having an optional value with a default
    DefineSwitch(CMDLINE_OutputToFile, ChangeFileExt(ExtractFilename(ParamStr(0)), ''));

    // The rest are simple switches that may work in conjunction with the previous
    //  switches or each other
    DefineSwitch(CMDLINE_DisablePerformanceCases);
    DefineSwitch(CMDLINE_VerboseOutput);
    DefineSwitch(CMDLINE_OutputXML);
    DefineSwitch(CMDLINE_OutputJSON);
    DefineSwitch(CMDLINE_OutputPlainText);
    DefineSwitch(CMDLINE_SilentRunning);
    DefineSwitch(CMDLINE_ThreadIsolation);

    DefineSwitch(CMDLINE_NoOutput); // Overrides any/all other output settings to disable any output
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_AutoRun: Boolean;
  begin
    result := Switch[CMDLINE_AutoRun].Specified;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_DisableCases: Boolean;
  begin
    result := Switch[CMDLINE_DisableCases].Specified;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_DisablePerformanceCases: Boolean;
  begin
    result := Switch[CMDLINE_DisablePerformanceCases].Specified;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_NoOutput: Boolean;
  begin
    result := Switch[CMDLINE_NoOutput].Specified;

    result := result
           or (IsConsole and (NOT OutputToConsole) and (OutputFilename = ''));

    result := result
           or (NOT IsConsole and (OutputFilename = ''));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_OutputFilename: UnicodeString;
  var
    ext: UnicodeString;
  begin
    result := '';

    if Switch[CMDLINE_OutputToFile].Specified then
      result := Switch[CMDLINE_OutputToFile].Value;

    if (result <> '') then
    begin
      ext := ExtractFileExt(result);

      // If there is a valid file extension on the default filename (txt, json, xml)
      //  then we will change it according to the specified file format
      //
      // Otherwise we ADD the appropriate extension

      if StringIndex(ext, ['json', 'txt', 'xml']) <> -1 then
      begin
        if OutputJSON then
          result := ChangeFileExt(result, '.json')
        else if OutputPlainText then
          result := ChangeFileExt(result, '.txt')
        else
          result := ChangeFileExt(result, '.xml');
      end
      else if OutputJSON then
        result := result + '.json'
      else if OutputPlainText then
        result := result + '.txt'
      else
        result := result + '.xml';
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_OutputToConsole: Boolean;
  begin
    result := NOT NoOutput and NOT Switch[CMDLINE_OutputToFile].Specified;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_OutputJSON: Boolean;
  begin
    result := Switch[CMDLINE_OutputJSON].Specified;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_OutputPlainText: Boolean;
  begin
    result := Switch[CMDLINE_OutputPlainText].Specified;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_OutputXML: Boolean;
  begin
    result := NOT NoOutput
               and (Switch[CMDLINE_OutputXML].Specified
                    or ((NOT OutputPlainText) and (NOT OutputJSON)));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_DisableList: TStringList;
  var
    sw: TCommandLineSwitch;
  begin
    if NOT Assigned(fDisableList) then
    begin
      fDisableList := TStringList.Create;

      sw := Switch[CMDLINE_DisableCases];
      if Assigned(sw.Values) then
        fDisableList.Assign(sw.Values);
    end;

    result := fDisableList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_RunList: TStringList;
  var
    sw: TCommandLineSwitch;
  begin
    if NOT Assigned(fRunList) then
    begin
      fRunList := TStringList.Create;

      sw := Switch[CMDLINE_AutoRun];
      if Assigned(sw.Values) then
        fRunList.Assign(sw.Values);
    end;

    result := fRunList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_SilentRunning: Boolean;
  begin
    result := Switch[CMDLINE_SilentRunning].Specified;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_ThreadIsolation: Boolean;
  begin
    result := Switch[CMDLINE_ThreadIsolation].Specified;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketestCommandLine.get_VerboseOutput: Boolean;
  begin
    result := Switch[CMDLINE_VerboseOutput].Specified;
  end;








{ TSmoketest ------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TSmoketest.Create;
  begin
    inherited Create;

    fName     := ChangeFileExt(ExtractFileName(Application.ExeName), '');

    fCmdLine    := TSmoketestCommandLine.Create;
    fCases      := TObjectList.Create(TRUE);
    fActiveCase := TPerThreadObjectStack.Create;

    fThread := TRunThread.CreateSuspended;
    fThread.Priority := tpNormal;
    fThread.FreeOnTerminate := FALSE;
    fThread.State.On_Change.Add(OnThreadStateChange);

    SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);

    if CommandLine.ThreadIsolation then
    begin
      SetThreadAffinityMask(GetCurrentThread, 1);
      fThread.Affinity := 2;
    end;

    TMultiCastNotify.CreateEvents(self, [@fOn_Finished,
                                         @fOn_Started,
                                         @fOn_Update]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.AfterConstruction;
  begin
    if Assigned(_Suite) then
      raise Exception.Create('Only one test suite permitted per smoke test');

    _Suite := self;

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TSmoketest.Destroy;
  begin
    FreeAndNIL(fTestRun);
    FreeAndNIL(fThread);
    FreeAndNIL(fOn_Finished);
    FreeAndNIL(fOn_Started);
    FreeAndNIL(fOn_Update);
    FreeAndNIL(fActiveCase);
    FreeAndNIL(fCases);
    FreeAndNIL(fCmdLine);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_CommandLine: TSmoketestCommandLine;
  begin
    result := fCmdLine;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_Count;
  begin
    result := fCases.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_IsInitialised: Boolean;
  begin
    result := Assigned(_Suite);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_Name: UnicodeString;
  begin
    result := fName;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_Child(const aIndex: Integer): ITestArticle;
  begin
    result := TTestArticle(fCases[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_On_Finished: TMultiCastNotify;
  begin
    result := fOn_Finished;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_On_Started: TMultiCastNotify;
  begin
    result := fOn_Started;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_On_Update: TMultiCastNotify;
  begin
    result := fOn_Update;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_TestRun: TTestRun;
  begin
    result := fTestRun;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_Thread: TWorkerThread;
  begin
    result := fThread;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.set_ActiveCase(const aValue: TCase);
  begin
    if Assigned(aValue) then
      fActiveCase.Push(aValue)
    else
      fActiveCase.Pop;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Add(const aArticle: TTestArticle);
  var
    idx: Integer;
  begin
    if (aArticle.ArticleType = atTestCase) then
    begin
      // Check for any existing performance cases - test cases
      //  are always inserted before those, so if we find one
      //  insert the test case there and bug out

      for idx := 0 to Pred(Count) do
        if (Cases[idx] is TPerformanceCase) then
        begin
          fCases.Insert(idx, aArticle);
          EXIT;
        end;
    end;

    // Either we are adding a(nother) performance case or there
    //  are no performance cases, so we can just add the case to
    //  the list of cases

    fCases.Add(aArticle);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Remove(const aArticle: TTestArticle);
  begin
    fCases.Extract(aArticle);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Abort;
  begin
    if IsConsole then
      ExitProcess(255)
    else if Thread.State[Deltics.Threads.tsRunning] then
      Thread.Terminate;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Add(const aCases: array of TTestCaseClass);
  var
    i: Integer;
  begin
    for i := Low(aCases) to High(aCases) do
      aCases[i].Create(_InitCase);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Add(const aParent: TTestCaseClass;
                           const aCases: array of TTestCaseClass);
  var
    i: Integer;
    parent: TTestCase;
  begin
    parent := TTestCase(Find(aParent));
    if NOT Assigned(parent) then
      parent := aParent.Create(NIL);

    for i := Low(aCases) to High(aCases) do
      aCases[i].Create(parent);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.FindArticle(const aPath: UnicodeString): TTestArticle;
  var
    name: UnicodeString;
    context: TTestArticle;
  begin
    result := NIL;

    name := aPath;
    if (name[1] = '\') then
    begin
      Delete(name, 1, 1);
      context := self;
    end
    else
      context := ActiveCase;

    if Assigned(context) then
      result := context.Find(name);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.FindCase(const aNamePath: UnicodeString): ITestCase;
  var
    target: TTestArticle;
  begin
    result := NIL;
    target := FindArticle(aNamePath);
    if Assigned(target) then
      target.GetInterface(ITestCase, result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.FindMethod(const aNamePath: UnicodeString): ITestMethod;
  var
    target: TTestArticle;
  begin
    result := NIL;
    target := FindArticle(aNamePath);
    if Assigned(target) then
      target.GetInterface(ITestMethod, result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.AverageTime(const aCases: array of TPerformanceCaseClass;
                                  const aRepeats: Integer): IPerformanceCaseAddition;
  begin
    result := TPerformanceAdder.Create(aCases, aRepeats);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.Time(const aCases: array of TPerformanceCaseClass): IPerformanceCaseAddition;
  begin
    result := TPerformanceAdder.Create(aCases);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.EndRun;
  begin
    fTestRun.EndTime := Now;

    if NOT CommandLine.NoOutput then
      OutputResults;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Execute;
  var
    i: Integer;
  begin
    State.Enter(tsRunning);
    try
      for i := 0 to Pred(CaseCount) do
        Cases[i].Execute;

    finally
      State.Leave(tsRunning);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.Find(const aCase: TCaseClass): TCase;

    function Locate(const aObj: ITestArticle;
                    var   aResult: TCase): Boolean;
    var
      i: Integer;
    begin
      result  := TRUE;
      aResult := NIL;

      for i := 0 to Pred(aObj.Count) do
        if aObj[i].AsObject(aResult, aCase) then
          EXIT;

      for i := 0 to Pred(aObj.Count) do
        if Locate(aObj[i], aResult) then
          EXIT;

      result := FALSE;
    end;

  begin
    Locate(self, result);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Initialise;
  var
    i: Integer;
  begin
    for i := 0 to Pred(CaseCount) do
      Cases[i].Initialise;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Initialize;
  begin
    TSmoketest.Create;

    Application.Initialize;
  end;

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_ActiveCase: TCase;
  begin
    result := TCase(fActiveCase.Peek);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_Case(const aIndex: Integer): TCase;
  begin
    result := TCase(fCases[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.get_CaseCount: Integer;
  begin
    result := fCases.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.OnThreadStateChange(Sender: TObject;
                                           const aStateID: TStateID);
  begin
    if (aStateID = Deltics.Threads.tsRunning) then
    begin
      if Thread.State[aStateID] then
        _VCL.SendStartNotification
      else
        _VCL.SendFinishNotification;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.OutputResults;
  begin
    if CommandLine.NoOutput then
      EXIT;

    Deltics.Smoketest.FileWriter.OutputResults;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Ready;
  begin
    if IsConsole then
    begin
      CoInitializeEx(NIL, 0);
      try
        _Suite.Startup;
        _Suite.StartRun;
        try
          _Suite.Initialise;
          _Suite.Execute;

        finally
          _Suite.EndRun;
          _Suite.Shutdown;
        end;
      finally
        CoUninitialize;
      end;
    end
    else
    begin
      _Suite.Startup;
      try
        Application.CreateForm(TSmokeTestConsole, Console);
        Console.Initialize;
        Application.Run;

      finally
        _Suite.Shutdown;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.FindExtension(const aIID: TGUID; var aClass: TExpectationClass): Boolean;
  var
    i: Integer;
    ext: TTestExtension;
  begin
    if NOT Assigned(_Extensions) then
      _Extensions := TObjectList.Create(TRUE);

    result := FALSE;

    for i := 0 to Pred(_Extensions.Count) do
    begin
      ext := TTestExtension(_Extensions[i]);

      result := SameGUID(ext.IID, aIID);
      if result then
      begin
        aClass := ext.TestClass;
        BREAK;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TSmoketest.FindExtension(const aIID: TGUID;
                                    var aClass: TInspectorClass): Boolean;
  var
    i: Integer;
    ext: TTestExtension;
  begin
    if NOT Assigned(_Extensions) then
      _Extensions := TObjectList.Create(TRUE);

    result := FALSE;

    for i := 0 to Pred(_Extensions.Count) do
    begin
      ext := TTestExtension(_Extensions[i]);

      result := SameGUID(ext.IID, aIID);
      if result then
      begin
        aClass := ext.InspectorClass;
        BREAK;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.RegisterExtension(const aIID: TGUID;
                                         const aTest: TExpectationClass);
  var
    ext: TTestExtension;
    unused: TExpectationClass;
  begin
    ASSERT(NOT FindExtension(aIID, unused), 'A test extension has already been registered with this Interface ID');

    if NOT Assigned(_Extensions) then
      _Extensions := TObjectList.Create(TRUE);

    ext := TTestExtension.Create;
    ext.IID       := aIID;
    ext.TestClass := aTest;

    _Extensions.Add(ext);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.RegisterExtension(const aIID: TGUID;
                                         const aInspector: TInspectorClass);
  var
    ext: TTestExtension;
    unused: TInspectorClass;
  begin
    ASSERT(NOT FindExtension(aIID, unused), 'An inspector has already been registered with this Interface ID');

    if NOT Assigned(_Extensions) then
      _Extensions := TObjectList.Create(TRUE);

    ext := TTestExtension.Create;
    ext.IID             := aIID;
    ext.InspectorClass  := aInspector;

    _Extensions.Add(ext);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Run;
  begin
    if Thread.State[Deltics.Threads.tsRunning] then
      EXIT;

//    TRunThread(Thread).RootObject := NIL;
    Thread.Resume;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Shutdown;
  var
    i: Integer;
  begin
    for i := 0 to Pred(CaseCount) do
      Cases[i].Shutdown;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.StartRun;
  begin
    if Assigned(fTestRun) then
      fTestRun.Clear
    else
      fTestRun := TTestRun.Create;

    fTestRun.fStartTime := Now;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TSmoketest.Startup;
  var
    i: Integer;
    c: TCase;
  begin
    for i := 0 to Pred(Count) do
    begin
      Child[i].AsObject(c, TCase);
      c.Startup;
    end;
  end;











  procedure _CreateDelegates(const aCase: TCase;
                             const aDelegateClass: TDelegateClass);
  var
    i: Integer;
    methods: TMethodList;
    methodName: UnicodeString;
    methodAddr: Pointer;
    autorun: Boolean;
    run: TStringList;
    delegate: TDelegate;
  begin
    methods := TMethodList.Create(aCase);
    try
      run     := _Suite.CommandLine.RunList;
      autorun := _Suite.CommandLine.AutoRun and (run.Count > 0);

      for i := 0 to Pred(methods.Count) do
      begin
        methodName := methods[i];
        methodAddr := aCase.MethodAddress(methodName);

        delegate := aDelegateClass.Create(aCase, methodName, methodAddr);

        if autorun then
        begin
          delegate.Enabled := run.Contains(delegate.DisplayName)
                           or run.Contains(delegate.Reference);

          if delegate.Enabled and NOT aCase.Enabled then
            aCase.Enabled := TRUE;
        end;
      end;

    finally
      methods.Free;
    end;
  end;








{ TCase ------------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCase.Create(const aParentCase: TCase);
  var
    disabled: TStringList;
    run: TStringList;
  begin
    inherited Create(aParentCase, ClassName);

    State.Add([tsDisabled, tsNotImplemented]);

    run       := _Suite.CommandLine.RunList;
    disabled  := _Suite.CommandLine.DisableList;

    Enabled := TRUE;

    if _Suite.CommandLine.AutoRun and (run.Count > 0) then
      Enabled := run.Contains(self.DisplayName)
              or run.Contains(self.Reference);

    if _Suite.CommandLine.DisableCases then
      Enabled := (disabled.Count > 0)
              and NOT (disabled.Contains(self.DisplayName))
              and NOT (disabled.Contains(self.Reference));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCase.SetDisplayName: UnicodeString;
  var
    util: INameCase;
  begin
    if GetInterface(INameCase, util) then
    begin
      result := util.NameForCase;
      EXIT;
    end;

    result := inherited SetDisplayName;

    if WIDE.BeginsWithText(result, 'TUnitTest_') then
    begin
      Delete(result, 1, 10);
    end
    else
    begin
      if (result[1] = 'T') and WIDE.IsUppercase(result[2]) then
          Delete(result, 1, 1);

      result := CamelCapsToWords(result);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCase.DoInitialise;
  var
    i: Integer;
    init: ISetupTestRun;
  begin
    if GetInterface(ISetupTestRun, init) then
      init.SetupTestRun;

    State.Leave(tsAborted);
    State.Leave(tsError);

    fStartTime  := 0;
    fElapsed    := 0;

    for i := 0 to Pred(DelegateCount) do
    begin
      Delegate[i].fElapsed := 0;
      Delegate[i].State.Leave(tsNotImplemented);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCase.DoShutdown;
  var
    init: ICleanupSmoketest;
  begin
    if GetInterface(ICleanupSmoketest, init) then
      init.CleanupSmoketest;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCase.DoStartup;
  var
    init: ISetupSmoketest;
  begin
    if GetInterface(ISetupSmoketest, init) then
      init.SetupSmoketest;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCase.Execute;
  var
    setup: ISetupTestCase;
    cleanup: ICleanupTestCase;
  begin
    if NOT EffectivelyEnabled then
      EXIT;

    State.Enter(tsRunning);
    try
      Inc(fRunCount);

      _Suite.ActiveCase := self;
      try
        if GetInterface(ISetupTestCase, setup) then
          setup.Setup;

        try
          if IsConsole and NOT _Suite.CommandLine.SilentRunning then
          begin
            WriteLn('');
            WriteLn(NameForConsole);
          end;
          DoExecute;

        finally
          if GetInterface(ICleanupTestCase, cleanup) then
            cleanup.Cleanup;

        end;

      finally
        _Suite.ActiveCase := NIL;
      end;

    finally
      State.Leave(tsRunning);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCase.get_StartTime: TDateTime;
  begin
    result := fStartTime;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCase.Initialise;
  begin
    State.Enter(tsInitialising);
    try
      DoInitialise;

    finally
      State.Leave(tsInitialising);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCase.Shutdown;
  begin
    DoShutdown;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCase.Startup;
  begin
    DoStartup;
  end;






{ TTestCase -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestCase.Create(const aParent: TTestCase);
  {
    Replaces inherited Create(TCase) constructor with type-safe constructor
  }
  begin
    inherited Create(aParent);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestCase.Create;
  begin
    inherited Create(_InitCase);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestCase.CreateEx;
  begin
    inherited CreateEx;

    fCases            := TObjectList.Create(TRUE);
    fInitialDelegates := TObjectList.Create(TRUE);
    fFinalDelegates   := TObjectList.Create(TRUE);

    fIncidents := TObjectList.Create;

    State.Add([tsAborted]);
    State.Add([tsPassed, tsFailed, tsError, tsAborted], TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTestCase.Destroy;
  begin
    FreeAndNIL([@fInitialDelegates,
                @fFinalDelegates,
                @fIncidents,
                @fCases]);
    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.AfterConstruction;
  var
    init: IHaveChildCases;
    prevInit: TTestCase;
  begin
    inherited;

    _CreateDelegates(self, TTestDelegate);

    if GetInterface(IHaveChildCases, init) then
    begin
      prevInit  := _InitCase;
      _InitCase := self;
      try
        init.AddCases;

      finally
        _InitCase := prevInit;
      end;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_CaseByIndex(const aIndex: Integer): ITestCase;
  begin
    result := TTestCase(fCases[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_CaseByName(const aName: UnicodeString): ITestCase;
  var
    i: Integer;
  begin
    for i := 0 to Pred(fCases.Count) do
    begin
      result := get_CaseByIndex(i);
      if ANSISameText(result.Name, aName)
       or ANSISameText(result.DisplayName, aName) then
        EXIT;
    end;

    result := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_CaseCount: Integer;
  begin
    result := fCases.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_Count: Integer;
  begin
    result := DelegateCount + fCases.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_Delegate(const aIndex: Integer): TDelegate;
  var
    beforeCount: Integer;
  begin
    beforeCount := fInitialDelegates.Count;

    if (aIndex < beforeCount) then
      result := TDelegate(fInitialDelegates[aIndex])
    else
      result := TDelegate(fFinalDelegates[aIndex - beforeCount]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_DelegateCount: Integer;
  begin
    result := fInitialDelegates.Count
            + fFinalDelegates.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_HasChildCases: Boolean;
  begin
    result := (get_CaseCount > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_IsAborted: Boolean;
  begin
    result := State.InState[tsAborted];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_MethodByIndex(const aIndex: Integer): ITestMethod;
  begin
    result := Delegate[aIndex] as ITestMethod;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_MethodByName(const aName: UnicodeString): ITestMethod;
  var
    i: Integer;
  begin
    for i := 0 to Pred(DelegateCount) do
    begin
      result := get_MethodByIndex(i);
      if ANSISameText(result.Name, aName)
       or ANSISameText(result.DisplayName, aName) then
        EXIT;
    end;

    result := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_MethodCount: Integer;
  begin
    result := DelegateCount;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_Child(const aIndex: Integer): ITestArticle;
  var
    beforeCount: Integer;
    caseCount: Integer;
  begin
    caseCount   := fCases.Count;
    beforeCount := fInitialDelegates.Count;

    if (aIndex < beforeCount) then
      result := TDelegate(fInitialDelegates[aIndex])
    else if (aIndex < (beforeCount + caseCount)) then
      result := TTestArticle(fCases[aIndex - beforeCount])
    else
      result := TDelegate(fFinalDelegates[aIndex - (beforeCount + caseCount)]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
//  function TTestCase.get_Parent: TTestCase;
//  begin
//    result := TTestCase(Owner);
//  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.get_Smoketest: ISmoketestMetadata;
  begin
    result := _Suite as ISmoketestMetadata;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.DoExecute;

    procedure ExecuteDelegates(const aList: TObjectList);
    var
      i: Integer;
    begin
      for i := 0 to Pred(aList.Count) do
      begin
        if State.InState[tsAborted] then
          EXIT;

        try
          TTestDelegate(aList[i]).Execute;

        finally
          fInspector := NIL;
        end;
      end;
    end;

  var
    i: Integer;
  begin
    try
      ExecuteDelegates(fInitialDelegates);

      for i := 0 to Pred(fCases.Count) do
      begin
        if State.InState[tsAborted] then
          EXIT;

        TTestCase(fCases[i]).Execute;
      end;

      ExecuteDelegates(fFinalDelegates);

    except
      on EAbortCase do { NO-OP };
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.DoInitialise;

    procedure ClearState(const aList: TStateList; const aState: TStateID);
    begin
      while aList.InState[aState] do
        aList.Leave(aState);
    end;

  var
    i: Integer;
  begin
    inherited;

    ClearState(State, tsFailed);
    ClearState(State, tsPassed);

    for i := 0 to Pred(DelegateCount) do
    begin
      ClearState(Delegate[i].State, tsFailed);
      ClearState(Delegate[i].State, tsPassed);
      ClearState(Delegate[i].State, tsError);
      ClearState(Delegate[i].State, tsAborted);
    end;

    for i := 0 to Pred(fCases.Count) do
      TTestCase(fCases[i]).Initialise;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.DoStartup;
  var
    i: Integer;
  begin
    inherited;

    for i := 0 to Pred(fCases.Count) do
      TTestCase(fCases[i]).Startup;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.DoShutdown;
  var
    i: Integer;
  begin
    inherited;

    for i := 0 to Pred(fCases.Count) do
      TTestCase(fCases[i]).Shutdown;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Passed: Boolean;
  var
    i: Integer;
  begin
    result := FALSE;

    for i := 0 to Pred(fInitialDelegates.Count) do
      if TTestDelegate(fInitialDelegates[i]).Failed then
        EXIT;

    for i := 0 to Pred(fCases.Count) do
      if NOT TTestCase(fCases[i]).Passed then
        EXIT;

    for i := 0 to Pred(fFinalDelegates.Count) do
      if TTestDelegate(fFinalDelegates[i]).Failed then
        EXIT;

    result := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.Add(const aObject: TTestArticle);
  begin
    inherited Add(aObject);

    if aObject is TCase then
      fCases.Add(aObject)
    else if aObject is TDelegate then
    begin
      if (aObject.Name[1] = '_') then
        fFinalDelegates.Add(aObject)
      else
        fInitialDelegates.Add(aObject);
    end;

    aObject.State.On_Enter.Add(ChildEnterState);
    aObject.State.On_Leave.Add(ChildLeaveState);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Inspect(aName: ANSIString): IInspector;
  begin
    result := Inspect(WIDE.FromANSI(aName));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Inspect(aName: ANSIString;
                             aArgs: array of const): IInspector;
  begin
    result := Inspect(WideFormat(WIDE.FromANSI(aName), aArgs));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Inspect(aName: UnicodeString): IInspector;
  begin
    result := TInspector.GetInspector(self, Trim(aName));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Inspect(aName: UnicodeString;
                             aArgs: array of const): IInspector;
  begin
    result := Inspect(WideFormat(aName, aArgs));
  end;


{$ifdef UNICODE}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Inspect(aName: UTF8String): IInspector;
  begin
    result := Inspect(WIDE.FromUTF8(aName));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Inspect(aName: UTF8String;
                             aArgs: array of const): IInspector;
  begin
    result := Inspect(WideFormat(WIDE.FromUTF8(aName), aArgs));
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Metadata: ITestCase;
  begin
    result := self as ITestCase;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.Alert(aMessage: UnicodeString);
  begin
    TAlert.Create(fActiveDelegate, aMessage);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.Note(aMessage: UnicodeString);
  begin
    TInformation.Create(fActiveDelegate, aMessage);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.Note(aMessage: UnicodeString; const aArgs: array of const);
  begin
    Note(WideFormat(aMessage, aArgs));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.Abort(const aMessage: UnicodeString);
  begin
    if (aMessage = '') then
      Alert('ABORTED')
    else
      Alert(aMessage);

    if Assigned(fActiveDelegate) then
      fActiveDelegate.State.Enter(tsAborted);

    raise EAbortTest.Create(aMessage);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.Abort(const aMessage: UnicodeString;
                            const aArgs: array of const);
  begin
    Abort(Format(aMessage, aArgs));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.AbortCase(const aMessage: UnicodeString);
  begin
    if (aMessage = '') then
      Alert('ABORTED')
    else
      Alert(aMessage);

    if Assigned(fActiveDelegate) then
      fActiveDelegate.State.Enter(tsAborted);

    State.Enter(tsAborted);

    raise EAbortCase.Create(aMessage);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.AbortCase(const aMessage: UnicodeString;
                                const aArgs: array of const);
  begin
    AbortCase(Format(aMessage, aArgs));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.Error(const aMessage: UnicodeString);
  begin
    if aMessage <> '' then
      Alert('ERROR: ' + aMessage)
    else
      Alert('ERROR');

    if Assigned(fActiveDelegate) then
      fActiveDelegate.State.Enter(tsError);

    State.Enter(tsError);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TheseTests: ITheseTests;
  begin
    result := TExpectedFailureRegistrar.Create(fActiveDelegate, ALL_TESTS);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TheNextTest: INextTest;
  begin
    result := TExpectedFailureRegistrar.Create(fActiveDelegate, 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TheNext(const aNumber: Integer): INextNTests;
  begin
    result := TExpectedFailureRegistrar.Create(fActiveDelegate, aNumber);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.Remove(const aObject: TTestArticle);
  begin
    if Assigned(fCases) then
      fCases.Remove(aObject);

    if Assigned(fInitialDelegates) then
      fInitialDelegates.Remove(aObject);

    if Assigned(fFinalDelegates)then
      fFinalDelegates.Remove(aObject);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.ChildEnterState(aSender: TObject; const aStateID: TStateID);
  begin
    if (aStateID = tsAborted) then
      EXIT;

    State.Enter(aStateID);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCase.ChildLeaveState(aSender: TObject; const aStateID: TStateID);
  begin
    State.Leave(aStateID);
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Test: ITest;
  begin
    result := Test('{actual}');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Test(aName: ANSIString): ITest;
  begin
    result := Test(WIDE.FromANSI(aName));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Test(aName: ANSIString;
                          aArgs: array of const): ITest;
  begin
    result := Test(WIDE.FromANSI(aName), aArgs);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Test(aName: UnicodeString): ITest;
  begin
    aName := Trim(aName);

    if (aName = '') then
      aName := '{actual}';

    result := TTest.Create(self, aName);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Test(aName: UnicodeString;
                          aArgs: array of const): ITest;
  begin
    result := Test(WideFormat(aName, aArgs));
  end;


{$ifdef UNICODE}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Test(aName: UTF8String): ITest;
  begin
    result := Test(WIDE.FromUTF8(aName));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.Test(aName: UTF8String;
                          aArgs: array of const): ITest;
  begin
    result := Test(WIDE.FromUTF8(aName), aArgs);
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TestDatetime: DatetimeTest;
  begin
    result := Test('{actual}') as DatetimeTest;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TestDatetime(aName: UnicodeString): DatetimeTest;
  begin
    result := Test(aName) as DatetimeTest;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TestDatetime(aName: UnicodeString;
                                  aArgs: array of const): DatetimeTest;
  begin
    result := TestDatetime(Format(aName, aArgs));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TestUTF8: UTF8Test;
  begin
    result := Test as UTF8Test;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TestUTF8(aName: UnicodeString): UTF8Test;
  begin
    result := Test(aName) as UTF8Test;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestCase.TestUTF8(aName: UnicodeString; aArgs: array of const): UTF8Test;
  begin
    result := Test(aName, aArgs) as UTF8Test;
  end;














{ TDelegate -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDelegate.Create(const aCase: TCase;
                               const aName: UnicodeString;
                               const aMethod: Pointer);
  begin
    inherited Create(aCase, aName);

    State.Add([tsDisabled, tsNotImplemented]);
    State.Add([tsPassed, tsFailed], TRUE);

    TMethod(fMethod).Code := aMethod;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDelegate.get_TestCase: TCase;
  begin
    result := TCase(Owner);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDelegate.SetDisplayName: UnicodeString;
  begin
    result := inherited SetDisplayName;

    if (result <> '') and (result[1] = '_') then
      Delete(result, 1, 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDelegate.Execute;
  begin
    if NOT EffectivelyEnabled then
      EXIT;

    State.Enter(tsRunning);
    try
      Inc(fRunCount);

      DoExecute;

    finally
      State.Leave(tsRunning);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDelegate.get_Count: Integer;
  begin
    result := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDelegate.get_StartTime: TDateTime;
  begin
    result := fStartTime;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TDelegate.get_Child(const aIndex: Integer): ITestArticle;
  begin
    result := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDelegate.SetStartTime;
  begin
    fStartTime  := Now;
    fElapsed    := 0;

    if (TestCase.fStartTime = 0) then
    begin
      TestCase.fStartTime := fStartTime;
      TestCase.fElapsed   := 0;
    end;
  end;







{ TTestDelegate ---------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestDelegate.SetDisplayName: UnicodeString;
  begin
    result := inherited SetDisplayName;

    if StrBeginsWithText(result, 'fn_') then
    begin
      Delete(result, 1, 3);
      result  := 'fn() ' + result;

      fDelegateType := dtFunctionTest;
    end
    else
      result := CamelCapsToWords(Name);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestDelegate.DoExecute;
  var
    start: Cardinal;
    setup: ISetupTest;
    cleanup: ICleanupTest;
    output: Integer;
  begin
    if IsConsole and NOT _Suite.CommandLine.SilentRunning then
      Write(NameForConsole + '...');

    try
      TTestCase(Owner).fActiveDelegate := self;

      if TestCase.GetInterface(ISetupTest, setup) then
        setup.SetupTest(self);
      try
        try
          TMethod(fMethod).Data := Owner;

          output := _Suite.fTestRun.fOutput.Count;

        {$ifdef LeakReport}
          FastMM4.LogMemoryManagerStateToFile(Name + '.before.txt');
        {$endif}

          SetStartTime;
          start := HPC.Value;

          Method;

          AddElapsed(HPC.Value - start);

        {$ifdef LeakReport}
          FastMM4.LogMemoryManagerStateToFile(Name + '.after.txt');
        {$endif}

          if _Suite.fTestRun.fOutput.Count = output then
            State.Enter(tsNotImplemented);

        except
          on EAbortTest do { nothing };
          on EAbortCase do raise;

          on e: Exception do
          begin
            TTestCase(Owner).Error(e.Message);
            State.Enter(tsError);
          end;
        end;

      finally
        if IsConsole and NOT _Suite.CommandLine.SilentRunning then
        begin
          if Aborted then
            WriteLn('aborted')
          else if HasPasses and NOT HasErrors and NOT HasFailures then
            WriteLn('ok')
          else if HasErrors then
            WriteLn('error')
          else if HasFailures then
            WriteLn('failed')
          else
            WriteLn('no tests performed');
        end;

        try
          if TestCase.GetInterface(ICleanupTest, cleanup) then
            cleanup.CleanupTest(self);

        finally
          TTestCase(Owner).fActiveDelegate := NIL;
        end;
      end;

    finally
      if NOT State.InState[tsError]
       and NOT State.InState[tsFailed] then
        State.Enter(tsPassed);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestDelegate.get_Aborted: Boolean;
  begin
    result := State.InState[tsAborted];
  end;


  function TTestDelegate.get_Failed: Boolean;
  begin
    result := fState.InState[tsFailed];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestDelegate.get_Failures: Integer;
  begin
    result := State.Ref[tsFailed].Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestDelegate.get_HasPasses: Boolean;
  begin
    result := fState.InState[tsPassed];
  end;


  function TTestDelegate.get_NotImplemented: Boolean;
  begin
    result := fState.InState[tsNotImplemented];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestDelegate.get_Passes: Integer;
  begin
    result := State.Ref[tsPassed].Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestDelegate.get_Passed: Boolean;
  begin
    result := fState.InState[tsPassed];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestDelegate.get_TestCase: ITestCase;
  begin
    result := Owner as ITestCase;
  end;









{ TPerformanceCase -------------------------------------------------------------------------------}

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TPerformanceCase.Create(const N: Integer;
                                      const aMode: TPerformanceMode;
                                      const aSamples: Integer);
  begin
    inherited Create;

    fN        := N;
    fMode     := aMode;
    fSamples  := aSamples;

    fDelegates := TObjectList.Create(TRUE);

    Enabled := NOT _Suite.CommandLine.DisablePerformanceCases;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TPerformanceCase.Destroy;
  begin
    FreeAndNIL(fDelegates);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.SetDisplayName: UnicodeString;
  begin
    result := inherited SetDisplayName;

    case fMode of
      pmIterations  : result := Format('%s for %d iterations', [result, N]);
      pmSeconds     : result := Format('%s for %d seconds', [result, N]);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_Count: Integer;
  begin
    result := DelegateCount;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_Delegate(const aIndex: Integer): TDelegate;
  begin
    result := TDelegate(fDelegates[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_DelegateCount: Integer;
  begin
    result := fDelegates.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_MethodByIndex(const aIndex: Integer): IPerformanceMethod;
  begin
    result := Delegate[aIndex] as IPerformanceMethod;
  end;


  {  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_MethodByName(const aName: UnicodeString): IPerformanceMethod;
  var
    i: Integer;
  begin
    for i := 0 to Pred(DelegateCount) do
      if ANSISameText(Delegate[i].Name, aName) then
      begin
        result := Delegate[i] as IPerformanceMethod;
        EXIT;
      end;

    result := NIL;
  end;


  {  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_MethodCount: Integer;
  begin
    result := DelegateCount;
  end;


  {  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_Samples: Integer;
  begin
    result := fSamples;
  end;


  {  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_Sampling: Integer;
  begin
    result := fSampling;
  end;


  {  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceCase.get_Child(const aIndex: Integer): ITestArticle;
  begin
    result := TTestArticle(fDelegates[aIndex]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceCase.Remove(const aObject: TTestArticle);
  begin
    inherited;
    if Assigned(fDelegates) then
      fDelegates.Remove(aObject);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceCase.AfterConstruction;
  begin
    inherited;

    _CreateDelegates(self, TPerformanceDelegate);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceCase.Add(const aObject: TTestArticle);
  begin
    inherited;
    fDelegates.Add(aObject);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceCase.DoExecute;
  var
    i, j: Integer;
  begin
    try
      for i := 0 to Pred(DelegateCount) do
        TPerformanceDelegate(Delegate[i]).Prepare(fSamples);

      for i := 1 to fSamples do
      begin
        fSampling := i;
        NotifyChange;

        for j := 0 to Pred(DelegateCount) do
        begin
          if State.InState[tsAborted] then
            EXIT;

          TPerformanceDelegate(Delegate[j]).Execute(i);
        end;
      end;

    finally
      fSampling := 0;
      NotifyChange;
    end;
  end;






{ TPerformanceDelegate --------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceDelegate.AddElapsed(const aValue: Cardinal);
  begin
    if (aValue = 0) then
      EXIT;

    inherited AddElapsed(aValue);

    Inc(fElapsed[fSample], aValue);

    if (aValue > fSlowest[fSample]) then
      fSlowest[fSample] := aValue;

    if (aValue < fFastest[fSample]) then
      fFastest[fSample] := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceDelegate.SetStartTime;
  begin
    fSlowest[fSample]  := 0;
    fFastest[fSample]  := $ffffffff;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_Sample(const aIndex: Integer): IPerformanceSample;
  begin
    result := TPerformanceSample.Create(self, aIndex + 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_SampleCount: Integer;
  begin
    result := Length(fFastest) - 1;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_Sampling: Integer;
  begin
    result := fSample;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceDelegate.Prepare(const aSamples: Integer);
  begin
    fIteration  := 0;
    fSample     := 0;

    SetLength(fElapsed, aSamples + 1);
    SetLength(fFastest, aSamples + 1);
    SetLength(fSlowest, aSamples + 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceDelegate.Execute(const aSample: Integer);
  begin
    fSample := aSample;

    inherited Execute;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceDelegate.Finalise;
  begin
    fSample := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TPerformanceDelegate.DoExecute;
  var
    hProcess: Cardinal;
    terminalValue: Cardinal;
    start: Cardinal;
    setup: ISetupTest;
    cleanup: ICleanupTest;
  begin
    TestCase.GetInterface(ISetupTest, setup);
    TestCase.GetInterface(ICleanupTest, cleanup);

    if IsConsole and NOT _Suite.CommandLine.SilentRunning then
      Write(NameForConsole + '...');

    try
      try
        TMethod(fMethod).Data := Owner;

        fIteration := 0;
        NotifyChange;

        hProcess := GetCurrentProcess;
        case TPerformanceCase(Owner).Mode of
          pmIterations  : begin
                            terminalValue := TPerformanceCase(Owner).N;
                            repeat
                              FlushInstructionCache(hProcess, NIL, 0);

                              if Assigned(setup) then
                                setup.SetupTest(self);

                              if (fIteration = 0) then
                                SetStartTime;

                              start := HPC.Value;
                              Method;
                              AddElapsed(HPC.Value - Start);

                              if IsConsole and NOT _Suite.CommandLine.SilentRunning then
                                Write('.');

                              if Assigned(cleanup) then
                                cleanup.CleanupTest(self);

                              Inc(fIteration);
                              NotifyChange;

                            until (fIteration = terminalValue);
                          end;

          pmSeconds     : begin
                            terminalValue := Cardinal(TPerformanceCase(Owner).N * HPC.Frequency);
                            repeat
                              FlushInstructionCache(hProcess, NIL, 0);

                              if Assigned(setup) then
                                setup.SetupTest(self);

                              if (fIteration = 0) then
                                SetStartTime;

                              start := HPC.Value;
                              Method;
                              AddElapsed(HPC.Value - Start);

                              if IsConsole and NOT _Suite.CommandLine.SilentRunning then
                                Write('.');

                              if Assigned(cleanup) then
                                cleanup.CleanupTest(self);

                              Inc(fIteration);
                              NotifyChange;

                            until (Elapsed >= terminalValue);
                          end;
        end;

      finally
        if IsConsole and NOT _Suite.CommandLine.SilentRunning then
          WriteLn;
      end;

    except
      on e: Exception do
        State.Enter(tsError);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_AverageRuntime: Double;
  var
    i: Integer;
    samples: Integer;
  begin
    result  := 0;
    samples := Pred(Length(fElapsed));

    if (samples > 0) then
    begin
      if (fSample = 0) then
      begin
        for i := 1 to samples do
          result := result + fElapsed[i];

        result := 1000 * (((result / samples) / HPC.Frequency) / Iteration);
      end
      else if (Iteration > 0) then
        result := 1000 * ((Elapsed / HPC.Frequency) / Iteration);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_Fastest: Double;
  var
    i: Integer;
    samples: Integer;
  begin
    result  := 0;
    samples := Pred(Length(fElapsed));

    if (samples > 0) then
    begin
      if (fSample = 0) then
      begin
        for i := 1 to samples do
          result := result + fFastest[i];

        result := 1000 * (((result / samples) / HPC.Frequency) / Iteration);
      end
      else
        result := 1000 * (fFastest[fSample] / HPC.Frequency);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_ExecutionsPerSecond: Double;
  var
    i: Integer;
    samples: Integer;
  begin
    result  := 0;
    samples := Pred(Length(fElapsed));

    if (samples > 0) then
    begin
      if (fSample = 0) then
      begin
        for i := 1 to samples do
          result := result + fElapsed[i];

        result := (Iteration / ((result / samples) / HPC.Frequency));
      end
      else if (Elapsed > 0) then
        result := (Iteration / (Elapsed / HPC.Frequency))
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_Slowest: Double;
  var
    i: Integer;
    samples: Integer;
  begin
    result  := 0;
    samples := Pred(Length(fElapsed));

    if (samples > 0) then
    begin
      if (fSample = 0) then
      begin
        for i := 1 to samples do
          result := result + fSlowest[i];

        result := 1000 * (((result / samples) / HPC.Frequency) / Iteration);
      end
      else
        result := 1000 * (fSlowest[fSample] / HPC.Frequency);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_Elapsed: Int64;
  begin
    if (fSample > 0) then
      result := fElapsed[fSample]
    else
      result := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceDelegate.get_TestCase: IPerformanceCase;
  begin
    result := Owner as IPerformanceCase;
  end;










{ TPerformanceSample ----------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TPerformanceSample.Create(const aDelegate: TPerformanceDelegate;
                                        const aSample: Integer);
  begin
    inherited Create;

    fAverageRunTime := 0;
    fTotalRuntime   := aDelegate.fElapsed[aSample];
    fFastest        := aDelegate.fFastest[aSample];
    fSlowest        := aDelegate.fSlowest[aSample];
    fIterations     := aDelegate.Iteration;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceSample.get_AverageRuntime: Double;
  begin
    result := 1000 * ((fTotalRuntime / HPC.Frequency) / fIterations)
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceSample.get_ExecutionsPerSecond: Double;
  begin
    result := (fIterations / (fTotalRuntime / HPC.Frequency))
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceSample.get_Fastest: Double;
  begin
    result := 1000 * (fFastest / HPC.Frequency);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceSample.get_Slowest: Double;
  begin
    result := 1000 * (fSlowest / HPC.Frequency);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TPerformanceSample.get_TotalRuntime: Double;
  begin
    result := fTotalRuntime;
  end;
















{ TTestOutput ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TOutput.Create(const aDelegate: TTestDelegate;
                             const aText: UnicodeString);
  begin
    inherited Create;

    fArticle := aDelegate;

    fText := aText;
    fTime := Now;

    _Suite.TestRun.Output.Add(self);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TOutput.Destroy;
  begin

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TOutput.get_Article: ITestArticle;
  begin
    result := fArticle;
  end;








{ TInformation ----------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInformation.get_Text: UnicodeString;
  begin
    result := fText;
  end;







{ TAlert ----------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TAlert.get_Text: UnicodeString;
  begin
    result := fText;
  end;







{ TInspection ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TInspection.Create(const aDelegate: TTestDelegate;
                                 const aSubject, aValue: UnicodeString;
                                 const aMonoSpaced: Boolean);
  begin
    inherited Create(aDelegate, aValue);

    fMonoSpaced := aMonoSpaced;
    fSubject    := aSubject;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspection.AddItem(const aName, aValue: UnicodeString);
  var
    idx: Integer;
  begin
    idx := Length(fItemLabels);
    SetLength(fItemLabels, idx + 1);
    SetLength(fItemValues, idx + 1);

    fItemLabels[idx] := aName;
    fItemValues[idx] := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspection.get_ItemCount: Integer;
  begin
    result := Length(fItemLabels);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspection.get_ItemLabel(const aIndex: Integer): UnicodeString;
  begin
    result := fItemLabels[aIndex];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspection.get_ItemValue(const aIndex: Integer): UnicodeString;
  begin
    result := fItemValues[aIndex];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspection.get_MonoSpaced: Boolean;
  begin
    result := fMonoSpaced;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspection.get_Subject: UnicodeString;
  begin
    result := fSubject;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspection.get_Value: UnicodeString;
  begin
    result := self.Text;
  end;









{ TTestResult ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestResult.Create(const aIncident: TIncident;
                                 const aMessage: UnicodeString;
                                 const aOK: Boolean);
  begin
    inherited Create(aIncident.Test, aMessage);

    fOK := aOK;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestResult.get_OK: Boolean;
  begin
    result := fOK;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestResult.set_OK(const aValue: Boolean);
  begin
    fOK := aValue;
  end;











{ TIncident -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TIncident.Create(const aCase: TTestCase;
                               const aName: UnicodeString);
  begin
    inherited Create;

    fTest     := aCase.fActiveDelegate;
    fTestCase := aCase;

    fName     := aName;

    aCase.fIncidents.Add(self);
  end;









{ TInspector ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TInspector.GetInspector(const aCase: TTestCase; const aName: UnicodeString): TInspector;
  begin
    if NOT Assigned(aCase.fInspector)
     or (aCase.fInspector.Name <> aName) then
      aCase.fInspector := TInspector.Create(aCase, aName);

    result := aCase.fInspector;
    result.fMonospaced  := FALSE;  // Need to reset the monospaced indicator in case it
                                   //  was set by the previous inspection for the specified case
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspector.get_MonoSpaced: IInspectorMethods;
  begin
    fMonoSpaced := TRUE;
    result := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspector.get_Part(aPart: Variant): IInspectorMethods;
  var
    inspector: TInspector;
  begin
    if NOT Assigned(fInspection) then
      fInspection := TInspection.Create(TestCase.fActiveDelegate, Name, '');

    inspector := TInspector.Create(TestCase, Name + ' [' + VarToStr(aPart) + ']');
    inspector.fMonospaced  := fMonospaced;
    inspector.fInspection  := fInspection;
    inspector.fPartName    := '[' + VarToStr(aPart) + ']';

    result := inspector;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TInspector.QueryInterface(const aIID: TGUID; out aObj): HRESULT; stdcall;
  var
    extClass: TInspectorClass;
    inspection: TInspector;
  begin
    if NOT SameGUID(aIID, IInspector)
     and _Suite.FindExtension(aIID, extClass) then
    begin
      result      := 0;
      inspection  := extClass.Create(TestCase, Name);

      if (fPartName <> '') then
      begin
        inspection.fInspection := fInspection;
        inspection.fPartName   := fPartName;
      end;

      if NOT inspection.GetInterface(aIID, aObj) then
        result := E_NOINTERFACE;
    end
    else
      result := inherited QueryInterface(aIID, aObj);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Output(const aString: UnicodeString);
  begin
    if (fPartName <> '') and Assigned(fInspection) then
    begin
      fInspection.fMonoSpaced := fMonoSpaced;
      fInspection.AddItem(fPartName, aString);
    end
    else
      TInspection.Create(Test, Name, aString, fMonospaced);

    fPartName := '';
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Emit(const aString: UnicodeString);
  begin
    Output(aString);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Emit(const aString: UnicodeString;
                            const aArgs: array of const);
  begin
    Output(WideFormat(aString, aArgs));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Emit(aBuffer: Pointer);
  begin
    Emit(IntToHex(NativeInt(aBuffer), sizeof(Pointer)));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Emit(aBuffer: Pointer; aSize: Integer);
  var
    i: Integer;
    s: UnicodeString;
    p: PChar;
  begin
    p := aBuffer;
    s := '';

    for i := 0 to Pred(aSize) do
    begin
      s := s + Format('%.2x ', [Byte(p^)]);
      Inc(p);
    end;

    SetLength(s, Length(s) - 1);

    Emit(s);
  end;


{$ifdef EnhancedOverloads}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: Currency);
  begin
    Emit(CurrToStr(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: Double);
  begin
    Emit(FloatToStr(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: TDatetime);
  begin
    Emit(DateTimeToStr(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: TDate);
  begin
    Emit(DateToStr(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: TTime);
  begin
    Emit(TimeToStr(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: UTF8String);
  begin
    if (aValue <> '') then
      Emit(WIDE.FromUTF8('''' + aValue + ''''));
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: ANSIString);
  begin
    if (aValue <> '') then
      Emit(WIDE.FromANSI('''' + aValue + ''''));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: UnicodeString);
  begin
    if (aValue <> '') then
      Emit('''' + aValue + '''');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: Boolean);
  const
    SBOOL: array[FALSE..TRUE] of UnicodeString = ('FALSE', 'TRUE');
  begin
    Emit(SBOOL[aValue]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: Int64);
  begin
    Emit(IntToStr(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aBuffer: Pointer; aSize: Integer);
  begin
    Emit(aBuffer, aSize);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: TGUID);
  begin
    Emit(GUIDToString(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: Single);
  begin
    Emit(FloatToStr(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: Extended);
  begin
    Emit(FloatToStr(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Value(aValue: TStrings);
  var
    i: Integer;
  begin
    if (aValue.Count > 0) then
    begin
      for i := 0 to Pred(aValue.Count) do
      {$ifdef UNICODE}
        Part[i].Value(aValue[i]);
      {$else}
        Part[i].Value(WIDE.FromANSI(aValue[i]));
      {$endif}
    end
    else
      Emit('<empty>');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TInspector.Values(const aArgs: array of const);
  var
    s: UnicodeString;
  begin
    s := Name;
    fName := '';
    Emit(s, aArgs);
  end;













{ TTest ------------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.get_Part(aPart: Variant): ITest;
  begin
    result := TTest.Create(TestCase, Name + ' [' + VarToStr(aPart) + ']');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.QueryInterface(const aIID: TGUID; out aObj): HRESULT;
  var
    extClass: TExpectationClass;
    expectation: TExpectation;
  begin
    if _Suite.FindExtension(aIID, extClass) then
    begin
      result      := 0;
      expectation := extClass.Create(self);

      if NOT expectation.GetInterface(aIID, aObj) then
        result := E_NOINTERFACE;
    end
    else
      result := inherited QueryInterface(aIID, aObj);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTest.Replace(const aOld, aNew: TExpectation);
  begin
    _Suite.TestRun.Output.Replace(aOld, aNew);
  end;


{$ifdef EnhancedOverloads}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: Currency): CurrencyExpectation;
  begin
    result := TCurrencyExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: Double): DoubleExpectation;
  begin
    result := TDoubleExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: TDatetime): DatetimeExpectation;
  begin
    result := TDatetimeExpectation.Create(self).Expect(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: TDate): DateExpectation;
  begin
    result := TDateExpectation.Create(self).Expect(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: TTime): TimeExpectation;
  begin
    result := TTimeExpectation.Create(self).Expect(aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: UTF8String): UTF8Expectation;
  begin
    result := TStringExpectation.Create(self, WIDE.FromUTF8(aValue));
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: Boolean): BooleanExpectation;
  begin
    result := TBooleanExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: Cardinal): CardinalExpectation;
  begin
    result := TCardinalExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: Single): SingleExpectation;
  begin
    result := TSingleExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: Extended): ExtendedExpectation;
  begin
    result := TExtendedExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: Int64): IntegerExpectation;
  begin
    result := TIntegerExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: TObject): ObjectExpectation;
  begin
    result := TObjectExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: TGUID): GUIDExpectation;
  begin
    result := TGUIDExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: Pointer): PointerExpectation;
  begin
    result := TPointerExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: IInterface): InterfaceExpectation;
  begin
    result := TInterfaceExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: ANSIString): StringExpectation;
  begin
    result := TStringExpectation.Create(self, WIDE.FromANSI(aValue));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expect(aValue: UnicodeString): StringExpectation;
  begin
    result := TStringExpectation.Create(self, aValue);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.Expecting(aClass: TExceptionClass;
                           aMessage: UnicodeString): ExceptionExpectation;
  begin
    result := TTestException.CreateExpecting(self, aClass);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTest.UnexpectedException: ExceptionExpectation;
  begin
    result := TTestException.CreateUnexpected(self);
  end;










{ TTestExpectation ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TExpectation.Create(const aSubject: TTest);
  begin
    inherited Create(aSubject, '');

    fSubject      := aSubject;
    fSubjectName  := aSubject.Name;

//    if (soDiagnostics in _Suite.Options) then
//      fSubjectName := '[' + ClassName + ']' + fSubjectName;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_Actual: UnicodeString;
  begin
    result := ReplaceTokens(fActual);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_Description: UnicodeString;
  var
    desc: UnicodeString;
  begin
    result  := Trim(ReplaceTokens('{subject}'));
    desc    := Trim(ReplaceTokens(fDescription));

    // By default our display description is our subject, then..
    //  - If we have no subject then we use the description
    //  - If the subject does NOT end with '!' then we APPEND the description
    //  - Otherwise we remove the trailing '!'

    if (result = '') then
      result := desc
    else if (result[Length(result)] <> '!') then
      result := result + ' '+ desc
    else
      SetLength(result, Length(result) - 1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_Evaluated: Boolean;
  begin
    result := fEvaluated;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_Expected: UnicodeString;
  begin
    result := ReplaceTokens(fExpected);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_ExpectedToFail: Boolean;
  begin
    result := fExpectedToFail;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_Explanation: UnicodeString;
  begin
    result := fExplanation;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_OK: Boolean;
  begin
    result := fEvaluated and fOK;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.IsExpectedToFail: IReason;
  begin
    result  := self;

    fExpectedToFail := TRUE;

    if NOT OK then
    begin
      Subject.Test.State.Leave(tsFailed);
      Subject.Test.State.Enter(tsPassed);
    end
    else
    begin
      Subject.Test.State.Leave(tsPassed);
      Subject.Test.State.Enter(tsFailed);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_SubjectName: UnicodeString;
  begin
    result := ReplaceTokens(fSubjectName);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_TestCase: ITestCase;
  begin
    result := Subject.TestCase as ITestCase;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.get_TestMethod: ITestMethod;
  begin
    result := Subject.Test as ITestMethod;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TExpectation.Because(const aExplanation: UnicodeString);
  begin
    fExplanation := ReplaceTokens(aExplanation);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TExpectation.set_OK(const aValue: Boolean);
  var
    firstEval: Boolean;
  begin
    if (OK = aValue) and Evaluated then
      EXIT;

    firstEval := NOT Evaluated;

    if NOT firstEval then
      case OK of
        FALSE : Subject.Test.State.Leave(tsFailed);
        TRUE  : Subject.Test.State.Leave(tsPassed);
      end;

    inherited set_OK(aValue);

    fEvaluated  := TRUE;

    fExpectedToFail := (fSubject.Test.fExpectedFailures <> 0);
    if (fSubject.Test.fExpectedFailures > 0) then
      Dec(fSubject.Test.fExpectedFailures);

    if fExpectedToFail then
      case OK of
        FALSE : Subject.Test.State.Enter(tsPassed);
        TRUE  : Subject.Test.State.Enter(tsFailed);
      end
    else
      case OK of
        FALSE : Subject.Test.State.Enter(tsFailed);
        TRUE  : Subject.Test.State.Enter(tsPassed);
      end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.DoReplaceToken(var aString: UnicodeString;
                                       const aToken, aValue: UnicodeString): Boolean;
  var
    original: UnicodeString;
  begin
    original  := aString;
    aString   := StringReplace(original, aToken,  aValue, [rfIgnoreCase, rfReplaceAll]);
    result    := (aString <> original);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TExpectation.ReplaceInSubject(const aNew: TExpectation);
  begin
    Subject.Replace(self, aNew);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.ReplaceToken(const aString: UnicodeString;
                                     const aToken, aValue: UnicodeString): UnicodeString;
  begin
    result := aString;
    while DoReplaceToken(result, aToken, aValue) do;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.ReplaceTokens(const aString: UnicodeString): UnicodeString;
  begin
    result := aString;
    while DoReplaceTokens(result) do;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.DoReplaceTokens(var aString: UnicodeString): Boolean;
  begin
    result := DoReplaceToken(aString, '{actual}',       fActual);
    result := DoReplaceToken(aString, '{expected}',     fExpected) or result;
    result := DoReplaceToken(aString, '{subject}',      fSubjectName) or result;
    result := DoReplaceToken(aString, '{expectation}',  fInterfaceName) or result;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.IsCritical: IReason;
  begin
    result := self;

    if NOT OK then
      TTestCase(Subject.Test.Owner).AbortCase('Test case cannot proceed');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.IsRequired: IReason;
  begin
    result := self;

    if NOT OK then
      TTestCase(Subject.Test.Owner).Abort('Test method cannot proceed');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TExpectation.IsShowStopper;
  begin
    if NOT OK then
    begin
      TTestCase(Subject.Test.Owner).AbortCase('Tests cannot proceed');
      _Suite.Abort;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TExpectation.Supports(const aIID: TGUID;
                                 const aName: UnicodeString): Evaluation;
  begin
    result := self;

    fInterfaceName  := IfThen(aName = '', SubjectName, aName);

    Description := ReplaceTokens('{subject} will be evaluated using {expectation}');
    SubjectName := '';

    Actual      := ClassName;
    Expected    := '{expectation}';

    OK := SysUtils.Supports(self, aIID);
  end;







{ TTestRun --------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestRun.Create;
  begin
    inherited Create;

    fOutput     := TOutputList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TTestRun.Destroy;
  begin
    FreeAndNIL(fOutput);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestRun.Clear;
  begin
    fOutput.fItems.Clear;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestRun.get_Name: UnicodeString;
  begin
    result := Trim(fName);
    if (result = '') then
      result := DateTimeToStr(StartTime);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestRun.set_EndTime(const aValue: TDateTime);
  begin
    fEndTime := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestRun.set_Name(const aValue: UnicodeString);
  begin
    fName := aValue;
  end;











{ TOutputList ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TOutputList.Create;
  begin
    inherited Create;

    fItems    := TObjectList.Create(TRUE);
    fReplaced := TObjectList.Create(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TOutputList.Destroy;
  begin
    FreeAndNIL(fReplaced);
    FreeAndNIL(fItems);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TOutputList.get_Count: Integer;
  begin
    result := fItems.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TOutputList.get_Item(const aIndex: Integer): IOutput;
  begin
    result := TOutput(fItems[aIndex]) as IOutput;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOutputList.Replace(const aOld, aNew: TOutput);
  var
    idx: Integer;
  begin
    idx   := fItems.IndexOf(aOld);

    // Remove the old output from the items list and move it to the
    //  "replaced" list.

    fItems.Extract(aOld);
    fReplaced.Add(aOld);

    // The new output is already in the items list thanks to the
    //  constructor chain for output classes so we simply need to move it
    //  to the position in the items list previously occupied by the old one.

    fItems.Move(fItems.IndexOf(aNew), idx);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TOutputList.Add(const aOutput: TOutput);
  begin
    fItems.Add(aOutput);
  end;















{ ------------------------------------------------------------------------------------------------ }


initialization
  if NOT IsConsole then
    _VCL := TVCLNotifier.Create;

finalization
  _Extensions.Free;
  _Suite.Free;
  _VCL.Free;
end.
