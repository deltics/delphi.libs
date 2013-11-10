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

{$ifdef deltics_smoketest_filewriter}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.Smoketest.FileWriter;


interface

  uses
    Classes,
    Deltics.Smoketest;


  type
    TFileWriter = class
    private
      fCommandLine: TSmoketestCommandLine;
      fDetailed: Boolean;
      fFile: TFileStream;
      fIndent: Integer;
      fTestRun: TTestRun;     // Should be ISmoketestRun
      fResults: TOutputList;  // Should be ISmoketestResults
      fToConsole: Boolean;
      fToFile: Boolean;
      fVerbose: Boolean;

      fCases: TInterfaceList;
      fCasesAborted: TInterfaceList;
      fTests: TInterfaceList;
      fTestsAborted: TInterfaceList;
      fTestsErrored: TInterfaceList;
      fTestsFailed: TInterfaceList;
      fTestsPassed: TInterfaceList;
      fTestsNotImplemented: TInterfaceList;
      fResultsNotEvaluated: TInterfaceList;
      fTestsToReport: TInterfaceList;
      procedure Write(const aOutput: String); overload;
      procedure Write(const aOutput: String; const aArgs: array of const); overload;
    protected
      procedure Indent(const aInc: Integer);
      procedure PrepareSummary;
      procedure Write; overload; virtual; abstract;

      property CommandLine: TSmoketestCommandLine read fCommandLine;
      property Detailed: Boolean read fDetailed;
      property ToConsole: Boolean read fToConsole;
      property ToFile: Boolean read fToFile;
      property Results: TOutputList read fResults;
      property Verbose: Boolean read fVerbose;
    public
      constructor Create(const aSmoketest: ISmoketestRuntime);
      destructor Destroy; override;
    end;



    TXMLFileWriter = class(TFileWriter)
    private
      function XMLEncode(const aValue: String): String;
      procedure Write(const aTag: String; const aList: TInterfaceList); overload;
      procedure WriteTimings;
    protected
      procedure Write; override;
    end;


    procedure OutputResults;


implementation

  uses
    SysUtils,
    Deltics.DateUtils,
    Deltics.SysUtils;


  const
    NOT_FOUND = -1;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TFileWriter.Create(const aSmoketest: ISmoketestRuntime);
  begin
    inherited Create;

    fCommandLine  := aSmoketest.CommandLine;
    fTestRun      := aSmoketest.TestRun;
    fResults      := fTestRun.Output;      // Could this be made: aSmoketest.TestRun.Results ?

    fToConsole := IsConsole and CommandLine.OutputToConsole;
    fToFile    := (CommandLine.OutputFilename <> '');

    fDetailed  := TRUE; // CommandLine,DetailedOutput;  TODO: optional - levels of verbosity
    fVerbose   := CommandLine.VerboseOutput;

    if ToFile then
      fFile := TFileStream.Create(CommandLine.OutputFilename, fmOpenWrite or fmCreate)
    else
      fFile := NIL;

    fCases                := TInterfaceList.Create;
    fCasesAborted         := TInterfaceList.Create;
    fTests                := TInterfaceList.Create;
    fTestsAborted         := TInterfaceList.Create;
    fTestsErrored         := TInterfaceList.Create;
    fTestsFailed          := TInterfaceList.Create;
    fTestsPassed          := TInterfaceList.Create;
    fTestsNotImplemented  := TInterfaceList.Create;
    fResultsNotEvaluated  := TInterfaceList.Create;
    fTestsToReport        := TInterfaceList.Create;

    PrepareSummary;
    Write;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TFileWriter.Destroy;
  begin
    FreeAndNIL(fCases);
    FreeAndNIL(fCasesAborted);
    FreeAndNIL(fTests);
    FreeAndNIL(fTestsAborted);
    FreeAndNIL(fTestsErrored);
    FreeAndNIL(fTestsFailed);
    FreeAndNIL(fTestsPassed);
    FreeAndNIL(fTestsNotImplemented);
    FreeAndNIL(fResultsNotEvaluated);
    FreeAndNIL(fTestsToReport);

    FreeAndNIL(fFile);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TFileWriter.Indent(const aInc: Integer);
  begin
    fIndent := fIndent + aInc;
    ASSERT(fIndent >= 0, 'INDENT screwed up!');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TFileWriter.PrepareSummary;
  var
    i, j: Integer;
    project: ISmoketestMetaData;
    result: IResult;
    testCase: ITestCase;
    testMethod: ITestMethod;
    prevMethod: ITestMethod;
  begin
    project := (Smoketest as ISmoketestMetaData);
    for i := 0 to Pred(project.CaseCount) do
    begin
      if NOT (project.Cases[i].ArticleType = atTestCase) then
        CONTINUE;

      testCase := project.Cases[i] as ITestCase;
      for j := 0 to Pred(testCase.MethodCount) do
      begin
        testMethod := testCase.MethodByIndex[j];
        if testMethod.NotImplemented then
          fTestsNotImplemented.Add(testMethod);
      end;
    end;

    prevMethod := NIL;

    for i := 0 to Pred(Results.Count) do
    begin
      if NOT Supports(Results[i], IResult, result) then
        CONTINUE;

      testCase    := result.TestCase;
      testMethod  := result.TestMethod;

      if fCases.IndexOf(testCase) = NOT_FOUND then
      begin
        fCases.Add(testCase);

        if testCase.IsAborted then
          fCasesAborted.Add(testCase);
      end;

      if testMethod <> prevMethod then
      begin
        prevMethod := testMethod;

        fTests.Add(testMethod);

        if testMethod.HasFailures
         or testMethod.HasErrors
         or testMethod.Aborted then
        begin
          if testMethod.Aborted then
            fTestsAborted.Add(testMethod);

          if testMethod.HasFailures then
            fTestsFailed.Add(testMethod);

          if testMethod.HasErrors then
            fTestsErrored.Add(testMethod);
        end
        else
          fTestsPassed.Add(testMethod);
      end;

      if NOT result.Evaluated then
      begin
        fResultsNotEvaluated.Add(result);
        fTestsToReport.Add(result);
      end
      else if Verbose or (result.OK = result.ExpectedToFail) then
        fTestsToReport.Add(result);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TFileWriter.Write(const aOutput: String);
  var
    pad: String;
    utf8: UTF8String;
  begin
    pad := StringOfChar(' ', fIndent * 2);
    utf8 := UTF8Encode(pad + aOutput);

    if ToFile then
    begin
      utf8 := utf8 + #13;
      fFile.Write(utf8[1], Length(utf8));
    end;

    if ToConsole then
      WriteLn(pad + aOutput);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TFileWriter.Write(const aOutput: String;
                              const aArgs: array of const);
  begin
    Write(Format(aOutput, aArgs));
  end;



{ TXMLFileWriter }

  function TXMLFileWriter.XMLEncode(const aValue: String): String;
  begin
    result := aValue;

    result := StringReplace(result, '&',  '&amp;',  [rfReplaceAll]);
    result := StringReplace(result, '<',  '&lt;',   [rfReplaceAll]);
    result := StringReplace(result, '>',  '&gt;',   [rfReplaceAll]);
    result := StringReplace(result, '"',  '&quot;', [rfReplaceAll]);
    result := StringReplace(result, '''', '&apos;', [rfReplaceAll]);
  end;


  procedure TXMLFileWriter.Write(const aTag: String;
                                 const aList: TInterfaceList);
  var
    i: Integer;
    result: IResult;
    test: ITestMethod;
    testCase: ITestCase;
    caseName: String;
    testName: String;
  begin
    if aList.Count = 0 then
    begin
      Write('<%s/>', [aTag]);
      EXIT;
    end;

    Write('<%s count="%d">', [aTag, aList.Count]);
    Indent(1);

    caseName := '';
    testName := '';

    for i := 0 to Pred(aList.Count) do
    begin
      if Supports(aList[i], ITestMethod, test) then
      begin
        testCase := test.TestCase;
        result   := NIL;
      end
      else if Supports(aList[i], IResult, result) then
      begin
        testCase := result.TestCase;
        test     := result.TestMethod;
      end;

      if (testCase.DisplayName <> caseName) then
      begin
        if (caseName <> '') then
        begin
          Indent(-1);
          Write('</test>');
          Indent(-1);
          Write('</case>');
        end;

        caseName  := testCase.DisplayName;
        testName  := '';

        Write('<case ref="' + testCase.Reference + '" name="' + XMLEncode(caseName) + '">');
        Indent(1);
      end;

      if (test.DisplayName <> testName) then
      begin
        if (testName <> '') then
        begin
          Indent(-1);
          Write('</test>');
        end;

        testName  := test.DisplayName;

        Write('<test ref="' + test.Reference + '" name="' + XMLEncode(testName) + '">');
        Indent(1);
      end;

      if NOT Assigned(result) then
        CONTINUE;

      Write('<expectation description="' + XMLEncode(result.Description) + '"');
      if NOT result.OK then
      begin
        if result.Evaluated then
        begin
          if (result.ExpectedToFail) then
            Write('             result="failed as expected" />')
          else
          begin
            if (Verbose or Detailed) and ((result.Expected <> '') or (result.Actual <> '')) then
            begin
              Write('             result="failed">');
              Indent(1);
              if result.Expected <> '' then
                Write('<expected>%s</expected>', [result.Expected]);
              if result.Actual <> '' then
                Write('<actual>%s</actual>', [result.Actual]);
              Write('</expectation>');
            end
            else
              Write('             result="failed" />');
          end;
        end
        else
          Write('             result="none" />');
      end
      else
        Write('             result="passed" />');
    end;

    if (aList.Count > 0) then
    begin
      Indent(-1);
      Write('</test>');
      Indent(-1);
      Write('</case>');
    end;

    Indent(-1);
    Write('</%s>', [aTag]);
  end;


  procedure TXMLFileWriter.Write;
  begin
    Write('<?xml version="1.0" encoding="utf-8" ?>');
    Write('<smoketest>');
    try
      Indent(1);
      Write('<summary>');
      Indent(1);
      Write('<start date="%s" time="%s" />', [DatetimeToISO8601(fTestRun.StartTime, [dtDate]), DatetimeToISO8601(fTestRun.StartTime, [dtTime])]);
      Write('<finish date="%s" time="%s" />', [DatetimeToISO8601(fTestRun.EndTime, [dtDate]), DatetimeToISO8601(fTestRun.EndTime, [dtTime])]);
      Write('<cases total="%d" aborted="%d" />', [fCases.Count, fCasesAborted.Count]);
      Write('<tests total="%d" aborted="%d" errored="%d" failed="%d" passed="%d" not-implemented="%d" />', [fTests.Count,
                                                                                                            fTestsAborted.Count,
                                                                                                            fTestsFailed.Count,
                                                                                                            fTestsErrored.Count,
                                                                                                            fTestsPassed.Count,
                                                                                                            fTestsNotImplemented.Count]);
      Indent(-1);
      Write('</summary>');

      Indent(-1);
      Write('tests', fTestsToReport);

      if (fTestsNotImplemented.Count > 0) then
        Write('not-implemented', fTestsNotImplemented);

      // TODO: Revisit performance case output when more concrete requirements are understood

    finally
      Write('</smoketest>');
    end;
  end;


  procedure TXMLFileWriter.WriteTimings;
  var
    i, j: Integer;
    perfcase: IPerformanceCase;
    times: TInterfaceList;
    test: IPerformanceMethod;
    testCase: IPerformanceCase;
    project: ISmoketestMetadata;
  begin
    times := TInterfaceList.Create;
    try
      project := (Smoketest as ISmoketestMetaData);
      for i := 0 to Pred(project.CaseCount) do
      begin
        if NOT (project.Cases[i] is TPerformanceCase) then
          CONTINUE;

        perfcase := project.Cases[i] as IPerformanceCase;
        for j := 0 to Pred(perfcase.MethodCount) do
          times.Add(perfcase.MethodByIndex[j]);
      end;

      if (times.Count = 0) then
        EXIT;

      Write('<timings>');
      try
        Indent(1);
        testCase := NIL;

        for i := 0 to Pred(times.Count) do
        begin
          test := times[i] as IPerformanceMethod;

          if (testCase <> test.TestCase) then
          begin
            if Assigned(testCase) then
            begin
              Indent(-1);
              Write('</case>');
            end;

            testCase  := test.TestCase;

            Write('<case name="%s">', [XMLEncode(testCase.DisplayName)]);
            Indent(1);
          end;

          Write('<test name="%s" avg="%f" per_s="%f" fastest="%f" slowest="%f" />',
                  [XMLEncode(test.DisplayName),
                   test.AverageRuntime, test.ExecutionsPerSecond, test.Fastest, test.Slowest]);
        end;
        Indent(-1);
        Write('</case>');

      finally
        Indent(-1);
        Write('</timings>');
      end;

    finally
      times.Free;
    end;
  end;













(*
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure OutputResults;
  var
    toConsole: Boolean;
    toFile: Boolean;
    fileOut: TFileStream;

    function XMLEncode(const aValue: String): String;
    begin
      result := aValue;

      result := StringReplace(result, '&',  '&amp;',  [rfReplaceAll]);
      result := StringReplace(result, '<',  '&lt;',   [rfReplaceAll]);
      result := StringReplace(result, '>',  '&gt;',   [rfReplaceAll]);
      result := StringReplace(result, '"',  '&quot;', [rfReplaceAll]);
      result := StringReplace(result, '''', '&apos;', [rfReplaceAll]);
    end;

    procedure Write(const aOutput: String); overload;
    var
      utf8: UTF8String;
    begin
      if toFile then
      begin
        utf8 := UTF8Encode(aOutput) + #13;
        fileOut.Write(utf8[1], Length(utf8));
      end;

      if toConsole then
        WriteLn(aOutput);
    end;

    procedure Write(const aOutput: String; const aArgs: array of const); overload;
    begin
      Write(Format(aOutput, aArgs));
    end;

    procedure Write(const aTag: String; const aList: TInterfaceList); overload;
    var
      i: Integer;
      result: IResult;
      test: ITestMethod;
      testCase: ITestCase;
      caseName: String;
      testName: String;
    begin
      if aList.Count = 0 then
      begin
        Write('<%s/>', [aTag]);
        EXIT;
      end;

      Write('<%s expectation_count="%d">', [aTag, aList.Count]);

      caseName := '';
      testName := '';

      for i := 0 to Pred(aList.Count) do
      begin
        result := aList[i] as IResult;

        testCase  := result.TestCase;
        if (testCase.DisplayName <> caseName) then
        begin
          if (caseName <> '') then
          begin
            Write('</test>');
            Write('</case>');
          end;

          caseName  := testCase.DisplayName;
          testName  := '';

          Write('<case name="%s" start="%s" runtime="%d">', [XMLEncode(caseName),
                                                             DateTimeToISO8601(testCase.StartTime),
                                                             testCase.Elapsed]);
        end;

        if (result.TestMethod.DisplayName <> testName) then
        begin
          if (testName <> '') then
            Write('</test>');

          test      := result.TestMethod;
          testName  := test.DisplayName;

          Write('<test name="%s" start="%s" runtime="%d">', [XMLEncode(testName),
                                                             DateTimeToISO8601(test.StartTime),
                                                             test.Elapsed]);
        end;

        Write('<expectation>', [XMLEncode(result.SubjectName)]);
        try
          Write('<description>%s</description>', [XMLEncode(result.Description)]);

          if result.Evaluated then
          begin
            if (NOT result.OK) then
            begin
              if (result.Expected <> '') then
                Write('<expected>%s</expected>', [XMLEncode(result.Expected)]);

              if (result.Actual <> '') and (result.Actual <> result.Expected) then
                Write('<actual>%s</actual>', [XMLEncode(result.Actual)]);

              if (result.ExpectedToFail) then
                Write('<result>EXPECTED</result>')
              else
                Write('<result>FAILED</result>');
            end
            else
              Write('<result>OK</result>');
          end
          else
            Write('<result>NOT EVALUATED</result>');

        finally
          Write('</expectation>');
        end;
      end;

      if (aList.Count > 0) then
      begin
        Write('</test>');
        Write('</case>');
      end;

      Write('</%s>', [aTag]);
    end;

    procedure WriteTimings;
    var
      i, j: Integer;
      perfcase: IPerformanceCase;
      times: TInterfaceList;
      test: IPerformanceMethod;
      testCase: IPerformanceCase;
      project: ISmoketestMetadata;
    begin
      times := TInterfaceList.Create;
      try
        project := (Smoketest as ISmoketestMetaData);
        for i := 0 to Pred(project.CaseCount) do
        begin
          if NOT (project.Cases[i] is TPerformanceCase) then
            CONTINUE;

          perfcase := project.Cases[i] as IPerformanceCase;
          for j := 0 to Pred(perfcase.MethodCount) do
            times.Add(perfcase.MethodByIndex[j]);
        end;

        if (times.Count = 0) then
          EXIT;

        Write('<timings>');
        try
          testCase := NIL;

          for i := 0 to Pred(times.Count) do
          begin
            test := times[i] as IPerformanceMethod;

            if (testCase <> test.TestCase) then
            begin
              if Assigned(testCase) then
                Write('</case>');

              testCase  := test.TestCase;

              Write('<case name="%s">', [XMLEncode(testCase.DisplayName)]);
            end;

            Write('<test name="%s" avg="%f" per_s="%f" fastest="%f" slowest="%f" />',
                    [XMLEncode(test.DisplayName),
                     test.AverageRuntime, test.ExecutionsPerSecond, test.Fastest, test.Slowest]);
          end;
          Write('</case>');

        finally
          Write('</timings>');
        end;

      finally
        times.Free;
      end;
    end;

  const
    NOT_FOUND = -1;
  var
    i: Integer;
    params: TSmoketestCommandLine;
    run: TTestRun;
    output: TOutputList;
    aborted: TInterfaceList;
    errors: TInterfaceList;
    noResult: TInterfaceList;
    passed: TInterfaceList;
    failed: TInterfaceList;
    results: TInterfaceList;
    tests: Integer;
    cases: Integer;
    abortedCases: Integer;
    abortedTests: Integer;
    processed: TInterfaceList;
    expectation: IResult;
    testcase: ITestCase;
    testmethod: ITestMethod;
    verbose: Boolean;
    benchmarks: TInterfaceList;
  begin
    params  := (Smoketest as ISmoketestRuntime).CommandLine;
    verbose := params.VerboseOutput;

    tests         := 0;
    cases         := 0;
    abortedCases  := 0;
    abortedTests  := 0;

    toConsole := IsConsole and params.OutputToConsole;
    toFile    := (params.OutputFilename <> '');

    if toFile then
      fileOut := TFileStream.Create(params.OutputFilename, fmOpenWrite or fmCreate)
    else
      fileOut := NIL;

    aborted     := TInterfaceList.Create;
    errors      := TInterfaceList.Create;
    noResult    := TInterfaceList.Create;
    passed      := TInterfaceList.Create;
    failed      := TInterfaceList.Create;
    results     := TInterfaceList.Create;
    benchmarks  := TInterfaceList.Create;

    processed := TInterfaceList.Create;
    try
      run     := (Smoketest as ISmoketestRuntime).TestRun;
      output  := run.Output;
      for i := 0 to Pred(output.Count) do
      begin
        if NOT Supports(output[i], IResult, expectation) then
          CONTINUE;

        Inc(tests);

        testcase    := expectation.TestCase;
        testmethod  := expectation.TestMethod;

        if processed.IndexOf(testcase) = NOT_FOUND then
        begin
          Inc(cases);
          processed.Add(testcase);

          if testcase.IsAborted then
            Inc(abortedCases);
        end;

        if testmethod.Aborted then
           Inc(abortedTests);

        if testmethod.HasErrors then
           errors.Add(expectation);

        if NOT expectation.Evaluated then
          noResult.Add(expectation)
        else if expectation.OK = NOT expectation.ExpectedToFail then
          passed.Add(expectation)
        else
          failed.Add(expectation);

        if verbose or (expectation.OK = expectation.ExpectedToFail) then
          results.Add(expectation);
      end;

      Write('<?xml version="1.0" ?>');
      Write('<smoketest>');
      try
        Write('<summary>');
        Write('<start date="%s" time="%s" />', [DateToStr(run.StartTime), TimeToStr(run.StartTime)]);
        Write('<finish date="%s" time="%s" />', [DateToStr(run.EndTime), TimeToStr(run.EndTime)]);
        Write('<cases total="%d" aborted="%d" />', [cases, abortedCases]);
        Write('<tests total="%d" aborted="%d" errored="%d" failed="%d" passed="%d" not-implemented="%d" />', [tests,
                                                                                                              abortedTests,
                                                                                                              failed.Count,
                                                                                                              errors.Count,
                                                                                                              passed.Count,
                                                                                                              noResult.Count]);
        Write('</summary>');

        Write('tests', results);
        WriteTimings;

      finally
        Write('</smoketest>');
      end;

    finally
      processed.Free;

      benchmarks.Free;
      aborted.Free;
      errors.Free;
      noResult.Free;
      passed.Free;
      failed.Free;
      results.Free;

      fileOut.Free;
    end;
  end;
*)

  procedure OutputResults;
  begin
    with TXMLFileWriter.Create(Smoketest as ISmoketestRuntime) do
      Free;
  end;


end.
