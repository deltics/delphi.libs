
{$i selftest.inc}

  unit SelfTest.TestControl;


interface

  uses
    Deltics.Smoketest,
    SelfTest.Consts;


  type
    TTestFramework = class(TTestCase, IHaveChildCases)
    private
      procedure AddCases;
    end;

      TTestControl = class(TTestCase)
        procedure ExpectedFailures;
        procedure DeliberateFailures;
        procedure MixedResults;
        procedure TestStates;
        procedure ConditionalTest;
        procedure DynamicTestExecution;
        procedure DisabledTests;
        procedure AccessViolation;
        procedure AbortTest;
        procedure AbortTestUsingIsRequired;
        procedure AbortTestBasedOnTestResult;
      end;

      TTestAbortCase = class(TTestCase)
        procedure AbortTestCase;
        procedure NotPerformed;
      end;

      TTestIsCritical = class(TTestCase)
        procedure TestIsCritical;
        procedure NotPerformed;
      end;


    TTestCounters = class(TTestCase, ISetupProject,
                                     ISetupTestRun,
                                     ISetupTestCase,
                                     ISetupTest,
                                     ICleanupTest,
                                     ICleanupTestCase,
                                     ICleanupTestRun,
                                     ICleanupProject)
    private
      fSetupProject: Integer;
      fSetupTestRun: Integer;
      fSetupCase: Integer;
      fSetupTest: Integer;
      fCleanupTest: Integer;
      fCleanupCase: Integer;
      fCleanupTestRun: Integer;
      fCleanupProject: Integer;
    protected
      procedure SetupProject;
      procedure SetupTestRun;
      procedure Setup;
      procedure SetupTest(const aTest: ITestMethod);
      procedure CleanupTest(const aTest: ITestMethod);
      procedure Cleanup;
      procedure CleanupTestRun;
      procedure CleanupProject;

    published
      procedure TestCaseMetaData;
      procedure CounterValues;
    end;



implementation

  uses
  { vcl: }
    Classes,
    Graphics,
    SysUtils,
  { deltics: }
    Deltics.StrUtils;






{ TTestFramework --------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestFramework.AddCases;
  begin
    TTestControl.Create;
    TTestAbortCase.Create;
    TTestIsCritical.Create;
    TTestCounters.Create;
  end;







{ TSelfTest -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.ExpectedFailures;
  {
    These self tests have inverted expectations - tests that should clearly
     pass are stated as expected to fail and vice versa.

    We use the "ShouldPass/ShouldFail" post test conditions INCORRECTLY in
     these cases.

    For the test that passes we state that we expect it to fail and for the
     test that fails we state that we expect is to pass.  The result is that
     ALL tests fail.

    Paradoxically, this is the desired outcome in this specific test.
  }
  begin
    Test('TRUE = FALSE!').Expect(TRUE).Equals(FALSE).IsExpectedToFail;

    TheNextTest.IsExpectedToFail;
    Test('TRUE = FALSE!').Expect(TRUE).Equals(FALSE);

    TheNext(2).Tests.AreExpectedToFail;
    Test('1 = 0!').Expect(1).Equals(0);
    Test('2 + 2 = 5!').Expect(2 + 2).Equals(5);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.DeliberateFailures;
  {
  }
  begin
    Test('TRUE = FALSE!').Expect(TRUE).Equals(FALSE);
    Test('TRUE = FALSE!').Expect(TRUE).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.MixedResults;
  {
    This test is designed to complete with two test results, one which
     has passed and the other which has failed.
  }
  begin
    Test('TRUE').Expect(TRUE).Equals(TRUE);
    Test('TRUE').Expect(TRUE).Equals(FALSE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.TestStates;
  var
    method: ITestMethod;
  begin
    method := TestCase.MethodByName['DeliberateFailures'];

    Test('Located ''DeliberateFailures'' method!').Expect(method).IsAssigned
      .IsRequired.Because('If we can''t find the method then we can''t test it''s state');

    Test('DeliberateFailures : {expected} Failures!').Expect(method.Failures).Equals(2);
    Test('DeliberateFailures : {expected} Passes!').Expect(method.Passes).Equals(0);

    method := TestCase.MethodByName['MixedResults'];

    Test('Located ''MixedResults'' method!').Expect(method).IsAssigned
      .IsRequired.Because('If we can''t find the method then we can''t test it''s state');

    Test('MixedResults : {expected} Failures!').Expect(method.Failures).Equals(1);
    Test('MixedResults : {expected} Passes!').Expect(method.Passes).Equals(1);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.ConditionalTest;
  {
    This performs two tests.  However, the first test result is checked
     and the second test is only performed if the first test passes.  Since
     that initial test is deliberately designed to fail, the result is that
     the test will complete with only the first test having been performed.
  }
  begin
    TheNextTest.IsExpectedToFail;

    if Test.Expect(TRUE).Equals(FALSE).OK then
      Test.Expect(TRUE).Equals(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.DynamicTestExecution;
  {
    Disables the "DisabledTests" test case.
  }
  var
    method: ITestMethod;
  begin
    method := TestCase.MethodByName['disabledtests'];
    method.Enabled := FALSE;

    Test('DisabledTest.EffectivelyEnabled').Expect(method.EffectivelyEnabled).IsFALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.DisabledTests;
  {
    Tests disabling test for a given test run.  Will abort the test run if
     performed.
  }
  begin
    Abort('This test should be disabled before running the self-tests');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.AccessViolation;
  begin
    raise EAccessViolation.Create('EAccessViolation explicitly raised to test error trapping and state');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.AbortTest;
  begin
    Abort;
    Note('This note will not appear');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.AbortTestUsingIsRequired;
  begin
    TheNextTest.IsExpectedToFail;

    Test.Expect(TRUE).Equals(FALSE).IsRequired;
    Note('This note will not appear');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestControl.AbortTestBasedOnTestResult;
  begin
    TheNextTest.IsExpectedToFail;

    if NOT Test.Expect(TRUE).Equals(FALSE).OK then
      Abort;

    Note('This note will not appear');
  end;





{ TTestAbortCase --------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestAbortCase.AbortTestCase;
  begin
    AbortCase('This will abort the entire test case');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestAbortCase.NotPerformed;
  begin
    Note('This note will not appear as this test will not be performed');
  end;





{ TTestIsCritical -------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestIsCritical.TestIsCritical;
  begin
    TheNextTest.IsExpectedToFail;

    Test.Expect(TRUE).IsFALSE.IsCritical;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestIsCritical.NotPerformed;
  begin
    Note('This note will not appear as this test will not be performed');
  end;







{ TTestCounters ---------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.SetupProject;
  begin
    Inc(fSetupProject);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.SetupTestRun;
  begin
    Inc(fSetupTestRun);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.Setup;
  begin
    Inc(fSetupCase);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.SetupTest(const aTest: ITestMethod);
  begin
    Inc(fSetupTest);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.CleanupTest(const aTest: ITestMethod);
  begin
    Inc(fCleanupTest);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.Cleanup;
  begin
    Inc(fCleanupCase);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.CleanupTestRun;
  begin
    Inc(fCleanupTestRun);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.CleanupProject;
  begin
    Inc(fCleanupProject);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.TestCaseMetaData;
  begin
    Test('TestCount').Expect(TestCase.MethodCount).Equals(2);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestCounters.CounterValues;
  begin
    Test('SetupProject').Expect(fSetupProject).Equals(1);

    Test('SetupTestRun').Expect(fSetupTestRun).Equals(RunCount);
    Test('Setup (case)').Expect(fSetupCase).Equals(RunCount);
    Test('Setup (per test)').Expect(fSetupTest).Equals(TestCase.MethodCount * RunCount);
    Test('Cleanup (per test)').Expect(fCleanupTest).Equals((TestCase.MethodCount * RunCount) - 1);
    Test('Cleanup (case)').Expect(fCleanupCase).Equals(RunCount - 1);
    Test('CleanupTestRun').Expect(fCleanupTestRun).Equals(RunCount - 1);

    Test('CleanupProject').Expect(fCleanupProject).Equals(0);
  end;






initialization
  Smoketest.Add([TTestFramework]);

end.
