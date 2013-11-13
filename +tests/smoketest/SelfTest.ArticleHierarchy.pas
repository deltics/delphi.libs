

  unit SelfTest.ArticleHierarchy;


interface

  uses
    Deltics.Smoketest;


  type
    TArticleHierarchy = class(TTestCase)
    end;

    TTestRoot = class(TTestCase)
    end;


    TTestWithChildren = class(TTestCase, IHaveChildCases)
    private
      procedure AddCases;
    end;


    TTestWithDynamicChildren = class(TTestCase)
    end;


    TTestWithGrandChildren = class(TTestCase)
    end;


    TChildTest = class(TTestCase);


    TParameterisedCase = class(TTestCase, IHaveChildCases)
    private
      fParamValue: String;
      procedure AddCases;
      procedure WithParam(const aString: String);
    published
      procedure CaseParam;
    end;


    TParameterisedChild = class(TTestCase)
      procedure InspectOwnerParam;
    end;


    THierarchyTests = class(TTestCase)
      procedure References;
      procedure Relationships;
      procedure ParameterValues;
    end;



implementation


{ TTestWithChildren ------------------------------------------------------------------------------ }

  procedure TTestWithChildren.AddCases;
  begin
    TChildTest.Create;
    TParameterisedCase.Create.WithParam('FIRST');
    TParameterisedCase.Create.WithParam('SECOND');
  end;



{ TParameterisedCase }

  procedure TParameterisedCase.CaseParam;
  begin
    Inspect('Case Param').Value(fParamValue);
    Inspect('Reference').Value(Reference);
  end;


  procedure TParameterisedCase.WithParam(const aString: String);
  begin
    fParamValue := aString;
  end;


  procedure TParameterisedCase.AddCases;
  begin
    TParameterisedChild.Create;
  end;



{ TParameterisedChild }

  procedure TParameterisedChild.InspectOwnerParam;
  var
    parent: TParameterisedCase;
  begin
    if self.Parent.AsObject(parent, TParameterisedCase) then
      Inspect('Parent Case Param').Value(parent.fParamValue);
  end;









{ THierarchyTests }

  procedure THierarchyTests.References;
  var
    tc: ITestCase;
    method: ITestMethod;
  begin
    tc := Smoketest.FindCase('\Article Hierarchy');
    Test('''\Article Hierarchy'' found!').Expect(tc).IsAssigned;
    Test('''\Article Hierarchy'' reference').Expect(tc.Reference).Equals('1');

    tc := Smoketest.FindCase('\Article Hierarchy\Test Root');
    Test('''\Article Hierarchy\Test Root'' found!').Expect(tc).IsAssigned;
    Test('''\Article Hierarchy\Test Root'' reference').Expect(tc.Reference).Equals('1.1');

    tc := Smoketest.FindCase('Article Hierarchy');
    Test('''Article Hierarchy'' is not found in current test case!').Expect(tc).IsNIL;

    method := Smoketest.FindMethod('Relationships');
    Test('''Relationships'' method found!').Expect(method).IsAssigned;
    Test('Method name').Expect(method.Name).Equals('Relationships');
    Test('Method reference').Expect(method.Reference).Equals(Reference + '.2');

    method := Smoketest.FindMethod('\Article Hierarchy\Test Root\Test With Children\parameterised case\case param');
    Test('''Case Param'' method found!').Expect(method).IsAssigned.IsRequired;
    Test('Method name').Expect(method.Name).Equals('CaseParam');
    Test('Method display name').Expect(method.DisplayName).Equals('Case Param');
    Test('Method reference').Expect(method.Reference).Equals('1.1.1.2.1');

    method := TestCase.MethodByName['Parameter Values'];
    Test('''Parameter Values'' method found!').Expect(method).IsAssigned.IsRequired.Because('If we fail to find the method we cannot test it''s meta data');
    Test('Method name').Expect(method.Name).Equals('ParameterValues');
    Test('Method display name').Expect(method.DisplayName).Equals('Parameter Values');
    Test('Method reference').Expect(method.Reference).Equals(Reference + '.3');
  end;


  procedure THierarchyTests.Relationships;
  begin

  end;


  procedure THierarchyTests.ParameterValues;
  begin

  end;



{ TArticleHierarchy }

initialization
  Smoketest.Add(TArticleHierarchy, [TTestRoot]);

  Smoketest.Add(TTestRoot, [TTestWithChildren,
                            TTestWithDynamicChildren,
                            TTestWithGrandChildren]);

  Smoketest.Add(TTestWithGrandChildren, [TTestWithChildren]);

  Smoketest.Add([THierarchyTests]);
end.
