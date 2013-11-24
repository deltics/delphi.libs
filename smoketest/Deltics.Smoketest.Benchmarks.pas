
{$i deltics.smoketest.inc}

  unit Deltics.Smoketest.Benchmarks;

interface

  uses
    Classes,
    Deltics.Classes,
    Deltics.Smoketest,
    Deltics.Strings;

  type
    TBenchmarkData = class(TCOMInterfacedObject, IBenchmarkData)
    private
      fMethods: array of UnicodeString;
      fSeries: array of UnicodeString;
      fResults: array of array of Double;
    private // IBenchmarkData
      function get_Method(const aIndex: Integer): UnicodeString;
      function get_MethodCount: Integer;
      function get_Result(const aMethodIndex: Integer; const aSeriesIndex: Integer): Double;
      function get_Series(const aIndex: Integer): UnicodeString;
      function get_SeriesCount: Integer;
    end;


    TCompareCaseResults = class(TBenchmark)
    private
      fCases: TList;
    protected
      function get_Count: Integer; override;
      function Contains(const aCase: IPerformanceCase): Boolean; override;
      function GetData(const aCase: IPerformanceCase): IBenchmarkData; override;
      procedure SaveCase(const aCase: IPerformanceCase); override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(const aCase: TPerformanceCase);
    end;


    TCompilerVersionResults = class(TBenchmark)
    private
      fFiles: TStringList;
      function get_Case(const aFileIndex, aCaseIndex: Integer): IPerformanceCase;
      function get_CaseCount(const aIndex: Integer): Integer;
      function get_Filename(const aIndex: Integer): UnicodeString;
      function get_Tag(const aIndex: Integer): UnicodeString;
    protected
      function get_Count: Integer; override;
      function Contains(const aCase: IPerformanceCase): Boolean; override;
      function GetData(const aCase: IPerformanceCase): IBenchmarkData; override;
      procedure SaveCase(const aCase: IPerformanceCase); override;
      procedure SaveResults(const aCase: IPerformanceCase; const aFilename: UnicodeString; const aTag: UnicodeString);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(const aCase: TPerformanceCase; const aTag: UnicodeString; const aFilename: UnicodeString);
      property CasesCount[const aIndex: Integer]: Integer read get_CaseCount;
      property Cases[const aFileIndex, aCaseIndex: Integer]: IPerformanceCase read get_Case;
      property Filename[const aIndex: Integer]: UnicodeString read get_Filename;
      property Tag[const aIndex: Integer]: UnicodeString read get_Tag;
    end;


implementation

  uses
    SysUtils,
    Deltics.Delphi.Versions,
    Deltics.JSON,
    Deltics.Shell,
    Deltics.Shell.Folders;





{ TCompareCaseResults }

  constructor TCompareCaseResults.Create;
  begin
    inherited Create;

    fCases := TList.Create;
  end;


  destructor TCompareCaseResults.Destroy;
  begin
    fCases.Free;

    inherited;
  end;


  procedure TCompareCaseResults.Add(const aCase: TPerformanceCase);
  begin
    fCases.Add(aCase);
  end;


  function TCompareCaseResults.Contains(const aCase: IPerformanceCase): Boolean;
  var
    c: TPerformanceCase;
  begin
    result := aCase.AsObject(c, TPerformanceCase);
    result := result and (fCases.IndexOf(c) <> -1);
  end;


  function TCompareCaseResults.GetData(const aCase: IPerformanceCase): IBenchmarkData;
  var
    i, j, k: Integer;
    data: TBenchmarkData;
    c: IPerformanceCase;
    m: IPerformanceMethod;
  begin
    if NOT Contains(aCase) then
      EXIT;

    data := TBenchmarkData.Create;
    SetLength(data.fSeries, fCases.Count);
    SetLength(data.fMethods, aCase.MethodCount);
    SetLength(data.fResults, aCase.MethodCount);

    for i := 0 to Pred(fCases.Count) do
    begin
      c := TPerformanceCase(fCases[i]) as IPerformanceCase;

      data.fSeries[i] := c.DisplayName;

      for j := 0 to Pred(aCase.MethodCount) do
      begin
        SetLength(data.fResults[j], Length(data.fSeries));

        m := aCase.MethodByIndex[j];
        data.fMethods[j] := m.DisplayName;

        for k := 0 to Pred(c.MethodCount) do
          if (c.MethodByIndex[k].Name = m.Name) then
            data.fResults[j][i] := c.MethodByIndex[k].AverageRuntime;
      end;
    end;

    result := data;
  end;


  function TCompareCaseResults.get_Count: Integer;
  begin
    result := fCases.Count;
  end;


  procedure TCompareCaseResults.SaveCase(const aCase: IPerformanceCase);
  begin
    // TO DO: Save case comparison results to output file
  end;




{ TCompilerVersionResults }

  constructor TCompilerVersionResults.Create;
  begin
    inherited Create;

    fFiles := TStringList.Create;
  end;


  destructor TCompilerVersionResults.Destroy;
  begin
    while (fFiles.Count > 0) do
    begin
      TList(fFiles.Objects[0]).Free;
      fFiles.Delete(0);
    end;
    fFiles.Free;

    inherited;
  end;


  function TCompilerVersionResults.Contains(const aCase: IPerformanceCase): Boolean;
  var
    i, j: Integer;
  begin
    result := FALSE;

    for i := 0 to Pred(Count) do
      for j := 0 to Pred(CasesCount[i]) do
      begin
        result := (Cases[i, j].ID = aCase.ID);
        if result then
          EXIT;
      end;
  end;


  function TCompilerVersionResults.GetData(const aCase: IPerformanceCase): IBenchmarkData;
  var
    i, j: Integer;
    data: TBenchmarkData;
    json: TJSONObject;
    version: TDelphiVersion;
    versionName: String;
    versionData: TJSONObject;
    caseData: TJSONObject;
    methodName: UnicodeString;
  begin
    json    := NIL;
    result  := NIL;

    for i := 0 to Pred(Count) do
      if Assigned(json) then
        BREAK
      else
        for j := 0 to Pred(CasesCount[i]) do
          if (Cases[i, j].ID = aCase.ID) then
          begin
            json := TJSONObject.CreateFromFile(Filename[i]);
            BREAK;
          end;

    if NOT Assigned(json) then
      EXIT;

    try
      data := TBenchmarkData.Create;
      SetLength(data.fSeries, json.ValueCount);
      SetLength(data.fMethods, aCase.MethodCount);
      SetLength(data.fResults, aCase.MethodCount);

      i := 0;
      for version := dvDelphi7 to High(TDelphiVersion) do
      begin
        versionName := TDelphi.ProductName(version);

        if NOT json.Contains(versionName) then
          CONTINUE;

        versionData := json[versionName].AsObject;
        data.fSeries[i] := versionName;

        if versionData.Contains(aCase.Name) then
        begin
          caseData := versionData[aCase.Name].AsObject['methods'].AsObject;
          for j := 0 to Pred(aCase.MethodCount) do
          begin
            SetLength(data.fResults[j], Length(data.fSeries));

            methodName := aCase.MethodByIndex[j].Name;
            data.fMethods[j] := methodName;
            if caseData.Contains(methodName) then
              data.fResults[j][i] := caseData[methodName].AsDouble;
          end;
        end;

        Inc(i);
      end;

      result := data;

    finally
      json.Free;
    end;
  end;


  function TCompilerVersionResults.get_Case(const aFileIndex, aCaseIndex: Integer): IPerformanceCase;
  var
    obj: TObject;
  begin
    obj := TObject(TList(fFiles.Objects[aFileIndex])[aCaseIndex]);
    obj.GetInterface(IPerformanceCase, result);
  end;


  function TCompilerVersionResults.get_CaseCount(const aIndex: Integer): Integer;
  begin
    result := TList(fFiles.Objects[aIndex]).Count;
  end;


  function TCompilerVersionResults.get_Count: Integer;
  begin
    result := fFiles.Count;
  end;


  function TCompilerVersionResults.get_Filename(const aIndex: Integer): UnicodeString;
  var
    notUsed: UnicodeString;
  begin
    WIDE.Split(fFiles[aIndex], WideChar('*'), result, notUsed);
  end;


  function TCompilerVersionResults.get_Tag(const aIndex: Integer): UnicodeString;
  var
    notUsed: UnicodeString;
  begin
    WIDE.Split(fFiles[aIndex], WideChar('*'), notUsed, result);
  end;


  procedure TCompilerVersionResults.SaveCase(const aCase: IPerformanceCase);
  var
    i, j: Integer;
  begin
    for i := 0 to Pred(Count) do
      for j := 0 to Pred(CasesCount[i]) do
        if Cases[i, j].ID = aCase.ID then
          SaveResults(aCase, Filename[i], Tag[i]);
  end;


  procedure TCompilerVersionResults.SaveResults(const aCase: IPerformanceCase;
                                                const aFilename: UnicodeString;
                                                const aTag: UnicodeString);
  (*
    Saves a JSON file with the following structure:

    {
      "Delphi Version": { "Case Name 1": { "date-time": <datetime stamp>,
                                           "methods": { "method-name-1": <result>,
                                                        "method-name-2": <result>,
                                                        "method-name-3": <result> } },

                          "Case Name 2": { "date-time": <datetime stamp>,
                                           "methods": { "method-name-1": <result>,
                                                        "method-name-2": <result>,
                                                        "method-name-3": <result> } }
    }

  *)
  var
    json: TJSONObject;
    sVersion: String;
    obj: TJSONObject;
    results: TJSONObject;
    i: Integer;
    method: IPerformanceMethod;
  begin
    if FileExists(aFilename) then
      json := TJSONObject.CreateFromFile(aFilename)
    else
      json := TJSONObject.Create;
    try
      sVersion := TDelphi.ProductName;

      if (aTag <> '') then
        sVersion := sVersion + ' #' + aTag;

      if json.Contains(sVersion) then
        obj := json[sVersion].AsObject
      else
        obj := json.AddObject(sVersion);

      if obj.Contains(aCase.Name) then
        obj.Delete(aCase.Name);

      obj := obj.AddObject(aCase.Name);
      obj.AddDateTime('date-time', Smoketest.TestRun.StartTime);
      results := obj.AddObject('methods');

      for i := 0 to Pred(aCase.MethodCount) do
      begin
        method := aCase.MethodByIndex[i];

        results.Add(method.Name, method.AverageRuntime);
      end;

      json.SaveToFile(aFilename, TRUE);

    finally
      json.Free;
    end;
  end;


  procedure TCompilerVersionResults.Add(const aCase: TPerformanceCase;
                                        const aTag: UnicodeString;
                                        const aFilename: UnicodeString);
  var
    idx: Integer;
    filename: UnicodeString;
  begin
    filename := TFileSystem.MakePath([TShellFolders.AppData,
                                      'nz.co.deltics\smoketests'], aFilename, TRUE);

    if aTag <> '' then
      filename := filename + '*' + aTag
    else
      filename := filename;

    idx := fFiles.IndexOf(filename);

    if (idx = -1) then
      idx := fFiles.AddObject(filename, TList.Create);

    if TList(fFiles.Objects[idx]).IndexOf(aCase) = -1 then
      TList(fFiles.Objects[idx]).Add(aCase);
  end;







{ TBenchmarkData }

  function TBenchmarkData.get_Method(const aIndex: Integer): UnicodeString;
  begin
    result := fMethods[aIndex];
  end;

  function TBenchmarkData.get_MethodCount: Integer;
  begin
    result := Length(fMethods);
  end;

  function TBenchmarkData.get_Result(const aMethodIndex, aSeriesIndex: Integer): Double;
  begin
    result := fResults[aMethodIndex][aSeriesIndex];
  end;

  function TBenchmarkData.get_Series(const aIndex: Integer): UnicodeString;
  begin
    result := fSeries[aIndex];
  end;

  function TBenchmarkData.get_SeriesCount: Integer;
  begin
    result := Length(fSeries);
  end;




end.
