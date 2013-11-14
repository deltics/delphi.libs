
{$i deltics.inc}

  unit Test.JSON;

interface

  uses
  { deltics: }
    Deltics.SmokeTest,
    Deltics.JSON;


  type
    TUnitTest_JSON = class(TTestCase, INameCase,
                                      ISetupTestCase,
                                      ICleanupTestCase,
                                      ICleanupTest)
    private
      fJSON: TJSONObject;
    protected
      function NameForCase: UnicodeString;
      procedure Setup;
      procedure Cleanup;
      procedure CleanupTest(const aTest: TDelegate);
    public
      property JSON: TJSONObject read fJSON;
    published
      procedure StringDecoding;
      procedure StringEncoding;
      procedure ValueAsJSON;
      procedure InitialiseSimpleObject;
      procedure InitialiseComplexObject;
      procedure SimpleObjectFromString;
      procedure ReadPrettyObjectFromUTF8String;
      procedure UnicodeEscaping;
    end;


implementation

  uses
  { vcl: }
    SysUtils,
  { deltics: }
    Deltics.DateUtils,
    Deltics.Strings,
    Deltics.SysUtils;


  const
    PRETTY_OBJECT = '{'#13
                  + '  "first"  : 10,'#13
                  + '  "second" : true,'#13
                  + '  "third"  : "This is the 3rd item",'#13
                  + '  "fourth" : "This item contains a Unicode ''trademark'' symbol: \u2122",'#13
                  + '  "fifth"  : [],'#13
                  + '  "sixth"  : ['#13
                  + '             ],'#13
                  + '  "seventh": null,'#13
                  + '  "Windows™ 8": "Microsoft™",'#13
                  + '  "ninth"  : { }'#13
                  + '}';


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TUnitTest_JSON.NameForCase: UnicodeString;
  begin
    result := 'Deltics.JSON';
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.Setup;
  begin
    fJSON := TJSONObject.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.Cleanup;
  begin
    FreeAndNIL(fJSON);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.CleanupTest(const aTest: TDelegate);
  begin
    fJSON.Clear;
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.StringDecoding;
  begin
    Test('String (ASCII)').     Expect(TJSONString.Decode('"windows"')).Equals('windows');
    Test('String (Unicode)').   Expect(TJSONString.Decode('Windows\u2122')).Equals('Windows™');
    Test('String (path)').      Expect(TJSONString.Decode('\\\\psf\\home')).Equals('\\psf\home');
    Test('String (url)').       Expect(TJSONString.Decode('"www.deltics.co.nz\/blog"')).Equals('www.deltics.co.nz/blog');;
    Test('String (quotes)').    Expect(TJSONString.Decode('\"Come here\", he said')).Equals('"Come here", he said');
    Test('String (ctrl chars)').Expect(TJSONString.Decode('\ttabbed\r\nnew lines\b\fnew page')).Equals(#9'tabbed'#13#10'new lines'#8#12'new page');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.StringEncoding;
  begin
    Test('String (ASCII)').     Expect(TJSONString.Encode('windows')).Equals('"windows"');
    Test('String (Unicode)').   Expect(TJSONString.Encode('Windows™')).Equals('"Windows\u2122"');
    Test('String (path)').      Expect(TJSONString.Encode('\\psf\home')).Equals('"\\\\psf\\home"');
    Test('String (url)').       Expect(TJSONString.Encode('www.deltics.co.nz/blog')).Equals('"www.deltics.co.nz\/blog"');
    Test('String (quotes)').    Expect(TJSONString.Encode('"Come here", he said')).Equals('"\"Come here\", he said"');
    Test('String (ctrl chars)').Expect(TJSONString.Encode(#9'tabbed'#13#10'new lines'#8#12'new page')).Equals('"\ttabbed\r\nnew lines\b\fnew page"');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.ValueAsJSON;
  var
    d: TDate;
    t: TTime;
    dt: TDateTime;
    dn: Double;
    dr: Double;
    obj: TJSONObject;
    arr: TJSONArray;
  begin
    d   := DateTimeFromISO8601('19631122', [dtDate]);
    t   := DateTimeFromISO8601('123456.7890', [dtTime]);
    dt  := DateTimeFromISO8601('19631122 123456.7890');

    dn := 42;
    dr := 3.14159;

    JSON.Add('bool true',   TRUE);
    JSON.Add('bool false',  FALSE);
    JSON.AddNull('null');

    JSON.Add('integer',           42);
  {$ifdef EnhancedOverloads}
    JSON.Add('natural double',    dn);
    JSON.Add('real double',       dr);
  {$else}
    JSON.AddDouble('natural double',  dn);
    JSON.AddDouble('real double',     dr);
  {$endif}

    JSON.Add('ascii string',      'windows');
    JSON.Add('unicode string',    'Windows™');
    JSON.Add('path',              '\\psf\home');
    JSON.Add('url',               'www.deltics.co.nz/blog');
    JSON.Add('ctrl chars',        #9'tabbed'#13#10'new lines'#8#12'new page');

  {$ifdef EnhancedOverloads}
    JSON.Add('date',      d);
    JSON.Add('time',      t);
    JSON.Add('datetime',  dt);
  {$else}
    JSON.AddDate('date',      d);
    JSON.AddTime('time',      t);
    JSON.AddDateTime('datetime',  dt);
  {$endif}
    JSON.Add('guid',      StringToGUID('{F2B65EEF-E45C-4F3C-B65E-C0CE208C44D7}'));

    Test('Boolean (TRUE)').     Expect(JSON.Values['bool true'].AsJSON).      Equals('true');
    Test('Boolean (FALSE)').    Expect(JSON.Values['bool false'].AsJSON).     Equals('false');
    Test('Null').               Expect(JSON.Values['null'].AsJSON).           Equals('null');

    Test('Integer').            Expect(JSON.Values['integer'].AsJSON).        Equals('42');
    Test('Double (natural)').   Expect(JSON.Values['natural double'].AsJSON). Equals('42.0');
    Test('Double (real)').      Expect(JSON.Values['real double'].AsJSON).    Equals('3.14159');

    Test('String (ASCII)').     Expect(JSON.Values['ascii string'].AsJSON).   Equals('"windows"');
    Test('String (Unicode)').   Expect(JSON.Values['unicode string'].AsJSON). Equals('"Windows\u2122"');
    Test('String (path)').      Expect(JSON.Values['path'].AsJSON).           Equals('"\\\\psf\\home"');
    Test('String (url)').       Expect(JSON.Values['url'].AsJSON).            Equals('"www.deltics.co.nz\/blog"');
    Test('String (ctrl chars)').Expect(JSON.Values['ctrl chars'].AsJSON).     Equals('"\ttabbed\r\nnew lines\b\fnew page"');

    Test('Date').     Expect(JSON.Values['date'].AsJSON).     Equals('"19631122"');
    Test('Time').     Expect(JSON.Values['time'].AsJSON).     Equals('"123456.7890"');
    Test('DateTime'). Expect(JSON.Values['datetime'].AsJSON). Equals('"19631122 123456.7890"');
    Test('GUID').     Expect(JSON.Values['guid'].AsJSON).     Equals('"{F2B65EEF-E45C-4F3C-B65E-C0CE208C44D7}"');

    obj := TJSONObject.Create;
    try
      obj.Add('1', 'first');
      obj.Add('2', 'second');
      obj.Add('3', '©2013');
      Test('object').Expect(obj.AsJSON).Equals('{"1":"first","2":"second","3":"\u00A92013"}');

    finally
      obj.Free;
    end;

    arr := TJSONArray.Create;
    try
      arr.Add('first');
      arr.Add('second');
      arr.Add('©2013');
      Test('array').Expect(arr.AsJSON).Equals('["first","second","\u00A92013"]');

    finally
      arr.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.InitialiseSimpleObject;
  const
    EXPECTED_RESULT = '{"ID":1,"Name":"Test Object","Boolean":true,"Null":null,"Empty String":""}';
  begin
    JSON.Add('ID',            1);
    JSON.Add('Name',          'Test Object');
    JSON.Add('Boolean',       TRUE);
    JSON.Add('Null',          '').Clear;
    JSON.Add('Empty String',  '');

    (Test('JSON')['ID (type)'] as EnumTest).ForEnum(TypeInfo(TJSONValueType)).Expect(JSON['ID'].ValueType).Equals(Ord(jsNumber));
    (Test('JSON')['Name (type)'] as EnumTest).ForEnum(TypeInfo(TJSONValueType)).Expect(JSON['Name'].ValueType).Equals(Ord(jsString));
    (Test('JSON')['Boolean (type)'] as EnumTest).ForEnum(TypeInfo(TJSONValueType)).Expect(JSON['Boolean'].ValueType).Equals(Ord(jsBoolean));
    (Test('JSON')['Null (type)'] as EnumTest).ForEnum(TypeInfo(TJSONValueType)).Expect(JSON['Null'].ValueType).Equals(Ord(jsString));
    (Test('JSON')['Empty String (type)'] as EnumTest).ForEnum(TypeInfo(TJSONValueType)).Expect(JSON['Empty String'].ValueType).Equals(Ord(jsString));

    Test('JSON')['ID (value)'].Expect(JSON['ID'].AsInteger).Equals(1);
    Test('JSON')['Name (value)'].Expect(JSON['Name'].AsString).Equals('Test Object');
    Test('JSON')['Boolean (value)'].Expect(JSON['Boolean'].AsBoolean).IsTRUE;

    try
      Test('JSON')['Null (value)'].Expect(JSON['Null'].AsString).Equals('');

    except
      Test.Expecting(EJSONError);
    end;

    Test('JSON')['Null (value)'].Expect(JSON['Null'].IsNull).IsTRUE;
    Test('JSON')['Empty String (value)'].Expect(JSON['Empty String'].AsString).Equals('');

    Test('JSON.AsString is correctly formatted!').Expect(JSON.AsString).Equals(EXPECTED_RESULT);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.InitialiseComplexObject;
  const
    EXPECTED_RESULT = '{"ID":1,"Name":"Test Object","Array":[{"Index":1,"Text":"One"},{"Index":2,"Text":"Two"}]}';
  var
    template: TJSONObject;
    itemArray: TJSONArray;
  begin
    JSON.Add('ID',   1);
    JSON.Add('Name', 'Test Object');

    template := TJSONObject.Create;
    try
      template.Add('Index', 0);
      template.Add('Text',  '');

      itemArray := JSON.AddArray('Array');

      with itemArray.AddObject(template) do
      begin
        Values['Index'].AsString  := '1';
        Values['Text'].AsString   := 'One';
      end;

      with itemArray.AddObject(template) do
      begin
        Values['Index'].AsString  := '2';
        Values['Text'].AsString   := 'Two';
      end;
    finally
      template.Free;
    end;

    Test['JSON.AsString'].Expect(JSON.AsString).Equals(EXPECTED_RESULT);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.SimpleObjectFromString;
  const
    EXPECTED_RESULT = '{"ID":1,"Name":"Test Object","Amount":42.0,"Boolean":true,"Null":null,"Empty String":""}';
  begin
    JSON.AsString := EXPECTED_RESULT;

    Test['JSON.AsString'].Expect(JSON.AsString).Equals(EXPECTED_RESULT);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.ReadPrettyObjectFromUTF8String;
  var
    local: TJSONObject;
  begin
    local := TJSONObject.CreateFromString(UTF8.Encode(PRETTY_OBJECT)) as TJSONObject;
    try
      Inspect('AsDisplayText').Value(local.AsDisplayText);

      Test('First value').Expect(local['first'].AsInteger).Equals(10);
      Test('Second value').Expect(local['second'].AsBoolean).Equals(TRUE);

      Test('™ symbol decoded in "fourth" item value!').Expect(local['fourth'].AsString).Contains('™');
      Test('™ symbol decoded in 8th item name!').Expect(local.ValueByIndex[7].Name).Contains('™');
      Test('™ symbol decoded in 8th item value!').Expect(local.ValueByIndex[7].AsString).Contains('™');

      Test('5th is an empty array!').Expect(local['fifth'].AsArray.Count).Equals(0);
      Test('6th is an empty array!').Expect(local['sixth'].AsArray.Count).Equals(0);
      Test('7th is null!').Expect(local['seventh'].IsNull).IsTRUE;
      Test('9th is an empty object!').Expect(local['ninth'].AsObject.ValueCount).Equals(0);

      // TODO: Separate test for this...

      Inspect('AsDisplayText').MonoSpaced.Value(local.AsDisplayText);

      local.AsString := WIDE.FromUTF8(UTF8.Encode(PRETTY_OBJECT));

      Inspect('AsDisplayText').Value(local.AsDisplayText);

      Test('First value').Expect(local['first'].AsInteger).Equals(10);
      Test('Second value').Expect(local['second'].AsBoolean).Equals(TRUE);

      Test('™ symbol decoded in "fourth" item value!').Expect(local['fourth'].AsString).Contains('™');
      Test('™ symbol decoded in 8th item name!').Expect(local.ValueByIndex[7].Name).Contains('™');
      Test('™ symbol decoded in 8th item value!').Expect(local.ValueByIndex[7].AsString).Contains('™');

    finally
      local.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TUnitTest_JSON.UnicodeEscaping;
  const
    DATA = '{"name" : "This item contains a Unicode ''trademark'' symbol: \u2122"}';
  begin
    JSON.AsString := DATA;
    Test('Unicode unescaped').Expect(JSON['name'].AsString).Contains('™');

    Test('Unicode escaped ({actual})').Expect(JSON['name'].AsJSON).Contains('\u2122');
  end;





initialization
  Smoketest.Add([TUnitTest_JSON]);

end.
