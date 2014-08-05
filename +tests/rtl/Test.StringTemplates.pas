

  unit Test.StringTemplates;


interface

  uses
    Deltics.Smoketest;


  type
    TStringTemplateTests = class(TTestCase)
      procedure SimpleTests;
    end;


implementation

  uses
    Classes,
    Deltics.StringTemplates;


{ TStringTemplateTests }

  procedure TStringTemplateTests.SimpleTests;
  var
    matched: Boolean;
    values: TStringList;
  begin
    values  := TStringList.Create;
    matched := TStringTemplate.Match('[album] - [track] - [title].[format]', 'Get Lucky - 01 - Border Reiver.mp3', values);

    Test('[album] - [track] - [title].[format]').Expect(matched).IsTRUE;
    Test('[album] - [track] - [title].[format]').Expect(values.Count).Equals(4);
    Test('[album]').Expect(values.Values['album']).Equals('Get Lucky');
    Test('[track]').Expect(values.Values['track']).Equals('01');
    Test('[title]').Expect(values.Values['title']).Equals('Border Reiver');
    Test('[format]').Expect(values.Values['format']).Equals('mp3');

    matched := TStringTemplate.Match('[artist] - [album]', 'Океан Ельзи - Суперсиметрія', values);
    Test('[artist] - [album]').Expect(matched).IsTRUE;
    Test('[artist] - [album]').Expect(values.Count).Equals(2);
    Test('[artist]').Expect(values.Values['artist']).Equals('Океан Ельзи');
    Test('[album]').Expect(values.Values['album']).Equals('Суперсиметрія');

    matched := TStringTemplate.Match('Океан Ельзи - [album]', 'Океан Ельзи - Суперсиметрія', values);
    Test('Океан Ельзи - [album]').Expect(matched).IsTRUE;
    Test('Океан Ельзи - [album]').Expect(values.Count).Equals(1);
    Test('[album]').Expect(values.Values['album']).Equals('Суперсиметрія');

    values.Free;
  end;





initialization
  Smoketest.Add(TStringTemplateTests);
end.
