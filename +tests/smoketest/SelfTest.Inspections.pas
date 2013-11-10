

  unit SelfTest.Inspections;

{$i selftest.inc}

interface

  uses
    Deltics.Smoketest;


  type
    TTestInspections = class(TTestCase)
      procedure BasicTypes;
      procedure ExoticTypes;
    end;



implementation

  uses
  { vcl: }
    ActiveX,
    Classes,
  { deltics: }
    Deltics.SysUtils;


{ TTestInspections ------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestInspections.BasicTypes;
  var
    sgl: Single;
    dbl: Double;
    ext: Extended;
    curr: Currency;
  begin
    sgl   := Sqrt(2);
    dbl   := Sqrt(2);
    ext   := Sqrt(2);
    curr  := Sqrt(2);

  {$ifdef EnhancedOverloads}
    Note('Compiler supports additional overloaded values for inspections');

    Inspect('Double Precision').Value(dbl);
    Inspect('Currency').Value(curr);

    Note('Now repeat these inspections using the extension syntax');
  {$endif}

    (Inspect('Double Precision') as DoubleInspector).Value(dbl);
    (Inspect('Currency') as CurrencyInspector).Value(curr);

    Inspect('Single Precision').Value(sgl);
    Inspect('Extended Precision').Value(ext);
    Inspect('Integer').Value(42);
    Inspect('Boolean').Value(TRUE);
    Inspect('String').Value('The quick brown fox™');
    Inspect('Indexed Item')[0].Value(0);
    Inspect('Indexed Item')[1].Value(1);
    Inspect('Indexed Item')[2].Value(2);
    Inspect('Named part')['sub-item'].Value('Sub-item Value');

    Inspect('Single: %g / Double: %g').Values([sgl, dbl]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestInspections.ExoticTypes;
  var
    uid: TGUID;
    sl: TStringList;
  begin
    AutoFree([@sl]);

    CoCreateGUID(uid);
    Inspect('GUID').Value(uid);

    sl := TStringList.Create;
    sl.Add('First item');
    sl.Add('Second item');
    sl.Add('Third item');
    Inspect('Stringlist').Value(sl);
    Inspect('Stringlist').MonoSpaced.Value(sl);


    sl.Clear;
    Inspect('Empty Stringlist').Value(sl);
  end;







initialization
  Smoketest.Add([TTestInspections]);

end.
