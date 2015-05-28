
  unit Benchmark.Strings.RTL;

{$i deltics.inc}

interface

  uses
    Deltics.Smoketest,
    Benchmark.Strings;


  type
    TRTLPerformanceA = class(TStringPerformanceCase, INameCase)
    private
      function NameForCase: UnicodeString;
    published
      procedure Pos_Char;
      procedure Pos_Str;
      procedure Pos_NextChar;
      procedure Pos_NextStr;
    end;

    TRTLPerformanceW = class(TStringPerformanceCase, INameCase)
    private
      function NameForCase: UnicodeString;
    published
      procedure Pos_Char;
      procedure Pos_Str;
      procedure Pos_NextChar;
      procedure Pos_NextStr;
    end;

  {$ifdef FASTSTRINGS}
    TFastStringsPerformance = class(TStringPerformanceCase, INameCase)
    private
      function NameForCase: UnicodeString;
    published
      procedure Pos_Char;
      procedure Pos_Str;
      procedure Pos_NextChar;
      procedure Pos_NextStr;
    end;
  {$endif}


implementation

{$ifNdef DELPHIXE3_OR_LATER}
  uses
  {$ifdef FASTSTRINGS}
    FastStrings,
  {$endif}
    StrUtils;
{$endif}


{ TRTLPerformance -------------------------------------------------------------------------------- }

  const// 0         1         2         3         4
    STR_A : ANSIString  = 'The quick, quick fox!  I said: The quick fox!';
    F_A   : ANSIChar    = 'f';
    FOX_A : ANSIString  = 'fox';

    STR_W : UnicodeString = 'The quick, quick fox!  I said: The quick fox!';
    F_W   : WIDEChar      = 'f';
    FOX_W : UnicodeString = 'fox';



  function TRTLPerformanceA.NameForCase: UnicodeString;
  begin
    result := 'RTL (ANSI)';
  end;


  procedure TRTLPerformanceA.Pos_Char;
  var
    p: Integer;
  begin
    p := Pos(F_A, STR_A);
  end;


  procedure TRTLPerformanceA.Pos_Str;
  var
    p: Integer;
  begin
    p := Pos(FOX_A, STR_A);
  end;


  procedure TRTLPerformanceA.Pos_NextChar;
  var
    p: Integer;
  begin
    p := 21;
  {$ifdef DELPHIXE3_OR_LATER}
    p := Pos(F_A, STR_A, p);
  {$else}
    p := PosEx(F_A, STR_A, p);
  {$endif}
  end;


  procedure TRTLPerformanceA.Pos_NextStr;
  var
    p: Integer;
  begin
    p := 21;
  {$ifdef DELPHIXE3_OR_LATER}
    p := Pos(FOX_A, STR_A, p);
  {$else}
    p := PosEx(FOX_A, STR_A, p);
  {$endif}
  end;





  function TRTLPerformanceW.NameForCase: UnicodeString;
  begin
    result := 'RTL (WIDE)';
  end;


  procedure TRTLPerformanceW.Pos_Char;
  var
    p: Integer;
  begin
    p := Pos(F_W, STR_W);
  end;


  procedure TRTLPerformanceW.Pos_Str;
  var
    p: Integer;
  begin
    p := Pos(FOX_W, STR_W);
  end;


  procedure TRTLPerformanceW.Pos_NextChar;
  var
    p: Integer;
  begin
    p := 21;
  {$ifdef DELPHIXE3_OR_LATER}
    p := Pos(F_W, STR_W, p);
  {$else}
    p := PosEx(F_W, STR_W, p);
  {$endif}
  end;


  procedure TRTLPerformanceW.Pos_NextStr;
  var
    p: Integer;
  begin
    p := 21;
  {$ifdef DELPHIXE3_OR_LATER}
    p := Pos(FOX_W, STR_W, p);
  {$else}
    p := PosEx(FOX_W, STR_W, p);
  {$endif}
  end;




{$ifdef FASTSTRINGS}
  function TFastStringsPerformance.NameForCase: UnicodeString;
  begin
    result := 'FastStrings (ANSI)';
  end;


  procedure TFastStringsPerformance.Pos_Char;
  var
    p: Integer;
  begin
    p := FastCharPos(STR_A, F_A, 1);
  end;


  procedure TFastStringsPerformance.Pos_Str;
  var
    p: Integer;
  begin
    p := FastPos(STR_A, FOX_A, Length(STR_A), Length(FOX_A), 1);
  end;


  procedure TFastStringsPerformance.Pos_NextChar;
  var
    p: Integer;
  begin
    p := FastCharPos(STR_A, F_A, 21);
  end;


  procedure TFastStringsPerformance.Pos_NextStr;
  var
    p: Integer;
  begin
    p := FastPos(STR_A, FOX_A, Length(STR_A), Length(FOX_A), 21);
  end;
{$endif}

end.
