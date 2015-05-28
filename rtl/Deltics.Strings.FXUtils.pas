
  unit Deltics.Strings.FXUtils;

interface

  uses
    Windows,
    Deltics.Strings;


  const
    FXCOMPAREFLAG_CASE: array[csCaseSensitive..csIgnoreCase] of Cardinal = (0, NORM_IGNORECASE);


  type
    FXContains = class
      class function HasResult(const aNeed: TContainNeeds; var aResult: Boolean; var aFoundOne: Boolean): Boolean;
      class procedure CheckFinalResult(const aNeed: TContainNeeds; var aResult: Boolean; var aFoundOne: Boolean);
    end;

  function IntToCompareResult(const A: Integer): TCompareResult;




implementation


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function IntToCompareResult(const A: Integer): TCompareResult;
  begin
    if A = 0 then      result := isEqual
    else if A < 0 then result := isLesser
    else               result := isGreater;
  end;




  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function FXContains.HasResult(const aNeed: TContainNeeds;
                                      var   aResult: Boolean;
                                      var   aFoundOne: Boolean): Boolean;
  begin
    result := TRUE;

    case aNeed of
      cnAny   : if aResult then
                  EXIT;

      cnEvery : if NOT aResult then
                  EXIT;

      cnOneOf : if aResult then
                begin
                  if {already} aFoundOne then
                  begin
                    aResult := FALSE;
                    EXIT;
                  end
                  else
                    aFoundOne := TRUE;
                end;
    end;

    result := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure FXContains.CheckFinalResult(const aNeed: TContainNeeds;
                                              var   aResult: Boolean;
                                              var   aFoundOne: Boolean);
  begin
    case aNeed of
      cnAny   : aResult := FALSE;
      cnEvery : aResult := TRUE;
      cnOneOf : aResult := (aFoundOne = TRUE);
    end;
  end;





end.
