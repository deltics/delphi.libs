
  unit Deltics.Strings.Encoding.UTF8;

interface

  uses
    Deltics.Strings.Encoding;


  type
    TUTF8Encoding = class(TMultiByteEncoding)
    protected
      constructor Create; override;
      function get_BOM: TBOM; override;
    end;



implementation


{ TUTF8Encoding }

  constructor TUTF8Encoding.Create;
  begin
    inherited Create(CP_UTF8);
  end;


  function TUTF8Encoding.get_BOM: TBOM;
  begin
    SetLength(result, 3);
    result[0] := BOM_UTF8[0];
    result[1] := BOM_UTF8[1];
    result[2] := BOM_UTF8[2];
  end;





end.
