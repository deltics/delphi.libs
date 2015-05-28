
  unit Deltics.Strings.Encoding.UTF32;

interface

  uses
    Deltics.Strings.Encoding;


  type
    TUTF32LEEncoding = class(TMultiByteEncoding)
    protected
      constructor Create; override;
      function get_BOM: TBOM; override;
    end;


    TUTF32BEEncoding = class(TUTF32LEEncoding)
    protected
      constructor Create; override;
      function get_BOM: TBOM; override;
    end;


implementation

  uses
    Windows,
    Deltics.SysUtils;


{ TUTF32LEEncoding }

  constructor TUTF32LEEncoding.Create;
  begin
    inherited Create(CP_UTF32LE);
  end;


  function TUTF32LEEncoding.get_BOM: TBOM;
  begin
    SetLength(result, 4);
    result[0] := BOM_UTF32LE[0];
    result[1] := BOM_UTF32LE[1];
    result[2] := BOM_UTF32LE[2];
    result[3] := BOM_UTF32LE[3];
  end;






{ TUTF32BEEncoding }

  constructor TUTF32BEEncoding.Create;
  begin
    inherited Create(CP_UTF32);
  end;


  function TUTF32BEEncoding.get_BOM: TBOM;
  begin
    SetLength(result, 4);
    result[0] := BOM_UTF32BE[0];
    result[1] := BOM_UTF32BE[1];
    result[2] := BOM_UTF32BE[2];
    result[3] := BOM_UTF32BE[3];
  end;






end.
