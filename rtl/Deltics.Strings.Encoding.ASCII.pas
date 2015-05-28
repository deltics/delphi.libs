
  unit Deltics.Strings.Encoding.ASCII;

interface

  uses
    Deltics.Strings.Encoding;


  type
    TASCIIEncoding = class(TMultiByteEncoding)
    protected
      constructor Create; override;
    public
      function GetByteCount(const aChars: PWIDEChar; const aCount: Integer): Integer; override;
      function GetCharCount(const aBytes: PByte; const aCount: Integer): Integer; override;
    end;



implementation


{ TASCIIEncoding }

  constructor TASCIIEncoding.Create;
  begin
    inherited Create(CP_ASCII);
  end;


  function TASCIIEncoding.GetByteCount(const aChars: PWIDEChar;
                                       const aCount: Integer): Integer;
  begin
    result := aCount div 2;
  end;


  function TASCIIEncoding.GetCharCount(const aBytes: PByte;
                                       const aCount: Integer): Integer;
  begin
    result := aCount;
  end;



end.
