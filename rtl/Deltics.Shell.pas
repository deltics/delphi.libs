
  unit Deltics.Shell;

interface

  uses
    Deltics.Strings;

  type
    TFileSystem = class
      class function MakePath(const aParts: array of UnicodeString; const aCreate: Boolean = FALSE): UnicodeString; overload;
      class function MakePath(const aParts: array of UnicodeString; const aFilename: UnicodeString; const aCreatePath: Boolean = FALSE): UnicodeString; overload;
    end;


implementation

  uses
    SysUtils,
    Windows;


  class function TFileSystem.MakePath(const aParts: array of UnicodeString;
                                      const aCreate: Boolean): UnicodeString;
  const
    APPEND_STRATEGY : array[FALSE..TRUE, FALSE..TRUE] of Integer = ((0, 1),(1, 2));
  var
    i: Integer;
  begin
    case Length(aParts) of
      0 : result := '';
      1 : result := aParts[0];
    else
      i := 0;
      while (aParts[i] = '') do
        Inc(i);

      result := aParts[i];

      for i := Succ(i) to Pred(Length(aParts)) do
      begin
        if (aParts[i] = '') then
          CONTINUE;

        case APPEND_STRATEGY[result[Length(result)] = '\', aParts[i][1] = '\'] of
          0 : result := result + '\' + aParts[i];
          2 : result := result + Copy(aParts[i], 2, Length(aParts[i]));
          1 : result := result + aParts[i];
        end
      end;

      if result[Length(result)] = '\' then
        SetLength(result, Length(result) - 1);
    end;

    if (result <> '') then
    begin
      if result[Length(result)] = '\' then
        SetLength(result, Length(result) - 1);

      if aCreate and NOT DirectoryExists(result) then
        ForceDirectories(result);
    end;
  end;


  class function TFileSystem.MakePath(const aParts: array of UnicodeString;
                                      const aFilename: UnicodeString;
                                      const aCreatePath: Boolean): UnicodeString;
  begin
    result := MakePath(aParts, aCreatePath) + '\' + aFilename;
  end;


end.
