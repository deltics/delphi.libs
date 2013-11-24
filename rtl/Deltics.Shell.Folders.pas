
  unit Deltics.Shell.Folders;

interface

  uses
    Deltics.Strings;

  type
    TShellFolders = class
      class function AppData(const aForceCreate: Boolean = TRUE): UnicodeString;
    end;


implementation

  uses
    SysUtils,
    Windows,
    Deltics.Shell.API;


  function GetFolderPath(aFlags: Integer; const aCreate: Boolean): UnicodeString;
  begin
    if aCreate then
      aFlags := aFlags or CSIDL_FLAG_CREATE;

    SetLength(result, MAX_PATH);

    SHGetFolderPath(0, aFlags, 0, 0, @result[1]);
    SetLength(result, WIDE.Len(@result[1]));
  end;



{ TShellFolders }

  class function TShellFolders.AppData(const aForceCreate: Boolean): UnicodeString;
  begin
    result := GetFolderPath(CSIDL_LOCAL_APPDATA, aForceCreate);
  end;



end.
