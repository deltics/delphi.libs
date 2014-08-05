

  unit Deltics.ADO;


interface

  uses
    Classes,
    Deltics.ADO.Parameters;

  type
    IADOConnection = interface
    ['{13CB39E7-B9FD-4ABE-87D6-85C458A609F9}']
    end;


    IADOCommand = interface
    ['{A37C1368-1033-4419-9283-C46C4E6F90D1}']
    end;


    IADOQuery = interface(IADOCommand)
    ['{BE46A627-247E-402C-8B90-509BEFB3CE1C}']
      function get_Parameters: TADOParameters;
      function get_SQL: TStringList;
      property Parameters: TADOParameters read get_Parameters;
      property SQL: TStringList read get_SQL;
    end;



implementation

end.
