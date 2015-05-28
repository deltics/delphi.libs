
{$i deltics.rtl.inc}

  unit Deltics.Delphi.Versions;

interface

  type
    TDelphiVersion = (
                      dvUnknown,
                      dvDelphi1,
                      dvDelphi2,
                      dvDelphi3,
                      dvDelphi4,
                      dvDelphi5,
                      dvDelphi6,
                      dvDelphi7,
                      dvDelphi8,
                      dvDelphi2005,
                      dvDelphi2006,
                      dvDelphi2007,
                      dvDelphi2009,
                      dvDelphi2010,
                      dvDelphiXE,
                      dvDelphiXE2,
                      dvDelphiXE3,
                      dvDelphiXE4,
                      dvDelphiXE5,
                      dvDelphiXE6,
                      dvDelphiXE7,
                      dvDelphiXE8
                     );

    TDelphiEdition = (
                      deUnknown,
                      dePersonal,
                      deExplorer,
                      deProfessional,
                      deEnterprise,
                      deArchitect
                     );

  const
    dvCurrent = dvUnknown;


  type
    TDelphi = class
      class function CurrentVersion: TDelphiVersion;
      class function ProductName(const aVersion: TDelphiVersion = dvCurrent): String;
      class function VersionFromName(const aName: String): TDelphiVersion;
    end;


implementation

  uses
    Deltics.Strings;


  const
    PRODUCT_NAME      : array[TDelphiVersion] of String
                          = (
                             'Unknown Delphi Version',
                             'Delphi 1',
                             'Delphi 2',
                             'Delphi 3',
                             'Delphi 4',
                             'Delphi 5',
                             'Delphi 6',
                             'Delphi 7',
                             'Delphi 8',
                             'Delphi 2005',
                             'Delphi 2006',
                             'Delphi 2007',
                             'Delphi 2009',
                             'Delphi 2010',
                             'Delphi XE',
                             'Delphi XE2',
                             'Delphi XE3',
                             'Delphi XE4',
                             'Delphi XE5',
                             'Delphi XE6',
                             'Delphi XE7',
                             'Delphi XE8'
                            );


  class function TDelphi.CurrentVersion: TDelphiVersion;
  begin
    result := {$ifdef DELPHI7}dvDelphi7{$endif}
              {$ifdef DELPHI2005}dvDelphi2005{$endif}
              {$ifdef DELPHI2006}dvDelphi2006{$endif}
              {$ifdef DELPHI2007}dvDelphi2007{$endif}
              {$ifdef DELPHI2009}dvDelphi2009{$endif}
              {$ifdef DELPHI2010}dvDelphi2010{$endif}
              {$ifdef DELPHIXE}dvDelphiXE{$endif}
              {$ifdef DELPHIXE2}dvDelphiXE2{$endif}
              {$ifdef DELPHIXE3}dvDelphiXE3{$endif}
              {$ifdef DELPHIXE4}dvDelphiXE4{$endif}
              {$ifdef DELPHIXE5}dvDelphiXE5{$endif}
              {$ifdef DELPHIXE6}dvDelphiXE6{$endif}
              {$ifdef DELPHIXE7}dvDelphiXE7{$endif}
              {$ifdef DELPHIXE8}dvDelphiXE8{$endif};
  end;


  class function TDelphi.ProductName(const aVersion: TDelphiVersion): String;
  begin
    if (aVersion = dvCurrent) then
      result := PRODUCT_NAME[CurrentVersion]
    else
      result := PRODUCT_NAME[aVersion];
  end;


  class function TDelphi.VersionFromName(const aName: String): TDelphiVersion;
  var
    v: TDelphiVersion;
  begin
    result := dvUnknown;

    for v := High(TDelphiVersion) downto dvUnknown do
      if STR(aName).Contains(PRODUCT_NAME[v], csIgnoreCase) then
      begin
        result := v;
        EXIT;
      end;
  end;




end.
