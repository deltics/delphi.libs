{
  * X11 (MIT) LICENSE *

  Copyright © 2011 Jolyon Smith

  Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
   so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.


  * GPL and Other Licenses *

  The FSF deem this license to be compatible with version 3 of the GPL.
   Compatability with other licenses should be verified by reference to those
   other license terms.


  * Contact Details *

  Original author : Jolyon Smith
  skype           : deltics
  e-mail          : <EXTLINK mailto: jsmith@deltics.co.nz>jsmith@deltics.co.nz</EXTLINK>
  website         : <EXTLINK http://www.deltics.co.nz>www.deltics.co.nz</EXTLINK>
}

{$i deltics.rtl.inc}

{$ifdef deltics_commandline}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.CommandLine;


interface

  uses
  { vcl: }
    Classes,
    SysUtils,
  { deltics: }
    Deltics.Classes,
    Deltics.Types;


  type
    TCommandLine = class;
    TCommandLineSwitch = class;

    EInvalidCommandLine = class(Exception);


    PSwitchDef = ^TSwitchDef;
    TSwitchDef = record
      Name: String;
      Switch: String;
      Default: String;
      MinValues: Integer;
      MaxValues: Integer;
      Separator: Char;
    end;


    TCommandLine = class
    private
      fCommandLine: String;
      fParameterDefs: array of String;
      fParameterDefaults: array of String;
      fParameters: TStringList;
      fSetupCount: Integer;
      fSwitchDefs: array of TSwitchDef;
      fSwitches: TStringList;
      function get_EXEFilename: String;
      function get_EXEPath: String;
      function get_InSetup: Boolean;
      function get_Other(const aIndex: Integer): String;
      function get_OtherCount: Integer;
      function get_Parameter(const aName: String): String;
      function get_Switch(const aName: String): TCommandLineSwitch;
      function get_Text: String;
      procedure DefineSwitch(const aName: String;
                             const aSwitch: String;
                             const aMinValues: Integer;
                             const aMaxValues: Integer;
                             const aValueSep: Char;
                             const aDefault: String); overload;
      function IsSwitch(const aString: String; var aSwitch: PSwitchDef; var aValue: String): Boolean;
      property InSetup: Boolean read get_InSetup;
    protected
      procedure Define; virtual;
    public
      constructor Create(const aCommandLine: String = '');
      procedure AfterConstruction; override;
      destructor Destroy; override;

      procedure Clear(const aReset: Boolean = FALSE);
      procedure BeginSetup;
      procedure EndSetup;
      procedure DefineParameter(const aName: String;
                                const aDefault: String = '');
      procedure DefineSwitch(const aSwitch: String); overload;
      procedure DefineSwitch(const aSwitch: String; const aDefault: String); overload;
// TODO:
//      procedure DefineValue(const aName: String; const aValueSep: String); overload;
//      procedure DefineValue(const aName: String; const aValueSep: String; const aDefault: String); overload;
      procedure Parse(const aCommandLine: String = '');
      property EXEPath: String read get_EXEPath;
      property EXEFilename: String read get_EXEFilename;
      property OtherCount: Integer read get_OtherCount;
      property Other[const aIndex: Integer]: String read get_Other;
      property Parameter[const aName: String]: String read get_Parameter;
      property Switch[const aName: String]: TCommandLineSwitch read get_Switch;
      property Text: String read get_Text;
    end;


    TCommandLineSwitch = class
    private
      fSpecified: Boolean;
      fValues: TStringList;
      constructor Create(const aDefault: String); reintroduce;
      function get_Value: String;
      procedure SetValues(const aValues: TStringList);
    public
      destructor Destroy; override;
      property Specified: Boolean read fSpecified;
      property Value: String read get_Value;
      property Values: TStringList read fValues;
    end;


  procedure CommandLineToArgs(const aString: String; const aArgs: TStrings);


implementation

  uses
  { deltics: }
    Deltics.StrUtils;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure CommandLineToArgs(const aString: String; const aArgs: TStrings);
  {
    Breaks a string into white-space separated elements.  Values in
     double-quotes are treated as single elements.  Double quotes within
     such values must be double-double-quoted:

    e.g.

      3 args:   arg1 "arg 2" arg3
      2 args:   arg1 arg2prefix;"arg 2 value";arg2suffix
      2 args:   arg1 arg2prefix;"arg ""2"" value";arg2suffix
  }
  const
    WHITESPACE = [' ', #8];
  var
    i: Integer;
    arg: String;
    inQuote: Boolean;
  begin
    inQuote := FALSE;
    arg     := '';
    i       := 1;

    while (i <= Length(aString)) do
    begin
      if inQuote then
      begin
        arg := arg + aString[i];

        if (aString[i] = '"') then
        begin
          if ((i = Length(aString)) or (aString[i + 1] <> '"')) then
            inQuote := FALSE;

          if ((i < Length(aString)) and (aString[i + 1] = '"')) then
          begin
            arg := arg + '"';
            Inc(i);
          end;
        end;

        Inc(i);
      end
      else if (aString[i] = '"') then
      begin
        inQuote := TRUE;
        arg     := arg + '"';
        Inc(i);
      end
      else if (ANSIChar(aString[i]) in WHITESPACE) then
      begin
        if (arg <> '') then
        begin
          aArgs.Add(arg);
          arg := '';
        end;

        while (i <= Length(aString)) and (ANSIChar(aString[i]) in WHITESPACE) do
          Inc(i);
      end
      else
      begin
        arg := arg + aString[i];
        Inc(i);
      end;
    end;

    if (arg <> '') then
      aArgs.Add(arg);
  end;




{ TCommandLine ----------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TCommandLine.Create(const aCommandLine: String);
  begin
    inherited Create;

    fCommandLine := aCommandLine;
    if fCommandLine = '' then
      fCommandLine := CmdLine;

    fParameters   := TStringList.Create;
    fSwitches     := TStringList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TCommandLine.Destroy;
  begin
    Clear(TRUE);

    FreeAndNIL(fSwitches);
    FreeAndNIL(fParameters);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.get_EXEFilename: String;
  begin
    result := ExtractFilename(ParamStr(0));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.get_EXEPath: String;
  begin
    result := ExtractFilePath(ParamStr(0));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.get_InSetup: Boolean;
  begin
    result := (fSetupCount > 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.get_Other(const aIndex: Integer): String;
  begin
    result := fParameters[aIndex + Length(fParameterDefs)];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.get_OtherCount: Integer;
  begin
    result := fParameters.Count - Length(fParameterDefs);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.get_Parameter(const aName: String): String;
  begin
    if fParameters.IndexOfName(aName) <> -1 then
      result := fParameters.Values[aName]
    else
      raise Exception.CreateFmt('No such command line parameter defined: ''%s''', [aName]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.get_Switch(const aName: String): TCommandLineSwitch;
  begin
    if fSwitches.IndexOf(aName) <> -1 then
      result := TCommandLineSwitch(fSwitches.Objects[fSwitches.IndexOf(aName)])
    else
      raise Exception.CreateFmt('No such command line switch defined: ''%s''', [aName]);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.get_Text: String;
  begin
    result := CmdLine;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TCommandLine.IsSwitch(const aString: String;
                                 var aSwitch: PSwitchDef;
                                 var aValue: String): Boolean;
  {
    Determines whether or not the specified string identifies a valid
     command-line switch, separating the switch from an associated
     value where present.

    A switch value is everything occuring after the switch and either
     a : or = separator, enabling switch values to include these symbols
     if required, e.g.

    String              Switch   aValue

     -d=DEBUG             -d      DEBUG
     -i:ABC;DEF           -i      ABC;DEF
     -a:Windows=WinAPI    -a      Windows=WinAPI
     -p=c:\output         -p      c:\output
  }
  var
    i: Integer;
    eqPos: Integer;
    coPos: Integer;
    sep: Char;
    sw: String;
    parts: TStringArray;
  begin
    sw := aString;

    // Locate any ':' and/or '=' symbols in the string
    eqPos := Pos('=', aString);
    coPos := Pos(':', aString);

    if (eqPos = 0) and (coPos = 0) then         // No ':' or '=' found - there is no value
      sep := #0
    else if (coPos = 0) and (eqPos <> 0) then   // No ':'. '=' separates switch from value
      sep := '='
    else if (eqPos = 0) and (coPos <> 0) then   // No '='. ':' separates switch from value
      sep := ':'
    else if (coPos < eqPos) then                // ':' before '='.  ':' separates switch from value
      sep := ':'
    else
      sep := '=';                               // '=' before ':'.  '=' separates switch from value

    // We identified a separator so split the string into the switch and value components
    if sep <> #0 then
      case Split(aString, sep, parts) of
        1 : sw := aString;
        2 : begin
              sw      := parts[0];
              aValue  := parts[1];
            end;
      end;

    // Locate a switch definition in the command line settings
    for i := 0 to Pred(Length(fSwitchDefs)) do
    begin
      aSwitch := @fSwitchDefs[i];
      result  := (aSwitch.Switch = sw);
      if result then
        EXIT;
    end;

    // No switch definition corresponding to the specified string - it's not
    //  a switch and there is no value

    aSwitch := NIL;
    aValue  := '';
    result  := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.Define;
  begin
    // NO-OP
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.DefineParameter(const aName: String;
                                         const aDefault: String);
{$ifopt C+}
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(fParameterDefs)) do
      if SameText(fParameterDefs[i], aName) then
        ASSERT(FALSE, 'Command line parameter ''' + aName + ''' already defined');
{$else}
  begin
{$endif}
    BeginSetup;
    try
      SetLength(fParameterDefs,     Length(fParameterDefs) + 1);
      SetLength(fParameterDefaults, Length(fParameterDefaults) + 1);

      fParameterDefs[Length(fParameterDefs) - 1]      := aName;
      fParameterDefaults[Length(fParameterDefs) - 1]  := aDefault;

    finally
      EndSetup;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.AfterConstruction;
  begin
    inherited AfterConstruction;

    BeginSetup;
    try
      Define;

    finally
      EndSetup;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.BeginSetup;
  begin
    Inc(fSetupCount);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.EndSetup;
  begin
    if (fSetupCount > 0) then
      Dec(fSetupCount);

    if (fSetupCount = 0) then
      Parse;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.DefineSwitch(const aName: String;
                                      const aSwitch: String;
                                      const aMinValues: Integer;
                                      const aMaxValues: Integer;
                                      const aValueSep: Char;
                                      const aDefault: String);
  var
    notUsedDef: PSwitchDef;
    notUsedStr: String;
  begin
    ASSERT(NOT IsSwitch(aSwitch, notUsedDef, notUsedStr), 'Command line switch ''' + aSwitch + ''' already defined');

    BeginSetup;
    try
      SetLength(fSwitchDefs, Length(fSwitchDefs) + 1);

      fSwitchDefs[Length(fSwitchDefs) - 1].Name       := aName;
      fSwitchDefs[Length(fSwitchDefs) - 1].Switch     := aSwitch;
      fSwitchDefs[Length(fSwitchDefs) - 1].Default    := aDefault;
      fSwitchDefs[Length(fSwitchDefs) - 1].MinValues  := aMinValues;
      fSwitchDefs[Length(fSwitchDefs) - 1].MaxValues  := aMaxValues;
      fSwitchDefs[Length(fSwitchDefs) - 1].Separator  := aValueSep;

    finally
      EndSetup;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.DefineSwitch(const aSwitch: String);
  begin
    DefineSwitch(aSwitch, aSwitch, 0, 0, #0, '');
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.DefineSwitch(const aSwitch, aDefault: String);
  begin
    // If a default value is specified then the inference is that a value does
    //  not HAVE to be specified on the command line.  If a default is NOT
    //  specified then a value is required (if the switch is specified)

    if (aDefault <> '') then
      DefineSwitch(aSwitch, aSwitch, 0, 1, #0, aDefault)
    else
      DefineSwitch(aSwitch, aSwitch, 0, 999, #0, aDefault);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.Clear(const aReset: Boolean);
  var
    i: Integer;
  begin
    for i := 0 to Pred(fSwitches.Count) do
      fSwitches.Objects[i].Free;

    fParameters.Clear;
    fSwitches.Clear;

    if aReset then
      SetLength(fSwitchDefs, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TCommandLine.Parse(const aCommandLine: String);
  {
    app.exe param1 param2 -s1 s1value1;s2value2 -s2 s2value1 s2value2 -s3
  }
  var
    i: Integer;
    s: String;
    elements: TStringList;
    param: Integer;
    prev: PSwitchDef;
    switch: PSwitchDef;
    value: String;
    values: TStringList;
  begin
    if InSetup then
      EXIT;

    Clear;

    if (aCommandLine = '') then
      fCommandLine := CmdLine
    else
      fCommandLine := '"' + ParamStr(0) + '" ' + aCommandLine;

    elements := TStringList.Create;
    try
      CommandLineToArgs(fCommandLine, elements);

      param   := -1;
      prev    := NIL;
      switch  := NIL;
      values  := NIL;

      // Ensure that named parameters will be listed first
      for i := 0 to Pred(Length(fParameterDefs)) do
        fParameters.Add(fParameterDefs[i] + '=' + fParameterDefaults[i]);

      for i := 0 to Pred(Length(fSwitchDefs)) do
        fSwitches.AddObject(fSwitchDefs[i].Name, TCommandLineSwitch.Create(fSwitchDefs[i].Default));

      for i := 1 to Pred(elements.Count) do
      begin
        s := elements[i];

        if IsSwitch(s, switch, value) then
        begin
          if Assigned(prev) and (values.Count < prev.MinValues) then
            raise EInvalidCommandLine.CreateFmt('''%s'' command line option (%s) does not have enough parameters (%d required)',[prev.Name, prev.Switch, prev.MinValues]);

          values := TStringList.Create;

          while (value <> '') do
            values.Add(StrDequote(StrPopQuoted(value, ';')));

          if (values.Count = 0) and (switch.Default <> '') then
            values.Add(switch.Default);

          TCommandLineSwitch(fSwitches.Objects[fSwitches.IndexOf(switch.Name)]).SetValues(values);

          prev := switch;
        end
        else if Assigned(prev) and (values.Count < prev.MaxValues) then
        begin
          while (Pos(prev.Separator, s) <> 0) do
          begin
            values.Add(StrDequote(Copy(s, 1, Pos(prev.Separator, s) - 1)));
            Delete(s, 1, Pos(prev.Separator, s));
          end;

          values.Add(StrDequote(s));
        end
        else if NOT Assigned(prev) or (values.Count = prev.MaxValues) then
        begin
          prev    := NIL;
          switch  := NIL;

          if (param < Length(fParameterDefs) - 1) then
          begin
            Inc(param);
            fParameters.Values[fParameterDefs[param]] := StrDequote(s);
          end
          else
            fParameters.Add(StrDequote(s));
        end
        else
          raise EInvalidCommandLine.CreateFmt('''%s'' command line option (%s) has too many parameters',[prev.Name, prev.Switch]);
      end;

    finally
      elements.Free;
    end;
  end;




{ TCommandLineSwitch }

  constructor TCommandLineSwitch.Create(const aDefault: String);
  begin
    inherited Create;

    fValues := TStringList.Create;
    fValues.Add(aDefault);
  end;


  procedure TCommandLineSwitch.SetValues(const aValues: TStringList);
  begin
    fSpecified := TRUE;

    FreeAndNIL(fValues);
    fValues := aValues;
  end;


  destructor TCommandLineSwitch.Destroy;
  begin
    FreeAndNIL(fValues);

    inherited;
  end;


  function TCommandLineSwitch.get_Value: String;
  begin
    if Assigned(fValues) and (fValues.Count = 1) then
      result := fValues[0]
    else
      result := '';
  end;





end.
