{
  * X11 (MIT) LICENSE *

  Copyright © 2008 Jolyon Smith

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

{$i Deltics.RTL.inc}

{$ifdef deltics_stringtemplates}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}


  unit Deltics.StringTemplates;


interface

  uses
    Classes,
    Deltics.StrUtils;


  type
    TTemplatePart = class;
    TStringTemplate = class;


    TTemplatePartType = (
                         ptLiteral,
                         ptStringVar,
                         ptGUIDVar,
                         ptIntegerVar
                        );

    TArrayOfString = array of String;

    TStringTemplate = class
    private
      fMutexMark: String;
      fParts: array of TTemplatePart;
      fText: String;
      fVarOpen: String;
      fVarClose: String;
      fTypeMark: String;
      function get_PartCount: Integer;
      function get_Parts(const aIndex: Integer): TTemplatePart;
      procedure set_Text(const aValue: String);
      procedure DeleteParts;
      function DoMatches(const aString: String;
                         const aVars: TStrings;
                         const aAllowTypeMismatch: Boolean;
                         const aIgnoreCase: Boolean): Boolean;
    public
      class function Format(const aTemplate: String;
                            const aArgs: TArrayOfString): String;
      class function Match(const aTemplate: String;
                           const aString: String;
                           const aVars: TStrings = NIL): Boolean; overload;
      class function Match(const aTemplates: array of String;
                           const aString: String;
                           const aVars: TStrings = NIL): Boolean; overload;
      class function MatchText(const aTemplate: String;
                               const aString: String;
                               const aVars: TStrings = NIL): Boolean; overload;
      class function MatchText(const aTemplates: array of String;
                               const aString: String;
                               const aVars: TStrings = NIL): Boolean; overload;

      constructor Create(const aTemplate: String = '';
                         const aVarOpen: String = '[';
                         const aVarClose: String = ']';
                         const aTypeMark: String = ':';
                         const aMutexMark: String = '|');
      destructor Destroy; override;
      function Matches(const aString: String;
                       const aVars: TStrings = NIL;
                       const aAllowTypeMismatch: Boolean = FALSE): Boolean;
      function MatchesText(const aString: String;
                           const aVars: TStrings = NIL;
                           const aAllowTypeMismatch: Boolean = FALSE): Boolean;
      property PartCount: Integer read get_PartCount;
      property Parts[const aIndex: Integer]: TTemplatePart read get_Parts;
      property Text: String read fText write set_Text;
    end;


    TTemplatePart = class
    private
      fLength: Integer;
      fIsMutex: Boolean;
      fName: String;
      fPartType: TTemplatePartType;
      function get_IsFixedLength: Boolean;
    protected
      constructor Create(const aType: TTemplatePartType;
                         const aName: String;
                         const aLength: Integer;
                         const aMutex: Boolean);
    public
      property Length: Integer read fLength;
      property IsFixedLength: Boolean read get_IsFixedLength;
      property IsMutex: Boolean read fIsMutex;
      property Name: String read fName;
      property PartType: TTemplatePartType read fPartType;
    end;



implementation

  uses
    SysUtils,
    Deltics.Strings,
    Deltics.SysUtils;


{ TStringTemplate -------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TStringTemplate.Format(const aTemplate: String;
                                        const aArgs: TArrayOfString): String;
  var
    i: Integer;
    template: TStringTemplate;
    values: TStringList;
    part: TTemplatePart;
  begin
    result := '';

    template := TStringTemplate.Create(aTemplate);
    try
      values := TStringList.Create;
      try
        for i := 0 to Pred(Length(aArgs)) do
          values.Add(aArgs[i]);

        for i := 0 to template.PartCount do
        begin
          part := template.Parts[i];
          if (part.PartType <> ptLiteral) then
          begin
            if (part.Name <> '') and (values.IndexOfName(part.Name) <> -1) then
              result := result + values.Values[part.Name];
          end
          else
            result := result + part.Name
        end;

      finally
        values.Free;
      end;

    finally
      template.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TStringTemplate.Match(const aTemplate: String;
                                       const aString: String;
                                       const aVars: TStrings): Boolean;
  var
    template: TStringTemplate;
  begin
    template := TStringTemplate.Create(aTemplate);
    try
      result := template.Matches(aString, aVars);

    finally
      template.Free;
    end;
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TStringTemplate.MatchText(const aTemplate: String;
                                           const aString: String;
                                           const aVars: TStrings): Boolean;
  var
    template: TStringTemplate;
  begin
    template := TStringTemplate.Create(aTemplate);
    try
      result := template.MatchesText(aString, aVars);

    finally
      template.Free;
    end;
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TStringTemplate.Match(const aTemplates: array of String;
                                       const aString: String;
                                       const aVars: TStrings): Boolean;
  var
    i: Integer;
    template: TStringTemplate;
  begin
    result := FALSE;

    template := TStringTemplate.Create;
    try
      for i := 0 to Pred(Length(aTemplates)) do
      begin
        template.Text := aTemplates[i];
        result := template.Matches(aString, aVars);
        if result then
          BREAK;
      end;

    finally
      template.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TStringTemplate.MatchText(const aTemplates: array of String;
                                           const aString: String;
                                           const aVars: TStrings): Boolean;
  var
    i: Integer;
    template: TStringTemplate;
  begin
    result := FALSE;

    template := TStringTemplate.Create;
    try
      for i := 0 to Pred(Length(aTemplates)) do
      begin
        template.Text := aTemplates[i];
        result := template.MatchesText(aString, aVars);
        if result then
          BREAK;
      end;

    finally
      template.Free;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TStringTemplate.Create(const aTemplate: String;
                                     const aVarOpen: String;
                                     const aVarClose: String;
                                     const aTypeMark: String;
                                     const aMutexMark: String);
  begin
    fMutexMark  := aMutexMark;
    fTypeMark   := aTypeMark;
    fVarOpen    := aVarOpen;
    fVarClose   := aVarClose;

    Text := aTemplate;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TStringTemplate.Destroy;
  begin
    DeleteParts;

    inherited Destroy;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TStringTemplate.set_Text(const aValue: String);

    procedure AddPart(const aType: TTemplatePartType;
                      const aName: String;
                      const aLength: Integer = 0;
                      const aMutex: Boolean = FALSE);
    begin
      if (aType = ptLiteral) and (aName = '') then
        EXIT;

      SetLength(fParts, Length(fParts) + 1);
      fParts[Pred(Length(fParts))] := TTemplatePart.Create(aType, aName, aLength, aMutex);
    end;

  var
    s: String;
    svars: String;
    svar: String;
    stype: String;
    varStart, varEnd: Integer;
    varLengthMark: Integer;
    varTypeMark: Integer;
    varMutexMark: Integer;
    varLength: Integer;
    mutex: Boolean;
  begin
    if (fText = aValue) then
      EXIT;

    DeleteParts;

    s := aValue;

    while TRUE do
    begin
      varStart := Pos(fVarOpen, s);
      if (varStart = 1) and ((PartCount > 1) and (Parts[Pred(PartCount)].PartType <> ptLiteral)) then
        raise Exception.CreateFmt('Template ''%s'' contains consecutive variables', [aValue]);

      if (varStart = 0) then
      begin
        AddPart(ptLiteral, s);
        BREAK;
      end;

      varEnd := Pos(fVarClose, s);
      if (varEnd = 0) then
        raise Exception.CreateFmt('Unterminated variable in string template (''%s'') at %d', [aValue, varStart]);

      AddPart(ptLiteral, Copy(s, 1, varStart - 1));

      svars := Copy(s, varStart + Length(fVarOpen), varEnd - varStart - Length(fVarClose));
      Delete(s, 1, varEnd);

      mutex := FALSE;

      while (svars <> '') do
      begin
        varLengthMark := Pos('#' {fLengthMark}, svars);
        if (varLengthMark > 0) then
        begin
          varLength := StrToIntDef(Copy(svars, 1, varLengthMark - 1), -1);
          // TODO: ERROR IF LENGTH = -1
          Delete(svars, 1, varLengthMark + 1 {Length(LengthMark)} - 1);
        end
        else
          varLength := 0;

        varMutexMark := Pos(fMutexMark, svars);
        if (varMutexMark > 0) then
        begin
          svar := Copy(svars, 1, varMutexMark - 1);
          Delete(svars, 1, varMutexMark + Length(fMutexMark) - 1);
        end
        else
        begin
          svar := svars;
          svars := '';
        end;

        varTypeMark := Pos(fTypeMark, svar);
        if varTypeMark > 0 then
        begin
          stype := ANSILowercase(Copy(svar, varTypeMark + Length(fTypeMark), Length(svar) - varTypeMark));
          SetLength(svar, varTypeMark - 1);

          stype := STR.Trim(stype);
          case STR(stype).IndexIn(['string', 'str', 'guid', 'integer', 'int']) of
            0, 1: AddPart(ptStringVar, svar, varLength, mutex);
            2   : AddPart(ptGUIDVar, svar, varLength, mutex);
            3, 4: AddPart(ptIntegerVar, svar, varLength, mutex);
          else
            raise Exception.CreateFmt('Invalid part type specified (%s) in string template (''%s'') at %d', [stype, aValue, varStart]);
          end;
        end
        else
          AddPart(ptStringVar, svar, varLength, mutex);

        if (varMutexMark > 0) then
          mutex := TRUE;
      end;
    end;

    fText := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringTemplate.get_PartCount: Integer;
  begin
    result := Length(fParts)
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringTemplate.get_Parts(const aIndex: Integer): TTemplatePart;
  begin
    result := fParts[aIndex];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TStringTemplate.DeleteParts;
  var
    i: Integer;
  begin
    for i := 0 to Pred(Length(fParts)) do
      FreeAndNIL(fParts[i]);

    SetLength(fParts, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringTemplate.DoMatches(const aString: String;
                                     const aVars: TStrings;
                                     const aAllowTypeMismatch: Boolean;
                                     const aIgnoreCase: Boolean): Boolean;

    procedure AddVar(const aPart: TTemplatePart;
                     const aValue: String;
                     var aOK: Boolean);
    var
      i: Integer;
      guid: TGUID;
    begin
      aOK := FALSE;

      case aPart.PartType of
        ptLiteral,
        ptStringVar   : { NO-OP };

        ptIntegerVar  : result := TryStrToInt(aValue, i);
        ptGUIDVar     : result := TryStringToGUID(aValue, guid);
      end;

      aOK := result;

      if Assigned(aVars) then
      begin
        if result then
        begin
          if aPart.Name <> '' then
            aVars.Add(aPart.Name + '=' + aValue)
          else
            aVars.Add(aValue);
        end
        else
        begin
          result := aAllowTypeMismatch;
          if result then
            aVars.Add('');
        end;
      end
      else
        result := result or aAllowTypeMismatch;
    end;

  var
    i: Integer;
    remainder: IString;
    part: TTemplatePart;
    nextLiteral: TTemplatePart;
    partIndex: Integer;
    varEnd: Integer;
    svar: String;
    ok: Boolean;
  begin
    result := (aString <> '');
    if NOT result then
      EXIT;

    if Assigned(aVars) then
      aVars.Clear;

    remainder := STR(aString);
    partIndex := 0;

    while result and (partIndex < PartCount) do
    begin
      part := Parts[partIndex];

      if (part.PartType = ptLiteral) then
      begin
        if remainder.BeginsWithText(Parts[partIndex].Name) then
        begin
          remainder.TrimLeft(Length(Parts[partIndex].Name));
          Inc(partIndex);
        end
        else
          result := FALSE;
      end
      else if Succ(partIndex) <= Pred(PartCount) then
      begin
        if part.IsFixedLength then
        begin
          svar := remainder.ExtractLeft(part.Length);
          i    := partIndex + 1;
        end
        else
        begin
          nextLiteral := NIL;

          for i := partIndex to Pred(PartCount) do
            if Parts[i].PartType = ptLiteral then
            begin
              nextLiteral := Parts[i];
              BREAK;
            end;

          if Assigned(nextLiteral) then
          begin
            if aIgnoreCase then
              remainder.FindFirstText(nextLiteral.Name, varEnd)
            else
              remainder.FindFirst(nextLiteral.Name, varEnd);

            if varEnd > 0 then
              svar := remainder.Leftmost(varEnd - 1)
            else
              BREAK;
          end
          else
          begin
            i     := PartCount;
            svar  := remainder.ToString;
          end;
        end;

        while (partIndex < i) do
        begin
          part    := Parts[partIndex];
          result  := TRUE;
          AddVar(part, svar, ok);
          if ok then
          begin
            remainder.TrimLeft(Length(svar));
            BREAK;
          end
          else
            Inc(partIndex)
        end;

        partIndex := i;
      end
      else
      begin
        AddVar(part, remainder.ToString, ok);
        remainder := NIL;
        BREAK;
      end;
    end;

    result := result AND NOT Assigned(remainder);

    if result and Assigned(aVars) then
      for i := 0 to Pred(aVars.Count) do
      begin
        svar := Trim(aVars.ValueFromIndex[i]);
        if (svar <> '') then
          aVars.ValueFromIndex[i] := svar
        else
          aVars[i] := aVars.Names[i] + '=';
      end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringTemplate.Matches(const aString: String;
                                   const aVars: TStrings;
                                   const aAllowTypeMismatch: Boolean): Boolean;
  begin
    result := DoMatches(aString, aVars, aAllowTypeMismatch, FALSE);
  end;



  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TStringTemplate.MatchesText(const aString: String;
                                       const aVars: TStrings;
                                       const aAllowTypeMismatch: Boolean): Boolean;
  begin
    result := DoMatches(aString, aVars, aAllowTypeMismatch, TRUE);
  end;








{ TTemplatePart ---------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTemplatePart.Create(const aType: TTemplatePartType;
                                   const aName: String;
                                   const aLength: Integer;
                                   const aMutex: Boolean);
  begin
    fIsMutex  := aMutex;
    fName     := aName;
    fPartType := aType;
    fLength   := aLength;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTemplatePart.get_IsFixedLength: Boolean;
  begin
    result := (fLength > 0);
  end;







end.
