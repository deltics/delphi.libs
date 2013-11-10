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

{$i deltics.rtl.inc}

{$ifdef deltics_statelist}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.StateList;


interface

  uses
  { vcl: }
    Classes,
    SyncObjs,
  { deltics: }
    Deltics.MultiCast;


  type
    TStateID = PChar;


    TState = packed record
      ID: TStateID;
      case Counted: Boolean of
        TRUE: (Count: Cardinal);
        FALSE: (Entered: LongBool);
    end;
    PState = ^TState;


    TStateChangeEvent = procedure(Sender: TObject; const aStateID: TStateID) of object;


    TMultiCastStateChange = class(TMultiCastNotify)
    private
      fEventState: TStateID;
      fInitial: TStateChangeEvent;
      fFinal: TStateChangeEvent;
    protected
      procedure Call(const aMethod: TMethod); override;
    public
      procedure Add(const aHandler: TStateChangeEvent);
      procedure Remove(const aHandler: TStateChangeEvent);
      procedure DoEvent(const aStateID: TStateID);
      property Initial: TStateChangeEvent read fInitial write fInitial;
      property Final: TStateChangeEvent read fFinal write fFinal;
    end;



    TStateList = class
    private
      _CriticalSection: TCriticalSection;

      fOwner: TObject;
      fStates: array of TState;

      eOn_Change: TMultiCastStateChange;
      eOn_Enter: TMultiCastStateChange;
      eOn_Leave: TMultiCastStateChange;

      function get_AsString: String;
      function get_Count: Integer;
      function get_InState(const aStateID: TStateID): Boolean;
      function get_StateRef(const aStateID: TStateID): PState;
      procedure set_InState(const aStateID: TStateID;
                            const aValue: Boolean);

    protected
      procedure Lock;
      procedure Unlock;

    public
      constructor Create(const aOwner: TObject;
                         const aStateList: array of TStateID);
      destructor Destroy; override;

      procedure Add(const aState: TStateID;
                    const aCounted: Boolean = FALSE); overload;
      procedure Add(const aStateList: array of TStateID;
                    const aCounted: Boolean = FALSE); overload;
      procedure GetStates(const aStringList: TStrings;
                          const aAllStates: Boolean = FALSE);

      procedure Enter(const aState: TStateID); overload;
      procedure Enter(const aStates: array of TStateID); overload;
      function InAll(const aStates: array of TStateID): Boolean;
      function InAny(const aStates: array of TStateID): Boolean;
      procedure Leave(const aState: TStateID); overload;
      procedure Leave(const aStates: array of TStateID); overload;
      function Supports(const aState: TStateID): Boolean;

      property AsString: String read get_AsString;
      property Count: Integer read get_Count;
      property InState[const aState: TStateID]: Boolean read get_InState write set_InState; default;
      property Owner: TObject read fOwner;
      property Ref[const aState: TStateID]: PState read get_StateRef;

      property On_Change: TMultiCastStateChange read eOn_Change;
      property On_Enter: TMultiCastStateChange read eOn_Enter;
      property On_Leave: TMultiCastStateChange read eOn_Leave;
    end;



implementation

  uses
  { vcl: }
    SysUtils;


{ TStateList ------------------------------------------------------------------------------------- }

  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  constructor TStateList.Create(const aOwner: TObject;
                                   const aStateList: array of TStateID);
  begin
    _CriticalSection := TCriticalSection.Create;

    fOwner := aOwner;

    eOn_Change := TMultiCastStateChange.Create(aOwner);
    eOn_Enter  := TMultiCastStateChange.Create(aOwner);
    eOn_Leave  := TMultiCastStateChange.Create(aOwner);

    Add(aStateList);
   end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  destructor TStateList.Destroy;
  begin
    FreeAndNIL(eOn_Change);
    FreeAndNIL(eOn_Enter);
    FreeAndNIL(eOn_Leave);
    FreeAndNIL(_CriticalSection);

    inherited;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  function TStateList.get_AsString: String;
  var
    i: Integer;
    state: PState;
  begin
    Lock;
    try
      result := '';
      for i := 0 to Pred(Count) do
      begin
        state := @fStates[i];
        if state.Entered then
          result := result + state.ID + ', ';
      end;

      if Length(result) > 0 then
        SetLength(result, Length(result) - 2);

      result := '[' + result + ']';
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  function TStateList.get_Count: Integer;
  begin
    Lock;
    try
      result := Length(fStates);
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  function TStateList.get_InState(const aStateID: TStateID): Boolean;
  begin
    Lock;
    try
      result := Ref[aStateID].Entered;
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  function TStateList.get_StateRef(const aStateID: TStateID): PState;
  var
    i: Integer;
  begin
    Lock;
    try
      for i := Pred(Count) downto 0 do
      begin
        result := @fStates[i];
        if result.ID = aStateID then
          EXIT;
      end;
    finally
      Unlock;
    end;

    raise Exception.CreateFmt('The state ''%s'' is not supported by a %s statemachine',
                              [String(aStateID), Owner.ClassName]);
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.set_InState(const aStateID: TStateID;
                                   const aValue: Boolean);
  var
    state: PState;
    changed: Boolean;
  begin
    Lock;
    try
      state   := Ref[aStateID];
      changed := FALSE;

      case state.Counted of
      FALSE:
        begin
          changed := (state.Entered xor aValue);
          if changed then
            state.Entered := aValue;
        end;

      TRUE:
        begin
          case aValue of
          FALSE:
            begin
              changed := (state.Count = 1);
              if (state.Count > 0) then
                Dec(state.Count);
            end;

          TRUE:
            begin
              Inc(state.Count);
              changed := (state.Count = 1);
            end;
          end;
        end;
      end;
    finally
      Unlock;
    end;

    if changed then
    begin
      if aValue then
        On_Enter.DoEvent(aStateID);

      On_Change.DoEvent(aStateID);

      if NOT aValue then
        On_Leave.DoEvent(aStateID);
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.Add(const aState: TStateID;
                           const aCounted: Boolean);
  var
    i: Integer;
    state: PState;
  begin
    Lock;
    try
      for i := 0 to Pred(Length(fStates)) do
        if (fStates[i].ID = aState) then
          EXIT;

      SetLength(fStates, Length(fStates) + 1);
      state := @fStates[Pred(Length(fStates))];
      state.ID      := aState;
      state.Counted := aCounted;
      state.Count   := 0;
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.Add(const aStateList: array of TStateID;
                           const aCounted: Boolean);
  var
    i: Integer;
  begin
    for i := Low(aStateList) to High(aStateList) do
      Add(aStateList[i], aCounted);
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.Lock;
  begin
    _CriticalSection.Enter;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.Unlock;
  begin
    _CriticalSection.Leave;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.Enter(const aState: TStateID);
  begin
    InState[aState] := TRUE;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.Enter(const aStates: array of TStateID);
  var
    i: Integer;
  begin
    Lock;
    try
      for i := Low(aStates) to High(aStates) do
        InState[aStates[i]] := TRUE;
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  function TStateList.InAll(const aStates: array of TStateID): Boolean;
  var
    i: Integer;
  begin
    result := FALSE;

    Lock;
    try
      for i := Low(aStates) to High(aStates) do
      begin
        result := InState[aStates[i]];
        if NOT result then
          EXIT;
      end;
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  function TStateList.InAny(const aStates: array of TStateID): Boolean;
  var
    i: Integer;
  begin
    Lock;
    try
      for i := Low(aStates) to High(aStates) do
      begin
        result := InState[aStates[i]];
        if result then
          EXIT;
      end;

      result := FALSE;
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.Leave(const aState: TStateID);
  begin
    InState[aState] := FALSE;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.Leave(const aStates: array of TStateID);
  var
    i: Integer;
  begin
    Lock;
    try
      for i := Low(aStates) to High(aStates) do
        InState[aStates[i]] := FALSE;
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TStateList.GetStates(const aStringList: TStrings;
                                 const aAllStates: Boolean);
  var
    i: Integer;
    state: PState;
  begin
    Lock;
    try
      aStringList.Clear;
      for i := 0 to Pred(Count) do
      begin
        state := @fStates[i];

        if aAllStates or state.Entered then
          aStringList.Add(String(state.ID));
      end;
    finally
      Unlock;
    end;
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  function TStateList.Supports(const aState: TStateID): Boolean;
  var
    i: Integer;
  begin
    result := FALSE;
    Lock;
    try
      for i := Pred(Count) downto 0 do
      begin
        result := (PState(@fStates[i]).ID = aState);
        if result then
          EXIT;
      end;

    finally
      Unlock;
    end;
  end;





{ TMultiCastStateChange -------------------------------------------------------------------------- }

  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TMultiCastStateChange.Add(const aHandler: TStateChangeEvent);
  begin
    inherited Add(TMethod(aHandler));
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TMultiCastStateChange.Remove(const aHandler: TStateChangeEvent);
  begin
    inherited Remove(TMethod(aHandler));
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TMultiCastStateChange.Call(const aMethod: TMethod);
  begin
    TStateChangeEvent(aMethod)(Sender, fEventState);
  end;


  {--  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  --}
  procedure TMultiCastStateChange.DoEvent(const aStateID: TStateID);
  begin
    if NOT Enabled then
      EXIT;
      
    fEventState := aStateID;

    if Assigned(Initial) then
      Initial(Sender, aStateID);
    try
      inherited DoEvent;
    finally
      if Assigned(Final) then
        Final(Sender, aStateID);
    end;
  end;






end.

