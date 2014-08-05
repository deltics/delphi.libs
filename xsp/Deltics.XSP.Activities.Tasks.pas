

  unit Deltics.XSP.Activities.Tasks;


interface

  uses
    Forms;


  type
    TXSPActivityTask = class
    private
      fCaption: String;
      fEnabled: Boolean;
      fSeparator: Boolean;
//    fTaskForm: TXSPTaskForm;
      function set_Caption: String;
    procedure set_Enabled(const Value: Boolean);
//      fTaskForm: TXSPTaskForm;
    public
      property Caption: String read set_Caption;
      property Enabled: Boolean read fEnabled write set_Enabled;
      property Separator: Boolean read fSeparator;
//      property TaskForm: TXSPTaskForm read fTaskForm;
    end;


    TXSPActivityTaskForm = class(TCustomForm)
    end;



implementation


{ TXSPActivityTask }

  function TXSPActivityTask.set_Caption: String;
  begin

  end;


  procedure TXSPActivityTask.set_Enabled(const Value: Boolean);
  begin
    fEnabled := Value;
  end;






end.
