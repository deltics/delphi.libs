

  unit Deltics.XSP.Activities;


interface

  uses
  { vcl: }
    Forms,
  { xsp: }
    Deltics.XSP.Activities.Tasks;


  type
    TXSPActivityHost = class(TCustomForm)
    end;


    TXSPActivityMenu = class(TCustomForm)
    end;


    TXSPActivity = class(TForm)
    private
//      fTasks: TXSPActivityTasks;
      procedure InitTasks;
    protected
      procedure DoInitTasks; virtual;
    public
      constructor Create; reintroduce;
      destructor Destroy; override;
//      property Tasks: TXSPActivityTasks read fTasks;
    end;


implementation


{ TXSPActivity }

  constructor TXSPActivity.Create;
  begin

  end;

  destructor TXSPActivity.Destroy;
  begin

    inherited;
  end;

  procedure TXSPActivity.DoInitTasks;
  begin

  end;

  procedure TXSPActivity.InitTasks;
  begin

  end;



end.
