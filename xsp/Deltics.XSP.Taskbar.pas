

  unit Deltics.XSP.Taskbar;


interface

  uses
    Classes,
    Controls,
    Windows;


  type
    TXSPTaskbar = class;
    TXSPTaskbarSettings = class;


    TXSPCustomTaskbar = class(TCustomControl)
    private
      fFormWndProc: TFNWndProc;
      fSettings: TXSPTaskbarSettings;
    protected
    end;


    TXSPTaskbar = class(TXSPCustomTaskbar)
    end;


    TXSPTaskbarSettings = class(TPersistent)
    private
    end;




implementation

  uses
    Forms,
    Messages;




end.
