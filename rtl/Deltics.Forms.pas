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

{$ifdef deltics_forms}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Forms;


interface

  uses
  { vcl: }
    Classes,
    Controls,
    Forms,
    Messages,
    Windows,
  { deltics: }
    Deltics.CommandLine,
    Deltics.Threads,
    Deltics.VersionInfo,
    Deltics.SizeGrip;


  type
    TApplicationHelper = class;
    TForm = class;
    TFormClass = class of TForm;


    TApplicationHelper = class(Forms.TApplication)
    {@@TApplication

      This class is not a true extension of the Forms TApplication in that
       the VCL will only ever create an Application object of type
       Forms.TApplication but by casting the Application object as this
       cast we can add methods, but we cannot add member data.

      Since Application is a singleton however, we can store member data
       in simple unit variables with getter and setter methods maintaining
       the illusion that it is maintained on the Application object itself.
    }
    {$ifNdef DELPHI2007_OR_LATER}
    private
      function get_MainFormOnTaskbar: Boolean;
      procedure set_MainFormOnTaskbar(const aValue: Boolean);
    public
      property MainFormOnTaskBar: Boolean read get_MainFormOnTaskbar write set_MainFormOnTaskbar;
    {$endif DELPHI2007_OR_LATER}
    private
      function get_ActiveFormHandle: HWND;
      function get_CommandLine: TCommandLine;
      function get_SplashClass: TFormClass;
      function get_SplashScreen: TForm;
      function get_Thread: TThread;
      function get_VersionInfo: TVersionInfo;
      procedure set_SplashClass(const aValue: TFormClass);
      procedure Idle(Sender: TObject; var Done: Boolean);
    public
      procedure CreateForm(InstanceClass: TComponentClass; var Reference);
      procedure Run;
      property CommandLine: TCommandLine read get_CommandLine;

      property ActiveFormHandle: HWND read get_ActiveFormHandle;
      property SplashClass: TFormClass read get_SplashClass write set_SplashClass;
      property SplashScreen: TForm read get_SplashScreen;
      property Thread: TThread read get_Thread;
      property VersionInfo: TVersionInfo read get_VersionInfo;
    end;


    TForm = class(Forms.TForm)
    {$ifNdef DELPHI2007_OR_LATER}
    private
      fShowOnTaskBar: Boolean;
      function get_ShowOnTaskBar: Boolean;
      procedure set_ShowOnTaskBar(const aValue: Boolean);
    protected
      procedure CreateParams(var Params: TCreateParams); override;
    public
      property ShowOnTaskBar: Boolean read get_ShowOnTaskBar write set_ShowOnTaskBar;
    {$endif DELPHI2007_OR_LATER}
    private
      fContainer: TWinControl;
      fIsEmbeddable: Boolean;
      fIsEmbedded: Boolean;
      fSizeGrip: TSizeGrip;
      fHasSizeGrip: Boolean;
      procedure set_Container(const aValue: TWinControl);
      procedure set_HasSizeGrip(const aValue: Boolean);
      procedure CreateSizeGrip;
      procedure DoEmbedding(const aEmbed: Boolean);
      procedure WMSysCommand(var aMessage: TWMSysCommand); message WM_SYSCOMMAND;
    protected
      procedure CreateWnd; override;
      procedure DestroyWnd; override;
      procedure Idle; virtual;
    public
      constructor CreateEmbeddable(const aOwner: TComponent);
      constructor CreateEmbedded(const aOwner: TComponent;
                                 const aContainer: TWinControl);
      function ShowModal: TModalResult; reintroduce; overload;
      function ShowModal(const aParent: TForm): TModalResult; reintroduce; overload;
      property Container: TWinControl read fContainer write set_Container;
      property HasSizeGrip: Boolean read fHasSizeGrip write set_HasSizeGrip;
      property IsEmbeddable: Boolean read fIsEmbeddable;
      property IsEmbedded: Boolean read fIsEmbedded;
    end;



  function Application: TApplicationHelper;



implementation

  uses
  { deltics: }
    Deltics.SysUtils;


  var
    _AppThread: TCapturedThread = NIL;
    _CommandLine: TCommandLine = NIL;
    _MainFormOnTaskBar: Boolean = TRUE;
    _SplashScreen: TForm = NIL;
    _VersionInfo: TVersionInfo = NIL;
    _AppDataFiles: TStringList = NIL;
    _UserDataFiles: TStringList = NIL;


  type
    TWinControlHelper = class(TWinControl);


  function Application: TApplicationHelper;
  begin
    result := TApplicationHelper(Forms.Application) ;
  end;


  function GetNonToolWindowPopupParent(WndParent: HWND): HWND;
  begin
    Result := GetParent(WndParent);
    while (Result <> 0) and (GetWindowLong(Result, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) do
      Result := GetParent(WndParent);
    if Result = 0 then
    begin
      if Assigned(Application.MainForm) and
         (GetWindowLong(Application.MainForm.Handle, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0) then
        Result := Application.MainForm.Handle
      else
        Result := Application.Handle;
    end;
  end;


  procedure _CallIdle(const aComponent: TComponent);
  var
    form: TForm absolute aComponent;
  begin
    // No need to call any form that is embeddable but which is not
    //  currently embedded otherwise (not embeddable, or embeddable and
    //  embedded) each form gets a chance to respond to going Idle
    if NOT form.IsEmbeddable or form.IsEmbedded then
      form.Idle;
  end;






{ TApplication }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApplicationHelper.get_ActiveFormHandle: HWND;
  begin
    result := 0;

    if result = 0 then
      result := GetActiveWindow;

    if result = 0 then
      result := GetLastActivePopup(Handle);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApplicationHelper.get_CommandLine: TCommandLine;
  begin
    if NOT Assigned(_CommandLine) then
      _CommandLine := TCommandLine.Create;

    result := _CommandLine;
  end;


{$ifNdef DELPHI2007_OR_LATER}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApplicationHelper.get_MainFormOnTaskbar: Boolean;
  begin
    result := _MainFormOnTaskBar;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TApplicationHelper.set_MainFormOnTaskbar(const aValue: Boolean);
  begin
    if (_MainFormOnTaskBar = aValue) then
      EXIT;

    ASSERT(MainForm = NIL);

    _MainFormOnTaskBar := aValue;
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApplicationHelper.get_SplashClass: TFormClass;
  begin
    if Assigned(_SplashScreen) then
      result := TFormClass(_SplashScreen.ClassType)
    else
      result := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApplicationHelper.get_SplashScreen: TForm;
  begin
    result := _SplashScreen;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApplicationHelper.get_Thread: TThread;
  begin
    result := _AppThread;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TApplicationHelper.get_VersionInfo: TVersionInfo;
  begin
    if NOT Assigned(_VersionInfo) then
      _VersionInfo := TVersionInfo.Create;

    result := _VersionInfo;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TApplicationHelper.set_SplashClass(const aValue: TFormClass);
  begin
    ASSERT(NOT Assigned(_SplashScreen), 'Already showing a splash screen');

    _SplashScreen := aValue.Create(NIL);

    with _SplashScreen do
    begin
      // Remove window frame and border decorations, set form to stay on
      //  top and center on the screen.
      Caption     := '';
      BorderStyle := bsNone;
      BorderIcons := [];
      FormStyle   := fsStayOnTop;
      Position    := poScreenCenter;

      // Display the form and force painting
      Show;
      Update;
    end;

    // The application may not have much initialisation so to avoid
    //  a blink-and-you-miss-it splash, we shall force a pause of
    //  1/2 a second

    Sleep(500);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TApplicationHelper.CreateForm(InstanceClass: TComponentClass;
                                          var Reference);
  var
    bMainForm: Boolean;
    hHidden: THandle;
  begin
    // If CreateForm is called with a NIL MainForm, then when we invoke
    //  inherited we will establish the resulting form AS the MainForm.
    //
    // Establishing the MainForm is our trigger to apply from changes to
    //  the hidden application window *IF* we are to show the MainForm
    //  itself on the TaskBar.
    bMainForm := (Forms.Application.MainForm = NIL);

    inherited;

    // If we aren't showing the MainForm on the TaskBar,
    //  or if we didn't just create the MainForm
    //  or if the MainForm is not derived from Deltics.Forms.TForm
    //
    // then we don't touch the hidden application window, for if we did
    //  our application would not behave correctly on the task bar

    if NOT MainFormOnTaskBar
     or NOT bMainForm
     or NOT (Application.MainForm.InheritsFrom(TForm)) then
      EXIT;

    // Ok, so conditions are right to tweak the hidden window.  Having
    //  done that we also need to recreate the Window of the MainForm
    //
    // This is necessary because when that MainForm initially created
    //  it's Window it had not been set as the Application.MainForm and
    //  so the Window created for it may not have the correct params

    hHidden := Forms.Application.Handle;
    ShowWindow(hHidden, SW_HIDE);
    SetWindowLong(hHidden, GWL_EXSTYLE, GetWindowLong(hHidden, GWL_EXSTYLE)
                                         and NOT WS_EX_APPWINDOW or WS_EX_TOOLWINDOW);
    ShowWindow(hHidden, SW_SHOW);

    TForm(Application.MainForm).RecreateWnd;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TApplicationHelper.Idle(Sender: TObject; var Done: Boolean);
  begin
    ForEachComponent(self, _CallIdle, TRUE, TForm);
    Done := TRUE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TApplicationHelper.Run;
  begin
    // If we're showing a splash screen then we want to ensure that the
    //  main form becomes visible first before the splash screen is
    //  dismissed.  Again, to avoid a blink-and-you-miss-it splash screen
    //  we pause for 1/2 a second before dismissing the splashscreen and
    //  actually allowing the application to run.

    if Assigned(_SplashScreen) then
    begin
      MainForm.Show;
      MainForm.Update;
      Sleep(500);
      FreeAndNIL(_SplashScreen);
    end;

    inherited;
  end;






{ TForm }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TForm.CreateEmbeddable(const aOwner: TComponent);
  begin
    fIsEmbeddable := TRUE;
    inherited Create(aOwner);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TForm.CreateEmbedded(const aOwner: TComponent;
                                   const aContainer: TWinControl);
  begin
    ASSERT(Assigned(aContainer));
    fContainer := aContainer;
    CreateEmbeddable(aOwner);
  end;


{$ifNdef DELPHI2007_OR_LATER}
  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TForm.get_ShowOnTaskBar: Boolean;
  begin
    if (Application.MainForm = self) then
      result := Application.MainFormOnTaskBar
    else
      result := fShowOnTaskBar;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.set_ShowOnTaskBar(const aValue: Boolean);
  begin
    if (Application.MainForm <> self) then
    begin
      fShowOnTaskBar := aValue;
      RecreateWnd;
    end
    else
      Application.MainFormOnTaskBar := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.CreateParams(var Params: TCreateParams);
  begin
    inherited;

    Params.Style := Params.Style and (NOT WS_CHILD) or WS_GROUP or WS_TABSTOP;

    if ShowOnTaskBar then
      Params.ExStyle := Params.ExStyle and (NOT WS_EX_TOOLWINDOW) or WS_EX_APPWINDOW;
  end;
{$endif}


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.set_Container(const aValue: TWinControl);
  begin
    ASSERT(IsEmbeddable);

    if (Container = aValue) then
      EXIT;

    if IsEmbedded then DoEmbedding(FALSE);

    fContainer := aValue;

    if Assigned(Container) then DoEmbedding(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.set_HasSizeGrip(const aValue: Boolean);
  begin
    if (fHasSizeGrip = aValue) then
      EXIT;

    fHasSizeGrip := aValue;

    if HasSizeGrip then CreateSizeGrip else FreeAndNIL(fSizeGrip);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.CreateSizeGrip;
  begin
    if (Handle = 0) then
      EXIT;

    fSizeGrip := TSizeGrip.Create(self);
    fSizeGrip.Parent := self;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.CreateWnd;
  begin
    inherited;

    if HasSizeGrip and NOT IsEmbedded then CreateSizeGrip;
    if IsEmbeddable and Assigned(Container) then DoEmbedding(TRUE);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.DestroyWnd;
  begin
    if IsEmbedded then DoEmbedding(FALSE);
    FreeAndNIL(fSizeGrip);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.DoEmbedding(const aEmbed: Boolean);
  var
    i: Integer;
    currentParent: TWinControl;
    newParent: TWinControl;
    rc: TRect;
  begin
    if (NOT IsEmbeddable) or (IsEmbedded = aEmbed) then
      EXIT;

    currentParent := self;
    newParent     := fContainer;

    if NOT aEmbed then
      Exchange(currentParent, newParent);

    for i := Pred(currentParent.ControlCount) downto 0 do
      currentParent.Controls[i].Parent := newParent;

    fIsEmbedded := aEmbed;

    rc := newParent.ClientRect;
    TWinControlHelper(newParent).AlignControls(NIL, rc);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.Idle;
  begin
    // NO-OP
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TForm.ShowModal: TModalResult;
  var
    oldParentWnd: HWND;
  begin
    oldParentWnd := ParentWindow;
    ParentWindow := Application.ActiveFormHandle;
    try
      if (ParentWindow <> 0) and (IsIconic(ParentWindow)
       or not IsWindowVisible(ParentWindow)
        or not IsWindowEnabled(ParentWindow)) then
        ParentWindow := 0;

      if (ParentWindow <> 0)
       and (GetWindowLong(ParentWindow, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW) then
        ParentWindow := GetNonToolWindowPopupParent(ParentWindow);

      if (ParentWindow <> 0) and (Screen.ActiveForm <> nil)
       and (Screen.ActiveForm.Handle = ParentWindow) then
        ParentWindow := Screen.ActiveForm.Handle
      else if ParentWindow = 0 then
        ParentWindow := Application.Handle;

      result := ShowModal(NIL);
    finally
      ParentWindow := oldParentWnd;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TForm.ShowModal(const aParent: TForm): TModalResult;
  begin
    {
    if (aParent = NIL) then
      PopupParent := Application.MainForm
    else
      PopupParent := aParent;
    }
    result := inherited ShowModal;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TForm.WMSysCommand(var aMessage: TWMSysCommand);
  begin
    if Application.MainFormOnTaskBar then
    begin
      case (aMessage.CmdType and $FFF0) of
        SC_MINIMIZE : begin
                        ShowWindow(Handle, SW_MINIMIZE);
                        aMessage.Result := 0;
                      end;

        SC_RESTORE  : begin
                        ShowWindow(Handle, SW_RESTORE);
                        aMessage.Result := 0;
                      end;
      else
        inherited;
      end;
    end
    else
      inherited;
  end;









initialization
  _AppThread := TCapturedThread.Capture(GetCurrentThread);
  Application.OnIdle := Application.Idle;

finalization
  FreeAndNIL([Addr(_AppThread),
              Addr(_CommandLine),
              Addr(_VersionInfo)]);
end.
