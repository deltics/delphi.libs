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

{$i deltics.smoketest.inc}

{$ifdef deltics_smoketest_console}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Smoketest.Console;


interface

  uses
  { vcl: }
    ActnList,
    Buttons,
    Classes,
    ComCtrls,
    Contnrs,
    Controls,
    Dialogs,
    ExtCtrls,
    Forms,
    Graphics,
    ImgList,
    Menus,
    Messages,
    StdCtrls,
    SyncObjs,
    SysUtils,
    ToolWin,
    Variants,
    Windows,
  { deltics: }
    Deltics.Classes,
    Deltics.Forms,
    Deltics.MultiCast,
    Deltics.StateList,
    Deltics.Threads,
    Deltics.ImageLists,
    Deltics.Smoketest,
    Deltics.Smoketest.Console.ResultsPanel;


  const
    STM_UpdateItem = WM_USER + 1;
    STM_AutoRun    = WM_USER + 2;

  type
    TWndProc = procedure(var aMessage: TMessage) of object;

    THierarchyAction = (
                        haExpandItem,
                        haExpandSiblings,
                        haExpandAll,
                        haCollapseItem,
                        haCollapseSiblings,
                        haCollapseAll
                       );
    THierarchyActions = set of THierarchyAction;


  type
    TSmoketestConsole = class(TForm, IOn_Destroy)
      StatusBar: TStatusBar;
      Actions: TActionList;
      actRunSelected: TAction;
      lvHierarchy: TListView;
      Splitter: TSplitter;
      actAbortRun: TAction;
      ResultImages: TImageList;
      actEditParameters: TAction;
      ToolBar: TToolBar;
      tbStart: TToolButton;
      tbStop: TToolButton;
      ProgressBar: TProgressBar;
      ResultsPopup: TPopupMenu;
      miDeleteRun: TMenuItem;
      miRenameRun: TMenuItem;
      PopupMenu: TPopupMenu;
      miEnabled: TMenuItem;
      miExpandArticle: TMenuItem;
      miCollapseArticle: TMenuItem;
      miSeparator: TMenuItem;
      miRunTest: TMenuItem;
      miExpandAll: TMenuItem;
      miCollapseAll: TMenuItem;
      miExpandSiblings: TMenuItem;
      miCollapseSiblings: TMenuItem;
      procedure FormCreate(Sender: TObject);
      procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
      procedure actRunSelectedExecute(Sender: TObject);
      procedure FormResize(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure actAbortRunExecute(Sender: TObject);
      procedure actRunSelectedUpdate(Sender: TObject);
      procedure actAbortRunUpdate(Sender: TObject);
      procedure lvHierarchyClick(Sender: TObject);
      procedure SuiteItemChecked(Sender: TObject; Item: TListItem);
      procedure lvHierarchyCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
        var DefaultDraw: Boolean);
      procedure PopupMenuPopup(Sender: TObject);
      procedure miEnabledClick(Sender: TObject);
      procedure miExpandArticleClick(Sender: TObject);
      procedure miCollapseArticleClick(Sender: TObject);
      procedure lvHierarchyDblClick(Sender: TObject);
      procedure miCollapseChildrenClick(Sender: TObject);
      procedure miExpandChildrenClick(Sender: TObject);
      procedure miCollapseAllClick(Sender: TObject);
      procedure miExpandAllClick(Sender: TObject);
      procedure miExpandSiblingsClick(Sender: TObject);
      procedure miCollapseSiblingsClick(Sender: TObject);
      procedure TestRunTabsResize(Sender: TObject);
    private
      fCollapsedArticles: TStringList;
      fOn_Destroy: IOn_Destroy;
      fSmoketest: ISmoketestRuntime;
      function get_SelectedArticle: ITestArticle;
      function get_SelectedListItem: TListItem;
      procedure set_SelectedArticle(const aValue: ITestArticle);
      procedure GetHierarchyActions(var aActions: THierarchyActions; var aDefault: THierarchyAction);
      function ArticleItem(const aArticle: ITestArticle): TListItem; overload;
      function IsCollapsed(const aArticle: ITestArticle): Boolean; overload;
      function IsExpandable(const aArticle: ITestArticle): Boolean; overload;
      function IsExpanded(const aArticle: ITestArticle): Boolean; overload;
      function ItemArticle(const aListItem: TListItem): ITestArticle;
      procedure ExpandCollapseAll(const aItem: TListItem; const aExpand: Boolean);
      procedure ExpandCollapseItem(const aItem: TListItem; const aExpand: Boolean);
      procedure ExpandCollapseChildren(const aItem: TListItem; const aExpand: Boolean);
      procedure ExpandCollapseSiblings(const aItem: TListItem; const aExpand: Boolean);
      procedure SetupCanvasForArticle(const aArticle: ITestArticle);
    private
      fListIcons: IImageList;
      fToolIcons: IImageList;
      fCSI: TCriticalSection;
      fResults: TResultsPanel;
      fRunningItems: TThreadList;
      fUpdatingItems: Boolean;
    {$ifNdef DELPHI2009_OR_LATER}
      fLVWndProc: TWndProc;
      procedure LVWndProcHook(var aMessage: TMessage);
    {$endif}
      procedure Add(const aParent: TListItem;
                    const aObject: ITestArticle);
      procedure AddChildren(const aItem: TListItem);
      procedure RemoveChildren(const aItem: TListItem);
      procedure Animate(const aFrame: Word);
      function CloneRunningItemsList: TList;
      procedure AlignProgressBar;
      function ItemImageIndex(const aObject: ITestArticle): Integer; overload;
//      procedure OnProgress(Sender: TObject);
      procedure LoadResources;
      procedure OnArticleChanged(Sender: TObject);
      procedure OnSuiteFinished(Sender: TObject);
      procedure OnSuiteStarted(Sender: TObject);
      procedure ResetItems;
      procedure UpdateItem(const aItem: TListItem; const aUpdateChildrenIfExpanded: Boolean = FALSE);
      procedure UpdateStatusBar;
      procedure UpdateResults;
      procedure STMAutoRun(var Msg: TMessage); message STM_AutoRun;
    public
      destructor Destroy; override;
      procedure Initialize;
      property On_Destroy: IOn_Destroy read fOn_Destroy implements IOn_Destroy;
      property SelectedArticle: ITestArticle read get_SelectedArticle write set_SelectedArticle;
      property SelectedListItem: TListItem read get_SelectedListItem;
      property Smoketest: ISmoketestRuntime read fSmoketest;
    end;

  var
    Console: TSmoketestConsole;

implementation

  uses
    CommCtrl,
    Types,
    Deltics.Finalizer,
    Deltics.Graphics,
    Deltics.MessageHandler,
//    Deltics.Progress,
    Deltics.RTTI,
    Deltics.SysUtils,
    Deltics.Threads.Worker,
    Deltics.Oscillator,
    Deltics.Windows;


{$R *.dfm}
{$R smoketest.res}

  type
    TSmoketestHelper = class(TSmoketest);


  const
    COL_PerSec  = 0;
    COL_Runtime = 1;

  var
    IMGLIST_LISTVIEW  : Integer;
//    IMGLIST_RESULTS   : Integer;

    IMG_NONE            : Integer = -1;
    IMG_ROOT            : Integer = -1;
    IMG_TestCase        : Integer = -1;
    IMG_PerformanceCase : Integer = -1;
    IMG_Test            : Integer = -1;
    IMG_Benchmark       : Integer = -1;
    IMG_Running         : Integer = -1;
    IMG_Failed          : Integer = -1;
    IMG_Passed          : Integer = -1;
    IMG_Aborted         : Integer = -1;
    IMG_PassedAndFailed : Integer = -1;
    IMG_NotImplemented  : Integer = -1;

    IMG_PerformanceCaseDisabled : Integer = -1;
    IMG_BenchmarkDisabled       : Integer = -1;
    IMG_TestCaseDisabled        : Integer = -1;
    IMG_TestDisabled            : Integer = -1;


  var
    FPS: TOscillator;



  procedure TSmoketestConsole.FormCreate(Sender: TObject);

    procedure Collapse(const aArticle: ITestArticle);
    var
      i: Integer;
    begin
      for i := 0 to Pred(aArticle.Count) do
      begin
        fCollapsedArticles.Add(aArticle[i].Reference);
        if (aArticle[i].ArticleType = atTestCase) then
          Collapse(aArticle[i]);
      end;
    end;

  begin
    DoubleBuffered := True;

    FPS := TOscillator.CreateSuspended(12, Animate);

    fCSI          := TCriticalSection.Create;
    fRunningItems := TThreadList.Create;
    LoadResources;

  {$ifdef DELPHI2009_OR_LATER}
    lvHierarchy.OnItemChecked := SuiteItemChecked;
    lvHierarchy.OnSelectItem  := NIL;
  {$else}
    fLVWndProc := lvHierarchy.WindowProc;
    lvHierarchy.WindowProc := LVWndProcHook;
  {$endif}

    fCollapsedArticles  := TStringList.Create;
    fSmoketest  := Deltics.Smoketest.Smoketest as ISmoketestRuntime;

    fOn_Destroy := TOnDestroy.Create(self);

    ProgressBar.Parent := StatusBar;
    ProgressBar.Smooth := TRUE;

//    TProgress.AddListener(OnProgress);

    Smoketest.On_Started.Add(OnSuiteStarted);
    Smoketest.On_Finished.Add(OnSuiteFinished);
    Smoketest.On_Update.Add(OnArticleChanged);

    Collapse(Smoketest);

    fResults := TResultsPanel.Create(self);
    fResults.Align      := alClient;
    fResults.ImageList  := ResultImages;
    fResults.Parent     := self;
    fResults.Visible    := TRUE;

    if Smoketest.CommandLine.OutputFilename = '' then
      StatusBar.Panels[2].Text := 'No results file will be saved'
    else
      StatusBar.Panels[2].Text := Smoketest.CommandLine.OutputFilename;
  end;


  procedure TSmoketestConsole.FormShow(Sender: TObject);
  begin
    lvHierarchy.Columns[0].Caption := Smoketest.Name;

    AddChildren(NIL);
  end;



  destructor TSmoketestConsole.Destroy;
  begin
    FreeAndNIL(fCSI);
    FreeAndNIL(fRunningItems);
    FreeAndNIL(fCollapsedArticles);
    inherited;
  end;


{$ifNdef DELPHI2009_OR_LATER}
  procedure TSmoketestConsole.LVWndProcHook(var aMessage: TMessage) ;
  var
    notify: PNMListView;
    oldState: Cardinal;
    newState: Cardinal;
  begin
    if (aMessage.Msg = CN_NOTIFY)
     and (PNMHdr(aMessage.LParam).Code = LVN_ITEMCHANGED) then
    begin
      notify := PNMListView(aMessage.LParam);

      oldState := (notify.uOldState and LVIS_STATEIMAGEMASK) shr 12;
      newState := (notify.uNewState and LVIS_STATEIMAGEMASK) shr 12;

      if ((notify.uChanged and LVIF_STATE) <> 0)
       and (newState <> oldState)
       and NOT fUpdatingItems then
        SuiteItemChecked(lvHierarchy, lvHierarchy.Items[notify.iItem]);
    end;

    //original ListView message handling
    fLVWndProc(aMessage) ;
  end;
{$endif}


  procedure TSmoketestConsole.Add(const aParent: TListItem;
                                  const aObject: ITestArticle);
  var
    i: Integer;
    item: TListItem;
    indent: Integer;
    obj: TObject;
  begin
    item := NIL;

    // If we found the parent item the now try to insert this item at the
    //  "tail" of the list of children for that item (if any)

    if Assigned(aParent) then
    begin
      indent := aParent.Indent + 1;

      for i := aParent.Index + 1 to Pred(lvHierarchy.Items.Count) do
      begin
        if lvHierarchy.Items[i].Indent <= aParent.Indent then
        begin
          item := lvHierarchy.Items.Insert(i);
          BREAK;
        end;
      end;
    end
    else
      indent := 0;

    // If all else fails, just add the item to the end

    if NOT Assigned(item) then
      item := lvHierarchy.Items.Add;

    item.Caption    := aObject.Reference + ' - ' + aObject.DisplayName;
    item.SubItems.Add('');
    item.SubItems.Add('');

    aObject.AsObject(obj);
    item.Data       := obj;
    item.Indent     := indent;
    item.ImageIndex := ItemImageIndex(aObject);
    item.Checked    := aObject.Enabled;

    UpdateItem(item);

    if NOT fCollapsedArticles.Contains(aObject.Reference) then
      AddChildren(item);
  end;


  procedure TSmoketestConsole.AddChildren(const aItem: TListItem);
  var
    i: Integer;
    article: ITestArticle;
  begin
    if Assigned(aItem) then
      article := ItemArticle(aItem)
    else
      article := Smoketest;

    fCSI.Enter;
    try
      for i := 0 to Pred(article.Count) do
        Add(aItem, article[i]);

    finally
      fCSI.Leave;
    end;
  end;


  procedure TSmoketestConsole.RemoveChildren(const aItem: TListItem);
  var
    idx: Integer;
    article: ITestArticle;
    item: TListItem;
  begin
    idx := aItem.Index + 1;
    while (idx < lvHierarchy.Items.Count) do
      if (lvHierarchy.Items[idx].Indent <= aItem.Indent) then
        BREAK
      else
        Inc(idx);

    Dec(idx);
    if lvHierarchy.Items[idx] = aItem then
      EXIT;

    fCSI.Enter;
    try
      while (idx > aItem.Index) do
      begin
        item    := lvHierarchy.Items[idx];
        article := ItemArticle(item);

        fRunningItems.LockList.Remove(item);
        fRunningItems.UnlockList;

        lvHierarchy.Items.Delete(idx);
        Dec(idx);
      end;

    finally
      fCSI.Leave;
    end;
  end;


  procedure TSmoketestConsole.ExpandCollapseAll(const aItem: TListItem;
                                                const aExpand: Boolean);
  var
    i: Integer;
    test: ITestCase;
  begin
    if NOT Assigned(aItem) then
      EXIT;

    test := ItemArticle(aItem) as ITestCase;

    if aExpand then
      ExpandCollapseItem(aItem, aExpand);

    for i := Pred(test.CaseCount) downto 0 do
      ExpandCollapseAll(ArticleItem(test.CaseByIndex[i]), aExpand);

    if NOT aExpand then
      ExpandCollapseItem(aItem, aExpand);
  end;


  procedure TSmoketestConsole.ExpandCollapseItem(const aItem: TListItem;
                                                 const aExpand: Boolean);
  var
    article: ITestArticle;
  begin
    article := ItemArticle(aItem);

    if aExpand then
    begin
      if fCollapsedArticles.Contains(article.Reference) then
      begin
        AddChildren(aItem);
        fCollapsedArticles.Remove(article.Reference);
      end;
    end
    else
    begin
      if NOT fCollapsedArticles.Contains(article.Reference) then
      begin
        RemoveChildren(aItem);
        fCollapsedArticles.Add(article.Reference);
      end;
    end;
  end;


  procedure TSmoketestConsole.ExpandCollapseChildren(const aItem: TListItem;
                                                     const aExpand: Boolean);
  var
    i: Integer;
  begin
    i := aItem.Index + 1;
    while (i < lvHierarchy.Items.Count) do
    begin
      if (lvHierarchy.Items[i].Indent = aItem.Indent + 1) then
        ExpandCollapseItem(lvHierarchy.Items[i], aExpand)
      else if (lvHierarchy.Items[i].Indent <= aItem.Indent) then
        BREAK;

      Inc(i);
    end;
  end;


  procedure TSmoketestConsole.ExpandCollapseSiblings(const aItem: TListItem;
                                                     const aExpand: Boolean);
  var
    i: Integer;
    item: TListItem;
  begin
    if aItem.Indent > 0 then
    begin
      for i := (aItem.Index - 1) downto 0 do
      begin
        item := lvHierarchy.Items[i];
        if item.Indent = aItem.Indent - 1 then
        begin
          ExpandCollapseChildren(item, aExpand);
          EXIT;
        end;
      end;
    end
    else
    begin
      for i := Pred(lvHierarchy.Items.Count) downto 0 do
      begin
        item := lvHierarchy.Items[i];
        if item.Indent = 0 then
          ExpandCollapseItem(item, aExpand);
      end;
    end;
  end;


  function TSmoketestConsole.ArticleItem(const aArticle: ITestArticle): TListItem;
  var
    i: Integer;
  begin
    for i := 0 to Pred(lvHierarchy.Items.Count) do
    begin
      result := lvHierarchy.Items[i];
      if (ItemArticle(result).ID = aArticle.ID) then
        EXIT;
    end;

    result := NIL;
  end;



  procedure TSmoketestConsole.GetHierarchyActions(var aActions: THierarchyActions;
                                                  var aDefault: THierarchyAction);

    procedure GetDescendantStates(const aTest: ITestCase;
                                  var aAnyExpanded: Boolean;
                                  var aAnyCollapsed: Boolean);
    var
      i: Integer;
      child: ITestCase;
      collapsed: Boolean;
    begin
      for i := 0 to Pred(aTest.CaseCount) do
      begin
        child := aTest.CaseByIndex[i];
        if (child.Count = 0) then
          CONTINUE;

        collapsed     := IsCollapsed(child);
        aAnyCollapsed := aAnyCollapsed or collapsed;
        aAnyExpanded  := aAnyExpanded or NOT collapsed;

        if NOT aAnyExpanded or NOT aAnyCollapsed then
          GetDescendantStates(child, aAnyExpanded, aAnyCollapsed);

        if aAnyExpanded and aAnyCollapsed then
          BREAK;
      end;
    end;


    function KeyIsDown(const aKey: Integer): Boolean;
    begin
      result := ((GetKeyState(aKey) and $8000) = $8000);
    end;

  var
    i: Integer;
    article: ITestArticle;
    parent: ITestArticle;
    sibling: ITestArticle;
    test: ITestCase;
    ctrlKey: Boolean;
    shiftKey: Boolean;
    collapsed: Boolean;
    expanded: Boolean;
    hasExpandedSiblings: Boolean;
    hasCollapsedSiblings: Boolean;
    hasExpandedDescendants: Boolean;
    hasCollapsedDescendants: Boolean;
  begin
    aActions  := [];
    aDefault  := haExpandItem;

    article := SelectedArticle;
    if NOT Assigned(article) then
      EXIT;

    shiftKey  := KeyIsDown(VK_SHIFT);
    ctrlKey   := KeyIsDown(VK_CONTROL);

    hasCollapsedSiblings    := FALSE;
    hasExpandedSiblings     := FALSE;
    hasCollapsedDescendants := FALSE;
    hasExpandedDescendants  := FALSE;

    if Supports(article, ITestCase, test) then
      GetDescendantStates(test, hasExpandedDescendants, hasCollapsedDescendants);

    parent := article.Parent;
    for i := 0 to Pred(parent.Count) do
    begin
      sibling := parent.Child[i];
      if (sibling.Reference = article.Reference)
       or NOT (sibling.ArticleType in [atTestCase, atPerformanceCase]) then
        CONTINUE;

      hasCollapsedSiblings  := hasCollapsedSiblings or IsExpandable(sibling);
      hasExpandedSiblings   := hasExpandedSiblings or IsExpanded(sibling);
    end;

    collapsed := IsCollapsed(article);
    expanded  := IsExpanded(article);

    if collapsed then Include(aActions, haExpandItem);
    if expanded then Include(aActions, haCollapseItem);

    if hasCollapsedSiblings then Include(aActions, haExpandSiblings);
    if hasExpandedSiblings then Include(aActions, haCollapseSiblings);

    if Assigned(test) then
    begin
      if (collapsed and test.HasChildCases) or hasCollapsedDescendants then Include(aActions, haExpandAll);
      if expanded and hasExpandedDescendants then Include(aActions, haCollapseAll);
    end;

    if (haExpandItem in aActions) then aDefault := haExpandItem;
    if (haCollapseItem in aActions) then aDefault := haCollapseItem;

    if ctrlKey and NOT shiftKey and (haExpandSiblings in aActions) then aDefault := haExpandSiblings;
    if ctrlKey and NOT shiftKey and (haCollapseSiblings in aActions) then aDefault := haCollapseSiblings;

    if Assigned(test) then
    begin
      if shiftKey and NOT ctrlKey and (haExpandAll in aActions) then aDefault := haExpandAll;
      if shiftKey and NOT ctrlKey and (haCollapseAll in aActions) then aDefault := haCollapseAll;
    end;
  end;


  procedure TSmoketestConsole.OnSuiteFinished(Sender: TObject);
  var
    i: Integer;
    list: TList;
  begin
    list := CloneRunningItemsList;
    try
      for i := 0 to Pred(list.Count) do
        UpdateItem(TListItem(list[i]));

      list.Clear;

    finally
      list.Free;
    end;

    FPS.Stop;

    UpdateStatusBar;
    Screen.Cursor := crDefault;
  end;


  procedure TSmoketestConsole.OnSuiteStarted(Sender: TObject);
  begin
    FPS.Start;

    Screen.Cursor := crAppStart;

    AlignProgressBar;

    ProgressBar.Min       := 0;
    ProgressBar.Max       := 100;
    ProgressBar.Position  := 0;

    UpdateStatusBar;
  end;


  procedure TSmoketestConsole.OnArticleChanged(Sender: TObject);
  var
    article: ITestArticle;
    item: TListItem;
    list: TList;
  begin
    UpdateStatusBar;

    Sender.GetInterface(ITestArticle, article);

    if fCSI.TryEnter then
    try
      item := ArticleItem(article);

      if NOT Assigned(item) then
        EXIT;

      UpdateItem(item);

    finally
      fCSI.Leave;
    end
    else
      EXIT;

    list := fRunningItems.LockList;
    try
      if article.IsRunning then
      begin
        if (list.IndexOf(item) = -1) then
          list.Add(item);
      end
      else
        list.Remove(item);

    finally
      fRunningItems.UnlockList;
    end;
  end;


  function TSmoketestConsole.ItemImageIndex(const aObject: ITestArticle): Integer;
  var
    test: ITestMethod;
  begin
    result := IMG_NONE;

    if (aObject.ArticleType = atSmoketest) then
      result := IMG_ROOT
    else if aObject.IsRunning then
      result := IMG_Running + FPS.Phase
    else if (aObject.ArticleType = atPerformanceCase) then
      result := IMG_PerformanceCase
    else if (aObject.ArticleType = atTestCase) then
      result := IMG_TestCase
    else if (aObject.ArticleType = atPerformanceDelegate) then
      result := IMG_Benchmark
    else if (aObject.ArticleType = atTestDelegate) then
    begin
      aObject.QueryInterface(ITestMethod, test);

      if test.NotImplemented then
        result := IMG_NotImplemented
      else if test.HasErrors then
        result := IMG_Failed
      else if test.Aborted then
        result := IMG_Aborted
      else if (test.HasFailures and test.HasPasses) then
        result := IMG_PassedAndFailed
      else if test.HasFailures then
        result := IMG_Failed
      else if test.HasPasses then
        result := IMG_Passed
      else
        result := IMG_Test
    end;

    if NOT aObject.EffectivelyEnabled then
      if (result = IMG_Test) then                 result := IMG_TestDisabled
      else if (result = IMG_TestCase) then        result := IMG_TestCaseDisabled
      else if (result = IMG_Benchmark) then       result := IMG_BenchmarkDisabled
      else if (result = IMG_PerformanceCase) then result := IMG_PerformanceCaseDisabled;
  end;


  procedure TSmoketestConsole.LoadResources;
  begin
    fListIcons := ImageLists.CreateList(IMGLIST_LISTVIEW, itSmallIcon, [isNormal, isDisabled]);

    fListIcons.LoadResource(IMG_ROOT,            'EAGLES');
    fListIcons.LoadResource(IMG_PerformanceCase, 'PERFORMANCECASE');
    fListIcons.LoadResource(IMG_Benchmark,       'PERFORMANCEMETHOD');
    fListIcons.LoadResource(IMG_Test,            'TESTMETHOD');
    fListIcons.LoadResource(IMG_TestCase,        'TESTCASE');
    fListIcons.LoadResource(IMG_NotImplemented,  'NOTIMPLEMENTED');
    fListIcons.LoadResource(IMG_Aborted,         'ABORT');
    fListIcons.LoadResource(IMG_Failed,          'FAIL');
    fListIcons.LoadResource(IMG_Passed,          'PASS');
    fListIcons.LoadResource(IMG_PassedAndFailed, 'MIXEDRESULTS');
    fListIcons.LoadAnimationResource(IMG_Running, 'RUNNING');

    IMG_PerformanceCaseDisabled := fListIcons.ImageIndex['PERFORMANCECASE',    isDisabled];
    IMG_BenchmarkDisabled       := fListIcons.ImageIndex['PERFORMANCEMETHOD',  isDisabled];
    IMG_TestCaseDisabled        := fListIcons.ImageIndex['TESTCASE',           isDisabled];
    IMG_TestDisabled            := fListIcons.ImageIndex['TESTMETHOD',         isDisabled];

    fToolIcons := ImageLists.CreateList(itSmallTool, [isNormal, isHighlighted, isDisabled], soLists);
    ToolBar.Images          := fToolIcons.ImageList[isNormal];
    ToolBar.HotImages       := fToolIcons.ImageList[isHighlighted];
    ToolBar.DisabledImages  := fToolIcons.ImageList[isDisabled];

    tbStart.ImageIndex  := fToolIcons.LoadResource('PLAY');
    tbStop.ImageIndex   := fToolIcons.LoadResource('STOP');


    lvHierarchy.SmallImages := fListIcons.ImageList[isNormal];
  end;


  procedure TSmoketestConsole.FormCloseQuery(Sender: TObject;
                                             var CanClose: Boolean);
  begin
    CanClose := NOT Smoketest.Thread.State[Deltics.Threads.tsRunning];

    if CanClose then
      lvHierarchy.Items.Clear;

    FreeAndNIL(FPS);
  end;



  procedure TSmoketestConsole.actRunSelectedExecute(Sender: TObject);
  begin
    ResetItems;
    Smoketest.Run;
  end;



  function TSmoketestConsole.CloneRunningItemsList: TList;
  begin
    result := TList.Create;
    CloneList(fRunningItems.LockList, result);
    fRunningItems.UnlockList;
  end;


  procedure TSmoketestConsole.AlignProgressBar;
  begin
    StatusBar.Panels[2].Width := StatusBar.ClientWidth - (StatusBar.Panels[0].Width + StatusBar.Panels[1].Width) - 100;

    ProgressBar.SetBounds(StatusBar.Panels[0].Width + StatusBar.Panels[1].Width + 2,
                          StatusBar.ClientRect.Top + 2,
                          StatusBar.Panels[2].Width - 2,
                          StatusBar.ClientHeight - 2);
  end;



  procedure TSmoketestConsole.FormResize(Sender: TObject);
  begin
    AlignProgressBar;
  end;



(*
  procedure TSmoketestConsole.OnProgress(Sender: TObject);
  var
    proc: IProgressInfo;
  begin
    proc := Progress.Overall[Smoketest.Thread.ID];
    if Assigned(proc) then
      ProgressBar.Position := proc.Percent
    else
      ProgressBar.Position := 100;
  end;
*)


  procedure TSmoketestConsole.ResetItems;
  var
    i: Integer;
    item: TListItem;
  begin
    for i := 0 to Pred(lvHierarchy.Items.Count) do
    begin
      item := lvHierarchy.Items[i];
      item.SubItems[COL_PerSec]   := '';
      item.SubItems[COL_Runtime]  := '';
    end;
  end;


  procedure TSmoketestConsole.UpdateItem(const aItem: TListItem;
                                         const aUpdateChildrenIfExpanded: Boolean);
  var
    i: Integer;
    article: ITestArticle;
    perfCase: IPerformanceCase;
    perf: IPerformanceMethod;
    newCaption: String;
    forcePaint: Boolean;
  begin
    if NOT Assigned(aItem) then
      EXIT;

    forcePaint := FALSE;

    article := ItemArticle(aItem);
    article.QueryInterface(IPerformanceCase, perfCase);
    article.QueryInterface(IPerformanceMethod, perf);

    if Assigned(article.Parent) and (article.Parent.EffectivelyEnabled) then
      aItem.Checked := article.Enabled
    else
      aItem.Checked := article.EffectivelyEnabled;

    aItem.ImageIndex  := ItemImageIndex(article);

    if article.EffectivelyEnabled then
    begin
      if Assigned(perf) then
      begin
        aItem.SubItems[COL_PerSec]   := Format('%.0f', [perf.ExecutionsPerSecond]);
        aItem.SubItems[COL_Runtime]  := Format('%.5f', [perf.AverageRuntime]);
      end
      else if Assigned(perfCase) then
      begin
        if perfCase.Sampling > 0 then
          newCaption := perfCase.DisplayName + Format(' [%d / %d]', [perfCase.Sampling, perfCase.Samples])
        else if perfCase.Samples > 1 then
          newCaption := perfCase.DisplayName + Format(' [%d]', [perfCase.Samples])
        else
          newCaption := perfCase.DisplayName;

        if aItem.Caption <> newCaption then
          aItem.Caption := newCaption
      end;
    end
    else
    begin
      aItem.SubItems[COL_PerSec]   := '';
      aItem.SubItems[COL_Runtime]  := '';
    end;

    if forcePaint then
      lvHierarchy.Update;

    if aUpdateChildrenIfExpanded and IsExpanded(article) then
      for i := 0 to Pred(article.Count) do
        UpdateItem(ArticleItem(article.Child[i]), TRUE);
  end;


  procedure TSmoketestConsole.UpdateStatusBar;
  begin
    ProgressBar.Visible := Smoketest.Thread.State[Deltics.Threads.tsRunning];

    if Smoketest.Thread.State[Deltics.Threads.tsRunning] then
    begin
      StatusBar.Panels[0].Text := 'Running...';
      StatusBar.Panels[1].Text := ''; // TODO: Display name of active case(s)
    end
    else
    begin
      StatusBar.Panels[0].Text := 'Ready';
      StatusBar.Panels[1].Text := '';
    end;
  end;




  procedure TSmoketestConsole.actAbortRunExecute(Sender: TObject);
  begin
    Smoketest.Abort;
  end;



  procedure TSmoketestConsole.actRunSelectedUpdate(Sender: TObject);
  begin
    actRunSelected.Enabled := NOT Smoketest.Thread.State[Deltics.Threads.tsRunning];
  end;



  procedure TSmoketestConsole.actAbortRunUpdate(Sender: TObject);
  begin
    actAbortRun.Enabled := Smoketest.Thread.State[Deltics.Threads.tsRunning];
  end;


  procedure TSmoketestConsole.lvHierarchyClick(Sender: TObject);
  begin
    if Assigned(SelectedListItem) then
      SelectedArticle := ItemArticle(SelectedListItem)
    else
      SelectedArticle := NIL;

    UpdateResults;
  end;


  procedure TSmoketestConsole.lvHierarchyDblClick(Sender: TObject);

    function KeyIsDown(const aKey: Integer): Boolean;
    begin
      result := ((GetKeyState(aKey) and $8000) = $8000);
    end;

  var
    available: THierarchyActions;
    action: THierarchyAction;
    expand: Boolean;
  begin
    fCSI.Enter;
    try
      GetHierarchyActions(available, action);
      if NOT (action in available) then
        EXIT;

      expand := action in [haExpandItem..haExpandAll];

      if action in [haExpandItem, haCollapseItem] then
        ExpandCollapseItem(SelectedListItem, expand)
      else if action in [haExpandSiblings, haCollapseSiblings] then
        ExpandCollapseSiblings(SelectedListItem, expand)
      else if action in [haExpandAll, haCollapseAll] then
        ExpandCollapseAll(SelectedListItem, expand);

    finally
      fCSI.Leave;
    end;
  end;


  procedure TSmoketestConsole.SetupCanvasForArticle(const aArticle: ITestArticle);
  const
    TEXTCOLOR : array[FALSE..TRUE] of TColor = (clSilver, clWindowText);
  begin
    if (aArticle.ArticleType in [atPerformanceCase, atTestCase])
     and (aArticle.Count = 0) then
      lvHierarchy.Canvas.Font.Color := clRed
    else
      lvHierarchy.Canvas.Font.Color := TEXTCOLOR[aArticle.EffectivelyEnabled];

    if aArticle.IsRunning then
      lvHierarchy.Canvas.Brush.Color := RGB(240, 255, 240)
//    else if NOT aArticle.EffectivelyEnabled then
//      lvHierarchy.Canvas.Brush.Color := RGB(245, 245, 250)
    else if aArticle.HasErrors then
      lvHierarchy.Canvas.Brush.Color := RGB(255, 200, 200)
    else if aArticle.HasFailures then
      lvHierarchy.Canvas.Brush.Color := RGB(255, 240, 240);
  end;


  procedure TSmoketestConsole.lvHierarchyCustomDrawItem(    Sender: TCustomListView;
                                                            Item: TListItem;
                                                            State: TCustomDrawState;
                                                        var DefaultDraw: Boolean);
  begin
    SetupCanvasForArticle(ItemArticle(Item));
    DefaultDraw := TRUE;
  end;


  procedure TSmoketestConsole.SuiteItemChecked(Sender: TObject; Item: TListItem);
  var
    article: ITestArticle;
  begin
    if fUpdatingItems then
      EXIT;

    if NOT Assigned(Item.Data) then
      EXIT;

    article := ItemArticle(Item);

    fUpdatingItems := TRUE;
    try
      article.Enabled := Item.Checked;
      UpdateItem(Item, TRUE);

    finally
      fUpdatingItems := FALSE;
    end;

    // TODO: If Shift pressed then apply recursively to children

    lvHierarchy.Invalidate;
  end;


  procedure TSmoketestConsole.TestRunTabsResize(Sender: TObject);
  begin
    if Assigned(fResults) then
      fResults.Update;
  end;


  function TSmoketestConsole.get_SelectedArticle: ITestArticle;
  begin
    result := ItemArticle(SelectedListItem);
  end;


  function TSmoketestConsole.get_SelectedListItem: TListItem;
  begin
    result := lvHierarchy.Selected;
  end;


  procedure TSmoketestConsole.set_SelectedArticle(const aValue: ITestArticle);
  var
    i: Integer;
    obj: TObject;
  begin
    if (SelectedArticle = aValue) then
      EXIT;

    obj := (aValue as IAsObject).AsObject;

    for i := 0 to Pred(lvHierarchy.Items.Count) do
      if (lvHierarchy.Items[i].Data = obj) then
      begin
        lvHierarchy.Selected := lvHierarchy.Items[i];
        BREAK;
      end;

    UpdateResults;
  end;


  procedure TSmoketestConsole.UpdateResults;
  var
    i: Integer;
    run: TTestRun;
    output: IOutput;
  begin
    fResults.Clear;

    if NOT Assigned(SelectedArticle) then
      EXIT;

    run := Smoketest.TestRun;
    if NOT Assigned(run) then
      EXIT;

    fResults.BeginUpdate;
    try
      for i := 0 to Pred(run.Output.Count) do
      begin
        output := run.Output[i];
        if (output.Article.Reference = SelectedArticle.Reference) then
          fResults.Add(output);
      end;

    finally
      fResults.EndUpdate;
    end;
  end;


  procedure TSmoketestConsole.Initialize;
  begin
    if Smoketest.CommandLine.AutoRun then
      PostMessage(Handle, STM_AutoRun, 0, 0);
  end;



  function TSmoketestConsole.IsCollapsed(const aArticle: ITestArticle): Boolean;
  begin
    result := (aArticle.Count = 0) or fCollapsedArticles.Contains(aArticle.Reference);
  end;


  function TSmoketestConsole.IsExpandable(const aArticle: ITestArticle): Boolean;
  begin
    result := (aArticle.Count > 0) and fCollapsedArticles.Contains(aArticle.Reference);
  end;


  function TSmoketestConsole.IsExpanded(const aArticle: ITestArticle): Boolean;
  begin
    result := (aArticle.Count > 0) and NOT fCollapsedArticles.Contains(aArticle.Reference);
  end;


  function TSmoketestConsole.ItemArticle(const aListItem: TListItem): ITestArticle;
  begin
    if Assigned(aListItem) then
      TObject(aListItem.Data).GetInterface(ITestArticle, result)
    else
      result := NIL;
  end;


  procedure TSmoketestConsole.Animate(const aFrame: Word);
  var
    i: Integer;
    item: TListItem;
    list: TList;
    rc: TRect;
  begin
    fCSI.Enter;
    try
      list := CloneRunningItemsList;
      try
        for i := 0 to Pred(list.Count) do
        begin
          item := TListItem(list[i]);
          item.ImageIndex := IMG_Running + aFrame;

          ListView_GetItemRect(item.ListView.Handle, item.Index, rc, LVIR_ICON);
          InvalidateRect(item.ListView.Handle, @rc, TRUE);
        end;

      finally
        list.Free;
      end;

    finally
      fCSI.Leave;
    end;
  end;


  procedure TSmoketestConsole.STMAutoRun(var Msg: TMessage);
  begin
    actRunSelected.Execute;
  end;


  procedure TSmoketestConsole.PopupMenuPopup(Sender: TObject);
  var
    actions: THierarchyActions;
    defaultAction: THierarchyAction;

    procedure UpdateItem(const aItem: TMenuItem;
                         const aAction: THierarchyAction);
    begin
      aItem.Visible := aAction in actions;
      aItem.Default := defaultAction = aAction;
    end;

  var
    article: ITestArticle;
  begin
    GetHierarchyActions(actions, defaultAction);

    UpdateItem(miExpandArticle,     haExpandItem);
    UpdateItem(miExpandAll,         haExpandAll);
    UpdateItem(miExpandSiblings,    haExpandSiblings);

    UpdateItem(miCollapseArticle,   haCollapseItem);
    UpdateItem(miCollapseAll,       haCollapseAll);
    UpdateItem(miCollapseSiblings,  haCollapseSiblings);

    article := SelectedArticle;

    miEnabled.Visible := Assigned(article);
    miEnabled.Checked := Assigned(article) and article.Enabled;
  end;


  procedure TSmoketestConsole.miEnabledClick(Sender: TObject);
  begin
    // TODO: Hold down "shift" to enable/disable all parents

    SelectedArticle.Enabled := NOT SelectedArticle.Enabled;
    lvHierarchy.Invalidate;
  end;


  procedure TSmoketestConsole.miExpandAllClick(Sender: TObject);
  begin
    ExpandCollapseAll(SelectedListItem, TRUE);
  end;


  procedure TSmoketestConsole.miExpandArticleClick(Sender: TObject);
  begin
    ExpandCollapseItem(SelectedListItem, TRUE);
  end;


  procedure TSmoketestConsole.miCollapseAllClick(Sender: TObject);
  begin
    ExpandCollapseAll(SelectedListItem, FALSE);
  end;


  procedure TSmoketestConsole.miCollapseArticleClick(Sender: TObject);
  begin
    ExpandCollapseItem(SelectedListItem, FALSE);
  end;



  procedure TSmoketestConsole.miCollapseSiblingsClick(Sender: TObject);
  begin
    ExpandCollapseSiblings(SelectedListItem, FALSE);
  end;


  procedure TSmoketestConsole.miExpandChildrenClick(Sender: TObject);
  begin
    ExpandCollapseChildren(SelectedListItem, TRUE);
  end;



  procedure TSmoketestConsole.miExpandSiblingsClick(Sender: TObject);
  begin
    ExpandCollapseSiblings(SelectedListItem, TRUE);
  end;


  procedure TSmoketestConsole.miCollapseChildrenClick(Sender: TObject);
  begin
    ExpandCollapseChildren(SelectedListItem, FALSE);
  end;





end.
