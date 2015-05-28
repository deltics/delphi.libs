
  unit Pascal.Tests;

interface

  uses
    Deltics.Smoketest;


  type
    TTestPascal = class(TTestCase)
      procedure SnippetAsList;
      procedure SnippetAsStream;
      procedure ClassDeclLF;
      procedure ClassDeclCRLF;
    end;



implementation

  uses
    Deltics.Tokeniser,
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary.Pascal;



  const
    SNIP = 'program Snippet;'#13#10
         + 'uses'#13#10
         + '  Classes,'#13#10
         + '  Forms;'#13#10
         + #13#10
         + 'var'#13#10
         + '  s™: String;'#13#10
         + #13#10
         + 'begin'#13#10
         + 'end.';

    CLASs_DECL_LF = 'TInterfacedPersistent = class(TPersistent, IUnknown,'#13#10
                  + '                                           IOn_Destroy)'#13#10
                  + '  // IUnknown'#13#10
                  + '  protected'#13#10
                  + '    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;'#13#10
                  + '    function _AddRef: Integer; stdcall;'#13#10
                  + '    function _Release: Integer; stdcall;'#13#10
                  + #13#10
                  + '  // IOn_Destroy'#13#10
                  + '  private'#13#10
                  + '    fOn_Destroy: IOn_Destroy;'#13#10
                  + '    function get_On_Destroy: IOn_Destroy;'#13#10
                  + '  public'#13#10
                  + '    property On_Destroy: IOn_Destroy read get_On_Destroy implements IOn_Destroy;'#13#10
                  + 'end;'#13#10;

    CLASs_DECL_CRLF = 'TInterfacedPersistent = class(TPersistent, IUnknown,'#13#10
                    + '                                           IOn_Destroy)'#13#10
                    + '  // IUnknown'#13#10
                    + '  protected'#13#10
                    + '    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;'#13#10
                    + '    function _AddRef: Integer; stdcall;'#13#10
                    + '    function _Release: Integer; stdcall;'#13#10
                    + #13#10
                    + '  // IOn_Destroy'#13#10
                    + '  private'#13#10
                    + '    fOn_Destroy: IOn_Destroy;'#13#10
                    + '    function get_On_Destroy: IOn_Destroy;'#13#10
                    + '  public'#13#10
                    + '    property On_Destroy: IOn_Destroy read get_On_Destroy implements IOn_Destroy;'#13#10
                    + 'end;'#13#10;



{ TTestPascal }

  procedure TTestPascal.SnippetAsList;
  var
    list: ITokenList;
    i: Integer;
  begin
    list := TTokenList.Create(SNIP, PascalLanguage, [toConsumeWhitespace]);

    Test('Token count in snippet').Expect(list.Count).Equals(16);

    for i := 0 to Pred(list.Count) do
      Inspect('token')[i].Value(list[i].Text);
  end;


  procedure TTestPascal.SnippetAsStream;
  var
    strm: ITokenStream;
    i: Integer;
  begin
    strm := TTokenStream.Create(SNIP, PascalLanguage, [toConsumeWhitespace]);

    i := 0;
    while NOT strm.EOF do
    begin
      strm.Read;

      Inspect('token')[i].Value(strm.Token.Text);
      Inc(i);
    end;
    Test('Token count in snippet').Expect(i).Equals(16);
  end;


  procedure TTestPascal.ClassDeclLF;
  var
    list: ITokenList;
    i: Integer;
  begin
    list := TTokenList.Create(CLASS_DECL_LF, PascalLanguage, [toConsumeWhitespace]);

    Test.Expect(list.Count).Equals(65);

    for i := 0 to Pred(list.Count) do
      Inspect('token')[i].Value(list[i].Text);
  end;


  procedure TTestPascal.ClassDeclCRLF;
  var
    list: ITokenList;
    i: Integer;
  begin
    list := TTokenList.Create(CLASS_DECL_CRLF, PascalLanguage, [toConsumeWhitespace]);

    Test.Expect(list.Count).Equals(65);

    for i := 0 to Pred(list.Count) do
      Inspect('token')[i].Value(list[i].Text);
  end;





initialization
  Smoketest.Add(TTestPascal);
end.
