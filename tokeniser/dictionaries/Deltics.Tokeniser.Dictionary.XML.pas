
  unit Deltics.Tokeniser.Dictionary.XML;

interface

  uses
    Deltics.Tokeniser.Consts,
    Deltics.Tokeniser.Dictionary;


  var
    XMLDictionary: TDtxDictionary = NIL;


  const
    // Symbolic Constants for Token Types - these may be used in constant expressions
    ttNode   =  #2;

    // Symbolic Constants for Token IDs - these may be used in constant expressions
    tkComment                = 110;
    tkProcessingInstruction  = 120;
    tkCDATA                  = 130;
    tkStartTag               = 140;
    tkEmptyTag               = 150;
    tkEndTag                 = 160;



implementation

  type
    TXMLDictionary = class(TDtxDictionary)
      procedure Initialise; override;
    end;



  procedure TXMLDictionary.Initialise;
  begin
    SetCaseSensitivity(TRUE);
    SetName('XML');

    TokenType := ttWhitespace;
    AddASCII([tkSpace, tkTab]);
    AddString(tkEOL, '[CR]',    #13);
    AddString(tkEOL, '[LF]',    #10);
    AddString(tkEOL, '[CRLF]',  #13#10);

    TokenType := ttNode;
    AddDelimited(tkCommentNode,           'Comment',                '<!--',     '-->');
    AddDelimited(tkProcessingInstruction, 'Processing Instruction', '<?',       '?>');
    AddDelimited(tkCDATA,                 'CDATA',                  '<[CDATA[', ']]>');
    AddDelimited(tkStartTag,              'Start Tag',              '<',        '>');
    AddDelimited(tkEmptyTag,              'Empty Tag',              '<',        '/>');
    AddDelimited(tkEndTag,                'End Tag',                '</',       '>');
  end;


initialization
  XMLDictionary := TXMLDictionary.Create;

finalization
  XMLDictionary.Free;
end.
