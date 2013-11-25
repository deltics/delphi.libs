{
  * X11 (MIT) LICENSE *

  Copyright й 2013 Jolyon Smith

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

  unit Deltics.Smoketest.Console.ResultsPanel;


interface

  uses
  { vcl: }
    Classes,
    Controls,
    ExtCtrls,
    Forms,
    Graphics,
    ImgList,
    Messages,
    Windows,
  { smoketest: }
    Deltics.Smoketest;


  type
    TResultsPanel = class(TScrollbox)
    private
      fImageList: TImageList;
      fLegend: TPaintBox;
      fLegendBoxes: array of TRect;
      fLegendLabels: array of TRect;
      fLegendLabelWidth: Integer;
      fLegendRows: Integer;
      fPanel: TPaintbox;
      fResults: TList;
      fSeries: TStringList;
      fUpdating: Integer;
      procedure Paint(aSender: TObject);
      procedure MeasureLegend;
      procedure PaintLegend(aSender: TObject);
      function get_SeriesLabel(const aIndex: Integer): UnicodeString;
      procedure set_Legend(const aValue: TPaintBox);   protected
      function get_SeriesCount: Integer;
      procedure DoMouseDown(aSender: TObject; aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer);
      function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
      function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure Resize; override;
      procedure Add(const aOutput: IOutput);
      procedure AddMethod(const aName: UnicodeString; const aDataSet: array of Double);
      procedure AddSeries(const aLabel: UnicodeString);
      procedure BeginUpdate;
      procedure Clear;
      procedure EndUpdate;
      procedure Update; reintroduce;  // I don't care about the TWinControl Update method
      property ImageList: TImageList read fImageList write fImageList;
      property Legend: TPaintBox read fLegend write set_Legend;
      property SeriesLabel[const aIndex: Integer]: UnicodeString read get_SeriesLabel;
      property SeriesCount: Integer read get_SeriesCount;

      class function SeriesColor(const aIndex: Integer): TColor;
    end;


implementation

  uses
    Contnrs,
    SysUtils,
  {$ifdef DELPHIXE2_OR_LATER} // Only needed for inlining Brush.GetColor
    UITypes,
  {$endif}
    Deltics.Graphics,
    Deltics.SysUtils;

  const
    ICON_INFO     = 0;
    ICON_PASS     = 1;
    ICON_FAIL     = 2;
    ICON_INSPECT  = 3;
    ICON_ALERT    = 4;

  type
    TResultArray = array of Double;

    TDrawable = class
    private
      fControl: TPaintBox;
      fSelected: Boolean;
      fRect: TRect;
      procedure set_Selected(const aValue: Boolean);
    public
      constructor Create(const aControl: TPaintBox);
      function DoLayout(const aCanvas: TCanvas; const aTop, aMaxWidth: Integer): Integer; virtual; abstract;
      procedure Draw(const aCanvas: TCanvas; const aImageList: TImageList); virtual; abstract;
      property Control: TPaintBox read fControl;
      property Rect: TRect read fRect;
      property Selected: Boolean read fSelected write set_Selected;
    end;

    TTestResult = class(TDrawable)
    protected
      Color: TColor;
      BorderColor: TColor;
      IconColor: Boolean;
      IconIndex: Integer;
      LabelRect: TRect;
      LabelText: WideString;
      Text: WideString;
      TextColor: TColor;
      TextRect: TRect;
      ChildCount: Integer;
      ChildLabels: array of WideString;
      ChildValues: array of WideString;
      ChildLabelRect: array of TRect;
      ChildValueRect: array of TRect;
      MonoSpaced: Boolean;
      constructor Create(const aControl: TPaintBox; const aText: WideString); overload;
      constructor Create(const aControl: TPaintBox; const aLabel, aText: WideString); overload;
      function DoLayout(const aCanvas: TCanvas; const aTop, aMaxWidth: Integer): Integer; override;
      procedure Draw(const aCanvas: TCanvas; const aImageList: TImageList); override;
      procedure AddChild(const aLabel, aValue: WideString);
    end;


    TBenchmark = class(TDrawable)
    protected
      fMethodName: UnicodeString;
      fResults: TResultArray;
      fMaxResult: Double;
      fMethodRect: TRect;
      fChartRect: TRect;
      fBarRects: array of TRect;
      fLabelRects: array of TRect;
      constructor Create(const aControl: TPaintBox; const aMethod: WideString; const aResults: TResultArray);
      function DoLayout(const aCanvas: TCanvas; const aTop, aMaxWidth: Integer): Integer; override;
      procedure Draw(const aCanvas: TCanvas; const aImageList: TImageList); override;
    end;


{ TResultsPanel ---------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TResultsPanel.Create(aOwner: TComponent);
  begin
    inherited Create(aOwner);

    fPanel  := TPaintbox.Create(self);
    fPanel.Parent     := self;
    fPanel.Align      := alTop;
    fPanel.Visible    := TRUE;
    fPanel.OnPaint    := Paint;
    fPanel.Font.Name  := 'Tahoma';

    fPanel.OnMouseDown := DoMouseDown;

    fResults := TObjectList.Create(TRUE);
    fSeries  := TStringList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TResultsPanel.Destroy;
  begin
    FreeAndNIL(fSeries);
    FreeAndNIL(fResults);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.EndUpdate;
  begin
    if fUpdating = 0 then
      EXIT;

    Dec(fUpdating);
    if fUpdating = 0 then
    begin
      Update;
      Invalidate;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TResultsPanel.get_SeriesLabel(const aIndex: Integer): UnicodeString;
  begin
    result := fSeries[aIndex]
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TResultsPanel.get_SeriesCount: Integer;
  begin
    result := fSeries.Count;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.DoMouseDown(aSender: TObject;
                                      aButton: TMouseButton;
                                      aShift: TShiftState;
                                      aX, aY: Integer);
  var
    i: Integer;
    pt: TPoint;
    draw: TDrawable;
  begin
    pt.X := aX;
    pt.Y := aY;

    for i := 0 to Pred(fResults.Count) do
    begin
      draw := TDrawable(fResults[i]);
      draw.Selected := PtInRect(draw.Rect, pt);
    end;

    SetFocus; // This mouse event occurs in the paintbox which cannot accept focus, so we set
              //  focus on ourselves (the scrollbox) so that we will receive any mousewheel
              //  events
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TResultsPanel.DoMouseWheelDown(Shift: TShiftState;
                                          MousePos: TPoint): Boolean;
  begin
    result := TRUE;
    if PtInRect(ClientRect, ScreenToClient(MousePos)) then
      Perform(WM_VSCROLL, 1, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TResultsPanel.DoMouseWheelUp(Shift: TShiftState;
                                        MousePos: TPoint): Boolean;
  begin
    result := TRUE;
    if PtInRect(ClientRect, ScreenToClient(MousePos)) then
      Perform(WM_VSCROLL, 0, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.Update;
  var
    i: Integer;
    top: Integer;
    benchmarks: Boolean;
    align: Integer;
  begin
    if (fUpdating > 0) then
      EXIT;

    benchmarks := (fResults.Count > 0) and (TDrawable(fResults[0]).ClassType = TBenchmark);

    top   := 3;
    align := 0;

    for i := 0 to Pred(fResults.Count) do
    begin
      top := TDrawable(fResults[i]).DoLayout(fPanel.Canvas, top, fPanel.ClientWidth);
      if benchmarks and (TBenchmark(fResults[i]).fMethodRect.Right > align) then
        align := TBenchmark(fResults[i]).fMethodRect.Right;
    end;

    if benchmarks then
      for i := 0 to Pred(fResults.Count) do
        TBenchmark(fResults[i]).DoLayout(fPanel.Canvas, -1, align + 8);

    fPanel.Height := top;

    if (fSeries.Count > 0) and Assigned(Legend) then
    begin
      MeasureLegend;
      Legend.Parent.Height  := (fLegendRows * 20) + 8;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.Add(const aOutput: IOutput);
  var
    i: Integer;
    drawable: TTestResult;
    alert: IAlert;
    info: IInformation;
    inspection: IInspection;
    result: IResult;
  begin
    if NOT Supports(aOutput, IResult, result)
    and NOT Supports(aOutput, IAlert, alert)
    and NOT Supports(aOutput, IInformation, info)
     and NOT Supports(aOutput, IInspection, inspection) then
      EXIT;

    drawable := NIL;
    try
      if Assigned(alert) then
      begin
        drawable := TTestResult.Create(fPanel, alert.Text, '');
        drawable.IconColor    := FALSE;
        drawable.IconIndex    := ICON_ALERT;
        drawable.Color        := clRed;
        drawable.BorderColor  := clBlack;
        drawable.TextColor    := clWhite;
        EXIT;
      end;

      if Assigned(info) then
      begin
        drawable := TTestResult.Create(fPanel, info.Text, '');
        drawable.IconIndex  := ICON_INFO;
        drawable.Color      := RGB(248, 248, 255);
        EXIT;
      end;

      if Assigned(inspection) then
      begin
        drawable := TTestResult.Create(fPanel, inspection.Subject, inspection.Value);
        drawable.IconIndex  := ICON_INSPECT;
        drawable.MonoSpaced := inspection.MonoSpaced;

        for i := 0 to Pred(inspection.ItemCount) do
          drawable.AddChild(inspection.ItemLabels[i], inspection.ItemValues[i]);

        EXIT;
      end;

      // Otherwise it's a result

      drawable := TTestResult.Create(fPanel, result.Description, '');

      if NOT result.Evaluated then
      begin
        drawable.Color  := RGB(255, 192, 0);
        drawable.Text   := 'Expectation has not been evaluated!'#13
                         + 'Ensure that your test statements are complete.';
      end
      else if NOT result.OK then
      begin
        drawable.Color        := RGB(255, 192, 192);
        drawable.BorderColor  := clRed;
        drawable.IconIndex    := ICON_FAIL;

        if result.Expected <> '' then
          drawable.AddChild('Expected', result.Expected);

        if result.Actual <> '' then
          drawable.AddChild('Actual', result.Actual);

        if result.ExpectedToFail then
          drawable.AddChild('OK, expected to fail', '');

        if (result.Explanation <> '') then
          drawable.AddChild(result.Explanation, '');
      end
      else
      begin
        drawable.Color        := RGB(192, 255, 192);
        drawable.BorderColor  := clGreen;
        drawable.IconIndex    := ICON_PASS;
      end;

    finally
      if Assigned(drawable) then
        fResults.Add(drawable);

      Update;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.AddMethod(const aName: UnicodeString;
                                    const aDataSet: array of Double);
  var
    i: Integer;
    benchmark: TBenchmark;
    data: TResultArray;
  begin
    SetLength(data, Length(aDataSet));
    for i := 0 to Pred(Length(data)) do
      data[i] := aDataSet[i];

    benchmark := TBenchmark.Create(fPanel, aName, data);
    fResults.Add(benchmark);

    Update;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.AddSeries(const aLabel: UnicodeString);
  var
    rc: TRect;
  begin
    fSeries.Add(aLabel);

    if NOT Assigned(Legend) then
      EXIT;

    SetLength(fLegendBoxes, fSeries.Count);
    SetLength(fLegendLabels, fSeries.Count);

    Legend.Canvas.Font.Name := 'Tahoma';
    Legend.Canvas.Font.Size := 8;

    rc.Left := 0;
    rc.Top  := 0;
    DrawTextExW(Legend.Canvas.Handle, PWideChar(aLabel), Length(aLabel), rc,
                DT_CALCRECT or DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);

    if rc.Right > fLegendLabelWidth then
      fLegendLabelWidth := rc.Right;

    MeasureLegend;

    Legend.Parent.Visible := TRUE;
    Legend.Parent.Height  := (fLegendRows * 20) + 8;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.BeginUpdate;
  begin
    Inc(fUpdating);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.Clear;
  begin
    fResults.Clear;
    fSeries.Clear;
    fPanel.Invalidate;

    if NOT Assigned(Legend) then
      EXIT;

    Legend.Parent.Visible := FALSE;

    SetLength(fLegendBoxes, 0);
    SetLength(fLegendLabels, 0);

    fLegendLabelWidth := 0;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.MeasureLegend;
  var
    DC: TCanvas;
    cols: Integer;
    rows: Integer;
    w: Integer;
    r, c: Integer;
    s: Integer;
    boxRC: TRect;
    txtRC: TRect;
  begin
    DC := Legend.Canvas;

    DC.Font.Name := 'Tahoma';
    DC.Font.Size := 8;

    cols := ClientWidth div (fLegendLabelWidth + 32); // left margin 8, box 12, gutter 4 right margin 8
    if cols = 0 then
      cols := 1;

    rows := fSeries.Count div cols;
    if (rows * cols) < fSeries.Count then
      Inc(rows);

    w := fLegendLabelWidth + 32;
    for c := 1 to cols do
    begin
      boxRC.Top := 8;
      txtRC.Top := 6;

      for r := 1 to rows do
      begin
        s := ((c - 1) * rows) + (r - 1);
        if (s >= fSeries.Count) then
          BREAK;

        boxRC.Left    := (w * (c - 1)) + 8;
        boxRC.Right   := boxRC.Left + 12;
        boxRC.Bottom  := boxRC.Top + 12;

        txtRC.Left    := boxRC.Right + 4;
        txtRC.Right   := (w * c) - 8;
        txtRC.Bottom  := txtRC.Top + 16;

        if txtRC.Right > (ClientWidth - 8) then
          txtRC.Right := ClientWidth - 8;

        fLegendBoxes[s]   := boxRC;
        fLegendLabels[s]  := txtRC;

        Inc(boxRC.Top, 20);
        Inc(txtRC.Top, 20);
      end;
    end;

    fLegendRows := rows;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.Paint(aSender: TObject);
  var
    i: Integer;
  begin
    for i := 0 to Pred(fResults.Count) do
      TDrawable(fResults[i]).Draw(fPanel.Canvas, fImageList);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.PaintLegend(aSender: TObject);
  var
    DC: TCanvas;
    i: Integer;
  begin
    DC := Legend.Canvas;

    DC.Font.Name := 'Tahoma';
    DC.Font.Size := 8;

    for i := 0 to Pred(fSeries.Count) do
    begin
      DC.Brush.Color := TResultsPanel.SeriesColor(i);
      DC.Brush.Style := bsSolid;
      FillRect(DC.Handle, fLegendBoxes[i], DC.Brush.Handle);

      DC.Brush.Color := clBlack;
      DC.Brush.Style := bsSolid;
      FrameRect(DC.Handle, fLegendBoxes[i], DC.Brush.Handle);

      DC.Brush.Color := clNone;
      DC.Brush.Style := bsClear;

      DrawTextExW(DC.Handle, PWideChar(fSeries[i]), Length(fSeries[i]), fLegendLabels[i],
                  DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.Resize;
  begin
    inherited;
    Update;
  end;



  var
    SERIES_COLOR: array[0..8] of TColor;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TResultsPanel.SeriesColor(const aIndex: Integer): TColor;
  begin
    result := SERIES_COLOR[aIndex mod Length(SERIES_COLOR)];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TResultsPanel.set_Legend(const aValue: TPaintBox);
  begin
    fLegend := aValue;
    fLegend.OnPaint := PaintLegend;
  end;







{ TDrawable -------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TDrawable.Create(const aControl: TPaintBox);
  begin
    inherited Create;

    fControl := aControl;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TDrawable.set_Selected(const aValue: Boolean);
  var
    rc: TRect;
  begin
    if fSelected = aValue then
      EXIT;

    fSelected := aValue;

    rc := Rect;
    rc.Left := 0;
    TranslateRect(rc, 0, Control.Top);
    InvalidateRect(Control.Parent.Handle, @rc, FALSE);
  end;






{ TTestResult ------------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestResult.Create(const aControl: TPaintBox;
                                 const aText: WideString);
  begin
    inherited Create(aControl);

    Text      := aText;
    IconIndex := -1;

    Color       := RGB(255, 255, 192);
    BorderColor := RGB(127, 127, 192);
    IconColor   := TRUE;
    TextColor   := clBlack;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TTestResult.Create(const aControl: TPaintBox;
                                 const aLabel, aText: WideString);
  begin
    Create(aControl, aText);

    LabelText := aLabel;

    if (aText = aLabel) then
      Text := '';

    if (Text <> '') and (LabelText <> '') then
      LabelText := LabelText + ':';
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestResult.AddChild(const aLabel, aValue: WideString);
  begin
    Inc(ChildCount);

    SetLength(ChildLabels, ChildCount);
    SetLength(ChildValues, ChildCount);
    SetLength(ChildLabelRect, ChildCount);
    SetLength(ChildValueRect, ChildCount);

    if (aLabel <> '') and (aValue <> '') then
      ChildValues[ChildCount - 1] := ': ' + aValue
    else
      ChildValues[ChildCount - 1] := aValue;

    ChildLabels[ChildCount - 1] := aLabel;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TTestResult.DoLayout(const aCanvas: TCanvas; const aTop, aMaxWidth: Integer): Integer;
  var
    i: Integer;
    rc: TRect;
    just: Integer;
  begin
    aCanvas.Font.Name := 'Tahoma';

    fRect.Left   := 26;
    fRect.Top    := aTop;
    fRect.Right  := aMaxWidth;
    fRect.Bottom := Rect.Top + 24;

    rc := fRect;
    InflateRect(rc, -1, -1);  // For the border
    InflateRect(rc, -3, -3);  // For a comfortable 'margin'

    LabelRect := rc;
    TextRect  := rc;

    if (LabelText <> '') then
    begin
      aCanvas.Font.Style := [fsBold];
      DrawTextExW(aCanvas.Handle, PWideChar(LabelText), Length(LabelText), LabelRect,
                  DT_CALCRECT or DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);
      TextRect.Left := LabelRect.Right + 4;
    end
    else
      LabelRect.Left := 0;

    if (Text <> '') and (TextRect.Left < (rc.Right - 16)) then
    begin
      if MonoSpaced then
        aCanvas.Font.Name := 'Courier New';

      aCanvas.Font.Style := [];
      if Pos(#13, Text) = 0 then
        DrawTextExW(aCanvas.Handle, PWideChar(Text), Length(Text), TextRect,
                    DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX, NIL)
      else
        DrawTextExW(aCanvas.Handle, PWideChar(Text), Length(Text), TextRect,
                    DT_CALCRECT or DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);
    end
    else
      TextRect.Left := 0;

    fRect.Bottom := Max(Max(LabelRect.Bottom, TextRect.Bottom) + 2, fRect.Top + 22);

    if MonoSpaced then
      aCanvas.Font.Name := 'Tahoma';

    just := fRect.Left + 4;
    for i := 0 to Pred(ChildCount) do
    begin
      if ChildLabels[i] <> '' then
      begin
        ChildLabelRect[i].Left  := fRect.Left + 12;
        ChildLabelRect[i].Top   := fRect.Bottom + 2;

        aCanvas.Font.Style := [fsBold];
        DrawTextExW(aCanvas.Handle, PWideChar(ChildLabels[i]), Length(ChildLabels[i]), ChildLabelRect[i],
                    DT_CALCRECT or DT_NOPREFIX, NIL);

        fRect.Bottom := ChildLabelRect[i].Bottom + 1;
        if ChildLabelRect[i].Right > (fRect.Right - 19) then
        begin
          ChildLabelRect[i].Right := fRect.Right - 19;
          ChildValueRect[i].Left  := 0;
          just  := fRect.Right - 19;
          CONTINUE;
        end;

        if (ChildLabelRect[i].Right > just) then
          just := ChildLabelRect[i].Right;
      end
      else
        ChildLabelRect[i].Left  := 0;
    end;

    if MonoSpaced then
      aCanvas.Font.Name := 'Courier New';

    for i := 0 to Pred(ChildCount) do
    begin
      if (ChildValues[i] <> '') then
      begin
        ChildLabelRect[i].Right := just;

        if just < TextRect.Left + 4 then
          ChildValueRect[i].Left  := fRect.Left + 4
        else
          ChildValueRect[i].Left  := just;

        ChildValueRect[i].Top   := ChildLabelRect[i].Top;

        aCanvas.Font.Style := [];
        DrawTextExW(aCanvas.Handle, PWideChar(ChildValues[i]), Length(ChildValues[i]), ChildValueRect[i],
                    DT_CALCRECT or DT_NOPREFIX, NIL);
        if ChildValueRect[i].Right > (fRect.Right - 3) then
          ChildValueRect[i].Right := fRect.Right - 3;
      end
      else
        ChildValueRect[i].Left := 0;
    end;

    fRect.Bottom := Max(fRect.Bottom + 3, fRect.Top + 24);

    result := fRect.Bottom + 3;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TTestResult.Draw(const aCanvas: TCanvas;
                             const aImageList: TImageList);
  var
    i: Integer;
    rc: TRect;
    iconRC: TRect;
  begin
    aCanvas.Font.Name := 'Tahoma';

    if (BorderColor <> clNone) then
      aCanvas.Brush.Color := BorderColor
    else
      aCanvas.Brush.Color := Color;

    aCanvas.Brush.Style := bsSolid;

    rc := Rect;

    if (IconIndex <> -1) then
    begin
      aCanvas.Pen.Color := aCanvas.Brush.Color;
      aCanvas.Pen.Style := psSolid;
      aCanvas.Pen.Width := 0;

      if (rc.Bottom > rc.Top + 24) then
      begin
        // Frame the icon and text areas, of differing heights
        //  __________________
        // |                  |
        // |___               |
        //     |              |
        //     |______________|
        //
        aCanvas.MoveTo(3, rc.Top);
        aCanvas.LineTo(rc.Right - 1, rc.Top);
        aCanvas.LineTo(rc.Right - 1, rc.Bottom - 1);
        aCanvas.LineTo(rc.Left,  rc.Bottom - 1);
        aCanvas.LineTo(rc.Left,  rc.Top + 24);
        aCanvas.LineTo(3,  rc.Top + 24);
        aCanvas.LineTo(3,  rc.Top);
      end
      else
      begin
        // Frame the icon and text areas, of equal height
        //  __________________
        // |                  |
        // |__________________|
        //
        rc.Left := 3;
        aCanvas.FrameRect(rc);
        rc.Left := Rect.Left;
      end;

      // Draw the separator between icon area and text area
      //  ...................
      // :    |              :
      // :    |              :
      //  ииии

      aCanvas.MoveTo(rc.Left, rc.Top);
      aCanvas.LineTo(rc.Left, rc.Bottom - 1);

      // Draw the icon from the image list
      //  ...................
      // :XXXX:              :
      // :XXXX:              :
      //  иииииииииииииииииии

      if IconColor then
      begin
        iconrc := Rect;
        iconrc.Left   := 4;
        iconrc.Right  := 26;
        iconrc.Top    := iconrc.Top + 1;
        iconrc.Bottom := iconrc.Top + 23;

        if Selected then
          aCanvas.Brush.Color := clHighlight
        else
          aCanvas.Brush.Color := Color;
        aCanvas.Brush.Style := bsSolid;
        aCanvas.FillRect(iconrc);

        if Assigned(aImageList) then
          aImageList.BkColor := aCanvas.Brush.Color;
      end
      else
      begin
        aCanvas.Brush.Color := clNone;
        aCanvas.Brush.Style := bsClear;

        if Assigned(aImageList) then
          aImageList.BkColor := clNone;
      end;

      if Assigned(aImageList) then
        aImageList.Draw(aCanvas, 5, rc.Top + 2, IconIndex, TRUE);
    end
    else
    begin
      // Frame the text area only.  Do not paint the icon area
      //  .....__________________
      // :    |                  |
      // :    |__________________|
      //  ииии
      aCanvas.FrameRect(rc);
    end;

    InflateRect(rc, -1, -1);
    if Selected then
      aCanvas.Brush.Color := clHighlight
    else
      aCanvas.Brush.Color := Color;
    aCanvas.Brush.Style := bsSolid;

    if ChildCount > 0 then
      rc.Bottom := Max(Max(LabelRect.Bottom, TextRect.Bottom) + 3, Rect.Top + 24);

    aCanvas.FillRect(rc);
    if Selected then
      aCanvas.Font.Color := clHighlightText
    else
      aCanvas.Font.Color := TextColor;

    if (LabelRect.Left <> 0) then
    begin
      if Selected then
        aCanvas.Brush.Color := clHighlight
      else
        aCanvas.Brush.Color := Color;

      aCanvas.Font.Style := [fsBold];
      DrawTextExW(aCanvas.Handle, PWideChar(LabelText), Length(LabelText), LabelRect,
                  DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);
    end;

    if (TextRect.Left <> 0) then
    begin
      if MonoSpaced then
        aCanvas.Font.Name := 'Courier New';

      aCanvas.Font.Style := [];
      if Pos(#13, Text) = 0 then
        DrawTextExW(aCanvas.Handle, PWideChar(Text), Length(Text), TextRect,
                    DT_WORDBREAK or DT_NOPREFIX, NIL)
      else
        DrawTextExW(aCanvas.Handle, PWideChar(Text), Length(Text), TextRect,
                    DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);
    end;

    if ChildCount > 0 then
    begin
      rc.Top    := rc.Bottom;
      rc.Bottom := Rect.Bottom - 1;
      if Selected then
      begin
        aCanvas.Brush.Color := TweenRGB(clHighlight, clSilver);
        aCanvas.Font.Color  := clHighlightText;
      end
      else
      begin
        aCanvas.Brush.Color := TweenRGB(Color, clWhite);
        aCanvas.Font.Color  := TextColor;
      end;
      aCanvas.FillRect(rc);
    end;

    if (Rect.Right - Rect.Left) < 8 then
      EXIT;

    for i := 0 to Pred(ChildCount) do
    begin
      if (ChildLabelRect[i].Left <> 0) then
      begin
        if MonoSpaced then
          aCanvas.Font.Name := 'Tahoma';

        aCanvas.Font.Style := [fsBold];
        DrawTextExW(aCanvas.Handle, PWideChar(ChildLabels[i]), Length(ChildLabels[i]), ChildLabelRect[i],
                    DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);
      end;

      if (ChildValueRect[i].Left <> 0) then
      begin
        if MonoSpaced then
          aCanvas.Font.Name  := 'Courier New';

        aCanvas.Font.Style := [];
        DrawTextExW(aCanvas.Handle, PWideChar(ChildValues[i]), Length(ChildValues[i]), ChildValueRect[i],
                    DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);
      end;
    end;
  end;




{ TBenchmark ------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TBenchmark.Create(const aControl: TPaintBox;
                                const aMethod: WideString;
                                const aResults: TResultArray);
  var
    i: Integer;
  begin
    inherited Create(aControl);

    fMethodName := aMethod;
    fResults    := aResults;

    SetLength(fBarRects, Length(fResults));
    SetLength(fLabelRects, Length(fResults));

    fMaxResult := 0;
    for i:= 0 to Pred(Length(fResults)) do
      if fResults[i] > fMaxResult then
        fMaxResult := fResults[i];
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TBenchmark.DoLayout(const aCanvas: TCanvas;
                               const aTop, aMaxWidth: Integer): Integer;
  var
    i: Integer;
    rc: TRect;
  begin
    if aTop <> -1 then
    begin
      aCanvas.Font.Name := 'Tahoma';

      fRect.Left   := 8;
      fRect.Top    := aTop;
      fRect.Right  := aMaxWidth;

      rc := fRect;
      InflateRect(rc, -1, -1);  // For the border
      InflateRect(rc, -3, -3);  // For a comfortable 'margin'

      fChartRect  := rc;
      fMethodRect := rc;

      // Calculate two primary rectangles for the benchmark output, one for the
      //  method name label, the other for the chart:
      //   ___________________________________________________________________
      //  |                      |                                            |
      //  |______________________| ##############                             |
      //  :                      | ##########################                 |
      //  :                      | ####################                       |
      //  :......................|____________________________________________|
      //

      DrawTextExW(aCanvas.Handle, PWideChar(fMethodName), Length(fMethodName), fMethodRect,
                  DT_CALCRECT or DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);
      fChartRect.Left := fMethodRect.Right + 4;

      fChartRect.Bottom := fChartRect.Top + ((Length(fResults) + 1) * 8);
      if fChartRect.Bottom < fMethodRect.Bottom then
        fChartRect.Bottom := fMethodRect.Bottom;

      fRect.Bottom := fChartRect.Bottom;
    end
    else
    begin
      fMethodRect.Right := aMaxWidth;
      fChartRect.Left   := aMaxWidth;
    end;

    // Now calculate the rectangles for each bar in the results chart.  The
    //  longest bar will always be 90% of the available width.
    //   ___________________________________________________________________
    //  |                      |                                            |
    //  |______________________| ###########################                |
    //  :                      | ######################################     |
    //  :                      | #############################              |
    //  :......................|____________________________________________|
    //

    if fMaxResult > 0 then
      for i := 0 to Pred(Length(fResults)) do
      begin
        fBarRects[i].Top    := fRect.Top + ((i + 1) * 8);
        fBarRects[i].Bottom := fBarRects[i].Top + 7;
        fBarRects[i].Left   := fChartRect.Left;

        if (fResults[i] <> 0) then
          fBarRects[i].Right := fChartRect.Left + Trunc((fChartRect.Right - fChartRect.Left) * 0.9 * fResults[i] / fMaxResult)
        else
          fBarRects[i].Right := fBarRects[i].Left;
      end;

    result := fRect.Bottom + 3;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TBenchmark.Draw(const aCanvas: TCanvas; const aImageList: TImageList);
  var
    i: Integer;
  begin
    aCanvas.Font.Name := 'Tahoma';
    aCanvas.Brush.Color := clNone;
    aCanvas.Brush.Style := bsClear;

    DrawTextExW(aCanvas.Handle, PWideChar(fMethodName), Length(fMethodName), fMethodRect,
                DT_WORD_ELLIPSIS or DT_NOPREFIX, NIL);

    aCanvas.Pen.Color := clBlack;
    aCanvas.Pen.Style := psSolid;
    aCanvas.MoveTo(fChartRect.Left, fChartRect.Top);
    aCanvas.LineTo(fChartRect.Left, fChartRect.Bottom);

    for i := 0 to Pred(Length(fResults)) do
    begin
      if fResults[i] = 0 then
        CONTINUE;

      aCanvas.Brush.Color := TResultsPanel.SeriesColor(i);
      aCanvas.Brush.Style := bsSolid;
      FillRect(aCanvas.Handle, fBarRects[i], aCanvas.Brush.Handle);

      aCanvas.Brush.Color := clBlack;
      aCanvas.Brush.Style := bsSolid;
      FrameRect(aCanvas.Handle, fBarRects[i], aCanvas.Brush.Handle);
    end;
  end;



initialization
  SERIES_COLOR[0] := RGB(127, 255, 127);
  SERIES_COLOR[1] := RGB(127, 127, 255);
  SERIES_COLOR[2] := RGB(255, 127, 127);
  SERIES_COLOR[3] := RGB(192, 227, 192);
  SERIES_COLOR[4] := RGB(192, 192, 227);
  SERIES_COLOR[5] := RGB(227, 192, 192);
  SERIES_COLOR[6] := RGB(227, 255, 255);
  SERIES_COLOR[7] := RGB(255, 227, 255);
  SERIES_COLOR[8] := RGB(255, 255, 227);

end.

