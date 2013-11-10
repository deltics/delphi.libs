{
  * X11 (MIT) LICENSE *

  Copyright © 2013 Jolyon Smith

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

{$ifdef deltics_imagelists}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.ImageLists;


interface

  uses
  { vcl: }
    Classes,
    Controls,
    Dialogs,
    Graphics,
    TypInfo;


  type
    TImageType = (
                  itSmallIcon,      // 16x16                SMICON
                  itLargeIcon,      // 32x32                LGICON
                  itExtraLargeIcon, // 48x48                XLICON
                  itSmallTool,      // 23x23                SMTOOL
                  itLargeTool,      // 32x32                LGTOOL
                  itCustom          // user specified
                 );

    TImageState = (
                   isNormal,
                   isDisabled,
                   isHighlighted,
                   isSelected
                  );
    TImageStates = set of TImageState;

    TImageListStateOption = (
                             soImages,   // load images for different states into the same list
                             soLists     // create separate lists for each state
                            );


    IImageList = interface
    ['{4CAB2DDF-3572-48B9-810F-16F1006E11BD}']
      function get_ID: Integer;
      function get_ImageList(const aState: TImageState): TImageList;
      function get_ImageIndex(const aName: String; const aState: TImageState): Integer;

      procedure LoadAnimationResource(var aID: Integer; const aResourceName: String);
      procedure LoadResource(var aID: Integer; const aResourceName: String); overload;
      function LoadResource(const aResourceName: String): Integer; overload;

      property ID: Integer read get_ID;
      property ImageList[const aState: TImageState]: TImageList read get_ImageList;
      property ImageIndex[const aName: String; const aState: TImageState]: Integer read get_ImageIndex;
    end;


    TImageListManager = class
    private
      fLists: TInterfaceList;
//      function get_ImageList(const aType: TFloImageListType): TImageList;
//      function Add(var aIndex: Integer; const aResourceID: String): Integer; overload;

      constructor Create; overload;
      function get_ListInfo(const aList: TImageList): IImageList;
    public
      destructor Destroy; override;
      function CreateList(const aType: TImageType; const aStates: TImageStates = [isNormal]; const aStateOption: TImageListStateOption = soImages): IImageList; overload;
      function CreateList(var aListID: Integer; const aType: TImageType; const aStates: TImageStates = [isNormal]; const aStateOption: TImageListStateOption = soImages): IImageList; overload;
      property ListInfo[const aList: TImageList]: IImageList read get_ListInfo;
//      class procedure CreateMaskedList(const aListID: Integer; const aStates: TImageStates = [isNormal]);
(*
      class procedure GetBitmap(const aType: TFloImageListType;
                                const aIndex: Integer;
                                const aBitmap: TBitmap);
      class function IndexFor(const aObject: IFloBase): Integer;
      constructor Create;
      destructor Destroy; override;
      function Add(const aResourceID: String): Integer; overload;
      property Lists[const aType: TFloImageListType]: TImageList read get_ImageList; default;
*)    end;


  function ImageLists: TImageListManager;


implementation

  uses
  { vcl: }
    Forms,
    SysUtils,
    Windows,
  { deltics: }
    Deltics.Classes,
    Deltics.Finalizer;


  const
    IMG_SIZE: array[TImageType] of Integer = (16, 32, 48, 23, 32, 0);

  var
    _ImageLists: TImageListManager;


  function ImageLists: TImageListManager;
  begin
    if NOT Assigned(_ImageLists) then
      _ImageLists := TImageListManager.Create;

    result := _ImageLists;
  end;


  type
    TImageListBase = class(TCOMInterfacedObject, IImageList)
    private
      fImageType: TImageType;
      fImages: TStringList;
      fStates: TImageStates;
    protected
      constructor Create(const aType: TImageType; const aStates: TImageStates); virtual;
      function AddImage(const aState: TImageState; const aBitmap, aMask: Graphics.TBitmap): Integer; virtual; abstract;
      function CreateNewList: TImageList;
      function IndexForImage(const aName: String): Integer;
      property States: TImageStates read fStates;
    public
      destructor Destroy; override;

    public // IImageList
      function get_ID: Integer;
      function get_ImageList(const aState: TImageState): TImageList; virtual; abstract;
      function get_ImageIndex(const aName: String; const aState: TImageState): Integer; virtual;

      procedure LoadAnimationResource(var aID: Integer; const aResourceName: String);
      procedure LoadResource(var aID: Integer; const aResourceName: String); overload;
      function LoadResource(const aResourceName: String): Integer; overload;
    end;


    TImageListOfImages = class(TImageListBase)
    private
      fImageList: TImageList;
      fStateOffsets: array[TImageState] of Integer;
    protected
      constructor Create(const aType: TImageType; const aStates: TImageStates); override;
      function AddImage(const aState: TImageState; const aBitmap, aMask: Graphics.TBitmap): Integer; override;
    public
      destructor Destroy; override;

    public // IImageList overrides
      function get_ImageIndex(const aName: String; const aState: TImageState): Integer; override;
      function get_ImageList(const aState: TImageState): TImageList; override;
    end;


    TImageListOfLists = class(TImageListBase)
    private
      fImageList: array[TImageState] of TImageList;
    protected
      constructor Create(const aType: TImageType; const aStates: TImageStates); override;
      function AddImage(const aState: TImageState; const aBitmap, aMask: Graphics.TBitmap): Integer; override;
    public
      destructor Destroy; override;
      function get_ImageList(const aState: TImageState): TImageList; override;
    end;




{ TImageListBase }

  constructor TImageListBase.Create(const aType: TImageType;
                                    const aStates: TImageStates);
  begin
    inherited Create;

    fImageType  := aType;
    fStates     := aStates;

    fImages     := TStringList.Create;
    fImages.Sorted  := TRUE;
  end;


  destructor TImageListBase.Destroy;
  begin
    fImages.Free;

    inherited;
  end;


  function TImageListBase.CreateNewList: TImageList;
  begin
    result := TImageList.Create(NIL);

    result.Height := IMG_SIZE[fImageType];
    result.Width  := IMG_SIZE[fImageType];
  end;


  function TImageListBase.IndexForImage(const aName: String): Integer;
  begin
    result := fImages.IndexOf(aName);
    if (result <> -1) then
      result := Integer(fImages.Objects[result]);
  end;


  function TImageListBase.get_ID: Integer;
  var
    unk: IUnknown;
  begin
    unk     := self as IUnknown;
    result  := _ImageLists.fLists.IndexOf(unk);
  end;


  function TImageListBase.get_ImageIndex(const aName: String;
                                         const aState: TImageState): Integer;
  begin
    result := IndexForImage(aName);
  end;


  procedure TImageListBase.LoadAnimationResource(var aID: Integer;
                                                 const aResourceName: String);
  const
    TYPES: array[TImageType] of String = ('SMICON_',
                                          'LGICON_',
                                          'XLICON_',
                                          'SMTOOL_',
                                          'LGTOOL_',
                                          '');

    function LoadMask(const aBitmap: Graphics.TBitmap;
                      const aID: String): Boolean;

      procedure TryLoad(const aID: String);
      begin
        result := (Windows.FindResource(HInstance, PChar(aID), RT_BITMAP) <> 0);
        if result then
          aBitmap.LoadFromResourceName(HInstance, aID);
      end;

    begin
      TryLoad('IDB_' + TYPES[fImageType] + aID + '_MASK');

      if NOT result then
      begin
        OutputDebugString(PChar('Expected mask resource not found: IDB_' + TYPES[fImageType] + aID + '_MASK'));
        TryLoad('IDB_' + aID + '_MASK');
        if result then
          OutputDebugString(PChar('Using alternate: IDB_' + aID + '_MASK'));
      end;
    end;

    function LoadBitmap(const aBitmap: Graphics.TBitmap;
                        const aID: String;
                        const aFrame: Integer): Boolean;

      procedure TryLoad(const aID: String);
      begin
        result := (Windows.FindResource(HInstance, PChar(aID), RT_BITMAP) <> 0);
        if result then
          aBitmap.LoadFromResourceName(HInstance, aID);
      end;

    var
      id: String;
    begin
      id := aID + Format('_%.2d', [aFrame]);
      TryLoad('IDB_' + TYPES[fImageType] + id);

      if NOT result then
      begin
        OutputDebugString(PChar('Expected resource not found: IDB_' + TYPES[fImageType] + id));
        TryLoad('IDB_' + id);
        if result then
          OutputDebugString(PChar('Using alternate: IDB_' + id));
      end;
    end;

  var
    bmp: Graphics.TBitmap;
    mask: Graphics.TBitmap;
    frame: Integer;
    idx: Integer;
  begin
    aID := fImages.IndexOf(aResourceName);
    if (aID <> -1) then
      raise Exception.Create('An image or animation has already been loaded for resource ''' + aResourceName + '''');

    bmp   := Graphics.TBitmap.Create;
    mask  := Graphics.TBitmap.Create;
    try
      LoadMask(mask, aResourceName);

      frame := 1;
      try
        while LoadBitmap(bmp, aResourceName, frame) do
        begin
          idx := AddImage(isNormal, bmp, mask);
//          idx := fImageList.Add(bmp, mask);
          if (frame = 1) then
          begin
            fImages.AddObject(aResourceName, TObject(idx));
            aID := idx;
          end;

          Inc(frame);
        end;

      except
        OutputDebugString(PChar('Error loading animation from resource: ' + aResourceName));
        EXIT;
      end;

    finally
      bmp.Free;
      mask.Free;
    end;
  end;


  procedure TImageListBase.LoadResource(var   aID: Integer;
                                        const aResourceName: String);
  const
    TYPES: array[TImageType] of String = ('SMICON_',
                                          'LGICON_',
                                          'XLICON_',
                                          'SMTOOL_',
                                          'LGTOOL_',
                                          '');

    STATES: array[TImageState] of String = ('',
                                            '_DISABLED',
                                            '_HOT',
                                            '_SELECTED');

    function LoadBitmap(const aBitmap: Graphics.TBitmap;
                        const aID: String;
                        const aState: TImageState;
                        const aMask: Boolean): Boolean;

      procedure TryLoad(const aID: String);
      var
        id: String;
      begin
        if aMask then
          id := aID + '_MASK'
        else
          id := aID;

        result := (Windows.FindResource(HInstance, PChar(id), RT_BITMAP) <> 0);
        if result then
          aBitmap.LoadFromResourceName(HInstance, id);
      end;

    begin
      TryLoad('IDB_' + TYPES[fImageType] + aID + STATES[aState]);

      if NOT result then
      begin
        OutputDebugString(PChar('Expected resource not found: IDB_' + TYPES[fImageType] + aID + STATES[aState]));
        TryLoad('IDB_' + aID + STATES[aState]);
        if NOT result then
        begin
          TryLoad('IDB_' + TYPES[fImageType] + aID);

          if NOT result then
          begin
            TryLoad('IDB_' + aID);

            if result then
              OutputDebugString(PChar('Using alternate: IDB_' + aID))
            else
              OutputDebugString(PChar('No suitable alternative resource found!'));
          end
          else
            OutputDebugString(PChar('Using alternate: IDB_' + TYPES[fImageType] + aID));
        end
        else
          OutputDebugString(PChar('Using alternate: IDB_' + aID + STATES[aState]));
      end;
    end;

  var
    state: TImageState;
    bmp: Graphics.TBitmap;
    mask: Graphics.TBitmap;
    idx: Integer;
  begin
    aID := fImages.IndexOf(aResourceName);
    if (aID <> -1) then
    begin
      aID := Integer(fImages.Objects[aID]);
      EXIT;
    end;

    bmp   := Graphics.TBitmap.Create;
    mask  := Graphics.TBitmap.Create;
    try
      for state := Low(TImageState) to High(TImageState) do
      begin
        if NOT (state in fStates) then
          CONTINUE;

        try
          LoadBitmap(bmp,  aResourceName, state, FALSE);
          LoadBitmap(mask, aResourceName, state, TRUE);

          idx := AddImage(state, bmp, mask);
//          idx := fImageList.Add(bmp, mask);
          if aID = -1 then
          begin
            fImages.AddObject(aResourceName, TObject(idx));
            aID := idx;
          end;

        except

          OutputDebugString(PChar('Error loading image from resource: ' + aResourceName));
          EXIT;
        end;
      end;

    finally
      bmp.Free;
      mask.Free;
    end;
  end;


  function TImageListBase.LoadResource(const aResourceName: String): Integer;
  begin
    LoadResource(result, aResourceName);
  end;





{ TImageListOfImages }

  constructor TImageListOfImages.Create(const aType: TImageType;
                                        const aStates: TImageStates);
  var
    i: Integer;
    s: TImageState;
  begin
    inherited;

    fImageList := CreateNewList;
    i := 0;
    for s := Low(TImageState) to High(TImageState) do
      if (s in fStates) then
      begin
        fStateOffsets[s] := i;
        Inc(i);
      end
      else
        fStateOffsets[s] := 0;
  end;


  destructor TImageListOfImages.Destroy;
  begin
    FreeAndNIL(fImageList);

    inherited;
  end;


  function TImageListOfImages.AddImage(const aState: TImageState;
                                       const aBitmap: Graphics.TBitmap;
                                       const aMask: Graphics.TBitmap): Integer;
  begin
    result := fImageList.Add(aBitmap, aMask);
  end;


  function TImageListOfImages.get_ImageIndex(const aName: String;
                                             const aState: TImageState): Integer;
  begin
    result := inherited IndexForImage(aName);
    if (result <> -1) then
      result := result + fStateOffsets[aState];
  end;


  function TImageListOfImages.get_ImageList(const aState: TImageState): TImageList;
  begin
    result := fImageList;
  end;





{ TImageListOfLists }

  constructor TImageListOfLists.Create(const aType: TImageType;
                                       const aStates: TImageStates);
  var
    s: TImageState;
  begin
    inherited;

    for s := Low(TImageState) to High(TImageState) do
      if (s in States) then
        fImageList[s] := CreateNewList;
  end;


  destructor TImageListOfLists.Destroy;
  var
    s: TImageState;
  begin
    for s := Low(TImageState) to High(TImageState) do
      if (s in States) then
        fImageList[s].Free;

    inherited;
  end;


  function TImageListOfLists.AddImage(const aState: TImageState;
                                      const aBitmap: Graphics.TBitmap;
                                      const aMask: Graphics.TBitmap): Integer;
  begin
    result := fImageList[aState].Add(aBitmap, aMask);
  end;


  function TImageListOfLists.get_ImageList(const aState: TImageState): TImageList;
  begin
    result := fImageList[aState];
  end;










{ TImageListManager ------------------------------------------------------------------------------ }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TImageListManager.Create;
  begin
    inherited Create;

    fLists := TInterfaceList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TImageListManager.CreateList(const aType: TImageType;
                                        const aStates: TImageStates;
                                        const aStateOption: TImageListStateOption): IImageList;
  var
    res: TImageListBase;
  begin
    case aStateOption of
      soImages  : res := TImageListOfImages.Create(aType, aStates);
      soLists   : res := TImageListOfLists.Create(aType, aStates);
    else
      res := NIL;
    end;

    if Assigned(res) then
      fLists.Add(res as IUnknown);

    result := res;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TImageListManager.CreateList(var aListID: Integer;
                                        const aType: TImageType;
                                        const aStates: TImageStates;
                                        const aStateOption: TImageListStateOption): IImageList;
  begin
    result  := CreateList(aType, aStates, aStateOption);
    aListID := result.ID;
  end;


  destructor TImageListManager.Destroy;
  begin
    fLists.Free;
    inherited;
  end;


  function TImageListManager.get_ListInfo(const aList: TImageList): IImageList;
  begin

  end;






procedure DoFinalization;
begin
  _ImageLists.Free;
end;





initialization
  RegisterFinalization(DoFinalization, 'Deltics.ImageLists');

end.
