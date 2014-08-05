

  unit Deltics.XSP.Menus.Popup;


interface

  uses
    Classes,
    Menus,
    Deltics.XSP.Menus;


  type
    TCustomXSPPopupMenu = class(Menus.TPopupMenu)
    private
      fMenu: array of TXSPMenuItem;
      fMenuWindow: TXSPMenuWindow;
    public
      constructor Create(Owner: TComponent); override;
      procedure Popup(aX: Integer; aY: Integer); override;
    end;


    TXSPPopupMenu = class(TCustomXSPPopupMenu);
    TPopupMenu = TXSPPopupMenu;


implementation

{ TXSPPopupMenu }

  constructor TCustomXSPPopupMenu.Create(Owner: TComponent);
  begin
    inherited;

  end;


  procedure TCustomXSPPopupMenu.Popup(aX, aY: Integer);
  var
    i: Integer;
    idx: Integer;
    menu: PXSPMenuItem;
  begin
    idx := 0;
    SetLength(fMenu, Items.Count);
    for i := 0 to Pred(Items.Count) do
    begin
      if NOT Items[i].Visible then
        CONTINUE;

      menu := @fMenu[idx];
      Inc(idx);

      menu.Source   := Items[i];
      menu.Caption  := Items[i].Caption;
      menu.Enabled  := Items[i].Enabled;

      if Assigned(Images) and (Items[i].ImageIndex <> -1) then
        Images.GetBitmap(Items[i].ImageIndex, menu.Glyph);
    end;
    SetLength(fMenu, idx);

    DoPopup(self);
  end;




end.
