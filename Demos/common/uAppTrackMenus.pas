unit uAppTrackMenus;

// kobik :
// From : https://stackoverflow.com/questions/5983217/how-to-select-a-menu-item-without-closing-the-menu
// Thank You !!

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Contnrs, Menus;

type
  TTrackMenuNotifyEvent = procedure(Sender: TMenu; Item: TMenuItem; var CanClose: Boolean) of object;

  TPopupMenu = class(Menus.TPopupMenu)
  private
    FTrackMenu: Boolean;
    FOnTrackMenuNotify: TTrackMenuNotifyEvent;
  public
    procedure Popup(X, Y: Integer); override;
    property TrackMenu: Boolean read FTrackMenu write FTrackMenu;
    property OnTrackMenuNotify: TTrackMenuNotifyEvent read FOnTrackMenuNotify write FOnTrackMenuNotify;
  end;

  TMainMenu = class(Menus.TMainMenu)
  private
    FTrackMenu: Boolean;
    FOnTrackMenuNotify: TTrackMenuNotifyEvent;
  public
    property TrackMenu: Boolean read FTrackMenu write FTrackMenu;
    property OnTrackMenuNotify: TTrackMenuNotifyEvent read FOnTrackMenuNotify write FOnTrackMenuNotify;
  end;

procedure FormMainMenuWndProcMessage(var Msg: TMessage; AForm: TCustomForm);

implementation

const
  { Undocumented Menu Messages }
  MN_SETHMENU                 = $01E0;
  MN_GETHMENU                 = $01E1;
  MN_SIZEWINDOW               = $01E2;
  MN_OPENHIERARCHY            = $01E3;
  MN_CLOSEHIERARCHY           = $01E4;
  MN_SELECTITEM               = $01E5;
  MN_CANCELMENUS              = $01E6;
  MN_SELECTFIRSTVALIDITEM     = $01E7;
  MN_GETPPOPUPMENU            = $01EA;
  MN_FINDMENUWINDOWFROMPOINT  = $01EB;
  MN_SHOWPOPUPWINDOW          = $01EC;
  MN_BUTTONDOWN               = $01ED;
  MN_MOUSEMOVE                = $01EE;
  MN_BUTTONUP                 = $01EF;
  MN_SETTIMERTOOPENHIERARCHY  = $01F0;
  MN_DBLCLK                   = $01F1;

var
  ActiveHookMenu: TMenu = nil;

type
  TPopupWndList = class;

  TPopupWnd = class
  private
    FHandle: THandle;
    FMenuHandle: HMENU;
    FOrgPopupWindowProc, FHookedPopupWindowProc: Pointer;
    FSelectedItemPos: Integer;
    FSelectedItemID: UINT;
    FHooked: Boolean;
    FPopupWndList: TPopupWndList;
    function GetHMenu: HMENU;
    procedure PopupWindowProc(var Msg: TMessage);
    procedure Hook;
    procedure UnHook;
    procedure MenuSelectPos(Menu: TMenu; ItemPos: UINT; out CanClose: Boolean);
    procedure MenuSelectID(Menu: TMenu; ItemID: UINT; out CanClose: Boolean);
  public
    property Handle: THandle read FHandle write FHandle;
    property MenuHandle: HMENU read FMenuHandle;
    constructor Create(APopupWndList: TPopupWndList; AHandle: THandle); overload;
    destructor Destroy; override;
  end;

  TPopupWndList = class(TObjectList)
  public
    function FindHookedPopupHWnd(MenuWindow: HWND): TPopupWnd;
    function FindHookedPopupHMenu(Menu: HMENU): TPopupWnd;
  end;

{ TPopupWnd }
constructor TPopupWnd.Create(APopupWndList: TPopupWndList; AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
  FMenuHandle := GetHMenu;
  FPopupWndList := APopupWndList;
  Hook;
end;

destructor TPopupWnd.Destroy;
begin
  if FHooked then // JIC: normally UnHook is called in PopupWindowProc WM_DESTROY
    UnHook;
  inherited;
end;

procedure TPopupWnd.Hook;
begin
  FOrgPopupWindowProc := Pointer(GetWindowLong(FHandle, GWL_WNDPROC));
  FHookedPopupWindowProc := MakeObjectInstance(PopupWindowProc);
  SetWindowLong(FHandle, GWL_WNDPROC, Longint(FHookedPopupWindowProc));
  FHooked := True;
end;

procedure TPopupWnd.UnHook;
begin
  SetWindowLong(FHandle, GWL_WNDPROC, Longint(FOrgPopupWindowProc));
  FreeObjectInstance(FHookedPopupWindowProc);
  FHooked := False;
end;

procedure TPopupWnd.PopupWindowProc(var Msg: TMessage);
var
  NormalItem: Boolean;
begin
  case Msg.Msg of
    MN_SELECTITEM:
      begin
        // -1 ($FFFF) => mouse is outside the menu window
        FSelectedItemPos := Integer(Msg.wParam); // HiWord(Msg.wParam)
      end;
    MN_DBLCLK:
      begin
        Exit; // eat
      end;
    MN_BUTTONDOWN:
      begin
        MenuSelectPos(ActiveHookMenu, UINT(Msg.WParamLo), NormalItem);
        if not NormalItem then
          Exit;
      end;
    WM_KEYDOWN:
      if (Msg.WParam = VK_RETURN) and (FSelectedItemPos <> -1) and (FSelectedItemID <> 0) then begin
        MenuSelectID(ActiveHookMenu, FSelectedItemID, NormalItem);
        if not NormalItem then
          Exit;
      end;
    WM_DESTROY:
      begin
        UnHook;
      end;
  end;
  Msg.Result := CallWindowProc(FOrgPopupWindowProc, FHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TPopupWnd.MenuSelectPos(Menu: TMenu; ItemPos: UINT; out CanClose: Boolean);
begin
  MenuSelectID(Menu, GetMenuItemID(GetHMenu, ItemPos), CanClose);
end;

function GetMenuItemPos(Menu: HMENU; ItemID: UINT): Integer;
var
  I: Integer;
  MenuItemInfo: TMenuItemInfo;
begin
  Result := -1;
  if IsMenu(Menu) then
    for I := 0 to GetMenuItemCount(Menu) do
    begin
      FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
      MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
      MenuItemInfo.fMask := MIIM_ID;
      if (GetMenuItemInfo(Menu, I, True, MenuItemInfo)) then
        if MenuItemInfo.wID = ItemID then
        begin
          Result := I;
          Exit;
        end;
    end;
end;

procedure TPopupWnd.MenuSelectID(Menu: TMenu; ItemID: UINT; out CanClose: Boolean);
var
  Item: TMenuItem;
  NotifyEvent: TTrackMenuNotifyEvent;
  R: TRect;
begin
  CanClose := True;
  Item := Menu.FindItem(ItemID, fkCommand);
  if Assigned(Item) then
  begin
    NotifyEvent := nil;
    if Menu is TPopupMenu then
      NotifyEvent := TPopupMenu(Menu).FOnTrackMenuNotify
    else
    if Menu is TMainMenu then
      NotifyEvent := TMainMenu(Menu).FOnTrackMenuNotify;

    if Assigned(NotifyEvent) then
      NotifyEvent(Menu, Item, CanClose);

    if not CanClose then
    begin
      Item.Click;
      if GetMenuItemRect(FHandle, FMenuHandle, GetMenuItemPos(FMenuHandle, ItemID), R) then
      begin
        MapWindowPoints(0, FHandle, R, 2);
        InvalidateRect(FHandle, @R, False);
      end else
        InvalidateRect(FHandle, nil, False);
    end;
  end;
end;

function TPopupWnd.GetHMenu: HMENU;
begin
  Result := SendMessage(FHandle, MN_GETHMENU, 0, 0);
end;

{ TPopupWndList }
function TPopupWndList.FindHookedPopupHWnd(MenuWindow: HWND): TPopupWnd;
var
  I: Integer;
  PopupWnd: TPopupWnd;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    PopupWnd := TPopupWnd(Items[I]);
    if (PopupWnd.FHooked) and (PopupWnd.Handle = MenuWindow) then
    begin
      Result := PopupWnd;
      Exit;
    end;
  end;
end;

function TPopupWndList.FindHookedPopupHMenu(Menu: HMENU): TPopupWnd;
var
  I: Integer;
  PopupWnd: TPopupWnd;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    PopupWnd := TPopupWnd(Items[I]);
    if (PopupWnd.FHooked) and (PopupWnd.MenuHandle{GetHMenu} = Menu) then
    begin
      Result := PopupWnd;
      Exit;
    end;
  end;
end;

var
  PopupWndList: TPopupWndList = nil;
  MenuCallWndHook: HHOOK = 0;
  SelectedItemID: UINT = 0;
  NeedPopupWindowHandle: Boolean = False;
  InitMenuPopupCount: Integer = 0;

function CallWndHookProc(nCode: Integer; wParam: WPARAM; Msg: PCWPStruct): LRESULT; stdcall;
var
  Menu: HMENU;
  MenuWnd: HWND;
  PopupWnd: TPopupWnd;
begin
  if (nCode = HC_ACTION) then
  begin
    case Msg.message of
      WM_INITMENUPOPUP:
        begin // TWMInitMenuPopup
          Inc(InitMenuPopupCount);
          NeedPopupWindowHandle := True;
          SelectedItemID := 0;
          if PopupWndList = nil then
          begin
            PopupWndList := TPopupWndList.Create(True); // OwnsObjects
          end;
        end;
      WM_UNINITMENUPOPUP:
        begin
          Dec(InitMenuPopupCount);
        end;
      WM_ENTERIDLE:
        begin
          if (Msg.wParam = MSGF_MENU) and NeedPopupWindowHandle then
          begin
            NeedPopupWindowHandle := False;
            MenuWnd := HWND(Msg.lParam);
            if Assigned(PopupWndList) and (PopupWndList.FindHookedPopupHWnd(MenuWnd) = nil) then
              PopupWndList.Add(TPopupWnd.Create(PopupWndList, MenuWnd));
          end;
        end;
      WM_MENUSELECT:
        begin
          // MSDN: If the high-order word of wParam contains 0xFFFF and the lParam parameter contains NULL, the system has closed the menu.
          if (Msg.lParam = 0) and (HiWord(Msg.wParam) = $FFFF) then // Menu Closed
          begin
            FreeAndNil(PopupWndList);
          end
          else
          begin
            Menu := HMENU(Msg.lParam);
            if HiWord(Msg.wParam) and MF_POPUP <> 0 then // fkHandle
              SelectedItemID := GetSubMenu(Menu, LoWord(Msg.WParam))
            else // fkCommand
              SelectedItemID := LoWord(Msg.wParam); // TWMMenuSelect(Msg).IDItem;
            if Assigned(PopupWndList) then
            begin
              PopupWnd := PopupWndList.FindHookedPopupHMenu(Menu);
              if Assigned(PopupWnd) then
              begin
                PopupWnd.FSelectedItemID := LoWord(Msg.wParam);
              end;
            end;
          end;
        end;
    end;
  end;
  Result := CallNextHookEx(MenuCallWndHook, nCode, WParam, Longint(Msg));
end;

procedure InstallMenuCallWndHook(Menu: TMenu);
begin
  ActiveHookMenu := Menu;
  MenuCallWndHook := SetWindowsHookEx(WH_CALLWNDPROC, @CallWndHookProc, 0, GetCurrentThreadId);
end;

procedure UnInstallMenuCallWndHook;
begin
  if MenuCallWndHook <> 0 then
    UnHookWindowsHookEx(MenuCallWndHook);
  MenuCallWndHook := 0;
  ActiveHookMenu := nil;
  PopupWndList := nil;
end;

{ TPopupMenu }
procedure TPopupMenu.Popup(X, Y: Integer);
begin
  if not FTrackMenu then
    inherited
  else
  try
    InstallMenuCallWndHook(Self);
    inherited;
  finally
    UnInstallMenuCallWndHook;
  end;
end;

function GetMenuForm(Menu: TMenu): TCustomForm;
var
  LForm: TWinControl;
begin
  Result := nil;
  if Menu.WindowHandle <> 0 then
  begin
    LForm := FindControl(Menu.WindowHandle);
    if (LForm <> nil) and (LForm is TCustomForm) then
      Result := LForm as TCustomForm;
  end;
end;

function FormMainMenuIsValid(AForm: TCustomForm): Boolean;
begin
  Result := False;
  if Assigned(AForm) and Assigned(AForm.Menu) then
  begin
    {$IFDEF TNT}
    if (AForm.Menu is TTntMainMenu) then
      Result := TTntMainMenu(AForm.Menu).FTrackMenu
    else
    {$ENDIF}
    if (AForm.Menu is TMainMenu) then
      Result := TMainMenu(AForm.Menu).FTrackMenu;
  end;
end;

procedure FormMainMenuWndProcMessage(var Msg: TMessage; AForm: TCustomForm);
begin
  if not FormMainMenuIsValid(AForm) then
    Exit;

  case Msg.Msg of
    WM_INITMENU:
      begin
        // MSDN: Sent when a menu is about to become active. It occurs when the user clicks an item on the menu bar or presses a menu key.
        // A window receives this message through its WindowProc function
        // A WM_INITMENU message is sent only when a menu is first accessed; only one WM_INITMENU message is generated for each access.
        // For example, moving the mouse across several menu items while holding down the button does not generate new messages
        InstallMenuCallWndHook(AForm.Menu);
      end;
    WM_MENUSELECT:
      begin
        // MSDN: If the high-order word of wParam contains 0xFFFF and the lParam parameter contains NULL, the system has closed the menu.
        if (Msg.lParam = 0) and (HiWord(Msg.wParam) = $FFFF) then // Menu Closed
        begin
          UnInstallMenuCallWndHook;
        end;
      end;
  end;
end;

end.
