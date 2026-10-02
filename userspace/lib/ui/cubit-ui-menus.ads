--  Native menubar controller. No allocation, application callbacks, or IPC.
with CuBit.UI.Controls;
package CuBit.UI.Menus is
   Max_Menus : constant := 8;
   Max_Items : constant := 64;
   type Text is access constant String;
   type Menu is record
      Caption : Text := null;
      Mnemonic : Character := ' ';
   end record;
   type Item is record
      Parent : Positive range 1 .. Max_Menus := 1;
      Caption, Shortcut : Text := null;
      Mnemonic : Character := ' ';
      Command : Natural := 0;
      Enabled : Boolean := True;
      Checked : Boolean := False;
      Separator : Boolean := False;
   end record;
   type Menu_List is array (Positive range 1 .. Max_Menus) of Menu;
   type Item_List is array (Positive range 1 .. Max_Items) of Item;
   type Model is record
      Menus : Menu_List;
      Items : Item_List;
      Menu_Count : Natural range 0 .. Max_Menus := 0;
      Item_Count : Natural range 0 .. Max_Items := 0;
   end record;
   --  Reserve Base .. Base + 72 in the application's Control_Map. There must
   --  also be room for its other controls within Controls.MAX_CONTROLS.
   subtype ID_Base is Positive range 1 .. Natural'Last - 72;
   function Title_ID (Base : ID_Base; Index : Positive)
      return Controls.Control_ID with Pre => Index <= Max_Menus;
   function Item_ID (Base : ID_Base; Index : Positive)
      return Controls.Control_ID with Pre => Index <= Max_Items;
   function Valid (Definition : Model) return Boolean;
   function Is_Menu_Control (Base : ID_Base; Target : Controls.Control_ID)
      return Boolean;

   type Menu_State is private;
   function Is_Open (State : Menu_State) return Boolean;
   function Open_Menu (State : Menu_State) return Natural;
   function Selected_Item (State : Menu_State) return Natural;
   procedure Dismiss (State : in out Menu_State);

   --  Draw AFTER content and its registrations, so popup hits win. Call again
   --  after any model/layout/state change before accepting the next pointer
   --  event. All geometry is logical; canvas density/clipping is preserved.
   --  Popup width is clipped to the canvas, and keyboard selection scrolls
   --  long menus into view. Command zero is reserved for "no action".
   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; State : Menu_State;
      Definition : Model; Base : ID_Base; Bounds : Rect; Colors : Theme;
      Popup_Width : Positive := 260; Row_Height : Positive := 28);

   type Key is (Activate, Left, Right, Up, Down, Home, End_Key,
                Enter, Space, Escape, Tab_Key, Mnemonic);
   --  Map F10/Alt to Activate, Alt+letter to Mnemonic when closed and plain
   --  letters to Mnemonic when open. Consume only when Handled is true.
   procedure Handle_Key
     (State : in out Menu_State; Definition : Model; Event : Key;
      Command : out Natural; Handled : out Boolean;
      Letter : Character := ' ');

   --  IMPORTANT: while open, route NON-menu PRESSES here BEFORE native App
   --  pointer dispatch, and consume them; this prevents underlying controls
   --  from capturing a dismissed popup's outside click. All moves/releases
   --  still go through App first so an existing menu capture is cleared. Then
   --  route App.Apply_Pointer_Event first, then pass the current Controls.Hit
   --  target (NOT the captured control) and action here. Title press-drag-release
   --  is tracked separately while App retains authoritative pointer capture.
   --  Use Is_Menu_Control to distinguish the two. Retained activation is consumed;
   --  it never invents a second hit-test or dispatches a command during draw.
   --  While open, outside presses dismiss AND consume (no click-through).
   procedure Handle_Pointer
     (State : in out Menu_State; Definition : Model;
      Map : in out Controls.Control_Map; Base : ID_Base;
      Target : Controls.Control_ID; Action : Controls.Pointer_Action;
      Command : out Natural; Handled : out Boolean);
private
   type Menu_State is record
      Opened : Natural range 0 .. Max_Menus := 0;
      Hot_Title : Natural range 0 .. Max_Menus := 0;
      Title_Drag : Boolean := False;
      Selected : Natural range 0 .. Max_Items := 0;
   end record;
end CuBit.UI.Menus;
