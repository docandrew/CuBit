with Interfaces;
with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.Icons;
with CuBit.UI.Splits;
with CuBit.UI.State;
with Files_Limits;
with Files_Order;

--  CuBit Files' view (docs/files-app.md): two Commander-style panes over
--  the filesystem's request queue, drawn with the shared toolkit.
--  Platform-free: the native main (userspace/apps/files/main.adb) and the
--  hosted harness (tests/files-app) translate input into Events, call Pump
--  between frames to move I/O and incremental work along, and Render into
--  their canvas. Nothing here blocks: listing, sorting and filtering
--  arrive in pieces and the view shows each as it lands.
package Files_View is
   use Interfaces;
   package Controls renames CuBit.UI.Controls;

   type Key_Name is
     (No_Key, Up, Down, Left, Right, Page_Up, Page_Down, Home, End_Key, Enter, Escape, Backspace, Delete,
      Insert, Tab, Space, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, Letter_A, Letter_B, Letter_D,
      Letter_I, Letter_N, Letter_R, Letter_T, Letter_U, Letter_W, Menu_Key);
   type Event_Kind is (Key_Event, Text_Event, Pointer_Event, Wheel, Resize);
   type Event is record
      Kind : Event_Kind := Key_Event;
      Key : Key_Name := No_Key;
      Shift, Control, Alt : Boolean := False;
      Character_Value : Character := ' ';
      Action : Controls.Pointer_Action := Controls.Pointer_Move;
      X, Y : Natural := 0;
      --  Wheel: positive scrolls toward the top.
      Steps : Integer := 0;
      --  Monotonic milliseconds, for double clicks.
      Time_Ms : Unsigned_64 := 0;
      --  The secondary (right) button: context menus.
      Secondary : Boolean := False;
      --  The middle button: closes the tab under it.
      Middle : Boolean := False;
   end record;

   --  Panes left to right: any number up to MAXIMUM_PANES, each shown or
   --  hidden (single-pane mode hides the others).
   MAXIMUM_PANES : constant := 6;
   type Side is range 1 .. MAXIMUM_PANES;
   Left_Pane : constant Side := 1;
   Right_Pane : constant Side := 2;
   --  Tabs per pane.
   MAXIMUM_TABS : constant := 8;
   type Load_State is (Not_Started, Opening, Reading, Loaded, Failed);

   --  Control IDs (CuBit.UI.Controls), for the platform and tests.
   --  Each pane's IDs: PANE_IDS of them from PANE_ID_FIRST.
   PANE_ID_FIRST : constant Controls.Control_ID := 200;
   PANE_IDS : constant := 24;
   function Pane_ID (Pane : Side; Offset : Natural) return Controls.Control_ID is
     (PANE_ID_FIRST + (Natural (Pane) - 1) * PANE_IDS + Offset);
   function ROWS_ID (Pane : Side) return Controls.Control_ID is (Pane_ID (Pane, 0));
   function SCROLL_ID (Pane : Side) return Controls.Control_ID is (Pane_ID (Pane, 1));
   function PATH_ID (Pane : Side) return Controls.Control_ID is (Pane_ID (Pane, 2));
   function TABS_ID (Pane : Side) return Controls.Control_ID is (Pane_ID (Pane, 3));
   --  The column header's 2 * CuBit.UI.Tables.MAX_COLUMNS IDs.
   function COLUMNS_BASE (Pane : Side) return Controls.Control_ID is (Pane_ID (Pane, 4));
   DIVIDER_FIRST : constant Controls.Control_ID := 180;
   FUNCTION_KEY_FIRST : constant Controls.Control_ID := 60;
   TOOL_FIRST : constant Controls.Control_ID := 70;
   SEARCH_ID : constant Controls.Control_ID := 90;
   DRAWER_ID : constant Controls.Control_ID := 91;
   DRAWER_EDGE_ID : constant Controls.Control_ID := 92;
   POPUP_BASE : constant Controls.Control_ID := 100;

   --  The toolbar's buttons, left to right.
   type Tool is
     (Tool_Back, Tool_Forward, Tool_Up, Tool_Refresh, Tool_New_Folder, Tool_Copy, Tool_Move, Tool_Delete,
      Tool_Drawer, Tool_Columns, Tool_Viewer, Tool_New_Tab, Tool_Add_Pane, Tool_Single_Pane);
   function Tool_ID (Item : Tool) return Controls.Control_ID is (TOOL_FIRST + Tool'Pos (Item));

   type View_State is limited private;

   --  A place the drawer offers (a granted root, a home folder): its
   --  caption, path and icon. The platform declares them after Initialize.
   procedure Add_Place
     (State : in out View_State; Caption, Path : String; Picture : CuBit.UI.Icons.Icon := CuBit.UI.Icons.Drive);

   --  Capacity: entries one pane can list; Name_Bytes: their names' bytes.
   --  Both explicit (docs/files-app.md: no hidden limits).
   --  Left_Path and Right_Path empty: the panes start at the folders the
   --  service lists as granted (Queue_List_Scopes), each also a place.
   procedure Initialize
     (State : out View_State; Capacity : Files_Limits.Entry_Capacity;
      Name_Bytes : Files_Limits.Arena_Capacity; Left_Path, Right_Path : String);

   --  Release the filesystem queue (handles still open are the service's
   --  to drop with the queue).
   procedure Close (State : in out View_State);

   --  The platform's monotonic clock (microseconds), given before Handle:
   --  what input starts (a listing, a message's lifetime) is timed from it,
   --  not from the last Pump.
   procedure Set_Clock (State : in out View_State; Now_Us : Unsigned_64);
   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean);
   procedure Render
     (State : in out View_State; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect;
      UI : in out CuBit.UI.State.UI_State; Map : in out Controls.Control_Map);

   --  Between frames: reap the filesystem's answers, submit what follows,
   --  and do at most Budget units of sorting and filtering. Now_Us is a
   --  monotonic clock (microseconds) for the listing statistics. Busy: work
   --  for this thread remains (call again next frame, without waiting);
   --  otherwise wait for input, the service's wake or Next_Deadline_Us.
   --  Changed: the frame should be redrawn.
   procedure Pump
     (State : in out View_State; Budget : Files_Limits.Work_Budget; Now_Us : Unsigned_64;
      Busy, Changed : out Boolean);

   --  What changed since the last Take_Damage (the platform repaints only
   --  that); empty when nothing did.
   procedure Take_Damage (State : in out View_State; Area : out CuBit.UI.Rect);
   --  Damage the platform's pointer dispatch found (a retained control's
   --  action or face), repainted with the view's own.
   procedure Note_Damage (State : in out View_State; Area : CuBit.UI.Rect);
   --  The platform's frame and pump times, shown by F12's overlay.
   procedure Note_Frame (State : in out View_State; Render_Us, Pump_Us : Unsigned_64);
   --  The person asked to quit (F10).
   function Quit_Requested (State : View_State) return Boolean;

   --  When the view next needs Pump without input (a status message
   --  expires): monotonic microseconds, Unsigned_64'Last for never.
   function Next_Deadline_Us (State : View_State) return Unsigned_64;
   --  Requests are out: the platform waits for the service's wake (or its
   --  input, or the deadline) rather than polling.
   function Waiting_For_IO (State : View_State) return Boolean;

   --  What the view shows, for tests and the overlay.
   function Viewer_Open (State : View_State) return Boolean;
   function Viewer_Line (State : View_State; Line : Positive) return String;
   function Status_Message (State : View_State) return String;
   function Active (State : View_State) return Side;
   function Path (State : View_State; Pane : Side) return String;
   function Load (State : View_State; Pane : Side) return Load_State;
   --  Entries listed, and rows shown (the ".." row included).
   function Listed (State : View_State; Pane : Side) return Natural;
   function Rows (State : View_State; Pane : Side) return Natural;
   function Cursor_Row (State : View_State; Pane : Side) return Natural;
   function Top_Row (State : View_State; Pane : Side) return Natural;
   --  The name on a row ("..", or an entry's name).
   function Row_Name (State : View_State; Pane : Side; Row : Positive) return String;
   function Cursor_Name (State : View_State; Pane : Side) return String;
   function Marked_Count (State : View_State; Pane : Side) return Natural;
   function Is_Marked (State : View_State; Pane : Side; Row : Positive) return Boolean;
   function Filter_Text (State : View_State; Pane : Side) return String;
   function Rule (State : View_State; Pane : Side) return Files_Order.Sort_Rule;
   --  The pane has nothing more to do: listed, sorted, filtered.
   function Settled (State : View_State; Pane : Side) return Boolean;
   --  Microseconds from asking for the listing to its last entry.
   function Listing_Us (State : View_State; Pane : Side) return Unsigned_64;
   function Overlay_Shown (State : View_State) return Boolean;
   function Menu_Open (State : View_State) return Boolean;
   --  The open context menu's selected row (0 none) and depth.
   function Menu_Selected (State : View_State) return Natural;
   function Drawer_Open (State : View_State) return Boolean;
   --  The drawer's rows: captions ("" for none), for tests.
   function Drawer_Row (State : View_State; Row : Positive) return String;
   function Bookmarked (State : View_State; Pane : Side) return Boolean;
   --  The pane's columns, by title: "Name|Size|Modified|".
   function Column_Titles (State : View_State; Pane : Side) return String;
   --  Panes open and shown; tabs of a pane and the current one.
   function Pane_Count (State : View_State) return Side;
   function Pane_Shown (State : View_State; Pane : Side) return Boolean;
   function Tab_Count (State : View_State; Pane : Side) return Natural;
   function Current_Tab (State : View_State; Pane : Side) return Natural;
   --  Tab K's close button (empty when the tab strip is hidden or narrow).
   function Tab_Close_Area (State : View_State; Pane : Side; K : Positive) return CuBit.UI.Rect;
   --  The pane's width from the last frame (for split tests).
   function Pane_Width (State : View_State; Pane : Side) return Natural;
   --  The function-key bar is shown (Ctrl+F12 toggles it).
   function Keys_Shown (State : View_State) return Boolean;
   --  The active pane's volume space as the status line shows it ("" until known).
   function Volume_Space (State : View_State) return String;
   --  The tooltip showing, "" for none.
   function Tooltip_Text (State : View_State) return String;
private
   type Pane_State;
   type Pane_Access is access Pane_State;
   type Pane_Table is array (Side) of Pane_Access;
   type Shown_Table is array (Side) of Boolean;

   type View_State is limited record
      Panes : Pane_Table := [others => null];
      Count : Side := 2;
      Shown : Shown_Table := [others => True];
      --  The split's lengths (CuBit.UI.Splits), by shown pane.
      Shares : CuBit.UI.Splits.Weights := [others => 1];
      Current : Side := Left_Pane;
      Dirty : CuBit.UI.Rect := (others => 0);
      Bounds : CuBit.UI.Rect := (others => 0);
      Overlay : Boolean := False;
      Quit : Boolean := False;
      Last_Render_Us, Worst_Render_Us, Last_Pump_Us, Last_Work : Unsigned_64 := 0;
      Frames : Unsigned_64 := 0;
      --  The last row press, for double clicks.
      Last_Press_Ms : Unsigned_64 := 0;
      Last_Press_Row : Natural := 0;
      Last_Press_Side : Side := Left_Pane;
   end record;
end Files_View;
