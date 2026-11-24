with CuBit.UI_Metrics;
with Ada.Unchecked_Deallocation;
with CuBit.Directory_Paths;
with CuBit.Filesystems;
with CuBit.File_Access;
with CuBit.Filesystem_Events;
with CuBit.Filesystem_Places;
with CuBit.Volume_Descriptions;
with CuBit.UI.Button_Bars;
with CuBit.UI.Drawers;
with CuBit.UI.Keycaps;
with CuBit.UI.Modifier_Reveal;
with CuBit.UI.Popup_Menus;
with CuBit.UI.Standard_Keys;
with CuBit.UI.Status_Lines;
with CuBit.UI.Tables;
with CuBit.UI.Tooltips;
with CuBit.UI.Widgets;
with Files_Commands;
with Files_Filter;
with Files_Listing;
with Files_Marks;
with Files_Operations;
with Files_Plan;
with Files_Pages;
with Files_Places;
with Files_Queue;
with Files_Reader;
with Files_Status;
with Files_Viewport;

package body Files_View is
   use CuBit.UI;
   use Files_Limits;
   use type Files_Listing.Entry_Kind;
   use type Files_Queue.Token;
   use type Files_Order.Sort_Direction, Files_Order.Sort_Key, Files_Reader.Read_State;
   use type Files_Operations.Conflict_Policy, Files_Operations.Phase_Kind;
   use type CuBit.UI.Tables.Sort_Order;
   use type Controls.Pointer_Action;
   package DP renames CuBit.Directory_Paths;
   package FS renames CuBit.Filesystems;
   package FE renames CuBit.Filesystem_Events;
   package FA renames CuBit.File_Access;
   package VD renames CuBit.Volume_Descriptions;
   package FQ renames Files_Queue.FQ;
   package Tables renames CuBit.UI.Tables;
   package VP renames Files_Viewport;

   --  Layout (logical pixels).
   --  From the UI em (CuBit.UI_Metrics): dense rows in the panes.
   package M renames CuBit.UI_Metrics;
   MARGIN : constant := M.Space_S;
   PATH_HEIGHT : constant := M.Compact_Row_Height;
   ROW_HEIGHT : constant := M.Dense_Row_Height;
   STATUS_HEIGHT : constant := M.Compact_Row_Height;
   SCROLLBAR_WIDTH : constant := 14;
   TEXT_INSET : constant := M.Text_Inset;
   MARK_BAR_WIDTH : constant := 3;
   NAME_COLUMN_MINIMUM : constant := 5 * M.Base_Em;
   OVERLAY_WIDTH : constant := 24 * M.Base_Em;
   OVERLAY_LINE : constant := M.Line_Height + 1;
   OVERLAY_LINES : constant := 6;
   --  Input.
   WHEEL_ROWS : constant := 3;
   DOUBLE_CLICK_MS : constant := 500;
   --  I/O: per pane, a page for the path the open names, then two read
   --  slots of sixteen Directory.Page.V2 pages (up to 896 entries, about
   --  800 with typical names) each, both in flight.
   READ_SLOTS : constant := 2;
   PAGES_PER_READ : constant := 16;
   SLOT_BYTES : constant := PAGES_PER_READ * Files_Pages.PAGE_BYTES;
   PATH_AREA_BYTES : constant := DP.Maximum_Bytes;
   PANE_ARENA_BYTES : constant := PATH_AREA_BYTES + READ_SLOTS * SLOT_BYTES;
   --  After the panes' areas: the quick viewer's reader.
   READER_BASE : constant := MAXIMUM_PANES * PANE_ARENA_BYTES;
   OPERATIONS_BASE : constant := READER_BASE + Files_Reader.AREA_BYTES;
   --  Then the places (Queue_List_Places: granted scopes on volumes that
   --  are present) and a volume description (Queue_Describe_Volume: the
   --  path in, the record out).
   PLACES_BASE : constant := OPERATIONS_BASE + Files_Operations.AREA_BYTES;
   PLACES_BYTES : constant := CuBit.File_Access.Maximum_Entries * CuBit.File_Access.Wire_Entry_Bytes;
   VOLUME_BASE : constant := PLACES_BASE + PLACES_BYTES;
   VOLUME_BYTES : constant := DP.Maximum_Bytes;
   ARENA_PAGES : constant := (VOLUME_BASE + VOLUME_BYTES + FQ.Page_Bytes - 1) / FQ.Page_Bytes;
   --  Change records taken per pump, at most (the rest wait for the next).
   EVENTS_PER_PUMP : constant := 1_024;
   DIALOG_WIDTH : constant := 560;
   DIALOG_HEIGHT : constant := 11 * M.Base_Em + M.Space_M;
   DIALOG_INSET : constant := M.Space_M;
   PROGRESS_WIDTH : constant := 240;
   --  How long a viewer read may take before it is reported as timed out,
   --  and how long a status message stays (explicit, docs/files-app.md).
   VIEW_DEADLINE_US : constant := 2_000_000;
   MESSAGE_LIFETIME_US : constant := 6_000_000;
   VIEW_HEADER_HEIGHT : constant := M.Compact_Row_Height;
   HEX_BYTES_PER_LINE : constant := 16;
   MAXIMUM_VIEW_COLUMNS : constant := 400;
   BINARY_PROBE_BYTES : constant := 4_096;
   TOOL_SIZE : constant := M.Control_Height;
   TOOLBAR_HEIGHT : constant := TOOL_SIZE + M.Space_S + 2;
   TAB_HEIGHT : constant := M.Compact_Row_Height;
   TAB_WIDTH : constant := 10 * M.Base_Em + M.Space_M;
   --  Each tab's close button (x) at its right end.
   TAB_CLOSE_SIZE : constant := 16;
   TAB_CLOSE_INSET : constant := 4;
   PANE_MINIMUM : constant := 220;
   MAXIMUM_CLOSED_TABS : constant := 8;
   TOOL_GAP : constant := 2;
   TOOL_GROUP_GAP : constant := 12;
   SEARCH_WIDTH : constant := 260;
   SEARCH_HEIGHT : constant := M.Compact_Row_Height;
   MAXIMUM_PLACES : constant := 12;
   MAXIMUM_BOOKMARKS : constant := 16;
   MAXIMUM_RECENT : constant := 8;
   --  Drawer rows carry what they lead to: a place, bookmark or recent
   --  folder, by index above these bases.
   PLACE_VALUE : constant := 0;
   BOOKMARK_VALUE : constant := 100;
   RECENT_VALUE : constant := 200;
   HISTORY_DEPTH : constant := 32;
   MAXIMUM_MESSAGE : constant := 120;
   MICROSECONDS_PER_SECOND : constant := 1_000_000;
   MILLISECONDS_PER_DAY : constant := 86_400_000;
   MILLISECONDS_PER_MINUTE : constant := 60_000;
   MINUTES_PER_HOUR : constant := 60;

   pragma Compile_Time_Error
     (PATH_AREA_BYTES mod FQ.Page_Bytes /= 0 or else PANE_ARENA_BYTES mod FQ.Page_Bytes /= 0,
      "arena areas must be whole pages");

   type Slot_State is record
      Busy : Boolean := False;
      Tag : Files_Queue.Token := Files_Queue.NO_TOKEN;
      Generation : Natural := 0;
      --  A read this listing still wants on the slot (submitted when free).
      Wanted : Boolean := False;
   end record;
   type Slot_Table is array (1 .. READ_SLOTS) of Slot_State;

   subtype Name_Text is String (1 .. MAXIMUM_NAME_BYTES);
   type History_Entry is record
      Where : DP.Path;
      Name : Name_Text := [others => ' '];
      Length : Name_Length := 0;
   end record;
   type History_Table is array (1 .. HISTORY_DEPTH) of History_Entry;
   subtype History_Count is Natural range 0 .. HISTORY_DEPTH;

   --  Where the cursor is: on "..", on an entry, or on the first row
   --  (a new listing, or a filter's first match).
   type Cursor_Kind is (On_Parent_Row, On_Entry, On_First);

   --  A tab: what the pane shows when it is the current one.
   type Tab_State is record
      Where : DP.Path;
      Name : Name_Text := [others => ' '];
      Length : Name_Length := 0;
      Rule : Files_Order.Sort_Rule;
   end record;
   type Tab_Table is array (1 .. MAXIMUM_TABS) of Tab_State;
   subtype Tab_Count_Range is Natural range 0 .. MAXIMUM_TABS;

   --  What a column shows. Name is always first (it carries the icon);
   --  the others can be added, removed and reordered.
   type Column_Kind is (Name_Column, Extension_Column, Size_Column, Modified_Column, Changed_Column,
                        Kind_Column, Mode_Column, Owner_Column);
   type Kind_Table is array (Tables.Column_Index) of Column_Kind;
   COLUMN_CAPTIONS : constant array (Column_Kind) of access constant String :=
     [new String'("Name"), new String'("Ext"), new String'("Size"), new String'("Modified"),
      new String'("Changed"), new String'("Type"), new String'("Permissions"), new String'("Owner")];
   --  The widest text each column shows (Name takes the width the others
   --  leave): columns are sized to fit it and their title.
   COLUMN_SAMPLES : constant array (Column_Kind) of access constant String :=
     [new String'(""), new String'(".jpeg"), new String'("1023 MiB"), new String'("0000-00-00 00:00"),
      new String'("0000-00-00 00:00"), new String'("Folder"), new String'("drwxr-xr-x"),
      new String'("65535:65535")];
   COLUMN_KEYS : constant array (Column_Kind) of Boolean :=
     [Name_Column | Extension_Column | Size_Column | Modified_Column => True, others => False];

   type Pane_State (Capacity : Entry_Capacity; Arena_Bytes : Arena_Capacity) is limited record
      Listing : Files_Listing.Listing (Capacity, Arena_Bytes);
      Order : Files_Order.Order_State (Capacity);
      Filter : Files_Filter.Filter_State (Capacity);
      Marks : Files_Marks.Mark_State (Capacity);
      View : VP.Viewport;
      Where : DP.Path;
      Root_Length : Natural := 0;
      Load : Load_State := Not_Started;
      Error : Unsigned_32 := 0;
      Generation : Natural := 0;
      Handle : Unsigned_64 := 0;
      Open : Slot_State;
      Reads : Slot_Table;
      Pages : Files_Pages.Cursor_State;
      Ended, Truncated, Malformed : Boolean := False;
      Started_Us, Finished_Us : Unsigned_64 := 0;
      Cursor : Cursor_Kind := On_First;
      Cursor_Id : Entry_Id := 1;
      --  A name to put the cursor on when it arrives (the folder just left).
      Wanted : Name_Text := [others => ' '];
      Wanted_Length : Name_Length := 0;
      Back, Forward : History_Table;
      Back_Count, Forward_Count : History_Count := 0;
      Columns : Tables.Column_Layout;
      Kinds : Kind_Table := [Name_Column, Size_Column, Modified_Column, others => Name_Column];
      --  A header press, for dragging a column to a new place.
      Header_Press : Natural := 0;
      Tabs : Tab_Table;
      Tab_Count : Tab_Count_Range := 1;
      Current_Tab : Natural range 0 .. MAXIMUM_TABS := 1;
      Tab_Press : Natural := 0;
      --  A tab's close button pressed (released on it: that tab closes).
      Close_Press : Natural := 0;
      Tabs_Area : Rect := (others => 0);
      Seen_Order, Seen_Filter : Unsigned_64 := 0;
      --  From the last Render, for hits and damage.
      Rows_Area, Path_Area, Header_Area : Rect := (others => 0);
      --  The change watch on the folder shown (Queue_Watch): its number
      --  (0: none) and folder, the request in flight and the folder it
      --  asked for, and whether a change came that the listing lacks.
      Watch : Natural := 0;
      Watched, Watch_Asked : DP.Path;
      Watch_Slot : Slot_State;
      Stale : Boolean := False;
      --  The folder is the one the pane showed last (Initialize), not yet
      --  listed: if it cannot be opened, the pane goes to the first place.
      Restoring : Boolean := False;
   end record;

   --  Per-view state the private part does not declare (one view per
   --  process): the clock and the status message.
   Now_Us : Unsigned_64 := 0;
   Message : String (1 .. MAXIMUM_MESSAGE) := [others => ' '];
   Message_Length : Natural range 0 .. MAXIMUM_MESSAGE := 0;
   Status_Area : Rect := (others => 0);

   Message_Until_Us : Unsigned_64 := 0;
   --  The places (Queue_List_Places), asked for once at Initialize; the
   --  first is where a pane starts that has no folder of its own.
   Places_Slot : Slot_State;
   Places_Known : Boolean := False;
   Has_First_Place : Boolean := False;
   First_Place : DP.Path;
   --  Free space of the active pane's volume (Queue_Describe_Volume).
   Volume_Slot : Slot_State;
   Volume_Asked, Volume_Of : DP.Path;
   Empty_Path : DP.Path;
   Volume_Known, Volume_Read_Only : Boolean := False;
   --  Something changed on it since: ask again.
   Volume_Stale : Boolean := False;
   Volume_Free, Volume_Total : Unsigned_64 := 0;

   --  A status message, shown for MESSAGE_LIFETIME_US and then cleared.
   procedure Say (Text : String) is
   begin
      Message_Length := Natural'Min (Text'Length, MAXIMUM_MESSAGE);
      Message (1 .. Message_Length) := Text (Text'First .. Text'First + Message_Length - 1);
      Message_Until_Us := Now_Us + MESSAGE_LIFETIME_US;
   end Say;

   --  The quick viewer (F3, or Enter on a file).
   type Line_Table is array (Positive range <>) of Natural;
   type Line_Table_Access is access Line_Table;
   Viewing : Boolean := False;
   Viewer_Name : String (1 .. MAXIMUM_NAME_BYTES) := [others => ' '];
   Viewer_Name_Length : Name_Length := 0;
   Viewer_Top : Natural := 0;
   Viewer_Rows : Positive := 1;
   Viewer_Hex : Boolean := False;
   Line_Starts : Line_Table_Access;
   Line_Count : Natural := 0;
   Reader_Seen : Unsigned_64 := 0;

   --  The drawer: places the platform declared, bookmarks, recent folders.
   type Place_Entry is record
      Caption : String (1 .. CuBit.UI.Drawers.MAXIMUM_CAPTION) := [others => ' '];
      Caption_Length : Natural range 0 .. CuBit.UI.Drawers.MAXIMUM_CAPTION := 0;
      Where : DP.Path;
      Picture : CuBit.UI.Icons.Icon := CuBit.UI.Icons.Drive;
   end record;
   type Place_Table is array (Positive range <>) of Place_Entry;
   Places : Place_Table (1 .. MAXIMUM_PLACES);
   Place_Count : Natural range 0 .. MAXIMUM_PLACES := 0;
   Bookmarks : Place_Table (1 .. MAXIMUM_BOOKMARKS);
   Bookmark_Count : Natural range 0 .. MAXIMUM_BOOKMARKS := 0;
   Recent : Place_Table (1 .. MAXIMUM_RECENT);
   Recent_Count : Natural range 0 .. MAXIMUM_RECENT := 0;
   Drawer : CuBit.UI.Drawers.Drawer_State;
   Shortcuts : CuBit.UI.Drawers.Shortcut_List;
   Drawer_Area, Drawer_Edge, Toolbar_Area : Rect := (others => 0);
   --  Hover tooltips (CuBit.UI.Tooltips): the frame's tips and the one shown.
   package Tooltips renames CuBit.UI.Tooltips;
   Tips : Tooltips.Tip_Table;
   Tip : Tooltips.Tooltip;
   MICROSECONDS_PER_MILLISECOND : constant := 1_000;

   --  Commands and keys (UI-052): every key, menu item, toolbar button and
   --  key bar cell runs a command (Files_Commands) through one of three
   --  keymaps, the context's: the panes, the viewer or a prompt.
   package Keys renames CuBit.UI.Keys;
   package Maps renames Files_Keymaps.Map;
   subtype Command is Files_Keymaps.Command;
   use all type Files_Keymaps.Command;
   use type Keys.Key, Keys.Chord, Keys.Modifier_Set;
   Panes_Map, Viewer_Map, Prompt_Map : Maps.Keymap;
   --  Whether each command can run now and whether it is on, refreshed
   --  from the view before each frame and each key (Note_State).
   Enabled_Now : array (Command) of Boolean := [others => True];
   Checked_Now : array (Command) of Boolean := [others => False];
   function Enabled (Item : Command) return Boolean is (Enabled_Now (Item));
   function Checked (Item : Command) return Boolean is (Checked_Now (Item));
   package Bars is new CuBit.UI.Button_Bars
     (Files_Keymaps.Map, Files_Commands.Caption, Files_Commands.Description, Enabled, Checked,
      Files_Commands.Shade, Files_Commands.Picture);
   --  The function-key bar (F1 .. F10, clusters as on a keyboard), the
   --  toolbar (icon-only buttons) and a prompt's buttons.
   Key_Bar, Tool_Bar, Dialog_Bar, Policy_Bar : Bars.Bar;
   --  Toggle_Key_Bar (Ctrl+F12) hides and shows the key bar.
   Key_Bar_Shown : Boolean := True;
   --  Held modifiers reveal their hints (CuBit.UI.Modifier_Reveal), and
   --  what was revealed at the last frame (to repaint when it changes).
   package MR renames CuBit.UI.Modifier_Reveal;
   Reveal : MR.State;
   Shown_Reveal : Keys.Modifier_Set := Keys.No_Modifiers;
   Key_Bar_Area, Reveal_Damage : Rect := (others => 0);
   --  The keymap of the context keys go to now.
   function Context_Map return Maps.Keymap;
   function Now_Ms return Unsigned_64;
   function Revealed_Now return Keys.Modifier_Set is (MR.Revealed (Reveal, Now_Ms));
   --  A hint as text, for status lines ("Esc"): generated from the keymap,
   --  never typed by hand; "" when the command has no chord.
   function Key_Text (Map : Maps.Keymap; Item : Command) return String is (Keys.Image (Maps.Hint (Map, Item)));
   function Key_Bar_Height (Width : Natural) return Natural is
     (if Key_Bar_Shown then Bars.Height (Key_Bar, Context_Map, Width, Revealed_Now) else 0);

   --  The toolbar's buttons and the commands they run.
   TOOL_COMMANDS : constant array (Tool) of Command :=
     [Tool_Back => History_Back, Tool_Forward => History_Forward, Tool_Up => Parent_Folder,
      Tool_Refresh => Refresh, Tool_New_Folder => New_Folder, Tool_Copy => Copy, Tool_Move => Move,
      Tool_Delete => Delete, Tool_Drawer => Toggle_Drawer, Tool_Columns => Columns_Menu, Tool_Viewer => View,
      Tool_New_Tab => New_Tab, Tool_Add_Pane => Add_Pane, Tool_Single_Pane => Single_Pane];

   --  A clipboard copy chosen from the menu, for the platform.
   Pending_Clipboard : Clipboard_Request := No_Request;
   Menu : CuBit.UI.Popup_Menus.Model;
   Popup : CuBit.UI.Popup_Menus.Popup_State;
   Menu_Side : Side := Left_Pane;

   --  The modal prompt: confirming an operation, asking for a name, or
   --  asking what to do about a name conflict.
   type Prompt_Kind is (No_Prompt, Confirm_Copy, Confirm_Move, Confirm_Delete, Ask_Folder_Name, Ask_New_Name,
                        Ask_Conflict);
   Prompt : Prompt_Kind := No_Prompt;
   Prompt_Side : Side := Left_Pane;
   Prompt_Policy : Files_Operations.Conflict_Policy := Files_Operations.Ask;
   Input : String (1 .. MAXIMUM_NAME_BYTES) := [others => ' '];
   Input_Length : Name_Length := 0;
   --  The rename's original name.
   Renaming : String (1 .. MAXIMUM_NAME_BYTES) := [others => ' '];
   Renaming_Length : Name_Length := 0;
   Operation_Seen : Unsigned_64 := 0;
   Operation_Started_Us : Unsigned_64 := 0;
   Dialog_Area : Rect := (others => 0);
   Closed_Tabs : array (1 .. MAXIMUM_CLOSED_TABS) of Tab_State;
   Closed_Count : Natural range 0 .. MAXIMUM_CLOSED_TABS := 0;

   ---------------------------------------------------------------------------
   --  Text.
   ---------------------------------------------------------------------------
   function Image (Value : Unsigned_64) return String is
      Raw : constant String := Unsigned_64'Image (Value);
   begin
      return Raw (Raw'First + 1 .. Raw'Last);
   end Image;

   --  1,234,567
   function Thousands (Value : Unsigned_64) return String is
      Raw : constant String := Image (Value);
      GROUP : constant := 3;
      Result : String (1 .. Raw'Length + (Raw'Length - 1) / GROUP);
      Out_At : Natural := Result'Last;
      Taken : Natural := 0;
   begin
      for K in reverse Raw'Range loop
         if Taken > 0 and then Taken mod GROUP = 0 then
            Result (Out_At) := ',';
            Out_At := Out_At - 1;
         end if;
         Result (Out_At) := Raw (K);
         Out_At := Out_At - 1;
         Taken := Taken + 1;
      end loop;
      return Result;
   end Thousands;

   --  A duration in microseconds as "0.123 ms".
   function Milliseconds (Us : Unsigned_64) return String is
      THOUSAND : constant := 1_000;
      Fraction : constant String := Image (THOUSAND + Us mod THOUSAND);
   begin
      return Image (Us / THOUSAND) & "." & Fraction (Fraction'First + 1 .. Fraction'Last) & " ms";
   end Milliseconds;

   function Size_Text (Bytes : Unsigned_64) return String is
      KIBI : constant := 1_024;
      TENTHS : constant := 10;
      UNITS : constant array (1 .. 5) of String (1 .. 3) := ["KiB", "MiB", "GiB", "TiB", "PiB"];
      Scale : Unsigned_64 := KIBI;
   begin
      if Bytes < KIBI then
         return Image (Bytes) & " B";
      end if;
      for U in UNITS'Range loop
         if Bytes / Scale < KIBI or else U = UNITS'Last then
            declare
               Whole : constant Unsigned_64 := Bytes / Scale;
               Tenth : constant Unsigned_64 := (Bytes mod Scale) * TENTHS / Scale;
            begin
               return (if Whole < TENTHS * TENTHS then Image (Whole) & "." & Image (Tenth) else Image (Whole))
                 & " " & UNITS (U);
            end;
         end if;
         Scale := Scale * KIBI;
      end loop;
      return Image (Bytes);
   end Size_Text;

   --  Milliseconds since the Unix epoch as "YYYY-MM-DD HH:MM" (UTC), by
   --  the days-to-civil algorithm (proleptic Gregorian).
   function Time_Text (Ms : Unsigned_64) return String is
      DAYS_PER_ERA : constant := 146_097;
      EPOCH_SHIFT : constant := 719_468;   --  1970-03-01 counted from 0000-03-01
      Days : constant Long_Long_Integer := Long_Long_Integer (Ms / MILLISECONDS_PER_DAY) + EPOCH_SHIFT;
      Era : constant Long_Long_Integer := Days / DAYS_PER_ERA;
      Day_Of_Era : constant Long_Long_Integer := Days - Era * DAYS_PER_ERA;
      Year_Of_Era : constant Long_Long_Integer :=
        (Day_Of_Era - Day_Of_Era / 1_460 + Day_Of_Era / 36_524 - Day_Of_Era / 146_096) / 365;
      Day_Of_Year : constant Long_Long_Integer :=
        Day_Of_Era - (365 * Year_Of_Era + Year_Of_Era / 4 - Year_Of_Era / 100);
      Shifted_Month : constant Long_Long_Integer := (5 * Day_Of_Year + 2) / 153;
      Day : constant Long_Long_Integer := Day_Of_Year - (153 * Shifted_Month + 2) / 5 + 1;
      Month : constant Long_Long_Integer := (if Shifted_Month < 10 then Shifted_Month + 3 else Shifted_Month - 9);
      Year : constant Long_Long_Integer := Year_Of_Era + Era * 400 + (if Month <= 2 then 1 else 0);
      Minutes : constant Unsigned_64 := (Ms mod MILLISECONDS_PER_DAY) / MILLISECONDS_PER_MINUTE;
      function Two (Value : Long_Long_Integer) return String is
         Raw : constant String := Long_Long_Integer'Image (100 + Value mod 100);
      begin
         return Raw (Raw'Last - 1 .. Raw'Last);
      end Two;
      Year_Text : constant String := Long_Long_Integer'Image (Year);
   begin
      return Year_Text (Year_Text'First + 1 .. Year_Text'Last) & "-" & Two (Month) & "-" & Two (Day) & " "
        & Two (Long_Long_Integer (Minutes / MINUTES_PER_HOUR)) & ":"
        & Two (Long_Long_Integer (Minutes mod MINUTES_PER_HOUR));
   end Time_Text;

   ---------------------------------------------------------------------------
   --  Rows: the optional ".." row, then the shown entries.
   ---------------------------------------------------------------------------
   function Full_Path (P : Pane_State) return String is (DP.Value (P.Where));
   function At_Root (P : Pane_State) return Boolean is (Full_Path (P)'Length <= P.Root_Length);
   function Parent_Rows (P : Pane_State) return Natural is
     (if At_Root (P) or else Files_Filter.Active (P.Filter) then 0 else 1);
   function Shown_Count (P : Pane_State) return Entry_Count is
     (if Files_Filter.Active (P.Filter) then Files_Filter.Count (P.Filter) else Files_Order.Count (P.Order));
   function Shown_At (P : Pane_State; Position : Entry_Id) return Entry_Id is
     (if Files_Filter.Active (P.Filter) then Files_Filter.At_Position (P.Filter, Position)
      else Files_Order.At_Position (P.Order, Position));
   function Row_Total (P : Pane_State) return Row_Count is (Shown_Count (P) + Parent_Rows (P));
   --  The entry on Row, 0 for "..".
   function Entry_Of (P : Pane_State; Row : Row_Index) return Entry_Count is
     (if Row <= Parent_Rows (P) or else Row - Parent_Rows (P) > Shown_Count (P) then 0
      else Shown_At (P, Row - Parent_Rows (P)));

   procedure Dirty (State : in out View_State; Area : Rect) is
   begin
      if not Is_Empty (Area) then
         State.Dirty := (if Is_Empty (State.Dirty) then Area else Union_Rect (State.Dirty, Area));
      end if;
   end Dirty;

   procedure Dirty_All (State : in out View_State) is
   begin
      Dirty (State, State.Bounds);
   end Dirty_All;

   --  The cursor's entry, as the order and filter should follow it.
   procedure Remember_Cursor (P : in out Pane_State) is
      Row : constant Row_Count := P.View.Cursor;
   begin
      if Row = 0 then
         P.Cursor := On_First;
      elsif Row <= Parent_Rows (P) then
         P.Cursor := On_Parent_Row;
      else
         P.Cursor := On_Entry;
         P.Cursor_Id := Entry_Of (P, Row);
         if Files_Filter.Active (P.Filter) then
            Files_Filter.Track (P.Filter, P.Cursor_Id, Row - Parent_Rows (P));
            Files_Order.Track (P.Order, P.Cursor_Id, 0);
         else
            Files_Order.Track (P.Order, P.Cursor_Id, Row - Parent_Rows (P));
         end if;
      end if;
   end Remember_Cursor;

   --  After the rows changed: the cursor back on its entry.
   procedure Sync (P : in out Pane_State) is
      Total : constant Row_Count := Row_Total (P);
      Row : Row_Count := (if Total > 0 then 1 else 0);
   begin
      if P.Cursor = On_Entry then
         declare
            Position : Entry_Count :=
              (if Files_Filter.Active (P.Filter) then Files_Filter.Tracked_Position (P.Filter)
               else Files_Order.Tracked_Position (P.Order));
         begin
            if Position = 0 or else Position > Shown_Count (P) or else Shown_At (P, Position) /= P.Cursor_Id then
               Position := 0;
               if Files_Filter.Active (P.Filter) then
                  for K in 1 .. Shown_Count (P) loop
                     if Shown_At (P, K) = P.Cursor_Id then
                        Position := K;
                        exit;
                     end if;
                  end loop;
               else
                  Position := Files_Order.Position_Of (P.Order, P.Cursor_Id);
               end if;
            end if;
            if Position > 0 then
               Row := Position + Parent_Rows (P);
               if Files_Filter.Active (P.Filter) then
                  Files_Filter.Track (P.Filter, P.Cursor_Id, Position);
               else
                  Files_Order.Track (P.Order, P.Cursor_Id, Position);
               end if;
            elsif not Files_Filter.Active (P.Filter) then
               --  Not published yet: hold the place.
               Row := Natural'Min (P.View.Cursor, Total);
            end if;
         end;
      elsif P.Cursor = On_Parent_Row then
         Row := (if Total > 0 then 1 else 0);
      end if;
      VP.Place (P.View, Total, Row);
      --  A filter's first match is the cursor's entry from now on: clearing
      --  the filter leaves the cursor on it.
      if P.Cursor = On_First and then Files_Filter.Active (P.Filter) and then P.View.Cursor > 0 then
         Remember_Cursor (P);
      end if;
      P.Seen_Order := Files_Order.Revision (P.Order);
      P.Seen_Filter := Files_Filter.Revision (P.Filter);
   end Sync;

   ---------------------------------------------------------------------------
   --  I/O.
   ---------------------------------------------------------------------------
   function Pane_Base (Which : Side) return Unsigned_64 is (Unsigned_64 (Which - 1) * PANE_ARENA_BYTES);
   function Slot_Base (Which : Side; Slot : Positive) return Unsigned_64 is
     (Pane_Base (Which) + PATH_AREA_BYTES + Unsigned_64 (Slot - 1) * SLOT_BYTES);

   procedure Submit (Request : FQ.Request; Slot : in out Slot_State; Generation : Natural; Sent : out Boolean) is
      Tag : Files_Queue.Token;
   begin
      Sent := Files_Queue.Ready and then Files_Queue.Can_Submit;
      if Sent then
         Files_Queue.Submit (Request, Tag);
         Slot := (Busy => True, Tag => Tag, Generation => Generation, Wanted => False);
      end if;
   end Submit;

   --  Fire and forget: the answer is reaped and dropped.
   procedure Close_Handle (Handle : Unsigned_64) is
      Tag : Files_Queue.Token;
   begin
      if Handle /= 0 and then Files_Queue.Ready and then Files_Queue.Can_Submit then
         Files_Queue.Submit ((Operation => FQ.Queue_Close_Directory, Handle => Handle, others => <>), Tag);
      end if;
   end Close_Handle;

   --  Fire and forget: end a watch (its Watch_Ended record is read and
   --  matches no pane).
   procedure Unwatch (Number : Natural) is
      Tag : Files_Queue.Token;
   begin
      if Number /= 0 and then Files_Queue.Ready and then Files_Queue.Can_Submit then
         Files_Queue.Submit ((Operation => FQ.Queue_Unwatch, Handle => Unsigned_64 (Number), others => <>), Tag);
      end if;
   end Unwatch;

   procedure Request_Reads (Which : Side; P : in out Pane_State) is
      Sent : Boolean;
   begin
      if P.Load /= Reading or else P.Ended then
         return;
      end if;
      for S in P.Reads'Range loop
         if P.Reads (S).Wanted and then not P.Reads (S).Busy then
            Submit ((Operation => FQ.Queue_Read_Directory, Options => FQ.Directory_Metadata,
                     Handle => P.Handle,
                     Length => SLOT_BYTES, Arena_Offset => Slot_Base (Which, S), others => <>),
                    P.Reads (S), P.Generation, Sent);
            if not Sent then
               P.Reads (S).Wanted := True;
            end if;
         end if;
      end loop;
   end Request_Reads;

   procedure Start_Listing (Which : Side; P : in out Pane_State; Wanted : String := "") is
      Text : constant String := Full_Path (P);
      Sent : Boolean;
   begin
      if P.Load = Reading then
         --  Answers still in flight are dropped as they arrive.
         Close_Handle (P.Handle);
      end if;
      --  A watch stays while the folder does (a refresh); another folder
      --  gets its own.
      if P.Watch /= 0 and then DP.Value (P.Watched) /= Text then
         Unwatch (P.Watch);
         P.Watch := 0;
      end if;
      P.Stale := False;
      P.Generation := P.Generation + 1;
      P.Handle := 0;
      Files_Listing.Clear (P.Listing);
      Files_Order.Reset (P.Order);
      Files_Filter.Reset (P.Filter);
      Files_Marks.Clear (P.Marks);
      P.Pages := (others => <>);
      P.Ended := False;
      P.Truncated := False;
      P.Malformed := False;
      P.Error := 0;
      P.Cursor := On_First;
      P.View := (others => <>);
      P.Wanted_Length := Natural'Min (Wanted'Length, MAXIMUM_NAME_BYTES);
      P.Wanted (1 .. P.Wanted_Length) := Wanted (Wanted'First .. Wanted'First + P.Wanted_Length - 1);
      for S of P.Reads loop
         S.Wanted := False;
      end loop;
      P.Started_Us := Now_Us;
      P.Finished_Us := 0;
      if not Files_Queue.Ready then
         P.Load := Failed;
         P.Error := FS.REPLY_ERR;
         return;
      end if;
      declare
         Bytes : Files_Listing.Name_Bytes (1 .. Text'Length);
      begin
         for K in Bytes'Range loop
            Bytes (K) := Character'Pos (Text (Text'First + K - 1));
         end loop;
         Files_Queue.Write_Arena (Pane_Base (Which), Bytes);
      end;
      Submit ((Operation => FQ.Queue_Open_Directory, Length => Text'Length,
               Arena_Offset => Pane_Base (Which), others => <>), P.Open, P.Generation, Sent);
      P.Load := (if Sent then Opening else Failed);
   end Start_Listing;

   --  A batch of pages arrived in Slot: decode it, then ask for more.
   procedure Take_Read (Which : Side; P : in out Pane_State; Slot : Positive; Pages : Unsigned_64) is
      Page : Files_Pages.Page_Image;
      Result : Files_Pages.Page_Result;
      Before : constant Entry_Count := P.Listing.Count;
   begin
      for Index in 0 .. Unsigned_64'Min (Pages, PAGES_PER_READ) - 1 loop
         exit when P.Ended;
         Files_Queue.Read_Arena (Slot_Base (Which, Slot) + Index * Files_Pages.PAGE_BYTES, Page);
         Files_Pages.Take_Page (P.Listing, Page, P.Pages, Result);
         case Result is
            when Files_Pages.Page_Taken => null;
            when Files_Pages.Page_Last => P.Ended := True;
            when Files_Pages.Page_Malformed =>
               P.Ended := True;
               P.Malformed := True;
            when Files_Pages.Listing_Full =>
               P.Ended := True;
               P.Truncated := True;
         end case;
      end loop;
      if Pages = 0 then
         P.Ended := True;
      end if;
      --  The name to land on, among what just arrived.
      if P.Wanted_Length > 0 then
         for Id in Before + 1 .. P.Listing.Count loop
            if Files_Listing.Name (P.Listing, Id) = P.Wanted (1 .. P.Wanted_Length) then
               P.Cursor := On_Entry;
               P.Cursor_Id := Id;
               Files_Order.Track (P.Order, Id, 0);
               P.Wanted_Length := 0;
               exit;
            end if;
         end loop;
      end if;
      if not P.Ended then
         P.Reads (Slot).Wanted := True;
         Request_Reads (Which, P);
      end if;
   end Take_Read;

   procedure Finish_If_Done (P : in out Pane_State) is
   begin
      if P.Load = Reading and then P.Ended and then (for all S of P.Reads => not S.Busy) then
         Close_Handle (P.Handle);
         P.Handle := 0;
         P.Load := (if P.Malformed then Failed else Loaded);
         P.Restoring := False;
         if P.Malformed then
            P.Error := FS.REPLY_MALFORMED_FILESYSTEM;
         end if;
         P.Listing.Complete := True;
         P.Finished_Us := Now_Us;
      end if;
   end Finish_If_Done;

   --  Why a failed pane failed, as Files_Status words.
   function Pane_Failure (P : Pane_State) return Files_Status.Status_Kind is
     (Files_Status.Listing_Kind (P.Error, At_Root (P)));

   --  The place a pane is in ("@scratch:0/").
   function Root_Of (P : Pane_State) return String is
     (Full_Path (P) (Full_Path (P)'First .. Full_Path (P)'First + Natural'Min (P.Root_Length, Full_Path (P)'Length) - 1));
   --  Its volume's space, once described.
   function Volume_Text (Root : String) return String is
     (if Volume_Known and then DP.Value (Volume_Of) = Root then
        (if Volume_Read_Only then "read-only volume"
         else Size_Text (Volume_Free) & " free of " & Size_Text (Volume_Total))
      else "");

   procedure Remember_Recent (Where : DP.Path);
   function Root_Length_Of (Text : String) return Natural;

   procedure Insert_Place (State : in out View_State; At_Index : Positive; Caption, Path : String;
                           Picture : CuBit.UI.Icons.Icon);

   function Picture_Of (Kind : Files_Places.Place_Kind) return CuBit.UI.Icons.Icon is
     (case Kind is
         when Files_Places.Live_Workspace => CuBit.UI.Icons.Folder,
         when Files_Places.Boot_Image => CuBit.UI.Icons.Archive_File,
         when Files_Places.NVMe_Volume | Files_Places.ATA_Volume | Files_Places.Optical_Disc
            | Files_Places.Other_Volume => CuBit.UI.Icons.Drive);

   --  Pane Which starts at the first place (it had no folder, or the one it
   --  showed last cannot be opened); with no place at all it says so.
   procedure Start_At_First_Place (State : in out View_State; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      P.Restoring := False;
      if Has_First_Place then
         P.Where := First_Place;
         P.Root_Length := Root_Length_Of (DP.Value (P.Where));
         Start_Listing (Which, P);
         Remember_Recent (P.Where);
      else
         P.Where := Empty_Path;
         P.Root_Length := 0;
         P.Load := Failed;
         P.Error := FS.REPLY_VOLUME_UNAVAILABLE;
      end if;
      Dirty_All (State);
   end Start_At_First_Place;

   --  The places arrived (granted scopes whose volume is present): each
   --  goes into the drawer ahead of the platform's own shortcuts, and panes
   --  without a folder start at the first.
   procedure Take_Places (State : in out View_State; Answer : Files_Queue.Answer) is
      Count : constant Natural :=
        (if Answer.Status = FS.REPLY_OK then Natural (Unsigned_64'Min (Answer.Value, FA.Maximum_Entries)) else 0);
      Bytes : Files_Listing.Name_Bytes (1 .. Natural'Max (1, Count * FA.Wire_Entry_Bytes));
      Wire : FA.Wire_Bytes (1 .. Natural'Max (1, Count * FA.Wire_Entry_Bytes));
      Policy : FA.Policy;
      Decoded : Boolean := False;
      Found : Natural := 0;
   begin
      Places_Known := True;
      Has_First_Place := False;
      if Count > 0 then
         --  Copied, then checked: the service is not trusted.
         Files_Queue.Read_Arena (PLACES_BASE, Bytes);
         for K in Wire'Range loop
            Wire (K) := Bytes (K);
         end loop;
         FA.Decode (Wire, Policy, Decoded);
      end if;
      if Decoded then
         for N in 1 .. Count loop
            declare
               Base : constant Positive := (N - 1) * FA.Wire_Entry_Bytes + 1;
               Length : constant Natural :=
                 Natural'Min (Natural (Wire (Base + CuBit.Filesystem_Places.Prefix_Length_At))
                              + CuBit.Filesystem_Places.Byte_Radix
                                * Natural (Wire (Base + CuBit.Filesystem_Places.Prefix_Length_At + 1)),
                              FA.Maximum_Prefix_Bytes);
               Prefix : String (1 .. Length);
               Where : DP.Path;
               Done : Boolean;
            begin
               for K in Prefix'Range loop
                  Prefix (K) := Character'Val (Wire (Base + CuBit.Filesystem_Places.Prefix_At + K - 1));
               end loop;
               if Length > 0 then
                  DP.Set_Root (Prefix, Where, Done);
                  if Done then
                     Found := Found + 1;
                     Insert_Place (State, Found, Files_Places.Caption (Prefix), Prefix,
                                   Picture_Of (Files_Places.Kind_Of (Prefix)));
                     if not Has_First_Place then
                        Has_First_Place := True;
                        First_Place := Where;
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end if;
      for W in Side range 1 .. State.Count loop
         if State.Panes (W).Load = Not_Started then
            Start_At_First_Place (State, W);
         end if;
      end loop;
      Dirty_All (State);
   end Take_Places;

   --  The active pane's volume: free and total space, for the status line.
   procedure Take_Volume (Answer : Files_Queue.Answer) is
      Bytes : Files_Listing.Name_Bytes (1 .. VD.Record_Bytes);
      Image : VD.Record_Image;
      Item : VD.Description;
      OK : Boolean := False;
   begin
      Volume_Of := Volume_Asked;
      if Answer.Status = FS.REPLY_OK and then Answer.Value = VD.Record_Bytes then
         Files_Queue.Read_Arena (VOLUME_BASE, Bytes);
         for K in Image'Range loop
            Image (K) := Bytes (K + 1);
         end loop;
         VD.Decode (Image, Item, OK);
      end if;
      Volume_Known := OK;
      if OK then
         Volume_Read_Only := (Item.Flags and VD.Read_Only) /= 0;
         Volume_Free := Item.Free_Blocks * Unsigned_64 (Item.Block);
         Volume_Total := Item.Total_Blocks * Unsigned_64 (Item.Block);
      end if;
   end Take_Volume;

   procedure Dispatch (State : in out View_State; Answer : Files_Queue.Answer) is
      Owned : Boolean;
   begin
      Files_Reader.Take (Answer, Owned);
      if Owned then
         return;
      end if;
      Files_Operations.Take (Answer, Owned);
      if Owned then
         return;
      end if;
      if Places_Slot.Busy and then Places_Slot.Tag = Answer.Tag then
         Places_Slot.Busy := False;
         Take_Places (State, Answer);
         return;
      end if;
      if Volume_Slot.Busy and then Volume_Slot.Tag = Answer.Tag then
         Volume_Slot.Busy := False;
         Take_Volume (Answer);
         return;
      end if;
      for Which in Side range 1 .. State.Count loop
         declare
            P : Pane_State renames State.Panes (Which).all;
         begin
            if P.Watch_Slot.Busy and then P.Watch_Slot.Tag = Answer.Tag then
               P.Watch_Slot.Busy := False;
               if Answer.Status = FS.REPLY_OK and then Answer.Value in 1 .. FE.Maximum_Watches then
                  if P.Watch = 0 and then DP.Value (P.Watch_Asked) = Full_Path (P) then
                     P.Watch := Natural (Answer.Value);
                     P.Watched := P.Watch_Asked;
                  else
                     --  The pane moved on meanwhile.
                     Unwatch (Natural (Answer.Value));
                  end if;
               end if;
               return;
            end if;
            if P.Open.Busy and then P.Open.Tag = Answer.Tag then
               P.Open.Busy := False;
               if P.Open.Generation = P.Generation and then P.Load = Opening then
                  if Answer.Status = FS.REPLY_OK then
                     P.Handle := Answer.Value;
                     P.Load := Reading;
                     for S of P.Reads loop
                        S.Wanted := True;
                     end loop;
                     Request_Reads (Which, P);
                     --  Watch the folder while it is shown (before the handle
                     --  closes; the watch outlives it).
                     if Files_Queue.Events_Open and then P.Watch = 0 and then not P.Watch_Slot.Busy then
                        declare
                           Sent, Done : Boolean;
                        begin
                           Submit ((Operation => FQ.Queue_Watch, Handle => P.Handle, others => <>),
                                   P.Watch_Slot, P.Generation, Sent);
                           if Sent then
                              DP.Set_Root (Full_Path (P), P.Watch_Asked, Done);
                           end if;
                        end;
                     end if;
                  elsif P.Restoring then
                     --  The folder it showed last is gone (or its volume):
                     --  the first place instead, once the places are known.
                     P.Restoring := False;
                     if Places_Known then
                        Start_At_First_Place (State, Which);
                     else
                        P.Load := Not_Started;
                     end if;
                  else
                     P.Load := Failed;
                     P.Error := Answer.Status;
                  end if;
                  Dirty_All (State);
               elsif Answer.Status = FS.REPLY_OK then
                  --  The person moved on before it opened.
                  Close_Handle (Answer.Value);
               end if;
               return;
            end if;
            for S in P.Reads'Range loop
               if P.Reads (S).Busy and then P.Reads (S).Tag = Answer.Tag then
                  P.Reads (S).Busy := False;
                  if P.Reads (S).Generation = P.Generation and then P.Load = Reading then
                     if Answer.Status = FS.REPLY_OK then
                        Take_Read (Which, P, S, Answer.Value);
                     else
                        P.Ended := True;
                        P.Malformed := True;
                     end if;
                     Finish_If_Done (P);
                  else
                     --  A stale slot is free again: this listing may use it.
                     Request_Reads (Which, P);
                  end if;
                  return;
               end if;
            end loop;
         end;
      end loop;
   end Dispatch;

   ---------------------------------------------------------------------------
   --  Navigation.
   ---------------------------------------------------------------------------
   --  The place's root: everything through the first '/'.
   function Root_Length_Of (Text : String) return Natural is
   begin
      for K in Text'Range loop
         if Text (K) = '/' then
            return K - Text'First + 1;
         end if;
      end loop;
      return Text'Length;
   end Root_Length_Of;

   procedure Push (Table : in out History_Table; Count : in out History_Count; Item : History_Entry) is
   begin
      if Count = HISTORY_DEPTH then
         Table (1 .. HISTORY_DEPTH - 1) := Table (2 .. HISTORY_DEPTH);
         Count := Count - 1;
      end if;
      Count := Count + 1;
      Table (Count) := Item;
   end Push;

   function Here (P : Pane_State) return History_Entry is
      Result : History_Entry := (Where => P.Where, others => <>);
   begin
      if P.Cursor = On_Entry then
         declare
            Name : constant String := Files_Listing.Name (P.Listing, P.Cursor_Id);
         begin
            Result.Length := Name'Length;
            Result.Name (1 .. Name'Length) := Name;
         end;
      end if;
      return Result;
   end Here;

   function Caption_Of (Where : DP.Path) return String is
      Text : constant String := DP.Value (Where);
      Start : Natural := Text'First;
   begin
      --  The last component, or the place itself at its root.
      for K in reverse Text'First .. Text'Last - 1 loop
         if Text (K) = '/' then
            Start := K + 1;
            exit;
         end if;
      end loop;
      return Text (Start .. Text'Last);
   end Caption_Of;

   function Make_Place (Caption : String; Where : DP.Path; Picture : CuBit.UI.Icons.Icon) return Place_Entry is
      Result : Place_Entry := (Where => Where, Picture => Picture, others => <>);
   begin
      Result.Caption_Length := Natural'Min (Caption'Length, CuBit.UI.Drawers.MAXIMUM_CAPTION);
      Result.Caption (1 .. Result.Caption_Length) := Caption (Caption'First .. Caption'First + Result.Caption_Length - 1);
      return Result;
   end Make_Place;

   procedure Rebuild_Shortcuts is
      use CuBit.UI.Drawers;
   begin
      Clear (Shortcuts);
      Add_Section (Shortcuts, "Places");
      for K in 1 .. Place_Count loop
         Add_Shortcut (Shortcuts, Places (K).Caption (1 .. Places (K).Caption_Length), Places (K).Picture,
                       PLACE_VALUE + K);
      end loop;
      Add_Section (Shortcuts, "Bookmarks" & (if Bookmark_Count = 0 and then Key_Text (Panes_Map, Bookmark_Folder)'Length > 0
                                             then "  (" & Key_Text (Panes_Map, Bookmark_Folder) & ")" else ""));
      for K in 1 .. Bookmark_Count loop
         Add_Shortcut (Shortcuts, Bookmarks (K).Caption (1 .. Bookmarks (K).Caption_Length), CuBit.UI.Icons.Favorites,
                       BOOKMARK_VALUE + K, Pinned => True);
      end loop;
      Add_Section (Shortcuts, "Recent");
      for K in 1 .. Recent_Count loop
         Add_Shortcut (Shortcuts, Recent (K).Caption (1 .. Recent (K).Caption_Length), CuBit.UI.Icons.Recent,
                       RECENT_VALUE + K);
      end loop;
   end Rebuild_Shortcuts;

   --  The folder first among the recent ones.
   procedure Remember_Recent (Where : DP.Path) is
      Text : constant String := DP.Value (Where);
      Found : Natural := 0;
   begin
      for K in 1 .. Recent_Count loop
         if DP.Value (Recent (K).Where) = Text then
            Found := K;
            exit;
         end if;
      end loop;
      if Found = 0 then
         Found := (if Recent_Count < MAXIMUM_RECENT then Recent_Count + 1 else MAXIMUM_RECENT);
         Recent_Count := Found;
      end if;
      for K in reverse 2 .. Found loop
         Recent (K) := Recent (K - 1);
      end loop;
      Recent (1) := Make_Place (Caption_Of (Where), Where, CuBit.UI.Icons.Recent);
      Rebuild_Shortcuts;
   end Remember_Recent;

   --  Bookmark Where, or remove it when it is one already.
   procedure Toggle_Bookmark (Where : DP.Path) is
      Text : constant String := DP.Value (Where);
   begin
      for K in 1 .. Bookmark_Count loop
         if DP.Value (Bookmarks (K).Where) = Text then
            Bookmarks (K .. Bookmark_Count - 1) := Bookmarks (K + 1 .. Bookmark_Count);
            Bookmark_Count := Bookmark_Count - 1;
            Say ("Bookmark removed: " & Text);
            Rebuild_Shortcuts;
            return;
         end if;
      end loop;
      if Bookmark_Count = MAXIMUM_BOOKMARKS then
         Say ("Bookmarks are full (" & Natural'Image (MAXIMUM_BOOKMARKS) & " ); remove one first.");
         return;
      end if;
      Bookmark_Count := Bookmark_Count + 1;
      Bookmarks (Bookmark_Count) := Make_Place (Caption_Of (Where), Where, CuBit.UI.Icons.Favorites);
      Say ("Bookmarked " & Text);
      Rebuild_Shortcuts;
   end Toggle_Bookmark;

   procedure Go_To (State : in out View_State; Which : Side; Where : DP.Path; Land_On : String;
                    Remember : Boolean := True) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      if Remember then
         Push (P.Back, P.Back_Count, Here (P));
         P.Forward_Count := 0;
      end if;
      P.Where := Where;
      P.Root_Length := Root_Length_Of (DP.Value (Where));
      P.Restoring := False;
      Start_Listing (Which, P, Land_On);
      Remember_Recent (Where);
      Dirty_All (State);
   end Go_To;

   procedure Go_Parent (State : in out View_State; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
      Text : constant String := Full_Path (P);
      Cut : Natural := 0;
      Parent : DP.Path;
      Done : Boolean;
   begin
      if At_Root (P) then
         return;
      end if;
      for K in reverse Text'First + P.Root_Length .. Text'Last loop
         if Text (K) = '/' then
            Cut := K;
            exit;
         end if;
      end loop;
      if Cut = 0 then
         --  A child of the root: the root keeps its '/'.
         DP.Set_Root (Text (Text'First .. Text'First + P.Root_Length - 1), Parent, Done);
         Go_To (State, Which, Parent, Text (Text'First + P.Root_Length .. Text'Last));
      else
         DP.Set_Root (Text (Text'First .. Cut - 1), Parent, Done);
         Go_To (State, Which, Parent, Text (Cut + 1 .. Text'Last));
      end if;
   end Go_Parent;

   --  The cursor's file in the quick viewer, read without waiting.
   procedure Open_Viewer (State : in out View_State; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
      Row : constant Row_Count := P.View.Cursor;
      Id : constant Entry_Count := (if Row = 0 then 0 else Entry_Of (P, Row));
      Child : DP.Path;
      Done : Boolean;
   begin
      if Id = 0 or else Files_Listing.Kind (P.Listing, Id) = Files_Listing.Directory_Kind then
         return;
      end if;
      declare
         Name : constant String := Files_Listing.Name (P.Listing, Id);
      begin
         DP.Append_Child (P.Where, Name, Child, Done);
         if not Done then
            Say ("That path would be longer than" & Natural'Image (DP.Maximum_Bytes) & " bytes.");
            Dirty (State, Status_Area);
            return;
         end if;
         Viewer_Name_Length := Name'Length;
         Viewer_Name (1 .. Name'Length) := Name;
      end;
      Viewing := True;
      Viewer_Top := 0;
      Viewer_Hex := False;
      Line_Count := 0;
      Files_Reader.Start (DP.Value (Child), READER_BASE, Now_Us, Now_Us + VIEW_DEADLINE_US);
      Files_Queue.Flush;
      Dirty_All (State);
   end Open_Viewer;

   procedure Close_Viewer (State : in out View_State) is
   begin
      Files_Reader.Cancel;
      Viewing := False;
      Dirty_All (State);
   end Close_Viewer;

   --  Where each shown line starts: text lines, or hex rows of 16 bytes.
   procedure Index_Lines is
      Total : constant Natural := Files_Reader.Length;
   begin
      if Line_Starts = null then
         Line_Starts := new Line_Table (1 .. Files_Reader.VIEW_LIMIT + 1);
      end if;
      Line_Count := 0;
      if Viewer_Hex then
         Line_Count := (Total + HEX_BYTES_PER_LINE - 1) / HEX_BYTES_PER_LINE;
         return;
      end if;
      if Total > 0 then
         Line_Count := 1;
         Line_Starts (1) := 1;
         for K in 1 .. Total loop
            if Files_Reader.Byte (K) = Character'Pos (ASCII.LF) and then K < Total then
               Line_Count := Line_Count + 1;
               Line_Starts (Line_Count) := K + 1;
            end if;
         end loop;
      end if;
   end Index_Lines;

   function Looks_Binary return Boolean is
   begin
      for K in 1 .. Natural'Min (Files_Reader.Length, BINARY_PROBE_BYTES) loop
         if Files_Reader.Byte (K) = 0 then
            return True;
         end if;
      end loop;
      return False;
   end Looks_Binary;

   --  Shown line Line (1-based) as text.
   function View_Line (Line : Positive) return String is
      HEX : constant String := "0123456789abcdef";
      NIBBLE : constant := 16;
      Total : constant Natural := Files_Reader.Length;
   begin
      if Viewer_Hex then
         declare
            First : constant Natural := (Line - 1) * HEX_BYTES_PER_LINE + 1;
            Result : String (1 .. 10 + 3 * HEX_BYTES_PER_LINE + 2 + HEX_BYTES_PER_LINE) := [others => ' '];
            Offset : Unsigned_64 := Unsigned_64 (First - 1);
         begin
            for D in reverse 1 .. 8 loop
               Result (D) := HEX (Natural (Offset mod NIBBLE) + 1);
               Offset := Offset / NIBBLE;
            end loop;
            for K in 0 .. HEX_BYTES_PER_LINE - 1 loop
               exit when First + K > Total;
               declare
                  B : constant Unsigned_8 := Files_Reader.Byte (First + K);
               begin
                  Result (11 + 3 * K) := HEX (Natural (B / NIBBLE) + 1);
                  Result (12 + 3 * K) := HEX (Natural (B mod NIBBLE) + 1);
                  Result (11 + 3 * HEX_BYTES_PER_LINE + 1 + K) :=
                    (if B in 32 .. 126 then Character'Val (B) else '.');
               end;
            end loop;
            return Result;
         end;
      end if;
      declare
         First : constant Positive := Line_Starts (Line);
         Last : Natural := (if Line < Line_Count then Line_Starts (Line + 1) - 1 else Total);
      begin
         if Last >= First and then Files_Reader.Byte (Last) = Character'Pos (ASCII.LF) then
            Last := Last - 1;
         end if;
         if Last >= First and then Files_Reader.Byte (Last) = Character'Pos (ASCII.CR) then
            Last := Last - 1;
         end if;
         Last := Natural'Min (Last, First + MAXIMUM_VIEW_COLUMNS - 1);
         declare
            Result : String (1 .. (if Last >= First then Last - First + 1 else 0));
         begin
            for K in Result'Range loop
               declare
                  B : constant Unsigned_8 := Files_Reader.Byte (First + K - 1);
               begin
                  Result (K) :=
                    (if B = Character'Pos (ASCII.HT) then ' ' elsif B in 32 .. 126 then Character'Val (B) else '.');
               end;
            end loop;
            return Result;
         end;
      end;
   end View_Line;

   procedure Open_Cursor (State : in out View_State; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
      Row : constant Row_Count := P.View.Cursor;
   begin
      if Row = 0 then
         return;
      elsif Row <= Parent_Rows (P) then
         Go_Parent (State, Which);
      elsif Files_Listing.Kind (P.Listing, Entry_Of (P, Row)) = Files_Listing.Directory_Kind then
         declare
            Child : DP.Path;
            Done : Boolean;
         begin
            DP.Append_Child (P.Where, Files_Listing.Name (P.Listing, Entry_Of (P, Row)), Child, Done);
            if Done then
               Go_To (State, Which, Child, "");
            else
               Say ("That path would be longer than" & Natural'Image (DP.Maximum_Bytes) & " bytes.");
               Dirty (State, Status_Area);
            end if;
         end;
      else
         Open_Viewer (State, Which);
      end if;
   end Open_Cursor;

   procedure History_Move (State : in out View_State; Which : Side; Backward : Boolean) is
      P : Pane_State renames State.Panes (Which).all;
      Item : History_Entry;
   begin
      if Backward and then P.Back_Count > 0 then
         Item := P.Back (P.Back_Count);
         P.Back_Count := P.Back_Count - 1;
         Push (P.Forward, P.Forward_Count, Here (P));
      elsif not Backward and then P.Forward_Count > 0 then
         Item := P.Forward (P.Forward_Count);
         P.Forward_Count := P.Forward_Count - 1;
         Push (P.Back, P.Back_Count, Here (P));
      else
         return;
      end if;
      Go_To (State, Which, Item.Where, Item.Name (1 .. Item.Length), Remember => False);
   end History_Move;

   procedure Refresh (State : in out View_State; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
      Item : constant History_Entry := Here (P);
   begin
      Start_Listing (Which, P, Item.Name (1 .. Item.Length));
      Dirty_All (State);
   end Refresh;

   ---------------------------------------------------------------------------
   --  Lifetime.
   ---------------------------------------------------------------------------
   Pane_Capacity : Entry_Capacity := 1;
   Pane_Name_Bytes : Arena_Capacity := 1;

   --  Column Column sized to its widest text and title (Name flexes).
   procedure Fit_Column (P : in out Pane_State; Column : Tables.Column_Index) is
      Kind : constant Column_Kind := P.Kinds (Column);
      Fitted : constant Natural :=
        Tables.Fitted_Width
          (P.Columns, Natural'Max (UI_Text_Width (COLUMN_CAPTIONS (Kind).all),
                                   UI_Text_Width (COLUMN_SAMPLES (Kind).all)));
   begin
      P.Columns.Width (Column) := Fitted;
      P.Columns.Minimum (Column) := (if Kind = Name_Column then NAME_COLUMN_MINIMUM else Fitted / 2);
   end Fit_Column;

   --  A new pane at Which, listing Path, with the default columns.
   procedure New_Pane (State : in out View_State; Which : Side; Path : String) is
      Done : Boolean;
   begin
      if State.Panes (Which) = null then
         State.Panes (Which) := new Pane_State (Pane_Capacity, Pane_Name_Bytes);
      end if;
      declare
         P : Pane_State renames State.Panes (Which).all;
      begin
         Files_Order.Reset (P.Order);
         Files_Filter.Reset (P.Filter);
         P.Kinds := [Name_Column, Size_Column, Modified_Column, others => Name_Column];
         P.Columns :=
           (Count => 3, Flexible => 1, Sortable => True, Sort_Column => 1, Order => Tables.Ascending,
            Cell_Padding => 5, others => <>);
         for Column in 1 .. P.Columns.Count loop
            Fit_Column (P, Column);
         end loop;
         P.Back_Count := 0;
         P.Forward_Count := 0;
         DP.Set_Root (Path, P.Where, Done);
         P.Root_Length := Root_Length_Of (Path);
         P.Tab_Count := 1;
         P.Current_Tab := 1;
         P.Watch := 0;
         P.Watch_Slot := (others => <>);
         P.Stale := False;
         P.Restoring := False;
         if Path'Length > 0 then
            Start_Listing (Which, P);
         else
            --  Its folder comes with the places.
            P.Load := Not_Started;
         end if;
      end;
   end New_Pane;

   --  The keymaps from the defaults (Files_Commands; the user's remaps
   --  go over them), the clipboard's chords reserved for the toolkit, and
   --  the bars' buttons.
   procedure Load_Keys is
      procedure Reserve_Clipboard is new CuBit.UI.Standard_Keys.Reserve_Clipboard (Maps);
      Refused, Clipboard_Refused : Natural;
      Total : Natural := 0;
   begin
      Maps.Load (Panes_Map, Files_Commands.Panes, Refused);
      Total := Total + Refused;
      Reserve_Clipboard (Panes_Map, Clipboard_Refused);
      Total := Total + Clipboard_Refused;
      Maps.Load (Viewer_Map, Files_Commands.Viewer, Refused);
      Total := Total + Refused;
      Maps.Load (Prompt_Map, Files_Commands.Prompt, Refused);
      Total := Total + Refused;
      if Total > 0 then
         Say ("Files: " & Natural'Image (Total) & " default keys conflict.");
      end if;
      Bars.Clear (Key_Bar, FUNCTION_KEY_FIRST, Bars.Key_Bar_Style);
      for K in Keys.Function_Key loop
         Bars.Add_Key (Key_Bar, K, Starts_Cluster => K in Keys.F5 | Keys.F9);
      end loop;
      Bars.Clear (Tool_Bar, TOOL_FIRST, Bars.Toolbar_Style);
      for T in Tool loop
         Bars.Add_Command (Tool_Bar, TOOL_COMMANDS (T), Bars.Icon_Only,
                           Starts_Cluster => T in Tool_New_Folder | Tool_Drawer | Tool_New_Tab);
      end loop;
   end Load_Keys;

   function Hint (State : View_State; Item : Files_Keymaps.Command) return Keys.Chord is (Maps.Hint (Panes_Map, Item));
   procedure Remap
     (State : in out View_State; Item : Files_Keymaps.Command; From, To : Keys.Chord;
      Result : out Files_Keymaps.Map.Bind_Result) is
   begin
      Maps.Remap (Panes_Map, Item, From, To, Result);
      Dirty_All (State);
   end Remap;

   procedure Initialize
     (State : out View_State; Capacity : Entry_Capacity; Name_Bytes : Arena_Capacity;
      Left_Path, Right_Path : String)
   is
      Opened : Boolean;
   begin
      Files_Queue.Open (ARENA_PAGES, Opened);
      Load_Keys;
      State.Current := Left_Pane;
      State.Dirty := (others => 0);
      State.Bounds := (others => 0);
      State.Overlay := False;
      State.Quit := False;
      State.Frames := 0;
      State.Last_Render_Us := 0;
      State.Worst_Render_Us := 0;
      State.Last_Pump_Us := 0;
      State.Last_Work := 0;
      State.Last_Press_Ms := 0;
      State.Last_Press_Row := 0;
      State.Last_Press_Side := Left_Pane;
      Pane_Capacity := Capacity;
      Pane_Name_Bytes := Name_Bytes;
      State.Count := Right_Pane;
      State.Shown := [others => True];
      State.Shares := [others => 1];
      declare
         Events : Boolean;
      begin
         --  Change watches need the event ring (without it, folders show
         --  what they held when listed).
         Files_Queue.Open_Events (Events);
      end;
      Volume_Known := False;
      Volume_Stale := False;
      Volume_Of := Empty_Path;
      Places_Slot := (others => <>);
      Places_Known := False;
      Has_First_Place := False;
      Volume_Slot := (others => <>);
      Place_Count := 0;
      Recent_Count := 0;
      Rebuild_Shortcuts;
      for Which in Side range 1 .. State.Count loop
         New_Pane (State, Which, (if Which = Left_Pane then Left_Path else Right_Path));
         --  A folder from before is shown only if it can still be opened.
         State.Panes (Which).Restoring := State.Panes (Which).Load /= Not_Started;
      end loop;
      declare
         Sent : Boolean;
      begin
         Submit ((Operation => FQ.Queue_List_Places, Length => PLACES_BYTES, Arena_Offset => PLACES_BASE,
                  others => <>), Places_Slot, 0, Sent);
         if not Sent then
            --  No queue: no places either; every pane says so.
            Places_Known := True;
            for Which in Side range 1 .. State.Count loop
               if State.Panes (Which).Load in Not_Started | Failed then
                  Start_At_First_Place (State, Which);
               end if;
            end loop;
         end if;
      end;
      Files_Queue.Flush;
      Message_Length := 0;
      Viewing := False;
      CuBit.UI.Popup_Menus.Close (Popup);
      for Which in Side range 1 .. State.Count loop
         if State.Panes (Which).Load /= Not_Started then
            Remember_Recent (State.Panes (Which).Where);
         end if;
      end loop;
   end Initialize;

   procedure Insert_Place (State : in out View_State; At_Index : Positive; Caption, Path : String;
                           Picture : CuBit.UI.Icons.Icon)
   is
      Where : DP.Path;
      Done : Boolean;
   begin
      DP.Set_Root (Path, Where, Done);
      if Done and then Place_Count < MAXIMUM_PLACES and then At_Index <= Place_Count + 1 then
         Places (At_Index + 1 .. Place_Count + 1) := Places (At_Index .. Place_Count);
         Place_Count := Place_Count + 1;
         Places (At_Index) := Make_Place (Caption, Where, Picture);
         Rebuild_Shortcuts;
         Dirty_All (State);
      end if;
   end Insert_Place;

   procedure Add_Place
     (State : in out View_State; Caption, Path : String; Picture : CuBit.UI.Icons.Icon := CuBit.UI.Icons.Drive)
   is
   begin
      if Place_Count < MAXIMUM_PLACES then
         Insert_Place (State, Place_Count + 1, Caption, Path, Picture);
      end if;
   end Add_Place;

   procedure Free is new Ada.Unchecked_Deallocation (Pane_State, Pane_Access);

   procedure Close (State : in out View_State) is
   begin
      for Which in Side range 1 .. State.Count loop
         if State.Panes (Which) /= null and then State.Panes (Which).Handle /= 0 then
            Close_Handle (State.Panes (Which).Handle);
         end if;
      end loop;
      Files_Queue.Flush;
      Files_Queue.Close;
      for Which in Side range 1 .. State.Count loop
         Free (State.Panes (Which));
      end loop;
   end Close;

   procedure Watch_Operation (State : in out View_State);

   ---------------------------------------------------------------------------
   --  Pumping.
   ---------------------------------------------------------------------------
   procedure Note_Reveal (State : in out View_State);
   function Bar_Map return Maps.Keymap;

   procedure Pump
     (State : in out View_State; Budget : Work_Budget; Now_Us : Unsigned_64; Busy, Changed : out Boolean)
   is
      Answer : Files_Queue.Answer;
      Got : Boolean;
      Work_Left : Work_Budget := Budget;
      Used : Work_Budget;
   begin
      Files_View.Now_Us := Now_Us;
      Changed := False;
      loop
         Files_Queue.Reap (Answer, Got);
         exit when not Got;
         Dispatch (State, Answer);
         Changed := True;
      end loop;
      for Which in Side range 1 .. State.Count loop
         Request_Reads (Which, State.Panes (Which).all);
      end loop;
      Files_Reader.Pump (Now_Us);
      declare
         Tip_Damage : Rect;
      begin
         Tooltips.Tick (Tip, Tips, Now_Us / MICROSECONDS_PER_MILLISECOND, Tip_Damage);
         if not Is_Empty (Tip_Damage) then
            Dirty (State, Tip_Damage);
            Changed := True;
         end if;
         --  A key's press flash ends; held modifiers reveal their hints.
         Bars.Tick (Key_Bar, Now_Ms, Tip_Damage);
         Dirty (State, Tip_Damage);
         Bars.Tick (Tool_Bar, Now_Ms, Tip_Damage);
         Dirty (State, Tip_Damage);
         Note_Reveal (State);
         Changed := Changed or else not Is_Empty (State.Dirty);
      end;
      Files_Operations.Pump (Now_Us);
      Watch_Operation (State);
      if Files_Reader.Revision /= Reader_Seen then
         Reader_Seen := Files_Reader.Revision;
         case Files_Reader.State is
            when Files_Reader.Done =>
               Viewer_Hex := Looks_Binary;
               Index_Lines;
            when Files_Reader.Failed =>
               Say ("Could not read " & Viewer_Name (1 .. Viewer_Name_Length) & ": "
                    & Files_Status.Reason (Files_Status.Kind_Of (Files_Reader.Status)) & ".");
               Viewing := False;
            when Files_Reader.Timed_Out =>
               Say ("Reading " & Viewer_Name (1 .. Viewer_Name_Length) & " took longer than"
                    & Natural'Image (VIEW_DEADLINE_US / 1_000) & " ms; stopped.");
               Viewing := False;
            when Files_Reader.Cancelled =>
               Say ("Reading " & Viewer_Name (1 .. Viewer_Name_Length) & " cancelled.");
            when others => null;
         end case;
         Dirty_All (State);
         Changed := True;
      end if;
      if Message_Length > 0 and then Now_Us > Message_Until_Us then
         Message_Length := 0;
         Dirty (State, Status_Area);
         Changed := True;
      end if;
      --  Changes to the folders shown (their watches' records): a folder
      --  with changes is read again once its listing is complete.
      if Files_Queue.Events_Open then
         declare
            Item : FE.Event;
            Name : FE.Name_Bytes;
            Length : FE.Name_Length;
            Result : Files_Queue.Event_Result;
            use type Files_Queue.Event_Result;
            use type FE.Event_Kind;
         begin
            for Count in 1 .. EVENTS_PER_PUMP loop
               Files_Queue.Take_Event (Item, Name, Length, Result);
               exit when Result = Files_Queue.Empty;
               Changed := True;
               if Result = Files_Queue.Malformed then
                  Say ("The filesystem sent a change record that does not read; the folders are read again.");
                  for W in Side range 1 .. State.Count loop
                     State.Panes (W).Stale := True;
                  end loop;
                  exit;
               end if;
               for W in Side range 1 .. State.Count loop
                  declare
                     P : Pane_State renames State.Panes (W).all;
                  begin
                     if P.Watch /= 0 and then P.Watch = Item.Watch then
                        if Item.Kind = FE.Watch_Ended then
                           P.Watch := 0;
                           Say (Full_Path (P) & " was removed or moved.");
                        end if;
                        P.Stale := True;
                     end if;
                  end;
               end loop;
            end loop;
         end;
      end if;
      for Which in Side range 1 .. State.Count loop
         if State.Panes (Which).Stale and then State.Panes (Which).Load in Loaded | Failed then
            State.Panes (Which).Stale := False;
            Volume_Stale := True;
            Refresh (State, Which);
         end if;
      end loop;
      --  The active pane's volume, when it changes place or after changes.
      declare
         P : Pane_State renames State.Panes (State.Current).all;
         Root : constant String := Root_Of (P);
         Sent, Done : Boolean;
      begin
         if P.Load = Loaded and then Root'Length > 0 and then not Volume_Slot.Busy
           and then (DP.Value (Volume_Of) /= Root or else Volume_Stale)
         then
            Volume_Stale := False;
            declare
               Bytes : Files_Listing.Name_Bytes (1 .. Root'Length);
            begin
               for K in Bytes'Range loop
                  Bytes (K) := Character'Pos (Root (Root'First + K - 1));
               end loop;
               Files_Queue.Write_Arena (VOLUME_BASE, Bytes);
            end;
            Submit ((Operation => FQ.Queue_Describe_Volume, Position => Root'Length,
                     Length => Unsigned_64'Max (Root'Length, VD.Record_Bytes), Arena_Offset => VOLUME_BASE,
                     others => <>), Volume_Slot, 0, Sent);
            if Sent then
               DP.Set_Root (Root, Volume_Asked, Done);
            end if;
         end if;
      end;
      Files_Queue.Flush;
      --  The active pane's work first.
      for Turn in Side range 1 .. State.Count loop
         declare
            --  The active pane's work first, then the rest in turn.
            Which : constant Side := Side ((Natural (State.Current) - 1 + Natural (Turn) - 1) mod Natural (State.Count) + 1);
            P : Pane_State renames State.Panes (Which).all;
         begin
            if Files_Order.Consistent (P.Order) and then Files_Order.Count (P.Order) <= P.Listing.Count then
               Files_Order.Step
                 (P.Order, P.Listing, Work_Left, Settle => P.Load /= Reading or else Files_Queue.Outstanding = 0,
                  Used => Used);
               Work_Left := Work_Left - Used;
            end if;
            Files_Filter.Step (P.Filter, P.Listing, P.Order, Work_Left, Used);
            Work_Left := Work_Left - Used;
            if Files_Order.Revision (P.Order) /= P.Seen_Order or else Files_Filter.Revision (P.Filter) /= P.Seen_Filter
            then
               Sync (P);
               Changed := True;
            end if;
         end;
      end loop;
      State.Last_Work := Unsigned_64 (Budget - Work_Left);
      if Changed then
         Dirty_All (State);
      end if;
      --  Requests out: the service wakes the loop when answers wait
      --  (OP_FS_WAKE); nothing polls. Busy is work for this thread alone.
      Files_Queue.Arm_Wake;
      Busy := False;
      for Which in Side range 1 .. State.Count loop
         Busy := Busy or else Files_Order.Busy (State.Panes (Which).Order, State.Panes (Which).Listing)
           or else not Files_Filter.Complete (State.Panes (Which).Filter);
      end loop;
   end Pump;

   ---------------------------------------------------------------------------
   --  Input.
   ---------------------------------------------------------------------------
   --  The pane operations go to: the next shown pane after Which, else the
   --  next pane.
   function Other (State : View_State; Which : Side) return Side is
      Candidate : Side := Which;
   begin
      for Step in 1 .. State.Count loop
         Candidate := (if Candidate >= State.Count then 1 else Candidate + 1);
         if Candidate /= Which and then State.Shown (Candidate) then
            return Candidate;
         end if;
      end loop;
      return (if Which >= State.Count then 1 else Which + 1);
   end Other;

   function Row_Area (P : Pane_State; Row : Row_Count) return Rect is
     (if Row > 0 and then VP.Shows (P.View, Row)
      then (P.Rows_Area.x, P.Rows_Area.y + (Row - P.View.Top - 1) * ROW_HEIGHT, P.Rows_Area.w, ROW_HEIGHT)
      else (others => 0));

   procedure Set_Mark (P : in out Pane_State; Row : Row_Index; Value : Boolean) is
      Id : constant Entry_Count := Entry_Of (P, Row);
   begin
      if Id > 0 then
         declare
            Facts : constant Files_Listing.Entry_Facts := Files_Listing.Facts (P.Listing, Id);
         begin
            Files_Marks.Set (P.Marks, Id, Value,
                             (if Facts.Kind /= Files_Listing.Directory_Kind and then Facts.Size_Known
                              then Unsigned_64 (Facts.Size) else 0));
         end;
      end if;
   end Set_Mark;

   function Marked_Row (P : Pane_State; Row : Row_Index) return Boolean is
     (Entry_Of (P, Row) > 0 and then Files_Marks.Marked (P.Marks, Entry_Of (P, Row)));

   --  The cursor by By rows; with Extend, the rows passed take the
   --  opposite of the first one's mark (Total Commander's Shift+move).
   procedure Move_Cursor (State : in out View_State; Which : Side; By : Row_Delta; Extend : Boolean) is
      P : Pane_State renames State.Panes (Which).all;
      Old_Row : constant Row_Count := P.View.Cursor;
      Old_Top : constant Row_Count := P.View.Top;
   begin
      if Old_Row = 0 then
         return;
      end if;
      VP.Move (P.View, By);
      --  The viewer tool follows whether the cursor is on a file.
      if (Entry_Of (P, Old_Row) > 0 and then Files_Listing.Kind (P.Listing, Entry_Of (P, Old_Row)) /= Files_Listing.Directory_Kind)
        /= (P.View.Cursor > 0 and then Entry_Of (P, P.View.Cursor) > 0
            and then Files_Listing.Kind (P.Listing, Entry_Of (P, P.View.Cursor)) /= Files_Listing.Directory_Kind)
      then
         Dirty (State, Toolbar_Area);
      end if;
      if Extend and then P.View.Cursor /= Old_Row then
         declare
            Value : constant Boolean := not Marked_Row (P, Old_Row);
            Last : constant Row_Index :=
              (if P.View.Cursor > Old_Row then P.View.Cursor - 1 else P.View.Cursor + 1);
         begin
            for Row in Natural'Min (Old_Row, Last) .. Natural'Max (Old_Row, Last) loop
               Set_Mark (P, Row, Value);
            end loop;
         end;
         Dirty (State, P.Rows_Area);
         Dirty (State, P.Path_Area);
         Dirty (State, Status_Area);
      elsif P.View.Top /= Old_Top then
         Dirty (State, P.Rows_Area);
      else
         --  Two rows, nothing else.
         Dirty (State, Row_Area (P, Old_Row));
         Dirty (State, Row_Area (P, P.View.Cursor));
      end if;
      Remember_Cursor (P);
   end Move_Cursor;

   procedure Open_Cursor_Entry (State : in out View_State; Which : Side) is
   begin
      Open_Cursor (State, Which);
   end Open_Cursor_Entry;

   function Key_Of (Kind : Column_Kind) return Files_Order.Sort_Key is
     (case Kind is
         when Extension_Column => Files_Order.By_Extension, when Size_Column => Files_Order.By_Size,
         when Modified_Column => Files_Order.By_Modified, when others => Files_Order.By_Name);
   --  The shown column a sort key belongs to, 0 if none shows it.
   function Column_Of_Key (P : Pane_State; Key : Files_Order.Sort_Key) return Tables.Column_Count is
   begin
      for Column in 1 .. P.Columns.Count loop
         if COLUMN_KEYS (P.Kinds (Column)) and then Key_Of (P.Kinds (Column)) = Key then
            return Column;
         end if;
      end loop;
      return 0;
   end Column_Of_Key;

   --  Show or hide a column (Name stays).
   procedure Toggle_Column (P : in out Pane_State; Kind : Column_Kind) is
   begin
      if Kind = Name_Column then
         return;
      end if;
      for Column in 2 .. P.Columns.Count loop
         if P.Kinds (Column) = Kind then
            for K in Column .. P.Columns.Count - 1 loop
               P.Kinds (K) := P.Kinds (K + 1);
               P.Columns.Width (K) := P.Columns.Width (K + 1);
               P.Columns.Minimum (K) := P.Columns.Minimum (K + 1);
            end loop;
            P.Columns.Count := P.Columns.Count - 1;
            P.Columns.Sort_Column := Column_Of_Key (P, Files_Order.Rule (P.Order).Key);
            return;
         end if;
      end loop;
      if P.Columns.Count < Tables.MAX_COLUMNS then
         P.Columns.Count := P.Columns.Count + 1;
         P.Kinds (P.Columns.Count) := Kind;
         Fit_Column (P, P.Columns.Count);
         P.Columns.Sort_Column := Column_Of_Key (P, Files_Order.Rule (P.Order).Key);
      end if;
   end Toggle_Column;

   --  Move column From to To's place (Name stays first).
   procedure Move_Column (P : in out Pane_State; From, To : Tables.Column_Index) is
      Kind : constant Column_Kind := P.Kinds (From);
      Width : constant Natural := P.Columns.Width (From);
      Minimum : constant Natural := P.Columns.Minimum (From);
   begin
      if From = 1 or else To = 1 or else From = To or else From > P.Columns.Count or else To > P.Columns.Count then
         return;
      end if;
      if From < To then
         for K in From .. To - 1 loop
            P.Kinds (K) := P.Kinds (K + 1);
            P.Columns.Width (K) := P.Columns.Width (K + 1);
            P.Columns.Minimum (K) := P.Columns.Minimum (K + 1);
         end loop;
      else
         for K in reverse To + 1 .. From loop
            P.Kinds (K) := P.Kinds (K - 1);
            P.Columns.Width (K) := P.Columns.Width (K - 1);
            P.Columns.Minimum (K) := P.Columns.Minimum (K - 1);
         end loop;
      end if;
      P.Kinds (To) := Kind;
      P.Columns.Width (To) := Width;
      P.Columns.Minimum (To) := Minimum;
      P.Columns.Sort_Column := Column_Of_Key (P, Files_Order.Rule (P.Order).Key);
   end Move_Column;

   procedure Set_Rule (State : in out View_State; Which : Side; Key : Files_Order.Sort_Key) is
      P : Pane_State renames State.Panes (Which).all;
      Current : constant Files_Order.Sort_Rule := Files_Order.Rule (P.Order);
      Next : constant Files_Order.Sort_Rule :=
        (Key => Key,
         Direction => (if Current.Key = Key and then Current.Direction = Files_Order.Ascending
                       then Files_Order.Descending else Files_Order.Ascending));
   begin
      Files_Order.Set_Rule (P.Order, Next);
      P.Columns.Sort_Column := Column_Of_Key (P, Key);
      P.Columns.Order := (if Next.Direction = Files_Order.Ascending then Tables.Ascending else Tables.Descending);
      Dirty_All (State);
   end Set_Rule;

   procedure Type_Filter (State : in out View_State; Which : Side; Text : String) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      if Text'Length <= Files_Filter.MAXIMUM_QUERY then
         Files_Filter.Set_Query (P.Filter, Text);
         --  The first match takes the cursor; an empty query leaves it on
         --  the entry it was on.
         if Text'Length > 0 then
            P.Cursor := On_First;
         end if;
         P.Seen_Filter := Files_Filter.Revision (P.Filter) - 1;
         Dirty_All (State);
      end if;
   end Type_Filter;

   ---------------------------------------------------------------------------
   --  Context menu and toolbar commands.
   ---------------------------------------------------------------------------
   --  A menu item's choice is its command (0 is the menus' "nothing").
   function Pos (Item : Command) return CuBit.UI.Popup_Menus.Command is (Command'Pos (Item) + 1);
   function Is_Choice (Choice : CuBit.UI.Popup_Menus.Command) return Boolean is
     (Choice in 1 .. Command'Pos (Command'Last) + 1);
   function Of_Choice (Choice : CuBit.UI.Popup_Menus.Command) return Command is (Command'Val (Choice - 1))
   with Pre => Is_Choice (Choice);
   --  A command's hint in the menus: the panes' keymap's, or the toolkit's
   --  clipboard chords for the clipboard commands (reserved, not bound).
   function Menu_Hint (Item : Command) return Keys.Chord is
     (case Item is
         when Copy_Entry_Names => CuBit.UI.Standard_Keys.Copy,
         when Copy_Entry_Paths => CuBit.UI.Standard_Keys.Copy_Shifted,
         when others => Maps.Hint (Panes_Map, Item));

   --  The operations not built yet (docs/files-app.md, Phase 3) show, disabled.
   OPERATIONS_READY : constant Boolean := True;

   ---------------------------------------------------------------------------
   --  Operations: prompts, starting, progress.
   ---------------------------------------------------------------------------
   --  The entries an operation acts on: the marks, else the cursor's entry.
   function Source_Count (P : Pane_State) return Natural is
     (if Files_Marks.Count (P.Marks) > 0 then Files_Marks.Count (P.Marks)
      elsif P.View.Cursor > 0 and then Entry_Of (P, P.View.Cursor) > 0 then 1 else 0);

   procedure Set_Input (Text : String) is
   begin
      Input_Length := Natural'Min (Text'Length, MAXIMUM_NAME_BYTES);
      Input (1 .. Input_Length) := Text (Text'First .. Text'First + Input_Length - 1);
   end Set_Input;

   procedure Ask (State : in out View_State; Kind : Prompt_Kind; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      if Files_Operations.Busy then
         Say ("An operation is running (" & Key_Text (Panes_Map, Clear_Or_Cancel) & " cancels it).");
         return;
      end if;
      if Kind in Confirm_Copy | Confirm_Move | Confirm_Delete | Ask_New_Name and then Source_Count (P) = 0 then
         Say ("Nothing to act on: put the cursor on an entry or mark some.");
         return;
      end if;
      if Kind in Confirm_Copy | Confirm_Move and then Full_Path (State.Panes (Other (State, Which)).all) = Full_Path (P)
        and then Kind = Confirm_Move
      then
         Say ("Both panes show the same folder.");
         return;
      end if;
      Prompt := Kind;
      Prompt_Side := Which;
      Prompt_Policy := Files_Operations.Ask;
      Input_Length := 0;
      if Kind = Ask_New_Name then
         declare
            Name : constant String := Files_Listing.Name (P.Listing, Entry_Of (P, P.View.Cursor));
         begin
            Renaming_Length := Name'Length;
            Renaming (1 .. Name'Length) := Name;
            Set_Input (Name);
         end;
      end if;
      Dirty_All (State);
   end Ask;

   --  The prompt was accepted: start what it asked.
   procedure Accept_Prompt (State : in out View_State) is
      Which : constant Side := Prompt_Side;
      P : Pane_State renames State.Panes (Which).all;
      Kind : constant Prompt_Kind := Prompt;
   begin
      Prompt := No_Prompt;
      Operation_Started_Us := Now_Us;
      case Kind is
         when No_Prompt => null;
         when Confirm_Copy | Confirm_Move | Confirm_Delete =>
            Files_Operations.Prepare
              ((case Kind is
                  when Confirm_Copy => Files_Operations.Copy_Operation,
                  when Confirm_Move => Files_Operations.Move_Operation,
                  when others => Files_Operations.Delete_Operation),
               Full_Path (P), Full_Path (State.Panes (Other (State, Which)).all), Prompt_Policy);
            if Files_Marks.Count (P.Marks) > 0 then
               for Id in 1 .. P.Listing.Count loop
                  if Files_Marks.Marked (P.Marks, Id) then
                     Files_Operations.Add_Source
                       (Files_Listing.Name (P.Listing, Id),
                        Files_Listing.Kind (P.Listing, Id) = Files_Listing.Directory_Kind,
                        Unsigned_64 (Files_Listing.Facts (P.Listing, Id).Size));
                  end if;
               end loop;
            else
               declare
                  Id : constant Entry_Count := Entry_Of (P, P.View.Cursor);
               begin
                  Files_Operations.Add_Source
                    (Files_Listing.Name (P.Listing, Id),
                     Files_Listing.Kind (P.Listing, Id) = Files_Listing.Directory_Kind,
                     Unsigned_64 (Files_Listing.Facts (P.Listing, Id).Size));
               end;
            end if;
            Files_Operations.Start (OPERATIONS_BASE, Now_Us);
         when Ask_Folder_Name =>
            if Input_Length > 0 then
               Files_Operations.Make_Folder (Full_Path (P), Input (1 .. Input_Length), OPERATIONS_BASE, Now_Us);
            end if;
         when Ask_New_Name =>
            if Input_Length > 0 and then Input (1 .. Input_Length) /= Renaming (1 .. Renaming_Length) then
               Files_Operations.Rename (Full_Path (P), Renaming (1 .. Renaming_Length), Input (1 .. Input_Length),
                                        OPERATIONS_BASE, Now_Us);
            end if;
         when Ask_Conflict => null;
      end case;
      Files_Queue.Flush;
      Dirty_All (State);
   end Accept_Prompt;

   function Policy_Text (Policy : Files_Operations.Conflict_Policy) return String is
     (case Policy is
         when Files_Operations.Ask => "ask", when Files_Operations.Skip_Existing => "skip",
         when Files_Operations.Overwrite_Existing => "overwrite", when Files_Operations.Keep_Both => "keep both");

   function Reason (Status : Unsigned_32) return String is (Files_Status.Reason (Files_Status.Kind_Of (Status)));

   --  An operation changed: report its end; it repaints the status line.
   procedure Watch_Operation (State : in out View_State) is
      use Files_Operations;
   begin
      if Revision = Operation_Seen then
         return;
      end if;
      Operation_Seen := Revision;
      if Phase = Asking and then Prompt = No_Prompt then
         Prompt := Ask_Conflict;
         Dirty_All (State);
      elsif Phase = Finished then
         Volume_Stale := True;
         declare
            Verb : constant String :=
              (case Kind is
                  when Copy_Operation => "Copied", when Move_Operation => "Moved",
                  when Delete_Operation => "Deleted", when Make_Folder_Operation => "Made",
                  when Rename_Operation => "Renamed");
            Took : constant Unsigned_64 := (if Now_Us > Operation_Started_Us then Now_Us - Operation_Started_Us else 0);
         begin
            case Result is
               when Succeeded =>
                  Say (Verb & Natural'Image (Items_Done) & (if Items_Done = 1 then " item" else " items")
                       & (if Bytes_Done > 0 then " (" & Size_Text (Bytes_Done) & ")" else "")
                       & (if Skipped > 0 then "," & Natural'Image (Skipped) & " skipped" else "")
                       & " in " & Milliseconds (Took) & ".");
               when Failed =>
                  Say ("Stopped at " & Current & ": " & Reason (Failure) & "." & Natural'Image (Items_Done)
                       & " done.");
               when Cancelled =>
                  Say ("Cancelled after" & Natural'Image (Items_Done) & " items.");
               when Timed_Out =>
                  Say ("Stopped at " & Current & ": no answer within"
                       & Natural'Image (STEP_DEADLINE_US / 1_000_000) & " s.");
               when No_Outcome => null;
            end case;
            Acknowledge;
            if Prompt = Ask_Conflict then
               Prompt := No_Prompt;
            end if;
            for W in Side range 1 .. State.Count loop
               Files_Marks.Clear (State.Panes (W).Marks);
            end loop;
         end;
         Dirty_All (State);
      else
         Dirty (State, Status_Area);
      end if;
   end Watch_Operation;

   --  Keys and text while a prompt is up; True when consumed.
   function Chord_Of (Item : Event) return Keys.Chord is
     ((Modifiers => [Keys.Shift => Item.Shift, Keys.Ctrl => Item.Control, Keys.Alt => Item.Alt, Keys.Super => False],
       Pressed => Item.Key));

   --  A prompt's commands (its keymap, or its buttons).
   procedure Run_Prompt (State : in out View_State; Item : Command) is
      Choice : Files_Operations.Conflict_Policy := Files_Operations.Ask;
   begin
      case Item is
         when Prompt_Cancel =>
            if Prompt = Ask_Conflict then
               Files_Operations.Cancel;
            end if;
            Prompt := No_Prompt;
         when Prompt_Accept => Accept_Prompt (State);
         when Prompt_Erase =>
            if Input_Length > 0 then
               Input_Length := Input_Length - 1;
            end if;
         when Skip_Existing | Overwrite_Existing | Keep_Both | Ask_Each | Skip_All | Overwrite_All | Keep_Both_All =>
            Choice := (case Item is
                          when Skip_Existing | Skip_All => Files_Operations.Skip_Existing,
                          when Overwrite_Existing | Overwrite_All => Files_Operations.Overwrite_Existing,
                          when Keep_Both | Keep_Both_All => Files_Operations.Keep_Both,
                          when others => Files_Operations.Ask);
            if Prompt = Ask_Conflict then
               if Choice /= Files_Operations.Ask then
                  Prompt := No_Prompt;
                  --  The Shift forms: for the rest of the operation too.
                  Files_Operations.Decide (Choice, For_All => Item in Skip_All | Overwrite_All | Keep_Both_All);
               end if;
            else
               Prompt_Policy := Choice;
            end if;
         when others => null;
      end case;
   end Run_Prompt;

   procedure Prompt_Input (State : in out View_State; Item : Event; Map : in out Controls.Control_Map;
                           Used : out Boolean) is
      Names : constant Boolean := Prompt in Ask_Folder_Name | Ask_New_Name;
      Found : Boolean;
      Chosen : Command;
   begin
      Used := Prompt /= No_Prompt;
      if not Used then
         return;
      end if;
      if Item.Kind = Key_Event then
         Maps.Find (Prompt_Map, Chord_Of (Item), Found, Chosen);
         if Found and then Enabled (Chosen) then
            Run_Prompt (State, Chosen);
         end if;
      elsif Item.Kind = Text_Event then
         --  A name prompt types; the others take their keys as commands.
         if Names and then Item.Character_Value in ' ' .. '~' and then Input_Length < MAXIMUM_NAME_BYTES then
            Input_Length := Input_Length + 1;
            Input (Input_Length) := Item.Character_Value;
         end if;
      elsif Item.Kind = Pointer_Event and then Item.Action = Controls.Pointer_Press
        and then not Point_In_Rect (Item.X, Item.Y, Dialog_Area)
      then
         --  A click outside the dialog dismisses it (a conflict stays).
         if Prompt /= Ask_Conflict then
            Prompt := No_Prompt;
         end if;
      elsif Item.Kind = Pointer_Event then
         --  The dialog's buttons do what their keys do.
         Bars.Take_Chosen (Dialog_Bar, Map, Prompt_Map, Found, Chosen);
         if Found and then Enabled (Chosen) then
            Run_Prompt (State, Chosen);
         end if;
         Bars.Take_Chosen (Policy_Bar, Map, Prompt_Map, Found, Chosen);
         if Found and then Enabled (Chosen) then
            Run_Prompt (State, Chosen);
         end if;
      elsif Item.Kind = Resize then
         Used := False;
      end if;
      Dirty_All (State);
   end Prompt_Input;

   function Shows (P : Pane_State; Kind : Column_Kind) return Boolean is
     ((for some Column in 1 .. P.Columns.Count => P.Kinds (Column) = Kind));

   --  Which columns the pane shows, as check items.
   procedure Open_Columns_Menu (State : in out View_State; Which : Side; X, Y : Natural) is
      package PM renames CuBit.UI.Popup_Menus;
      P : Pane_State renames State.Panes (Which).all;
   begin
      PM.Clear (Menu);
      Menu_Side := Which;
      for Item in Files_Commands.Column_Command loop
         declare
            Kind : constant Column_Kind :=
              Column_Kind'Val (Column_Kind'Pos (Extension_Column) + Command'Pos (Item)
                               - Command'Pos (Files_Commands.Column_Command'First));
         begin
            PM.Add (Menu, PM.ROOT, COLUMN_CAPTIONS (Kind).all, Pos (Item), Checked => Shows (P, Kind));
         end;
      end loop;
      PM.Add_Separator (Menu, PM.ROOT);
      PM.Add (Menu, PM.ROOT, "Drag a header to move its column", PM.NO_COMMAND, Enabled => False);
      PM.Open (Popup, Menu, X, Y, State.Bounds);
      Dirty (State, PM.Covered (Popup));
   end Open_Columns_Menu;

   --  The menu for the row under the pointer (0: empty space) of Which.
   --  Each item names a command; its keycap is the keymap's hint.
   procedure Open_Menu (State : in out View_State; Which : Side; Row : Row_Count; X, Y : Natural) is
      package PM renames CuBit.UI.Popup_Menus;
      P : Pane_State renames State.Panes (Which).all;
      Id : constant Entry_Count := (if Row = 0 then 0 else Entry_Of (P, Row));
      Marked : constant Natural := Files_Marks.Count (P.Marks);
      Folder : constant Boolean := Id > 0 and then Files_Listing.Kind (P.Listing, Id) = Files_Listing.Directory_Kind;
      Rule : constant Files_Order.Sort_Rule := Files_Order.Rule (P.Order);
      Sort_Menu : PM.Menu_Count;
      procedure Add
        (Into : PM.Menu_Index; Caption : String; Run : Command; Enabled : Boolean := True;
         Checked : Boolean := False; Icon : Boolean := False) is
      begin
         PM.Add (Menu, Into, Caption, Pos (Run), Menu_Hint (Run), Enabled, Checked,
                 Has_Icon => Icon, Picture => Files_Commands.Picture (Run), Shade => Files_Commands.Shade (Run));
      end Add;
   begin
      PM.Clear (Menu);
      Menu_Side := Which;
      if Marked > 0 and then (Id = 0 or else Files_Marks.Marked (P.Marks, Id)) then
         declare
            Count_Text : constant String := Thousands (Unsigned_64 (Marked)) & (if Marked = 1 then " item" else " items");
         begin
            Add (PM.ROOT, "Copy " & Count_Text & " to the other pane", Copy, OPERATIONS_READY, Icon => True);
            Add (PM.ROOT, "Move " & Count_Text & " to the other pane", Move, OPERATIONS_READY, Icon => True);
            Add (PM.ROOT, "Delete " & Count_Text, Delete, OPERATIONS_READY, Icon => True);
            PM.Add_Separator (Menu, PM.ROOT);
            Add (PM.ROOT, "Copy names", Copy_Entry_Names);
            Add (PM.ROOT, "Copy paths", Copy_Entry_Paths);
            PM.Add_Separator (Menu, PM.ROOT);
            Add (PM.ROOT, "Unmark all", Unmark_All);
            Add (PM.ROOT, "Invert marks", Invert_Marks);
         end;
      elsif Id > 0 then
         if Folder then
            Add (PM.ROOT, "Open", Open_Entry, Icon => True);
            Add (PM.ROOT, "Open in the other pane", Open_In_Other_Pane);
            Add (PM.ROOT, "Bookmark", Bookmark_Entry, Icon => True);
         else
            Add (PM.ROOT, "View", View, Icon => True);
         end if;
         PM.Add_Separator (Menu, PM.ROOT);
         Add (PM.ROOT, "Copy to the other pane", Copy, OPERATIONS_READY, Icon => True);
         Add (PM.ROOT, "Move to the other pane", Move, OPERATIONS_READY, Icon => True);
         Add (PM.ROOT, "Rename", Rename, OPERATIONS_READY, Icon => True);
         Add (PM.ROOT, "Delete", Delete, OPERATIONS_READY, Icon => True);
         PM.Add_Separator (Menu, PM.ROOT);
         Add (PM.ROOT, "Copy name", Copy_Entry_Names);
         Add (PM.ROOT, "Copy path", Copy_Entry_Paths);
         Add (PM.ROOT, "Mark", Toggle_Mark);
      else
         Add (PM.ROOT, "New folder", New_Folder, OPERATIONS_READY, Icon => True);
         Add (PM.ROOT, "Refresh", Refresh, Icon => True);
         Add (PM.ROOT, "Up", Parent_Folder, not At_Root (P), Icon => True);
         PM.Add_Separator (Menu, PM.ROOT);
         PM.Add_Submenu (Menu, PM.ROOT, "Sort by", Sort_Menu);
         if Sort_Menu > 0 then
            Add (Sort_Menu, "Name", Sort_By_Name, Checked => Rule.Key = Files_Order.By_Name);
            Add (Sort_Menu, "Extension", Sort_By_Extension, Checked => Rule.Key = Files_Order.By_Extension);
            Add (Sort_Menu, "Modified", Sort_By_Modified, Checked => Rule.Key = Files_Order.By_Modified);
            Add (Sort_Menu, "Size", Sort_By_Size, Checked => Rule.Key = Files_Order.By_Size);
         end if;
         Add (PM.ROOT, "Mark all", Mark_All);
         PM.Add_Separator (Menu, PM.ROOT);
         Add (PM.ROOT, "Bookmark this folder", Bookmark_Folder, Icon => True);
         Add (PM.ROOT, (if Drawer.Open then "Hide the drawer" else "Show the drawer"), Toggle_Drawer, Icon => True);
      end if;
      PM.Open (Popup, Menu, X, Y, State.Bounds);
      Dirty (State, PM.Covered (Popup));
   end Open_Menu;

   --  The menu's commands, on the menu's pane.
   procedure Run_Command (State : in out View_State; Item : Command) is
      Which : constant Side := Menu_Side;
      P : Pane_State renames State.Panes (Which).all;
   begin
      case Item is
         when Open_Entry => Open_Cursor_Entry (State, Which);
         when View => Open_Viewer (State, Which);
         when Open_In_Other_Pane =>
            if P.View.Cursor > 0 and then Entry_Of (P, P.View.Cursor) > 0 then
               declare
                  Child : DP.Path;
                  Done : Boolean;
               begin
                  DP.Append_Child (P.Where, Files_Listing.Name (P.Listing, Entry_Of (P, P.View.Cursor)), Child, Done);
                  if Done then
                     Go_To (State, Other (State, Which), Child, "");
                  end if;
               end;
            end if;
         when Copy => Ask (State, Confirm_Copy, Which);
         when Copy_Entry_Names | Copy_Entry_Paths =>
            --  The entries are the menu's pane's: make it the active one.
            State.Current := Which;
            Pending_Clipboard := (if Item = Copy_Entry_Names then Request_Copy_Names else Request_Copy_Paths);
         when Move => Ask (State, Confirm_Move, Which);
         when Delete => Ask (State, Confirm_Delete, Which);
         when Rename => Ask (State, Ask_New_Name, Which);
         when New_Folder => Ask (State, Ask_Folder_Name, Which);
         when Toggle_Mark =>
            if P.View.Cursor > 0 then
               Set_Mark (P, P.View.Cursor, not Marked_Row (P, P.View.Cursor));
            end if;
         when Mark_All | Unmark_All =>
            for Row in 1 .. Row_Total (P) loop
               Set_Mark (P, Row, Item = Mark_All);
            end loop;
         when Invert_Marks =>
            for Row in 1 .. Row_Total (P) loop
               Set_Mark (P, Row, not Marked_Row (P, Row));
            end loop;
         when Refresh => Refresh (State, Which);
         when Parent_Folder => Go_Parent (State, Which);
         when Bookmark_Folder => Toggle_Bookmark (P.Where);
         when Bookmark_Entry =>
            if P.View.Cursor > 0 and then Entry_Of (P, P.View.Cursor) > 0 then
               declare
                  Child : DP.Path;
                  Done : Boolean;
               begin
                  DP.Append_Child (P.Where, Files_Listing.Name (P.Listing, Entry_Of (P, P.View.Cursor)), Child, Done);
                  if Done then
                     Toggle_Bookmark (Child);
                  end if;
               end;
            end if;
         when Sort_By_Name => Set_Rule (State, Which, Files_Order.By_Name);
         when Sort_By_Extension => Set_Rule (State, Which, Files_Order.By_Extension);
         when Sort_By_Modified => Set_Rule (State, Which, Files_Order.By_Modified);
         when Sort_By_Size => Set_Rule (State, Which, Files_Order.By_Size);
         when Toggle_Drawer => Drawer.Open := not Drawer.Open;
         when Files_Commands.Column_Command =>
            Toggle_Column
              (P, Column_Kind'Val (Column_Kind'Pos (Extension_Column)
                                   + Command'Pos (Item) - Command'Pos (Files_Commands.Column_Command'First)));
         when others => null;
      end case;
      Dirty_All (State);
   end Run_Command;

   procedure New_Tab (State : in out View_State; Which : Side);
   procedure Add_Pane (State : in out View_State; Which : Side);
   procedure Toggle_Single (State : in out View_State);
   function Tab_Box (P : Pane_State; K : Positive) return Rect;
   function Tab_Close_Box (P : Pane_State; K : Positive) return Rect;


   ---------------------------------------------------------------------------
   --  Tabs and panes.
   ---------------------------------------------------------------------------
   function Tab_Now (P : Pane_State) return Tab_State is
      Here_Entry : constant History_Entry := Here (P);
   begin
      return (Where => P.Where, Name => Here_Entry.Name, Length => Here_Entry.Length,
              Rule => Files_Order.Rule (P.Order));
   end Tab_Now;

   --  Show tab Index of Which (its folder listed afresh, the cursor back on
   --  its entry).
   procedure Show_Tab (State : in out View_State; Which : Side; Index : Positive) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      if Index > P.Tab_Count then
         return;
      end if;
      if P.Current_Tab in 1 .. P.Tab_Count then
         P.Tabs (P.Current_Tab) := Tab_Now (P);
      end if;
      P.Current_Tab := Index;
      declare
         T : constant Tab_State := P.Tabs (Index);
      begin
         P.Where := T.Where;
         P.Root_Length := Root_Length_Of (DP.Value (T.Where));
         Start_Listing (Which, P, T.Name (1 .. T.Length));
         Files_Order.Set_Rule (P.Order, T.Rule);
         P.Columns.Sort_Column := Column_Of_Key (P, T.Rule.Key);
      end;
      Dirty_All (State);
   end Show_Tab;

   procedure New_Tab (State : in out View_State; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      if P.Tab_Count = MAXIMUM_TABS then
         Say ("A pane holds" & Natural'Image (MAXIMUM_TABS) & " tabs.");
         return;
      end if;
      P.Tabs (P.Current_Tab) := Tab_Now (P);
      P.Tab_Count := P.Tab_Count + 1;
      for K in reverse P.Current_Tab + 2 .. P.Tab_Count loop
         P.Tabs (K) := P.Tabs (K - 1);
      end loop;
      P.Tabs (P.Current_Tab + 1) := Tab_Now (P);
      P.Current_Tab := P.Current_Tab + 1;
      Dirty_All (State);
   end New_Tab;

   procedure Close_Pane (State : in out View_State; Which : Side);

   procedure Close_Tab (State : in out View_State; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
      Gone : constant Tab_State := Tab_Now (P);
   begin
      if P.Tab_Count <= 1 then
         Close_Pane (State, Which);
         return;
      end if;
      if Closed_Count = MAXIMUM_CLOSED_TABS then
         Closed_Tabs (1 .. MAXIMUM_CLOSED_TABS - 1) := Closed_Tabs (2 .. MAXIMUM_CLOSED_TABS);
         Closed_Count := Closed_Count - 1;
      end if;
      Closed_Count := Closed_Count + 1;
      Closed_Tabs (Closed_Count) := Gone;
      for K in P.Current_Tab .. P.Tab_Count - 1 loop
         P.Tabs (K) := P.Tabs (K + 1);
      end loop;
      P.Tab_Count := P.Tab_Count - 1;
      P.Current_Tab := 0;
      Show_Tab (State, Which, Natural'Max (1, Natural'Min (P.Tab_Count, Natural'Max (1, P.Tab_Count))));
   end Close_Tab;

   --  Close tab K of the pane: the current one through Close_Tab; another
   --  one without showing it first (no listing for a tab about to go).
   procedure Close_Tab_At (State : in out View_State; Which : Side; K : Positive) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      if K = P.Current_Tab or else K > P.Tab_Count then
         Close_Tab (State, Which);
         return;
      end if;
      if Closed_Count = MAXIMUM_CLOSED_TABS then
         Closed_Tabs (1 .. MAXIMUM_CLOSED_TABS - 1) := Closed_Tabs (2 .. MAXIMUM_CLOSED_TABS);
         Closed_Count := Closed_Count - 1;
      end if;
      Closed_Count := Closed_Count + 1;
      Closed_Tabs (Closed_Count) := P.Tabs (K);
      for J in K .. P.Tab_Count - 1 loop
         P.Tabs (J) := P.Tabs (J + 1);
      end loop;
      P.Tab_Count := P.Tab_Count - 1;
      if K < P.Current_Tab then
         P.Current_Tab := P.Current_Tab - 1;
      end if;
      Dirty_All (State);
   end Close_Tab_At;

   procedure Restore_Tab (State : in out View_State; Which : Side) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      if Closed_Count = 0 or else P.Tab_Count = MAXIMUM_TABS then
         return;
      end if;
      P.Tabs (P.Current_Tab) := Tab_Now (P);
      P.Tab_Count := P.Tab_Count + 1;
      P.Tabs (P.Tab_Count) := Closed_Tabs (Closed_Count);
      Closed_Count := Closed_Count - 1;
      P.Current_Tab := 0;
      Show_Tab (State, Which, P.Tab_Count);
   end Restore_Tab;

   procedure Move_Tab (State : in out View_State; Which : Side; From, To : Positive) is
      P : Pane_State renames State.Panes (Which).all;
   begin
      if From > P.Tab_Count or else To > P.Tab_Count or else From = To then
         return;
      end if;
      P.Tabs (P.Current_Tab) := Tab_Now (P);
      declare
         Moving : constant Tab_State := P.Tabs (From);
      begin
         if From < To then
            P.Tabs (From .. To - 1) := P.Tabs (From + 1 .. To);
         else
            P.Tabs (To + 1 .. From) := P.Tabs (To .. From - 1);
         end if;
         P.Tabs (To) := Moving;
      end;
      if P.Current_Tab = From then
         P.Current_Tab := To;
      elsif From < P.Current_Tab and then To >= P.Current_Tab then
         P.Current_Tab := P.Current_Tab - 1;
      elsif From > P.Current_Tab and then To <= P.Current_Tab then
         P.Current_Tab := P.Current_Tab + 1;
      end if;
      Dirty_All (State);
   end Move_Tab;

   --  A new pane after Which, on the same folder.
   procedure Add_Pane (State : in out View_State; Which : Side) is
   begin
      if State.Count = MAXIMUM_PANES then
         Say ("Files shows at most" & Natural'Image (MAXIMUM_PANES) & " panes.");
         return;
      end if;
      State.Count := State.Count + 1;
      --  Panes after Which shift right; the new one takes Which + 1.
      declare
         Spare : constant Pane_Access := State.Panes (State.Count);
      begin
         for K in reverse Which + 2 .. State.Count loop
            State.Panes (K) := State.Panes (K - 1);
            State.Shown (K) := State.Shown (K - 1);
         end loop;
         State.Panes (Which + 1) := Spare;
      end;
      State.Shown (Which + 1) := True;
      New_Pane (State, Which + 1, Full_Path (State.Panes (Which).all));
      State.Shares := [others => 1];
      State.Current := Which + 1;
      Dirty_All (State);
   end Add_Pane;

   procedure Close_Pane (State : in out View_State; Which : Side) is
   begin
      if State.Count = 1 then
         Say ("The last pane stays.");
         return;
      end if;
      declare
         Gone : constant Pane_Access := State.Panes (Which);
      begin
         if Gone.Load = Reading then
            Close_Handle (Gone.Handle);
         end if;
         for K in Which .. State.Count - 1 loop
            State.Panes (K) := State.Panes (K + 1);
            State.Shown (K) := State.Shown (K + 1);
         end loop;
         --  Kept for reuse by the next Add_Pane.
         State.Panes (State.Count) := Gone;
      end;
      State.Count := State.Count - 1;
      if (for all K in Side range 1 .. State.Count => not State.Shown (K)) then
         State.Shown (1) := True;
      end if;
      State.Current := Side'Min (Which, State.Count);
      if not State.Shown (State.Current) then
         State.Current := Other (State, State.Current);
      end if;
      State.Shares := [others => 1];
      Dirty_All (State);
   end Close_Pane;

   --  Single-pane mode: only the active pane, or all of them again.
   procedure Toggle_Single (State : in out View_State) is
      Alone : constant Boolean :=
        (for all K in Side range 1 .. State.Count => K = State.Current or else not State.Shown (K));
   begin
      for K in Side range 1 .. State.Count loop
         State.Shown (K) := Alone or else K = State.Current;
      end loop;
      State.Shares := [others => 1];
      Dirty_All (State);
   end Toggle_Single;

   procedure Toggle_Shown (State : in out View_State; Which : Side) is
   begin
      if Which > State.Count then
         return;
      end if;
      if State.Shown (Which)
        and then (for all K in Side range 1 .. State.Count => K = Which or else not State.Shown (K))
      then
         return;   --  the last shown pane stays
      end if;
      State.Shown (Which) := not State.Shown (Which);
      if not State.Shown (State.Current) then
         State.Current := Other (State, State.Current);
      end if;
      State.Shares := [others => 1];
      Dirty_All (State);
   end Toggle_Shown;

   --  The next (or previous) shown pane.
   function Cycle (State : View_State; From : Side; Forward : Boolean) return Side is
      Candidate : Side := From;
   begin
      for Step in 1 .. State.Count loop
         Candidate := (if Forward then (if Candidate >= State.Count then 1 else Candidate + 1)
                       else (if Candidate = 1 then State.Count else Candidate - 1));
         if State.Shown (Candidate) then
            return Candidate;
         end if;
      end loop;
      return From;
   end Cycle;

   function Now_Ms return Unsigned_64 is (Now_Us / MICROSECONDS_PER_MILLISECOND);
   function Context_Map return Maps.Keymap is
     (if Prompt /= No_Prompt then Prompt_Map elsif Viewing then Viewer_Map else Panes_Map);
   --  The key bar shows the panes' keys, or the viewer's while it is open.
   function Bar_Map return Maps.Keymap is (if Viewing then Viewer_Map else Panes_Map);

   --  What can run now, and what is on: the bars and the dispatch read it.
   procedure Note_State (State : View_State) is
      P : Pane_State renames State.Panes (State.Current).all;
      On_File : constant Boolean :=
        P.View.Cursor > 0 and then Entry_Of (P, P.View.Cursor) > 0
        and then Files_Listing.Kind (P.Listing, Entry_Of (P, P.View.Cursor)) /= Files_Listing.Directory_Kind;
      Policies : constant Boolean := Prompt in Confirm_Copy | Confirm_Move | Ask_Conflict;
   begin
      Enabled_Now := [others => True];
      Checked_Now := [others => False];
      --  Not built yet (docs/files-app.md): shown, dimmed.
      Enabled_Now (Help) := False;
      Enabled_Now (Edit) := False;
      Enabled_Now (History_Back) := P.Back_Count > 0;
      Enabled_Now (History_Forward) := P.Forward_Count > 0;
      Enabled_Now (Parent_Folder) := not At_Root (P);
      for Item in Command range Copy .. Delete loop
         Enabled_Now (Item) := OPERATIONS_READY;
      end loop;
      Enabled_Now (View) := On_File;
      for Item in Command range Close_Viewer .. Viewer_Text_Or_Hex loop
         Enabled_Now (Item) := Viewing;
      end loop;
      Enabled_Now (Prompt_Accept) := Prompt not in No_Prompt | Ask_Conflict;
      Enabled_Now (Prompt_Cancel) := Prompt /= No_Prompt;
      Enabled_Now (Prompt_Erase) := Prompt in Ask_Folder_Name | Ask_New_Name;
      for Item in Command range Skip_Existing .. Keep_Both loop
         Enabled_Now (Item) := Policies;
      end loop;
      Enabled_Now (Ask_Each) := Prompt in Confirm_Copy | Confirm_Move;
      for Item in Command range Skip_All .. Keep_Both_All loop
         Enabled_Now (Item) := Prompt = Ask_Conflict;
      end loop;
      Checked_Now (Toggle_Drawer) := Drawer.Open;
      Checked_Now (View) := Viewing;
      Checked_Now (Single_Pane) :=
        (for all K in Side range 1 .. State.Count => K = State.Current or else not State.Shown (K));
      Checked_Now (Skip_Existing) := Prompt /= Ask_Conflict and then Prompt_Policy = Files_Operations.Skip_Existing;
      Checked_Now (Overwrite_Existing) :=
        Prompt /= Ask_Conflict and then Prompt_Policy = Files_Operations.Overwrite_Existing;
      Checked_Now (Keep_Both) := Prompt /= Ask_Conflict and then Prompt_Policy = Files_Operations.Keep_Both;
      Checked_Now (Ask_Each) := Prompt_Policy = Files_Operations.Ask;
   end Note_State;

   --  A command from the panes' keymap, a toolbar button or a key bar cell.
   procedure Run (State : in out View_State; Item : Command; Map : in out Controls.Control_Map) is
      Which : constant Side := State.Current;
      P : Pane_State renames State.Panes (Which).all;
      Page : constant Row_Delta := Integer'Max (1, P.View.Rows - 1);
      procedure Cycle_Tab (Forward : Boolean) is
      begin
         if P.Tab_Count > 1 then
            Show_Tab (State, Which,
                      (if Forward then (if P.Current_Tab >= P.Tab_Count then 1 else P.Current_Tab + 1)
                       else (if P.Current_Tab <= 1 then P.Tab_Count else P.Current_Tab - 1)));
         end if;
      end Cycle_Tab;
      procedure Other_Pane (Forward : Boolean) is
      begin
         State.Current := Cycle (State, Which, Forward);
         Dirty (State, P.Path_Area);
         Dirty (State, Row_Area (P, P.View.Cursor));
         Dirty (State, State.Panes (State.Current).Path_Area);
         Dirty (State, Row_Area (State.Panes (State.Current).all, State.Panes (State.Current).View.Cursor));
         Dirty (State, Status_Area);
      end Other_Pane;
   begin
      Menu_Side := Which;
      case Item is
         when Cursor_Up | Extend_Up => Move_Cursor (State, Which, -1, Item = Extend_Up);
         when Cursor_Down | Extend_Down => Move_Cursor (State, Which, 1, Item = Extend_Down);
         when Page_Up_Rows | Extend_Page_Up => Move_Cursor (State, Which, -Page, Item = Extend_Page_Up);
         when Page_Down_Rows | Extend_Page_Down => Move_Cursor (State, Which, Page, Item = Extend_Page_Down);
         when First_Row | Extend_First => Move_Cursor (State, Which, -Row_Count'Last, Item = Extend_First);
         when Last_Row | Extend_Last => Move_Cursor (State, Which, Row_Count'Last, Item = Extend_Last);
         when Parent_Folder => Go_Parent (State, Which);
         when Erase_Or_Parent =>
            if Files_Filter.Active (P.Filter) then
               declare
                  Query : constant String := Files_Filter.Query (P.Filter);
               begin
                  Type_Filter (State, Which, Query (Query'First .. Query'Last - 1));
               end;
            else
               Go_Parent (State, Which);
            end if;
         when History_Back | History_Forward => History_Move (State, Which, Backward => Item = History_Back);
         when Clear_Or_Cancel =>
            if Files_Operations.Busy then
               Files_Operations.Cancel;
               Dirty (State, Status_Area);
            elsif Files_Filter.Active (P.Filter) then
               Type_Filter (State, Which, "");
            elsif Files_Marks.Count (P.Marks) > 0 then
               Files_Marks.Clear (P.Marks);
               Dirty_All (State);
            end if;
         when Next_Pane | Previous_Pane => Other_Pane (Item = Next_Pane);
         when Next_Tab | Previous_Tab => Cycle_Tab (Item = Next_Tab);
         when Move_Tab_Left | Move_Tab_Right =>
            Move_Tab (State, Which, P.Current_Tab,
                      (if Item = Move_Tab_Left then Natural'Max (1, P.Current_Tab - 1)
                       else Natural'Min (P.Tab_Count, P.Current_Tab + 1)));
         when New_Tab => New_Tab (State, Which);
         when Restore_Tab => Restore_Tab (State, Which);
         when Close_Tab => Close_Tab (State, Which);
         when Close_Pane => Close_Pane (State, Which);
         when Add_Pane => Add_Pane (State, Which);
         when Single_Pane => Toggle_Single (State);
         when Toggle_Left_Pane => Toggle_Shown (State, Left_Pane);
         when Toggle_Right_Pane => Toggle_Shown (State, Right_Pane);
         when Swap_Panes =>
            declare
               Next : constant Side := Other (State, Which);
               Swap : constant Pane_Access := State.Panes (Which);
            begin
               State.Panes (Which) := State.Panes (Next);
               State.Panes (Next) := Swap;
            end;
            Dirty_All (State);
         when Mark_And_Next =>
            if P.View.Cursor > 0 then
               Set_Mark (P, P.View.Cursor, not Marked_Row (P, P.View.Cursor));
               Dirty (State, P.Path_Area);
               Dirty (State, Status_Area);
               Move_Cursor (State, Which, 1, False);
            end if;
         when Toggle_Mark =>
            if P.View.Cursor > 0 then
               Set_Mark (P, P.View.Cursor, not Marked_Row (P, P.View.Cursor));
               Dirty (State, Row_Area (P, P.View.Cursor));
               Dirty (State, P.Path_Area);
               Dirty (State, Status_Area);
            end if;
         when Open_Entry => Open_Cursor (State, Which);
         when Context_Menu =>
            declare
               Box : constant Rect := Row_Area (P, P.View.Cursor);
            begin
               Open_Menu (State, Which, P.View.Cursor,
                          (if Is_Empty (Box) then P.Rows_Area.x else Box.x + Box.w / 4),
                          (if Is_Empty (Box) then P.Rows_Area.y else Box.y + Box.h));
            end;
         when Columns_Menu =>
            declare
               Box : constant Rect := Bars.Button_Area (Tool_Bar, Tool'Pos (Tool_Columns) + 1);
            begin
               Open_Columns_Menu (State, Which, Box.x, Box.y + Box.h);
            end;
         when Quit => State.Quit := True;
         when Toggle_Overlay =>
            State.Overlay := not State.Overlay;
            Dirty_All (State);
         when Toggle_Key_Bar =>
            Key_Bar_Shown := not Key_Bar_Shown;
            Dirty_All (State);
         when Help | Edit => null;
         when others =>
            Run_Command (State, Item);
      end case;
      pragma Unreferenced (Map);
   end Run;

   --  The viewer's commands.
   procedure Run_Viewer (State : in out View_State; Item : Command) is
   begin
      case Item is
         when Close_Viewer => Close_Viewer (State);
         when Viewer_Line_Up => Viewer_Top := (if Viewer_Top > 0 then Viewer_Top - 1 else 0);
         when Viewer_Line_Down => Viewer_Top := Natural'Min (Viewer_Top + 1, Natural'Max (0, Line_Count - Viewer_Rows));
         when Viewer_Page_Up => Viewer_Top := Natural'Max (0, Viewer_Top - Viewer_Rows);
         when Viewer_Page_Down =>
            Viewer_Top := Natural'Min (Viewer_Top + Viewer_Rows, Natural'Max (0, Line_Count - Viewer_Rows));
         when Viewer_To_Top => Viewer_Top := 0;
         when Viewer_To_Bottom => Viewer_Top := Natural'Max (0, Line_Count - Viewer_Rows);
         when Viewer_Text_Or_Hex =>
            Viewer_Hex := not Viewer_Hex;
            Viewer_Top := 0;
            if Files_Reader.State = Files_Reader.Done then
               Index_Lines;
            end if;
         when others => null;
      end case;
      Dirty_All (State);
   end Run_Viewer;

   --  Where held modifiers' hints changed: the bars and the old and new
   --  reveal labels repaint.
   procedure Note_Reveal (State : in out View_State) is
      Now : constant Keys.Modifier_Set := Revealed_Now;
   begin
      if Now /= Shown_Reveal then
         Dirty (State, Bars.Area_Of (Key_Bar));
         Dirty (State, Bars.Area_Of (Tool_Bar));
         Dirty (State, Reveal_Damage);
         Dirty (State, Bars.Reveal_Area (Tool_Bar, Panes_Map, Now, State.Bounds));
         --  The key bar's height may follow its layer.
         if Key_Bar_Shown then
            Dirty_All (State);
         end if;
      end if;
   end Note_Reveal;

   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean)
   is
      Which : constant Side := State.Current;
      Before : constant Rect := State.Dirty;
      package PM renames CuBit.UI.Popup_Menus;
      Tip_Damage : Rect;
      Found : Boolean;
      Chosen_Command : Command;
      Flash_Damage : Rect;
   begin
      --  Every key event: held modifiers reveal their hints after the hold
      --  delay; any key or click with them held is a chord (no reveal).
      if Item.Kind in Key_Event | Key_Release then
         MR.Key_Event (Reveal, Item.Key, Item.Kind = Key_Event, Chord_Of (Item).Modifiers, Now_Ms);
         Note_Reveal (State);
         if Item.Kind = Key_Release then
            Redraw := not Is_Empty (State.Dirty);
            return;
         end if;
      elsif Item.Kind = Pointer_Event and then Item.Action = Controls.Pointer_Press then
         MR.Note_Use (Reveal);
         Note_Reveal (State);
      end if;
      Note_State (State);
      --  Tooltips follow the pointer and go away on any other input.
      if Item.Kind = Pointer_Event and then Item.Action = Controls.Pointer_Move then
         Tooltips.Pointer_At (Tip, Tips, Map, Item.X, Item.Y, Now_Ms, Tip_Damage);
      else
         Tooltips.Dismiss (Tip, Tip_Damage);
      end if;
      Dirty (State, Tip_Damage);
      --  An open context menu takes all input until it closes.
      if PM.Is_Open (Popup) then
         declare
            Was : constant Rect := PM.Covered (Popup);
            Chosen : PM.Command := PM.NO_COMMAND;
            Handled : Boolean := True;
         begin
            case Item.Kind is
               when Key_Event =>
                  PM.Handle_Chord (Popup, Menu, Chord_Of (Item), Chosen, Handled);
               when Pointer_Event =>
                  PM.Handle_Pointer (Popup, Menu, Item.Action, Item.X, Item.Y, Chosen, Handled);
               when Wheel | Text_Event | Key_Release =>
                  PM.Close (Popup);
               when Resize =>
                  PM.Close (Popup);
                  Dirty_All (State);
            end case;
            Dirty (State, Was);
            Dirty (State, PM.Covered (Popup));
            if Is_Choice (Chosen) then
               Run_Command (State, Of_Choice (Chosen));
            end if;
            if Handled or else not PM.Is_Open (Popup) then
               Redraw := not Is_Empty (State.Dirty);
               return;
            end if;
         end;
      end if;
      declare
         Used : Boolean;
      begin
         Prompt_Input (State, Item, Map, Used);
         if Used then
            Redraw := not Is_Empty (State.Dirty);
            return;
         end if;
      end;
      if Item.Kind = Key_Event then
         --  A pressed key flashes its key bar cell or toolbar button.
         Bars.Flash (Key_Bar, Bar_Map, Chord_Of (Item), Now_Ms, Flash_Damage);
         Dirty (State, Flash_Damage);
         Bars.Flash (Tool_Bar, Panes_Map, Chord_Of (Item), Now_Ms, Flash_Damage);
         Dirty (State, Flash_Damage);
         --  The panes' cancel (Esc) stops a running operation first.
         Maps.Find (Panes_Map, Chord_Of (Item), Found, Chosen_Command);
         if Found and then Chosen_Command = Clear_Or_Cancel and then Files_Operations.Busy then
            Files_Operations.Cancel;
            Dirty (State, Status_Area);
            Redraw := True;
            return;
         end if;
      end if;
      if Viewing then
         if Item.Kind = Key_Event then
            Maps.Find (Viewer_Map, Chord_Of (Item), Found, Chosen_Command);
            if Found then
               Run_Viewer (State, Chosen_Command);
            end if;
         elsif Item.Kind = Wheel then
            Viewer_Top := Natural'Max (0, Natural'Min (Viewer_Top - Item.Steps * WHEEL_ROWS,
                                                       Natural'Max (0, Line_Count - Viewer_Rows)));
            Dirty_All (State);
         elsif Item.Kind = Resize then
            Dirty_All (State);
         elsif Item.Kind = Pointer_Event then
            Bars.Take_Chosen (Key_Bar, Map, Viewer_Map, Found, Chosen_Command);
            if Found and then Enabled (Chosen_Command) then
               Run_Viewer (State, Chosen_Command);
            end if;
         end if;
         Redraw := not Is_Empty (State.Dirty);
         return;
      end if;
      case Item.Kind is
         when Resize =>
            Dirty_All (State);
         when Key_Release =>
            null;
         when Key_Event =>
            if Found and then Enabled (Chosen_Command) then
               Run (State, Chosen_Command, Map);
            end if;
         when Text_Event =>
            if not Item.Control and then not Item.Alt and then Item.Character_Value in '!' .. '~' then
               Type_Filter (State, Which, Files_Filter.Query (State.Panes (Which).Filter) & Item.Character_Value);
            end if;
         when Wheel =>
            --  The toolkit's scroll containers take the wheel (UI-035): the
            --  pane under the pointer scrolls through its scrollbar.
            declare
               Taken : Boolean;
               Wheel_Damage : Rect;
            begin
               Controls.Route_Wheel (Map, Item.X, Item.Y, Item.Steps, Taken, Wheel_Damage);
               Dirty (State, Wheel_Damage);
            end;
         when Pointer_Event =>
            if Item.Action = Controls.Pointer_Press and then Item.Secondary
              and then (for some W in Side range 1 .. State.Count => Point_In_Rect (Item.X, Item.Y, State.Panes (W).Header_Area))
            then
               for W in Side range 1 .. State.Count loop
                  if Point_In_Rect (Item.X, Item.Y, State.Panes (W).Header_Area) then
                     Open_Columns_Menu (State, W, Item.X, Item.Y);
                  end if;
               end loop;
            elsif Item.Action = Controls.Pointer_Press and then Item.Secondary then
               for W in Side range 1 .. State.Count loop
                  declare
                     Q : Pane_State renames State.Panes (W).all;
                  begin
                     if Point_In_Rect (Item.X, Item.Y, Q.Rows_Area) then
                        declare
                           Row : constant Natural := Q.View.Top + (Item.Y - Q.Rows_Area.y) / ROW_HEIGHT + 1;
                           On_Row : constant Boolean := Row in 1 .. Q.View.Count;
                        begin
                           if W /= State.Current then
                              State.Current := W;
                              Dirty_All (State);
                           end if;
                           if On_Row and then not Marked_Row (Q, Row) then
                              VP.Go (Q.View, Row);
                              Remember_Cursor (Q);
                              Dirty (State, Q.Rows_Area);
                           end if;
                           Open_Menu (State, W, (if On_Row and then Row > Parent_Rows (Q) then Row else 0),
                                      Item.X, Item.Y);
                        end;
                     end if;
                  end;
               end loop;
            elsif Item.Action = Controls.Pointer_Press and then Point_In_Rect (Item.X, Item.Y, Drawer_Area) then
               declare
                  Row : constant Natural := CuBit.UI.Drawers.Row_At (Shortcuts, Drawer_Area, Item.X, Item.Y);
               begin
                  if Row > 0 then
                     declare
                        Value : constant Natural := Shortcuts.Rows (Row).Value;
                        Where : constant DP.Path :=
                          (if Value > RECENT_VALUE then Recent (Value - RECENT_VALUE).Where
                           elsif Value > BOOKMARK_VALUE then Bookmarks (Value - BOOKMARK_VALUE).Where
                           else Places (Value - PLACE_VALUE).Where);
                     begin
                        Go_To (State, State.Current, Where, "");
                     end;
                  end if;
               end;
            elsif Item.Action = Controls.Pointer_Press
              and then (for some W in Side range 1 .. State.Count => Point_In_Rect (Item.X, Item.Y, State.Panes (W).Tabs_Area))
            then
               for W in Side range 1 .. State.Count loop
                  declare
                     Q : Pane_State renames State.Panes (W).all;
                     Was_Current : constant Boolean := State.Current = W;
                  begin
                     if Point_In_Rect (Item.X, Item.Y, Q.Tabs_Area) then
                        State.Current := W;
                        for K in 1 .. Q.Tab_Count loop
                           if Point_In_Rect (Item.X, Item.Y, Tab_Close_Box (Q, K)) or else
                             (Item.Middle and then Point_In_Rect (Item.X, Item.Y, Tab_Box (Q, K)))
                           then
                              --  Closes on release over the same tab.
                              Q.Close_Press := K;
                           elsif Point_In_Rect (Item.X, Item.Y, Tab_Box (Q, K)) then
                              Q.Tab_Press := K;
                              if K /= Q.Current_Tab then
                                 Show_Tab (State, W, K);
                              end if;
                           end if;
                        end loop;
                        if Q.Close_Press > 0 and then Was_Current then
                           --  Only the pressed x changes face.
                           Dirty (State, Q.Tabs_Area);
                        else
                           Dirty_All (State);
                        end if;
                     end if;
                  end;
               end loop;
            elsif Item.Action = Controls.Pointer_Press then
               declare
                  Target : constant Controls.Control_ID := Controls.Hit (Map, Item.X, Item.Y);
               begin
                  for W in Side range 1 .. State.Count loop
                     State.Panes (W).Header_Press := 0;
                     if Target >= COLUMNS_BASE (W) + Tables.MAX_COLUMNS
                       and then Target < COLUMNS_BASE (W) + 2 * Tables.MAX_COLUMNS
                     then
                        State.Panes (W).Header_Press := Target - COLUMNS_BASE (W) - Tables.MAX_COLUMNS + 1;
                     end if;
                  end loop;
               end;
               for W in Side range 1 .. State.Count loop
                  declare
                     Q : Pane_State renames State.Panes (W).all;
                  begin
                     if Point_In_Rect (Item.X, Item.Y, Q.Rows_Area) or else Point_In_Rect (Item.X, Item.Y, Q.Path_Area)
                     then
                        if W /= State.Current then
                           State.Current := W;
                           Dirty_All (State);
                        end if;
                     end if;
                     if Point_In_Rect (Item.X, Item.Y, Q.Rows_Area) then
                        declare
                           Row : constant Natural := Q.View.Top + (Item.Y - Q.Rows_Area.y) / ROW_HEIGHT + 1;
                        begin
                           if Row in 1 .. Q.View.Count then
                              if Item.Control then
                                 Set_Mark (Q, Row, not Marked_Row (Q, Row));
                              elsif Item.Shift and then Q.View.Cursor > 0 then
                                 for R in Natural'Min (Row, Q.View.Cursor) .. Natural'Max (Row, Q.View.Cursor) loop
                                    Set_Mark (Q, R, True);
                                 end loop;
                              end if;
                              VP.Go (Q.View, Row);
                              Remember_Cursor (Q);
                              Dirty (State, Q.Rows_Area);
                              Dirty (State, Q.Path_Area);
                              Dirty (State, Status_Area);
                              if State.Last_Press_Side = W and then State.Last_Press_Row = Row
                                and then Item.Time_Ms >= State.Last_Press_Ms
                                and then Item.Time_Ms - State.Last_Press_Ms <= DOUBLE_CLICK_MS
                                and then not Item.Control and then not Item.Shift
                              then
                                 State.Last_Press_Row := 0;
                                 Open_Cursor (State, W);
                              else
                                 State.Last_Press_Side := W;
                                 State.Last_Press_Row := Row;
                                 State.Last_Press_Ms := Item.Time_Ms;
                              end if;
                           end if;
                        end;
                     end if;
                  end;
               end loop;
            elsif Item.Action = Controls.Pointer_Release then
               declare
                  Target : constant Controls.Control_ID := Controls.Hit (Map, Item.X, Item.Y);
                  Changed : Boolean;
               begin
                  --  A tab dragged onto another: it moves there.
                  for W in Side range 1 .. State.Count loop
                     declare
                        Q : Pane_State renames State.Panes (W).all;
                     begin
                        if Q.Close_Press > 0 then
                           if Q.Close_Press <= Q.Tab_Count and then
                             (Point_In_Rect (Item.X, Item.Y, Tab_Close_Box (Q, Q.Close_Press))
                              or else (Item.Middle and then Point_In_Rect (Item.X, Item.Y, Tab_Box (Q, Q.Close_Press))))
                           then
                              Close_Tab_At (State, W, Q.Close_Press);
                           end if;
                           Q.Close_Press := 0;
                           Dirty_All (State);
                        end if;
                        if Q.Tab_Press > 0 then
                           for K in 1 .. Q.Tab_Count loop
                              if K /= Q.Tab_Press and then Point_In_Rect (Item.X, Item.Y, Tab_Box (Q, K)) then
                                 Move_Tab (State, W, Q.Tab_Press, K);
                              end if;
                           end loop;
                           Q.Tab_Press := 0;
                        end if;
                     end;
                  end loop;
                  --  A header dragged onto another: its column moves there.
                  for W in Side range 1 .. State.Count loop
                     declare
                        Q : Pane_State renames State.Panes (W).all;
                     begin
                        if Q.Header_Press > 0 and then Target >= COLUMNS_BASE (W) + Tables.MAX_COLUMNS
                          and then Target < COLUMNS_BASE (W) + 2 * Tables.MAX_COLUMNS
                          and then Target - COLUMNS_BASE (W) - Tables.MAX_COLUMNS + 1 /= Q.Header_Press
                        then
                           Move_Column (Q, Q.Header_Press, Target - COLUMNS_BASE (W) - Tables.MAX_COLUMNS + 1);
                           Dirty_All (State);
                        end if;
                        Q.Header_Press := 0;
                     end;
                  end loop;
                  for W in Side range 1 .. State.Count loop
                     Tables.Handle_Header_Release
                       (State.Panes (W).Columns, Map, COLUMNS_BASE (W), Target, Changed);
                     if Changed then
                        State.Current := W;
                        declare
                           Q : Pane_State renames State.Panes (W).all;
                           Direction : constant Files_Order.Sort_Direction :=
                             (if Q.Columns.Order = Tables.Ascending then Files_Order.Ascending
                              else Files_Order.Descending);
                        begin
                           if Q.Columns.Sort_Column in 1 .. Q.Columns.Count
                             and then COLUMN_KEYS (Q.Kinds (Q.Columns.Sort_Column))
                           then
                              Files_Order.Set_Rule
                                (Q.Order, (Key => Key_Of (Q.Kinds (Q.Columns.Sort_Column)), Direction => Direction));
                           else
                              --  A column without an order: the sort stays.
                              Q.Columns.Sort_Column := Column_Of_Key (Q, Files_Order.Rule (Q.Order).Key);
                           end if;
                        end;
                        Dirty_All (State);
                     end if;
                  end loop;
                  --  A clicked key bar cell or toolbar button runs its command.
                  Bars.Take_Chosen (Key_Bar, Map, Panes_Map, Found, Chosen_Command);
                  if Found and then Enabled (Chosen_Command) then
                     Run (State, Chosen_Command, Map);
                  end if;
                  Bars.Take_Chosen (Tool_Bar, Map, Panes_Map, Found, Chosen_Command);
                  if Found and then Enabled (Chosen_Command) then
                     Run (State, Chosen_Command, Map);
                     Dirty_All (State);
                  end if;
               end;
            end if;
      end case;
      Redraw := State.Dirty /= Before or else not Is_Empty (State.Dirty);
   end Handle;

   ---------------------------------------------------------------------------
   --  Drawing.
   ---------------------------------------------------------------------------
   ICON_INDENT : constant := CuBit.UI.Icons.ICON_SIZE + 5;

   function Right_Columns (P : Pane_State) return Tables.Column_Flags is
      Result : Tables.Column_Flags := Tables.NO_COLUMNS;
   begin
      for Column in 1 .. P.Columns.Count loop
         Result (Column) := P.Kinds (Column) in Size_Column | Owner_Column;
      end loop;
      return Result;
   end Right_Columns;

   function Extension_Text (Name : String; At_Byte : Natural) return String is
     (if At_Byte = 0 or else At_Byte > Name'Length then "" else Name (Name'First + At_Byte - 1 .. Name'Last));

   --  Permission bits as ls shows them ("drwxr-xr-x").
   function Mode_Text (Mode : Unsigned_32) return String is
      TYPE_MASK : constant := 8#170000#;
      DIRECTORY_TYPE : constant := 8#040000#;
      LINK_TYPE : constant := 8#120000#;
      LETTERS : constant String := "rwxrwxrwx";
      Result : String (1 .. 10) := [others => '-'];
   begin
      Result (1) := (if (Mode and TYPE_MASK) = DIRECTORY_TYPE then 'd'
                     elsif (Mode and TYPE_MASK) = LINK_TYPE then 'l' else '-');
      for K in 0 .. 8 loop
         if (Mode and Shift_Left (1, 8 - K)) /= 0 then
            Result (K + 2) := LETTERS (K + 1);
         end if;
      end loop;
      return Result;
   end Mode_Text;

   --  A file's icon by its extension (folders are drawn as folders).
   function Icon_For (Name : String; Extension_At : Natural) return CuBit.UI.Icons.Icon is
      function Lower (C : Character) return Character is
        (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
      Raw : constant String :=
        (if Extension_At = 0 or else Extension_At > Name'Length then "" else Name (Name'First + Extension_At - 1 .. Name'Last));
      Ext : String (1 .. Raw'Length);
      function Has (Text : String) return Boolean is (Ext = Text);
   begin
      for K in Raw'Range loop
         Ext (K - Raw'First + 1) := Lower (Raw (K));
      end loop;
      if Has ("png") or else Has ("jpg") or else Has ("jpeg") or else Has ("gif") or else Has ("bmp") or else Has ("svg")
        or else Has ("webp") or else Has ("ppm") or else Has ("ico")
      then
         return CuBit.UI.Icons.Image_File;
      elsif Has ("zip") or else Has ("tar") or else Has ("gz") or else Has ("xz") or else Has ("bz2") or else Has ("7z")
        or else Has ("zst") or else Has ("iso") or else Has ("img")
      then
         return CuBit.UI.Icons.Archive_File;
      elsif Has ("app") or else Has ("elf") or else Has ("exe") or else Has ("svc") or else Has ("so") or else Has ("a")
        or else Has ("o")
      then
         return CuBit.UI.Icons.Program_File;
      elsif Has ("txt") or else Has ("md") or else Has ("adb") or else Has ("ads") or else Has ("c") or else Has ("h")
        or else Has ("py") or else Has ("rs") or else Has ("toml") or else Has ("json") or else Has ("ccl")
        or else Has ("gpr") or else Has ("pdf") or else Has ("html") or else Has ("sh") or else Has ("nix")
        or else Has ("log") or else Has ("yaml") or else Has ("xml") or else Has ("ld") or else Has ("s")
      then
         return CuBit.UI.Icons.Document_File;
      end if;
      return CuBit.UI.Icons.File;
   end Icon_For;
   function Intersects (A, B : Rect) return Boolean is
     (not Is_Empty (A) and then not Is_Empty (B) and then A.x < B.x + B.w and then B.x < A.x + A.w
      and then A.y < B.y + B.h and then B.y < A.y + A.h);

   function Tab_Box (P : Pane_State; K : Positive) return Rect is
      Width : constant Natural :=
        (if P.Tab_Count = 0 then 0 else Natural'Min (TAB_WIDTH, P.Tabs_Area.w / Natural'Max (1, P.Tab_Count)));
   begin
      return (P.Tabs_Area.x + (K - 1) * Width, P.Tabs_Area.y, Width, P.Tabs_Area.h);
   end Tab_Box;

   function Tab_Close_Box (P : Pane_State; K : Positive) return Rect is
      Box : constant Rect := Tab_Box (P, K);
   begin
      if Box.w < TAB_CLOSE_SIZE * 3 or else Box.h < TAB_CLOSE_SIZE then
         return (others => 0);
      end if;
      return (Box.x + Box.w - TAB_CLOSE_SIZE - TAB_CLOSE_INSET, Box.y + (Box.h - TAB_CLOSE_SIZE) / 2,
              TAB_CLOSE_SIZE, TAB_CLOSE_SIZE);
   end Tab_Close_Box;

   procedure Draw_Pane
     (State : in out View_State; Which : Side; C : Canvas; Area, Damage : Rect;
      UI : in out CuBit.UI.State.UI_State; Map : in out Controls.Control_Map)
   is
      Colors : constant Theme := Current_Theme;
      P : Pane_State renames State.Panes (Which).all;
      Is_Active : constant Boolean := State.Current = Which;
      Strip : constant Natural := (if State.Panes (Which).Tab_Count > 1 then TAB_HEIGHT else 0);
      Tab_Bar : constant Rect := (Area.x, Area.y, Area.w, Strip);
      Path_Bar : constant Rect := (Area.x, Area.y + Strip, Area.w, PATH_HEIGHT);
      Table : constant Rect :=
        (Area.x, Area.y + Strip + PATH_HEIGHT, Area.w,
         (if Area.h > Strip + PATH_HEIGHT then Area.h - Strip - PATH_HEIGHT else 0));
      Regions : constant Table_Regions := Layout_Table (Table);
      Rows : constant VP.Page_Rows := Positive'Max (1, Regions.Rows.h / ROW_HEIGHT);
      Scrolls : constant Boolean := Row_Total (P) > Rows;
      Row_Width : constant Natural :=
        (if Scrolls and then Regions.Rows.w > SCROLLBAR_WIDTH + 1 then Regions.Rows.w - SCROLLBAR_WIDTH - 1
         else Regions.Rows.w);
      Rows_Box : constant Rect := (Regions.Rows.x, Regions.Rows.y, Row_Width, Regions.Rows.h);
      Widget : Widget_Result;
      function Title (Column : Tables.Column_Index) return String is (COLUMN_CAPTIONS (P.Kinds (Column)).all);
      procedure Header is new Tables.Columns_Header (Title);
   begin
      P.Path_Area := Path_Bar;
      P.Rows_Area := Rows_Box;
      P.Header_Area := Regions.Header;
      P.Tabs_Area := Tab_Bar;
      if Strip > 0 then
         Controls.Add_Surface (Map, TABS_ID (Which), Tab_Bar);
         if Intersects (Tab_Bar, Damage) then
            Draw_Tab_Strip (C, Tab_Bar, Colors);
            for K in 1 .. P.Tab_Count loop
               declare
                  Box : constant Rect := Tab_Box (P, K);
                  Where : constant DP.Path := (if K = P.Current_Tab then P.Where else P.Tabs (K).Where);
                  Close : constant Rect := Tab_Close_Box (P, K);
                  Hot_Close : constant Boolean :=
                    UI.pointer.enabled and then Point_In_Rect (UI.pointer.x, UI.pointer.y, Close);
               begin
                  Draw_Tab (C, Box, Colors, K = P.Current_Tab,
                            UI.pointer.enabled and then Point_In_Rect (UI.pointer.x, UI.pointer.y, Box), False,
                            Caption_Of (Where));
                  if not Is_Empty (Close) then
                     Tooltips.Set_Region (Tips, Close, "Close tab", Maps.Hint (Panes_Map, Close_Tab));
                     if Hot_Close or else P.Close_Press = K then
                        Fill_Rect (C, Close, (if P.Close_Press = K then Colors.shadow else Colors.edge));
                     end if;
                     CuBit.UI.Icons.Draw (C, Close.x, Close.y, CuBit.UI.Icons.Close);
                  end if;
               end;
            end loop;
         end if;
      end if;
      if Rows /= P.View.Rows then
         VP.Fit (P.View, Rows);
      end if;
      --  The path bar: the active pane in the active title's colors.
      Controls.Add_Surface (Map, PATH_ID (Which), Path_Bar);
      if Intersects (Path_Bar, Damage) then
         if Is_Active then
            Fill_Vertical_Gradient (C, Path_Bar, Colors.activeTitleTop, Colors.activeTitleBottom);
         else
            Fill_Vertical_Gradient (C, Path_Bar, Colors.inactiveTitleTop, Colors.inactiveTitleBottom);
         end if;
         declare
            Text_Y : constant Natural :=
              Path_Bar.y + (if PATH_HEIGHT > UI_Text_Height then (PATH_HEIGHT - UI_Text_Height) / 2 else 0);
            Counts : constant String :=
              (if P.Load in Opening | Reading then "listing " & Thousands (Unsigned_64 (P.Listing.Count)) & "  "
               else "")
              & Thousands (Unsigned_64 (Files_Marks.Count (P.Marks))) & " / "
              & Thousands (Unsigned_64 (Shown_Count (P)))
              & (if P.Truncated then " (full)" else "");
            Counts_Width : constant Natural := UI_Text_Width (Counts);
            Room : constant Natural :=
              (if Path_Bar.w > Counts_Width + 3 * TEXT_INSET then Path_Bar.w - Counts_Width - 3 * TEXT_INSET else 0);
            Text : constant String := Full_Path (P);
            Ink : constant Color := (if Is_Active then Colors.selectionText else Colors.text);
         begin
            --  The path's tail after "..." when it is too long: the folder
            --  you are in.
            Draw_UI_Text_Transparent
              (With_Clip (C, (Path_Bar.x, Path_Bar.y, Room + TEXT_INSET, PATH_HEIGHT)), Path_Bar.x + TEXT_INSET,
               Text_Y, Fit_UI_Text_Tail (Text, Room), Ink);
            Draw_UI_Text_Transparent
              (With_Clip (C, Path_Bar), Path_Bar.x + Path_Bar.w - Counts_Width - TEXT_INSET, Text_Y, Counts, Ink);
         end;
      end if;

      --  The table: frame, header, scrollbar, rows.
      if Intersects (Table, Damage) then
         Draw_Table_Viewport_Frame (C, Table, Colors);
      end if;
      --  The header's columns line up with the rows' (left of the scrollbar).
      Header (C, UI, Map, COLUMNS_BASE (Which), Regions.Header, Table, Colors, P.Columns,
              Scrollbar_Room => Regions.Rows.w - Row_Width);
      if Scrolls then
         declare
            Value : Natural := P.View.Top;
         begin
            CuBit.UI.Widgets.Vertical_Scrollbar
              (C, UI, Map, SCROLL_ID (Which),
               (Regions.Rows.x + Regions.Rows.w - SCROLLBAR_WIDTH, Regions.Rows.y, SCROLLBAR_WIDTH, Regions.Rows.h),
               Table, Colors, 0, Row_Total (P) - 1, Value, Widget, pageSize => Rows, retainedInput => True,
               --  UI-035: the wheel over the rows scrolls them (the
               --  toolkit routes it), unless a menu or prompt is up.
               wheelArea =>
                 (if CuBit.UI.Popup_Menus.Is_Open (Popup) or else Prompt /= No_Prompt then (others => 0)
                  else Regions.Rows),
               wheelStep => WHEEL_ROWS);
            VP.Scroll_To (P.View, Natural'Min (Value, Row_Count'Last));
         end;
      end if;
      Controls.Add_Surface (Map, ROWS_ID (Which), Rows_Box);
      for Line in 0 .. Rows - 1 loop
         declare
            Row : constant Natural := P.View.Top + Line + 1;
            Box : constant Rect := (Rows_Box.x, Rows_Box.y + Line * ROW_HEIGHT, Row_Width, ROW_HEIGHT);
         begin
            exit when Row > Row_Total (P);
            if Intersects (Box, Damage) then
               declare
                  Id : constant Entry_Count := Entry_Of (P, Row);
                  Facts : constant Files_Listing.Entry_Facts :=
                    (if Id > 0 then Files_Listing.Facts (P.Listing, Id) else (others => <>));
                  Folder : constant Boolean := Id = 0 or else Facts.Kind = Files_Listing.Directory_Kind;
                  Cursor_Here : constant Boolean := Row = P.View.Cursor;
                  Marked : constant Boolean := Id > 0 and then Files_Marks.Marked (P.Marks, Id);
                  function Cell (Column : Tables.Column_Index) return String is
                    (case P.Kinds (Column) is
                        when Name_Column => (if Id = 0 then ".." else Files_Listing.Name (P.Listing, Id))
                                            & (if Folder and then Id > 0 then "/" else ""),
                        when Extension_Column =>
                          (if Id = 0 or else Folder or else Files_Listing.Extension_At (P.Listing, Id) = 0 then ""
                           else Extension_Text (Files_Listing.Name (P.Listing, Id),
                                                Files_Listing.Extension_At (P.Listing, Id))),
                        when Size_Column => (if Folder then "<DIR>" elsif Facts.Size_Known
                                             then Size_Text (Unsigned_64 (Facts.Size)) else ""),
                        when Modified_Column => (if Facts.Modified_Known then Time_Text (Unsigned_64 (Facts.Modified))
                                                 else ""),
                        when Changed_Column => (if Facts.Changed_Known then Time_Text (Unsigned_64 (Facts.Changed))
                                                else ""),
                        when Kind_Column =>
                          (if Id = 0 then "" else
                             (case Facts.Kind is
                                 when Files_Listing.Directory_Kind => "Folder", when Files_Listing.File_Kind => "File",
                                 when Files_Listing.Link_Kind => "Link", when Files_Listing.Unknown_Kind => "Other")),
                        when Mode_Column => (if Facts.Mode_Known then Mode_Text (Facts.Mode) else ""),
                        when Owner_Column =>
                          (if Facts.Owner_Known then Image (Unsigned_64 (Facts.Owner)) & ":"
                             & Image (Unsigned_64 (Facts.Group)) else ""));
                  function Ink (Column : Tables.Column_Index; Default : Color) return Color is
                    (if Cursor_Here and then Is_Active then Default
                     elsif Marked then Colors.danger
                     elsif Column > 1 then Colors.muted
                     else Default);
                  procedure Draw_Row is new Tables.Draw_Columns_Row (Cell, Ink);
               begin
                  Draw_Row (C, Box, Colors, P.Columns, Cursor_Here and then Is_Active, False,
                            Right_Aligned => Right_Columns (P), First_Indent => ICON_INDENT);
                  CuBit.UI.Icons.Draw
                    (With_Clip (C, Box), Box.x + P.Columns.Cell_Padding, Box.y + (ROW_HEIGHT - CuBit.UI.Icons.ICON_SIZE) / 2,
                     (if Id = 0 then CuBit.UI.Icons.Go_Up
                      elsif Folder then CuBit.UI.Icons.Folder
                      else Icon_For (Files_Listing.Name (P.Listing, Id), Files_Listing.Extension_At (P.Listing, Id))));
                  if Marked then
                     Fill_Rect (C, (Box.x, Box.y, MARK_BAR_WIDTH, ROW_HEIGHT - 1), Colors.danger);
                  end if;
                  --  The other pane's cursor: a two-pixel outline in the
                  --  accent colour, which reads on light and dark.
                  if Cursor_Here and then not Is_Active then
                     Stroke_Rect (C, Box, Colors.accent, Colors.accent);
                     if Box.w > 2 and then Box.h > 2 then
                        Stroke_Rect (C, (Box.x + 1, Box.y + 1, Box.w - 2, Box.h - 2), Colors.accent,
                                     Colors.accent);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
      declare
         Drawn : constant Natural :=
           Natural'Min (Rows, (if Row_Total (P) > P.View.Top then Row_Total (P) - P.View.Top else 0));
         Below : constant Natural := Rows_Box.y + Drawn * ROW_HEIGHT;
         Rest : constant Rect :=
           (Rows_Box.x, Below, Row_Width,
            (if Rows_Box.y + Rows_Box.h > Below then Rows_Box.y + Rows_Box.h - Below else 0));
      begin
         if Intersects (Rest, Damage) then
            Fill_Rect (C, Rest, Colors.field);
            if P.Load = Failed then
               --  Why, in words (Files_Status): muted when it is expected
               --  (a volume this machine lacks, a folder that is gone), the
               --  danger ink only for a genuine error.
               declare
                  use type Files_Status.Severity;
                  Kind : constant Files_Status.Status_Kind := Pane_Failure (P);
               begin
                  Draw_UI_Text_Fitted
                    (C, (Rest.x, Rest.y, (if Rest.w > TEXT_INSET then Rest.w - TEXT_INSET else 0), Rest.h),
                     Rest.x + TEXT_INSET, Rest.y + TEXT_INSET,
                     (if Full_Path (P)'Length = 0 then "No volume granted to Files is available."
                      else Files_Status.Listing_Text (Kind)),
                     (if Files_Status.Severity_Of (Kind) = Files_Status.Expected then Colors.muted
                      else Colors.danger),
                     Colors.field);
               end;
            elsif P.Load in Opening | Reading and then Row_Total (P) = 0 then
               Draw_UI_Text (With_Clip (C, Rest), Rest.x + TEXT_INSET, Rest.y + TEXT_INSET, "Listing...",
                             Colors.muted, Colors.field);
            end if;
         end if;
      end;
   end Draw_Pane;

   procedure Draw_Viewer (C : Canvas; Area, Damage : Rect) is
      Colors : constant Theme := Current_Theme;
      Header : constant Rect := (Area.x, Area.y, Area.w, VIEW_HEADER_HEIGHT);
      Body_Area : constant Rect :=
        (Area.x, Area.y + VIEW_HEADER_HEIGHT, Area.w, (if Area.h > VIEW_HEADER_HEIGHT then Area.h - VIEW_HEADER_HEIGHT else 0));
      Line_Height : constant Positive := Positive'Max (1, Code_Text_Height + 2);
      State_Text : constant String :=
        (case Files_Reader.State is
            when Files_Reader.Opening | Files_Reader.Reading =>
              "reading " & Thousands (Unsigned_64 (Files_Reader.Length)) & " bytes...",
            when Files_Reader.Done =>
              Thousands (Unsigned_64 (Files_Reader.Length)) & " bytes"
              & (if Files_Reader.Truncated then " (first " & Thousands (Files_Reader.VIEW_LIMIT) & ")" else "")
              & (if Viewer_Hex then "  hex" else "  text"),
            when others => "");
   begin
      Viewer_Rows := Positive'Max (1, Body_Area.h / Line_Height);
      if Intersects (Header, Damage) then
         Fill_Vertical_Gradient (C, Header, Colors.activeTitleTop, Colors.activeTitleBottom);
         --  The file name elided to stop TEXT_INSET short of the state.
         Draw_UI_Text_Transparent (With_Clip (C, Header), Header.x + TEXT_INSET, Header.y + 4,
                                   Fit_UI_Text (Viewer_Name (1 .. Viewer_Name_Length),
                                     (if Header.w > UI_Text_Width (State_Text) + 3 * TEXT_INSET
                                      then Header.w - UI_Text_Width (State_Text) - 3 * TEXT_INSET else 0)),
                                   Colors.selectionText);
         Draw_UI_Text_Transparent
           (With_Clip (C, Header), Header.x + Header.w - UI_Text_Width (State_Text) - TEXT_INSET, Header.y + 4,
            State_Text, Colors.selectionText);
      end if;
      if Intersects (Body_Area, Damage) then
         Fill_Rect (C, Body_Area, Colors.field);
         if Files_Reader.State = Files_Reader.Done then
            for Row in 0 .. Viewer_Rows - 1 loop
               exit when Viewer_Top + Row + 1 > Line_Count;
               Draw_Code_Text
                 (With_Clip (C, Body_Area), Body_Area.x + TEXT_INSET, Body_Area.y + 2 + Row * Line_Height,
                  View_Line (Viewer_Top + Row + 1), Colors.text, Colors.field);
            end loop;
         end if;
      end if;
   end Draw_Viewer;

   function Quoted (Text : String) return String is ('"' & Text & '"');

   procedure Draw_Dialog
     (State : View_State; C : Canvas; Bounds : Rect; UI : CuBit.UI.State.UI_State; Map : in out Controls.Control_Map)
   is
      Colors : constant Theme := Current_Theme;
      Box : constant Rect :=
        (Bounds.x + (if Bounds.w > DIALOG_WIDTH then (Bounds.w - DIALOG_WIDTH) / 2 else 0),
         Bounds.y + (if Bounds.h > DIALOG_HEIGHT then (Bounds.h - DIALOG_HEIGHT) / 3 else 0),
         Natural'Min (DIALOG_WIDTH, Bounds.w), Natural'Min (DIALOG_HEIGHT, Bounds.h));
      Title_Bar : constant Rect := (Box.x, Box.y, Box.w, PATH_HEIGHT);
      Inner : constant Canvas := With_Clip (C, Box);
      P : Pane_State renames State.Panes (Prompt_Side).all;
      Count : constant Natural := Source_Count (P);
      What : constant String :=
        (if Count = 1 and then Files_Marks.Count (P.Marks) = 0 and then P.View.Cursor > 0
           and then Entry_Of (P, P.View.Cursor) > 0
         then Quoted (Files_Listing.Name (P.Listing, Entry_Of (P, P.View.Cursor)))
         else Thousands (Unsigned_64 (Count)) & " items");
      Destination : constant String := Full_Path (State.Panes (Other (State, Prompt_Side)).all);
      Title : constant String :=
        (case Prompt is
            when Confirm_Copy => "Copy", when Confirm_Move => "Move", when Confirm_Delete => "Delete",
            when Ask_Folder_Name => "New folder", when Ask_New_Name => "Rename",
            when Ask_Conflict => "Name conflict", when No_Prompt => "");
      Line_1 : constant String :=
        (case Prompt is
            when Confirm_Copy => "Copy " & What & " to",
            when Confirm_Move => "Move " & What & " to",
            when Confirm_Delete => "Delete " & What & " from " & Full_Path (P) & "?",
            when Ask_Folder_Name => "Name of the new folder in " & Full_Path (P) & ":",
            when Ask_New_Name => "New name:",
            when Ask_Conflict => Quoted (Files_Operations.Current) & " exists in the target.",
            when No_Prompt => "");
      Line_2 : constant String :=
        (case Prompt is
            when Confirm_Copy | Confirm_Move => Destination,
            when Confirm_Delete => "This cannot be undone.",
            when Ask_Conflict => "Shift with a choice: for every name that exists.",
            when others => "");
      Line_3 : constant String :=
        (case Prompt is
            when Confirm_Copy | Confirm_Move => "If a name exists:",
            when others => "");
      Line_Height : constant Natural := UI_Text_Height + 6;
      Y : Natural := Title_Bar.y + PATH_HEIGHT + 10;
   begin
      Dialog_Area := Box;
      Fill_Rect (C, Box, Colors.face);
      --  A dark outer edge, then the bevel inside it: the dialog stands
      --  apart from the panes in the dark scheme as well as the light one.
      Stroke_Rect (C, Box, Colors.darkShadow, Colors.darkShadow);
      if Box.w > 2 and then Box.h > 2 then
         Stroke_Rect (C, (Box.x + 1, Box.y + 1, Box.w - 2, Box.h - 2), Colors.highlight, Colors.edge);
      end if;
      Fill_Vertical_Gradient (C, Title_Bar, Colors.activeTitleTop, Colors.activeTitleBottom);
      Draw_UI_Text_Transparent (Inner, Title_Bar.x + TEXT_INSET, Title_Bar.y + 4, Title, Colors.selectionText);
      Draw_UI_Text_Transparent (Inner, Box.x + 16, Y, Line_1, Colors.text);
      Y := Y + Line_Height;
      if Prompt in Ask_Folder_Name | Ask_New_Name then
         Draw_Text_Edit_Field (Inner, (Box.x + 16, Y, Box.w - 32, SEARCH_HEIGHT), Colors, Input (1 .. Input_Length),
                               Input_Length, Input_Length, Input_Length, True, False);
      else
         Draw_UI_Text_Transparent (Inner, Box.x + 16, Y, Line_2,
                                   (if Prompt = Confirm_Delete then Colors.danger else Colors.text));
         Y := Y + Line_Height;
         Draw_UI_Text_Transparent (Inner, Box.x + 16, Y, Line_3, Colors.muted);
      end if;
      --  The prompt's choices are toolkit buttons with their keycaps
      --  (UI-051): "OK [Enter]", "Cancel [Esc]", "Skip [S]".
      Bars.Clear (Dialog_Bar, DIALOG_FIRST, Bars.Plain_Style, Bars.Align_Right);
      if Prompt = Ask_Conflict then
         Bars.Add_Command (Dialog_Bar, Skip_Existing);
         Bars.Add_Command (Dialog_Bar, Overwrite_Existing);
         Bars.Add_Command (Dialog_Bar, Keep_Both);
      else
         Bars.Add_Command (Dialog_Bar, Prompt_Accept);
      end if;
      Bars.Add_Command (Dialog_Bar, Prompt_Cancel);
      declare
         Row : constant Natural := Bars.Height (Dialog_Bar, Prompt_Map, Box.w - 2 * DIALOG_INSET, Revealed_Now);
      begin
         Bars.Draw (Inner, Map, Tips, Dialog_Bar, Prompt_Map,
                    (Box.x + DIALOG_INSET, Box.y + Box.h - Row - DIALOG_INSET, Box.w - 2 * DIALOG_INSET, Row),
                    Colors, UI.pointer, Revealed_Now, Now_Ms);
      end;
      if Prompt in Confirm_Copy | Confirm_Move then
         Bars.Clear (Policy_Bar, POLICY_FIRST, Bars.Plain_Style, Bars.Align_Left);
         Bars.Add_Command (Policy_Bar, Skip_Existing);
         Bars.Add_Command (Policy_Bar, Overwrite_Existing);
         Bars.Add_Command (Policy_Bar, Keep_Both);
         Bars.Add_Command (Policy_Bar, Ask_Each);
         declare
            Left : constant Natural := Box.x + 16 + UI_Text_Width (Line_3) + M.Space_S;
            Row : constant Natural :=
              Bars.Height (Policy_Bar, Prompt_Map, (if Box.x + Box.w > Left then Box.x + Box.w - Left else 0),
                           Revealed_Now);
         begin
            Bars.Draw (Inner, Map, Tips, Policy_Bar, Prompt_Map,
                       (Left, Y + (UI_Text_Height / 2) - Row / 2, (if Box.x + Box.w > Left then Box.x + Box.w - Left else 0),
                        Row), Colors, UI.pointer, Revealed_Now, Now_Ms);
         end;
      end if;
   end Draw_Dialog;

   procedure Draw_Overlay (State : View_State; C : Canvas; Bounds : Rect) is
      Colors : constant Theme := Current_Theme;
      Box : constant Rect :=
        (Bounds.x + (if Bounds.w > OVERLAY_WIDTH + MARGIN then Bounds.w - OVERLAY_WIDTH - MARGIN else 0),
         Bounds.y + PATH_HEIGHT + Table_Header_Height + MARGIN, OVERLAY_WIDTH, OVERLAY_LINES * OVERLAY_LINE + 2 * MARGIN);
      function Rate (Which : Side) return String is
         P : Pane_State renames State.Panes (Which).all;
         Took : constant Unsigned_64 :=
           (if P.Finished_Us > P.Started_Us then P.Finished_Us - P.Started_Us else 0);
      begin
         return "pane" & Side'Image (Which) & ": " & Thousands (Unsigned_64 (P.Listing.Count))
           & (if Took > 0 then " in " & Milliseconds (Took) & " ("
                & Thousands (Unsigned_64 (P.Listing.Count) * MICROSECONDS_PER_SECOND / Took) & "/s)"
              else " listing");
      end Rate;
      Line : Natural := 0;
      procedure Put (Text : String) is
      begin
         Draw_Code_Text_Transparent
           (With_Clip (C, Box), Box.x + MARGIN, Box.y + MARGIN + Line * OVERLAY_LINE, Text, Colors.text);
         Line := Line + 1;
      end Put;
   begin
      Fill_Rect (C, Box, Colors.panel);
      Stroke_Rect (C, Box, Colors.edge, Colors.shadow);
      Put ("frame " & Milliseconds (State.Last_Render_Us) & " (worst " & Milliseconds (State.Worst_Render_Us) & ")");
      Put ("pump " & Milliseconds (State.Last_Pump_Us) & ", work " & Thousands (State.Last_Work));
      Put (Rate (State.Current));
      Put (Rate (Other (State, State.Current)));
      Put ("queue" & Natural'Image (Files_Queue.Outstanding) & " outstanding");
      Put ("frames " & Thousands (State.Frames));
   end Draw_Overlay;


   procedure Draw_Toolbar
     (State : View_State; C : Canvas; Bar, Damage : Rect; UI : CuBit.UI.State.UI_State;
      Map : in out Controls.Control_Map)
   is
      Colors : constant Theme := Current_Theme;
      P : Pane_State renames State.Panes (State.Current).all;
      X : Natural := Bar.x + MARGIN;
      Top : constant Natural := Bar.y + (TOOLBAR_HEIGHT - TOOL_SIZE) / 2;
      Search : constant Rect :=
        (Bar.x + (if Bar.w > SEARCH_WIDTH + MARGIN then Bar.w - SEARCH_WIDTH - MARGIN else 0),
         Bar.y + (TOOLBAR_HEIGHT - SEARCH_HEIGHT) / 2, Natural'Min (SEARCH_WIDTH, Bar.w), SEARCH_HEIGHT);
      Drawn : constant Boolean := Intersects (Bar, Damage);
   begin
      --  The toolbar is a button bar of icon-only buttons (UI-051): their
      --  shortcuts show in the tooltips, under them while Alt is held and
      --  as their controls' accessibility data.
      Bars.Draw (C, Map, Tips, Tool_Bar, Panes_Map, Bar, Colors, UI.pointer, Revealed_Now, Now_Ms);
      Controls.Add (Map, SEARCH_ID, Search, Search, Pointer_Text);
      if Drawn and then Intersects (Search, Damage) then
         declare
            Query : constant String := Files_Filter.Query (P.Filter);
         begin
            Draw_Text_Edit_Field (C, Search, Colors, Query, Query'Length, Query'Length, Query'Length,
                                  Files_Filter.Active (P.Filter),
                                  UI.pointer.enabled and then Point_In_Rect (UI.pointer.x, UI.pointer.y, Search));
            if Query'Length = 0 then
               CuBit.UI.Icons.Draw (With_Clip (C, Search), Search.x + 6, Search.y + (SEARCH_HEIGHT - 16) / 2,
                                    CuBit.UI.Icons.Search);
               Draw_UI_Text_Transparent (With_Clip (C, Search), Search.x + 28, Search.y + (if SEARCH_HEIGHT > UI_Text_Height then (SEARCH_HEIGHT - UI_Text_Height) / 2 else 0),
                                         "Filter: type anywhere", Colors.muted);
            end if;
         end;
      end if;
   end Draw_Toolbar;

   function Shown_Panes (State : View_State) return CuBit.UI.Splits.Part_Count is
      Result : CuBit.UI.Splits.Part_Count := 0;
   begin
      for W in Side range 1 .. State.Count loop
         if State.Shown (W) and then Result < CuBit.UI.Splits.MAXIMUM_PARTS then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Shown_Panes;

   procedure Render
     (State : in out View_State; C : Canvas; Bounds : Rect; UI : in out CuBit.UI.State.UI_State;
      Map : in out Controls.Control_Map)
   is
      Colors : constant Theme := Current_Theme;
      Damage : constant Rect := (if C.clipEnabled then C.clip else Bounds);
      Key_Area : constant Rect :=
        (Bounds.x, Bounds.y + Bounds.h - Key_Bar_Height (Bounds.w), Bounds.w, Key_Bar_Height (Bounds.w));
      Status : constant Rect :=
        (Bounds.x + MARGIN, Key_Area.y - STATUS_HEIGHT, (if Bounds.w > 2 * MARGIN then Bounds.w - 2 * MARGIN else 0),
         STATUS_HEIGHT);
      Bar : constant Rect := (Bounds.x, Bounds.y, Bounds.w, TOOLBAR_HEIGHT);
      Middle_Top : constant Natural := Bar.y + TOOLBAR_HEIGHT + MARGIN;
      Middle : constant Rect :=
        (Bounds.x, Middle_Top, Bounds.w, (if Status.y > Middle_Top + MARGIN then Status.y - Middle_Top - MARGIN else 0));
      Drawer_Box, Content, Edge : Rect;
   begin
      State.Bounds := Bounds;
      Note_State (State);
      Status_Area := Status;
      Toolbar_Area := Bar;
      CuBit.UI.State.Begin_Frame (UI);
      Controls.Clear (Map);
      Tooltips.Clear (Tips);
      CuBit.UI.Drawers.Track_Edge (Drawer, Map, Middle, DRAWER_EDGE_ID);
      CuBit.UI.Drawers.Layout (Drawer, Middle, Drawer_Box, Content, Edge);
      Drawer_Area := Drawer_Box;
      Drawer_Edge := Edge;
      declare
         Panes_Top : constant Natural := Middle.y;
         Panes_Height : constant Natural := Middle.h;
         Panes_Box : constant Rect :=
           (Content.x + MARGIN, Panes_Top, (if Content.w > 2 * MARGIN then Content.w - 2 * MARGIN else 0), Panes_Height);
         Shown_Count : constant Natural := Natural (Shown_Panes (State));
         Parts, Dividers : CuBit.UI.Splits.Rect_Table;
         Widget : Widget_Result;
      begin
      CuBit.UI.Splits.Track (Map, Panes_Box, Shown_Count, State.Shares, PANE_MINIMUM, DIVIDER_FIRST, Parts, Dividers);
      Draw_Toolbar (State, C, Bar, Damage, UI, Map);
      --  The margins and gaps; every section paints its own pixels.
      if Intersects (Damage, Bounds) then
         Fill_Rect (C, (Bounds.x, Bar.y + TOOLBAR_HEIGHT, Bounds.w, MARGIN), Colors.face);
         Fill_Rect (C, (Content.x, Panes_Top, MARGIN, Panes_Height), Colors.face);
         Fill_Rect (C, (Panes_Box.x + Panes_Box.w, Panes_Top,
                        (if Bounds.x + Bounds.w > Panes_Box.x + Panes_Box.w
                         then Bounds.x + Bounds.w - Panes_Box.x - Panes_Box.w else 0), Panes_Height), Colors.face);
         Fill_Rect (C, (Bounds.x, Panes_Top + Panes_Height,
                        Bounds.w, (if Status.y > Panes_Top + Panes_Height then Status.y - Panes_Top - Panes_Height
                                   else 0)), Colors.face);
         Fill_Rect (C, (Bounds.x, Status.y, MARGIN, STATUS_HEIGHT), Colors.face);
         Fill_Rect (C, (Status.x + Status.w, Status.y, MARGIN, STATUS_HEIGHT), Colors.face);
      end if;
      if Intersects (Damage, Drawer_Box) or else Intersects (Damage, Edge) then
         declare
            Hot : constant Natural :=
              (if UI.pointer.enabled then CuBit.UI.Drawers.Row_At (Shortcuts, Drawer_Box, UI.pointer.x, UI.pointer.y)
               else 0);
         begin
            CuBit.UI.Drawers.Draw (C, Map, Drawer_Box, Edge, Shortcuts, 0, Hot, Colors, DRAWER_ID,
                                   Controls.Is_Active (Map, DRAWER_EDGE_ID));
         end;
      elsif not Is_Empty (Drawer_Box) then
         Controls.Add_Surface (Map, DRAWER_ID, Drawer_Box);
      end if;
      if Viewing then
         Draw_Viewer (C, Panes_Box, Damage);
      else
         declare
            Part : Natural := 0;
         begin
            for W in Side range 1 .. State.Count loop
               if State.Shown (W) then
                  Part := Part + 1;
                  Draw_Pane (State, W, C, Parts (Part), Damage, UI, Map);
               else
                  State.Panes (W).Rows_Area := (others => 0);
                  State.Panes (W).Path_Area := (others => 0);
                  State.Panes (W).Header_Area := (others => 0);
                  State.Panes (W).Tabs_Area := (others => 0);
               end if;
            end loop;
         end;
         if Intersects (Damage, Panes_Box) then
            CuBit.UI.Splits.Draw_Dividers (C, Map, Shown_Count, Dividers, Colors, DIVIDER_FIRST,
                                           UI.pointer.x, UI.pointer.y, UI.pointer.enabled);
         end if;
      end if;

      if Intersects (Status, Damage) then
         declare
            P : Pane_State renames State.Panes (State.Current).all;
            Rule : constant Files_Order.Sort_Rule := Files_Order.Rule (P.Order);
            Left_Text : constant String :=
              (if Files_Marks.Count (P.Marks) > 0 then
                 Thousands (Unsigned_64 (Files_Marks.Count (P.Marks))) & " marked, "
                 & Size_Text (Files_Marks.Bytes (P.Marks)) & "   "
               else "")
              & (if Files_Filter.Active (P.Filter) then "filter """ & Files_Filter.Query (P.Filter) & """"
                   & (if Files_Filter.Complete (P.Filter) then "" else " ...") & "   "
                 else "")
              & (if Message_Length > 0 then Message (1 .. Message_Length) else "");
            --  The key hints (UI-051): keycaps from the keymaps, so a
            --  remap shows here too; lowest priority last.
            Hints : CuBit.UI.Status_Lines.Line;
            package SL renames CuBit.UI.Status_Lines;
            Root : constant String := Root_Of (P);
            Right_Text : constant String :=
              Volume_Text (Root) & (if Volume_Text (Root)'Length > 0 then "   " else "")
              & "by " & (case Rule.Key is
                          when Files_Order.By_Name => "name", when Files_Order.By_Extension => "extension",
                          when Files_Order.By_Modified => "modified", when Files_Order.By_Size => "size")
              & (if Rule.Direction = Files_Order.Ascending then " up" else " down")
              & (if Files_Order.Busy (P.Order, P.Listing) then " (sorting)" else "");
         begin
            SL.Clear (Hints);
            if Files_Operations.Busy then
               SL.Add_Hint (Hints, Maps.Hint (Panes_Map, Clear_Or_Cancel), "cancel");
            elsif Message_Length > 0 then
               null;
            elsif Viewing then
               SL.Add_Hint (Hints, Maps.Hint (Viewer_Map, Close_Viewer), "close");
               SL.Add_Hint (Hints, Maps.Hint (Viewer_Map, Viewer_Text_Or_Hex), "text or hex");
               SL.Add_Hint (Hints, Maps.Hint (Viewer_Map, Viewer_Page_Down), "scroll");
            else
               SL.Add_Hint (Hints, Maps.Hint (Panes_Map, Next_Pane), "other pane");
               SL.Add_Hint (Hints, Maps.Hint (Panes_Map, Open_Entry), "open");
               SL.Add_Hint (Hints, Maps.Hint (Panes_Map, Erase_Or_Parent), "up");
               SL.Add_Hint (Hints, Maps.Hint (Panes_Map, Mark_And_Next), "mark");
               SL.Add_Text (Hints, "type to filter");
               SL.Add_Hint (Hints, Maps.Hint (Panes_Map, View), "view");
               SL.Add_Hint (Hints, Maps.Hint (Panes_Map, Toggle_Overlay), "timing");
            end if;
            if Files_Operations.Busy then
               declare
                  use Files_Operations;
                  Verb : constant String :=
                    (if Phase = Scanning then "Counting"
                     elsif Phase = Cancelling then "Cancelling"
                     else (case Kind is
                              when Copy_Operation => "Copying", when Move_Operation => "Moving",
                              when Delete_Operation => "Deleting", when Make_Folder_Operation => "Making",
                              when Rename_Operation => "Renaming"));
                  Text : constant String :=
                    Verb & Natural'Image (Items_Done) & " /" & Natural'Image (Items_Total) & " items, "
                    & Size_Text (Bytes_Done) & " / " & Size_Text (Bytes_Total) & "   " & Current;
                  Bar : constant Rect :=
                    (Status.x + (if Status.w > PROGRESS_WIDTH + MARGIN then Status.w - PROGRESS_WIDTH - MARGIN else 0),
                     Status.y + 4, Natural'Min (PROGRESS_WIDTH, Status.w), STATUS_HEIGHT - 8);
               begin
                  SL.Draw (C, Status, Colors, Text, Hints, "");
                  Draw_Progress_Bar
                    (C, Bar, Colors, 0, 100,
                     Files_Plan.Percent ((if Bytes_Total > 0 then Bytes_Done else Unsigned_64 (Items_Done)),
                                   (if Bytes_Total > 0 then Bytes_Total else Unsigned_64 (Items_Total))));
               end;
            else
               SL.Draw (C, Status, Colors, Left_Text, Hints, Right_Text);
            end if;
         end;
      end if;

      --  The function-key bar (UI-051): toolkit buttons sized to their
      --  captions, wrapping when narrow, showing the held layer's keys.
      Key_Bar_Area := Key_Area;
      if Key_Bar_Shown then
         Bars.Draw (C, Map, Tips, Key_Bar, Bar_Map, Key_Area, Colors, UI.pointer, Revealed_Now, Now_Ms);
      end if;
      if State.Overlay then
         Draw_Overlay (State, C, Bounds);
      end if;
      if Prompt /= No_Prompt then
         Draw_Dialog (State, C, Bounds, UI, Map);
      else
         Dialog_Area := (others => 0);
      end if;
      CuBit.UI.Popup_Menus.Draw (C, Map, Popup, Menu, Colors, POPUP_BASE);
      if not CuBit.UI.Popup_Menus.Is_Open (Popup) then
         Tooltips.Draw (C, Tip, Tips, Bounds, Colors);
      end if;
      --  Held modifiers' reveal labels, over everything.
      Shown_Reveal := Revealed_Now;
      if Prompt = No_Prompt and then not CuBit.UI.Popup_Menus.Is_Open (Popup) then
         Bars.Draw_Reveal (C, Tool_Bar, Panes_Map, Colors, Shown_Reveal, Bounds);
         Reveal_Damage := Bars.Reveal_Area (Tool_Bar, Panes_Map, Shown_Reveal, Bounds);
      else
         Reveal_Damage := (others => 0);
      end if;
      end;
      State.Frames := State.Frames + 1;
      CuBit.UI.State.Finish_Frame (UI);
   end Render;

   procedure Set_Clock (State : in out View_State; Now_Us : Unsigned_64) is
      pragma Unreferenced (State);
   begin
      Files_View.Now_Us := Now_Us;
   end Set_Clock;

   procedure Take_Damage (State : in out View_State; Area : out Rect) is
   begin
      Area := State.Dirty;
      --  The overlay sits over the panes: any change may cover it.
      if State.Overlay and then not Is_Empty (Area) then
         Area := State.Bounds;
      end if;
      State.Dirty := (others => 0);
   end Take_Damage;

   procedure Note_Damage (State : in out View_State; Area : Rect) is
   begin
      Dirty (State, Area);
   end Note_Damage;

   procedure Note_Frame (State : in out View_State; Render_Us, Pump_Us : Unsigned_64) is
   begin
      State.Last_Render_Us := Render_Us;
      State.Worst_Render_Us := Unsigned_64'Max (State.Worst_Render_Us, Render_Us);
      State.Last_Pump_Us := Pump_Us;
   end Note_Frame;

   function Quit_Requested (State : View_State) return Boolean is (State.Quit);

   function Microseconds_Of (Ms : Unsigned_64) return Unsigned_64 is
     (if Ms >= Unsigned_64'Last / MICROSECONDS_PER_MILLISECOND then Unsigned_64'Last
      else Ms * MICROSECONDS_PER_MILLISECOND);
   function Next_Deadline_Us (State : View_State) return Unsigned_64 is
     (Unsigned_64'Min
        (Unsigned_64'Min (Unsigned_64'Min ((if Message_Length > 0 then Message_Until_Us else Unsigned_64'Last),
                                           Files_Reader.Deadline), Files_Operations.Deadline),
         Unsigned_64'Min
           (Microseconds_Of (Tooltips.Next_Deadline (Tip)),
            Unsigned_64'Min (Microseconds_Of (MR.Next_Deadline (Reveal, Now_Ms)),
                             Unsigned_64'Min (Microseconds_Of (Bars.Next_Deadline (Key_Bar)),
                                              Microseconds_Of (Bars.Next_Deadline (Tool_Bar)))))));
   function Waiting_For_IO (State : View_State) return Boolean is
     (Files_Queue.Outstanding > 0
      or else (Files_Operations.Busy and then Files_Operations.Phase /= Files_Operations.Asking)
      or else (for some W in Side range 1 .. State.Count => State.Panes (W).Load in Opening | Reading));
   function Viewer_Open (State : View_State) return Boolean is (Viewing);
   function Viewer_Line (State : View_State; Line : Positive) return String is
     (if Files_Reader.State = Files_Reader.Done and then Line <= Line_Count then View_Line (Line) else "");
   function Status_Message (State : View_State) return String is (Message (1 .. Message_Length));

   ---------------------------------------------------------------------------
   --  Queries.
   ---------------------------------------------------------------------------
   function Active (State : View_State) return Side is (State.Current);
   function Tab_Close_Area (State : View_State; Pane : Side; K : Positive) return Rect is
     (if K <= State.Panes (Pane).Tab_Count and then State.Panes (Pane).Tab_Count > 1
      then Tab_Close_Box (State.Panes (Pane).all, K) else (others => 0));
   function Tooltip_Text (State : View_State) return String is (Tooltips.Text (Tip, Tips));
   function Volume_Space (State : View_State) return String is
     (Volume_Text (Root_Of (State.Panes (State.Current).all)));
   function Keys_Shown (State : View_State) return Boolean is
     (Key_Bar_Shown);
   function Key_Index (Key : Keys.Key) return Natural is
     (if Key in Keys.Function_Key then Keys.Key'Pos (Key) - Keys.Key'Pos (Keys.F1) + 1 else 0);
   function Key_Cell (State : View_State; Key : Keys.Key) return Rect is
     (if Key_Index (Key) = 0 or else not Key_Bar_Shown then (others => 0)
      else Bars.Button_Area (Key_Bar, Key_Index (Key)));
   function Key_Cell_Caption (State : View_State; Key : Keys.Key) return String is
     (if Key_Index (Key) = 0 then "" else Bars.Shown_Caption (Key_Bar, Bar_Map, Key_Index (Key), Shown_Reveal));
   function Key_Cell_Needs (State : View_State; Key : Keys.Key) return Natural is
     (if Key_Index (Key) = 0 then 0 else Bars.Needed_Width (Key_Bar, Bar_Map, Key_Index (Key), Shown_Reveal));
   function Key_Bar_Rows (State : View_State) return Natural is (Bars.Rows (Key_Bar));
   function Menu_Row_Hint (State : View_State; Row : Positive) return Keys.Chord is
     (if Row <= CuBit.UI.Popup_Menus.Items (Menu, CuBit.UI.Popup_Menus.ROOT)
      then CuBit.UI.Popup_Menus.Hint (Menu, CuBit.UI.Popup_Menus.ROOT, Row) else Keys.No_Chord);
   function Tooltip_Hint (State : View_State) return Keys.Chord is (Tooltips.Hint (Tip, Tips));
   function Revealed (State : View_State) return Keys.Modifier_Set is (Shown_Reveal);
   function Tool_Revealed (State : View_State; Item : Tool) return Boolean is
     (Bars.Revealed_At (Tool_Bar, Panes_Map, Tool'Pos (Item) + 1, Shown_Reveal));
   function Path (State : View_State; Pane : Side) return String is (Full_Path (State.Panes (Pane).all));
   function Load (State : View_State; Pane : Side) return Load_State is (State.Panes (Pane).Load);
   function Listed (State : View_State; Pane : Side) return Natural is (State.Panes (Pane).Listing.Count);
   function Rows (State : View_State; Pane : Side) return Natural is (Row_Total (State.Panes (Pane).all));
   function Cursor_Row (State : View_State; Pane : Side) return Natural is (State.Panes (Pane).View.Cursor);
   function Top_Row (State : View_State; Pane : Side) return Natural is (State.Panes (Pane).View.Top);
   function Row_Name (State : View_State; Pane : Side; Row : Positive) return String is
      P : Pane_State renames State.Panes (Pane).all;
   begin
      if Row > Row_Total (P) then
         return "";
      elsif Entry_Of (P, Row) = 0 then
         return "..";
      end if;
      return Files_Listing.Name (P.Listing, Entry_Of (P, Row));
   end Row_Name;
   procedure Take_Clipboard_Request (State : in out View_State; Request : out Clipboard_Request) is
      pragma Unreferenced (State);
   begin
      Request := Pending_Clipboard;
      Pending_Clipboard := No_Request;
   end Take_Clipboard_Request;

   procedure Selection_Copy
     (State : View_State; Form : Copy_Form; Text, Literal : in out Clipboard_Text.Builder; Count : out Natural)
   is
      package CT renames Clipboard_Text;
      P : Pane_State renames State.Panes (State.Current).all;
      procedure Add (Id : Entry_Id) is
         Name : constant String := Files_Listing.Name (P.Listing, Id);
         Child : DP.Path;
         Done : Boolean := True;
      begin
         if Form = Copy_Paths then
            DP.Append_Child (P.Where, Name, Child, Done);
         end if;
         if not Done then
            Text.Fits := False;
            return;
         end if;
         declare
            Item : constant String := (if Form = Copy_Paths then DP.Value (Child) else Name);
         begin
            if Count > 0 then
               CT.Append (Text, [1 => ASCII.LF]);
               CT.Append (Literal, " ");
            end if;
            CT.Append (Text, Item);
            CT.Append_Quoted (Literal, Item);
            Count := Count + 1;
         end;
      end Add;
   begin
      CT.Clear (Text);
      CT.Clear (Literal);
      Count := 0;
      CT.Append (Literal, CT.List_Open);
      if Files_Marks.Count (P.Marks) > 0 then
         --  In the pane's sort order, filtered out or not (as F5 takes them).
         for Position in 1 .. Files_Order.Count (P.Order) loop
            if Files_Marks.Marked (P.Marks, Files_Order.At_Position (P.Order, Position)) then
               Add (Files_Order.At_Position (P.Order, Position));
            end if;
         end loop;
      elsif P.View.Cursor > 0 and then Entry_Of (P, P.View.Cursor) > 0 then
         Add (Entry_Of (P, P.View.Cursor));
      end if;
      CT.Append (Literal, CT.List_Close);
   end Selection_Copy;

   function Prompt_Takes_Text (State : View_State) return Boolean is
      pragma Unreferenced (State);
   begin
      return Prompt in Ask_Folder_Name | Ask_New_Name;
   end Prompt_Takes_Text;

   procedure Edit_Prompt (State : in out View_State) is
      Length : Natural := Input_Length;
      Changed : Boolean;
   begin
      if Prompt not in Ask_Folder_Name | Ask_New_Name then
         return;
      end if;
      Process (Input, Length, Changed);
      if Changed then
         Input_Length := Natural'Min (Length, MAXIMUM_NAME_BYTES);
         Dirty_All (State);
      end if;
   end Edit_Prompt;

   function Cursor_Name (State : View_State; Pane : Side) return String is
     (if State.Panes (Pane).View.Cursor = 0 then "" else Row_Name (State, Pane, State.Panes (Pane).View.Cursor));
   function Marked_Count (State : View_State; Pane : Side) return Natural is
     (Files_Marks.Count (State.Panes (Pane).Marks));
   function Is_Marked (State : View_State; Pane : Side; Row : Positive) return Boolean is
     (Row <= Row_Total (State.Panes (Pane).all) and then Marked_Row (State.Panes (Pane).all, Row));
   function Filter_Text (State : View_State; Pane : Side) return String is
     (Files_Filter.Query (State.Panes (Pane).Filter));
   function Rule (State : View_State; Pane : Side) return Files_Order.Sort_Rule is
     (Files_Order.Rule (State.Panes (Pane).Order));
   function Settled (State : View_State; Pane : Side) return Boolean is
     (State.Panes (Pane).Load in Loaded | Failed | Not_Started
      and then not Files_Order.Busy (State.Panes (Pane).Order, State.Panes (Pane).Listing)
      and then Files_Filter.Complete (State.Panes (Pane).Filter));
   function Listing_Us (State : View_State; Pane : Side) return Unsigned_64 is
     (if State.Panes (Pane).Finished_Us > State.Panes (Pane).Started_Us
      then State.Panes (Pane).Finished_Us - State.Panes (Pane).Started_Us else 0);
   function Overlay_Shown (State : View_State) return Boolean is (State.Overlay);
   function Menu_Open (State : View_State) return Boolean is (CuBit.UI.Popup_Menus.Is_Open (Popup));
   function Menu_Selected (State : View_State) return Natural is (CuBit.UI.Popup_Menus.Selected (Popup));
   function Drawer_Open (State : View_State) return Boolean is (Drawer.Open);
   function Drawer_Row (State : View_State; Row : Positive) return String is
     (if Row > Shortcuts.Count then ""
      else Shortcuts.Rows (Row).Caption (1 .. Shortcuts.Rows (Row).Length));
   function Pane_Count (State : View_State) return Side is (State.Count);
   function Pane_Shown (State : View_State; Pane : Side) return Boolean is
     (Pane <= State.Count and then State.Shown (Pane));
   function Tab_Count (State : View_State; Pane : Side) return Natural is (State.Panes (Pane).Tab_Count);
   function Current_Tab (State : View_State; Pane : Side) return Natural is (State.Panes (Pane).Current_Tab);
   function Pane_Width (State : View_State; Pane : Side) return Natural is (State.Panes (Pane).Path_Area.w);

   function Column_Titles (State : View_State; Pane : Side) return String is
      P : Pane_State renames State.Panes (Pane).all;
      function From (Column : Positive) return String is
        (if Column > P.Columns.Count then "" else COLUMN_CAPTIONS (P.Kinds (Column)).all & "|" & From (Column + 1));
   begin
      return From (1);
   end Column_Titles;

   function Bookmarked (State : View_State; Pane : Side) return Boolean is
     ((for some K in 1 .. Bookmark_Count =>
         DP.Value (Bookmarks (K).Where) = Full_Path (State.Panes (Pane).all)));
end Files_View;
