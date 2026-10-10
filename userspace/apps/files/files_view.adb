with Ada.Unchecked_Deallocation;
with CuBit.Directory_Paths;
with CuBit.Filesystems;
with CuBit.File_Access;
with CuBit.Filesystem_Events;
with CuBit.Volume_Descriptions;
with CuBit.UI.Drawers;
with CuBit.UI.Popup_Menus;
with CuBit.UI.Tables;
with CuBit.UI.Tooltips;
with CuBit.UI.Widgets;
with Files_Filter;
with Files_Listing;
with Files_Marks;
with Files_Operations;
with Files_Plan;
with Files_Pages;
with Files_Queue;
with Files_Reader;
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
   MARGIN : constant := 4;
   PATH_HEIGHT : constant := 24;
   ROW_HEIGHT : constant := 20;
   STATUS_HEIGHT : constant := 24;
   --  The function-key bar: slim, one keycap per F1 .. F10.
   KEYS_HEIGHT : constant := 22;
   KEY_BADGE_PADDING : constant := 4;
   KEY_GAP : constant := 4;
   SCROLLBAR_WIDTH : constant := 14;
   TEXT_INSET : constant := 8;
   MARK_BAR_WIDTH : constant := 3;
   SIZE_COLUMN_WIDTH : constant := 92;
   TIME_COLUMN_WIDTH : constant := 140;
   NAME_COLUMN_MINIMUM : constant := 80;
   OVERLAY_WIDTH : constant := 380;
   OVERLAY_LINE : constant := 18;
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
   --  Then the granted scopes (Queue_List_Scopes) and a volume description
   --  (Queue_Describe_Volume: the path in, the record out).
   SCOPES_BASE : constant := OPERATIONS_BASE + Files_Operations.AREA_BYTES;
   SCOPES_BYTES : constant := CuBit.File_Access.Maximum_Entries * CuBit.File_Access.Wire_Entry_Bytes;
   VOLUME_BASE : constant := SCOPES_BASE + SCOPES_BYTES;
   VOLUME_BYTES : constant := DP.Maximum_Bytes;
   ARENA_PAGES : constant := (VOLUME_BASE + VOLUME_BYTES + FQ.Page_Bytes - 1) / FQ.Page_Bytes;
   --  Change records taken per pump, at most (the rest wait for the next).
   EVENTS_PER_PUMP : constant := 1_024;
   DIALOG_WIDTH : constant := 560;
   DIALOG_HEIGHT : constant := 150;
   PROGRESS_WIDTH : constant := 240;
   --  How long a viewer read may take before it is reported as timed out,
   --  and how long a status message stays (explicit, docs/files-app.md).
   VIEW_DEADLINE_US : constant := 2_000_000;
   MESSAGE_LIFETIME_US : constant := 6_000_000;
   VIEW_HEADER_HEIGHT : constant := 24;
   HEX_BYTES_PER_LINE : constant := 16;
   MAXIMUM_VIEW_COLUMNS : constant := 400;
   BINARY_PROBE_BYTES : constant := 4_096;
   TOOLBAR_HEIGHT : constant := 34;
   TAB_HEIGHT : constant := 24;
   TAB_WIDTH : constant := 168;
   --  Each tab's close button (x) at its right end.
   TAB_CLOSE_SIZE : constant := 16;
   TAB_CLOSE_INSET : constant := 4;
   PANE_MINIMUM : constant := 220;
   MAXIMUM_CLOSED_TABS : constant := 8;
   TOOL_SIZE : constant := 28;
   TOOL_GAP : constant := 2;
   TOOL_GROUP_GAP : constant := 12;
   SEARCH_WIDTH : constant := 260;
   SEARCH_HEIGHT : constant := 24;
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
   COLUMN_WIDTHS : constant array (Column_Kind) of Natural := [240, 64, 92, 140, 140, 72, 104, 96];
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
      Table_Width : Natural := 0;
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
   end record;

   --  Per-view state the private part does not declare (one view per
   --  process): the clock and the status message.
   Now_Us : Unsigned_64 := 0;
   Message : String (1 .. MAXIMUM_MESSAGE) := [others => ' '];
   Message_Length : Natural range 0 .. MAXIMUM_MESSAGE := 0;
   Status_Area : Rect := (others => 0);

   Message_Until_Us : Unsigned_64 := 0;
   --  The granted scopes, asked for when the platform names no folders.
   Scopes_Slot : Slot_State;
   --  Free space of the active pane's volume (Queue_Describe_Volume).
   Volume_Slot : Slot_State;
   Volume_Asked, Volume_Of : DP.Path;
   Empty_Path : DP.Path;
   Volume_Known, Volume_Read_Only : Boolean := False;
   --  Something changed on it since: ask again.
   Volume_Stale : Boolean := False;
   Volume_Free, Volume_Total : Unsigned_64 := 0;
   HINT : constant String :=
     "Tab: other pane   Enter: open   Backspace: up   Ins/Space: mark   type to filter   F3: view   F12: timing";

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

   --  The function-key bar: F1 .. F10, every key shown; a key without an
   --  action here is drawn greyed and takes no click.
   subtype Function_Key is Key_Name range F1 .. F10;
   function Key_Caption (K : Function_Key) return String is
     (case K is
         when F1 => "Help", when F2 => "Refresh", when F3 => "View", when F4 => "Edit",
         when F5 => "Copy", when F6 => "Move", when F7 => "Folder", when F8 => "Delete",
         when F9 => "Single", when F10 => "Quit");
   function Key_Badge (K : Function_Key) return String is
     (case K is
         when F1 => "F1", when F2 => "F2", when F3 => "F3", when F4 => "F4", when F5 => "F5",
         when F6 => "F6", when F7 => "F7", when F8 => "F8", when F9 => "F9", when F10 => "F10");
   function Key_Bound (K : Function_Key) return Boolean is
     (case K is
         when F1 | F4 => False,
         when others => True);
   function Key_ID (K : Function_Key) return Controls.Control_ID is
     (FUNCTION_KEY_FIRST + (Key_Name'Pos (K) - Key_Name'Pos (F1)));
   --  Ctrl+F12 hides and shows the bar.
   Key_Bar_Shown : Boolean := True;
   function Key_Bar_Height return Natural is (if Key_Bar_Shown then KEYS_HEIGHT else 0);

   --  The context menu and what it was opened on.
   type Menu_Command is
     (No_Menu_Command, Cmd_Open, Cmd_View, Cmd_Open_Other, Cmd_Copy, Cmd_Move, Cmd_Rename, Cmd_Delete,
      Cmd_Mark_Toggle, Cmd_Mark_All, Cmd_Unmark_All, Cmd_Invert, Cmd_New_Folder, Cmd_Refresh, Cmd_Bookmark,
      Cmd_Bookmark_Entry, Cmd_Sort_Name, Cmd_Sort_Extension, Cmd_Sort_Modified, Cmd_Sort_Size, Cmd_Up, Cmd_Drawer,
      Cmd_Column_Extension, Cmd_Column_Size, Cmd_Column_Modified, Cmd_Column_Changed, Cmd_Column_Kind,
      Cmd_Column_Mode, Cmd_Column_Owner);
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
         if P.Malformed then
            P.Error := FS.REPLY_MALFORMED_FILESYSTEM;
         end if;
         P.Listing.Complete := True;
         P.Finished_Us := Now_Us;
      end if;
   end Finish_If_Done;

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

   --  The granted scopes arrived: each becomes a place, and panes not yet
   --  given a folder start at the first two.
   procedure Take_Scopes (State : in out View_State; Answer : Files_Queue.Answer) is
      Count : constant Natural :=
        (if Answer.Status = FS.REPLY_OK then Natural (Unsigned_64'Min (Answer.Value, FA.Maximum_Entries)) else 0);
      Bytes : Files_Listing.Name_Bytes (1 .. Natural'Max (1, Count * FA.Wire_Entry_Bytes));
      Wire : FA.Wire_Bytes (1 .. Natural'Max (1, Count * FA.Wire_Entry_Bytes));
      Policy : FA.Policy;
      Decoded : Boolean := False;
      PREFIX_LENGTH_AT : constant := 1;   --  after the rights byte
      PREFIX_AT : constant := FA.Wire_Header_Bytes;
      Firsts : array (1 .. 2) of DP.Path;
      Found : Natural := 0;
   begin
      if Count > 0 then
         --  Copied, then checked: the service is not trusted.
         Files_Queue.Read_Arena (SCOPES_BASE, Bytes);
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
                 Natural'Min (Natural (Wire (Base + PREFIX_LENGTH_AT)) + 256 * Natural (Wire (Base + PREFIX_LENGTH_AT + 1)),
                              FA.Maximum_Prefix_Bytes);
               Prefix : String (1 .. Length);
               Where : DP.Path;
               Done : Boolean;
            begin
               for K in Prefix'Range loop
                  Prefix (K) := Character'Val (Wire (Base + PREFIX_AT + K - 1));
               end loop;
               if Length > 0 then
                  DP.Set_Root (Prefix, Where, Done);
                  if Done then
                     Add_Place (State, Prefix, Prefix, CuBit.UI.Icons.Drive);
                     if Found < Firsts'Last then
                        Found := Found + 1;
                        Firsts (Found) := Where;
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end if;
      if Found = 0 then
         Say ("No folders are granted to Files (its manifest lists none).");
         for W in Side range 1 .. State.Count loop
            if State.Panes (W).Load = Not_Started then
               State.Panes (W).Load := Failed;
               State.Panes (W).Error := FS.REPLY_ACCESS_DENIED;
            end if;
         end loop;
      else
         for W in Side range 1 .. State.Count loop
            declare
               P : Pane_State renames State.Panes (W).all;
            begin
               if P.Load = Not_Started then
                  P.Where := Firsts (Natural'Min (Natural (W), Found));
                  P.Root_Length := Root_Length_Of (DP.Value (P.Where));
                  Start_Listing (W, P);
                  Remember_Recent (P.Where);
               end if;
            end;
         end loop;
      end if;
      Dirty_All (State);
   end Take_Scopes;

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
      if Scopes_Slot.Busy and then Scopes_Slot.Tag = Answer.Tag then
         Scopes_Slot.Busy := False;
         Take_Scopes (State, Answer);
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
      Add_Section (Shortcuts, "Bookmarks" & (if Bookmark_Count = 0 then "  (Ctrl+D)" else ""));
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
           (Count => 3, Width => [NAME_COLUMN_MINIMUM * 3, SIZE_COLUMN_WIDTH, TIME_COLUMN_WIDTH, others => 96],
            Minimum => [NAME_COLUMN_MINIMUM, SIZE_COLUMN_WIDTH / 2, TIME_COLUMN_WIDTH / 2, others => 32],
            Sortable => True, Sort_Column => 1, Order => Tables.Ascending, Cell_Padding => 5);
         P.Table_Width := 0;
         P.Back_Count := 0;
         P.Forward_Count := 0;
         DP.Set_Root (Path, P.Where, Done);
         P.Root_Length := Root_Length_Of (Path);
         P.Tab_Count := 1;
         P.Current_Tab := 1;
         P.Watch := 0;
         P.Watch_Slot := (others => <>);
         P.Stale := False;
         if Path'Length > 0 then
            Start_Listing (Which, P);
         else
            --  Its folder comes with the granted scopes.
            P.Load := Not_Started;
         end if;
      end;
   end New_Pane;

   procedure Initialize
     (State : out View_State; Capacity : Entry_Capacity; Name_Bytes : Arena_Capacity;
      Left_Path, Right_Path : String)
   is
      Opened : Boolean;
   begin
      Files_Queue.Open (ARENA_PAGES, Opened);
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
      Scopes_Slot := (others => <>);
      Volume_Slot := (others => <>);
      for Which in Side range 1 .. State.Count loop
         New_Pane (State, Which, (if Which = Left_Pane then Left_Path else Right_Path));
      end loop;
      --  No folders named: start at the granted scopes.
      if Left_Path'Length = 0 or else Right_Path'Length = 0 then
         declare
            Sent : Boolean;
         begin
            Submit ((Operation => FQ.Queue_List_Scopes, Length => SCOPES_BYTES, Arena_Offset => SCOPES_BASE,
                     others => <>), Scopes_Slot, 0, Sent);
         end;
      end if;
      Files_Queue.Flush;
      Message_Length := 0;
      Viewing := False;
      Place_Count := 0;
      Recent_Count := 0;
      CuBit.UI.Popup_Menus.Close (Popup);
      for Which in Side range 1 .. State.Count loop
         if State.Panes (Which).Load /= Not_Started then
            Remember_Recent (State.Panes (Which).Where);
         end if;
      end loop;
   end Initialize;

   procedure Add_Place
     (State : in out View_State; Caption, Path : String; Picture : CuBit.UI.Icons.Icon := CuBit.UI.Icons.Drive)
   is
      Where : DP.Path;
      Done : Boolean;
   begin
      DP.Set_Root (Path, Where, Done);
      if Done and then Place_Count < MAXIMUM_PLACES then
         Place_Count := Place_Count + 1;
         Places (Place_Count) := Make_Place (Caption, Where, Picture);
         Rebuild_Shortcuts;
         Dirty_All (State);
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
                    & (if Files_Reader.Status = FS.REPLY_ACCESS_DENIED then "not granted"
                       elsif Files_Reader.Status = FS.REPLY_NOT_FOUND then "not found"
                       else "status" & Unsigned_32'Image (Files_Reader.Status)) & ".");
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
            P.Table_Width := 0;
            return;
         end if;
      end loop;
      if P.Columns.Count < Tables.MAX_COLUMNS then
         P.Columns.Count := P.Columns.Count + 1;
         P.Kinds (P.Columns.Count) := Kind;
         P.Columns.Width (P.Columns.Count) := COLUMN_WIDTHS (Kind);
         P.Columns.Minimum (P.Columns.Count) := COLUMN_WIDTHS (Kind) / 2;
         P.Columns.Sort_Column := Column_Of_Key (P, Files_Order.Rule (P.Order).Key);
         P.Table_Width := 0;
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
      P.Table_Width := 0;
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
   function Pos (Command : Menu_Command) return CuBit.UI.Popup_Menus.Command is (Menu_Command'Pos (Command));

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
         Say ("An operation is running (Esc cancels it).");
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

   function Reason (Status : Unsigned_32) return String is
     (if Status = FS.REPLY_READ_ONLY then "the place is read-only"
      elsif Status = FS.REPLY_ACCESS_DENIED then "not granted"
      elsif Status = FS.REPLY_NOT_FOUND then "not found"
      elsif Status = FS.REPLY_ALREADY_EXISTS then "the name exists"
      elsif Status = FS.REPLY_NOT_EMPTY then "the folder is not empty"
      elsif Status = FS.REPLY_NO_SPACE then "no space"
      elsif Status = FS.REPLY_INVALID_MOVE then "a folder cannot go inside itself"
      elsif Status = FS.REPLY_ERR then "refused (an invalid name?)"
      else "status" & Unsigned_32'Image (Status));

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
   procedure Prompt_Input (State : in out View_State; Item : Event; Used : out Boolean) is
      Names : constant Boolean := Prompt in Ask_Folder_Name | Ask_New_Name;
   begin
      Used := Prompt /= No_Prompt;
      if not Used then
         return;
      end if;
      if Item.Kind = Key_Event then
         case Item.Key is
            when Escape =>
               if Prompt = Ask_Conflict then
                  Files_Operations.Cancel;
               end if;
               Prompt := No_Prompt;
            when Enter =>
               if Prompt = Ask_Conflict then
                  null;
               else
                  Accept_Prompt (State);
               end if;
            when Backspace =>
               if Names and then Input_Length > 0 then
                  Input_Length := Input_Length - 1;
               end if;
            when others => null;
         end case;
      elsif Item.Kind = Text_Event then
         if Names then
            if Item.Character_Value in ' ' .. '~' and then Input_Length < MAXIMUM_NAME_BYTES then
               Input_Length := Input_Length + 1;
               Input (Input_Length) := Item.Character_Value;
            end if;
         else
            declare
               Choice : Files_Operations.Conflict_Policy := Files_Operations.Ask;
               C : constant Character := Item.Character_Value;
            begin
               case C is
                  when 's' | 'S' => Choice := Files_Operations.Skip_Existing;
                  when 'o' | 'O' => Choice := Files_Operations.Overwrite_Existing;
                  when 'k' | 'K' => Choice := Files_Operations.Keep_Both;
                  when 'a' | 'A' => Choice := Files_Operations.Ask;
                  when others => null;
               end case;
               if Prompt = Ask_Conflict then
                  if Choice /= Files_Operations.Ask then
                     Prompt := No_Prompt;
                     --  Upper case: for the rest of the operation too.
                     Files_Operations.Decide (Choice, For_All => C in 'A' .. 'Z');
                  end if;
               elsif C in 's' | 'S' | 'o' | 'O' | 'k' | 'K' | 'a' | 'A' then
                  Prompt_Policy := Choice;
               end if;
            end;
         end if;
      elsif Item.Kind = Pointer_Event and then Item.Action = Controls.Pointer_Press
        and then not Point_In_Rect (Item.X, Item.Y, Dialog_Area)
      then
         --  A click outside the dialog dismisses it (a conflict stays).
         if Prompt /= Ask_Conflict then
            Prompt := No_Prompt;
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
      Command : Menu_Command := Cmd_Column_Extension;
   begin
      PM.Clear (Menu);
      Menu_Side := Which;
      for Kind in Extension_Column .. Column_Kind'Last loop
         PM.Add (Menu, PM.ROOT, COLUMN_CAPTIONS (Kind).all, Pos (Command), Checked => Shows (P, Kind));
         if Command < Menu_Command'Last then
            Command := Menu_Command'Succ (Command);
         end if;
      end loop;
      PM.Add_Separator (Menu, PM.ROOT);
      PM.Add (Menu, PM.ROOT, "Drag a header to move its column", PM.NO_COMMAND, Enabled => False);
      PM.Open (Popup, Menu, X, Y, State.Bounds);
      Dirty (State, PM.Covered (Popup));
   end Open_Columns_Menu;

   --  The menu for the row under the pointer (0: empty space) of Which.
   procedure Open_Menu (State : in out View_State; Which : Side; Row : Row_Count; X, Y : Natural) is
      package PM renames CuBit.UI.Popup_Menus;
      package IC renames CuBit.UI.Icons;
      P : Pane_State renames State.Panes (Which).all;
      Id : constant Entry_Count := (if Row = 0 then 0 else Entry_Of (P, Row));
      Marked : constant Natural := Files_Marks.Count (P.Marks);
      Folder : constant Boolean := Id > 0 and then Files_Listing.Kind (P.Listing, Id) = Files_Listing.Directory_Kind;
      Rule : constant Files_Order.Sort_Rule := Files_Order.Rule (P.Order);
      Sort_Menu : PM.Menu_Count;
   begin
      PM.Clear (Menu);
      Menu_Side := Which;
      if Marked > 0 and then (Id = 0 or else Files_Marks.Marked (P.Marks, Id)) then
         declare
            Count_Text : constant String := Thousands (Unsigned_64 (Marked)) & (if Marked = 1 then " item" else " items");
         begin
            PM.Add (Menu, PM.ROOT, "Copy " & Count_Text & " to the other pane", Pos (Cmd_Copy), "F5", OPERATIONS_READY,
                    Has_Icon => True, Picture => IC.Copy);
            PM.Add (Menu, PM.ROOT, "Move " & Count_Text & " to the other pane", Pos (Cmd_Move), "F6", OPERATIONS_READY,
                    Has_Icon => True, Picture => IC.Move);
            PM.Add (Menu, PM.ROOT, "Delete " & Count_Text, Pos (Cmd_Delete), "F8", OPERATIONS_READY,
                    Has_Icon => True, Picture => IC.Delete);
            PM.Add_Separator (Menu, PM.ROOT);
            PM.Add (Menu, PM.ROOT, "Unmark all", Pos (Cmd_Unmark_All), "Esc");
            PM.Add (Menu, PM.ROOT, "Invert marks", Pos (Cmd_Invert), "Ctrl+I");
         end;
      elsif Id > 0 then
         if Folder then
            PM.Add (Menu, PM.ROOT, "Open", Pos (Cmd_Open), "Enter", Has_Icon => True, Picture => IC.Folder);
            PM.Add (Menu, PM.ROOT, "Open in the other pane", Pos (Cmd_Open_Other));
            PM.Add (Menu, PM.ROOT, "Bookmark", Pos (Cmd_Bookmark_Entry), Has_Icon => True, Picture => IC.Favorites);
         else
            PM.Add (Menu, PM.ROOT, "View", Pos (Cmd_View), "F3", Has_Icon => True, Picture => IC.Document_File);
         end if;
         PM.Add_Separator (Menu, PM.ROOT);
         PM.Add (Menu, PM.ROOT, "Copy to the other pane", Pos (Cmd_Copy), "F5", OPERATIONS_READY,
                 Has_Icon => True, Picture => IC.Copy);
         PM.Add (Menu, PM.ROOT, "Move to the other pane", Pos (Cmd_Move), "F6", OPERATIONS_READY,
                 Has_Icon => True, Picture => IC.Move);
         PM.Add (Menu, PM.ROOT, "Rename", Pos (Cmd_Rename), "Shift+F6", OPERATIONS_READY,
                 Has_Icon => True, Picture => IC.Rename);
         PM.Add (Menu, PM.ROOT, "Delete", Pos (Cmd_Delete), "F8", OPERATIONS_READY,
                 Has_Icon => True, Picture => IC.Delete);
         PM.Add_Separator (Menu, PM.ROOT);
         PM.Add (Menu, PM.ROOT, "Mark", Pos (Cmd_Mark_Toggle), "Ins");
      else
         PM.Add (Menu, PM.ROOT, "New folder", Pos (Cmd_New_Folder), "F7", OPERATIONS_READY,
                 Has_Icon => True, Picture => IC.New_Folder);
         PM.Add (Menu, PM.ROOT, "Refresh", Pos (Cmd_Refresh), "Ctrl+R", Has_Icon => True, Picture => IC.Refresh);
         PM.Add (Menu, PM.ROOT, "Up", Pos (Cmd_Up), "Backspace", not At_Root (P), Has_Icon => True, Picture => IC.Go_Up);
         PM.Add_Separator (Menu, PM.ROOT);
         PM.Add_Submenu (Menu, PM.ROOT, "Sort by", Sort_Menu);
         if Sort_Menu > 0 then
            PM.Add (Menu, Sort_Menu, "Name", Pos (Cmd_Sort_Name), "Ctrl+F3", Checked => Rule.Key = Files_Order.By_Name);
            PM.Add (Menu, Sort_Menu, "Extension", Pos (Cmd_Sort_Extension), "Ctrl+F4",
                    Checked => Rule.Key = Files_Order.By_Extension);
            PM.Add (Menu, Sort_Menu, "Modified", Pos (Cmd_Sort_Modified), "Ctrl+F5",
                    Checked => Rule.Key = Files_Order.By_Modified);
            PM.Add (Menu, Sort_Menu, "Size", Pos (Cmd_Sort_Size), "Ctrl+F6", Checked => Rule.Key = Files_Order.By_Size);
         end if;
         PM.Add (Menu, PM.ROOT, "Mark all", Pos (Cmd_Mark_All), "Ctrl+A");
         PM.Add_Separator (Menu, PM.ROOT);
         PM.Add (Menu, PM.ROOT, "Bookmark this folder", Pos (Cmd_Bookmark), "Ctrl+D",
                 Has_Icon => True, Picture => IC.Favorites);
         PM.Add (Menu, PM.ROOT, (if Drawer.Open then "Hide the drawer" else "Show the drawer"), Pos (Cmd_Drawer),
                 "Ctrl+B", Has_Icon => True, Picture => IC.Drawer);
      end if;
      PM.Open (Popup, Menu, X, Y, State.Bounds);
      Dirty (State, PM.Covered (Popup));
   end Open_Menu;

   procedure Run_Command (State : in out View_State; Command : Menu_Command) is
      Which : constant Side := Menu_Side;
      P : Pane_State renames State.Panes (Which).all;
   begin
      case Command is
         when No_Menu_Command => null;
         when Cmd_Open => Open_Cursor_Entry (State, Which);
         when Cmd_View => Open_Viewer (State, Which);
         when Cmd_Open_Other =>
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
         when Cmd_Copy => Ask (State, Confirm_Copy, Which);
         when Cmd_Move => Ask (State, Confirm_Move, Which);
         when Cmd_Delete => Ask (State, Confirm_Delete, Which);
         when Cmd_Rename => Ask (State, Ask_New_Name, Which);
         when Cmd_New_Folder => Ask (State, Ask_Folder_Name, Which);
         when Cmd_Mark_Toggle =>
            if P.View.Cursor > 0 then
               Set_Mark (P, P.View.Cursor, not Marked_Row (P, P.View.Cursor));
            end if;
         when Cmd_Mark_All | Cmd_Unmark_All =>
            for Row in 1 .. Row_Total (P) loop
               Set_Mark (P, Row, Command = Cmd_Mark_All);
            end loop;
         when Cmd_Invert =>
            for Row in 1 .. Row_Total (P) loop
               Set_Mark (P, Row, not Marked_Row (P, Row));
            end loop;
         when Cmd_Refresh => Refresh (State, Which);
         when Cmd_Up => Go_Parent (State, Which);
         when Cmd_Bookmark => Toggle_Bookmark (P.Where);
         when Cmd_Bookmark_Entry =>
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
         when Cmd_Sort_Name => Set_Rule (State, Which, Files_Order.By_Name);
         when Cmd_Sort_Extension => Set_Rule (State, Which, Files_Order.By_Extension);
         when Cmd_Sort_Modified => Set_Rule (State, Which, Files_Order.By_Modified);
         when Cmd_Sort_Size => Set_Rule (State, Which, Files_Order.By_Size);
         when Cmd_Drawer => Drawer.Open := not Drawer.Open;
         when Cmd_Column_Extension .. Cmd_Column_Owner =>
            Toggle_Column
              (P, Column_Kind'Val (Column_Kind'Pos (Extension_Column)
                                   + Menu_Command'Pos (Command) - Menu_Command'Pos (Cmd_Column_Extension)));
      end case;
      Dirty_All (State);
   end Run_Command;

   procedure New_Tab (State : in out View_State; Which : Side);
   procedure Add_Pane (State : in out View_State; Which : Side);
   procedure Toggle_Single (State : in out View_State);
   function Tab_Box (P : Pane_State; K : Positive) return Rect;
   function Tab_Close_Box (P : Pane_State; K : Positive) return Rect;

   procedure Run_Tool (State : in out View_State; Item : Tool) is
      Which : constant Side := State.Current;
   begin
      Menu_Side := Which;
      case Item is
         when Tool_Back => History_Move (State, Which, Backward => True);
         when Tool_Forward => History_Move (State, Which, Backward => False);
         when Tool_Up => Go_Parent (State, Which);
         when Tool_Refresh => Refresh (State, Which);
         when Tool_New_Folder => Run_Command (State, Cmd_New_Folder);
         when Tool_Copy => Run_Command (State, Cmd_Copy);
         when Tool_Move => Run_Command (State, Cmd_Move);
         when Tool_Delete => Run_Command (State, Cmd_Delete);
         when Tool_Drawer => Drawer.Open := not Drawer.Open;
         when Tool_Columns =>
            declare
               Box : constant Rect := Toolbar_Area;
            begin
               Open_Columns_Menu (State, Which, Box.x + 10 * (TOOL_SIZE + TOOL_GAP), Box.y + Box.h);
            end;
         when Tool_Viewer => Open_Viewer (State, Which);
         when Tool_New_Tab => New_Tab (State, Which);
         when Tool_Add_Pane => Add_Pane (State, Which);
         when Tool_Single_Pane => Toggle_Single (State);
      end case;
      Dirty_All (State);
   end Run_Tool;

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

   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean)
   is
      Which : constant Side := State.Current;
      P : Pane_State renames State.Panes (Which).all;
      Page : constant Row_Delta := Integer'Max (1, P.View.Rows - 1);
      Before : constant Rect := State.Dirty;
      package PM renames CuBit.UI.Popup_Menus;
      Tip_Damage : Rect;
   begin
      --  Tooltips follow the pointer and go away on any other input.
      if Item.Kind = Pointer_Event and then Item.Action = Controls.Pointer_Move then
         Tooltips.Pointer_At (Tip, Tips, Map, Item.X, Item.Y, Now_Us / MICROSECONDS_PER_MILLISECOND, Tip_Damage);
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
                  case Item.Key is
                     when Up => PM.Handle_Key (Popup, Menu, PM.Up, Chosen, Handled);
                     when Down => PM.Handle_Key (Popup, Menu, PM.Down, Chosen, Handled);
                     when Left => PM.Handle_Key (Popup, Menu, PM.Left, Chosen, Handled);
                     when Right => PM.Handle_Key (Popup, Menu, PM.Right, Chosen, Handled);
                     when Home => PM.Handle_Key (Popup, Menu, PM.Home, Chosen, Handled);
                     when End_Key => PM.Handle_Key (Popup, Menu, PM.End_Key, Chosen, Handled);
                     when Enter | Space => PM.Handle_Key (Popup, Menu, PM.Enter, Chosen, Handled);
                     when others => PM.Handle_Key (Popup, Menu, PM.Escape, Chosen, Handled);
                  end case;
               when Pointer_Event =>
                  PM.Handle_Pointer (Popup, Menu, Item.Action, Item.X, Item.Y, Chosen, Handled);
               when Wheel | Text_Event =>
                  PM.Close (Popup);
               when Resize =>
                  PM.Close (Popup);
                  Dirty_All (State);
            end case;
            Dirty (State, Was);
            Dirty (State, PM.Covered (Popup));
            if Chosen /= PM.NO_COMMAND and then Chosen <= Menu_Command'Pos (Menu_Command'Last) then
               Run_Command (State, Menu_Command'Val (Chosen));
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
         Prompt_Input (State, Item, Used);
         if Used then
            Redraw := not Is_Empty (State.Dirty);
            return;
         end if;
      end;
      --  Escape cancels a running operation first.
      if Item.Kind = Key_Event and then Item.Key = Escape and then Files_Operations.Busy then
         Files_Operations.Cancel;
         Dirty (State, Status_Area);
         Redraw := True;
         return;
      end if;
      if Viewing then
         if Item.Kind = Key_Event then
            case Item.Key is
               when Escape | F3 | F10 | Enter => Close_Viewer (State);
               when Up => Viewer_Top := (if Viewer_Top > 0 then Viewer_Top - 1 else 0);
               when Down => Viewer_Top := Natural'Min (Viewer_Top + 1, Natural'Max (0, Line_Count - Viewer_Rows));
               when Page_Up => Viewer_Top := Natural'Max (0, Viewer_Top - Viewer_Rows);
               when Page_Down =>
                  Viewer_Top := Natural'Min (Viewer_Top + Viewer_Rows, Natural'Max (0, Line_Count - Viewer_Rows));
               when Home => Viewer_Top := 0;
               when End_Key => Viewer_Top := Natural'Max (0, Line_Count - Viewer_Rows);
               when F4 =>
                  Viewer_Hex := not Viewer_Hex;
                  Viewer_Top := 0;
                  if Files_Reader.State = Files_Reader.Done then
                     Index_Lines;
                  end if;
               when others => null;
            end case;
            Dirty_All (State);
         elsif Item.Kind = Wheel then
            Viewer_Top := Natural'Max (0, Natural'Min (Viewer_Top - Item.Steps * WHEEL_ROWS,
                                                       Natural'Max (0, Line_Count - Viewer_Rows)));
            Dirty_All (State);
         elsif Item.Kind = Resize then
            Dirty_All (State);
         end if;
         Redraw := not Is_Empty (State.Dirty);
         return;
      end if;
      case Item.Kind is
         when Resize =>
            Dirty_All (State);
         when Key_Event =>
            case Item.Key is
               when Up =>
                  if Item.Alt then
                     Go_Parent (State, Which);
                  else
                     Move_Cursor (State, Which, -1, Item.Shift);
                  end if;
               when Down => Move_Cursor (State, Which, 1, Item.Shift);
               when Page_Up | Page_Down =>
                  if Item.Control and then Item.Shift then
                     Move_Tab (State, Which, P.Current_Tab,
                               (if Item.Key = Page_Up then Natural'Max (1, P.Current_Tab - 1)
                                else Natural'Min (P.Tab_Count, P.Current_Tab + 1)));
                  elsif Item.Control then
                     if P.Tab_Count > 1 then
                        Show_Tab (State, Which,
                                  (if Item.Key = Page_Up then (if P.Current_Tab <= 1 then P.Tab_Count else P.Current_Tab - 1)
                                   else (if P.Current_Tab >= P.Tab_Count then 1 else P.Current_Tab + 1)));
                     end if;
                  else
                     Move_Cursor (State, Which, (if Item.Key = Page_Up then -Page else Page), Item.Shift);
                  end if;
               when Home => Move_Cursor (State, Which, -Row_Count'Last, Item.Shift);
               when End_Key => Move_Cursor (State, Which, Row_Count'Last, Item.Shift);
               when Left | Right =>
                  if Item.Alt then
                     History_Move (State, Which, Backward => Item.Key = Left);
                  end if;
               when Tab =>
                  if Item.Control then
                     if P.Tab_Count > 1 then
                        Show_Tab (State, Which,
                                  (if Item.Shift then (if P.Current_Tab <= 1 then P.Tab_Count else P.Current_Tab - 1)
                                   else (if P.Current_Tab >= P.Tab_Count then 1 else P.Current_Tab + 1)));
                     end if;
                     return;
                  end if;
                  State.Current := Cycle (State, Which, not Item.Shift);
                  Dirty (State, P.Path_Area);
                  Dirty (State, Row_Area (P, P.View.Cursor));
                  Dirty (State, State.Panes (State.Current).Path_Area);
                  Dirty (State, Row_Area (State.Panes (State.Current).all, State.Panes (State.Current).View.Cursor));
                  Dirty (State, Status_Area);
               when Enter => Open_Cursor (State, Which);
               when Backspace =>
                  if Files_Filter.Active (P.Filter) then
                     declare
                        Query : constant String := Files_Filter.Query (P.Filter);
                     begin
                        Type_Filter (State, Which, Query (Query'First .. Query'Last - 1));
                     end;
                  else
                     Go_Parent (State, Which);
                  end if;
               when Escape =>
                  if Files_Filter.Active (P.Filter) then
                     Type_Filter (State, Which, "");
                  elsif Files_Marks.Count (P.Marks) > 0 then
                     Files_Marks.Clear (P.Marks);
                     Dirty_All (State);
                  end if;
               when Insert =>
                  if P.View.Cursor > 0 then
                     Set_Mark (P, P.View.Cursor, not Marked_Row (P, P.View.Cursor));
                     Dirty (State, P.Path_Area);
                     Dirty (State, Status_Area);
                     Move_Cursor (State, Which, 1, False);
                  end if;
               when Space =>
                  if P.View.Cursor > 0 then
                     Set_Mark (P, P.View.Cursor, not Marked_Row (P, P.View.Cursor));
                     Dirty (State, Row_Area (P, P.View.Cursor));
                     Dirty (State, P.Path_Area);
                     Dirty (State, Status_Area);
                  end if;
               when Letter_A =>
                  if Item.Control then
                     for Row in 1 .. Row_Total (P) loop
                        Set_Mark (P, Row, not Item.Shift);
                     end loop;
                     Dirty_All (State);
                  end if;
               when Letter_I =>
                  if Item.Control then
                     for Row in 1 .. Row_Total (P) loop
                        Set_Mark (P, Row, not Marked_Row (P, Row));
                     end loop;
                     Dirty_All (State);
                  end if;
               when Letter_R =>
                  if Item.Control then
                     Refresh (State, Which);
                  end if;
               when Letter_U =>
                  if Item.Control then
                     declare
                        Next : constant Side := Other (State, Which);
                        Swap : constant Pane_Access := State.Panes (Which);
                     begin
                        State.Panes (Which) := State.Panes (Next);
                        State.Panes (Next) := Swap;
                     end;
                     Dirty_All (State);
                  end if;
               when Letter_B =>
                  if Item.Control then
                     Drawer.Open := not Drawer.Open;
                     Dirty_All (State);
                  end if;
               when Letter_D =>
                  if Item.Control then
                     Toggle_Bookmark (P.Where);
                     Dirty_All (State);
                  end if;
               when Menu_Key =>
                  declare
                     Box : constant Rect := Row_Area (P, P.View.Cursor);
                  begin
                     Open_Menu (State, Which, P.View.Cursor,
                                (if Is_Empty (Box) then P.Rows_Area.x else Box.x + Box.w / 4),
                                (if Is_Empty (Box) then P.Rows_Area.y else Box.y + Box.h));
                  end;
               when Letter_T =>
                  if Item.Control then
                     if Item.Shift then
                        Restore_Tab (State, Which);
                     else
                        New_Tab (State, Which);
                     end if;
                  end if;
               when Letter_W =>
                  if Item.Control then
                     if Item.Shift then
                        Close_Pane (State, Which);
                     else
                        Close_Tab (State, Which);
                     end if;
                  end if;
               when Letter_N =>
                  if Item.Control then
                     Add_Pane (State, Which);
                  end if;
               when F9 => Toggle_Single (State);
               when F1 =>
                  if Item.Control then
                     Toggle_Shown (State, Left_Pane);
                  end if;
               when F2 =>
                  if Item.Control then
                     Toggle_Shown (State, Right_Pane);
                  else
                     Refresh (State, Which);
                  end if;
               when F3 =>
                  if Item.Control then
                     Set_Rule (State, Which, Files_Order.By_Name);
                  else
                     Open_Viewer (State, Which);
                  end if;
               when F4 =>
                  if Item.Control then
                     Set_Rule (State, Which, Files_Order.By_Extension);
                  end if;
               when F5 | F6 | F7 | F8 | Delete =>
                  if Item.Control and then Item.Key = F5 then
                     Set_Rule (State, Which, Files_Order.By_Modified);
                  elsif Item.Control and then Item.Key = F6 then
                     Set_Rule (State, Which, Files_Order.By_Size);
                  else
                     Ask (State,
                          (case Item.Key is
                              when F5 => Confirm_Copy,
                              when F6 => (if Item.Shift then Ask_New_Name else Confirm_Move),
                              when F7 => Ask_Folder_Name,
                              when others => Confirm_Delete), Which);
                  end if;
               when F10 => State.Quit := True;
               when F12 =>
                  if Item.Control then
                     Key_Bar_Shown := not Key_Bar_Shown;
                  else
                     State.Overlay := not State.Overlay;
                  end if;
                  Dirty_All (State);
               when others => null;
            end case;
         when Text_Event =>
            if not Item.Control and then not Item.Alt and then Item.Character_Value in '!' .. '~' then
               Type_Filter (State, Which, Files_Filter.Query (P.Filter) & Item.Character_Value);
            end if;
         when Wheel =>
            for W in Side range 1 .. State.Count loop
               if Point_In_Rect (Item.X, Item.Y, State.Panes (W).Rows_Area) then
                  VP.Scroll (State.Panes (W).View, -(Item.Steps * WHEEL_ROWS));
                  Dirty (State, State.Panes (W).Rows_Area);
               end if;
            end loop;
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
                  --  A clicked key does what pressing it does.
                  for K in Function_Key loop
                     if Target = Key_ID (K) and then Controls.Take_Activated (Map, Target) then
                        declare
                           Ignore : Boolean;
                        begin
                           Handle (State, (Kind => Key_Event, Key => K, others => <>), Map, Ignore);
                        end;
                     end if;
                  end loop;
                  for T in Tool loop
                     if Target = Tool_ID (T) and then Controls.Take_Activated (Map, Target) then
                        Run_Tool (State, T);
                     end if;
                  end loop;
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
                     Tooltips.Set_Region (Tips, Close, "Close tab", "Ctrl+W");
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
      --  The name column follows the table's width; the others keep theirs.
      if Table.w /= P.Table_Width then
         declare
            Others_Width : Natural := SCROLLBAR_WIDTH;
         begin
            for Column in 2 .. P.Columns.Count loop
               Others_Width := Others_Width + P.Columns.Width (Column);
            end loop;
            P.Columns.Width (1) :=
              Natural'Max (NAME_COLUMN_MINIMUM, (if Table.w > Others_Width then Table.w - Others_Width else 0));
            P.Table_Width := Table.w;
         end;
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
            First : Natural := Text'First;
         begin
            --  The path's tail when it is too long: the folder you are in.
            while First < Text'Last and then UI_Text_Width (Text (First .. Text'Last)) > Room loop
               First := First + 1;
            end loop;
            Draw_UI_Text_Transparent
              (With_Clip (C, (Path_Bar.x, Path_Bar.y, Room + TEXT_INSET, PATH_HEIGHT)), Path_Bar.x + TEXT_INSET,
               Text_Y, Text (First .. Text'Last), Ink);
            Draw_UI_Text_Transparent
              (With_Clip (C, Path_Bar), Path_Bar.x + Path_Bar.w - Counts_Width - TEXT_INSET, Text_Y, Counts, Ink);
         end;
      end if;

      --  The table: frame, header, scrollbar, rows.
      if Intersects (Table, Damage) then
         Draw_Table_Viewport_Frame (C, Table, Colors);
      end if;
      Header (C, UI, Map, COLUMNS_BASE (Which), Regions.Header, Table, Colors, P.Columns);
      if Scrolls then
         declare
            Value : Natural := P.View.Top;
         begin
            CuBit.UI.Widgets.Vertical_Scrollbar
              (C, UI, Map, SCROLL_ID (Which),
               (Regions.Rows.x + Regions.Rows.w - SCROLLBAR_WIDTH, Regions.Rows.y, SCROLLBAR_WIDTH, Regions.Rows.h),
               Table, Colors, 0, Row_Total (P) - 1, Value, Widget, pageSize => Rows, retainedInput => True);
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
               Draw_UI_Text
                 (With_Clip (C, Rest), Rest.x + TEXT_INSET, Rest.y + TEXT_INSET,
                  (if P.Error = FS.REPLY_ACCESS_DENIED then "Not granted to Files."
                   elsif P.Error = FS.REPLY_NOT_FOUND then "Not found."
                   elsif P.Error = FS.REPLY_MALFORMED_FILESYSTEM then
                     "The filesystem sent a malformed listing; showing what was valid."
                   else "Could not list this folder (status" & Unsigned_32'Image (P.Error) & ")."),
                  Colors.danger, Colors.field);
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
         Draw_UI_Text_Transparent (With_Clip (C, Header), Header.x + TEXT_INSET, Header.y + 4,
                                   Viewer_Name (1 .. Viewer_Name_Length), Colors.selectionText);
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

   procedure Draw_Dialog (State : View_State; C : Canvas; Bounds : Rect) is
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
            when Ask_Conflict => "S: skip  O: overwrite  K: keep both   (capital: for all)   Esc: cancel",
            when others => "");
      Line_3 : constant String :=
        (case Prompt is
            when Confirm_Copy | Confirm_Move =>
              "If a name exists: " & Policy_Text (Prompt_Policy) & "  (S skip, O overwrite, K keep both, A ask)",
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
      if Prompt /= Ask_Conflict then
         Draw_UI_Text_Transparent
           (Inner, Box.x + 16, Box.y + Box.h - Line_Height - 4,
            "Enter: " & (if Prompt = Confirm_Delete then "delete" else "OK") & "    Esc: cancel", Colors.muted);
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


   --  Tooltips: what a tool does, and its key.
   function Tool_Tip (Item : Tool) return String is
     (case Item is
         when Tool_Back => "Back", when Tool_Forward => "Forward", when Tool_Up => "Up to the parent folder",
         when Tool_Refresh => "Refresh", when Tool_New_Folder => "New folder", when Tool_Copy => "Copy to the other pane",
         when Tool_Move => "Move to the other pane", when Tool_Delete => "Delete", when Tool_Drawer => "Places",
         when Tool_Columns => "Columns", when Tool_Viewer => "View the file", when Tool_New_Tab => "New tab",
         when Tool_Add_Pane => "Add a pane", when Tool_Single_Pane => "One pane");
   function Tool_Hint (Item : Tool) return String is
     (case Item is
         when Tool_Back => "Alt+Left", when Tool_Forward => "Alt+Right", when Tool_Up => "Backspace",
         when Tool_Refresh => "Ctrl+R", when Tool_New_Folder => "F7", when Tool_Copy => "F5",
         when Tool_Move => "F6", when Tool_Delete => "F8", when Tool_Drawer => "Ctrl+B", when Tool_Columns => "",
         when Tool_Viewer => "F3", when Tool_New_Tab => "Ctrl+T", when Tool_Add_Pane => "Ctrl+N",
         when Tool_Single_Pane => "F9");
   function Key_Tip (K : Function_Key) return String is
     (case K is
         when F1 => "Help", when F2 => "Refresh the folder", when F3 => "View the file", when F4 => "Edit",
         when F5 => "Copy to the other pane", when F6 => "Move to the other pane", when F7 => "New folder",
         when F8 => "Delete", when F9 => "One pane or all", when F10 => "Quit");

   function Tool_Icon (Item : Tool) return CuBit.UI.Icons.Icon is
     (case Item is
         when Tool_Back => CuBit.UI.Icons.Go_Back, when Tool_Forward => CuBit.UI.Icons.Go_Forward,
         when Tool_Up => CuBit.UI.Icons.Go_Up, when Tool_Refresh => CuBit.UI.Icons.Refresh,
         when Tool_New_Folder => CuBit.UI.Icons.New_Folder, when Tool_Copy => CuBit.UI.Icons.Copy,
         when Tool_Move => CuBit.UI.Icons.Move, when Tool_Delete => CuBit.UI.Icons.Delete,
         when Tool_Drawer => CuBit.UI.Icons.Drawer, when Tool_Columns => CuBit.UI.Icons.Columns,
         when Tool_Viewer => CuBit.UI.Icons.Document_File, when Tool_New_Tab => CuBit.UI.Icons.New_Tab,
         when Tool_Add_Pane => CuBit.UI.Icons.Add_Pane, when Tool_Single_Pane => CuBit.UI.Icons.Close);

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
      function Enabled (Item : Tool) return Boolean is
        (case Item is
            when Tool_Back => P.Back_Count > 0, when Tool_Forward => P.Forward_Count > 0,
            when Tool_Up => not At_Root (P),
            when Tool_New_Folder | Tool_Copy | Tool_Move | Tool_Delete => OPERATIONS_READY,
            when Tool_Viewer => P.View.Cursor > 0 and then Entry_Of (P, P.View.Cursor) > 0
                                and then Files_Listing.Kind (P.Listing, Entry_Of (P, P.View.Cursor))
                                         /= Files_Listing.Directory_Kind,
            when others => True);
   begin
      if Drawn then
         CuBit.UI.Widgets.Toolbar (C, Bar, Colors);
      end if;
      for T in Tool loop
         declare
            Box : constant Rect := (X, Top, TOOL_SIZE, TOOL_SIZE);
         begin
            if Enabled (T) then
               Controls.Add_Button (Map, Tool_ID (T), Box, Box);
               Tooltips.Set (Tips, Tool_ID (T), Tool_Tip (T), Tool_Hint (T));
            end if;
            if Drawn and then Intersects (Box, Damage) then
               CuBit.UI.Icons.Draw_Tool_Button
                 (C, Box, Colors, Tool_Icon (T), Enabled (T),
                  Hot => UI.pointer.enabled and then Point_In_Rect (UI.pointer.x, UI.pointer.y, Box),
                  Pressed => Controls.Is_Active (Map, Tool_ID (T)),
                  Checked => (T = Tool_Drawer and then Drawer.Open) or else (T = Tool_Viewer and then Viewing)
                    or else (T = Tool_Single_Pane
                             and then (for all K in Side range 1 .. State.Count =>
                                         K = State.Current or else not State.Shown (K))));
            end if;
            X := X + TOOL_SIZE + TOOL_GAP;
            if T in Tool_Refresh | Tool_Delete | Tool_Viewer then
               if Drawn then
                  CuBit.UI.Widgets.Toolbar_Separator (C, (X, Bar.y + 2, TOOL_GROUP_GAP, TOOLBAR_HEIGHT - 4), Colors);
               end if;
               X := X + TOOL_GROUP_GAP;
            end if;
         end;
      end loop;
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
      Keys : constant Rect := (Bounds.x, Bounds.y + Bounds.h - Key_Bar_Height, Bounds.w, Key_Bar_Height);
      Status : constant Rect :=
        (Bounds.x + MARGIN, Keys.y - STATUS_HEIGHT, (if Bounds.w > 2 * MARGIN then Bounds.w - 2 * MARGIN else 0),
         STATUS_HEIGHT);
      Bar : constant Rect := (Bounds.x, Bounds.y, Bounds.w, TOOLBAR_HEIGHT);
      Middle_Top : constant Natural := Bar.y + TOOLBAR_HEIGHT + MARGIN;
      Middle : constant Rect :=
        (Bounds.x, Middle_Top, Bounds.w, (if Status.y > Middle_Top + MARGIN then Status.y - Middle_Top - MARGIN else 0));
      Drawer_Box, Content, Edge : Rect;
   begin
      State.Bounds := Bounds;
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
              & (if Message_Length > 0 then Message (1 .. Message_Length)
                 elsif Viewing then "Esc: close   F4: text/hex   arrows, PgUp/PgDn: scroll"
                 else HINT);
            Root : constant String := Root_Of (P);
            Right_Text : constant String :=
              Volume_Text (Root) & (if Volume_Text (Root)'Length > 0 then "   " else "")
              & "by " & (case Rule.Key is
                          when Files_Order.By_Name => "name", when Files_Order.By_Extension => "extension",
                          when Files_Order.By_Modified => "modified", when Files_Order.By_Size => "size")
              & (if Rule.Direction = Files_Order.Ascending then " up" else " down")
              & (if Files_Order.Busy (P.Order, P.Listing) then " (sorting)" else "");
         begin
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
                    & Size_Text (Bytes_Done) & " / " & Size_Text (Bytes_Total) & "   " & Current & "   Esc: cancel";
                  Bar : constant Rect :=
                    (Status.x + (if Status.w > PROGRESS_WIDTH + MARGIN then Status.w - PROGRESS_WIDTH - MARGIN else 0),
                     Status.y + 4, Natural'Min (PROGRESS_WIDTH, Status.w), STATUS_HEIGHT - 8);
               begin
                  Draw_Status_Bar (C, Status, Colors, Text, "");
                  Draw_Progress_Bar
                    (C, Bar, Colors, 0, 100,
                     Files_Plan.Percent ((if Bytes_Total > 0 then Bytes_Done else Unsigned_64 (Items_Done)),
                                   (if Bytes_Total > 0 then Bytes_Total else Unsigned_64 (Items_Total))));
               end;
            else
               Draw_Status_Bar (C, Status, Colors, Left_Text, Right_Text);
            end if;
         end;
      end if;

      --  The function-key bar: a keycap badge, then what the key does.
      if Key_Bar_Shown then
         declare
            COUNT : constant := Function_Key'Pos (F10) - Function_Key'Pos (F1) + 1;
            Width : constant Natural :=
              (if Keys.w > (COUNT + 1) * KEY_GAP then (Keys.w - (COUNT + 1) * KEY_GAP) / COUNT else 0);
         begin
            if Intersects (Keys, Damage) then
               Fill_Rect (C, Keys, Colors.panel);
               Fill_Rect (C, (Keys.x, Keys.y, Keys.w, 1), Colors.edge);
            end if;
            for K in Function_Key loop
               declare
                  Slot : constant Natural := Key_Name'Pos (K) - Key_Name'Pos (F1);
                  Box : constant Rect :=
                    (Keys.x + KEY_GAP + Slot * (Width + KEY_GAP), Keys.y + 2, Width,
                     (if Keys.h > 3 then Keys.h - 3 else 0));
                  Badge : constant String := Key_Badge (K);
                  Badge_Box : constant Rect :=
                    (Box.x + 2, Box.y + 2, UI_Text_Width (Badge) + 2 * KEY_BADGE_PADDING,
                     (if Box.h > 4 then Box.h - 4 else 0));
                  Bound : constant Boolean := Key_Bound (K);
                  Hot : constant Boolean :=
                    Bound and then UI.pointer.enabled and then Point_In_Rect (UI.pointer.x, UI.pointer.y, Box);
                  Pressed : constant Boolean := Bound and then Controls.Is_Active (Map, Key_ID (K));
                  Text_Y : constant Natural :=
                    Box.y + (if Box.h > UI_Text_Height then (Box.h - UI_Text_Height) / 2 else 0);
               begin
                  if Bound then
                     Controls.Add_Button (Map, Key_ID (K), Box, Box);
                     Tooltips.Set (Tips, Key_ID (K), Key_Tip (K), Badge);
                  end if;
                  if Intersects (Box, Damage) and then Width > 0 then
                     declare
                        Inner : constant Canvas := With_Clip (C, Box);
                     begin
                        Fill_Rect (C, Box, (if Pressed then Colors.shadow elsif Hot then Colors.face else Colors.panel));
                        if Hot or else Pressed then
                           Stroke_Rect (C, Box, (if Pressed then Colors.shadow else Colors.edge),
                                        (if Pressed then Colors.edge else Colors.shadow));
                        end if;
                        Fill_Rect (C, Badge_Box, (if Bound then Colors.text else Colors.edge));
                        Draw_UI_Text_Transparent
                          (Inner, Badge_Box.x + KEY_BADGE_PADDING, Text_Y, Badge,
                           (if Bound then Colors.panel else Colors.face));
                        --  A narrow bar shows the keycaps alone (the tooltip and
                        --  the key's action still tell what it does).
                        if Badge_Box.w + 2 * KEY_BADGE_PADDING + UI_Text_Width (Key_Caption (K)) <= Box.w then
                           Draw_UI_Text_Transparent
                             (Inner, Badge_Box.x + Badge_Box.w + KEY_BADGE_PADDING, Text_Y, Key_Caption (K),
                              (if Bound then Colors.text else Colors.muted));
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end;
      end if;
      if State.Overlay then
         Draw_Overlay (State, C, Bounds);
      end if;
      if Prompt /= No_Prompt then
         Draw_Dialog (State, C, Bounds);
      else
         Dialog_Area := (others => 0);
      end if;
      CuBit.UI.Popup_Menus.Draw (C, Map, Popup, Menu, Colors, POPUP_BASE);
      if not CuBit.UI.Popup_Menus.Is_Open (Popup) then
         Tooltips.Draw (C, Tip, Tips, Bounds, Colors);
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

   function Next_Deadline_Us (State : View_State) return Unsigned_64 is
     (Unsigned_64'Min
        (Unsigned_64'Min (Unsigned_64'Min ((if Message_Length > 0 then Message_Until_Us else Unsigned_64'Last),
                                           Files_Reader.Deadline), Files_Operations.Deadline),
         (if Tooltips.Next_Deadline (Tip) >= Unsigned_64'Last / MICROSECONDS_PER_MILLISECOND then Unsigned_64'Last
          else Tooltips.Next_Deadline (Tip) * MICROSECONDS_PER_MILLISECOND)));
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
