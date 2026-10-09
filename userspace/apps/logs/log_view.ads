with Interfaces;
with CuBit.UI;
with CuBit.UI.Combo_Boxes;
with CuBit.UI.Controls;
with CuBit.UI.Editor;
with CuBit.UI.State;
with CuBit.UI.Tables;
with CuBit.Log_Records;
with CuBit.Log_Protocol;
with Log_Viewport;

--  The Logs app's view (docs/logs-app.md): what logstore delivers, as a live
--  table built from the toolkit's controls. A search field, service, time and
--  level combo boxes, a Follow button and a sortable, resizable table above a
--  detail pane. The search marks matching rows among all of them (Only shows
--  the matches alone); the table scrolls freely of its selection
--  (Log_Viewport). Platform-free: the native main feeds it from a log-observer
--  subscription; hosted tests feed it records and read frames back.
package Log_View is
   use Interfaces;
   package Controls renames CuBit.UI.Controls;

   --  Input, as the platform translates it. Pointer events arrive after the
   --  toolkit's retained dispatch (CuBit.UI.App.Run, or the test doing the
   --  same), so Handle sees only what the controls reported.
   type Key_Name is
     (No_Key, Up, Down, Left, Right, Page_Up, Page_Down, Home, End_Key, Enter, Escape, Backspace, Delete,
      Tab, Space, F3);
   type Event_Kind is (Key_Event, Text_Event, Pointer_Event, Wheel, Resize);
   type Event is record
      Kind : Event_Kind := Key_Event;
      Key : Key_Name := No_Key;
      Shift, Control : Boolean := False;
      Character_Value : Character := ' ';
      Action : Controls.Pointer_Action := Controls.Pointer_Move;
      X, Y : Natural := 0;
      --  Wheel: positive scrolls toward older rows.
      Steps : Integer := 0;
   end record;

   --  How the app reaches logstore, for the status bar and its explanation.
   type Connection is (Connecting, Connected, Denied, Unavailable);
   --  Records shown: all, or those from the last minute ... hour.
   type Time_Window is (All_Time, Last_Minute, Last_5_Minutes, Last_15_Minutes, Last_Hour);
   type Column_Name is (Time_Column, Node_Column, Level_Column, Source_Column, Message_Column);

   --  Records kept for scrolling back; older ones leave first.
   MAXIMUM_RECORDS : constant := Log_Viewport.MAXIMUM_ROWS;
   --  Sources that published, each with a name; one service choice per
   --  distinct name, after "All services".
   MAXIMUM_SOURCES : constant := CuBit.UI.Combo_Boxes.Max_Choices - 1;
   MAXIMUM_NAME : constant := 64;

   --  Control IDs (CuBit.UI.Controls), for the platform and tests.
   SEARCH_ID : constant Controls.Control_ID := 1;
   FOLLOW_ID : constant Controls.Control_ID := 2;
   CLEAR_ID : constant Controls.Control_ID := 3;
   SCROLL_ID : constant Controls.Control_ID := 4;
   ONLY_ID : constant Controls.Control_ID := 5;
   COLUMNS_BASE : constant CuBit.UI.Tables.Column_ID_Base := 10;
   SERVICE_BASE : constant CuBit.UI.Combo_Boxes.ID_Base := 100;
   TIME_BASE : constant CuBit.UI.Combo_Boxes.ID_Base := 200;
   LEVEL_BASE : constant CuBit.UI.Combo_Boxes.ID_Base := 300;
   KEEP_BASE : constant CuBit.UI.Combo_Boxes.ID_Base := 400;
   ROW_FIRST : constant Controls.Control_ID := 1_000;
   function Header_ID (Column : Column_Name) return Controls.Control_ID is
     (COLUMNS_BASE + CuBit.UI.Tables.MAX_COLUMNS + Column_Name'Pos (Column));

   type View_State is limited private;

   procedure Initialize (State : out View_State);
   --  A record as logstore delivered it: when (monotonic ms), from whom, and
   --  on which node.
   procedure Add
     (State : in out View_State; Time_Ms : Unsigned_64; Source : Unsigned_64;
      Item : CuBit.Log_Records.Log_Record;
      Node : CuBit.Log_Protocol.Node_Id := CuBit.Log_Protocol.This_Node);
   --  Records logstore could not deliver (a Gap): shown in place.
   procedure Add_Gap (State : in out View_State; Lost : Unsigned_64);
   --  What to call a source: a service name or a manifest identity. Process
   --  numbers are reused, so the name covers only records published at or
   --  after Started (the process's start, monotonic ms); older records from
   --  the same number were another process's and show the bare number.
   procedure Name_Source
     (State : in out View_State; Source : Unsigned_64; Name : String; Started : Unsigned_64 := 0);
   procedure Set_Connection (State : in out View_State; Value : Connection);
   --  What logstore keeps (its minimum), as the platform last learned it.
   procedure Set_Kept (State : in out View_State; Level : CuBit.Log_Records.Severity);
   --  Why a change to what logstore keeps did not happen, for the status bar.
   procedure Set_Keep_Refused (State : in out View_State; Reason : String);
   --  A change to what logstore keeps that the person chose, taken once by
   --  the platform, which asks logstore and reports back with Set_Kept or
   --  Set_Keep_Refused.
   procedure Take_Keep_Request
     (State : in out View_State; Level : out CuBit.Log_Records.Severity; Requested : out Boolean);
   --  The current monotonic time, for the time window.
   procedure Set_Time (State : in out View_State; Now_Ms : Unsigned_64);

   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean);
   procedure Render
     (State : in out View_State; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect;
      UI : in out CuBit.UI.State.UI_State; Map : in out Controls.Control_Map);

   --  Advances whenever what the view would draw changes: the platform
   --  redraws only then.
   function Revision (State : View_State) return Unsigned_64;
   --  The part of Bounds that streaming records change: everything below
   --  the toolbar.
   function Content_Area (Bounds : CuBit.UI.Rect) return CuBit.UI.Rect;

   --  What the view shows, for tests and the window title.
   function Total (State : View_State) return Natural;
   function Shown (State : View_State) return Natural;
   function Following (State : View_State) return Boolean;
   function Minimum (State : View_State) return CuBit.Log_Records.Severity;
   function Window (State : View_State) return Time_Window;
   --  What logstore keeps, if known.
   function Kept_Known (State : View_State) return Boolean;
   function Kept (State : View_State) return CuBit.Log_Records.Severity;
   function Search_Text (State : View_State) return String;
   --  The service shown alone, or "" for every service.
   function Service_Filter (State : View_State) return String;
   function Sorted_By (State : View_State) return Column_Name;
   function Descending (State : View_State) return Boolean;
   --  The message of the Row'th shown row (1 = top of the table).
   function Row_Text (State : View_State; Row : Positive) return String;
   function Selected_Text (State : View_State) return String;
   --  New records not yet looked at while paused.
   function Unseen (State : View_State) return Natural;
   --  The first row drawn and the selected row (0: none), in shown rows.
   function Top_Row (State : View_State) return Positive;
   function Selected_Row (State : View_State) return Natural;
   --  Search matches among the shown rows: whether Row is one, how many,
   --  and which of them is selected (0: the selection is not a match).
   function Row_Matches (State : View_State; Row : Positive) return Boolean;
   function Match_Count (State : View_State) return Natural;
   function Match_Index (State : View_State) return Natural;
   --  Whether Only is on: with a search, only its matches are shown.
   function Only_Matches (State : View_State) return Boolean;
private
   package CB renames CuBit.UI.Combo_Boxes;
   type Entry_Kind is (Record_Entry, Gap_Entry);
   type Log_Entry is record
      Kind : Entry_Kind := Record_Entry;
      Sequence : Unsigned_64 := 0;
      Time_Ms : Unsigned_64 := 0;
      Source : Unsigned_64 := 0;
      Node : CuBit.Log_Protocol.Node_Id := CuBit.Log_Protocol.This_Node;
      Item : CuBit.Log_Records.Log_Record := CuBit.Log_Records.Empty_Record;
      Lost : Unsigned_64 := 0;
      --  Whether the search matches it.
      Hit : Boolean := False;
   end record;
   subtype Entry_Count is Log_Viewport.Row_Count;
   subtype Entry_Slot is Natural range 0 .. MAXIMUM_RECORDS - 1;
   type Entry_Ring is array (Entry_Slot) of Log_Entry;
   --  Shown entries, by sequence number, in table order.
   type Sequence_List is array (1 .. MAXIMUM_RECORDS) of Unsigned_64;

   subtype Name_Length is Natural range 0 .. MAXIMUM_NAME;
   type Source_Name is record
      Source : Unsigned_64 := 0;
      Name : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Length : Name_Length := 0;
      Started : Unsigned_64 := 0;
   end record;
   subtype Source_Count is Natural range 0 .. MAXIMUM_SOURCES;
   type Source_Table is array (1 .. MAXIMUM_SOURCES) of Source_Name;
   --  A process that published, and when it first did.
   type Publisher_Seen is record
      Source : Unsigned_64 := 0;
      First_Ms : Unsigned_64 := 0;
   end record;
   type Publisher_Table is array (1 .. MAXIMUM_SOURCES) of Publisher_Seen;
   --  Distinct service names, the service combo box's captions after the
   --  first. Fixed buffers borrowed by the model during synchronous calls.
   type Caption_Table is array (1 .. MAXIMUM_SOURCES) of aliased String (1 .. MAXIMUM_NAME);
   type Caption_Lengths is array (1 .. MAXIMUM_SOURCES) of Name_Length;
   type Severity_Counts is array (CuBit.Log_Records.Severity) of Natural;
   type Focus_Target is (List_Focus, Search_Focus, Service_Focus, Time_Focus, Level_Focus, Keep_Focus);
   MAXIMUM_NOTE : constant := 120;

   type View_State is limited record
      Entries : Entry_Ring;
      Count : Entry_Count := 0;
      Next_Sequence : Unsigned_64 := 1;
      Visible : Sequence_List := [others => 0];
      Names : Source_Table;
      Name_Count : Source_Count := 0;
      Publishers : Publisher_Table;
      Publisher_Count : Source_Count := 0;
      Captions : Caption_Table;
      Caption_Length : Caption_Lengths := [others => 0];
      Service_Count : Source_Count := 0;
      Levels : Severity_Counts := [others => 0];
      Lost : Unsigned_64 := 0;
      Now_Ms : Unsigned_64 := 0;
      --  Filters: the search field (marking matches, or with Only showing
      --  just them) and the three combo boxes.
      Search : CuBit.UI.Editor.Edit_State;
      Only : Boolean := False;
      --  Which way typing looks for the nearest match: away from the newest
      --  record for a search begun while following, else forward.
      Search_Forward : Boolean := True;
      Service : CB.Combo_State;
      Service_Model : CB.Model;
      --  The chosen service's name, kept across name updates; "" for all.
      Service_Name : String (1 .. MAXIMUM_NAME) := [others => ' '];
      Service_Name_Length : Name_Length := 0;
      Time : CB.Combo_State;
      Time_Model : CB.Model;
      Level : CB.Combo_State;
      Level_Model : CB.Model;
      --  What logstore keeps: shown, and changed through log-control.
      Keep : CB.Combo_State;
      Keep_Known : Boolean := False;
      Keep_Requested : Boolean := False;
      Keep_Request : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Keep_Note : String (1 .. MAXIMUM_NOTE) := [others => ' '];
      Keep_Note_Length : Natural range 0 .. MAXIMUM_NOTE := 0;
      Columns : CuBit.UI.Tables.Column_Layout;
      Focus : Focus_Target := List_Focus;
      --  The shown rows, the selected one, the first drawn, the rows that
      --  fit (from the last Render) and following.
      Position : Log_Viewport.Viewport;
      New_Since_Pause : Natural := 0;
      Link : Connection := Connecting;
      Revision : Unsigned_64 := 0;
   end record;
end Log_View;
