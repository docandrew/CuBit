with CuBit.UI; use CuBit.UI;
with CuBit.UI.Widgets;

package body Log_View is
   package LR renames CuBit.Log_Records;
   package Ed renames CuBit.UI.Editor;
   package Tables renames CuBit.UI.Tables;
   use type LR.Severity;
   use type LR.Field_Kind;
   use type Controls.Pointer_Action;
   use type Tables.Sort_Order;
   use type CuBit.Log_Protocol.Node_Id;

   --  Layout, in pixels.
   MARGIN : constant := 8;
   GAP : constant := 6;
   TOOLBAR_HEIGHT : constant := 38;
   CONTROL_HEIGHT : constant := CB.Default_Height;
   ROW_HEIGHT : constant := 20;
   DETAIL_HEIGHT : constant := 112;
   STATUS_HEIGHT : constant := CONTROL_HEIGHT;
   KEEP_WIDTH : constant := 170;
   SCROLLBAR_WIDTH : constant := 14;
   SEARCH_MINIMUM : constant := 140;
   PLACEHOLDER_INSET : constant := 6;
   SERVICE_WIDTH : constant := 170;
   TIME_WIDTH : constant := 140;
   LEVEL_WIDTH : constant := 150;
   FOLLOW_WIDTH : constant := 72;
   CLEAR_WIDTH : constant := 60;
   --  Rows moved by one wheel step.
   WHEEL_ROWS : constant := 3;

   --  Captions with program lifetime, borrowed by the combo box models.
   ALL_SERVICES : aliased constant String := "All services";
   ALL_TIME_CAPTION : aliased constant String := "All time";
   LAST_MINUTE_CAPTION : aliased constant String := "Last minute";
   LAST_5_MINUTES_CAPTION : aliased constant String := "Last 5 minutes";
   LAST_15_MINUTES_CAPTION : aliased constant String := "Last 15 minutes";
   LAST_HOUR_CAPTION : aliased constant String := "Last hour";
   ALL_LEVELS : aliased constant String := "All levels";
   DEBUG_UP : aliased constant String := "Debug and above";
   INFO_UP : aliased constant String := "Info and above";
   WARNING_UP : aliased constant String := "Warnings and above";
   ERROR_UP : aliased constant String := "Errors and above";
   CRITICAL_ONLY : aliased constant String := "Critical only";
   type Window_Captions is array (Time_Window) of CB.Text;
   WINDOW_CAPTION : constant Window_Captions :=
     [ALL_TIME_CAPTION'Access, LAST_MINUTE_CAPTION'Access, LAST_5_MINUTES_CAPTION'Access, LAST_15_MINUTES_CAPTION'Access, LAST_HOUR_CAPTION'Access];
   type Level_Captions is array (LR.Severity) of CB.Text;
   LEVEL_CAPTION : constant Level_Captions :=
     [ALL_LEVELS'Access, DEBUG_UP'Access, INFO_UP'Access, WARNING_UP'Access, ERROR_UP'Access,
      CRITICAL_ONLY'Access];
   type Window_Lengths is array (Time_Window) of Unsigned_64;
   WINDOW_MS : constant Window_Lengths := [0, 60_000, 300_000, 900_000, 3_600_000];
   function Column_Of (Name : Column_Name) return Tables.Column_Index is (Column_Name'Pos (Name) + 1);
   function Name_Of_Column (Column : Tables.Column_Index) return Column_Name is
     (Column_Name'Val (Column - 1));

   --  Whether a background is dark: colors beyond the theme have a light
   --  and a dark variant, so they read on either.
   function Dark (Background : Color) return Boolean is
     ((Shift_Right (Unsigned_32 (Background), 16) and 16#FF#) * 3 +
      (Shift_Right (Unsigned_32 (Background), 8) and 16#FF#) * 6 +
      (Unsigned_32 (Background) and 16#FF#) < 128 * 10);
   --  One color per severity.
   function Level_Color (Level : LR.Severity; On : Color) return Color is
     (if Dark (On) then
        (case Level is
            when LR.Trace => 16#7C8696#, when LR.Debug => 16#9AA5B5#,
            when LR.Information => 16#73D0FF#, when LR.Warning => 16#FFCC66#,
            when LR.Error => 16#F28779#, when LR.Critical => 16#FF4D4D#)
      else
        (case Level is
            when LR.Trace => 16#6A7280#, when LR.Debug => 16#4E5866#,
            when LR.Information => 16#0B6BB8#, when LR.Warning => 16#9A6200#,
            when LR.Error => 16#C0392B#, when LR.Critical => 16#B00000#));
   function Source_Color (On : Color) return Color is
     (if Dark (On) then 16#FFCC66# else 16#7A3E9D#);
   function Time_Color (On : Color) return Color is
     (if Dark (On) then 16#9AA5B5# else 16#5A6372#);
   function Level_Label (Level : LR.Severity) return String is
     (case Level is
         when LR.Trace => "TRACE", when LR.Debug => "DEBUG", when LR.Information => "INFO",
         when LR.Warning => "WARN", when LR.Error => "ERROR", when LR.Critical => "CRIT");

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;
   function Image64 (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image64;
   function Two (Value : Unsigned_64) return String is
     (if Value < 10 then "0" & Image64 (Value) else Image64 (Value));
   function Three (Value : Unsigned_64) return String is
     (if Value < 10 then "00" & Image64 (Value) elsif Value < 100 then "0" & Image64 (Value)
      else Image64 (Value));
   --  Time since boot, as hh:mm:ss.mmm.
   function Uptime (Ms : Unsigned_64) return String is
     (Two (Ms / 3_600_000) & ":" & Two ((Ms / 60_000) mod 60) & ":" & Two ((Ms / 1_000) mod 60) & "." &
      Three (Ms mod 1_000));

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
   --  Whether Needle occurs in Hay, ignoring case.
   function Contains (Hay, Needle : String) return Boolean is
   begin
      if Needle'Length = 0 then return True; end if;
      if Needle'Length > Hay'Length then return False; end if;
      for Start in Hay'First .. Hay'Last - Needle'Length + 1 loop
         if (for all I in Needle'Range =>
               Lower (Hay (Start + I - Needle'First)) = Lower (Needle (I)))
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains;
   --  -1, 0 or 1 as Left sorts before, with or after Right, ignoring case.
   function Compare_Text (Left, Right : String) return Integer is
   begin
      for I in 0 .. Natural'Min (Left'Length, Right'Length) - 1 loop
         if Lower (Left (Left'First + I)) /= Lower (Right (Right'First + I)) then
            return (if Lower (Left (Left'First + I)) < Lower (Right (Right'First + I)) then -1 else 1);
         end if;
      end loop;
      return (if Left'Length < Right'Length then -1 elsif Left'Length > Right'Length then 1 else 0);
   end Compare_Text;
   function Compare (Left, Right : Unsigned_64) return Integer is
     (if Left < Right then -1 elsif Left > Right then 1 else 0);

   --  Ring storage, by sequence number.
   function Slot (Sequence : Unsigned_64) return Entry_Slot is
     (Entry_Slot ((Sequence - 1) mod MAXIMUM_RECORDS));
   function Oldest (State : View_State) return Unsigned_64 is
     (State.Next_Sequence - Unsigned_64 (State.Count));
   function At_Sequence (State : View_State; Sequence : Unsigned_64) return Log_Entry is
     (State.Entries (Slot (Sequence)));
   function Shown_Entry (State : View_State; Row : Positive) return Log_Entry is
     (At_Sequence (State, State.Visible (Row)));

   --  The name of Source as it was at Time_Ms (default: now).
   function Name_Of
     (State : View_State; Source : Unsigned_64; Time_Ms : Unsigned_64 := Unsigned_64'Last) return String is
   begin
      for I in 1 .. State.Name_Count loop
         if State.Names (I).Source = Source and then Time_Ms >= State.Names (I).Started then
            return State.Names (I).Name (1 .. State.Names (I).Length);
         end if;
      end loop;
      return "pid " & Image64 (Source);
   end Name_Of;
   --  A node: "local" for this one, else its identity in hex (Short: the
   --  first 8 digits, for the table).
   HEX_DIGITS : constant String := "0123456789abcdef";
   function Hex (Value : Unsigned_64; Digits_Shown : Positive) return String is
      Text : String (1 .. Digits_Shown);
   begin
      for I in Text'Range loop
         Text (I) := HEX_DIGITS (Natural (Shift_Right (Value, 64 - 4 * I) and 16#F#) + 1);
      end loop;
      return Text;
   end Hex;
   function Node_Text (Node : CuBit.Log_Protocol.Node_Id; Short : Boolean := True) return String is
     (if Node = CuBit.Log_Protocol.This_Node then "local"
      elsif Short then Hex (Node.High, 8)
      else Hex (Node.High, 16) & Hex (Node.Low, 16));
   function Gap_Text (Item : Log_Entry) return String is
     ("--- " & Image64 (Item.Lost) & " records lost before delivery ---");
   function Message_Of (Item : Log_Entry) return String is
     (if Item.Kind = Gap_Entry then Gap_Text (Item) else LR.Text (Item.Item));

   --  The filters, as the controls hold them.
   function Floor (State : View_State) return LR.Severity is
     (LR.Severity'Val (Natural'Max (1, CB.Selection (State.Level)) - 1));
   function Window (State : View_State) return Time_Window is
     (Time_Window'Val (Natural'Max (1, CB.Selection (State.Time)) - 1));
   function Query (State : View_State) return String is (Ed.Content (State.Search));
   function Filtered (State : View_State) return Boolean is
     (Query (State)'Length > 0 or else State.Service_Name_Length > 0 or else
      Window (State) /= All_Time or else Floor (State) /= LR.Trace);

   function Matches (State : View_State; Item : Log_Entry) return Boolean is
   begin
      if Item.Kind = Gap_Entry then
         return True;
      end if;
      declare
         Name : constant String := Name_Of (State, Item.Source, Item.Time_Ms);
         Span : constant Unsigned_64 := WINDOW_MS (Window (State));
      begin
         return LR.Level (Item.Item) >= Floor (State)
           and then (Span = 0 or else Item.Time_Ms >= State.Now_Ms or else State.Now_Ms - Item.Time_Ms <= Span)
           and then (State.Service_Name_Length = 0 or else
                     Name = State.Service_Name (1 .. State.Service_Name_Length))
           and then (Contains (LR.Text (Item.Item), Query (State)) or else Contains (Name, Query (State))
                     or else Contains (Node_Text (Item.Node, Short => False), Query (State)));
      end;
   end Matches;

   --  Table order: the sort column's value, then arrival.
   function Before (State : View_State; Left, Right : Unsigned_64) return Boolean is
      --  In place: entries are large, and sorting compares many.
      A : Log_Entry renames State.Entries (Slot (Left));
      B : Log_Entry renames State.Entries (Slot (Right));
      function Rank (Item : Log_Entry) return Natural is
        (if Item.Kind = Gap_Entry then LR.Severity'Pos (LR.Severity'Last) + 1
         else LR.Severity'Pos (LR.Level (Item.Item)));
      Order : Integer := 0;
   begin
      if State.Columns.Sort_Column > 0 then
         case Name_Of_Column (State.Columns.Sort_Column) is
            when Time_Column => Order := Compare (A.Time_Ms, B.Time_Ms);
            when Node_Column =>
               Order := Compare (A.Node.High, B.Node.High);
               if Order = 0 then
                  Order := Compare (A.Node.Low, B.Node.Low);
               end if;
            when Level_Column => Order := Compare (Unsigned_64 (Rank (A)), Unsigned_64 (Rank (B)));
            when Source_Column =>
               Order := Compare_Text (Name_Of (State, A.Source, A.Time_Ms), Name_Of (State, B.Source, B.Time_Ms));
            when Message_Column => Order := Compare_Text (Message_Of (A), Message_Of (B));
         end case;
      end if;
      if Order = 0 then
         Order := Compare (Left, Right);
      end if;
      return (if State.Columns.Order = Tables.Descending then Order > 0 else Order < 0);
   end Before;

   --  Heap sort of the shown rows into table order.
   procedure Sort (State : in out View_State) is
      Count : constant Natural := State.Visible_Count;
      procedure Sift (Start, Last : Positive) is
         Root : Positive := Start;
         Child : Positive;
         Held : Unsigned_64;
      begin
         while Root * 2 <= Last loop
            Child := Root * 2;
            if Child < Last and then Before (State, State.Visible (Child), State.Visible (Child + 1)) then
               Child := Child + 1;
            end if;
            exit when not Before (State, State.Visible (Root), State.Visible (Child));
            Held := State.Visible (Root);
            State.Visible (Root) := State.Visible (Child);
            State.Visible (Child) := Held;
            Root := Child;
         end loop;
      end Sift;
      Held : Unsigned_64;
   begin
      if Count < 2 then return; end if;
      for Start in reverse 1 .. Count / 2 loop
         Sift (Start, Count);
      end loop;
      for Last in reverse 2 .. Count loop
         Held := State.Visible (1);
         State.Visible (1) := State.Visible (Last);
         State.Visible (Last) := Held;
         Sift (1, Last - 1);
      end loop;
   end Sort;

   --  The row showing the newest record.
   function Newest_Row (State : View_State) return Entry_Count is
      Row : Entry_Count := 0;
      Newest : Unsigned_64 := 0;
      By_Time : constant Boolean := State.Columns.Sort_Column = Column_Of (Time_Column);
   begin
      --  Arrival order is time order, ties broken by arrival: no search.
      if State.Visible_Count = 0 or else (By_Time and then State.Columns.Order = Tables.Ascending) then
         return State.Visible_Count;
      elsif By_Time then
         return 1;
      end if;
      for I in 1 .. State.Visible_Count loop
         if State.Visible (I) > Newest then
            Newest := State.Visible (I);
            Row := I;
         end if;
      end loop;
      return Row;
   end Newest_Row;

   --  Keep the selection in view; following selects the newest record.
   procedure Settle (State : in out View_State) is
      Rows : constant Positive := State.Rows;
      Last_Top : constant Natural := (if State.Visible_Count > Rows then State.Visible_Count - Rows else 0);
   begin
      if State.Follow then
         State.Selected := Newest_Row (State);
         State.New_Since_Pause := 0;
      end if;
      State.Selected := Natural'Min (State.Selected, State.Visible_Count);
      if State.Selected > 0 then
         if State.Selected <= State.Top then
            State.Top := State.Selected - 1;
         elsif State.Selected > State.Top + Rows then
            State.Top := State.Selected - Rows;
         end if;
      end if;
      State.Top := Natural'Min (State.Top, Last_Top);
   end Settle;

   procedure Rebuild (State : in out View_State) is
      Selected_Sequence : constant Unsigned_64 :=
        (if State.Selected in 1 .. State.Visible_Count then State.Visible (State.Selected) else 0);
   begin
      State.Revision := State.Revision + 1;
      State.Visible_Count := 0;
      for Sequence in Oldest (State) .. State.Next_Sequence - 1 loop
         if Matches (State, State.Entries (Slot (Sequence))) then
            State.Visible_Count := State.Visible_Count + 1;
            State.Visible (State.Visible_Count) := Sequence;
         end if;
      end loop;
      Sort (State);
      State.Selected := 0;
      for I in 1 .. State.Visible_Count loop
         if State.Visible (I) = Selected_Sequence then
            State.Selected := I;
         end if;
      end loop;
      Settle (State);
   end Rebuild;

   --  The service combo box: every distinct name, after "All services".
   procedure Rebuild_Services (State : in out View_State) is
      Chosen : Natural := 1;
   begin
      State.Service_Model := (others => <>);
      State.Service_Model.Count := 1;
      State.Service_Model.Choices (1) := (ALL_SERVICES'Access, True);
      for I in 1 .. State.Service_Count loop
         State.Service_Model.Count := State.Service_Model.Count + 1;
         --  Fixed buffers in the view, borrowed only during synchronous UI calls.
         State.Service_Model.Choices (State.Service_Model.Count) := (State.Captions (I)'Unrestricted_Access, True);
         if State.Service_Name_Length > 0 and then
           State.Captions (I) (1 .. State.Caption_Length (I)) = State.Service_Name (1 .. State.Service_Name_Length)
         then
            Chosen := State.Service_Model.Count;
         end if;
      end loop;
      CB.Set_Selection (State.Service, State.Service_Model, Chosen);
   end Rebuild_Services;

   procedure Choose_Service (State : in out View_State; Name : String) is
      Length : constant Name_Length := Natural'Min (Name'Length, MAXIMUM_NAME);
   begin
      State.Service_Name := [others => ' '];
      State.Service_Name (1 .. Length) := Name (Name'First .. Name'First + Length - 1);
      State.Service_Name_Length := Length;
      Rebuild_Services (State);
   end Choose_Service;

   procedure Initialize (State : out View_State) is
      Accepted : Boolean;
      Closed : CB.Combo_State;
   begin
      --  Field by field: the records themselves are only read once stored.
      State.Count := 0;
      State.Next_Sequence := 1;
      State.Visible_Count := 0;
      State.Name_Count := 0;
      State.Publisher_Count := 0;
      State.Service_Count := 0;
      State.Caption_Length := [others => 0];
      State.Levels := [others => 0];
      State.Lost := 0;
      State.Now_Ms := 0;
      Ed.Initialize (State.Search, "", Accepted);
      State.Service_Name := [others => ' '];
      State.Service_Name_Length := 0;
      State.Service := Closed;
      State.Time := Closed;
      State.Level := Closed;
      State.Keep := Closed;
      State.Keep_Known := False;
      State.Keep_Requested := False;
      State.Keep_Note_Length := 0;
      State.Time_Model := (others => <>);
      for Span in Time_Window loop
         State.Time_Model.Count := State.Time_Model.Count + 1;
         State.Time_Model.Choices (State.Time_Model.Count) := (WINDOW_CAPTION (Span), True);
      end loop;
      CB.Set_Selection (State.Time, State.Time_Model, 1);
      State.Level_Model := (others => <>);
      for Level in LR.Severity loop
         State.Level_Model.Count := State.Level_Model.Count + 1;
         State.Level_Model.Choices (State.Level_Model.Count) := (LEVEL_CAPTION (Level), True);
      end loop;
      CB.Set_Selection (State.Level, State.Level_Model, 1);
      CB.Set_Selection (State.Keep, State.Level_Model, 1);
      Rebuild_Services (State);
      State.Columns :=
        (Count => Column_Name'Pos (Column_Name'Last) + 1,
         Width => [1 => 118, 2 => 76, 3 => 60, 4 => 190, others => 96],
         Minimum => [1 => 100, 2 => 48, 3 => 48, 4 => 72, 5 => 120, others => 32],
         Sortable => True, Sort_Column => Column_Of (Time_Column), Order => Tables.Ascending,
         Cell_Padding => 5);
      State.Focus := List_Focus;
      State.Selected := 0;
      State.Top := 0;
      State.Follow := True;
      State.New_Since_Pause := 0;
      State.Link := Connecting;
      State.Rows := 20;
   end Initialize;

   procedure Recompute_Services (State : in out View_State);

   procedure Store (State : in out View_State; Item : Log_Entry) is
      Kept : Log_Entry := Item;
      Position : Positive;
   begin
      State.Revision := State.Revision + 1;
      --  A full ring lets its oldest entry go, and with it any row showing it.
      if State.Count = MAXIMUM_RECORDS then
         for I in 1 .. State.Visible_Count loop
            if State.Visible (I) = Oldest (State) then
               State.Visible (I .. State.Visible_Count - 1) := State.Visible (I + 1 .. State.Visible_Count);
               State.Visible_Count := State.Visible_Count - 1;
               if State.Selected >= I and then State.Selected > 0 then State.Selected := State.Selected - 1; end if;
               if State.Top >= I and then State.Top > 0 then State.Top := State.Top - 1; end if;
               exit;
            end if;
         end loop;
         State.Count := State.Count - 1;
      end if;
      Kept.Sequence := State.Next_Sequence;
      State.Entries (Slot (State.Next_Sequence)) := Kept;
      State.Next_Sequence := State.Next_Sequence + 1;
      State.Count := State.Count + 1;
      if Matches (State, Kept) then
         --  Into table order: after every row it does not sort before.
         Position := State.Visible_Count + 1;
         while Position > 1 and then Before (State, Kept.Sequence, State.Visible (Position - 1)) loop
            Position := Position - 1;
         end loop;
         State.Visible (Position + 1 .. State.Visible_Count + 1) := State.Visible (Position .. State.Visible_Count);
         State.Visible (Position) := Kept.Sequence;
         State.Visible_Count := State.Visible_Count + 1;
         if State.Selected >= Position then State.Selected := State.Selected + 1; end if;
         if not State.Follow then
            State.New_Since_Pause := State.New_Since_Pause + 1;
            if State.Top >= Position then State.Top := State.Top + 1; end if;
         end if;
      end if;
      Settle (State);
   end Store;

   procedure Add
     (State : in out View_State; Time_Ms : Unsigned_64; Source : Unsigned_64;
      Item : CuBit.Log_Records.Log_Record;
      Node : CuBit.Log_Protocol.Node_Id := CuBit.Log_Protocol.This_Node) is
   begin
      State.Levels (LR.Level (Item)) := State.Levels (LR.Level (Item)) + 1;
      State.Now_Ms := Unsigned_64'Max (State.Now_Ms, Time_Ms);
      if not (for some I in 1 .. State.Publisher_Count => State.Publishers (I).Source = Source)
        and then State.Publisher_Count < MAXIMUM_SOURCES
      then
         State.Publisher_Count := State.Publisher_Count + 1;
         State.Publishers (State.Publisher_Count) := (Source => Source, First_Ms => Time_Ms);
         Recompute_Services (State);
      end if;
      Store (State, (Kind => Record_Entry, Time_Ms => Time_Ms, Source => Source, Node => Node, Item => Item,
                     others => <>));
   end Add;

   procedure Add_Gap (State : in out View_State; Lost : Unsigned_64) is
   begin
      State.Lost := State.Lost + Lost;
      --  Shown where the loss was noticed: at the latest time seen.
      Store (State, (Kind => Gap_Entry, Lost => Lost, Time_Ms => State.Now_Ms, others => <>));
   end Add_Gap;

   --  The service choices: every distinct name a publisher's records carry.
   --  A publisher named after its first record also published as "pid N".
   procedure Recompute_Services (State : in out View_State) is
      Changed : Boolean := False;
      procedure Offer (Text : String) is
      begin
         for I in 1 .. State.Service_Count loop
            if State.Captions (I) (1 .. State.Caption_Length (I)) = Text then
               return;
            end if;
         end loop;
         if Text'Length > 0 and then State.Service_Count < MAXIMUM_SOURCES then
            State.Service_Count := State.Service_Count + 1;
            State.Captions (State.Service_Count) := [others => ' '];
            State.Captions (State.Service_Count) (1 .. Text'Length) := Text;
            State.Caption_Length (State.Service_Count) := Text'Length;
            Changed := True;
         end if;
      end Offer;
      Before_Count : constant Source_Count := State.Service_Count;
   begin
      State.Service_Count := 0;
      for I in 1 .. State.Publisher_Count loop
         declare
            Seen : Publisher_Seen renames State.Publishers (I);
            Bare : constant String := "pid " & Image64 (Seen.Source);
            Now_Name : constant String := Name_Of (State, Seen.Source);
         begin
            if Name_Of (State, Seen.Source, Seen.First_Ms) = Bare then
               Offer (Bare);
            end if;
            Offer (Now_Name);
         end;
      end loop;
      if Changed or else State.Service_Count /= Before_Count then
         Rebuild_Services (State);
      end if;
   end Recompute_Services;

   procedure Name_Source
     (State : in out View_State; Source : Unsigned_64; Name : String; Started : Unsigned_64 := 0) is
      Length : constant Name_Length := Natural'Min (Name'Length, MAXIMUM_NAME);
      Text : constant String := Name (Name'First .. Name'First + Length - 1);
      Index : Natural := 0;
      Published : Boolean := False;
   begin
      --  Only publishers are named: the list of services is who logged.
      for I in 1 .. State.Publisher_Count loop
         Published := Published or else State.Publishers (I).Source = Source;
      end loop;
      if not Published then
         return;
      end if;
      for I in 1 .. State.Name_Count loop
         if State.Names (I).Source = Source then Index := I; end if;
      end loop;
      if Index > 0 and then State.Names (Index).Started = Started and then
        State.Names (Index).Name (1 .. State.Names (Index).Length) = Text
      then
         return;
      end if;
      if Index = 0 then
         if State.Name_Count = MAXIMUM_SOURCES then return; end if;
         State.Name_Count := State.Name_Count + 1;
         Index := State.Name_Count;
      end if;
      State.Names (Index) := (Source => Source, Length => Length, Started => Started, others => <>);
      State.Names (Index).Name (1 .. Length) := Text;
      State.Revision := State.Revision + 1;
      Recompute_Services (State);
      --  A name changes which rows match only through a name filter, the
      --  search, or the order when sorted by source.
      if State.Service_Name_Length > 0 or else Query (State)'Length > 0 or else
        State.Columns.Sort_Column = Column_Of (Source_Column)
      then
         Rebuild (State);
      end if;
   end Name_Source;

   procedure Set_Connection (State : in out View_State; Value : Connection) is
   begin
      if State.Link /= Value then
         State.Revision := State.Revision + 1;
      end if;
      State.Link := Value;
   end Set_Connection;

   procedure Set_Kept (State : in out View_State; Level : LR.Severity) is
   begin
      if not State.Keep_Known or else CB.Selection (State.Keep) /= LR.Severity'Pos (Level) + 1 then
         State.Revision := State.Revision + 1;
      end if;
      State.Keep_Known := True;
      --  Leave an open popup, or a change on its way, alone.
      if not CB.Is_Open (State.Keep) and then not State.Keep_Requested then
         CB.Set_Selection (State.Keep, State.Level_Model, LR.Severity'Pos (Level) + 1);
      end if;
   end Set_Kept;

   procedure Set_Keep_Refused (State : in out View_State; Reason : String) is
      Length : constant Natural := Natural'Min (Reason'Length, MAXIMUM_NOTE);
   begin
      State.Keep_Note (1 .. Length) := Reason (Reason'First .. Reason'First + Length - 1);
      State.Revision := State.Revision + 1;
      State.Keep_Note_Length := Length;
   end Set_Keep_Refused;

   procedure Take_Keep_Request
     (State : in out View_State; Level : out LR.Severity; Requested : out Boolean) is
   begin
      Requested := State.Keep_Requested;
      Level := State.Keep_Request;
      State.Keep_Requested := False;
      if Requested then
         State.Keep_Note_Length := 0;
      end if;
   end Take_Keep_Request;

   procedure Set_Time (State : in out View_State; Now_Ms : Unsigned_64) is
      Span : constant Unsigned_64 := WINDOW_MS (Window (State));
      Expired : Boolean := False;
   begin
      State.Now_Ms := Unsigned_64'Max (State.Now_Ms, Now_Ms);
      if Span > 0 then
         for I in 1 .. State.Visible_Count loop
            declare
               Item : Log_Entry renames State.Entries (Slot (State.Visible (I)));
            begin
               Expired := Expired or else
                 (Item.Kind = Record_Entry and then State.Now_Ms > Item.Time_Ms and then
                  State.Now_Ms - Item.Time_Ms > Span);
            end;
         end loop;
         if Expired then
            Rebuild (State);
         end if;
      end if;
   end Set_Time;

   --  Moving by hand pauses following; End resumes it.
   procedure Move (State : in out View_State; Rows : Integer) is
      Target : constant Integer := Integer (State.Selected) + Rows;
   begin
      if State.Visible_Count = 0 then return; end if;
      State.Follow := False;
      State.Selected := Natural (Integer'Max (1, Integer'Min (Target, Integer (State.Visible_Count))));
      Settle (State);
   end Move;

   procedure Clear_Filters (State : in out View_State) is
      Accepted : Boolean;
   begin
      Ed.Initialize (State.Search, "", Accepted);
      State.Service_Name_Length := 0;
      Rebuild_Services (State);
      CB.Set_Selection (State.Time, State.Time_Model, 1);
      CB.Set_Selection (State.Level, State.Level_Model, 1);
      Rebuild (State);
   end Clear_Filters;

   --  A combo box's new selection takes effect.
   procedure Apply_Service (State : in out View_State) is
      Choice : constant Natural := CB.Selection (State.Service);
   begin
      if Choice <= 1 then
         State.Service_Name_Length := 0;
      else
         State.Service_Name := State.Captions (Choice - 1);
         State.Service_Name_Length := State.Caption_Length (Choice - 1);
      end if;
      Rebuild (State);
   end Apply_Service;

   procedure Dismiss_Popups (State : in out View_State) is
   begin
      CB.Dismiss (State.Service);
      CB.Dismiss (State.Time);
      CB.Dismiss (State.Level);
      CB.Dismiss (State.Keep);
   end Dismiss_Popups;

   procedure Handle
     (State : in out View_State; Item : Event; Map : in out Controls.Control_Map; Redraw : out Boolean)
   is
      Changed, Handled : Boolean := False;
      Rows : constant Positive := State.Rows;

      --  The keep box's choice goes to the platform, which asks logstore.
      procedure Request_Keep is
      begin
         State.Keep_Requested := True;
         State.Keep_Request := LR.Severity'Val (Natural'Max (1, CB.Selection (State.Keep)) - 1);
      end Request_Keep;
      --  Keys for the focused combo box.
      procedure Combo_Key (Combo : in out CB.Combo_State; Model : CB.Model; Key : CB.Key; Letter : Character := ' ') is
      begin
         CB.Handle_Key (Combo, Model, Key, Changed, Handled, Letter);
      end Combo_Key;
      procedure Focused_Combo_Key (Key : CB.Key; Letter : Character := ' ') is
      begin
         case State.Focus is
            when Service_Focus =>
               Combo_Key (State.Service, State.Service_Model, Key, Letter);
               if Changed then Apply_Service (State); end if;
            when Time_Focus =>
               Combo_Key (State.Time, State.Time_Model, Key, Letter);
               if Changed then Rebuild (State); end if;
            when Level_Focus =>
               Combo_Key (State.Level, State.Level_Model, Key, Letter);
               if Changed then Rebuild (State); end if;
            when Keep_Focus =>
               Combo_Key (State.Keep, State.Level_Model, Key, Letter);
               if Changed then Request_Keep; end if;
            when List_Focus | Search_Focus => null;
         end case;
      end Focused_Combo_Key;
      function Focused_Open return Boolean is
        (case State.Focus is
            when Service_Focus => CB.Is_Open (State.Service),
            when Time_Focus => CB.Is_Open (State.Time),
            when Level_Focus => CB.Is_Open (State.Level),
            when Keep_Focus => CB.Is_Open (State.Keep),
            when List_Focus | Search_Focus => False);

      procedure Edit_Search is
         Edited : Boolean := False;
      begin
         case Item.Key is
            when Left => Ed.Move (State.Search, (if Item.Control then Ed.Move_Word_Left else Ed.Move_Left), Item.Shift);
            when Right =>
               Ed.Move (State.Search, (if Item.Control then Ed.Move_Word_Right else Ed.Move_Right), Item.Shift);
            when Home => Ed.Move (State.Search, Ed.Move_Start, Item.Shift);
            when End_Key => Ed.Move (State.Search, Ed.Move_End, Item.Shift);
            when Backspace => Ed.Backspace (State.Search, Edited);
            when Delete => Ed.Delete_Forward (State.Search, Edited);
            when Enter | Down => State.Focus := List_Focus;
            when Escape =>
               if Query (State)'Length > 0 then
                  Ed.Select_All (State.Search);
                  Ed.Backspace (State.Search, Edited);
               else
                  State.Focus := List_Focus;
               end if;
            when others => Redraw := False;
         end case;
         if Edited then Rebuild (State); end if;
      end Edit_Search;

      --  Which combo box a control belongs to, if any.
      type Combo_Name is (No_Combo, Service_Combo, Time_Combo, Level_Combo, Keep_Combo);
      function Owner (Target : Controls.Control_ID) return Combo_Name is
        (if CB.Is_Combo_Control (SERVICE_BASE, Target) then Service_Combo
         elsif CB.Is_Combo_Control (TIME_BASE, Target) then Time_Combo
         elsif CB.Is_Combo_Control (LEVEL_BASE, Target) then Level_Combo
         elsif CB.Is_Combo_Control (KEEP_BASE, Target) then Keep_Combo
         else No_Combo);
      procedure Pointer_To (Combo : Combo_Name; Target : Controls.Control_ID) is
      begin
         case Combo is
            when Service_Combo =>
               CB.Handle_Pointer (State.Service, State.Service_Model, Map, SERVICE_BASE, Target, Item.Action,
                                  Changed, Handled);
               if Changed then Apply_Service (State); end if;
            when Time_Combo =>
               CB.Handle_Pointer (State.Time, State.Time_Model, Map, TIME_BASE, Target, Item.Action, Changed, Handled);
               if Changed then Rebuild (State); end if;
            when Level_Combo =>
               CB.Handle_Pointer (State.Level, State.Level_Model, Map, LEVEL_BASE, Target, Item.Action,
                                  Changed, Handled);
               if Changed then Rebuild (State); end if;
            when Keep_Combo =>
               CB.Handle_Pointer (State.Keep, State.Level_Model, Map, KEEP_BASE, Target, Item.Action,
                                  Changed, Handled, Enabled => State.Keep_Known);
               if Changed then Request_Keep; end if;
            when No_Combo => null;
         end case;
      end Pointer_To;
      function Open_Combo return Combo_Name is
        (if CB.Is_Open (State.Service) then Service_Combo
         elsif CB.Is_Open (State.Time) then Time_Combo
         elsif CB.Is_Open (State.Level) then Level_Combo
         elsif CB.Is_Open (State.Keep) then Keep_Combo else No_Combo);
   begin
      Redraw := True;
      case Item.Kind is
         when Resize => Settle (State);
         when Text_Event =>
            --  / reaches the search field from anywhere outside it.
            if Item.Character_Value = '/' and then State.Focus /= Search_Focus then
               Dismiss_Popups (State);
               State.Focus := Search_Focus;
               return;
            end if;
            case State.Focus is
               when Search_Focus =>
                  if Item.Character_Value in ' ' .. '~' then
                     Ed.Insert (State.Search, [1 => Item.Character_Value], Changed);
                     if Changed then Rebuild (State); end if;
                  end if;
               when Service_Focus | Time_Focus | Level_Focus | Keep_Focus =>
                  Focused_Combo_Key (CB.Type_Character, Item.Character_Value);
               when List_Focus =>
                  case Item.Character_Value is
                     when '1' .. '6' =>
                        CB.Set_Selection
                          (State.Level, State.Level_Model,
                           Character'Pos (Item.Character_Value) - Character'Pos ('1') + 1);
                        Rebuild (State);
                     when 's' =>
                        --  Only the selected record's service, or every service again.
                        if State.Service_Name_Length > 0 then
                           Choose_Service (State, "");
                        elsif State.Selected in 1 .. State.Visible_Count and then
                          Shown_Entry (State, State.Selected).Kind = Record_Entry
                        then
                           Choose_Service
                             (State, Name_Of (State, Shown_Entry (State, State.Selected).Source,
                                              Shown_Entry (State, State.Selected).Time_Ms));
                        end if;
                        Rebuild (State);
                     when 'f' =>
                        State.Follow := not State.Follow;
                        Settle (State);
                     when others => Redraw := False;
                  end case;
            end case;
         when Key_Event =>
            if Item.Key = Tab then
               Dismiss_Popups (State);
               State.Focus :=
                 (if Item.Shift then
                    (if State.Focus = Focus_Target'First then Focus_Target'Last else Focus_Target'Pred (State.Focus))
                  else
                    (if State.Focus = Focus_Target'Last then Focus_Target'First else Focus_Target'Succ (State.Focus)));
            else
               case State.Focus is
                  when Search_Focus => Edit_Search;
                  when Service_Focus | Time_Focus | Level_Focus | Keep_Focus =>
                     case Item.Key is
                        when Up => Focused_Combo_Key (CB.Up);
                        when Down => Focused_Combo_Key ((if Item.Control then CB.Toggle else CB.Down));
                        when Home => Focused_Combo_Key (CB.Home);
                        when End_Key => Focused_Combo_Key (CB.End_Key);
                        when Enter | Space =>
                           Focused_Combo_Key ((if Focused_Open then CB.Commit else CB.Toggle));
                        when Escape =>
                           if Focused_Open then Focused_Combo_Key (CB.Cancel); else State.Focus := List_Focus; end if;
                        when others => Redraw := False;
                     end case;
                  when List_Focus =>
                     case Item.Key is
                        when Up => Move (State, -1);
                        when Down => Move (State, 1);
                        when Page_Up => Move (State, -Rows);
                        when Page_Down => Move (State, Rows);
                        when Home => Move (State, -Integer (State.Visible_Count));
                        when End_Key =>
                           State.Follow := True;
                           Settle (State);
                        when Escape => Clear_Filters (State);
                        when others => Redraw := False;
                     end case;
               end case;
            end if;
         when Wheel =>
            if Open_Combo /= No_Combo then
               case Open_Combo is
                  when Service_Combo => CB.Handle_Wheel (State.Service, State.Service_Model, Item.Steps, Handled);
                  when Time_Combo => CB.Handle_Wheel (State.Time, State.Time_Model, Item.Steps, Handled);
                  when Level_Combo => CB.Handle_Wheel (State.Level, State.Level_Model, Item.Steps, Handled);
                  when Keep_Combo => CB.Handle_Wheel (State.Keep, State.Level_Model, Item.Steps, Handled);
                  when No_Combo => null;
               end case;
            elsif State.Visible_Count > Rows then
               State.Follow := False;
               State.Top := Natural (Integer'Max (0, Integer'Min
                 (Integer (State.Top) - WHEEL_ROWS * Item.Steps, Integer (State.Visible_Count - Rows))));
            end if;
         when Pointer_Event =>
            declare
               Target : constant Controls.Control_ID := Controls.Hit (Map, Item.X, Item.Y);
               Opened : constant Combo_Name := Open_Combo;
            begin
               --  An open popup takes the press outside it, and closes.
               if Item.Action = Controls.Pointer_Press and then Opened /= No_Combo and then Owner (Target) /= Opened then
                  Pointer_To (Opened, Target);
                  return;
               end if;
               if Owner (Target) /= No_Combo then
                  Pointer_To (Owner (Target), Target);
                  if Item.Action = Controls.Pointer_Press then
                     State.Focus :=
                       (case Owner (Target) is
                           when Service_Combo => Service_Focus, when Time_Combo => Time_Focus,
                           when Level_Combo => Level_Focus, when Keep_Combo => Keep_Focus,
                           when No_Combo => List_Focus);
                  end if;
                  return;
               elsif Opened /= No_Combo then
                  Pointer_To (Opened, Target);
               end if;
               if Item.Action = Controls.Pointer_Press then
                  if Target = SEARCH_ID then
                     State.Focus := Search_Focus;
                     Ed.Move (State.Search, Ed.Move_End);
                  elsif State.Focus = Search_Focus then
                     State.Focus := List_Focus;
                  end if;
               elsif Item.Action = Controls.Pointer_Release then
                  Tables.Handle_Header_Release (State.Columns, Map, COLUMNS_BASE, Target, Changed);
                  if Changed then
                     Rebuild (State);
                  elsif Target = FOLLOW_ID and then Controls.Take_Activated (Map, Target) then
                     State.Follow := not State.Follow;
                     Settle (State);
                  elsif Target = CLEAR_ID and then Controls.Take_Activated (Map, Target) then
                     Clear_Filters (State);
                  elsif Target >= ROW_FIRST and then Controls.Take_Activated (Map, Target) then
                     if State.Top + (Target - ROW_FIRST) + 1 <= State.Visible_Count then
                        State.Follow := False;
                        State.Focus := List_Focus;
                        State.Selected := State.Top + (Target - ROW_FIRST) + 1;
                        Settle (State);
                     end if;
                  else
                     Redraw := Item.Action /= Controls.Pointer_Move;
                  end if;
               else
                  Redraw := Opened /= No_Combo;
               end if;
            end;
      end case;
   end Handle;

   function Column_Title (Column : Tables.Column_Index) return String is
     (case Name_Of_Column (Column) is
         when Time_Column => "Time", when Node_Column => "Node", when Level_Column => "Level",
         when Source_Column => "Source", when Message_Column => "Message");
   procedure Header is new Tables.Columns_Header (Column_Title);

   procedure Render
     (State : in out View_State; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect;
      UI : in out CuBit.UI.State.UI_State; Map : in out Controls.Control_Map)
   is
      Colors : constant Theme := Current_Theme;
      Toolbar : constant Rect := (Bounds.x + MARGIN, Bounds.y + MARGIN, Bounds.w - 2 * MARGIN, TOOLBAR_HEIGHT);
      Control_Y : constant Natural := Toolbar.y + (TOOLBAR_HEIGHT - CONTROL_HEIGHT) / 2;
      KEEP_LABEL : constant String := "logstore keeps";
      Keep_Room : constant Natural := UI_Text_Width (KEEP_LABEL) + GAP + KEEP_WIDTH;
      Keep_Box : constant Rect :=
        (Bounds.x + Bounds.w - MARGIN - KEEP_WIDTH, Bounds.y + Bounds.h - MARGIN - STATUS_HEIGHT,
         KEEP_WIDTH, CONTROL_HEIGHT);
      Status : constant Rect :=
        (Bounds.x + MARGIN, Bounds.y + Bounds.h - MARGIN - STATUS_HEIGHT,
         (if Bounds.w > 2 * MARGIN + Keep_Room + GAP then Bounds.w - 2 * MARGIN - Keep_Room - GAP else 0),
         STATUS_HEIGHT);
      Detail : constant Rect :=
        (Bounds.x + MARGIN, Status.y - GAP - DETAIL_HEIGHT, Bounds.w - 2 * MARGIN, DETAIL_HEIGHT);
      Table_Top : constant Natural := Toolbar.y + TOOLBAR_HEIGHT + GAP;
      Table : constant Rect :=
        (Bounds.x + MARGIN, Table_Top, Bounds.w - 2 * MARGIN,
         (if Detail.y > Table_Top + GAP then Detail.y - GAP - Table_Top else 0));
      Regions : constant Table_Regions := Layout_Table (Table);
      Right_Edge : constant Natural := Toolbar.x + Toolbar.w - 7;
      Follow_Box : constant Rect := (Right_Edge - FOLLOW_WIDTH, Control_Y, FOLLOW_WIDTH, CONTROL_HEIGHT);
      Clear_Box : constant Rect := (Follow_Box.x - GAP - CLEAR_WIDTH, Control_Y, CLEAR_WIDTH, CONTROL_HEIGHT);
      Combos_Width : constant Natural := SERVICE_WIDTH + TIME_WIDTH + LEVEL_WIDTH + 3 * GAP;
      Search_Width : constant Natural :=
        (if Clear_Box.x > Toolbar.x + 7 + Combos_Width + GAP + SEARCH_MINIMUM
         then Clear_Box.x - Toolbar.x - 7 - Combos_Width - GAP else SEARCH_MINIMUM);
      Search_Box : constant Rect := (Toolbar.x + 7, Control_Y, Search_Width, CONTROL_HEIGHT);
      Service_Box : constant Rect := (Search_Box.x + Search_Width + GAP, Control_Y, SERVICE_WIDTH, CONTROL_HEIGHT);
      Time_Box : constant Rect := (Service_Box.x + SERVICE_WIDTH + GAP, Control_Y, TIME_WIDTH, CONTROL_HEIGHT);
      Level_Box : constant Rect := (Time_Box.x + TIME_WIDTH + GAP, Control_Y, LEVEL_WIDTH, CONTROL_HEIGHT);
      Rows : constant Positive := Positive'Max (1, Regions.Rows.h / ROW_HEIGHT);
      Scrolls : constant Boolean := State.Visible_Count > Rows;
      Row_Width : constant Natural :=
        (if Scrolls and then Regions.Rows.w > SCROLLBAR_WIDTH + 1 then Regions.Rows.w - SCROLLBAR_WIDTH - 1
         else Regions.Rows.w);
      Widget : Widget_Result;
      Previous_Top : Natural;
      function Pointer_In (Area : Rect) return Boolean is
        (UI.pointer.enabled and then Point_In_Rect (UI.pointer.x, UI.pointer.y, Area));
   begin
      CuBit.UI.State.Begin_Frame (UI);
      Controls.Clear (Map);
      --  Each pixel once: the sections paint themselves; only the margins
      --  and gaps between them get the window background.
      Fill_Rect (C, (Bounds.x, Bounds.y, Bounds.w, MARGIN), Colors.face);
      Fill_Rect (C, (Bounds.x, Bounds.y, MARGIN, Bounds.h), Colors.face);
      if Bounds.w > MARGIN then
         Fill_Rect (C, (Bounds.x + Bounds.w - MARGIN, Bounds.y, MARGIN, Bounds.h), Colors.face);
      end if;
      Fill_Rect (C, (Bounds.x, Toolbar.y + TOOLBAR_HEIGHT, Bounds.w, GAP), Colors.face);
      Fill_Rect (C, (Bounds.x, Table.y + Table.h, Bounds.w,
                     (if Detail.y > Table.y + Table.h then Detail.y - Table.y - Table.h else 0)), Colors.face);
      Fill_Rect (C, (Bounds.x, Detail.y + Detail.h, Bounds.w,
                     (if Status.y > Detail.y + Detail.h then Status.y - Detail.y - Detail.h else 0)), Colors.face);
      Fill_Rect (C, (Status.x + Status.w, Status.y,
                     (if Keep_Box.x > Status.x + Status.w then Keep_Box.x - Status.x - Status.w else 0),
                     STATUS_HEIGHT), Colors.face);
      Fill_Rect (C, (Bounds.x, Status.y + STATUS_HEIGHT, Bounds.w,
                     (if Bounds.y + Bounds.h > Status.y + STATUS_HEIGHT
                      then Bounds.y + Bounds.h - Status.y - STATUS_HEIGHT else 0)), Colors.face);
      State.Rows := Rows;
      Settle (State);

      --  Toolbar: the search field, the combo boxes (drawn last, so their
      --  popups cover the table), Clear and Follow.
      CuBit.UI.Widgets.Toolbar (C, Toolbar, Colors);
      Controls.Add (Map, SEARCH_ID, Search_Box, Toolbar, Pointer_Text);
      Draw_Text_Edit_Field
        (C, Search_Box, Colors, Query (State), Ed.Cursor (State.Search) - 1,
         Ed.Selection_First (State.Search) - 1, Ed.Selection_Last (State.Search) - 1,
         State.Focus = Search_Focus, Pointer_In (Search_Box));
      if Query (State)'Length = 0 and then State.Focus /= Search_Focus then
         Draw_UI_Text_Transparent
           (With_Clip (C, Search_Box), Search_Box.x + PLACEHOLDER_INSET,
            Search_Box.y + (if CONTROL_HEIGHT > UI_Text_Height then (CONTROL_HEIGHT - UI_Text_Height) / 2 else 0),
            "Search messages and sources", Colors.muted);
      end if;
      if Filtered (State) then
         CuBit.UI.Widgets.Button (C, UI, Map, CLEAR_ID, Clear_Box, Toolbar, Colors, "Clear", Widget,
                                  retainedInput => True);
      else
         CuBit.UI.Widgets.Disabled_Button (C, Clear_Box, Colors, "Clear");
      end if;
      CuBit.UI.Widgets.Button (C, UI, Map, FOLLOW_ID, Follow_Box, Toolbar, Colors,
                               (if State.Follow then "Pause" else "Follow"), Widget, retainedInput => True);

      --  The table: header, scrollbar, rows.
      --  The frame now; the interior is the header, the rows and whatever
      --  lies below the last row (filled after the rows).
      Draw_Table_Viewport_Frame (C, Table, Colors);
      Header (C, UI, Map, COLUMNS_BASE, Regions.Header, Table, Colors, State.Columns);
      if Scrolls then
         Previous_Top := State.Top;
         CuBit.UI.Widgets.Vertical_Scrollbar
           (C, UI, Map, SCROLL_ID,
            (Regions.Rows.x + Regions.Rows.w - SCROLLBAR_WIDTH, Regions.Rows.y, SCROLLBAR_WIDTH, Regions.Rows.h),
            Table, Colors, 0, State.Visible_Count - 1, State.Top, Widget,
            pageSize => Rows, retainedInput => True);
         State.Top := Natural'Min (State.Top, State.Visible_Count - Rows);
         if State.Top /= Previous_Top then
            State.Follow := False;
         end if;
      end if;
      for Row in 0 .. Rows - 1 loop
         exit when State.Top + Row + 1 > State.Visible_Count;
         declare
            Index : constant Positive := State.Top + Row + 1;
            E : constant Log_Entry := Shown_Entry (State, Index);
            Row_Box : constant Rect := (Regions.Rows.x, Regions.Rows.y + Row * ROW_HEIGHT, Row_Width, ROW_HEIGHT);
            Selected : constant Boolean := Index = State.Selected;
            function Cell (Column : Tables.Column_Index) return String is
              (if E.Kind = Gap_Entry then
                 (case Name_Of_Column (Column) is
                     when Time_Column => Uptime (E.Time_Ms), when Message_Column => Gap_Text (E),
                     when Node_Column | Level_Column | Source_Column => "")
               else
                 (case Name_Of_Column (Column) is
                     when Time_Column => Uptime (E.Time_Ms),
                     when Node_Column => Node_Text (E.Node),
                     when Level_Column => Level_Label (LR.Level (E.Item)),
                     when Source_Column => Name_Of (State, E.Source, E.Time_Ms),
                     when Message_Column => LR.Text (E.Item)));
            function Ink (Column : Tables.Column_Index; Default : Color) return Color is
              (if Selected then Default
               elsif E.Kind = Gap_Entry then Level_Color (LR.Error, Colors.field)
               else
                 (case Name_Of_Column (Column) is
                     when Time_Column | Node_Column => Time_Color (Colors.field),
                     when Level_Column => Level_Color (LR.Level (E.Item), Colors.field),
                     when Source_Column => Source_Color (Colors.field),
                     when Message_Column => Default));
            procedure Draw_Row is new Tables.Draw_Columns_Row (Cell, Ink);
         begin
            Controls.Add_Button (Map, ROW_FIRST + Row, Row_Box, Regions.Rows);
            Draw_Row (C, Row_Box, Colors, State.Columns, Selected, Pointer_In (Row_Box), Table_Code_Text);
         end;
      end loop;
      declare
         Drawn_Rows : constant Natural :=
           Natural'Min (Rows, (if State.Visible_Count > State.Top then State.Visible_Count - State.Top else 0));
         Below : constant Natural := Regions.Rows.y + Drawn_Rows * ROW_HEIGHT;
      begin
         if Below < Regions.Rows.y + Regions.Rows.h then
            Fill_Rect (C, (Regions.Rows.x, Below, Row_Width, Regions.Rows.y + Regions.Rows.h - Below), Colors.field);
         end if;
      end;
      if State.Link = Denied then
         Draw_UI_Text (With_Clip (C, Regions.Rows), Regions.Rows.x + MARGIN, Regions.Rows.y + MARGIN,
           "Reading logs is not granted to this program. To allow it, its manifest must request " &
           "(request-service log-observer read-write log-observer).", Colors.danger, Colors.field);
      elsif State.Visible_Count = 0 then
         Draw_UI_Text (With_Clip (C, Regions.Rows), Regions.Rows.x + MARGIN, Regions.Rows.y + MARGIN,
           (if State.Count = 0 then "Waiting for records from logstore..."
            else "No records match the filters. Clear shows everything."), Colors.muted, Colors.field);
      end if;

      --  The selected record in full: when, how severe, from whom, its whole
      --  message (wrapped) and its fields.
      Draw_Pane (C, Detail, Colors, "Selected record");
      declare
         --  Inside the pane's frame, below its title.
         Inner : constant Rect :=
           (Detail.x + 2 * GAP, Detail.y + UI_Text_Height + GAP,
            (if Detail.w > 4 * GAP then Detail.w - 4 * GAP else 0),
            (if Detail.h > UI_Text_Height + 2 * GAP then Detail.h - UI_Text_Height - 2 * GAP else 0));
         CW : constant Positive := Positive'Max (1, Code_Text_Width ("0"));
         Columns : constant Positive := Positive'Max (1, Inner.w / CW);
         Text_C : constant Canvas := With_Clip (C, Inner);
         Line : Natural := 0;
      begin
         if State.Selected in 1 .. State.Visible_Count then
            declare
               E : constant Log_Entry := Shown_Entry (State, State.Selected);
            begin
               if E.Kind = Gap_Entry then
                  Draw_UI_Text (Text_C, Inner.x, Inner.y,
                    Image64 (E.Lost) & " records were lost: this viewer fell behind logstore's queue for it.",
                    Level_Color (LR.Error, Colors.panel), Colors.panel);
               else
                  Draw_UI_Text (Text_C, Inner.x, Inner.y,
                    Uptime (E.Time_Ms) & "   " & Level_Label (LR.Level (E.Item)) & "   " &
                    Name_Of (State, E.Source, E.Time_Ms) & "   (pid " & Image64 (E.Source) & ", node " &
                    Node_Text (E.Node, Short => False) & ")",
                    Level_Color (LR.Level (E.Item), Colors.panel), Colors.panel);
                  declare
                     Text : constant String := LR.Text (E.Item);
                     First : Natural := Text'First;
                  begin
                     while First <= Text'Last and then Line < 3 loop
                        Draw_Code_Text (Text_C, Inner.x, Inner.y + 22 + Line * ROW_HEIGHT,
                          Text (First .. Natural'Min (Text'Last, First + Columns - 1)), Colors.text, Colors.panel);
                        First := First + Columns;
                        Line := Line + 1;
                     end loop;
                  end;
                  declare
                     Fields : String (1 .. 256) := [others => ' '];
                     Length : Natural := 0;
                     procedure Put (S : String) is
                     begin
                        if Length + S'Length <= Fields'Length then
                           Fields (Length + 1 .. Length + S'Length) := S;
                           Length := Length + S'Length;
                        end if;
                     end Put;
                  begin
                     for I in 1 .. LR.Field_Total (E.Item) loop
                        declare
                           F : constant LR.Field := LR.Field_At (E.Item, I);
                        begin
                           Put ((if Length = 0 then "" else "  ") & LR.Name (F) & "=");
                           case LR.Kind (F) is
                              when LR.Signed_Integer =>
                                 declare
                                    V : constant String := Integer_64'Image (LR.Signed_Value (F));
                                 begin
                                    Put ((if V (V'First) = ' ' then V (V'First + 1 .. V'Last) else V));
                                 end;
                              when LR.Unsigned_Integer => Put (Image64 (LR.Value (F)));
                              when LR.Duration_Microseconds => Put (Image64 (LR.Value (F)) & "us");
                              when LR.Truth => Put ((if LR.Value (F) /= 0 then "true" else "false"));
                           end case;
                        end;
                     end loop;
                     if Length > 0 then
                        Draw_Code_Text (Text_C, Inner.x, Inner.y + 22 + Line * ROW_HEIGHT, Fields (1 .. Length),
                                        Source_Color (Colors.panel), Colors.panel);
                     end if;
                  end;
               end if;
            end;
         else
            Draw_UI_Text (Text_C, Inner.x, Inner.y, "Select a record to see it in full.", Colors.muted, Colors.panel);
         end if;
      end;

      --  Status: the connection, how many are shown, what was lost.
      Draw_Status_Bar
        (C, Status, Colors,
         (case State.Link is
             when Connecting => "Connecting to logstore",
             when Connected => (if State.Follow then "Live" else "Paused"),
             when Denied => "Not granted",
             when Unavailable => "Logstore unavailable") &
         "  |  " & Image (State.Visible_Count) & " shown of " & Image (State.Count) &
         (if State.Lost > 0 then "  |  " & Image64 (State.Lost) & " lost" else "") &
         (if not State.Follow and then State.New_Since_Pause > 0
          then "  |  " & Image (State.New_Since_Pause) & " new" else "") &
         (if State.Keep_Note_Length > 0 then "  |  " & State.Keep_Note (1 .. State.Keep_Note_Length) else ""),
         (if State.Keep_Note_Length > 0 then "" else "/ search  Tab next"));
      Draw_UI_Text
        (C, Keep_Box.x - GAP - UI_Text_Width (KEEP_LABEL),
         Keep_Box.y + (if CONTROL_HEIGHT > UI_Text_Height then (CONTROL_HEIGHT - UI_Text_Height) / 2 else 0),
         KEEP_LABEL, Colors.muted, Colors.face);

      --  The combo boxes last, the open one on top.
      declare
         procedure Draw_Service is
         begin
            CB.Draw (C, Map, State.Service, State.Service_Model, SERVICE_BASE, Service_Box, Colors,
                     Focused => State.Focus = Service_Focus);
         end Draw_Service;
         procedure Draw_Time is
         begin
            CB.Draw (C, Map, State.Time, State.Time_Model, TIME_BASE, Time_Box, Colors,
                     Focused => State.Focus = Time_Focus);
         end Draw_Time;
         procedure Draw_Level is
         begin
            CB.Draw (C, Map, State.Level, State.Level_Model, LEVEL_BASE, Level_Box, Colors,
                     Focused => State.Focus = Level_Focus);
         end Draw_Level;
         procedure Draw_Keep is
         begin
            CB.Draw (C, Map, State.Keep, State.Level_Model, KEEP_BASE, Keep_Box, Colors,
                     Focused => State.Focus = Keep_Focus, Enabled => State.Keep_Known);
         end Draw_Keep;
      begin
         if CB.Is_Open (State.Service) then
            Draw_Time; Draw_Level; Draw_Keep; Draw_Service;
         elsif CB.Is_Open (State.Time) then
            Draw_Service; Draw_Level; Draw_Keep; Draw_Time;
         elsif CB.Is_Open (State.Keep) then
            Draw_Service; Draw_Time; Draw_Level; Draw_Keep;
         else
            Draw_Service; Draw_Time; Draw_Keep; Draw_Level;
         end if;
      end;
      CuBit.UI.State.Finish_Frame (UI);
   end Render;

   function Total (State : View_State) return Natural is (State.Count);
   function Shown (State : View_State) return Natural is (State.Visible_Count);
   function Following (State : View_State) return Boolean is (State.Follow);
   function Minimum (State : View_State) return CuBit.Log_Records.Severity is (Floor (State));
   function Search_Text (State : View_State) return String is (Query (State));
   function Service_Filter (State : View_State) return String is
     (State.Service_Name (1 .. State.Service_Name_Length));
   function Sorted_By (State : View_State) return Column_Name is
     (Name_Of_Column (Tables.Column_Index'Max (1, State.Columns.Sort_Column)));
   function Descending (State : View_State) return Boolean is (State.Columns.Order = Tables.Descending);
   function Row_Text (State : View_State; Row : Positive) return String is
     (if Row <= State.Visible_Count then Message_Of (Shown_Entry (State, Row)) else "");
   function Selected_Text (State : View_State) return String is
     (if State.Selected in 1 .. State.Visible_Count
         and then Shown_Entry (State, State.Selected).Kind = Record_Entry
      then LR.Text (Shown_Entry (State, State.Selected).Item) else "");
   function Unseen (State : View_State) return Natural is (State.New_Since_Pause);
   function Revision (State : View_State) return Unsigned_64 is (State.Revision);
   function Content_Area (Bounds : CuBit.UI.Rect) return CuBit.UI.Rect is
     (if Bounds.h > MARGIN + TOOLBAR_HEIGHT
      then (Bounds.x, Bounds.y + MARGIN + TOOLBAR_HEIGHT, Bounds.w, Bounds.h - MARGIN - TOOLBAR_HEIGHT)
      else Bounds);
   function Kept_Known (State : View_State) return Boolean is (State.Keep_Known);
   function Kept (State : View_State) return CuBit.Log_Records.Severity is
     (LR.Severity'Val (Natural'Max (1, CB.Selection (State.Keep)) - 1));
end Log_View;
