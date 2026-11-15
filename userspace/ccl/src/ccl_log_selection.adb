with CCL.Language;

package body CCL_Log_Selection is
   use type Logs.Gap_Reason;
   use type Logs.Log_Window;

   --  A result is shown whole only while its literal fits
   --  (CCL.Language.MAX_LITERAL_BYTES): the most one gap's and one tally's
   --  literal take, every count at its 20 digits and the message with them.
   GAP_LITERAL_BOUND : constant := 192;
   TALLY_LITERAL_BOUND : constant := 384;
   --  An item's literal around its numbers and texts:
   --  (LogItem.Logged (LogEntry  Severity.  "")) and spaces, and
   --  (LogItem.Gap (LogGap    GapReason. "")) and spaces.
   ENTRY_LITERAL : constant := 43;
   GAP_LITERAL : constant := 40;
   --  Gaps a shown instance may need where the middle was left out (lost,
   --  not retained, not shown), and the older instances' gaps (not
   --  retained, not shown); the list's brackets.
   GAPS_PER_INSTANCE : constant := 3;
   OLDER_GAPS : constant := 2;
   LIST_BRACKETS : constant := 2;
   pragma Compile_Time_Error
     (CCL.Language.MAX_LITERAL_BYTES > CCL.Objects.Maximum_Text_Bytes,
      "a result's text must fit wherever its literal does");

   function Sum (A, B : Unsigned_64) return Unsigned_64 is
     (if A > Unsigned_64'Last - B then Unsigned_64'Last else A + B);

   function Shown (Item : Selection) return Boolean is
     (Item.Instances in 1 .. MAXIMUM_INSTANCES);

   procedure Start (Item : out Selection; Within : Logs.Log_Window) is
   begin
      Item.Within := Within;
      Item.Used := 0;
      Item.Order := [others => 1];
      Item.Instances := 0;
      Item.Shown := 0;
      Item.Kept := [others => <>];
      Item.Older_Records := 0;
      Item.Older_Not_Retained := 0;
      Item.Base := 0;
      Item.Room := 0;
      Item.Head_Room := 0;
      Item.Head_Held := 0;
      Item.Ring_Held := 0;
      Item.Ring_Next := 1;
      Item.Source := 0;
      Item.Last := 0;
   end Start;

   procedure Begin_Instance (Item : in out Selection; Source : Unsigned_64) is
   begin
      Item.Instances := Item.Instances + 1;
      Item.Source := Source;
      Item.Last := 0;
      Item.Base := Item.Used;
      Item.Room := Logs.MAX_ITEMS - Item.Used;
      Item.Head_Room := Item.Room / HEAD_SHARE;
      Item.Head_Held := 0;
      Item.Ring_Next := 1;
      Item.Ring_Held := 0;
      if Shown (Item) then
         Item.Shown := Item.Instances;
         Item.Kept (Item.Instances) := (Source => Source, others => <>);
         Item.Kept (Item.Instances).Tally := (others => 0);
      end if;
   end Begin_Instance;

   --  An item the ring gives up (or the fit leaves out), in its account.
   procedure Leave_Out (Item : in out Selection; Left : Pending) is
      A : Account renames Item.Kept (Left.Instance);
   begin
      case Left.Kind is
         when Record_Item => A.Not_Shown := Sum (A.Not_Shown, 1);
         when Gap_Item =>
            if Left.Reason = Logs.Lost then
               A.Lost := Sum (A.Lost, Left.Count);
            else
               A.Not_Retained := Sum (A.Not_Retained, Left.Count);
            end if;
      end case;
   end Leave_Out;

   procedure Push (Item : in out Selection; New_Item : Pending) is
      Ring_Size : constant Item_Count := Item.Room - Item.Head_Room;
      Slot : Item_Index;
   begin
      if Item.Head_Held < Item.Head_Room then
         Item.Head_Held := Item.Head_Held + 1;
         Item.Items (Item.Base + Item.Head_Held) := New_Item;
         Item.Items (Item.Base + Item.Head_Held).Head := True;
         return;
      end if;
      Slot := Item.Base + Item.Head_Room + Item.Ring_Next;
      if Item.Ring_Held = Ring_Size then
         Leave_Out (Item, Item.Items (Slot));
      else
         Item.Ring_Held := Item.Ring_Held + 1;
      end if;
      Item.Items (Slot) := New_Item;
      Item.Ring_Next := (if Item.Ring_Next = Ring_Size then 1 else Item.Ring_Next + 1);
   end Push;

   procedure Omitted (Item : in out Selection; Not_Retained, Lost, Next : Logs.Boot_Milliseconds) is
      --  The gap lies between the record before it and the next.
      Touches : constant Boolean := Logs.Overlaps (Item.Within, Item.Last, Next);
      I : constant Natural := Item.Instances;
   begin
      if not Touches or else I = 0 then
         return;
      elsif not Shown (Item) then
         --  An older instance: its whole history comes from its tally,
         --  a window's from what the window touches.
         if Item.Within /= Logs.WHOLE_HISTORY then
            Item.Older_Not_Retained := Sum (Item.Older_Not_Retained, Not_Retained);
         end if;
         return;
      end if;
      if Not_Retained /= 0 and then Item.Room > 0 then
         Push (Item, (Kind => Gap_Item, Count => Not_Retained, Reason => Logs.Not_Retained,
                      Source => Item.Source, Time => Item.Last, Instance => I, others => <>));
      elsif Not_Retained /= 0 then
         Item.Kept (I).Not_Retained := Sum (Item.Kept (I).Not_Retained, Not_Retained);
      end if;
      if Lost /= 0 and then Item.Room > 0 then
         Push (Item, (Kind => Gap_Item, Count => Lost, Reason => Logs.Lost, Source => Item.Source,
                      Time => Item.Last, Instance => I, others => <>));
      elsif Lost /= 0 then
         Item.Kept (I).Lost := Sum (Item.Kept (I).Lost, Lost);
      end if;
   end Omitted;

   procedure Logged (Item : in out Selection; Time : Logs.Boot_Milliseconds; Value : Log_Record) is
      I : constant Natural := Item.Instances;
   begin
      Item.Last := Time;
      if I = 0 or else not Logs.Holds (Item.Within, Time) then
         return;
      elsif not Shown (Item) then
         if Item.Within /= Logs.WHOLE_HISTORY then
            Item.Older_Records := Sum (Item.Older_Records, 1);
         end if;
      elsif Item.Room > 0 then
         Push (Item, (Kind => Record_Item, Value => Value, Source => Item.Source, Time => Time,
                      Instance => I, others => <>));
      else
         Item.Kept (I).Not_Shown := Sum (Item.Kept (I).Not_Shown, 1);
      end if;
   end Logged;

   procedure Tallied (Item : in out Selection; Tally : Tally_Counts) is
   begin
      if Item.Instances = 0 then
         return;
      elsif Shown (Item) then
         Item.Kept (Item.Instances).Tally := Tally;
      elsif Item.Within = Logs.WHOLE_HISTORY then
         Item.Older_Records := Sum (Item.Older_Records, Tally.Retained);
         Item.Older_Not_Retained := Sum (Item.Older_Not_Retained, Tally.Not_Retained);
      end if;
   end Tallied;

   --  The ring of the instance just read, oldest first, in place after its
   --  head; then the instance's items are the result's.
   procedure End_Instance (Item : in out Selection) is
      Ring_Size : constant Item_Count := Item.Room - Item.Head_Room;
      First : constant Item_Count := Item.Base + Item.Head_Room;
   begin
      if Item.Ring_Held = Ring_Size and then Item.Ring_Next /= 1 then
         declare
            Ring : constant array (1 .. Ring_Size) of Pending :=
              [for K in 1 .. Ring_Size => Item.Items (First + K)];
         begin
            for K in 1 .. Ring_Size loop
               Item.Items (First + K) := Ring ((Item.Ring_Next + K - 2) mod Ring_Size + 1);
            end loop;
         end;
      end if;
      --  The ring follows the head with no hole.
      if Item.Head_Held < Item.Head_Room then
         for K in 1 .. Item.Ring_Held loop
            Item.Items (Item.Base + Item.Head_Held + K) := Item.Items (First + K);
         end loop;
      end if;
      Item.Used := Item.Used + Item.Head_Held + Item.Ring_Held;
   end End_Instance;

   procedure Build (Item : in out Selection; Contract : CCL.Objects.Binding; Image : out CCL.Objects.Image) is
      Used : constant Item_Count := Item.Used;
      Added : Boolean;

      procedure Add_Item (Placed : Pending) is
      begin
         case Placed.Kind is
            when Record_Item =>
               Logs.Add (Image, Placed.Time, Level (Placed.Value), Placed.Source, Text (Placed.Value), Added);
            when Gap_Item =>
               Logs.Add_Gap (Image, Placed.Time, Placed.Source, Placed.Count, Placed.Reason, Added);
         end case;
      end Add_Item;

      function Cells_Of (Placed : Pending) return Natural is
        (if Placed.Kind = Record_Item then Logs.ENTRY_CELLS else Logs.GAP_CELLS);
      --  The length of an item's literal in the result, and the space after
      --  it: "(LogItem.Logged (LogEntry 12 Severity.Information 7 "text"))"
      --  with the text's escapes, or a gap's.
      function Digits_Of (Value : Unsigned_64) return Natural is (Unsigned_64'Image (Value)'Length - 1);
      function Escaped (Message : String) return Natural is
        (Message'Length +
         [for C of Message => (if C in '"' | '\' | ASCII.LF | ASCII.CR | ASCII.HT then 1 else 0)]'Reduce ("+", 0));
      function Text_Of (Placed : Pending) return Natural is
        (if Placed.Kind = Record_Item
         then ENTRY_LITERAL + Digits_Of (Placed.Time) + Digits_Of (Placed.Source) +
              Logs.Severity'Image (Level (Placed.Value))'Length + Escaped (Text (Placed.Value))
         else GAP_LITERAL + Digits_Of (Placed.Time) + Digits_Of (Placed.Source) + Digits_Of (Placed.Count) +
              Logs.Gap_Reason'Image (Placed.Reason)'Length +
              Logs.Gap_Message (Placed.Count, Placed.Reason)'Length);

      Order : Item_Order renames Item.Order;
   begin
      Logs.Start (Contract, Image);
      --  Oldest first across instances (stable: a gap stays before its
      --  record, and equal times keep their order).
      for I in 1 .. Used loop
         Order (I) := I;
         for J in reverse 1 .. I - 1 loop
            exit when Item.Items (Order (J)).Time <= Item.Items (Order (J + 1)).Time;
            declare
               Swap : constant Item_Index := Order (J);
            begin
               Order (J) := Order (J + 1);
               Order (J + 1) := Swap;
            end;
         end loop;
      end loop;
      --  Fit one result: after what each shown instance needs (its gaps,
      --  and its tally) and the older instances' gaps, the first items (a
      --  HEAD_SHARE of the room) and then the newest; what lies between is
      --  left out, counted, and shown as gaps where it was.
      declare
         Reserved : constant Natural :=
           1 + Item.Shown * (GAPS_PER_INSTANCE * Logs.GAP_CELLS + Logs.TALLY_CELLS) +
           OLDER_GAPS * Logs.GAP_CELLS;
         --  The literal the gaps and tallies take, at most, and the list's
         --  brackets. Its text is part of its literal, and the literal's
         --  bound is below the image's text bound, so fitting the literal
         --  fits the text too.
         Reserved_Literal : constant Natural :=
           LIST_BRACKETS + Item.Shown * (GAPS_PER_INSTANCE * GAP_LITERAL_BOUND + TALLY_LITERAL_BOUND) +
           OLDER_GAPS * GAP_LITERAL_BOUND;
         Cell_Room : Natural := CCL.Objects.Maximum_Cells - Natural'Min (Reserved, CCL.Objects.Maximum_Cells);
         Text_Room : Natural :=
           CCL.Language.MAX_LITERAL_BYTES - Natural'Min (Reserved_Literal, CCL.Language.MAX_LITERAL_BYTES);
         Head_Cells : Natural := Cell_Room / HEAD_SHARE;
         Head_Text : Natural := Text_Room / HEAD_SHARE;
         Front, Back : Item_Count := 0;
         First_Time : constant Logs.Boot_Milliseconds := (if Used > 0 then Item.Items (Order (1)).Time else 0);
         Cut_Time : Logs.Boot_Milliseconds := 0;
      begin
         --  The first items, while they are their instance's first.
         while Front < Used loop
            declare
               Placed : Pending renames Item.Items (Order (Front + 1));
            begin
               exit when not Placed.Head or else Cells_Of (Placed) > Head_Cells
                 or else Text_Of (Placed) > Head_Text;
               Head_Cells := Head_Cells - Cells_Of (Placed);
               Head_Text := Head_Text - Text_Of (Placed);
               Cell_Room := Cell_Room - Cells_Of (Placed);
               Text_Room := Text_Room - Text_Of (Placed);
               Front := Front + 1;
            end;
         end loop;
         --  Then the newest.
         while Front + Back < Used loop
            declare
               Placed : Pending renames Item.Items (Order (Used - Back));
            begin
               exit when Cells_Of (Placed) > Cell_Room or else Text_Of (Placed) > Text_Room;
               Cell_Room := Cell_Room - Cells_Of (Placed);
               Text_Room := Text_Room - Text_Of (Placed);
               Back := Back + 1;
            end;
         end loop;
         for Position in Front + 1 .. Used - Back loop
            Leave_Out (Item, Item.Items (Order (Position)));
         end loop;
         --  The older instances' records come before everything.
         if Item.Older_Not_Retained > 0 then
            Logs.Add_Gap (Image, First_Time, 0, Item.Older_Not_Retained, Logs.Not_Retained, Added);
         end if;
         if Item.Older_Records > 0 then
            Logs.Add_Gap (Image, First_Time, 0, Item.Older_Records, Logs.Not_Shown, Added);
         end if;
         for Position in 1 .. Front loop
            Add_Item (Item.Items (Order (Position)));
         end loop;
         --  The gaps where the middle was left out take the time of the
         --  first item after them.
         Cut_Time := (if Back > 0 then Item.Items (Order (Used - Back + 1)).Time
                      elsif Front > 0 then Item.Items (Order (Front)).Time else 0);
         --  Where the middle was left out: each shown instance's account,
         --  oldest instance first.
         for I in reverse 1 .. Item.Shown loop
            declare
               A : Account renames Item.Kept (I);
            begin
               if A.Lost > 0 then
                  Logs.Add_Gap (Image, Cut_Time, A.Source, A.Lost, Logs.Lost, Added);
               end if;
               if A.Not_Retained > 0 then
                  Logs.Add_Gap (Image, Cut_Time, A.Source, A.Not_Retained, Logs.Not_Retained, Added);
               end if;
               if A.Not_Shown > 0 then
                  Logs.Add_Gap (Image, Cut_Time, A.Source, A.Not_Shown, Logs.Not_Shown, Added);
               end if;
            end;
         end loop;
         for Position in Used - Back + 1 .. Used loop
            Add_Item (Item.Items (Order (Position)));
         end loop;
         for I in reverse 1 .. Item.Shown loop
            declare
               A : Account renames Item.Kept (I);
            begin
               Logs.Add_Tally
                 (Image, A.Source, A.Tally.Published, A.Tally.Retained, A.Tally.Not_Retained,
                  A.Tally.Over_Budget, A.Tally.Publisher_Full, Added);
            end;
         end loop;
      end;
   end Build;
end CCL_Log_Selection;
