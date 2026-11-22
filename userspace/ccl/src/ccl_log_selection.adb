with Ada.Unchecked_Deallocation;

package body CCL_Log_Selection is
   use type Logs.Gap_Reason;
   use type Logs.Log_Window;

   procedure Free is new Ada.Unchecked_Deallocation (Pending_Items, Pending_Access);
   --  The items a selection starts with room for; it doubles from there.
   FIRST_ROOM : constant := 64;
   type Order_Array is array (Item_Index range <>) of Item_Index;
   type Order_Access is access Order_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Order_Array, Order_Access);

   overriding procedure Finalize (Item : in out Selection) is
   begin
      Free (Item.Items);
   end Finalize;

   --  Storage for item Index.
   procedure Make_Room (Item : in out Selection; Index : Item_Index) is
      Room : Natural := (if Item.Items = null then FIRST_ROOM else Item.Items'Length);
      Grown : Pending_Access;
   begin
      if Item.Items /= null and then Index <= Item.Items'Last then
         return;
      end if;
      while Room < Index loop
         Room := Natural'Min (2 * Room, Logs.RESULT_ITEM_CEILING);
      end loop;
      Grown := new Pending_Items (1 .. Room);
      if Item.Items /= null then
         Grown (Item.Items'Range) := Item.Items.all;
         Free (Item.Items);
      end if;
      Item.Items := Grown;
   end Make_Room;

   function Sum (A, B : Unsigned_64) return Unsigned_64 is
     (if A > Unsigned_64'Last - B then Unsigned_64'Last else A + B);

   function Shown (Item : Selection) return Boolean is
     (Item.Instances in 1 .. MAXIMUM_INSTANCES);

   procedure Start (Item : out Selection; Within : Logs.Log_Window) is
   begin
      Item.Within := Within;
      Item.Used := 0;
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
      Item.Room := Logs.RESULT_ITEM_CEILING - Item.Used;
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
         Make_Room (Item, Item.Base + Item.Head_Held);
         Item.Items (Item.Base + Item.Head_Held) := New_Item;
         Item.Items (Item.Base + Item.Head_Held).Head := True;
         return;
      end if;
      Slot := Item.Base + Item.Head_Room + Item.Ring_Next;
      Make_Room (Item, Slot);
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
            Ring : Pending_Access := new Pending_Items'(Item.Items (First + 1 .. First + Ring_Size));
         begin
            for K in 1 .. Ring_Size loop
               Item.Items (First + K) := Ring (Ring'First + (Item.Ring_Next + K - 2) mod Ring_Size);
            end loop;
            Free (Ring);
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

   procedure Build
     (Item : in out Selection; Contract : CCL.Objects.Binding; Result : out CCL.Objects.Holders.Holder)
   is
      Used : constant Item_Count := Item.Used;
      Added : Boolean;
      --  Records a full value refuses (past its ceilings): shown as one
      --  Not_Shown gap at the end.
      Refused : Unsigned_64 := 0;
      Refused_Source : Unsigned_64 := 0;
      Refused_Time : Logs.Boot_Milliseconds := 0;

      procedure Add_Item (Placed : Pending) is
      begin
         case Placed.Kind is
            when Record_Item =>
               Logs.Add (Result, Placed.Time, Level (Placed.Value), Placed.Source, Text (Placed.Value), Added);
               if not Added then
                  Refused := Sum (Refused, 1);
                  Refused_Source := Placed.Source;
                  Refused_Time := Placed.Time;
               end if;
            when Gap_Item =>
               Logs.Add_Gap (Result, Placed.Time, Placed.Source, Placed.Count, Placed.Reason, Added);
         end case;
      end Add_Item;

      Order : Order_Access := (if Used > 0 then new Order_Array (1 .. Used) else null);
      Front : Item_Count := 0;
      First_Time : Logs.Boot_Milliseconds := 0;
      Cut_Time : Logs.Boot_Milliseconds := 0;

      --  Order (1 .. Used) by time, oldest first, stable: a gap stays before
      --  its record, and equal times keep their order. Each instance's
      --  items are in time order already (newest instance first), so a
      --  bottom-up merge of runs.
      procedure Sort is
         Merged : Order_Access := new Order_Array (1 .. Used);
         Width : Positive := 1;
      begin
         for I in 1 .. Used loop
            Order (I) := I;
         end loop;
         while Width < Used loop
            declare
               Low : Positive := 1;
            begin
               while Low <= Used loop
                  declare
                     Middle : constant Natural := Natural'Min (Low + Width - 1, Used);
                     High : constant Natural := Natural'Min (Low + 2 * Width - 1, Used);
                     L : Positive := Low;
                     R : Positive := Middle + 1;
                  begin
                     for K in Low .. High loop
                        if L <= Middle and then
                          (R > High or else Item.Items (Order (L)).Time <= Item.Items (Order (R)).Time)
                        then
                           Merged (K) := Order (L);
                           L := L + 1;
                        else
                           Merged (K) := Order (R);
                           R := R + 1;
                        end if;
                     end loop;
                  end;
                  exit when Low > Used - 2 * Width;
                  Low := Low + 2 * Width;
               end loop;
            end;
            Order (1 .. Used) := Merged (1 .. Used);
            exit when Width > Used / 2;
            Width := 2 * Width;
         end loop;
         Free (Merged);
      end Sort;
   begin
      Logs.Start (Contract, Result);
      if Used > 0 then
         Sort;
         First_Time := Item.Items (Order (1)).Time;
         --  The first items, while they are their instance's first (its
         --  head, kept whatever the ring gave up after it).
         while Front < Used and then Item.Items (Order (Front + 1)).Head loop
            Front := Front + 1;
         end loop;
      end if;
      --  The older instances' records come before everything.
      if Item.Older_Not_Retained > 0 then
         Logs.Add_Gap (Result, First_Time, 0, Item.Older_Not_Retained, Logs.Not_Retained, Added);
      end if;
      if Item.Older_Records > 0 then
         Logs.Add_Gap (Result, First_Time, 0, Item.Older_Records, Logs.Not_Shown, Added);
      end if;
      for Position in 1 .. Front loop
         Add_Item (Item.Items (Order (Position)));
      end loop;
      --  The gaps where a full ring gave up the middle take the time of the
      --  first item after them.
      Cut_Time := (if Front < Used then Item.Items (Order (Front + 1)).Time
                   elsif Front > 0 then Item.Items (Order (Front)).Time else 0);
      --  Each shown instance's account, oldest instance first.
      for I in reverse 1 .. Item.Shown loop
         declare
            A : Account renames Item.Kept (I);
         begin
            if A.Lost > 0 then
               Logs.Add_Gap (Result, Cut_Time, A.Source, A.Lost, Logs.Lost, Added);
            end if;
            if A.Not_Retained > 0 then
               Logs.Add_Gap (Result, Cut_Time, A.Source, A.Not_Retained, Logs.Not_Retained, Added);
            end if;
            if A.Not_Shown > 0 then
               Logs.Add_Gap (Result, Cut_Time, A.Source, A.Not_Shown, Logs.Not_Shown, Added);
            end if;
         end;
      end loop;
      for Position in Front + 1 .. Used loop
         Add_Item (Item.Items (Order (Position)));
      end loop;
      if Refused > 0 then
         Logs.Add_Gap (Result, Refused_Time, Refused_Source, Refused, Logs.Not_Shown, Added);
      end if;
      for I in reverse 1 .. Item.Shown loop
         declare
            A : Account renames Item.Kept (I);
         begin
            Logs.Add_Tally
              (Result, A.Source, A.Tally.Published, A.Tally.Retained, A.Tally.Not_Retained,
               A.Tally.Over_Budget, A.Tally.Publisher_Full, Added);
         end;
      end loop;
      Free (Order);
   end Build;
end CCL_Log_Selection;
