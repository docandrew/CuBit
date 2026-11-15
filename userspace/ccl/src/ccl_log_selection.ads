with Interfaces; use Interfaces;
with CCL.Objects;
with CCL.Interfaces.Logs;

--  Which of a service's records one List<LogItem> result shows
--  (logs.recent, logs.between): a host replays each instance of the name,
--  newest instance first, through to its tally, and this keeps the records
--  in the window (CCL.Interfaces.Logs.Log_Window) and the gaps that touch
--  it. When the window holds more than one result carries, the first
--  records (an instance's boot, a Head_Share of the room) and then the
--  newest are shown; what lies between is counted in a typed Not_Shown gap
--  where it was, so the caller narrows the window. Records outside the
--  window are not counted: they are simply not asked for. Platform-neutral
--  (no logstore here), so the Linux-hosted tests run the same selection as
--  CuBit's ccl_log_io.
generic
   --  A record as the log source holds it: its severity and its message.
   type Log_Record is private;
   with function Level (Item : Log_Record) return CCL.Interfaces.Logs.Severity;
   with function Text (Item : Log_Record) return String;
package CCL_Log_Selection is
   package Logs renames CCL.Interfaces.Logs;

   --  Instances of the name whose records a result shows, newest first;
   --  older ones are counted in gaps of their own (source 0).
   MAXIMUM_INSTANCES : constant := 8;

   --  An instance's accounting, as its log source ends a replay with it.
   type Tally_Counts is record
      Published, Retained, Not_Retained, Over_Budget, Publisher_Full : Unsigned_64 := 0;
   end record;

   type Selection is limited private;

   --  An empty selection of the records in Within.
   procedure Start (Item : out Selection; Within : Logs.Log_Window)
     with Pre => Logs.Valid (Within);

   --  The next instance's replay begins (newest instance first).
   procedure Begin_Instance (Item : in out Selection; Source : Unsigned_64);
   --  Records the source does not give, just before its next record (Next:
   --  that record's time) or, for a gap with no record after it (Lost), at
   --  the last record's time (Last_Time).
   procedure Omitted (Item : in out Selection; Not_Retained, Lost, Next : Logs.Boot_Milliseconds);
   function Last_Time (Item : Selection) return Logs.Boot_Milliseconds;
   --  One record, at Time.
   procedure Logged (Item : in out Selection; Time : Logs.Boot_Milliseconds; Value : Log_Record);
   --  The instance's tally: its replay's end.
   procedure Tallied (Item : in out Selection; Tally : Tally_Counts);
   --  The instance's replay is over.
   procedure End_Instance (Item : in out Selection);

   --  The result: oldest first across instances, fitting one image of
   --  Contract and one result's literal (CCL.Language.MAX_LITERAL_BYTES).
   procedure Build (Item : in out Selection; Contract : CCL.Objects.Binding; Image : out CCL.Objects.Image);

private
   --  Of a result's room, the part for the first records (an instance's
   --  boot, as logstore keeps it); the rest is for the newest.
   HEAD_SHARE : constant := 4;

   --  One item of the result before it is placed: a record, or the gap
   --  just before the next record of its instance (Time: the record's
   --  before it).
   type Item_Kind is (Record_Item, Gap_Item);
   subtype Shown_Instance is Positive range 1 .. MAXIMUM_INSTANCES;
   subtype Shown_Count is Natural range 0 .. MAXIMUM_INSTANCES;
   type Pending is record
      Kind : Item_Kind := Record_Item;
      Value : Log_Record;
      Count : Unsigned_64 := 0;
      Reason : Logs.Gap_Reason := Logs.Not_Retained;
      Source : Unsigned_64 := 0;
      Time : Logs.Boot_Milliseconds := 0;
      Instance : Shown_Instance := 1;
      --  Among its instance's first items (HEAD_SHARE), kept whatever
      --  follows.
      Head : Boolean := False;
   end record;
   subtype Item_Count is Natural range 0 .. Logs.MAX_ITEMS;
   subtype Item_Index is Positive range 1 .. Logs.MAX_ITEMS;
   type Pending_Items is array (Item_Index) of Pending;
   type Item_Order is array (Item_Index) of Item_Index;

   --  What a shown instance's items leave out, before the first shown:
   --  records in the window (Not_Shown), and the counts of the gaps among
   --  them (not retained, lost); then its tally from the source.
   type Account is record
      Source : Unsigned_64 := 0;
      Not_Shown, Not_Retained, Lost : Unsigned_64 := 0;
      Tally : Tally_Counts;
   end record;
   type Accounts is array (Shown_Instance) of Account;

   type Selection is limited record
      Within : Logs.Log_Window := Logs.WHOLE_HISTORY;
      Items : Pending_Items;
      Used : Item_Count := 0;
      Order : Item_Order := [others => 1];
      Instances : Natural := 0;
      Shown : Shown_Count := 0;
      Kept : Accounts;
      --  Records of the instances beyond MAXIMUM_INSTANCES: in the window,
      --  and given up.
      Older_Records, Older_Not_Retained : Unsigned_64 := 0;
      --  The instance being replayed: its items go to Items (Base + 1 ..
      --  Base + Room): its first Head_Room in order, then a ring of its
      --  newest (Ring_Next is where the next goes, Ring_Held how many are
      --  there) until its replay ends. What the ring gives up is the middle
      --  of its history: counted in its account, and shown as gaps where
      --  it was.
      Base, Room, Head_Room, Head_Held, Ring_Held : Item_Count := 0;
      Ring_Next : Item_Index := 1;
      Source : Unsigned_64 := 0;
      Last : Logs.Boot_Milliseconds := 0;
   end record;

   function Last_Time (Item : Selection) return Logs.Boot_Milliseconds is (Item.Last);
end CCL_Log_Selection;
