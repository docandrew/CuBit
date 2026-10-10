with Interfaces; use Interfaces;
with Files_Limits; use Files_Limits;
with Files_Listing;

--  A listing's entries in sort order, built in bounded slices
--  (docs/files-app.md, "Data model"). Folders come first; ties fall to the
--  name and then to the Entry_Id, so the order is total. Names compare
--  naturally ("file2" before "file10"), ASCII case-folded.
--
--  The published order is what panes show. A new order is built in two
--  work buffers by a resumable bottom-up merge sort and replaces the
--  published one in one step when complete, so a re-sort never shows a half
--  sorted list. While a listing streams in, arrived entries are sorted as a
--  tail and merged into the published order once the tail reaches half its
--  size (or the caller settles): O(n log n) in all, the first page at once.
--
--  Proved: no run-time errors, the published order holds exactly the
--  published count of slots, and no step does more than its budget.
--  Tested (tests/files-app): sortedness and that the order is a permutation
--  of the arrived entries, against a reference sort.
package Files_Order with SPARK_Mode is
   type Sort_Key is (By_Name, By_Extension, By_Modified, By_Size);
   type Sort_Direction is (Ascending, Descending);
   type Sort_Rule is record
      Key : Sort_Key := By_Name;
      Direction : Sort_Direction := Ascending;
   end record;

   type Ordering is (Less, Same, Greater);
   --  Natural, case-folded name order.
   function Compare_Names (L : Files_Listing.Listing; A, B : Entry_Id) return Ordering;
   --  Whether A comes before B under Rule (a strict total order on Ids).
   function Before (L : Files_Listing.Listing; Rule : Sort_Rule; A, B : Entry_Id) return Boolean;

   --  Below this many published entries every arrival is merged at once.
   FIRST_ROWS : constant := 512;

   type Order_State (Capacity : Entry_Capacity) is private;

   --  What every operation keeps (an initialized or Reset state has it).
   function Consistent (O : Order_State) return Boolean;
   function Count (O : Order_State) return Entry_Count;
   --  The published order's entry at Position (1 = first).
   function At_Position (O : Order_State; Position : Entry_Id) return Entry_Id
     with Pre => Position <= Count (O);
   function Rule (O : Order_State) return Sort_Rule;
   --  The rule the published order follows.
   function Published_Rule (O : Order_State) return Sort_Rule;
   --  Advances whenever the published order changes.
   function Revision (O : Order_State) return Unsigned_64;
   --  Work is waiting: arrived entries not yet published, or a new rule.
   function Busy (O : Order_State; L : Files_Listing.Listing) return Boolean;

   --  The pane's cursor entry, followed through every new order: its
   --  published position (0: unknown, then Position_Of finds it).
   procedure Track (O : in out Order_State; Id : Entry_Id; Position : Entry_Count)
     with Pre => Consistent (O),
          Post => Consistent (O) and (Count (O) = Count (O)'Old) and (Rule (O) = Rule (O)'Old);
   function Tracked_Position (O : Order_State) return Entry_Count;
   --  Where Id is in the published order, 0 if it is not (a linear search).
   function Position_Of (O : Order_State; Id : Entry_Id) return Entry_Count
     with Post => Position_Of'Result <= Count (O);

   --  Nothing published, nothing in progress (a new listing begins).
   procedure Reset (O : in out Order_State)
     with Post => (Consistent (O) and then Count (O) = 0) and (Rule (O) = Rule (O)'Old);
   --  Sort by Rule from now on; the published order stays until the new one
   --  is complete.
   procedure Set_Rule (O : in out Order_State; Value : Sort_Rule)
     with Pre => Consistent (O),
          Post => (Consistent (O) and then Rule (O) = Value) and (Count (O) = Count (O)'Old);
   --  Do at most Budget units of work (one unit: one entry compared and
   --  moved). Settle: merge whatever has arrived without waiting for the
   --  tail to grow (the listing is complete or its stream paused).
   procedure Step
     (O : in out Order_State; L : Files_Listing.Listing; Budget : Work_Budget; Settle : Boolean;
      Used : out Work_Budget)
     with Pre => Consistent (O) and then L.Capacity <= O.Capacity and then Files_Listing.Valid (L)
                 and then Count (O) <= L.Count,
          Post => (Consistent (O) and then Used <= Budget and then Count (O) <= L.Count)
                  and (Rule (O) = Rule (O)'Old);
private
   type Buffer_Number is range 1 .. 3;
   type Id_Grid is array (Buffer_Number range <>, Entry_Id range <>) of Entry_Id;
   type Phase_Kind is (Idle, Sorting, Merging);

   type Order_State (Capacity : Entry_Capacity) is record
      Wanted : Sort_Rule;
      Shown_Rule : Sort_Rule;
      Front : Buffer_Number := 1;
      Published : Entry_Count := 0;
      Changes : Unsigned_64 := 0;
      Phase : Phase_Kind := Idle;
      --  The job: a full sort of Ids 1 .. Last, or the tail Base + 1 ..
      --  Last merged into the published Base.
      Full : Boolean := False;
      Base : Entry_Count := 0;
      Last : Entry_Count := 0;
      --  Its runs live in Runs (merged into the other work buffer); Length
      --  of them at positions 1 .. Length.
      Runs : Buffer_Number := 2;
      Length : Entry_Count := 0;
      Width : Natural := 1;
      --  The current pair's start, left and right cursors and output.
      Pair : Natural := 1;
      Left, Right, Output : Natural := 1;
      Tracked : Entry_Id := 1;
      Tracked_At : Entry_Count := 0;
      Built_Tracked_At : Natural := 0;
      Ids : Id_Grid (1 .. 3, 1 .. Capacity) := [others => [others => 1]];
   end record;

   function Consistent (O : Order_State) return Boolean is (O.Published <= O.Capacity);
   function Count (O : Order_State) return Entry_Count is
     (if O.Published <= O.Capacity then O.Published else 0);
   function At_Position (O : Order_State; Position : Entry_Id) return Entry_Id is
     (O.Ids (O.Front, Position));
   function Rule (O : Order_State) return Sort_Rule is (O.Wanted);
   function Tracked_Position (O : Order_State) return Entry_Count is
     (if O.Tracked_At <= Count (O) then O.Tracked_At else 0);
   function Published_Rule (O : Order_State) return Sort_Rule is (O.Shown_Rule);
   function Revision (O : Order_State) return Unsigned_64 is (O.Changes);
   function Busy (O : Order_State; L : Files_Listing.Listing) return Boolean is
     (O.Phase /= Idle or else O.Published < L.Count or else O.Wanted /= O.Shown_Rule);
end Files_Order;
