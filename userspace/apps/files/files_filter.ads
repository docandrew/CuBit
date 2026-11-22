with Interfaces; use Interfaces;
with Files_Limits; use Files_Limits;
with Files_Listing;
with Files_Order;

--  The type-ahead filter (docs/files-app.md, "Data model"): the published
--  order's entries whose names contain the query, case-folded. It scans in
--  order, so every partial result is an exact prefix of the final one: a
--  query's first rows are right in its first frame, however large the
--  folder. Extending a query refines the previous matches only. A query
--  change shows its result as it grows (live); a new published order keeps
--  the previous result until the rescan completes. With no query the filter
--  is inactive and panes show the order itself.
--
--  Proved: no run-time errors; the shown count never exceeds the capacity
--  and no step exceeds its budget. Tested: the result equals a reference
--  filter of the order.
package Files_Filter with SPARK_Mode is
   --  Typed query bytes (a name component's worth is plenty to narrow).
   MAXIMUM_QUERY : constant := 64;
   subtype Query_Length is Natural range 0 .. MAXIMUM_QUERY;

   type Filter_State (Capacity : Entry_Capacity) is private;

   function Active (F : Filter_State) return Boolean;
   function Query (F : Filter_State) return String;
   --  Shown entries and the one at Position.
   function Count (F : Filter_State) return Entry_Count;
   function At_Position (F : Filter_State; Position : Entry_Id) return Entry_Id
     with Pre => Position <= Count (F);
   --  The scan has covered the whole published order.
   function Complete (F : Filter_State) return Boolean;
   function Revision (F : Filter_State) return Unsigned_64;
   --  The shown position of the tracked entry (the pane's cursor), 0 if it
   --  is not shown.
   function Tracked_Position (F : Filter_State) return Entry_Count;

   --  Whether Name contains the folded Query.
   function Matches (L : Files_Listing.Listing; Id : Entry_Id; F : Filter_State) return Boolean;

   procedure Reset (F : in out Filter_State)
     with Post => not Active (F) and then Count (F) = 0;
   procedure Set_Query (F : in out Filter_State; Text : String)
     with Pre => Text'Length <= MAXIMUM_QUERY,
          Post => Active (F) = (Text'Length > 0);
   procedure Track (F : in out Filter_State; Id : Entry_Id; Position : Entry_Count);
   procedure Step
     (F : in out Filter_State; L : Files_Listing.Listing; O : Files_Order.Order_State;
      Budget : Work_Budget; Used : out Work_Budget)
     with Pre => O.Capacity <= F.Capacity,
          Post => (Used <= Budget and then Count (F) <= F.Capacity) and (Active (F) = Active (F)'Old);
private
   type Buffer_Number is range 1 .. 2;
   type Id_Grid is array (Buffer_Number range <>, Entry_Id range <>) of Entry_Id;
   type Query_Bytes is array (1 .. MAXIMUM_QUERY) of Unsigned_8;

   type Filter_State (Capacity : Entry_Capacity) is record
      Text : Query_Bytes := [others => 0];
      Length : Query_Length := 0;
      Front : Buffer_Number := 1;
      Shown : Entry_Count := 0;
      Changes : Unsigned_64 := 0;
      --  The scan: from the order (or refining the shown result) into the
      --  other buffer; Live shows it as it grows.
      Scanning : Boolean := False;
      Live : Boolean := False;
      From_Order : Boolean := True;
      Target : Buffer_Number := 2;
      --  The order revision the shown result (or the scan) is of.
      Seen : Unsigned_64 := 0;
      Scan_Of : Unsigned_64 := 0;
      Fresh : Boolean := False;
      Source_Count : Entry_Count := 0;
      Next : Natural := 1;
      Built : Entry_Count := 0;
      Tracked : Entry_Id := 1;
      Tracked_At : Entry_Count := 0;
      Built_Tracked_At : Entry_Count := 0;
      Ids : Id_Grid (1 .. 2, 1 .. Capacity) := [others => [others => 1]];
   end record;

   function Active (F : Filter_State) return Boolean is (F.Length > 0);
   function Count (F : Filter_State) return Entry_Count is
     (if F.Shown <= F.Capacity then F.Shown else 0);
   function At_Position (F : Filter_State; Position : Entry_Id) return Entry_Id is
     (F.Ids (F.Front, Position));
   function Complete (F : Filter_State) return Boolean is (not F.Scanning and then not F.Fresh);
   function Revision (F : Filter_State) return Unsigned_64 is (F.Changes);
   function Tracked_Position (F : Filter_State) return Entry_Count is
     (if F.Tracked_At <= Count (F) then F.Tracked_At else 0);
end Files_Filter;
