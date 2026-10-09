pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
with Metric_Histograms;
with Metric_History;

--  Bounded metric aggregation for metrics.svc. Single owner: the service
--  loop serializes every call. Callers authenticate the immediate peer and
--  its kernel-stamped tag and gate each operation with
--  CuBit.Metric_Protocol.May_Invoke before calling in. Pages passed here are
--  private copies, never shared producer memory.
package Metric_Store with SPARK_Mode is
   package Records renames CuBit.Metric_Records;
   package Protocol renames CuBit.Metric_Protocol;
   use type Protocol.Status;

   Maximum_Sources : constant := 16;
   subtype Source_Count is Natural range 0 .. Maximum_Sources;
   subtype Source_Index is Source_Count range 1 .. Maximum_Sources;
   Series_Slots : constant := Maximum_Sources * Records.Maximum_Keys;
   subtype Series_Cursor is Natural range 0 .. Series_Slots;
   --  An idle source may be evicted for a new one after this long.
   Source_Lease_Ms : constant Unsigned_64 := 60_000;

   --  Not limited only so contracts can name the prior state.
   package Raw is new Metric_History;
   type Store is private with Default_Initial_Condition => Valid (Store);
   function Valid (Item : Store) return Boolean;
   function History_Next (Item : Store) return Unsigned_64;
   function History_Dropped (Item : Store) return Unsigned_64;
   procedure Read_History (Item : Store; Cursor : Unsigned_64;
      Value : out Raw.Event; Next, Gap : out Unsigned_64;
      Available, Valid_Cursor : out Boolean)
     with Pre => Valid (Item),
       Post => Valid_Cursor = (Cursor /= 0 and Cursor <= History_Next (Item)) and then
         (if Valid_Cursor then Next >= Cursor and Next <= History_Next (Item)) and then
         (if Available then Valid_Cursor and Raw.Valid_Event (Value));

   type Ingest_Outcome is record
      Result : Protocol.Status := Protocol.Invalid_Request;
      Accepted : Records.Record_Count := 0;
      Rejected : Records.Record_Count := 0;
   end record;

   --  Trusted monotonic service time; backwards readings are ignored.
   procedure Advance_Time (Item : in out Store; Now_Ms : Unsigned_64)
     with Pre => Valid (Item), Post => Valid (Item);

   --  Identity of the source a publisher writes to, and the guarantee that
   --  a publication touches no other publisher's data.
   function Owned_By
     (Item : Store; Source : Source_Index; Pid, Tag : Unsigned_64)
      return Boolean;
   function Same_Except (Before, After : Store; Pid, Tag : Unsigned_64)
     return Boolean with Ghost;

   procedure Ingest
     (Item : in out Store; Pid, Tag : Unsigned_64;
      Page : Records.Page_Words; Bytes : Unsigned_64;
      Outcome : out Ingest_Outcome)
     with Pre => Valid (Item) and Pid /= 0 and Protocol.Is_Publisher (Tag),
          Post => Valid (Item) and then Same_Except (Item'Old, Item, Pid, Tag) and then
            (if Outcome.Result /= Protocol.OK then
               Outcome.Accepted = 0 and Outcome.Rejected = 0 and
               History_Next (Item) = History_Next (Item'Old)
             else History_Next (Item) = History_Next (Item'Old) +
               Unsigned_64'Min (Unsigned_64 (Outcome.Accepted),
                 Unsigned_64'Last - History_Next (Item'Old)));

   --  Copies summaries of declared series starting at ordinal Cursor.
   --  Next is the ordinal to resume from; Series_Slots means done.
   procedure Fill_Summaries
     (Item : Store; Cursor : Series_Cursor; Rows : out Protocol.Summary_Page;
      Written : out Protocol.Row_Count; Next : out Series_Cursor)
     with Post => Next >= Cursor and then
       (Next = Series_Slots or else Written = Protocol.Rows_Per_Page);
private
   type Series is record
      Declared : Boolean := False;
      Kind : Records.Metric_Kind := Records.Counter;
      Measure : Records.Unit := Records.Count;
      Name : Records.Metric_Name;
      Samples : Metric_Histograms.Histogram := Metric_Histograms.Empty;
      Total : Unsigned_64 := 0;
      Total_Saturated : Boolean := False;
      Last_Time : Unsigned_64 := 0;
      Rejected : Unsigned_64 := 0;
   end record;
   type Series_Table is array (Records.Metric_Key) of Series;
   type Source is record
      Active : Boolean := False;
      Pid : Unsigned_64 := 0;
      Tag : Unsigned_64 := 0;
      --  Zero until the first accepted batch.
      Next_Sequence : Unsigned_64 := 0;
      Batches : Unsigned_64 := 0;
      Batch_Gaps : Unsigned_64 := 0;
      Producer_Dropped : Unsigned_64 := 0;
      Rejected : Unsigned_64 := 0;
      Last_Use_Ms : Unsigned_64 := 0;
      Keys : Series_Table;
   end record;
   type Source_Table is array (Source_Index) of Source;
   type Store is record
      History : Raw.State;
      History_Lost : Unsigned_64 := 0;
      Sources : Source_Table;
      Now_Ms : Unsigned_64 := 0;
   end record;

   function Valid (Item : Store) return Boolean is (Raw.Valid (Item.History));
   function History_Next (Item : Store) return Unsigned_64 is (Raw.Following (Item.History));
   function History_Dropped (Item : Store) return Unsigned_64 is (Item.History_Lost);
   function Owned_By
     (Item : Store; Source : Source_Index; Pid, Tag : Unsigned_64)
      return Boolean is
     (Item.Sources (Source).Active and then
      Item.Sources (Source).Pid = Pid and then
      Item.Sources (Source).Tag = Tag);
   function Same_Except (Before, After : Store; Pid, Tag : Unsigned_64)
     return Boolean is
     (After.Now_Ms = Before.Now_Ms and then
      (for all S in Source_Index =>
         After.Sources (S) = Before.Sources (S) or else
         Owned_By (After, S, Pid, Tag)));
end Metric_Store;
