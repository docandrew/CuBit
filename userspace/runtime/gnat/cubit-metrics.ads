pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Metric_Records;
with CuBit.Metric_Protocol;
with CuBit.Metric_Batches;

--  Native metrics.svc client adapter, not part of the portable SPARK proof
--  (the batching state machine it drives is proved). Keep these limited
--  objects alive while connected: their aligned pages back grants. Calls on
--  each object are serialized by the owning event loop.
package CuBit.Metrics is
   package Records renames CuBit.Metric_Records;
   package Protocol renames CuBit.Metric_Protocol;

   --  Default slot assignments come from the program's CCL manifest
   --  (generated CCL_Manifest_Bindings); there is no fixed binding.
   type Publisher (Slot : CuBit.Messages.CapabilitySlot) is limited private;

   --  Appends to the filling page: no IPC, syscall or allocation. A full or
   --  busy page drops the record and counts it (reported in the next batch).
   procedure Put
     (Item : in out Publisher; Value : Records.Metric_Record;
      Accepted : out Boolean)
     with Pre => Records.Valid (Value);
   --  True when Put would accept a record now; producers that prefer
   --  flushing to dropping check this first.
   function Has_Room (Item : Publisher) return Boolean;
   --  Four raw trace fragments are accepted together or refused together.
   --  No IPC or allocation; disabled publishers count them as rejected.
   function Has_Group_Room (Item : Publisher) return Boolean;
   procedure Put_Group
     (Item : in out Publisher; Values : Records.Trace_Group;
      Accepted : out Boolean)
     with Pre => Records.Valid_Group (Values);
   --  Seals the filling page and submits it asynchronously. Returns without
   --  submitting when the page is empty or no page is free. Token must be
   --  unique among the application's live asynchronous requests; the
   --  completion carrying it must be passed to Complete.
   procedure Flush
     (Item : in out Publisher; Token : Unsigned_64; Submitted : out Boolean);
   procedure Complete
     (Item : in out Publisher;
      Completion : CuBit.Messages.CompletionEntry;
      Handled : out Boolean);
   function Dropped (Item : Publisher) return Unsigned_64;
   --  Records the service rejected (malformed or undeclared) or failed
   --  batches. Disabled publishers drop everything; no automatic retry.
   function Rejected (Item : Publisher) return Unsigned_64;
   function Disabled (Item : Publisher) return Boolean;
   --  Terminal: revoke both grants; Done once both retired and no batch is
   --  in flight. Keep the object alive and forward completions until Done.
   procedure Disconnect (Item : in out Publisher; Done : out Boolean);

   --  Interactive, synchronous query; never use on latency-sensitive paths.
   type Observer (Slot : CuBit.Messages.CapabilitySlot) is limited private;
   procedure Query
     (Item : in out Observer; Cursor : Unsigned_64;
      Rows : out Protocol.Summary_Page; Written : out Protocol.Row_Count;
      Next : out Unsigned_64; Result : out Protocol.Status);
private
   type Grant_Array is array (CuBit.Metric_Batches.Page_Id)
     of CuBit.Memory_Grants.Grant_Reference;
   type Flag_Array is array (CuBit.Metric_Batches.Page_Id) of Boolean;
   type Token_Array is array (CuBit.Metric_Batches.Page_Id) of Unsigned_64;
   type Publisher (Slot : CuBit.Messages.CapabilitySlot) is limited record
      Pages : CuBit.Metric_Batches.Page_Pair :=
        [others => [others => 0]];
      State : CuBit.Metric_Batches.Builder;
      Grants : Grant_Array;
      Has_Grant : Flag_Array := [others => False];
      Tokens : Token_Array := [others => 0];
      Refused : Unsigned_64 := 0;
      Off : Boolean := False;
   end record;

   type Summary_Buffer is new Protocol.Summary_Page
     with Alignment => Records.Page_Bytes;
   type Observer (Slot : CuBit.Messages.CapabilitySlot) is limited record
      Page : Summary_Buffer := [others => [others => 0]];
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Has_Grant : Boolean := False;
   end record;
end CuBit.Metrics;
