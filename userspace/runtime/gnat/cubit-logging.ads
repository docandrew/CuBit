pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Log_Protocol;
with CuBit.Log_Records;

--  Native adapter, not part of the portable SPARK proof. Keep these limited
--  objects alive while connected: their aligned pages back grants. Publishers
--  may be destroyed after Disconnect reports Done.
--  Readers remain process-lived.
--  Calls on each object are serialized by its application event loop.
package CuBit.Logging is
   type Publisher
     (Slot : CuBit.Messages.CapabilitySlot :=
        CuBit.Log_Protocol.Publisher_Slot)
     is limited private;
   --  One outstanding publication. Busy/unavailable returns False and counts
   --  a drop; never waits for the collector. Tokens must be unique among the
   --  application's live async requests. No hidden completion polling.
   --  Rate-limited completions count a drop but leave the publisher usable.
   --  No automatic retry: that would turn diagnostic loss into more load.
   procedure Emit
     (Item : in out Publisher; Value : CuBit.Log_Records.Log_Record;
      Token : Unsigned_64; Submitted : out Boolean);
   procedure Complete
     (Item : in out Publisher; Completion : CuBit.Messages.CompletionEntry;
      Handled : out Boolean);
   function Dropped (Item : Publisher) return Unsigned_64;
   function Pending (Item : Publisher) return Boolean;
   --  Terminal disconnect: prohibits further Emit calls from submitting work.
   --  No service RPC wait. Revoke, then query retirement; repeat after
   --  handling completions. Done requires BOTH grant retirement and no CQE
   --  outstanding. Failure/death alone never releases the buffer.
   --  No discovery/rebind/retry or automatic reconnection is performed.
   procedure Disconnect (Item : in out Publisher; Done : out Boolean);

   type Reader
     (Slot : CuBit.Messages.CapabilitySlot := CuBit.Log_Protocol.Observer_Slot)
     is limited private;
   --  Interactive synchronous operations. Never use these in interrupt or
   --  latency-sensitive service paths. Close closes the subscription only;
   --  its buffer/grant stays owned by the Reader for safe reuse.
   procedure Subscribe
     (Item : in out Reader; Result : out CuBit.Log_Protocol.Status);
   procedure Read_Next
     (Item : in out Reader; Value : out CuBit.Log_Protocol.Event;
      Lost : out Unsigned_64; Result : out CuBit.Log_Protocol.Status);
   procedure Close
     (Item : in out Reader; Result : out CuBit.Log_Protocol.Status);
private
   type Transfer_Page is array (Positive range 1 .. 4096) of Unsigned_8
     with Alignment => 4096;
   type Writer_State is (Uninitialized, Ready, In_Flight, Disabled);
   type Publisher
     (Slot : CuBit.Messages.CapabilitySlot :=
        CuBit.Log_Protocol.Publisher_Slot)
     is limited record
      Page : Transfer_Page := [others => 0];
      Grant : CuBit.Memory_Grants.Grant_Reference;
      State : Writer_State := Uninitialized;
      Token : Unsigned_64 := 0;
      Loss : Unsigned_64 := 0;
      Has_Grant : Boolean := False;
      Disconnecting : Boolean := False;
   end record;
   type Reader
     (Slot : CuBit.Messages.CapabilitySlot := CuBit.Log_Protocol.Observer_Slot)
     is limited record
      Page : Transfer_Page := [others => 0];
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Has_Grant : Boolean := False;
      Subscription : Unsigned_64 := 0;
   end record;
end CuBit.Logging;
