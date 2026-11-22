with Interfaces; use Interfaces;
with Intel_GPU_Ring_Reservation;
-- Native writes of one application context's ring (GPU-001 step 2): many
-- segments outstanding, each placed where the context's ring window
-- (Intel_GPU_Segment_Window) planned it, behind the retired head the
-- window derives from the completed timeline. Replaces the step 1 rule
-- "the previous segment completed" for application rings; the boot render
-- context keeps Intel_GPU_Native_Live_Ring.
--
-- The driver's window is the truth about where the tail is. The saved
-- RING_TAIL dword in the LRC image is checked against it through the
-- register's tail field only (Intel_GPU_Ring_Registers: the QWORD offset in
-- bits 3..20); the saved RING_HEAD is the engine's progress (offset plus a
-- wrap count) and is reported, never compared. A tail field that differs is
-- another writer's tail: nothing is written and the raw dwords are returned
-- for the log.
generic
   -- The selected context's retained backing: PPHWSP at page 0, the LRC
   -- state after it (saved ring head and tail at Saved_Head_Offset and
   -- Saved_Tail_Offset), and the 16 KiB ring at Ring_Offset. Caller
   -- serializes selection with Write.
   with function CPU_Base return Unsigned_64;
   with function Backing_Bytes return Unsigned_64;
   -- Exact retained backing, published context, and admission to publish
   -- a tail (enabled, or parked awaiting the enable that submits it).
   with function Owner_Ready return Boolean;
   -- Explicit ADL-N LLC/system-memory coherent WB saved-context mapping of
   -- THIS context (not the boot render context's).
   with function Coherent_Ready return Boolean;
package Intel_GPU_Native_Queue_Ring is
   Ring_Offset : constant Unsigned_64 := 65_536;
   Saved_Head_Offset : constant Unsigned_64 := 4_116;
   Saved_Tail_Offset : constant Unsigned_64 := 4_124;
   Minimum_Backing : constant Unsigned_64 := 81_920;
   Max_Words : constant := 96;
   type Word_Array is array (Natural range 0 .. Max_Words - 1) of Unsigned_32;

   type Outcome is
     (Written,         -- the segment and the new saved tail are published
      Bad_Request,     -- the plan, the words or the backing are not usable
      Not_Owned,       -- ownership or coherence was not established
      Tail_Mismatch,   -- the saved tail field is not the window's tail
      Flush_Failed,    -- the ring pages could not be made visible
      Ownership_Lost); -- ownership changed during the write
   type Report is record
      Result : Outcome := Bad_Request;
      -- The saved dwords as read before writing (0 when not read).
      Raw_Head, Raw_Tail : Unsigned_32 := 0;
   end record;

   -- Write Count words where Plan says (MI_NOOP padding from Expected_Tail
   -- to the ring's end first when it wraps), make them visible, then move
   -- the saved ring tail from Expected_Tail to Plan.Tail. Anything but
   -- Written: nothing may be assumed about the ring; the caller loses the
   -- context.
   procedure Write
     (Words : Word_Array; Count : Natural; Plan : Intel_GPU_Ring_Reservation.Plan;
      Expected_Tail : Unsigned_32; Status : out Report);
end Intel_GPU_Native_Queue_Ring;
