with Interfaces; use Interfaces;
package Intel_GPU_Ring_Reservation with SPARK_Mode is
   Ring_Bytes : constant Unsigned_32 := 16384;
   Guard_Bytes : constant Unsigned_32 := 64;
   type Outcome is (Invalid, No_Space, Ready);
   type Plan is record
      Status : Outcome := Invalid;
      Padding, Start, Tail, Consumed : Unsigned_32 := 0;
   end record;
   -- Pure arithmetic only. Retired_Head is a trusted consumed-prefix boundary
   -- for this exact ring incarnation, NOT a client offset or raw saved MMIO.
   -- Caller serializes reservation/publication/retirement and authenticates
   -- completion. Equal head/tail means empty, never a full ambiguous ring.
   -- Preserve a cacheline gap (Linux __intel_ring_space) and the current
   -- effective-end guard. On wrap, every Padding byte from old tail to the
   -- physical end must be MI_NOOP before Start=0 commands are published.
   -- Never write the hardware head. A failed publication cannot be retried
   -- merely because a new arithmetic plan can be calculated.
   -- The free bytes between the tail and the retired head (equal means the
   -- whole ring is free).
   function Free_Distance (Retired_Head, Current_Tail : Unsigned_32) return Unsigned_32 is
     (if Retired_Head > Current_Tail then Retired_Head - Current_Tail
      else Ring_Bytes - Current_Tail + Retired_Head)
     with Pre => Retired_Head < Ring_Bytes and Current_Tail < Ring_Bytes;
   function Reserve (Retired_Head, Current_Tail, Bytes : Unsigned_32) return Plan
     with Post =>
       (if Reserve'Result.Status = Ready then
          Retired_Head < Ring_Bytes and then Retired_Head mod 4 = 0 and then
          -- Everything written (padding and segment) lies in the free bytes,
          -- a cacheline short of the retired head: no unretired byte is
          -- overwritten.
          Reserve'Result.Consumed + Guard_Bytes <=
            Free_Distance (Retired_Head, Current_Tail) and then
          Bytes in 8 .. Ring_Bytes - Guard_Bytes and then Bytes mod 8 = 0 and then
          Current_Tail < Ring_Bytes and then Current_Tail mod 8 = 0 and then
          Reserve'Result.Start <= Ring_Bytes - Guard_Bytes - Bytes and then
          Reserve'Result.Start mod 8 = 0 and then
          Reserve'Result.Tail = Reserve'Result.Start + Bytes and then
          Reserve'Result.Consumed = Reserve'Result.Padding + Bytes and then
          Reserve'Result.Consumed <= Ring_Bytes - Guard_Bytes and then
          (if Current_Tail > Ring_Bytes - Guard_Bytes - Bytes then
             Reserve'Result.Start = 0 and then
             Reserve'Result.Padding = Ring_Bytes - Current_Tail
           else Reserve'Result.Start = Current_Tail and then
             Reserve'Result.Padding = 0));
end Intel_GPU_Ring_Reservation;
