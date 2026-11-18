pragma Ada_2022;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Monotonic;

package body CuBit.Waits is

   Microseconds_Per_Millisecond : constant := 1_000;

   function Wait (Sources, Deadline : Unsigned_64; A, B, C : Unsigned_64 := 0)
     return Unsigned_64 is
     (CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Wait, Sources, Deadline, A, B, C));

   procedure Sleep_Until (Instant : Unsigned_64) is
      Ignore : constant Unsigned_64 := Wait (W.Timer_Sources, W.Forever, Instant);
   begin
      null;
   end Sleep_Until;

   function Sleep_For (Milliseconds : Unsigned_64) return Unsigned_64 is
      Now_Us : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      if Milliseconds = 0 or else Now_Us = CuBit.Monotonic.Unavailable_Us
        or else Milliseconds > (Unsigned_64'Last - Now_Us) / Microseconds_Per_Millisecond
      then
         return 0;
      end if;
      Sleep_Until (Now_Us + Milliseconds * Microseconds_Per_Millisecond);
      return 0;
   end Sleep_For;

   procedure Sleep_For (Milliseconds : Unsigned_64) is
      Ignore : constant Unsigned_64 := Sleep_For (Milliseconds);
   begin
      null;
   end Sleep_For;

   function Futex_Wait (Word : System.Address; Expected, Deadline : Unsigned_64)
     return Unsigned_64 is
     (Wait (W.Futex_Sources, Deadline,
            Unsigned_64 (System.Storage_Elements.To_Integer (Word)), Expected));

   function Futex_Wait (Name : Futex_Name; Expected, Deadline : Unsigned_64)
     return Unsigned_64 is
     (Wait (W.Futex_Sources + (if Name.Shared then W.Shared_Futex else 0), Deadline,
            Name.Word, Expected));

   function Futex_Wake (Name : Futex_Name; Count : Unsigned_64) return Unsigned_64 is
     (CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Futex_Wake, Name.Word, Count,
         (if Name.Shared then FA.Shared_Space else FA.Private_Space)));

   function Hand_Off (Peer, Mine : Futex_Name; Expected, Deadline : Unsigned_64)
     return Unsigned_64 is
     (CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Wait,
         W.Futex_Sources + W.Wake_First
           + (if Mine.Shared then W.Shared_Futex else 0)
           + (if Peer.Shared then W.Wake_Shared else 0),
         Deadline, Mine.Word, Expected, 0, Peer.Word));

end CuBit.Waits;
