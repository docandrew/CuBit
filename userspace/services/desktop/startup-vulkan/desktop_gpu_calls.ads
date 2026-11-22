with Interfaces;
--  Desktop's bound on its calls to the GPU service (Native_GPU_Calls in the
--  Mesa glue): a stalled or deadlocked driver costs Desktop the GPU path, not
--  the UI (IPC-004). External state: the glue's call settings and counters.
package Desktop_GPU_Calls with SPARK_Mode,
  Abstract_State => (Service with External)
is
   --  Every later GPU-service call waits at most Milliseconds for its reply.
   procedure Bound (Milliseconds : Interfaces.Unsigned_64)
     with Global => (In_Out => Service),
       Pre => Milliseconds in 1 .. 60_000;
   --  Calls that timed out so far, and the request label of the latest one.
   procedure Read (Timeouts, Label : out Interfaces.Unsigned_32)
     with Global => (Input => Service);
end Desktop_GPU_Calls;
