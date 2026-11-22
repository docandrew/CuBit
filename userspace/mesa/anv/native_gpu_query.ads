with Interfaces; use Interfaces;
with Native_GPU_Calls;
package Native_GPU_Query is
   type Reply_Words is array (Natural range 0 .. 3) of Unsigned_64
     with Convention => C;
   pragma Compile_Time_Error (Reply_Words'Size /= 256, "query FFI must be four uint64_t words");
   -- Caller supplies a writable four-word array, retained only for this call.
   -- Endpoint is already admitted and stays open for the whole discovery.
   -- Return 0 means a well-formed transport envelope; words carry query status.
   function Execute
     (Endpoint : Native_GPU_Calls.C_Endpoint; Selector : Unsigned_64; Output : access Reply_Words)
      return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_query";
   -- Fresh shared backing-pool observation, not a reservation. Successful
   -- transport returns [status,total bytes,retained bytes,unused tickets].
   -- Busy/unavailable remain service statuses, never fabricated heap sizes.
   function Budget (Endpoint : Native_GPU_Calls.C_Endpoint; Output : access Reply_Words)
      return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_budget";
end Native_GPU_Query;
