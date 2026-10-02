with Interfaces; use Interfaces;
with System;
package Mesa_Probe_Log is
   type Probe_Callback is access function (Context : System.Address)
      return Unsigned_32 with Convention => C;
   -- In-process trusted test FFI, not a remotely callable pointer interface.
   -- Context/text remain valid during callback; no library-global publisher.
   function Run (Callback : Probe_Callback) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_test_run_logged";
   procedure Emit (Context, Text : System.Address; Length : Unsigned_32)
     with Export, Convention => C, External_Name => "cubit_test_log";
end Mesa_Probe_Log;
