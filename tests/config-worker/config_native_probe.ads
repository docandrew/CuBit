with Interfaces;
with System;

--  The same probe can be called by a Linux fixture or a native Rust startup.
--  The database pointer is exclusively borrowed for this call and never saved.
--  No filesystem path, command line, hosted IO, or global state is used here.
package Config_Native_Probe is
   type Boot_Phase is (Seed, Advance, Verify_Only);
   for Boot_Phase use (Seed => 0, Advance => 1, Verify_Only => 2);

   --  Raw phase admits every bit pattern across the ABI. Return 0 on success,
   --  otherwise the numbered failed checkpoint. Checks are ordinary code,
   --  not assertions that disappear from native release builds.
   function Run (Database : System.Address; Phase : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32
     with Export, Convention => C, External_Name => "cubit_config_worker_probe";
end Config_Native_Probe;
