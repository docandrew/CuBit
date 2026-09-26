with Interfaces;
with System;

--  Single-threaded service shell; Rust owns the database for the whole Run
--  call. These addresses are same-process FFI, never IPC payload pointers.
package Worker_Host is
   function Database_Path
     (Buffer : System.Address; Capacity : Interfaces.Unsigned_64;
      Length : access Interfaces.Unsigned_64) return Interfaces.Unsigned_32
     with Export, Convention => C, External_Name => "cubit_config_storage_path";

   function Run
     (Database : System.Address; Config_Endpoint : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_32
     with Export, Convention => C, External_Name => "cubit_config_storage_run";
end Worker_Host;
