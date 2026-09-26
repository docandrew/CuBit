with Interfaces; use Interfaces;
with System;

--  Single-owner native bridge. Rust serializes entries with one shared mutex.
--  No borrowed pointer is retained. Only the static transfer buffer is granted.
package Native_Storage is
   function Transfer_Capacity return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_storage_transfer_capacity";
   function Initialize (Endpoint : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_storage_initialize";
   function Execute
     (Operation : Unsigned_32; Handle, Position : Unsigned_64;
      Data : System.Address; Length : Unsigned_64;
      Value : access Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_storage_execute";
   -- Parts is a borrowed C array of {const void *data; uint64_t length}.
   -- Each part is nonempty; total bytes must fit the owned transfer buffer.
   -- No descriptor/data pointer survives this synchronous call.
   function Write_Vector
     (Handle, Position : Unsigned_64; Parts : System.Address; Count : Unsigned_64;
      Value : access Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_storage_write_vector";
   function Shutdown return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_storage_shutdown";
end Native_Storage;
