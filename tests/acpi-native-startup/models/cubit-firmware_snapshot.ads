with Interfaces;
-- Only the result types needed by the mirrored adapter declarations.
-- No real authority, syscall, native copy or hardware implementation here.
package CuBit.Firmware_Snapshot is
   type Table_ID is new Interfaces.Unsigned_32;
   type Byte_Count is new Interfaces.Unsigned_64;
   type Outcome is
     (Ok, Not_Ready, Unknown_Table, Beyond_End, Empty_Before_End, Copy_Failed,
      Not_Authorized, Malformed);
end CuBit.Firmware_Snapshot;
