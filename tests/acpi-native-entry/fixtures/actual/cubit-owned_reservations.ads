with Interfaces;
package CuBit.Owned_Reservations is
   --  Stable CPU virtual storage, not a device or DMA mapping API.
   --  Return conventions match the documented CuBit.Messages syscalls.
   function Reserve (Bytes : Interfaces.Unsigned_64)
                     return Interfaces.Unsigned_64
     with Export, Convention => C, External_Name => "cubit_owned_reserve";
   function Commit_Prefix (Base, Offset, Bytes : Interfaces.Unsigned_64)
                           return Interfaces.Unsigned_64
     with Export, Convention => C,
          External_Name => "cubit_owned_commit_prefix";
   function Release (Base, Bytes : Interfaces.Unsigned_64)
                     return Interfaces.Unsigned_64
     with Export, Convention => C,
          External_Name => "cubit_owned_release_reservation";
end CuBit.Owned_Reservations;
