with CuBit.Owned_Reservations;
with Interfaces; use Interfaces;
package body Intel_GPU_Metadata_Platform is
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
     (CuBit.Owned_Reservations.Reserve (Bytes));
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
     (CuBit.Owned_Reservations.Commit_Prefix (Base, Offset, Bytes) = 0);
end Intel_GPU_Metadata_Platform;
