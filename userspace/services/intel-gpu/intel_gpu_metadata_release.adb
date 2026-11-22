with CuBit.Owned_Reservations;
with Interfaces; use Interfaces;
package body Intel_GPU_Metadata_Release is
   function Unreserve (Base, Bytes : Unsigned_64) return Boolean is
     (CuBit.Owned_Reservations.Release (Base, Bytes));
end Intel_GPU_Metadata_Release;
