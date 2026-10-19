with CuBit.Owned_Reservations;
with Interfaces; use Interfaces;
package body Intel_GPU_Metadata_Release is
   function Unreserve (Base, Bytes : Unsigned_64) return Boolean is
     (CuBit.Owned_Reservations.Release (Base, Bytes) = 0);
end Intel_GPU_Metadata_Release;
