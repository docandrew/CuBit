with CuBit.Owned_Reservations;
with Interfaces; use Interfaces;
package body Intel_GPU_Slot_Platform is
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
     (CuBit.Owned_Reservations.Reserve (Bytes));
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
     (CuBit.Owned_Reservations.Change
        (Base + Offset, Bytes, CuBit.Owned_Reservations.Commit));
end Intel_GPU_Slot_Platform;
