--  CuAlloc's provider on Linux, for hosted tests: reservations are PROT_NONE
--  mappings, commits make an exact prefix readable and writable (the
--  kernel's contract, checked), releases unmap. Quota bounds the committed
--  bytes, so tests can run the heap out of memory.
with Interfaces; use Interfaces;
package Linux_Provider is
   Maximum_Commit : constant := 16 * 1_048_576;
   Quota : Unsigned_64 := Unsigned_64'Last;
   Committed : Unsigned_64 := 0;
   Contract_Violations : Natural := 0;
   function Reserve (Bytes : Unsigned_64) return Unsigned_64;
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean;
   function Release (Base, Bytes : Unsigned_64) return Boolean;
   function Live_Reservations return Natural;
end Linux_Provider;
