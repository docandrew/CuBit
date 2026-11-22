--  CuAlloc's provider on Linux, for hosted tests: reservations are PROT_NONE
--  mappings (aligned by over-mapping and trimming), commits make an exact
--  prefix readable and writable (the kernel's contract, checked), releases
--  unmap. Quota bounds the committed bytes, so tests can run the heap out
--  of memory. Threads call it concurrently (each heap commits its own
--  arenas): a spin lock serializes its table.
with Interfaces; use Interfaces;
package Linux_Provider is
   Maximum_Commit : constant := 16 * 1_048_576;
   --  The largest alignment Reserve accepts (the kernel's: 1 GiB).
   Maximum_Alignment : constant := 2 ** 30;
   Quota : Unsigned_64 := Unsigned_64'Last with Volatile;
   Committed : Unsigned_64 := 0 with Volatile;
   Contract_Violations : Natural := 0 with Volatile;
   --  Tests: refuse every release (the reservation stays, still counted).
   Refuse_Release : Boolean := False with Volatile;
   function Reserve (Bytes, Alignment : Unsigned_64) return Unsigned_64;
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean;
   function Release (Base, Bytes : Unsigned_64) return Boolean;
   function Live_Reservations return Natural;
   --  Set Quota under the table's lock.
   procedure Set_Quota (Bytes : Unsigned_64);
end Linux_Provider;
