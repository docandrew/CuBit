with Interfaces;
package Owned_Reservation_Policy with SPARK_Mode => On is
   use Interfaces;
   Page_Bytes : constant Unsigned_64 := 4096;
   Maximum_Reserved_Bytes : constant Unsigned_64 := 2 * 1024 * 1024 * 1024;
   Maximum_Commit_Bytes : constant Unsigned_64 := 16 * 1024 * 1024;

   -- Capacity consumes virtual addresses only. A successful commit appends
   -- a bounded, page-aligned prefix; it never moves or shrinks old storage.
   -- Ownership, generation, overlap, frame allocation and TLB retirement
   -- remain obligations of the kernel adapter, not properties of this policy.
   function Valid_Capacity (Capacity : Unsigned_64) return Boolean is
     (Capacity > 0 and then Capacity <= Maximum_Reserved_Bytes and then
      Capacity mod Page_Bytes = 0);

   function Can_Commit (Capacity, Committed, Bytes : Unsigned_64)
                       return Boolean is
     (Valid_Capacity (Capacity) and then Committed <= Capacity and then
      Committed mod Page_Bytes = 0 and then Bytes > 0 and then
      Bytes <= Maximum_Commit_Bytes and then Bytes mod Page_Bytes = 0 and then
      Bytes <= Capacity - Committed);

   function After_Commit (Capacity, Committed, Bytes : Unsigned_64)
                         return Unsigned_64 is
     (if Can_Commit (Capacity, Committed, Bytes) then Committed + Bytes
      else Committed)
   with Post =>
     (if Can_Commit (Capacity, Committed, Bytes) then
         After_Commit'Result > Committed and then
         After_Commit'Result <= Capacity and then
         After_Commit'Result mod Page_Bytes = 0
      else After_Commit'Result = Committed);
end Owned_Reservation_Policy;
