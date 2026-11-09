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

   -- A reservation's base may be asked to be a multiple of an alignment:
   -- 0 (callers that pass none) or a power of two up to 1 GiB; anything up
   -- to a page means a page. CuAlloc places its arenas this way, so the
   -- arena of an address is the address with its low bits cleared.
   Maximum_Alignment : constant Unsigned_64 := 2 ** 30;
   function Valid_Alignment (Alignment : Unsigned_64) return Boolean is
     (Alignment = 0 or else
      (Alignment <= Maximum_Alignment and then (Alignment and (Alignment - 1)) = 0));
   function Effective_Alignment (Alignment : Unsigned_64) return Unsigned_64 is
     (Unsigned_64'Max (Alignment, Page_Bytes))
   with Pre => Valid_Alignment (Alignment),
        Post => Effective_Alignment'Result in Page_Bytes .. Maximum_Alignment and then
                (Effective_Alignment'Result and (Effective_Alignment'Result - 1)) = 0;

   -- The first multiple of Alignment at or above Candidate; 0 when there
   -- is none (it would wrap).
   function Align_Up (Candidate, Alignment : Unsigned_64) return Unsigned_64
   with Pre => Alignment in Page_Bytes .. Maximum_Alignment and then
               (Alignment and (Alignment - 1)) = 0,
        Post => (if Align_Up'Result /= 0 or else Candidate = 0 then
                   Align_Up'Result >= Candidate and then
                   (Align_Up'Result and (Alignment - 1)) = 0 and then
                   Align_Up'Result - Candidate < Alignment);
end Owned_Reservation_Policy;
