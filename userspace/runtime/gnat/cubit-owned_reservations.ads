with Interfaces; use Interfaces;
with CuBit.Kernel_ABI;
with Owned_Memory_ABI;
--  Owned memory: the one region API (kernel Process.Owned_Memory, ABI-002).
--  A region is reserved address space; one call changes a page range of it
--  (commit, decommit, or the access of committed pages); one call gives it
--  back. Each call does one thing; there are no flags. Stable CPU virtual
--  storage, not a device or DMA mapping API.
--
--  Commit is explicit: a range is backed (fresh zero pages, read/write)
--  only when committed, and a refused commit is a clean failure here, not
--  a fault later. Decommit gives the frames (and their quota charge) back
--  and keeps the range reserved; it faults until committed again, which
--  then reads zero.
package CuBit.Owned_Reservations with Preelaborate is
   subtype Range_Operation is CuBit.Kernel_ABI.Owned_Range_Operation;
   function Commit return Range_Operation renames CuBit.Kernel_ABI.Commit;
   function Decommit return Range_Operation renames CuBit.Kernel_ABI.Decommit;
   function Make_Inaccessible return Range_Operation
     renames CuBit.Kernel_ABI.Make_Inaccessible;
   function Make_Read_Only return Range_Operation renames CuBit.Kernel_ABI.Make_Read_Only;
   function Make_Read_Write return Range_Operation renames CuBit.Kernel_ABI.Make_Read_Write;
   function Discard return Range_Operation renames CuBit.Kernel_ABI.Discard;
   Page_Bytes : constant := 4096;
   --  No size limit is compiled in (MEM-007): the kernel refuses what it
   --  cannot give, and says why (Owned_Memory_ABI.Refusal).
   subtype Refusal is Owned_Memory_ABI.Refusal;

   --  Page-aligned Bytes of address space, no RAM, its base a multiple of
   --  Alignment (0: a page; else a power of two). The base, or 0.
   function Reserve (Bytes : Unsigned_64; Alignment : Unsigned_64 := 0)
                     return Unsigned_64;
   --  The same, and why the kernel refused (Why is meaningful when the
   --  answer is 0).
   function Reserve (Bytes, Alignment : Unsigned_64; Why : out Refusal)
                     return Unsigned_64;
   --  One operation on [Address, Address + Bytes), page-aligned, inside one
   --  region. Commit: no page of it committed yet, any size, all or
   --  nothing. Decommit: committed pages dropped, others ignored. Access
   --  operations: every page committed.
   function Change (Address, Bytes : Unsigned_64; Operation : Range_Operation)
                    return Boolean;
   --  munmap's semantics: every page of [Base, Base + Bytes) that lies in a
   --  region stops being part of it (a whole region, its head, its tail or
   --  its middle), across regions and the holes between them.
   function Release (Base, Bytes : Unsigned_64) return Boolean;

   --  Bytes rounded up to whole pages (0 for 0, or when that would wrap).
   function Page_Round (Bytes : Unsigned_64) return Unsigned_64 is
     (if Bytes = 0 or else Bytes > Unsigned_64'Last - (Page_Bytes - 1) then 0
      else (Bytes + Page_Bytes - 1) / Page_Bytes * Page_Bytes);

   --  A region of Bytes (rounded up to pages) committed whole: zero-filled
   --  read/write memory. The base, or 0 (and the region released again).
   --  Free it with Free and the same Bytes.
   function Allocate (Bytes : Unsigned_64) return Unsigned_64;
   function Free (Base, Bytes : Unsigned_64) return Boolean is
     (Page_Round (Bytes) /= 0 and then Release (Base, Page_Round (Bytes)));

   --  For C (Mesa's ANV CPU tables): 0 on success, else the kernel's
   --  refusal word (Owned_Memory_ABI). Commit_From_C takes any page
   --  multiple, all or nothing.
   function Reserve_From_C (Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_owned_reserve";
   function Commit_From_C (Address, Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_owned_commit";
   function Release_From_C (Base, Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_owned_release";
end CuBit.Owned_Reservations;
