with Interfaces; use Interfaces;
with CuBit.Kernel_ABI;
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
   Page_Bytes : constant := 4096;
   --  The most one Change (Commit) may back; Allocate steps through more.
   Maximum_Commit_Bytes : constant := CuBit.Kernel_ABI.Maximum_Owned_Commit_Bytes;
   Maximum_Region_Bytes : constant := CuBit.Kernel_ABI.Maximum_Owned_Region_Bytes;

   --  Page-aligned Bytes (up to Maximum_Region_Bytes) of address space, no
   --  RAM, its base a multiple of Alignment (0: a page; else a power of two
   --  up to 1 GiB). The base, or 0.
   function Reserve (Bytes : Unsigned_64; Alignment : Unsigned_64 := 0)
                     return Unsigned_64;
   --  One operation on [Address, Address + Bytes), page-aligned, inside one
   --  region. Commit: no page of it committed yet, at most
   --  Maximum_Commit_Bytes. Decommit: committed pages dropped, others
   --  ignored. Access operations: every page committed.
   function Change (Address, Bytes : Unsigned_64; Operation : Range_Operation)
                    return Boolean;
   --  The exact base and capacity Reserve gave.
   function Release (Base, Bytes : Unsigned_64) return Boolean;

   --  Commit [Address, Address + Bytes) of one region, any page multiple:
   --  in steps of at most Maximum_Commit_Bytes. On False some leading steps
   --  may be committed; releasing the region gives them back.
   function Commit_Steps (Address, Bytes : Unsigned_64) return Boolean;

   --  Bytes rounded up to whole pages (0 when that would exceed a region).
   function Page_Round (Bytes : Unsigned_64) return Unsigned_64 is
     (if Bytes = 0 or else Bytes > Maximum_Region_Bytes then 0
      else (Bytes + Page_Bytes - 1) / Page_Bytes * Page_Bytes);

   --  A region of Bytes (rounded up to pages) committed whole: zero-filled
   --  read/write memory. The base, or 0. On 0 the region (with any steps
   --  already committed) is released; only if the kernel refused that
   --  release would it stay, charged to the process until it exits. Free
   --  it with Free and the same Bytes.
   function Allocate (Bytes : Unsigned_64) return Unsigned_64;
   function Free (Base, Bytes : Unsigned_64) return Boolean is
     (Page_Round (Bytes) /= 0 and then Release (Base, Page_Round (Bytes)));

   --  For C (Mesa's ANV CPU tables): 0 on success, else nonzero, as before.
   --  Commit_From_C takes any page multiple, in steps, all or nothing.
   function Reserve_From_C (Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_owned_reserve";
   function Commit_From_C (Address, Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_owned_commit";
   function Release_From_C (Base, Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cubit_owned_release";
end CuBit.Owned_Reservations;
