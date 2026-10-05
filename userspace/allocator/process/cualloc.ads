------------------------------------------------------------------------------
--  CuAlloc: the process heap (docs/userspace-allocator.md, "CuAlloc: one
--  allocator for everything"). Every CuBit process allocates through it,
--  whatever its language: Ada System.Memory, the libc malloc family, Rust
--  GlobalAlloc. It grows until the provider (the kernel's owned-memory
--  quota) says no; it has no size limit of its own.
--
--  Three paths, by size:
--    small, up to 4 KiB   16 MiB arenas of 64 KiB slabs, each with its own
--                         proved Heap_Slabs metadata;
--    medium, up to 1 MiB  16 MiB arenas of 4 KiB page runs, each with its
--                         own proved Heap_Extents metadata;
--    huge, above 1 MiB    one reservation per block, given back on Free.
--  Each arena is one reservation: its metadata first, then the payload,
--  backed (committed) as blocks reach further into it. A directory, also
--  reserved and backed as it grows, keeps the arenas sorted by address, so
--  Free finds a block's arena by binary search.
--
--  One instance serves a process; callers serialize calls (the adapters'
--  lock). The proved cores keep live blocks disjoint within an arena; this
--  layer keeps arenas disjoint and routes each address to its own arena
--  (regression-tested, not proved).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

generic
   --  Reserve Bytes (a page multiple) of address space with no backing: its
   --  page-aligned base, or 0.
   with function Reserve (Bytes : Unsigned_64) return Unsigned_64;
   --  Back [Offset, Offset + Bytes) of the reservation at Base, where Offset
   --  is exactly what is backed already and Bytes at most Maximum_Commit.
   --  Fresh backing reads as zero.
   with function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean;
   --  Give back the whole reservation at Base, backed or not.
   with function Release (Base, Bytes : Unsigned_64) return Boolean;
   Maximum_Commit : Unsigned_64;
package CuAlloc is
   No_Address : constant Unsigned_64 := 0;
   Page_Bytes : constant := 4_096;
   --  The largest alignment a request may ask for.
   Maximum_Alignment : constant := 16#4000_0000#;

   --  A block of at least Bytes (1 when 0), aligned to Alignment (a power of
   --  two; at least 16), or No_Address when the provider refuses.
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64;
   --  As Allocate, every byte zero.
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64;
   --  No_Address is ignored; anything not from Allocate is ignored too.
   procedure Free (Item : Unsigned_64);
   --  The bytes the block at Item holds (0 when Item is not a live block).
   function Usable_Size (Item : Unsigned_64) return Unsigned_64;
   --  Item's contents in a block of at least Bytes (Item itself when it
   --  already holds them); No_Address on failure, Item then unchanged.
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64;

   --  What the heap holds from the provider, for diagnostics and tests.
   function Arena_Count return Natural;
   function Reserved_Bytes return Unsigned_64;
   function Committed_Bytes return Unsigned_64;
end CuAlloc;
