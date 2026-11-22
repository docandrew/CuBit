------------------------------------------------------------------------------
--  CuAlloc on CuBit: the one process heap, over the kernel's growable owned
--  memory (reserve, commit an exact prefix, release). Its C entry points are
--  what every adapter calls: the libc malloc family (CuBit.Libc_Memory),
--  Ada's System.Memory, and Rust's GlobalAlloc. The same objects are in
--  libc.a, libgnat-user.a and libcualloc.a; a link pulls one copy, so a
--  process mixing languages has one heap.
--
--  A spin lock serializes the heap (a process may run threads); per-thread
--  caches come with tuning (docs/userspace-allocator.md, stage D).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuAlloc_Native is
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_allocate";
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_allocate_zeroed";
   procedure Free (Item : Unsigned_64)
     with Export, Convention => C, External_Name => "cualloc_free";
   function Usable_Size (Item : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_usable_size";
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_reallocate";
end CuAlloc_Native;
