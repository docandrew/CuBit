------------------------------------------------------------------------------
--  CuAlloc on CuBit: the one process heap, over the kernel's growable owned
--  memory (reserve, commit an exact prefix, release). Its C entry points are
--  what every adapter calls: the libc malloc family (CuBit.Libc_Memory),
--  Ada's System.Memory (CuAlloc_Gnat), and Rust's GlobalAlloc. The same
--  objects are in libc.a, libgnat-user.a and libcualloc.a; a link pulls one
--  copy, so a process mixing languages has one heap.
--
--  Each thread holds its own heap (CuAlloc); a thread's exit hands it on.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuAlloc_Native is
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_allocate", Inline;
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_allocate_zeroed", Inline;
   procedure Free (Item : Unsigned_64)
     with Export, Convention => C, External_Name => "cualloc_free", Inline;
   --  Free given the block's size and alignment (Rust's GlobalAlloc dealloc).
   procedure Free_Sized (Item, Bytes, Alignment : Unsigned_64)
     with Export, Convention => C, External_Name => "cualloc_free_sized", Inline;
   --  For adapters with their own failure handling (CuBit.Libc_Memory's
   --  errno): CuAlloc's fast path alone, inline (No_Address: not served,
   --  not a failure), and the rest of the allocation, out of line.
   function Allocate_Quick (Bytes, Alignment : Unsigned_64) return Unsigned_64 with Inline;
   --  Untouched: the block reads as zero already.
   function Allocate_Quick (Bytes, Alignment : Unsigned_64; Untouched : out Boolean) return Unsigned_64
     with Inline;
   function Allocate_Rest (Bytes, Alignment : Unsigned_64) return Unsigned_64;
   function Allocate_Zeroed_Rest (Bytes, Alignment : Unsigned_64) return Unsigned_64;
   function Reallocate_Quick (Item, Bytes : Unsigned_64) return Unsigned_64 with Inline;
   function Usable_Size (Item : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_usable_size";
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_reallocate";
   --  The calling thread ends: its heap is drained and left for the next
   --  thread to adopt. Every thread exit calls it (weakly linked): musl's
   --  (CuBit.Libc_System_Calls, __unmapself), Rust std's (moto-rt-cubit)
   --  and the user runtime's (CuBit.Thread_Areas.Exit_Thread).
   procedure Thread_Exit
     with Export, Convention => C, External_Name => "cualloc_thread_exit";
end CuAlloc_Native;
