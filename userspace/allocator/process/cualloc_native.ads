------------------------------------------------------------------------------
--  CuAlloc on CuBit: the one process heap, over the kernel's growable owned
--  memory (reserve, commit an exact prefix, release). Its C entry points are
--  what every adapter calls: the libc malloc family (CuBit.Libc_Memory),
--  Ada's System.Memory, and Rust's GlobalAlloc. The same objects are in
--  libc.a, libgnat-user.a and libcualloc.a; a link pulls one copy, so a
--  process mixing languages has one heap.
--
--  Each thread holds its own heap (CuAlloc); a thread's exit hands it on.
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
   --  Free given the block's size and alignment (Rust's GlobalAlloc dealloc).
   procedure Free_Sized (Item, Bytes, Alignment : Unsigned_64)
     with Export, Convention => C, External_Name => "cualloc_free_sized";
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
