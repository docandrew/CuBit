------------------------------------------------------------------------------
--  CuAlloc on Linux, for hosted tests and benchmarks: the same C entry
--  points as CuAlloc_Native (cualloc_allocate and the rest), over Linux
--  memory (Linux_Provider), plus a quota hook so tests can run the heap out
--  of memory. A thread's heap is left for adoption when the thread ends
--  (a pthread key destructor calls Thread_Exit).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuAlloc_Host is
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
   procedure Thread_Exit
     with Export, Convention => C, External_Name => "cualloc_thread_exit";
   --  Bound the bytes the provider will back (Unsigned_64'Last: none).
   procedure Set_Quota (Bytes : Unsigned_64)
     with Export, Convention => C, External_Name => "cualloc_test_set_quota";
   --  What the provider backs now.
   function Committed return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_test_committed";
   --  The heap's own view: live arenas and huge blocks, reserved and backed
   --  bytes (C: three uint64_t pointers), for benchmarks' retained-memory
   --  columns.
   procedure Statistics (Arenas, Reserved, Committed : out Unsigned_64)
     with Export, Convention => C, External_Name => "cualloc_test_statistics";
end CuAlloc_Host;
