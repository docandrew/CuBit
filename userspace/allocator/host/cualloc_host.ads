------------------------------------------------------------------------------
--  CuAlloc on Linux, for hosted tests and benchmarks: the same C entry
--  points as CuAlloc_Native (cualloc_allocate and the rest), over Linux
--  memory (Linux_Provider), plus a quota hook so tests can run the heap out
--  of memory.
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
   function Usable_Size (Item : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_usable_size";
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_reallocate";
   --  Bound the bytes the provider will back (Unsigned_64'Last: none).
   procedure Set_Quota (Bytes : Unsigned_64)
     with Export, Convention => C, External_Name => "cualloc_test_set_quota";
   --  What the provider backs now.
   function Committed return Unsigned_64
     with Export, Convention => C, External_Name => "cualloc_test_committed";
end CuAlloc_Host;
