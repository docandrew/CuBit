--  The hosted tests' view of CuAlloc: its C entry points over Linux memory
--  (CuAlloc_Host), threads and all, and its statistics.
with Interfaces; use Interfaces;
with CuAlloc_Host;
package Heap is
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 renames CuAlloc_Host.Allocate;
   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64
     renames CuAlloc_Host.Allocate_Zeroed;
   procedure Free (Item : Unsigned_64) renames CuAlloc_Host.Free;
   procedure Free_Sized (Item, Bytes, Alignment : Unsigned_64) renames CuAlloc_Host.Free_Sized;
   function Usable_Size (Item : Unsigned_64) return Unsigned_64 renames CuAlloc_Host.Usable_Size;
   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64 renames CuAlloc_Host.Reallocate;
   procedure Thread_Exit renames CuAlloc_Host.Thread_Exit;
   --  Small/medium arenas plus huge blocks; reserved and backed bytes.
   function Arena_Count return Natural;
   function Reserved_Bytes return Unsigned_64;
   function Committed_Bytes return Unsigned_64;
end Heap;
