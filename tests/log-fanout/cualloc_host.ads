with Interfaces; use Interfaces;

--  CuAlloc's two entry points (userspace/allocator) for this Linux-hosted
--  test, over the C heap: Source_Identities grows through them on CuBit.
package CuAlloc_Host is
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
   with Export, Convention => C, External_Name => "cualloc_allocate";
   procedure Free (Block : Unsigned_64)
   with Export, Convention => C, External_Name => "cualloc_free";
end CuAlloc_Host;
