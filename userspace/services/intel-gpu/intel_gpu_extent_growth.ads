with Interfaces; use Interfaces;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Buffer_Reply;
generic
   with package Allocator is new Intel_GPU_Extent_Allocator (<>);
   Pool : in out Allocator.Pool;
   with package Storage is new Intel_GPU_Metadata_Arena (<>);
   with function Owner_Ready return Boolean;
   Metadata_Bytes : Unsigned_64 := Intel_GPU_Buffer_Reply.Layout.Default_Heap.Metadata_Bytes;
package Intel_GPU_Extent_Growth is
   -- One retained adapter per stable pool/incarnation. Serialized saved-request
   -- caller must keep the same request while Pending. Metadata is independent
   -- from BO records and physical backing; it is never freed on failure.
   procedure Step
     (Arena_ID : Unsigned_64; Index : Positive;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count; Generation : Unsigned_32;
      Buffer : out Intel_GPU_Buffer_Reply.Extent_View; Success, Pending : out Boolean);
end Intel_GPU_Extent_Growth;
