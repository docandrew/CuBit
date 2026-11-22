with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Extent_Directory;
with Intel_GPU_Buffer_Backing;
package Intel_GPU_Extent_Replies with SPARK_Mode is
   package E renames Intel_GPU_Physical_Extents;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   -- Four-word supervisor reply: index, DMA address, CPU address, arena ID.
   -- The transport must authenticate endpoint/incarnation and completion token
   -- before each Accept_Reply. Arena ID is correlation, never authority.
   type Assembly is limited private;
   procedure Start
     (Object : in out Assembly; CPU_Base, Arena_ID : Unsigned_64;
      Success : out Boolean; Required_Blocks : Natural :=
        Natural (Intel_GPU_Buffer_Backing.Default_Heap.Byte_Quota / E.Block_Bytes);
      Byte_Quota : Unsigned_64 := Intel_GPU_Buffer_Backing.Default_Heap.Byte_Quota;
      DMA_Limit : Unsigned_64 := Intel_GPU_Buffer_Backing.Default_Heap.DMA_Limit);
   -- Quota/address width come from trusted device policy, never reply words.
   function Metadata_Capacity (Object : Assembly) return Positive;
   procedure Extend_Metadata
     (Object : in out Assembly; Base, Bytes : Unsigned_64; Success : out Boolean);
   -- Serialized, retained disjoint CPU metadata. Caller must grow before
   -- accepting replies beyond capacity; this does not request physical backing.
   procedure Extend
     (Object : in out Assembly; Required_Blocks : Natural; Success : out Boolean);
   -- Only a completed snapshot may extend. Retain previously validated bases;
   -- request just the missing suffix. Invalid extension is terminal.
   procedure Accept_Reply
     (Object : in out Assembly; Data : Words; Success : out Boolean);
   procedure Cancel (Object : in out Assembly);
   function Result (Object : Assembly)
      return Intel_GPU_Extent_Directory.Borrowed_View;
   -- Assembly is limited and retained for the service incarnation; it and
   -- its metadata must outlive all buffers built from this internal view.
   -- No partial map escapes. Failure/cancellation is terminal for this object;
   -- backing is retained by the supervisor, not freed by this decoder.
private
   type Assembly is limited record
      Started, Broken : Boolean := False;
      CPU, Identity : Unsigned_64 := 0;
      Count, Wanted, Limit : Natural := 0;
      DMA_Limit : Unsigned_64 := 0;
      Directory : aliased Intel_GPU_Extent_Directory.Directory;
   end record;
end Intel_GPU_Extent_Replies;
