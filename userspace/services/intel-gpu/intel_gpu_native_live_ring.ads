with Interfaces;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_ADLN_Barrier;
with Intel_GPU_Live_Ring_Publish;
generic
   -- Selected retained context mapping. Caller serializes selection with all
   -- operations. Each Channel binds the exact mapping on its first append.
   -- Backing may not be recycled/rebound to another context. Numerical CPU
   -- identity checks do not establish DMA authority or detect hidden aliases.
   with function CPU_Base return Interfaces.Unsigned_64;
   with function Backing_Bytes return Interfaces.Unsigned_64;
   -- Exact retained backing identity, initial publication, enabled context,
   -- and submission admission (including VM-update holds). Caller serializes
   -- this predicate with append/hold: blocking only Notify_Work is too late.
   with function Owner_Ready return Boolean;
   -- Explicit ADL-N LLC/system-memory coherent WB saved-context mapping.
   -- Not a generic assertion that all Intel GPUs share this property.
   with function Coherent_Ready return Boolean;
   with procedure Read_Marker (Value : out Interfaces.Unsigned_64;
                               OK : out Boolean);
package Intel_GPU_Native_Live_Ring is
   type Channel is limited private;
   procedure Append (Object : in out Channel; Segment : Intel_GPU_ADLN_Barrier.Segment;
                     Success : out Boolean);
   procedure Append (Object : in out Channel; Segment : Intel_GPU_ADLN_Context_Init.Segment;
                     Success : out Boolean);
   procedure Fail (Object : in out Channel);
   function Tail (Object : Channel) return Interfaces.Unsigned_32;
   function Sequence (Object : Channel) return Interfaces.Unsigned_32;
   -- Best-effort saved memory snapshot, NOT live engine MMIO or an atomic
   -- head/tail pair. May lag until the GPU saves context. No state writes.
   procedure Read_Saved_Pointers
     (Object : Channel; Head, Tail : out Interfaces.Unsigned_32; OK : out Boolean);
private
   function Owned return Boolean;
   procedure Load_Tail (Value : out Interfaces.Unsigned_32; OK : out Boolean);
   procedure Store_Word (Offset, Value : Interfaces.Unsigned_32; OK : out Boolean);
   function Publish_Words (Offset, Bytes : Interfaces.Unsigned_32) return Boolean;
   procedure Store_Tail (Value : Interfaces.Unsigned_32; OK : out Boolean);
   function Visible return Boolean;
   package Writer is new Intel_GPU_Live_Ring_Publish
     (Owned, Read_Marker, Load_Tail, Store_Word, Publish_Words, Store_Tail, Visible);
   type Channel is limited record
      Inner : Writer.Channel;
      Bound : Boolean := False;
      Base, Bytes : Interfaces.Unsigned_64 := 0;
   end record;
end Intel_GPU_Native_Live_Ring;
