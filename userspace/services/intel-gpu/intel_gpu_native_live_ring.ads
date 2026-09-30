with Interfaces;
with Intel_GPU_ADLN_Context_Init;
generic
   -- Exact retained backing identity, initial publication and enabled context.
   with function Owner_Ready return Boolean;
   -- Explicit ADL-N LLC/system-memory coherent WB saved-context mapping.
   -- Not a generic assertion that all Intel GPUs share this property.
   with function Coherent_Ready return Boolean;
   with procedure Read_Marker (Value : out Interfaces.Unsigned_64;
                               OK : out Boolean);
package Intel_GPU_Native_Live_Ring is
   procedure Append (Segment : Intel_GPU_ADLN_Context_Init.Segment;
                     Success : out Boolean);
   procedure Fail;
   function Tail return Interfaces.Unsigned_32;
   function Sequence return Interfaces.Unsigned_32;
   -- Best-effort saved memory snapshot, NOT live engine MMIO or an atomic
   -- head/tail pair. May lag until the GPU saves context. No state writes.
   procedure Read_Saved_Pointers
     (Head, Tail : out Interfaces.Unsigned_32; OK : out Boolean);
end Intel_GPU_Native_Live_Ring;
