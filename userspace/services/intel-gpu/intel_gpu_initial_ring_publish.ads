with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Init;
generic
   -- True only while the caller owns an initialized, never-scheduled context
   -- and its retained ring. Caller serializes all access through completion.
   with function Exclusive_Ready return Boolean;
   -- Byte offsets relative to retained context image, not MMIO addresses.
   with procedure Write_32 (Offset, Value : Unsigned_32; OK : out Boolean);
   with procedure Read_32 (Offset : Unsigned_32; Value : out Unsigned_32;
                          OK : out Boolean);
   -- Must establish device visibility, not merely a compiler barrier.
   with function Flush (Offset, Bytes : Unsigned_32) return Boolean;
package Intel_GPU_Initial_Ring_Publish is
   type Result is (Rejected, Ownership_Lost, Read_Failed, Write_Failed,
                   Verify_Failed, Flush_Failed, Published);
   type Attempt is limited private;
   -- Publishes only the fixed initialization segment (256 bytes, 8-aligned).
   -- No GuC message, GPU execution, observed completion or retry is implied.
   -- Backing must remain retained on every result, including uncertainty
   -- after the tail store. Never use this on a live or previously run context.
   procedure Publish (Object : in out Attempt;
                      Segment : Intel_GPU_ADLN_Context_Init.Segment;
                      Status : out Result);
private
   type Attempt is limited record
      Used : Boolean := False;
   end record;
end Intel_GPU_Initial_Ring_Publish;
