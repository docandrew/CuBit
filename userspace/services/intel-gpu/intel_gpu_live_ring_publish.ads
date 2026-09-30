with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Init;
generic
   -- Single retained ADL-N context, first segment already published, context
   -- enabled, system-memory backing and coherent saved-tail access. Caller
   -- serializes Append, notification and completion; no reentrant callbacks.
   with function Owner_Ready return Boolean;
   with procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean);
   with procedure Read_Tail (Value : out Unsigned_32; OK : out Boolean);
   -- Ring-relative byte offset. No context-state or HWSP writes permitted.
   with procedure Write_Word (Offset, Value : Unsigned_32; OK : out Boolean);
   with function Publish_Words (Offset, Bytes : Unsigned_32) return Boolean;
   -- Exactly the saved tail DWORD, not a context image copy or head update.
   with procedure Write_Tail (Value : Unsigned_32; OK : out Boolean);
   with function Tail_Visible return Boolean;
package Intel_GPU_Live_Ring_Publish with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   Segment_Bytes : constant Unsigned_32 :=
     Intel_GPU_ADLN_Context_Init.Command_Words'Length * 4;
   Ring_Bytes : constant Unsigned_32 := 16 * 1024;
   Guard_Bytes : constant Unsigned_32 := 64;
   type Phase is (Available, Quarantined);
   type Result is (Rejected, Full, Ownership_Lost, Read_Failed,
                   Prior_Not_Complete, Tail_Mismatch, Write_Failed,
                   Visibility_Failed, Published);
   type Channel is limited private;
   function State (Object : Channel) return Phase;
   function Tail (Object : Channel) return Unsigned_32
     with Post => Tail'Result in Segment_Bytes .. Ring_Bytes - Guard_Bytes;
   function Sequence (Object : Channel) return Unsigned_32;
   procedure Fail (Object : in out Channel)
     with Post => State (Object) = Quarantined and
       Tail (Object) = Tail (Object)'Old and Sequence (Object) = Sequence (Object)'Old;
   -- Appends the next segment after the fixed initial segment. Sequence is
   -- allocated here, not supplied by an IPC client. Segment must contain its
   -- successor marker. Checks previous GPU completion and saved tail first.
   -- No wrap/recycling yet: Full leaves backing retained and performs no IO.
   -- Published is NOT notification or completion. Any uncertain callback
   -- quarantines permanently, including failure after the saved tail store.
   procedure Append (Object : in out Channel;
                     Segment : Intel_GPU_ADLN_Context_Init.Segment;
                     Status : out Result)
     with Post =>
       (if State (Object)'Old = Quarantined then
          State (Object) = Quarantined and Status = Rejected) and then
       (if Status = Published then
          State (Object) = Available and
          Tail (Object) = Tail (Object)'Old + Segment_Bytes and
          Sequence (Object)'Old < Unsigned_32'Last and
          Sequence (Object) = Sequence (Object)'Old + 1
        else Tail (Object) = Tail (Object)'Old and
          Sequence (Object) = Sequence (Object)'Old) and then
       (if Status not in Rejected | Full | Published then
          State (Object) = Quarantined);
private
   subtype Ring_Tail is Unsigned_32 range Segment_Bytes .. Ring_Bytes - Guard_Bytes
     with Dynamic_Predicate => Ring_Tail mod 8 = 0;
   type Channel is limited record
      Value : Phase := Available;
      Current_Tail : Ring_Tail := Segment_Bytes;
      Current_Sequence : Unsigned_32 := 1;
   end record;
end Intel_GPU_Live_Ring_Publish;
