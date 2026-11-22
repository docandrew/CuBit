with Interfaces;
package Intel_GPU_GuC_CTB with SPARK_Mode is
   use Interfaces;
   -- GuC CTB HXG framing; sizes/cursors are DWORDS, not bytes.
   -- ABI: Linux v6.16 gt/uc/abi/guc_communication_ctb_abi.h.
   -- Pure plans only. No shared memory, descriptor publication or doorbells.
   subtype Ring_Size is Unsigned_32 range 2 .. 65_536;
   type Result is (Invalid_Descriptor, Invalid_Message, Empty, Full,
                   Truncated, Ready);
   type Plan is record
      State : Result := Invalid_Descriptor;
      Start, Next_Cursor, Words : Unsigned_32 := 0;
      Fence : Unsigned_16 := 0;
   end record;
   function Header (Fence : Unsigned_16; Payload_Words : Unsigned_32)
     return Unsigned_32
   with Pre => Payload_Words in 1 .. 255;
   -- Local_Tail is the sender's retained cursor. A firmware-modified tail
   -- invalidates the channel. Leave one DWORD unused to distinguish full/empty.
   function Send (Size : Ring_Size; Head, Tail, Local_Tail, Status,
                  Payload_Words : Unsigned_32; Fence : Unsigned_16) return Plan
   with Post => (if Send'Result.State = Ready then
      Send'Result.Start < Size and Send'Result.Next_Cursor < Size and
      Send'Result.Words in 2 .. 256 and Send'Result.Words < Size);
   -- Read Frame_Header only after observing nonempty validated cursors with
   -- the native acquire/cache ordering. This function does not perform reads.
   -- Local_Head is receiver-owned. Unknown format/reserved bits fail closed.
   function Receive (Size : Ring_Size; Head, Tail, Local_Head, Status,
                     Frame_Header : Unsigned_32) return Plan
   with Post => (if Receive'Result.State = Ready then
      Receive'Result.Start < Size and Receive'Result.Next_Cursor < Size and
      Receive'Result.Words in 2 .. 256 and Receive'Result.Words < Size);
end Intel_GPU_GuC_CTB;
