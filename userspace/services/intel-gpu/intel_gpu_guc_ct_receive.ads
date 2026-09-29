with Interfaces;
with Intel_GPU_GuC_CTB;
generic
   -- Serialized, bounded, nonraising callbacks. No reentry. Descriptor read
   -- validates reserved words and acquires firmware writes before ring reads.
   with procedure Read_Descriptor
     (Head, Tail, Status : out Interfaces.Unsigned_32; Success : out Boolean);
   with procedure Read_Word
     (Index : Interfaces.Unsigned_32; Value : out Interfaces.Unsigned_32;
      Success : out Boolean);
   -- Complete/order all preceding ring reads before releasing their storage.
   with procedure Finish_Reads (Success : out Boolean);
   with procedure Write_Head
     (Value : Interfaces.Unsigned_32; Success : out Boolean);
   -- Make the new descriptor head visible to firmware.
   with procedure Make_Visible (Success : out Boolean);
package Intel_GPU_GuC_CT_Receive is
   type Words is array (Positive range 1 .. 255) of Interfaces.Unsigned_32;
   type Message is record
      Length : Natural range 0 .. 255 := 0;
      Fence : Interfaces.Unsigned_16 := 0;
      Payload : Words := [others => 0];
   end record;
   type Phase is (Uninitialized, Active, Broken);
   type Channel is limited private;
   function State (Object : Channel) return Phase;
   -- Registered is trusted local evidence, not a caller-supplied IPC claim.
   -- Initialization is one-shot; broken channels retain their backing.
   procedure Initialize
     (Object : in out Channel; Registered : Boolean;
      Size : Intel_GPU_GuC_CTB.Ring_Size; Initial_Head : Interfaces.Unsigned_32);
   type Result is (Rejected, Empty, Corrupt, Quarantined, Received);
   -- Output is zero unless Received. It is then an owned copy, but HXG
   -- semantics/fence matching must still be validated before dispatch.
   -- A truncated published frame is corruption, not retryable backpressure.
   procedure Poll
     (Object : in out Channel; Output : out Message; Status : out Result);
private
   type Channel is limited record
      Value : Phase := Uninitialized;
      Size : Intel_GPU_GuC_CTB.Ring_Size := 2;
      Head : Interfaces.Unsigned_32 := 0;
   end record;
end Intel_GPU_GuC_CT_Receive;
