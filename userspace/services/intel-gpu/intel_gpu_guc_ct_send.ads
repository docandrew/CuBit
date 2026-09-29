with Interfaces;
with Intel_GPU_GuC_CTB;
generic
   -- Serialized, bounded, nonraising callbacks over one retained registered
   -- H2G ring. No callback may reenter the channel. Descriptor read includes
   -- reserved-word validation and native acquire ordering.
   with procedure Read_Descriptor
     (Head, Tail, Status : out Interfaces.Unsigned_32; Success : out Boolean);
   with procedure Write_Word
     (Index, Value : Interfaces.Unsigned_32; Success : out Boolean);
   -- Flush/order prior writes for firmware visibility (not just a compiler fence).
   with procedure Make_Visible (Success : out Boolean);
   with procedure Write_Tail (Value : Interfaces.Unsigned_32; Success : out Boolean);
   with procedure Notify (Success : out Boolean);
package Intel_GPU_GuC_CT_Send is
   type Words is array (Natural range <>) of Interfaces.Unsigned_32;
   type Phase is (Uninitialized, Active, Broken);
   type Channel is limited private;
   function State (Object : Channel) return Phase;
   -- One initialization attempt. Registered is trusted local evidence of
   -- authenticated firmware and successful ring registration, not an IPC flag.
   procedure Initialize
     (Object : in out Channel; Registered : Boolean;
      Size : Intel_GPU_GuC_CTB.Ring_Size; Initial_Tail : Interfaces.Unsigned_32);
   type Result is (Rejected, Would_Block, Corrupt, Quarantined, Queued);
   -- Payload includes an independently validated HXG header. Fence uniqueness,
   -- response credits and lifetime are caller obligations. Queued does not
   -- mean acknowledged, executed or completed. Retain backing on Broken.
   procedure Send
     (Object : in out Channel; Payload : Words; Fence : Interfaces.Unsigned_16;
      Status : out Result);
private
   type Channel is limited record
      Value : Phase := Uninitialized;
      Size : Intel_GPU_GuC_CTB.Ring_Size := 2;
      Tail : Interfaces.Unsigned_32 := 0;
   end record;
end Intel_GPU_GuC_CT_Send;
