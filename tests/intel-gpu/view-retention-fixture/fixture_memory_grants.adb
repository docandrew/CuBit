package body CuBit.Memory_Grants is
   use type Interfaces.Unsigned_64;
   procedure Create_Via_Capability
     (Slot : CuBit.Messages.CapabilitySlot; LocalAddr : System.Address;
      NumPages : Natural; ReadWrite : Boolean;
      Reference : out CuBit.Grant_References.Reference; Success : out Boolean) is
   begin
      Creates := Creates + 1;
      Last_Writable := ReadWrite;
      Reference := (Slot => 1, Generation => Interfaces.Unsigned_64 (Creates));
      Success := Create_OK;
   end Create_Via_Capability;
   procedure Create_Forwardable_Via_Capability
     (Slot : CuBit.Messages.CapabilitySlot; LocalAddr : System.Address;
      NumPages : Natural; ReadWrite : Boolean;
      Reference : out CuBit.Grant_References.Reference; Success : out Boolean) is
   begin
      pragma Assert (not ReadWrite);
      Forwardable_Creates := Forwardable_Creates + 1;
      Create_Via_Capability (Slot, LocalAddr, NumPages, ReadWrite, Reference, Success);
   end Create_Forwardable_Via_Capability;
   procedure Revoke
     (Reference : CuBit.Grant_References.Reference; Success : out Boolean) is
   begin
      Revokes := Revokes + 1;
      Success := Revoke_OK;
   end Revoke;
   function Retirement_Confirmed (Reference : CuBit.Grant_References.Reference)
      return Boolean is
   begin
      Retirement_Queries := Retirement_Queries + 1;
      return Gone or else CuBit.Grant_References.Encode (Reference) = Completed_Wire;
   end Retirement_Confirmed;
end CuBit.Memory_Grants;
