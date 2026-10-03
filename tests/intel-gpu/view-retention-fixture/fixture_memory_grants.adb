package body CuBit.Memory_Grants is
   procedure Create_Via_Capability
     (Slot : CuBit.Messages.CapabilitySlot; LocalAddr : System.Address;
      NumPages : Natural; ReadWrite : Boolean;
      Reference : out CuBit.Grant_References.Reference; Success : out Boolean) is
   begin
      Creates := Creates + 1;
      Reference := (Slot => 1, Generation => 1);
      Success := Create_OK;
   end Create_Via_Capability;
   procedure Create_Forwardable_Via_Capability
     (Slot : CuBit.Messages.CapabilitySlot; LocalAddr : System.Address;
      NumPages : Natural; ReadWrite : Boolean;
      Reference : out CuBit.Grant_References.Reference; Success : out Boolean) is
   begin
      pragma Assert (not ReadWrite);
      Create_Via_Capability (Slot, LocalAddr, NumPages, ReadWrite, Reference, Success);
   end Create_Forwardable_Via_Capability;
   procedure Revoke
     (Reference : CuBit.Grant_References.Reference; Success : out Boolean) is
   begin
      Revokes := Revokes + 1;
      Success := Revoke_OK;
   end Revoke;
   function Retirement_Confirmed (Reference : CuBit.Grant_References.Reference)
      return Boolean is (Gone);
end CuBit.Memory_Grants;
