with System.Storage_Elements;
package body CuBit.Memory_Grants is
   use type System.Storage_Elements.Integer_Address;
   use type CuBit.Grant_References.Reference;
   procedure Acquire_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; reference : Grant_Reference;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean) is
   begin
      Calls := Calls + 1;
      pragma Assert (slot = Expected_Slot and reference = Expected_Reference);
      pragma Assert (byteOffset = Expected_Offset and byteLength = Expected_Bytes);
      pragma Assert (requiredAccess = Expected_Access);
      --  Deliberately return a nonzero address on failure: the FFI must clear it.
      mappedAddress := System.Storage_Elements.To_Address (16#7000_1234_5000#);
      success := Succeed;
   end Acquire_Via_Capability;
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean) is
   begin
      Returns := Returns + 1;
      pragma Assert (reference = Expected_Reference);
      success := Succeed;
   end Return_Acquisition;
   procedure Create_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean) is
   begin
      Creates := Creates + 1;
      pragma Assert (slot = (if Separate_Create_Expectations then Create_Slot else Expected_Slot));
      pragma Assert (System.Storage_Elements.To_Integer (localAddr) =
                     System.Storage_Elements.Integer_Address
                       (if Separate_Create_Expectations then Create_Offset else Expected_Offset));
      pragma Assert (Unsigned_64 (numPages) * 4096 = Expected_Bytes);
      pragma Assert (readWrite = (Expected_Access = Write_Access));
      reference := Expected_Reference;
      success := Succeed;
   end Create_Via_Capability;
   procedure Revoke (reference : Grant_Reference; success : out Boolean) is
   begin
      Revokes := Revokes + 1;
      pragma Assert (reference = Expected_Reference);
      success := Succeed;
   end Revoke;
   procedure Create_Forwardable_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean) is
   begin
      pragma Assert (not readWrite);
      Forwardable_Creates := Forwardable_Creates + 1;
      Create_Via_Capability (slot, localAddr, numPages, readWrite, reference, success);
   end Create_Forwardable_Via_Capability;
   function Retirement_Confirmed (reference : Grant_Reference) return Boolean is
   begin
      Polls := Polls + 1;
      pragma Assert (reference = Expected_Reference);
      return Gone;
   end Retirement_Confirmed;
end CuBit.Memory_Grants;
