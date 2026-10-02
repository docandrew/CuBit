package body CuBit.Memory_Grants is
   procedure Acquire_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; reference : Grant_Reference;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean) is
   begin
      Acquisitions := Acquisitions + 1;
      Last_Slot := slot; Last_Reference := reference;
      Last_Length := byteLength; Last_Offset := byteOffset;
      Last_Access := requiredAccess;
      mappedAddress := Source; success := Acquire_OK;
   end Acquire_Via_Capability;
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean) is
   begin
      Returns := Returns + 1; Returned_Reference := reference;
      success := Return_OK;
   end Return_Acquisition;
end CuBit.Memory_Grants;
