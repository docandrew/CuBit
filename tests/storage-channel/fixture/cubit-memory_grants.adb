with System.Storage_Elements;
package body CuBit.Memory_Grants is
   procedure Acquire_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; reference : Grant_Reference;
      byteOffset, byteLength : Unsigned_64; requiredAccess : Required_Access;
      mappedAddress : out System.Address; success : out Boolean) is
   begin
      pragma Assert (slot = Expected_Owner_Endpoint and reference.slot = 7 and reference.generation = 9);
      pragma Assert (byteOffset = 0 and byteLength =
        (if Expected_Transfer_Bytes = 0 then Unsigned_64 (Expected_Pages * 4096) else Expected_Transfer_Bytes));
      pragma Assert (Active_Acquisitions = 0);
      Acquisitions := Acquisitions + 1;
      pragma Assert (requiredAccess = (if Acquisitions = 1 then Read_Access else Write_Access));
      success := Acquisitions /= Deny_Acquisition;
      mappedAddress := (if success then Mapping else System.Null_Address);
      if success then Active_Acquisitions := Active_Acquisitions + 1; end if;
   end Acquire_Via_Capability;
   procedure Return_Acquisition (reference : Grant_Reference; success : out Boolean) is
      pragma Unreferenced (reference);
   begin
      pragma Assert (Active_Acquisitions = 1);
      Returns := Returns + 1;
      success := Returns /= Fail_Return;
      if success then Active_Acquisitions := Active_Acquisitions - 1; end if;
   end Return_Acquisition;
   procedure Create_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean)
   is
      use System.Storage_Elements;
      pragma Unreferenced (slot);
   begin
      pragma Assert (numPages = Expected_Pages and readWrite);
      pragma Assert (To_Integer (localAddr) mod 4096 = 0);
      Mapping := localAddr;
      reference := (slot => 7, generation => 9);
      success := Allow_Grant;
   end Create_Via_Capability;
   procedure Revoke (reference : Grant_Reference; success : out Boolean) is
      pragma Unreferenced (reference);
   begin
      Revocations := Revocations + 1;
      success := Allow_Revoke;
   end Revoke;
   function Retirement_Confirmed (reference : Grant_Reference) return Boolean is
      pragma Unreferenced (reference);
   begin
      return Is_Retired;
   end Retirement_Confirmed;
end CuBit.Memory_Grants;
