with CuBit.Messages; use CuBit.Messages;
package body CuBit.Memory_Grants is
   procedure Create_Via_Capability
     (slot : CapabilitySlot; localAddr : System.Address; numPages : Natural;
      readWrite : Boolean; reference : out Grant_Reference; success : out Boolean) is
      use type System.Address;
   begin
      pragma Assert (slot = CAP_SLOT_CONFIG and localAddr = Storage'Address and numPages = 2 and readWrite);
      reference := (8, 9);
      success := True;
   end Create_Via_Capability;
end CuBit.Memory_Grants;
