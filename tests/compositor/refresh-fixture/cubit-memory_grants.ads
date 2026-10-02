with System;
with Interfaces; use Interfaces;
with CuBit.Messages;
package CuBit.Memory_Grants is
   type Grant_Reference is record
      slot, generation : Unsigned_64 := 0;
   end record;
   procedure Create_Via_Capability
     (slot : CuBit.Messages.CapabilitySlot; localAddr : System.Address;
      numPages : Natural; readWrite : Boolean;
      reference : out Grant_Reference; success : out Boolean);
end CuBit.Memory_Grants;
