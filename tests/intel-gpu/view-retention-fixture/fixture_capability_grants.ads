with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Process_IDs;
package CuBit.Capability_Grants is
   Endpoint_Ready : Boolean := True;
   function Endpoint_Matches
     (Slot : CuBit.Messages.CapabilitySlot; Identity : CuBit.Process_IDs.Process_ID)
      return Boolean is
     (Endpoint_Ready and Slot = 7 and CuBit.Process_IDs.To_Word (Identity) = 42);
end CuBit.Capability_Grants;
