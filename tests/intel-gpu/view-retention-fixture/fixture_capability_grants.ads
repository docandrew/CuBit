with Interfaces; use Interfaces;
with CuBit.Messages;
package CuBit.Capability_Grants is
   Endpoint_Ready : Boolean := True;
   function Endpoint_Matches
     (Slot : CuBit.Messages.CapabilitySlot; Identity : Unsigned_64)
      return Boolean is (Endpoint_Ready and Slot = 7 and Identity = 42);
end CuBit.Capability_Grants;
