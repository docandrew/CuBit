with Interfaces; use Interfaces;
with CuBit.Messages;
package CuBit.Capability_Grants is
 type Recipient is record Identity : Unsigned_64 := 0; end record;
 function Capture (Slot : CuBit.Messages.CapabilitySlot) return Recipient;
 function Incarnation (Target : Recipient) return Unsigned_64;
 function Endpoint_Matches (Slot : CuBit.Messages.CapabilitySlot; Identity : Unsigned_64) return Boolean;
end CuBit.Capability_Grants;
