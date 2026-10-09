with Control;
package body CuBit.Capability_Grants is
 function Capture (Slot : CuBit.Messages.CapabilitySlot) return Recipient is ((Identity => Control.Identity));
 function Incarnation (Target : Recipient) return Unsigned_64 is (Target.Identity);
 function Endpoint_Matches (Slot : CuBit.Messages.CapabilitySlot; Identity : Unsigned_64) return Boolean is
  (Identity /= 0 and Identity = Control.Identity);
end CuBit.Capability_Grants;
