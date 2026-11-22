with CuBit.Handles;
with Control;
package body CuBit.Capability_Grants is
 function Capture (Slot : CuBit.Handles.Endpoint) return Recipient is ((Identity => Control.Identity));
 function Incarnation (Target : Recipient) return CuBit.Process_IDs.Process_ID is
  (CuBit.Process_IDs.From_Word (Target.Identity));
 function Endpoint_Matches (Slot : CuBit.Handles.Endpoint; Identity : CuBit.Process_IDs.Process_ID) return Boolean is
  (CuBit.Process_IDs.To_Word (Identity) /= 0 and CuBit.Process_IDs.To_Word (Identity) = Control.Identity);
end CuBit.Capability_Grants;
