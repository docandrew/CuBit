with CuBit.Handles;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Process_IDs;
package CuBit.Capability_Grants is
 type Recipient is record Identity : Unsigned_64 := 0; end record;
 function Capture (Slot : CuBit.Handles.Endpoint) return Recipient;
 function Incarnation (Target : Recipient) return CuBit.Process_IDs.Process_ID;
 function Endpoint_Matches (Slot : CuBit.Handles.Endpoint; Identity : CuBit.Process_IDs.Process_ID) return Boolean;
end CuBit.Capability_Grants;
