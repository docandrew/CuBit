package body CuBit.Capability_Grants is
   function Valid (Target : Recipient) return Boolean is (Target.Wire /= 0);
   function Incarnation (Target : Recipient) return CuBit.Process_IDs.Process_ID is
     (CuBit.Process_IDs.From_Word (Target.Wire));
   function Capture (Item : CuBit.Handles.Endpoint) return Recipient is
     ((Wire => (if CuBit.Handles.Valid (Item) then Endpoint_Identity else 0)));
   function Of_Identity (Word : Unsigned_64) return Recipient is ((Wire => Word));
end CuBit.Capability_Grants;
