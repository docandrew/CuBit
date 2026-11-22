with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Process_IDs;
--  CuBit.Capability_Grants for the launch client test: a captured endpoint
--  names the identity the test registered for its handle word.
package CuBit.Capability_Grants is
   type Recipient is private;
   function Valid (Target : Recipient) return Boolean;
   function Incarnation (Target : Recipient) return CuBit.Process_IDs.Process_ID;
   function Capture (Item : CuBit.Handles.Endpoint) return Recipient;
   function Of_Identity (Word : Unsigned_64) return Recipient;
   --  The identity Capture reports for any endpoint (0: none).
   Endpoint_Identity : Unsigned_64 := 0;
private
   type Recipient is record
      Wire : Unsigned_64 := 0;
   end record;
end CuBit.Capability_Grants;
