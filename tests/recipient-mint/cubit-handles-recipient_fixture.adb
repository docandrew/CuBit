package body CuBit.Handles.Recipient_Fixture is
   function Endpoint_Word (Word : Unsigned_64) return Endpoint is ((Word => Word));
   function Process_Word (Word : Unsigned_64) return Process is ((Word => Word));
   function Cspace_Word (Word : Unsigned_64) return Cspace is ((Word => Word));
   function Word_Of (Item : Endpoint) return Unsigned_64 is (Item.Word);
   function Word_Of (Item : Foreign_Handle) return Unsigned_64 is (Item.Word);
   function Words_Of (Copy : Derivation) return ABI.Derivation is (Copy.Words);
end CuBit.Handles.Recipient_Fixture;
