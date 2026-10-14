package body CuBit.Handles.Fixture is
   function Endpoint_Of (Word : Unsigned_64) return Endpoint is ((Word => Word));
   function Reply_Of (Word : Unsigned_64) return Reply is ((Word => Word));
   function Word_Of (Item : Endpoint) return Unsigned_64 is (Item.Word);
   function Word_Of (Item : Reply) return Unsigned_64 is (Item.Word);
   function Source_Of (Copy : Derivation) return Unsigned_64 is (Copy.Words (ABI.Source));
   function Rights_Of (Copy : Derivation) return Unsigned_64 is (Copy.Words (ABI.Rights));
   function Tag_Of (Copy : Derivation) return Unsigned_64 is (Copy.Words (ABI.Tag));
end CuBit.Handles.Fixture;
