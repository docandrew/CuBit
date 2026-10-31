package body CuBit.Handles.Block_Fixture is
   function Reply_Of (Word : Unsigned_64) return Reply is ((Word => Word));
   function Word_Of (Item : Reply) return Unsigned_64 is (Item.Word);
end CuBit.Handles.Block_Fixture;
