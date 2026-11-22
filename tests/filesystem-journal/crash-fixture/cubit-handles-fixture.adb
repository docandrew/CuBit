package body CuBit.Handles.Fixture is
   --  A small number N is the slot of a handle of generation 1 (a handle
   --  word's generation is never zero, CuBit.Handles.Valid).
   Generation_One : constant Unsigned_64 := 2 ** 32;
   function Endpoint_Of (Word : Unsigned_64) return Endpoint is
     ((Word => (if Word in 1 .. Generation_One - 1 then Word + Generation_One else Word)));
   function Word_Of (Item : Endpoint) return Unsigned_64 is (Item.Word);
end CuBit.Handles.Fixture;
