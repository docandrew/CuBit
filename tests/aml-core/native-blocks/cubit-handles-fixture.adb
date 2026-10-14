package body CuBit.Handles.Fixture is
   --  A small number N is the slot of a handle of generation 1.
   Generation_One : constant Interfaces.Unsigned_64 := 2 ** 32;
   function Endpoint_Of (Word : Interfaces.Unsigned_64) return Endpoint is
     ((Word => (if Word in 1 .. Generation_One - 1 then Word + Generation_One else Word)));
   function Word_Of (Item : Endpoint) return Interfaces.Unsigned_64 is (Item.Word);
end CuBit.Handles.Fixture;
