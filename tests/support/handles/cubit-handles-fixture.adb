package body CuBit.Handles.Fixture is
   Generation_One : constant Unsigned_64 := 2 ** 32;
   function Endpoint_Of (Slot : Unsigned_32) return Endpoint is
     ((Word => Generation_One + Unsigned_64 (Slot)));
   function Reply_Of (Slot : Unsigned_32) return Reply is
     ((Word => Generation_One + Unsigned_64 (Slot)));
   function Device_Memory_Of (Slot : Unsigned_32) return Device_Memory is
     ((Word => Generation_One + Unsigned_64 (Slot)));
   function Word_Of (Item : Endpoint) return Unsigned_64 is (Item.Word);
   function Word_Of (Item : Reply) return Unsigned_64 is (Item.Word);
end CuBit.Handles.Fixture;
