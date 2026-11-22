package body CuBit.Handles.Fixture is
   function IO_Ports_Of (Word : Unsigned_64) return IO_Ports is ((Word => Word));
   function Notification_Of (Word : Unsigned_64) return Notification is ((Word => Word));
   function Interrupt_Of (Word : Unsigned_64) return Interrupt is ((Word => Word));
   function Endpoint_Of (Word : Unsigned_64) return Endpoint is ((Word => Word));
   function Word_Of (Item : IO_Ports) return Unsigned_64 is (Item.Word);
   function Word_Of (Item : Notification) return Unsigned_64 is (Item.Word);
end CuBit.Handles.Fixture;
