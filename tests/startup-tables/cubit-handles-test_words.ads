--  Test only: the words behind handles, to check what the startup table
--  gave (tests/startup-tables). Never part of the runtime.
with Interfaces; use Interfaces;
package CuBit.Handles.Test_Words is
   function Word (Item : Endpoint) return Unsigned_64;
   function Word (Item : Process) return Unsigned_64;
   function Word (Item : Device_Memory) return Unsigned_64;
   function Word (Item : Interrupt) return Unsigned_64;
   function Word (Item : Notification) return Unsigned_64;
   function Word (Item : Scheduling) return Unsigned_64;
private
   function Word (Item : Endpoint) return Unsigned_64 is (Item.Word);
   function Word (Item : Process) return Unsigned_64 is (Item.Word);
   function Word (Item : Device_Memory) return Unsigned_64 is (Item.Word);
   function Word (Item : Interrupt) return Unsigned_64 is (Item.Word);
   function Word (Item : Notification) return Unsigned_64 is (Item.Word);
   function Word (Item : Scheduling) return Unsigned_64 is (Item.Word);
end CuBit.Handles.Test_Words;
