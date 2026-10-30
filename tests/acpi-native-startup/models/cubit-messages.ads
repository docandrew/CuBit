with Interfaces;
with CuBit.Handles;
-- Runtime model, not an ABI mirror. Actual API is checked by native-entry.
package CuBit.Messages is
   DRIVER_ACPI : constant Interfaces.Unsigned_64 := 29;
   type Process_ID is private;
   type Message is null record;
   function registerDriver
     (Role : Interfaces.Unsigned_64; Authority : CuBit.Handles.Notification)
      return Interfaces.Unsigned_64;
private
   type Process_ID is new Interfaces.Unsigned_64;
end CuBit.Messages;
