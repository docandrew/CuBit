with Interfaces;
with CuBit.Handles;
with CuBit.Process_IDs;
-- Runtime model, not an ABI mirror. Actual API is checked by native-entry.
package CuBit.Messages is
   DRIVER_ACPI : constant Interfaces.Unsigned_64 := 29;
   subtype Process_ID is CuBit.Process_IDs.Process_ID;
   type Message is null record;
   function registerDriver
     (Role : Interfaces.Unsigned_64; Authority : CuBit.Handles.Notification)
      return Interfaces.Unsigned_64;
end CuBit.Messages;
