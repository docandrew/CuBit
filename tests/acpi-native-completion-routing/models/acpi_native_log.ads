with CuBit.Control_Events;
with CuBit.Process_IDs;
package ACPI_Native_Log is
   Peer : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
   Notices, Disconnects : Natural := 0;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event);
   procedure Info (Text : String);
   procedure Warning (Text : String);
   procedure Critical (Text : String);
end ACPI_Native_Log;
