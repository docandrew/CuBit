with CuBit.Control_Events;
-- Process-lifetime typed logger, serialized with the ACPI owner loop.
package ACPI_Native_Log is
   procedure Info (Text : String);
   procedure Warning (Text : String);
   procedure Critical (Text : String);
   -- Kernel-validated event path only. Only matching full-width logstore peer
   -- notices disconnect this publisher; pages remain process-lived afterward.
   procedure Grant_Notice (Item : CuBit.Control_Events.Event);
end ACPI_Native_Log;
