with CuBit.Control_Events;
-- Process-lifetime typed logger, serialized with the ACPI owner loop.
package ACPI_Native_Log is
   procedure Info (Text : String);
   procedure Warning (Text : String);
   procedure Critical (Text : String);
   -- Diagnostic barrier only: call after AML stopped, never with unresolved IO.
   -- Uses the existing bounded logger Flush; no retry of a dropped publication.
   procedure Drain_Diagnostics (IO_Pending, IO_Uncertain : Boolean; Drained : out Boolean);
   -- Kernel-validated event path only. Only matching full-width logstore peer
   -- notices disconnect this publisher; pages remain process-lived afterward.
   procedure Grant_Notice (Item : CuBit.Control_Events.Event);
end ACPI_Native_Log;
