with CuBit.Control_Events;
with Interfaces;
-- Process-lifetime typed logger, serialized with the ACPI owner loop.
package ACPI_Native_Log is
   procedure Info (Text : String);
   procedure Warning (Text : String);
   procedure Critical (Text : String);
   -- Copied observations may use the established publisher ring even while
   -- hardware is uncertain: append/shed plus a non-blocking wakeup only.
   -- Opening the channel may block and is forbidden unless Peer_Quiet.
   procedure Copied (Text : String; Peer_Quiet : Boolean; Success : Boolean := False);
   function Copied_Dropped return Interfaces.Unsigned_64;
   -- Existing publisher loss includes ordinary plain ring submissions.
   function Publisher_Dropped return Interfaces.Unsigned_64;
   -- Diagnostic barrier only: call after AML stopped, never with unresolved IO.
   -- Uses the existing bounded logger Flush; no retry of a dropped publication.
   procedure Drain_Diagnostics (IO_Pending, IO_Uncertain : Boolean; Drained : out Boolean);
   -- Kernel-validated event path only. Only matching full-width logstore peer
   -- notices disconnect this publisher; pages remain process-lived afterward.
   procedure Grant_Notice (Item : CuBit.Control_Events.Event);
end ACPI_Native_Log;
