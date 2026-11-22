pragma Ada_2022;
with ACPI_Startup_Timing; with AML_Work_Kinds;
-- Process-local startup diagnostics. No IPC, authority, deadlines or replay
-- decisions are changed. Enter/Leave sample the same monotonic microsecond
-- source; Report stops collection before calling the existing logger.
package ACPI_Native_Startup_Timing is
   package P renames ACPI_Startup_Timing;
   subtype Token is P.Token;
   procedure Start;
   procedure Enter (Item : P.Kind; Handle : out Token);
   procedure Leave (Handle : in out Token);
   -- Normally returning passive observer, with no hardware/logging/reentry.
   procedure Observe_Work (Item : AML_Work_Kinds.Kind; Entering : Boolean);
   function Snapshot (Item : P.Kind) return P.Counters;
   -- Passive counter copy only; no clock/logging or collection-state changes.
   procedure Report;
end ACPI_Native_Startup_Timing;
