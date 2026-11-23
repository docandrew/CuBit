------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  power.svc as the power-source aggregator (PWR-004 slice 1): it watches
--  acpi.svc's power sources, reads them when they change
--  (Power_ACPI_Sources: values, mapped, named by their ACPI identity) and
--  serves the snapshot, read only, to its observers (CuBit.Power_Sources:
--  Desktop's Battery item, the CCL console's power.sources).
--
--  @description
--  When the snapshot is read, and nothing else: there is no timer and no
--  poll of a source.
--    * acpi.svc's power-source watch (Power_ACPI_Watch) answers Changed:
--      a Notify on a battery or the adapter, a completed publication (the
--      first one once acpi.svc has started), a changed sample, a failure;
--    * an observer asks Read_Now while acpi.svc serves its sources: the
--      answer waits for one demand read (acpi.svc samples _BST again when
--      its cached sample is older than reading_age_ms, at most once a
--      second; a younger one is answered at once), then the reading.
--  Each reading starts with that demand read, submitted (never waited
--  on); the reading itself follows its completion, so acpi.svc has
--  nothing left to sample and answers each page at once.
--
--  acpi.svc not serving yet is not an end: the watch is submitted at start
--  (acpi.svc takes it when it serves); one answered Unavailable is
--  submitted again after Power_ACPI_Watch.Unavailable_Retry_Milliseconds;
--  and a power.svc started before acpi.svc is introduced to it late
--  (CuBit.Introductions, the manifest's optional "acpi-observer"): the
--  watch goes out when the endpoint arrives. Until acpi.svc first answers
--  the watch, Read_Now is answered at once with the snapshot as it is
--  (none yet). A reading acpi.svc did not answer in full keeps the last
--  snapshot, marked Stale.
--
--  The generation moves on only when the snapshot changes, and every kept
--  Watch that has not seen it is answered then. Every change is one log
--  line per source (logstore).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Handles.Startup;

package Power_Source_Service is
   --  Observer: acpi.svc's observer endpoint (absent when acpi.svc did not
   --  run at power.svc's start: introduced later when it starts, or never
   --  on profiles without acpi.svc). Maximum_Age_Ms: how old a sample may
   --  be and still be Fresh (Power_Parameters.reading_age_ms).
   procedure Start (Observer : CuBit.Handles.Startup.Optional_Endpoint; Maximum_Age_Ms : Unsigned_64);

   --  The completions of power.svc's own submits to acpi.svc for the
   --  sources (the watch, the demand read).
   function Handles_Completion (Token : Unsigned_64) return Boolean;
   procedure Collect (Completion : CuBit.Messages.CompletionEntry);

   --  When Advance must run (monotonic milliseconds; 0: never): a watch
   --  answered Unavailable is submitted again then. Advance also takes an
   --  endpoint procmgr introduced late (on every wake of the main loop).
   function Next_Due return Unsigned_64;
   procedure Advance (Now : Unsigned_64);

   --  Summary and source requests (CuBit.Power_Sources labels).
   function Handles (Label : Unsigned_32) return Boolean;
   procedure Serve (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message);
end Power_Source_Service;
