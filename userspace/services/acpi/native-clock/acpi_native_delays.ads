pragma Ada_2022;
with Interfaces;
with AML_Delays;
with AML_Events;
-- Trusted process-lifetime delay domain. The current native observer closes
-- startup before registration. An event controller may instead transfer a
-- successful startup to runtime, then supply one finite window per event.
-- No wire operation or AML operand can open/replenish these deadlines.
package ACPI_Native_Delays is
   procedure Begin_Startup (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean);
   -- Local startup controller only. Caller must establish physical Field/EC
   -- quiet and preserve all owners across IPC; this time gate alone does not.
   -- Resume uses the same original startup deadline, never a new allowance.
   procedure Pause_Startup (Accepted : out Boolean);
   procedure Resume_Startup (Accepted : out Boolean);
   procedure Inspect_Paused_Startup
      (Deadline : out Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure Enter_Runtime (Accepted : out Boolean);
   procedure Begin_Event (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure End_Event (Accepted : out Boolean);
   -- Trusted local observation only, using this owner's actual microsecond
   -- clock. Returns the original active deadline or zero on refusal. It grants
   -- no hardware authority and never opens or extends an event window.
   procedure Inspect_Window (Deadline : out Interfaces.Unsigned_64; Accepted : out Boolean);
   -- Terminal in every phase; does not cancel an outstanding wait.
   procedure Close_Startup;
   procedure Perform (Item : AML_Delays.Request; Result : out AML_Delays.Outcome);
   -- Finite event timeout through the same original delay domain. No signal
   -- delivery or new time allowance is implied; poll/forever are refused here.
   procedure Wait_Event_Timeout
     (Milliseconds : AML_Events.Timeout; Outcome : out AML_Events.Timeout_Outcome);
end ACPI_Native_Delays;
