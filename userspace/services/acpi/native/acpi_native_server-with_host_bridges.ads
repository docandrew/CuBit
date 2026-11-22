pragma Ada_2022;
with ACPI_Host_Bridge_Owners;
generic
   with package Bridges is new ACPI_Host_Bridge_Owners.Managers (<>);
   with procedure Maybe_Start (Server : in out ACPI_Requests.State);
   with procedure Advance_Normal (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   with procedure Advance_Power_Only (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   with function Power_Blocks_Runtime return Boolean;
   with procedure Maintain_Normal (Deadline : out Interfaces.Unsigned_64; May_Continue : out Boolean);
   with function Diagnostics_Normal (Server : ACPI_Requests.State) return Boolean;
   --  Maintain hardware and deadlines while this manager exclusively holds AML.
   --  Normal maintenance may require an idle runtime and cannot serve this role.
   with procedure Maintain_Exclusive (Deadline : out Interfaces.Unsigned_64; May_Continue : out Boolean);
-- Reuse the existing service loop. Maybe_Start captures finite original
-- deadlines once, at the real quiet boundary, from trusted provisioning.
package ACPI_Native_Server.With_Host_Bridges is
   --  A retained publication alone does not close ordinary request admission.
   --  A retained AML window still excludes all other AML work.
   function Admission_Open return Boolean;
   -- Startup-only serialized step: no EC/event/passive/power callbacks.
   -- The caller must still establish metadata/query readiness, preserve the
   -- original deadlines, process completions/notices and honor Stop.
   procedure Advance_Publication (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   procedure Drain (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   procedure Maintain (Deadline : out Interfaces.Unsigned_64; May_Continue : out Boolean);
   function Diagnostics_Quiet (Server : ACPI_Requests.State) return Boolean;
   procedure Handle_Completion (Server : in out ACPI_Requests.State;
      Item : CuBit.Messages.CompletionEntry; Handled : out Boolean);
end ACPI_Native_Server.With_Host_Bridges;
