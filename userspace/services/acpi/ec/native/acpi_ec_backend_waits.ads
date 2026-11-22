pragma Ada_2022;
with ACPI_EC_Types;
with ACPI_EC_Registers;
with ACPI_EC_Backend_Progress;
with ACPI_EC_Clock_Policy;
--  Serialized backend owner. Pause returns normally,does not reenter/change
--  controller ownership,and consumes exactly one positive-budget attempt.
generic
   with package Registers is new ACPI_EC_Registers (<>);
   with package Progress is new ACPI_EC_Backend_Progress (Registers => Registers, others => <>);
   with procedure Pause
     (Target : ACPI_EC_Types.Microseconds;
      Remaining : in out ACPI_EC_Types.Work_Budget;
      Result : out ACPI_EC_Clock_Policy.Pause_Report);
package ACPI_EC_Backend_Waits is
   type Report is record
      Progress_Result : Progress.Report;
      Wait_Checked, Stop_Requested : Boolean := False;
      Pause_Result : ACPI_EC_Clock_Policy.Pause_Report;
   end record;
   --  At most one bounded pause and one owning adapter step. A returned pause
   --  is never assumed to have reached its target. Even a stopped clock gets
   --  an engine clock/budget check,preventing perpetual advisory-wait deferral.
   --  Pause exhaustion/refusal/clock failure requests stop without claiming
   --  cancellation: postwrite work may need draining or remain in recovery.
   --  Preserve Remaining through this operation; reset only for new admitted
   --  work. IPC draining,wait-forever idle and lease supervision remain outside.
   procedure Advance
     (Now : ACPI_EC_Types.Microseconds;
      Remaining : in out ACPI_EC_Types.Work_Budget; Result : out Report);
end ACPI_EC_Backend_Waits;
