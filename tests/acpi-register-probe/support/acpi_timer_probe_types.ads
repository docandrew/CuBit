pragma Ada_2022;
with Interfaces;
with ACPI_Inventory_Protocol;
package ACPI_Timer_Probe_Types is
   subtype Time_Instant is ACPI_Inventory_Protocol.Wire.Time_Instant;
   type Descriptions is array (Positive range <>) of
     ACPI_Inventory_Protocol.Description;
   type Read_Status is (Completed, Rejected, Failed, Unknown);
   type Outcome is
     (Not_Started, Running, Metadata_Failed, Invalid_Inventory,
      Timer_Missing, Unsafe_Timer, Invalid_Deadline, Clock_Unavailable,
      Clock_Regressed, Deadline_Expired, Read_Rejected, Read_Failed,
      Read_Unknown, Invalid_Value, Counter_Unchanged, Counter_Changed);
   type Report is record
      Status : Outcome := Not_Started;
      Attempts, Completed_Reads : Natural := 0;
      First, Last : Interfaces.Unsigned_64 := 0;
   end record;
end ACPI_Timer_Probe_Types;
