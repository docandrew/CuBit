pragma Ada_2022;
with Interfaces;
with ACPI_PCI_Bus_Geometry;
with ACPI_PCI_Protocol;
package ACPI_Local_PCI_Results with Pure, SPARK_Mode is
   subtype Word is Interfaces.Unsigned_64;
   type Window_Description is record
      Status : ACPI_PCI_Protocol.Outcome := ACPI_PCI_Protocol.Unavailable;
      Epoch,ID : Word := 0;
      Geometry : ACPI_PCI_Bus_Geometry.Bus_Window;
   end record;
   type Outcome is (Read_Complete, Invalid_Geometry, Invalid_Mapping, Guard_Refused,
      Scope_Refused, Stale, Unknown_Record, Not_Ready, Reentered, Stopped,
      Clock_Unavailable, Clock_Reversed, Deadline_Expired, Backing_Unavailable,
      Completion_Unknown);
   type Result is record
      Status : Outcome := Not_Ready;
      Value : Word := 0;
   end record;
end ACPI_Local_PCI_Results;
