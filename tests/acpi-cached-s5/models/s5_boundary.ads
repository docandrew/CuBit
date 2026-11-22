pragma Ada_2022;
with Interfaces; with ACPI_Sleep_Entry;
package S5_Boundary is
   Microseconds : Interfaces.Unsigned_64 := 1_000;
   Milliseconds : Interfaces.Unsigned_64 := 10;
   Clock_Known, Hardware_Ready, Fields_Quiet, EC_Quiet, Runtime_Idle : Boolean := True;
   Runtime_Stopped, Runtime_Fresh : Boolean := False;
   Saved_Allowed, Delivered : Boolean := True;
   Saves, Answers, Commands, Normal_Invocations, Windows : Natural := 0;
   Model : ACPI_Sleep_Entry.Register_Model := ACPI_Sleep_Entry.PM1A_Only;
   type Actions is array (Positive range 1 .. ACPI_Sleep_Entry.Maximum_Commands) of ACPI_Sleep_Entry.Command;
   Issued : Actions;
   Hardware_Result : ACPI_Sleep_Entry.Completion := ACPI_Sleep_Entry.Completed;
   Saved_Read : ACPI_Sleep_Entry.Control_Word := 16#0001#;
   Reply_Status : Interfaces.Unsigned_64 := 0;
end S5_Boundary;
