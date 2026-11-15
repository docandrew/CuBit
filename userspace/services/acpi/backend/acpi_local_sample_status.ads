pragma Ada_2022;
with Interfaces;
package ACPI_Local_Sample_Status with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   type Outcome is (Idle, Pending, Refused);
   type Notice is (Sample_Available, Sample_Missing, Sample_Failed,
      Publication_Refused, No_Device);
   type Failure is (None, Not_Ready, Wrong_Context, Clock_Unavailable,
      Clock_Reversed, Deadline_Overflow, Window_Refused, Acquisition_Failed,
      Routing_Lost, Source_Unhealthy, Owner_Stopped);
   subtype Window_Allowance is Interfaces.Unsigned_64 range 1 .. Interfaces.Unsigned_64'Last-1;
   Default_Window_Microseconds : constant Window_Allowance := 1_000_000;
   Default_Fuel : constant Positive := 10_000;
end ACPI_Local_Sample_Status;
