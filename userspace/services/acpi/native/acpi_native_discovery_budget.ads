pragma Ada_2022;
with Interfaces;
-- Native full-namespace discovery, matching the existing finite EC startup
-- allowance. No timer, authority change, retry or unsafe-owner recovery.
package ACPI_Native_Discovery_Budget with Pure, SPARK_Mode is
   use type Interfaces.Unsigned_64;
   Window_Microseconds:constant Interfaces.Unsigned_64:=5_000_000;
   procedure Make_Deadline(Now:Interfaces.Unsigned_64;Known:Boolean;
      Deadline:out Interfaces.Unsigned_64;Accepted:out Boolean)
     with Global=>null,
       Post=>Accepted=(Known and then Now<Interfaces.Unsigned_64'Last-Window_Microseconds)
         and then (if Accepted then Known and then Now<Interfaces.Unsigned_64'Last-Window_Microseconds
         and then Deadline=Now+Window_Microseconds and then Deadline not in 0|Interfaces.Unsigned_64'Last
         else Deadline=0);
end ACPI_Native_Discovery_Budget;
