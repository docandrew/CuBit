pragma Ada_2022;
package body ACPI_Native_Discovery_Budget with SPARK_Mode is
   procedure Make_Deadline(Now:Interfaces.Unsigned_64;Known:Boolean;
      Deadline:out Interfaces.Unsigned_64;Accepted:out Boolean) is
   begin
      Deadline:=0;Accepted:=False;
      if not Known or else Now>=Interfaces.Unsigned_64'Last-Window_Microseconds then return;end if;
      Deadline:=Now+Window_Microseconds;Accepted:=True;
   end Make_Deadline;
end ACPI_Native_Discovery_Budget;
