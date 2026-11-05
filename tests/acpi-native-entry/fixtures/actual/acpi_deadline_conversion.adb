pragma Ada_2022;
package body ACPI_Deadline_Conversion with SPARK_Mode is
   function Ceiling_Milliseconds (Value : Microsecond_Instant) return Millisecond_Instant is
   begin
      if Value = No_Finite_Microsecond then return No_Finite_Millisecond; end if;
      return Millisecond_Instant (Value / Microseconds_Per_Millisecond)
        + (if Value mod Microseconds_Per_Millisecond = 0 then 0 else 1);
   end Ceiling_Milliseconds;
end ACPI_Deadline_Conversion;
