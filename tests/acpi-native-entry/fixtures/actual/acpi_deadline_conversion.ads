pragma Ada_2022;
with Interfaces;
package ACPI_Deadline_Conversion with Pure, SPARK_Mode is
   type Microsecond_Instant is new Interfaces.Unsigned_64;
   type Millisecond_Instant is new Interfaces.Unsigned_64;
   No_Finite_Microsecond : constant Microsecond_Instant := Microsecond_Instant'Last;
   No_Finite_Millisecond : constant Millisecond_Instant := Millisecond_Instant'Last;
   Microseconds_Per_Millisecond : constant := 1_000;
   -- Same monotonic epoch, different units. Ceiling avoids repeated immediate
   -- wakes in a fractional millisecond. It schedules cleanup only: every data
   -- access must still check the original microsecond lease deadline.
   function Ceiling_Milliseconds (Value : Microsecond_Instant) return Millisecond_Instant
     with Global => null,
       Post => (if Value = No_Finite_Microsecond then Ceiling_Milliseconds'Result = No_Finite_Millisecond
         else Ceiling_Milliseconds'Result /= No_Finite_Millisecond
           and then Ceiling_Milliseconds'Result = Millisecond_Instant (Value / Microseconds_Per_Millisecond)
             + (if Value mod Microseconds_Per_Millisecond = 0 then 0 else 1));
end ACPI_Deadline_Conversion;
