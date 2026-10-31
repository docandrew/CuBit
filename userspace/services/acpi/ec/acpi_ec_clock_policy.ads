pragma Ada_2022;
with Interfaces;
with ACPI_EC_Types;
package ACPI_EC_Clock_Policy with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   subtype Positive_Microseconds is ACPI_EC_Types.Microseconds
     range 1 .. ACPI_EC_Types.Microseconds'Last;
   Default_Maximum_Slice : constant Positive_Microseconds := 1_000;
   type Sample_Status is (Sampled, Unavailable, Representation_Limit);
   type Sample (Status : Sample_Status := Unavailable) is record
      case Status is
         when Sampled => Value : ACPI_EC_Types.Microseconds;
         when others => null;
      end case;
   end record;
   function Decode (Value : Interfaces.Unsigned_64; Available : Boolean) return Sample
     with Global => null,
       Post => (if not Available then Decode'Result.Status = Unavailable
         elsif Value > Interfaces.Unsigned_64 (ACPI_EC_Types.Microseconds'Last) then
            Decode'Result.Status = Representation_Limit
         else Decode'Result.Status = Sampled
           and then Decode'Result.Value = ACPI_EC_Types.Microseconds (Value));
   type Pause_Outcome is (Pause_Returned, Already_Due, Clock_Unavailable,
      Clock_Representation_Limit, Pause_Refused, Budget_Exhausted);
   type Pause_Report is record
      Status : Pause_Outcome := Budget_Exhausted;
      Requested : ACPI_EC_Types.Microseconds := 0;
   end record;
   use type ACPI_EC_Types.Microseconds;
   function Slice (Now, Target : ACPI_EC_Types.Microseconds;
                   Maximum : Positive_Microseconds) return ACPI_EC_Types.Microseconds
     with Global => null, Pre => Target > Now,
       Post => Slice'Result > Now and Slice'Result <= Target
         and Slice'Result - Now <= Maximum
         and Slice'Result = (if Target - Now <= Maximum then Target else Now + Maximum);
end ACPI_EC_Clock_Policy;
