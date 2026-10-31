pragma Ada_2022;
package body ACPI_EC_Clock_Policy with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   function Decode (Value : Interfaces.Unsigned_64; Available : Boolean) return Sample is
   begin
      if not Available then return (Status => Unavailable);
      elsif Value > Interfaces.Unsigned_64 (ACPI_EC_Types.Microseconds'Last) then
         return (Status => Representation_Limit);
      else return (Status => Sampled, Value => ACPI_EC_Types.Microseconds (Value));
      end if;
   end Decode;
   function Slice (Now, Target : ACPI_EC_Types.Microseconds;
                   Maximum : Positive_Microseconds) return ACPI_EC_Types.Microseconds is
   begin
      if Target - Now <= Maximum then return Target;
      else return Now + Maximum;
      end if;
   end Slice;
end ACPI_EC_Clock_Policy;
