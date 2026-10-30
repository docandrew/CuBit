pragma Ada_2022;
with Interfaces;
-- Numeric _PRT policy only; a decoded route confers no interrupt authority.
package ACPI_PCI_Routes with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   type Device_Number is range 0 .. 31;
   type Interrupt_Pin is (INTA, INTB, INTC, INTD);
   type Source_Index is new Interfaces.Unsigned_32;
   type Global_Interrupt is new Interfaces.Unsigned_32;
   Function_Wildcard : constant Interfaces.Unsigned_64 := 16#FFFF#;
   Address_Radix : constant Interfaces.Unsigned_64 := 65_536;
   type Address_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Device : Device_Number;
         when False => null;
      end case;
   end record;
   function Decode_Address (Value : Interfaces.Unsigned_64) return Address_Result
     with Global => null,
     Post => Decode_Address'Result.Valid =
       (Value mod Address_Radix = Function_Wildcard
        and then Value / Address_Radix <= Interfaces.Unsigned_64 (Device_Number'Last))
       and then (if Decode_Address'Result.Valid then
         Interfaces.Unsigned_64 (Decode_Address'Result.Device) = Value / Address_Radix);
end ACPI_PCI_Routes;
