pragma Ada_2022;
package body ACPI_PCI_Routes with SPARK_Mode is
   function Decode_Address (Value : Interfaces.Unsigned_64) return Address_Result is
   begin
      if Value mod Address_Radix /= Function_Wildcard
        or else Value / Address_Radix > Interfaces.Unsigned_64 (Device_Number'Last) then
         return (Valid => False);
      end if;
      return (Valid => True, Device => Device_Number (Value / Address_Radix));
   end Decode_Address;
end ACPI_PCI_Routes;
