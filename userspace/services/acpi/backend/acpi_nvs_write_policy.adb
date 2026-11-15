pragma Ada_2022;
with ACPI_Firmware_Data_Layouts;
with ACPI_FACS_Header;
package body ACPI_NVS_Write_Policy with SPARK_Mode is
   use type Word;
   function Permitted (Address : Word; Width : ACPI_FADT.Transactions.Access_Width;
      FACS_Address : Word) return Boolean is
      Bytes : constant Word := Word (ACPI_FADT.Transactions.Octets (Width));
      FACS_Alignment : constant Word := 64; -- Same geometry required by FACS_Binding.
      Page_Bytes : constant Word := ACPI_Firmware_Data_Layouts.Page_Bytes;
   begin
      return Address mod Bytes = 0 and then Address <= Word'Last-(Bytes-1)
        and then (FACS_Address = 0 or else
          (FACS_Address mod FACS_Alignment = 0
           and then FACS_Address <= Word'Last-Word (ACPI_FACS_Header.Minimum_Length-1)
           and then Address/Page_Bytes /= FACS_Address/Page_Bytes
           and then (Address+(Bytes-1))/Page_Bytes /= FACS_Address/Page_Bytes));
   end Permitted;
end ACPI_NVS_Write_Policy;
