pragma Ada_2022;
with Firmware_Tables;
with Firmware_Tables.Identifiers;
package ACPI_Catalog_Header_Notes with Pure, SPARK_Mode is
   -- Fixed header fields only: no table payload, physical address or grant.
   type Header is record
      Index, ID, Length : Positive;
      Revision : Firmware_Tables.Byte;
      Identity : Firmware_Tables.Identifiers.Identity;
      Checksum_Admitted : Boolean;
   end record;
   -- Covers worst-case escaped fixed identifiers and maximum decimal images.
   Line_Capacity : constant Positive := 256;
   subtype Line_Text is String (1 .. Line_Capacity);
   subtype Line_Length is Natural range 0 .. Line_Capacity;
   type Format_Status is (Formatted, Format_Limit);
   procedure Format (Item : Header; Text : out Line_Text;
      Last : out Line_Length; Status : out Format_Status);
end ACPI_Catalog_Header_Notes;
