pragma Ada_2022;
with Interfaces;
-- Portable descriptor words for the host-bridge snapshot protocol. These are
-- firmware claims, not allocations, translations or register authority.
package ACPI_Host_Bridge_Wire with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   type Column is
     (Ordinal, Form, Kind, Address_Bits, General_Flags, Specific_Flags,
      Claims, Minimum, Maximum, Granularity_Or_Alignment, Translation, Length,
      Attributes, Cache, Writable, Source_Index, Source_Name_Offset,
      Source_Name_Length, Descriptor_Offset, Descriptor_Extent,
      Encoded_Alignment, Decoding_Bits, Vendor_Type);
   type Row is array (Column) of Word with Component_Size => 64;
   type Rows is array (Positive range <>) of Row;
   Empty_Row : constant Row := [others => 0];
   No_Form : constant Word := 0;
   Word_Address : constant Word := 1;
   DWord_Address : constant Word := 2;
   QWord_Address : constant Word := 3;
   Extended_Address : constant Word := 4;
   IO_Descriptor : constant Word := 5;
   Fixed_IO_Descriptor : constant Word := 6;
   Fixed_Memory_32 : constant Word := 7;
   Memory_24 : constant Word := 8;
   Memory_32 : constant Word := 9;
   Memory_Kind : constant Word := 1;
   IO_Kind : constant Word := 2;
   Bus_Kind : constant Word := 3;
   PCC_Kind : constant Word := 4;
   Vendor_Kind : constant Word := 5;
   -- Presence is explicit. In particular, legacy resources have no producer
   -- claim; a zero General_Flags word must not invent one for them.
   Has_General : constant Word := 2 ** 0;
   Has_Minimum : constant Word := 2 ** 1;
   Has_Maximum : constant Word := 2 ** 2;
   Has_Granularity : constant Word := 2 ** 3;
   Has_Translation : constant Word := 2 ** 4;
   Has_Length : constant Word := 2 ** 5;
   Has_Attributes : constant Word := 2 ** 6;
   Has_Cache : constant Word := 2 ** 7;
   Has_Writable : constant Word := 2 ** 8;
   Has_Source : constant Word := 2 ** 9;
   Has_Decoding : constant Word := 2 ** 10;
   Has_Encoded_Alignment : constant Word := 2 ** 11;
end ACPI_Host_Bridge_Wire;
