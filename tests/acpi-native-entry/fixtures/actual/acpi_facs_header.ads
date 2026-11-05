pragma Ada_2022;
with Interfaces;
with Firmware_Tables;
package ACPI_FACS_Header with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_8;
   Minimum_Length : constant Positive := 64;
   Global_Lock_Offset : constant Natural := 16;
   Version_Offset : constant Natural := 32;
   type Header_Status is (Too_Short, Bad_Signature, Bad_Length, Recognized_Header);
   type Description is record
      Status : Header_Status := Too_Short;
      Declared_Length : Interfaces.Unsigned_32 := 0;
      Version : Interfaces.Unsigned_8 := 0;
   end record;
   -- FACS has no SDT header/checksum. Decode an immutable copied prefix; do
   -- not read its changing GlobalLock as ownership evidence. A recognized
   -- header is not a complete-table validation or a physical mapping grant.
   function Decode_Prefix (Data : Firmware_Tables.Bytes) return Description
     with Post =>
       (if Decode_Prefix'Result.Status = Recognized_Header then
          Data'Length >= Minimum_Length
          and then Decode_Prefix'Result.Declared_Length >= Interfaces.Unsigned_32 (Minimum_Length)
          and then Decode_Prefix'Result.Version = Data (Data'First + Version_Offset)
        else Decode_Prefix'Result.Declared_Length = 0 and then Decode_Prefix'Result.Version = 0);
end ACPI_FACS_Header;
