pragma Ada_2022;
with Interfaces;
with Firmware_Tables;
with ACPI_FADT;
with AML_Names;
--  ECDT descriptions only (ACPI6.6 5.2.15). No memory/port mapping, kernel
--  admission, namespace binding, Global Lock fact or hardware rights are minted.
package ACPI_EC_Boot_Table with SPARK_Mode, Pure is
   Fixed_Size : constant := 65;
   type Decode_Status is (Decoded, Invalid_Table, Truncated_Fixed_Fields,
                          Missing_Terminator, Non_ASCII_Identifier);
   type Description (Status : Decode_Status := Invalid_Table) is record
      case Status is
         when Decoded =>
            Revision : Firmware_Tables.Byte;
            Control, Data : ACPI_FADT.Generic_Address;
            UID : Interfaces.Unsigned_32;
            GPE_Bit : Firmware_Tables.Byte;
            Path_Length, Suffix_Length : Natural;
         when others => null;
      end case;
   end record;
   --  Exact copied SDT extent/checksum, full GAS fields and a terminated ASCII
   --  identifier are required. Revision/GAS claims are preserved; bytes after the
   --  first terminator are counted for separate compatibility/admission policy.
   function Decode (Data : Firmware_Tables.Bytes) return Description
     with Global => null,
       Post => (if Decode'Result.Status = Decoded then
         Data'Length > Fixed_Size
         and then Decode'Result.Path_Length < Data'Length - Fixed_Size
         and then Decode'Result.Suffix_Length =
           Data'Length - Fixed_Size - Decode'Result.Path_Length - 1);
   subtype Segment_Count is Natural range 0 .. AML_Names.Segment_Array'Length;
   type Path_Status is (Parsed, Invalid_Description, Invalid_Path, Path_Too_Deep);
   type Path_Result (Status : Path_Status := Invalid_Description) is record
      case Status is
         when Parsed =>
            Count : Segment_Count range 1 .. Segment_Count'Last;
            Parts : AML_Names.Segment_Array;
         when others => null;
      end case;
   end record;
   --  Revalidates supplied bytes. Requires a rooted textual path with1..4-byte
   --  ACPI name segments, padding short segments with underscores. Does not
   --  resolve names, evaluate AML or establish that the target is an EC Device.
   function Read_Path (Data : Firmware_Tables.Bytes) return Path_Result
     with Global => null,
       Post => (if Read_Path'Result.Status = Parsed then
         (for all I in 1 .. Read_Path'Result.Count =>
            AML_Names.Valid (Read_Path'Result.Parts (I))));
   type Register_Status is (Byte_Registers_Described, Invalid_Description,
      Unsupported_Revision, Unrecognized_Suffix, Unsupported_Space,
      Unsupported_Access, IO_Address_Too_Wide, Aliased_Registers);
   --  Byte-transaction compatibility only, not platform ownership validation.
   --  ECDT gives no Global Lock/sharing fact; its absence cannot imply exclusive
   --  ownership. Native use still needs independently reserved named resources.
   function Check_Registers (Data : Firmware_Tables.Bytes) return Register_Status
     with Global => null;
end ACPI_EC_Boot_Table;
