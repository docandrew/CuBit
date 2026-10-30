pragma Ada_2022;
with Interfaces;
-- Firmware descriptions only. No translation, mapping or resource admission.
package ACPI_Resource_Data.Address_Spaces with SPARK_Mode, Pure is
   type Address_Width is (Word_Width, DWord_Width, QWord_Width);
   type Address_Value is new Interfaces.Unsigned_64;
   type Resource_Kind is (Memory_Range, IO_Range, Bus_Range, PCC_Range, Vendor_Range);
   type Decode_Mode is (Positive_Decode, Subtractive_Decode);
   type Memory_Cache_Claim is (Non_Cacheable, Cacheable, Write_Combining, Prefetchable);
   type Memory_Range_Claim is (Address_Memory, Address_Reserved, Address_ACPI, Address_NVS);
   type IO_Range_Claim is (Non_ISA_Only, ISA_Only, Entire_Range);
   type Type_Information (Kind : Resource_Kind:=Memory_Range) is record
      Raw : Firmware_Tables.Byte;
      case Kind is
         when Memory_Range =>
            Declared_Writeable, Memory_To_IO : Boolean;
            Cache : Memory_Cache_Claim;
            Memory_Type : Memory_Range_Claim;
         when IO_Range =>
            IO_To_Memory, Sparse_Bit : Boolean;
            Coverage : IO_Range_Claim;
         when Vendor_Range => Vendor_Type : Firmware_Tables.Byte;
         when Bus_Range | PCC_Range => null;
      end case;
   end record;
   type Source_Field (Present : Boolean:=False) is record
      case Present is
         when True =>
            Index : Firmware_Tables.Byte;
            Name_Offset : Byte_Offset;
            Name_Length : Byte_Count;
         when False => null;
      end case;
   end record;
   type QWord_Source_Index_Policy is (Preserve_Unnegotiated, Require_Zero);
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Reserved_Resource_Type, Reserved_Bits_Set, Reserved_IO_Coverage,
      Bad_Source, Bad_Source_Index);
   type Decode_Result (Status : Decode_Status:=Unsupported_Descriptor) is record
      case Status is
         when Decoded =>
            Width : Address_Width;
            Information : Type_Information;
            General_Flags : Firmware_Tables.Byte; -- Includes ignored bit 0.
            Mode : Decode_Mode;
            Minimum_Fixed, Maximum_Fixed : Boolean;
            Granularity, Minimum, Maximum, Translation, Length : Address_Value;
            Source : Source_Field;
            Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Word/DWord source indices retain their descriptor-index meaning. The
   -- optional QWord zero-index rule comes from independent platform negotiation.
   -- A source name is opaque copied bytes, terminated at the descriptor boundary.
   -- Encoded address geometry and translation are retained, never applied here.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      QWord_Indices : QWord_Source_Index_Policy:=Preserve_Unnegotiated)
      return Decode_Result with Global=>null,
      Post=>(if Decode'Result.Status=Decoded then
        Decode'Result.Next_Offset>Offset
        and then Decode'Result.Next_Offset<=Byte_Count(Data'Length)
        and then (if Decode'Result.Source.Present then
          Decode'Result.Source.Name_Offset>Offset
          and then Decode'Result.Source.Name_Offset<Decode'Result.Next_Offset
          and then Decode'Result.Source.Name_Length=
            Decode'Result.Next_Offset-Decode'Result.Source.Name_Offset-1));
end ACPI_Resource_Data.Address_Spaces;
