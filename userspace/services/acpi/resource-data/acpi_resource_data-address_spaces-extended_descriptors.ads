pragma Ada_2022;
with Interfaces;
with ACPI_Resource_Data.Address_Spaces.Geometry;
-- Revision-one Extended Address Space descriptions, never mapping authority.
package ACPI_Resource_Data.Address_Spaces.Extended_Descriptors with SPARK_Mode, Pure is
   type Resource_Role is (Produces_And_Consumes, Consumes_Resource);
   type Extended_Kind is (Memory_Range, IO_Range, Bus_Range, Vendor_Range);
   type Cache_Bits is range 0 .. 3;
   type Attribute_Value is new Interfaces.Unsigned_64;
   type Specific_Information (Kind : Extended_Kind:=Memory_Range) is record
      Raw : Firmware_Tables.Byte;
      case Kind is
         when Memory_Range =>
            Declared_Writeable, Memory_To_IO : Boolean;
            Memory_Type : Memory_Range_Claim;
            Ignored_Cache_Bits : Cache_Bits; -- _MEM is ignored for this form.
         when IO_Range =>
            IO_To_Memory, Sparse_Bit : Boolean;
            Coverage : IO_Range_Claim;
         when Vendor_Range => Vendor_Type : Firmware_Tables.Byte;
         when Bus_Range => null;
      end case;
   end record;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Unsupported_Revision, Reserved_Byte_Set, Reserved_Resource_Type,
      Reserved_Bits_Set, Reserved_IO_Coverage, Reserved_Attributes_Set);
   type Decode_Result (Status : Decode_Status:=Unsupported_Descriptor) is record
      case Status is
         when Decoded =>
            Information : Specific_Information;
            General_Flags : Firmware_Tables.Byte;
            Role : Resource_Role;
            Mode : Decode_Mode;
            Minimum_Fixed, Maximum_Fixed : Boolean;
            Granularity, Minimum, Maximum, Translation, Length : Address_Value;
            Attributes : Attribute_Value; -- Raw UEFI memory-attribute claim.
            Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- No optional source tail. Attributes must be zero for non-memory types.
   -- Memory attributes are preserved without ISA-specific validation/mapping.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result with Global=>null,
      Post=>(if Decode'Result.Status=Decoded then
        Decode'Result.Next_Offset>Offset
        and then Decode'Result.Next_Offset<=Byte_Count(Data'Length)
        and then (if Decode'Result.Information.Kind/=Memory_Range then
          Decode'Result.Attributes=0));
   function Geometry_Of (Item : Decode_Result) return Geometry.Parameters
      with Global=>null, Pre=>Item.Status=Decoded;
end ACPI_Resource_Data.Address_Spaces.Extended_Descriptors;
