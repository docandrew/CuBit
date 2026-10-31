pragma Ada_2022;
with Interfaces;
-- Legacy memory descriptors describe allowed BASE addresses, not the end of a
-- window. Do not feed their MAX directly to address-space Geometry.Check.
package ACPI_Resource_Data.Memory_Ranges with SPARK_Mode, Pure is
   type Memory_Encoding is (Memory_24, Memory_32);
   type Memory_Value is new Interfaces.Unsigned_32;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor, Bad_Length);
   type Decode_Result (Status : Decode_Status:=Unsupported_Descriptor) is record
      case Status is
         when Decoded =>
            Encoding : Memory_Encoding;
            Information : Firmware_Tables.Byte;
            Declared_Writeable : Boolean;
            Minimum_Base, Maximum_Base, Length : Memory_Value;
            Encoded_Alignment, Alignment : Memory_Value;
            Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Memory24 base/length fields use 256-byte units. Its zero alignment means
   -- 64 KiB; Memory32 alignment is retained verbatim, including zero. Ignored
   -- information bits are retained. No geometry or ownership is established.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result with Global=>null,
      Post=>(if Decode'Result.Status=Decoded then
        Decode'Result.Next_Offset>Offset
        and then Decode'Result.Next_Offset<=Byte_Count(Data'Length));
end ACPI_Resource_Data.Memory_Ranges;
