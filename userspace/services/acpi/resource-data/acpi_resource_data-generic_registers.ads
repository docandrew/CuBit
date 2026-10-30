pragma Ada_2022;
with ACPI_FADT;
-- Copied GAS metadata, never an admitted register ID or hardware capability.
package ACPI_Resource_Data.Generic_Registers with SPARK_Mode, Pure is
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Reserved_Access_Size);
   type Decode_Result (Status : Decode_Status:=Unsupported_Descriptor) is record
      case Status is
         when Decoded => Value : ACPI_FADT.Generic_Address; Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Space IDs and bit geometry are preserved without selecting a backend.
   -- Zero address/width/access-size are metadata, not an absence decision.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result with Global=>null,
      Post=>(if Decode'Result.Status=Decoded then
        Decode'Result.Next_Offset>Offset
        and then Decode'Result.Next_Offset<=Byte_Count(Data'Length));
end ACPI_Resource_Data.Generic_Registers;
