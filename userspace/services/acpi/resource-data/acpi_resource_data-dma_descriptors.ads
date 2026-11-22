pragma Ada_2022;
with Interfaces;
-- Channel/request metadata only. No controller programming or DMA authority.
package ACPI_Resource_Data.DMA_Descriptors with SPARK_Mode, Pure is
   type Resource_Use is (Current_Resources, Possible_Resources, Set_Resources);
   type Channel_Mask is new Interfaces.Unsigned_8;
   type Request_Line is new Interfaces.Unsigned_16;
   type Channel_Number is new Interfaces.Unsigned_16;
   type Channel_Speed is (Compatibility, Type_A, Type_B, Type_F);
   type Transfer_Preference is (Only_8, Either_8_16, Only_16);
   type Fixed_Transfer_Width is (Width_8, Width_16, Width_32, Width_64, Width_128, Width_256);
   type Resource_Kind is (Legacy_DMA, Fixed_DMA);
   type Description (Kind : Resource_Kind:=Legacy_DMA) is record
      case Kind is
         when Legacy_DMA =>
            Mask : Channel_Mask;
            Information : Firmware_Tables.Byte;
            Speed : Channel_Speed;
            Bus_Master_Claim : Boolean;
            Preference : Transfer_Preference;
         when Fixed_DMA =>
            Request : Request_Line;
            Channel : Channel_Number;
            Width : Fixed_Transfer_Width;
      end case;
   end record;
   type Decode_Status is (Decoded, Malformed_Frame, Unsupported_Descriptor,
      Bad_Length, Reserved_Bits_Set, Reserved_Transfer_Preference,
      Reserved_Transfer_Width, Wrong_Use);
   type Decode_Result (Status : Decode_Status:=Unsupported_Descriptor) is record
      case Status is
         when Decoded => Value : Description; Next_Offset : Byte_Offset;
         when others => null;
      end case;
   end record;
   -- Legacy bits4:3 ignored/preserved, bit7 reserved; preference3 reserved.
   -- Fixed DMA is allowed only in _CRS, not _PRS or _SRS. Request/controller
   -- association and actual channel allocation remain independent platform work.
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Purpose : Resource_Use:=Current_Resources) return Decode_Result
      with Global=>null,
      Post=>(if Decode'Result.Status=Decoded then
        Decode'Result.Next_Offset>Offset
        and then Decode'Result.Next_Offset<=Byte_Count(Data'Length)
        and then (if Decode'Result.Value.Kind=Fixed_DMA then Purpose=Current_Resources));
end ACPI_Resource_Data.DMA_Descriptors;
