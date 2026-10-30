pragma Ada_2022;
package body ACPI_Resource_Data.DMA_Descriptors with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_16;
   Legacy_Name : constant Item_Name:=5;
   Fixed_Name : constant Item_Name:=10;
   Legacy_Length : constant Payload_Length:=2;
   Fixed_Length : constant Payload_Length:=5;
   Information_Offset : constant Byte_Offset:=1;
   Channel_Offset : constant Byte_Offset:=2;
   Width_Offset : constant Byte_Offset:=4;
   Reserved_Mask : constant Firmware_Tables.Byte:=16#80#;
   Preference_Mask : constant Firmware_Tables.Byte:=3;
   Bus_Master_Bit : constant Firmware_Tables.Byte:=4;
   Speed_Divisor : constant Firmware_Tables.Byte:=32;
   Speed_Mask : constant Firmware_Tables.Byte:=3;
   Maximum_Fixed_Width : constant Firmware_Tables.Byte:=Fixed_Transfer_Width'Pos(Fixed_Transfer_Width'Last);
   Word_Bytes : constant Byte_Count:=2;
   Byte_Radix : constant Interfaces.Unsigned_16:=256;
   function Word (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Interfaces.Unsigned_16 with
      Pre=>Offset<=Byte_Count(Data'Length)
        and then Byte_Count(Data'Length)-Offset>=Word_Bytes;
   function Word (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Interfaces.Unsigned_16 is
     (Interfaces.Unsigned_16(Data(Data'First+Natural(Offset)))+
      Byte_Radix*Interfaces.Unsigned_16(Data(Data'First+Natural(Offset)+1)));
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Purpose : Resource_Use:=Current_Resources) return Decode_Result is
      Item : constant Frame:=Read_Frame(Data,Offset);
      Info : Firmware_Tables.Byte;
   begin
      if Item.Status/=ACPI_Resource_Data.Decoded then return(Status=>Malformed_Frame); end if;
      if Item.Encoding/=Small_Item then return(Status=>Unsupported_Descriptor); end if;
      case Item.Name is
         when Legacy_Name =>
            if Item.Length/=Legacy_Length then return(Status=>Bad_Length); end if;
            Info:=Data(Data'First+Natural(Item.Payload+Information_Offset));
            if (Info and Reserved_Mask)/=0 then return(Status=>Reserved_Bits_Set); end if;
            if (Info and Preference_Mask)=Preference_Mask then return(Status=>Reserved_Transfer_Preference); end if;
            return(Status=>Decoded,Value=>(Kind=>Legacy_DMA,
              Mask=>Channel_Mask(Data(Data'First+Natural(Item.Payload))),Information=>Info,
              Speed=>Channel_Speed'Val(Natural((Info/Speed_Divisor) and Speed_Mask)),
              Bus_Master_Claim=>(Info and Bus_Master_Bit)/=0,
              Preference=>Transfer_Preference'Val(Natural(Info and Preference_Mask))),Next_Offset=>Item.Next_Offset);
         when Fixed_Name =>
            if Item.Length/=Fixed_Length then return(Status=>Bad_Length); end if;
            if Purpose/=Current_Resources then return(Status=>Wrong_Use); end if;
            Info:=Data(Data'First+Natural(Item.Payload+Width_Offset));
            if Info>Maximum_Fixed_Width then return(Status=>Reserved_Transfer_Width); end if;
            return(Status=>Decoded,Value=>(Kind=>Fixed_DMA,Request=>Request_Line(Word(Data,Item.Payload)),
              Channel=>Channel_Number(Word(Data,Item.Payload+Channel_Offset)),
              Width=>Fixed_Transfer_Width'Val(Natural(Info))),Next_Offset=>Item.Next_Offset);
         when others => return(Status=>Unsupported_Descriptor);
      end case;
   end Decode;
end ACPI_Resource_Data.DMA_Descriptors;
