pragma Ada_2022;
package body ACPI_Resource_Data.Extended_Interrupts with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_32;
   Extended_IRQ_Name : constant Item_Name:=9;
   Minimum_Payload : constant Payload_Length:=6;
   Count_Offset : constant Byte_Offset:=1;
   Numbers_Offset : constant Byte_Offset:=2;
   Number_Bytes : constant Byte_Count:=4;
   Bits_Per_Byte : constant Natural:=8;
   Consumer_Bit : constant Firmware_Tables.Byte:=1;
   Edge_Bit : constant Firmware_Tables.Byte:=2;
   Low_Bit : constant Firmware_Tables.Byte:=4;
   Sharing_Bit : constant Firmware_Tables.Byte:=8;
   Wake_Bit : constant Firmware_Tables.Byte:=16;
   Reserved_Mask : constant Firmware_Tables.Byte:=16#E0#;
   function Read_Number(Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Interrupt_Number with
      Pre=>Offset<=Byte_Count(Data'Length)
        and then Byte_Count(Data'Length)-Offset>=Number_Bytes;
   function Read_Number(Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Interrupt_Number is
      Value : Interfaces.Unsigned_32:=0;
   begin
      for I in Natural range 0 .. Natural(Number_Bytes)-1 loop
         Value:=Value or Interfaces.Shift_Left(Interfaces.Unsigned_32
           (Data(Data'First+Natural(Offset)+I)),Bits_Per_Byte*I);
      end loop;
      return Interrupt_Number(Value);
   end Read_Number;
   function Decode(Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Purpose : Descriptor_Use:=Current_Resources;
      Source_Indices : Source_Index_Policy:=Preserve_Unnegotiated)
      return Decode_Result is
      Item : constant Frame:=Read_Frame(Data,Offset);
      Flags : Firmware_Tables.Byte;
      Encoded_Count : Natural;
      Table_Bytes : Byte_Count;
      Source : Source_Field := (Present=>False);
   begin
      if Item.Status/=ACPI_Resource_Data.Decoded then return(Status=>Malformed_Frame); end if;
      if Item.Encoding/=Large_Item or else Item.Name/=Extended_IRQ_Name then
         return(Status=>Unsupported_Descriptor);
      end if;
      if Item.Length<Minimum_Payload then return(Status=>Bad_Length); end if;
      Flags:=Data(Data'First+Natural(Item.Payload));
      if (Flags and Reserved_Mask)/=0 then return(Status=>Reserved_Bits_Set); end if;
      Encoded_Count:=Natural(Data(Data'First+Natural(Item.Payload+Count_Offset)));
      if Encoded_Count=0 then return(Status=>Bad_Interrupt_Count); end if;
      Table_Bytes:=Numbers_Offset+Number_Bytes*Byte_Count(Encoded_Count);
      if Table_Bytes>Byte_Count(Item.Length) then return(Status=>Bad_Length); end if;
      if Purpose/=Possible_Resources and then Encoded_Count/=1 then return(Status=>Wrong_Usage_Count); end if;
      if Table_Bytes<Byte_Count(Item.Length) then
         declare
            Tail : constant Byte_Offset:=Item.Payload+Table_Bytes;
            Name_First : constant Byte_Offset:=Tail+1;
         begin
            if Item.Next_Offset-Name_First<1 then return(Status=>Bad_Source); end if;
            if Source_Indices=Require_Zero and then Data(Data'First+Natural(Tail))/=0 then
               return(Status=>Bad_Source_Index);
            end if;
            -- The first NUL must be the final byte of this descriptor. Never
            -- search the next descriptor or an EndTag for a missing terminator.
            for Cursor in Byte_Offset range Name_First .. Item.Next_Offset-1 loop
               if Data(Data'First+Natural(Cursor))=0 and then Cursor/=Item.Next_Offset-1 then
                  return(Status=>Bad_Source);
               end if;
            end loop;
            if Data(Data'First+Natural(Item.Next_Offset-1))/=0 then return(Status=>Bad_Source); end if;
            Source:=(Present=>True,Index=>Data(Data'First+Natural(Tail)),
              Name_Offset=>Name_First,Name_Length=>Item.Next_Offset-Name_First-1);
         end;
      end if;
      return Result : Decode_Result(Decoded) do
         Result.Role:=(if (Flags and Consumer_Bit)/=0 then Consumer else Producer);
         Result.Mode:=(if (Flags and Edge_Bit)/=0 then Edge_Triggered else Level_Triggered);
         Result.Polarity:=(if (Flags and Low_Bit)/=0 then Active_Low else Active_High);
         Result.Shared:=(Flags and Sharing_Bit)/=0; Result.Wake_Capable:=(Flags and Wake_Bit)/=0;
         Result.Count:=Interrupt_Count(Encoded_Count); Result.Source:=Source;
         Result.Next_Offset:=Item.Next_Offset;
         for I in Interrupt_Count range 1 .. Result.Count loop
            Result.Interrupts(I):=Read_Number(Data,Item.Payload+Numbers_Offset+Number_Bytes*Byte_Count(I-1));
         end loop;
      end return;
   end Decode;
end ACPI_Resource_Data.Extended_Interrupts;
