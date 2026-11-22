pragma Ada_2022;
package body ACPI_Resource_Data.Basic_Descriptors with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   Byte_Radix : constant Positive := 256;
   Word_Bytes : constant Byte_Count := 2;
   DWord_Bytes : constant Byte_Count := 4;
   IRQ_Name : constant Item_Name := 4;
   IO_Name : constant Item_Name := 8;
   Fixed_IO_Name : constant Item_Name := 9;
   Fixed_Memory_Name : constant Item_Name := 6;
   IRQ_Mask_Bytes : constant Payload_Length := 2;
   IRQ_With_Information : constant Payload_Length := 3;
   IO_Bytes : constant Payload_Length := 7;
   Fixed_IO_Bytes : constant Payload_Length := 3;
   Fixed_Memory_Bytes : constant Payload_Length := 9;
   IRQ_Default_Information : constant Firmware_Tables.Byte := 1;
   IRQ_Mode_Bit : constant Firmware_Tables.Byte := 1;
   IRQ_Polarity_Bit : constant Firmware_Tables.Byte := 8;
   IRQ_Sharing_Bit : constant Firmware_Tables.Byte := 16;
   IRQ_Wake_Bit : constant Firmware_Tables.Byte := 32;
   IRQ_Reserved_Mask : constant Firmware_Tables.Byte := 16#C0#;
   IO_Decode_Bit : constant Firmware_Tables.Byte := 1;
   IO_Reserved_Mask : constant Firmware_Tables.Byte := 16#FE#;
   Memory_Write_Bit : constant Firmware_Tables.Byte := 1;
   Word_High_Offset : constant Byte_Offset := 1;
   Word_High_Shift : constant Natural := 16;
   IO_Minimum_Offset : constant Byte_Offset := 1;
   IO_Maximum_Offset : constant Byte_Offset := 3;
   IO_Alignment_Offset : constant Byte_Offset := 5;
   IO_Length_Offset : constant Byte_Offset := 6;
   Fixed_IO_Length_Offset : constant Byte_Offset := 2;
   Memory_Base_Offset : constant Byte_Offset := 1;
   Memory_Length_Offset : constant Byte_Offset := 5;
   function Word (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
     return Interfaces.Unsigned_16 with
     Pre => Offset <= Byte_Count(Data'Length)
       and then Byte_Count(Data'Length)-Offset >= Word_Bytes;
   function Word (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
     return Interfaces.Unsigned_16 is
     (Interfaces.Unsigned_16(Data(Data'First+Natural(Offset))) +
      Interfaces.Unsigned_16(Byte_Radix)*Interfaces.Unsigned_16(Data(Data'First+Natural(Offset+Word_High_Offset))));
   function DWord (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
     return Interfaces.Unsigned_32 with
     Pre => Offset <= Byte_Count(Data'Length)
       and then Byte_Count(Data'Length)-Offset >= DWord_Bytes;
   function DWord (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
     return Interfaces.Unsigned_32 is
     (Interfaces.Unsigned_32(Word(Data,Offset)) or
      Interfaces.Shift_Left(Interfaces.Unsigned_32(Word(Data,Offset+Word_Bytes)),Word_High_Shift));
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
     return Decode_Result is
      Item : constant Frame := Read_Frame(Data,Offset);
      Info : Firmware_Tables.Byte;
   begin
      if Item.Status /= ACPI_Resource_Data.Decoded then return (Status=>Malformed_Frame); end if;
      if Item.Encoding=Small_Item then
         case Item.Name is
            when IRQ_Name =>
               if Item.Length not in IRQ_Mask_Bytes | IRQ_With_Information then return (Status=>Bad_Length); end if;
               Info:=IRQ_Default_Information;
               if Item.Length=IRQ_With_Information then Info:=Data(Data'First+Natural(Item.Payload+Byte_Count(IRQ_Mask_Bytes))); end if;
               if (Info and IRQ_Reserved_Mask)/=0 then return (Status=>Reserved_Bits_Set); end if;
               if ((Info and IRQ_Mode_Bit)/=0) = ((Info and IRQ_Polarity_Bit)/=0) then
                  return (Status=>Invalid_IRQ_Combination);
               end if;
               return (Decoded,(Kind=>IRQ_Resource,IRQ=>
                 (Mask=>IRQ_Mask(Word(Data,Item.Payload)),
                  Mode=>(if (Info and IRQ_Mode_Bit)/=0 then Edge_Triggered else Level_Triggered),
                  Polarity=>(if (Info and IRQ_Polarity_Bit)/=0 then Active_Low else Active_High),
                  Shared=>(Info and IRQ_Sharing_Bit)/=0,Wake_Capable=>(Info and IRQ_Wake_Bit)/=0,
                  Information_Present=>Item.Length=IRQ_With_Information)),Item.Next_Offset);
            when IO_Name =>
               if Item.Length/=IO_Bytes then return (Status=>Bad_Length); end if;
               Info:=Data(Data'First+Natural(Item.Payload));
               if (Info and IO_Reserved_Mask)/=0 then return (Status=>Reserved_Bits_Set); end if;
               return (Decoded,(Kind=>IO_Resource,IO=>
                 (Decoding=>(if (Info and IO_Decode_Bit)/=0 then Decode_16 else Decode_10),
                  Minimum=>Port_Address(Word(Data,Item.Payload+IO_Minimum_Offset)),Maximum=>Port_Address(Word(Data,Item.Payload+IO_Maximum_Offset)),
                  Alignment=>Data(Data'First+Natural(Item.Payload+IO_Alignment_Offset)),Length=>Data(Data'First+Natural(Item.Payload+IO_Length_Offset)))),Item.Next_Offset);
            when Fixed_IO_Name =>
               if Item.Length/=Fixed_IO_Bytes then return (Status=>Bad_Length); end if;
               return (Decoded,(Kind=>Fixed_IO_Resource,Fixed_IO=>
                 (Base=>Port_Address(Word(Data,Item.Payload)),Length=>Data(Data'First+Natural(Item.Payload+Fixed_IO_Length_Offset)))),Item.Next_Offset);
            when others => return (Status=>Unsupported_Descriptor);
         end case;
      elsif Item.Name=Fixed_Memory_Name then
         if Item.Length/=Fixed_Memory_Bytes then return (Status=>Bad_Length); end if;
         Info:=Data(Data'First+Natural(Item.Payload));
         return (Decoded,(Kind=>Fixed_Memory_32_Resource,Memory=>
           (Base=>Memory_Base_32(DWord(Data,Item.Payload+Memory_Base_Offset)),Length=>Memory_Length_32(DWord(Data,Item.Payload+Memory_Length_Offset)),
            Declared_Writeable=>(Info and Memory_Write_Bit)/=0,Information=>Info)),Item.Next_Offset);
      else return (Status=>Unsupported_Descriptor);
      end if;
   end Decode;
end ACPI_Resource_Data.Basic_Descriptors;
