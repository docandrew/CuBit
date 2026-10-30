pragma Ada_2022;
package body ACPI_Resource_Data.Memory_Ranges with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_32;
   Memory_24_Name : constant Item_Name:=1;
   Memory_32_Name : constant Item_Name:=5;
   Memory_24_Length : constant Payload_Length:=9;
   Memory_32_Length : constant Payload_Length:=17;
   Word_Bytes : constant Positive:=2;
   DWord_Bytes : constant Positive:=4;
   Bits_Per_Byte : constant Natural:=8;
   Field_Start : constant Byte_Offset:=1;
   Maximum_Field : constant Byte_Count:=1;
   Alignment_Field : constant Byte_Count:=2;
   Length_Field : constant Byte_Count:=3;
   Block_Bytes : constant Memory_Value:=256;
   Zero_Alignment_24 : constant Memory_Value:=65_536;
   Write_Bit : constant Firmware_Tables.Byte:=1;
   function Read_Value (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Count : Positive) return Memory_Value with
      Pre=>Count<=DWord_Bytes and then Offset<=Byte_Count(Data'Length)
        and then Byte_Count(Data'Length)-Offset>=Byte_Count(Count);
   function Read_Value (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Count : Positive) return Memory_Value is
      Value : Interfaces.Unsigned_32:=0;
   begin
      for I in Natural range 0 .. Count-1 loop
         Value:=Value or Interfaces.Shift_Left(Interfaces.Unsigned_32
           (Data(Data'First+Natural(Offset)+I)),Bits_Per_Byte*I);
      end loop;
      return Memory_Value(Value);
   end Read_Value;
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result is
      Item : constant Frame:=Read_Frame(Data,Offset);
      Encoding : Memory_Encoding;
      Count : Positive range Word_Bytes .. DWord_Bytes;
      Scale : Memory_Value;
   begin
      if Item.Status/=ACPI_Resource_Data.Decoded then return(Status=>Malformed_Frame); end if;
      if Item.Encoding/=Large_Item then return(Status=>Unsupported_Descriptor); end if;
      case Item.Name is
         when Memory_24_Name =>
            if Item.Length/=Memory_24_Length then return(Status=>Bad_Length); end if;
            Encoding:=Memory_24; Count:=Word_Bytes; Scale:=Block_Bytes;
         when Memory_32_Name =>
            if Item.Length/=Memory_32_Length then return(Status=>Bad_Length); end if;
            Encoding:=Memory_32; Count:=DWord_Bytes; Scale:=1;
         when others => return(Status=>Unsupported_Descriptor);
      end case;
      declare
         Info : constant Firmware_Tables.Byte:=Data(Data'First+Natural(Item.Payload));
         First : constant Byte_Offset:=Item.Payload+Field_Start;
         Minimum : constant Memory_Value:=Read_Value(Data,First,Count);
         Maximum : constant Memory_Value:=Read_Value(Data,First+Maximum_Field*Byte_Count(Count),Count);
         Alignment : constant Memory_Value:=Read_Value(Data,First+Alignment_Field*Byte_Count(Count),Count);
         Length : constant Memory_Value:=Read_Value(Data,First+Length_Field*Byte_Count(Count),Count);
      begin
         return(Status=>Decoded,Encoding=>Encoding,Information=>Info,Declared_Writeable=>(Info and Write_Bit)/=0,
           Minimum_Base=>Minimum*Scale,Maximum_Base=>Maximum*Scale,Length=>Length*Scale,
           Encoded_Alignment=>Alignment,
           Alignment=>(if Encoding=Memory_24 and then Alignment=0 then Zero_Alignment_24 else Alignment),
           Next_Offset=>Item.Next_Offset);
      end;
   end Decode;
end ACPI_Resource_Data.Memory_Ranges;
