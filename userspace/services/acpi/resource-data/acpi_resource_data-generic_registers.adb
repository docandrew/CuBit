pragma Ada_2022;
with Interfaces;
package body ACPI_Resource_Data.Generic_Registers with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_64;
   Register_Name : constant Item_Name:=2;
   Register_Length : constant Payload_Length:=12;
   Width_Offset : constant Byte_Offset:=1;
   Bit_Offset_Offset : constant Byte_Offset:=2;
   Access_Offset : constant Byte_Offset:=3;
   Address_Offset : constant Byte_Offset:=4;
   Address_Bytes : constant Positive:=8;
   Bits_Per_Byte : constant Natural:=8;
   Maximum_Access_Code : constant Firmware_Tables.Byte:=4;
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result is
      Item : constant Frame:=Read_Frame(Data,Offset);
      Address : Interfaces.Unsigned_64:=0;
      Access_Code : Firmware_Tables.Byte;
   begin
      if Item.Status/=ACPI_Resource_Data.Decoded then return(Status=>Malformed_Frame); end if;
      if Item.Encoding/=Large_Item or else Item.Name/=Register_Name then
         return(Status=>Unsupported_Descriptor);
      end if;
      if Item.Length/=Register_Length then return(Status=>Bad_Length); end if;
      Access_Code:=Data(Data'First+Natural(Item.Payload+Access_Offset));
      if Access_Code>Maximum_Access_Code then return(Status=>Reserved_Access_Size); end if;
      for I in Natural range 0 .. Address_Bytes-1 loop
         Address:=Address or Interfaces.Shift_Left(Interfaces.Unsigned_64
           (Data(Data'First+Natural(Item.Payload+Address_Offset)+I)),Bits_Per_Byte*I);
      end loop;
      return(Status=>Decoded,Value=>(Space=>Data(Data'First+Natural(Item.Payload)),
        Width=>Data(Data'First+Natural(Item.Payload+Width_Offset)),
        Bit_Offset=>Data(Data'First+Natural(Item.Payload+Bit_Offset_Offset)),
        Access_Size=>Access_Code,Address=>Address),Next_Offset=>Item.Next_Offset);
   end Decode;
end ACPI_Resource_Data.Generic_Registers;
