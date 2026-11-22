pragma Ada_2022;
package body ACPI_Resource_Data.Address_Spaces.Extended_Descriptors with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_64;
   Extended_Name : constant Item_Name:=11;
   Extended_Length : constant Payload_Length:=53;
   General_Offset : constant Byte_Offset:=1;
   Specific_Offset : constant Byte_Offset:=2;
   Revision_Offset : constant Byte_Offset:=3;
   Reserved_Offset : constant Byte_Offset:=4;
   Numeric_Start : constant Byte_Offset:=5;
   Numeric_Bytes : constant Byte_Count:=8;
   Numeric_Count : constant Byte_Count:=5;
   Attribute_Offset : constant Byte_Offset:=Numeric_Start+Numeric_Count*Numeric_Bytes;
   Revision_One : constant Firmware_Tables.Byte:=1;
   General_Reserved : constant Firmware_Tables.Byte:=16#F0#;
   Memory_Reserved : constant Firmware_Tables.Byte:=16#C0#;
   IO_Reserved : constant Firmware_Tables.Byte:=16#CC#;
   Consumer_Bit : constant Firmware_Tables.Byte:=1;
   Decode_Bit : constant Firmware_Tables.Byte:=2;
   Minimum_Bit : constant Firmware_Tables.Byte:=4;
   Maximum_Bit : constant Firmware_Tables.Byte:=8;
   Write_Bit : constant Firmware_Tables.Byte:=1;
   Memory_Translation_Bit : constant Firmware_Tables.Byte:=32;
   IO_Translation_Bit : constant Firmware_Tables.Byte:=16;
   Sparse_Bit : constant Firmware_Tables.Byte:=32;
   Pair_Mask : constant Firmware_Tables.Byte:=3;
   Cache_Divisor : constant Firmware_Tables.Byte:=2;
   Memory_Type_Divisor : constant Firmware_Tables.Byte:=8;
   Memory_Code : constant Firmware_Tables.Byte:=0;
   IO_Code : constant Firmware_Tables.Byte:=1;
   Bus_Code : constant Firmware_Tables.Byte:=2;
   First_Vendor_Code : constant Firmware_Tables.Byte:=192;
   Bits_Per_Byte : constant Natural:=8;
   type Numeric_Field is (Granularity_Field, Minimum_Field, Maximum_Field, Translation_Field, Length_Field);
   function Qword (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Interfaces.Unsigned_64 with
      Pre=>Offset<=Byte_Count(Data'Length)
        and then Byte_Count(Data'Length)-Offset>=Numeric_Bytes;
   function Qword (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Interfaces.Unsigned_64 is
      Value : Interfaces.Unsigned_64:=0;
   begin
      for I in Natural range 0 .. Natural(Numeric_Bytes)-1 loop
         Value:=Value or Interfaces.Shift_Left(Interfaces.Unsigned_64
           (Data(Data'First+Natural(Offset)+I)),Bits_Per_Byte*I);
      end loop;
      return Value;
   end Qword;
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset)
      return Decode_Result is
      Item : constant Frame:=Read_Frame(Data,Offset);
      Type_Code, General, Specific : Firmware_Tables.Byte;
      Info : Specific_Information;
      Attributes : Attribute_Value;
   begin
      if Item.Status/=ACPI_Resource_Data.Decoded then return(Status=>Malformed_Frame); end if;
      if Item.Encoding/=Large_Item or else Item.Name/=Extended_Name then return(Status=>Unsupported_Descriptor); end if;
      if Item.Length/=Extended_Length then return(Status=>Bad_Length); end if;
      if Data(Data'First+Natural(Item.Payload+Revision_Offset))/=Revision_One then return(Status=>Unsupported_Revision); end if;
      if Data(Data'First+Natural(Item.Payload+Reserved_Offset))/=0 then return(Status=>Reserved_Byte_Set); end if;
      Type_Code:=Data(Data'First+Natural(Item.Payload));
      General:=Data(Data'First+Natural(Item.Payload+General_Offset));
      Specific:=Data(Data'First+Natural(Item.Payload+Specific_Offset));
      if (General and General_Reserved)/=0 then return(Status=>Reserved_Bits_Set); end if;
      case Type_Code is
         when Memory_Code =>
            if (Specific and Memory_Reserved)/=0 then return(Status=>Reserved_Bits_Set); end if;
            Info:=(Kind=>Memory_Range,Raw=>Specific,
              Declared_Writeable=>(Specific and Write_Bit)/=0,Memory_To_IO=>(Specific and Memory_Translation_Bit)/=0,
              Memory_Type=>Memory_Range_Claim'Val(Natural((Specific/Memory_Type_Divisor) and Pair_Mask)),
              Ignored_Cache_Bits=>Cache_Bits((Specific/Cache_Divisor) and Pair_Mask));
         when IO_Code =>
            if (Specific and IO_Reserved)/=0 then return(Status=>Reserved_Bits_Set); end if;
            if (Specific and Pair_Mask)=0 then return(Status=>Reserved_IO_Coverage); end if;
            Info:=(Kind=>IO_Range,Raw=>Specific,IO_To_Memory=>(Specific and IO_Translation_Bit)/=0,
              Sparse_Bit=>(Specific and Sparse_Bit)/=0,Coverage=>IO_Range_Claim'Val(Natural(Specific and Pair_Mask)-1));
         when Bus_Code =>
            if Specific/=0 then return(Status=>Reserved_Bits_Set); end if;
            Info:=(Kind=>Bus_Range,Raw=>Specific);
         when First_Vendor_Code .. Firmware_Tables.Byte'Last =>
            Info:=(Kind=>Vendor_Range,Raw=>Specific,Vendor_Type=>Type_Code);
         when others => return(Status=>Reserved_Resource_Type);
      end case;
      Attributes:=Attribute_Value(Qword(Data,Item.Payload+Attribute_Offset));
      if Info.Kind/=Memory_Range and then Attributes/=0 then return(Status=>Reserved_Attributes_Set); end if;
      declare
         First : constant Byte_Offset:=Item.Payload+Numeric_Start;
         function At_Field (Field : Numeric_Field) return Address_Value with
           Pre=>First<=Byte_Count(Data'Length)
             and then Numeric_Count*Numeric_Bytes<=Byte_Count(Data'Length)-First;
         function At_Field (Field : Numeric_Field) return Address_Value is
           (Address_Value(Qword(Data,First+Byte_Count(Numeric_Field'Pos(Field))*Numeric_Bytes)));
      begin
         return(Status=>Decoded,Information=>Info,General_Flags=>General,
           Role=>(if (General and Consumer_Bit)/=0 then Consumes_Resource else Produces_And_Consumes),
           Mode=>(if (General and Decode_Bit)/=0 then Subtractive_Decode else Positive_Decode),
           Minimum_Fixed=>(General and Minimum_Bit)/=0,Maximum_Fixed=>(General and Maximum_Bit)/=0,
           Granularity=>At_Field(Granularity_Field),Minimum=>At_Field(Minimum_Field),Maximum=>At_Field(Maximum_Field),
           Translation=>At_Field(Translation_Field),Length=>At_Field(Length_Field),Attributes=>Attributes,Next_Offset=>Item.Next_Offset);
      end;
   end Decode;
   function Geometry_Of (Item : Decode_Result) return Geometry.Parameters is
     (Width=>QWord_Width,Granularity=>Item.Granularity,Minimum=>Item.Minimum,
      Maximum=>Item.Maximum,Length=>Item.Length,Minimum_Fixed=>Item.Minimum_Fixed,Maximum_Fixed=>Item.Maximum_Fixed);
end ACPI_Resource_Data.Address_Spaces.Extended_Descriptors;
