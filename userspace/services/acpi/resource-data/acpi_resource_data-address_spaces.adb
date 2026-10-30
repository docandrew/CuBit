pragma Ada_2022;
package body ACPI_Resource_Data.Address_Spaces with SPARK_Mode is
   use type Firmware_Tables.Byte;
   use type Interfaces.Unsigned_64;
   Word_Name : constant Item_Name:=8;
   DWord_Name : constant Item_Name:=7;
   QWord_Name : constant Item_Name:=10;
   Prefix_Bytes : constant Byte_Count:=3;
   Field_Count : constant Byte_Count:=5;
   Bits_Per_Byte : constant Natural:=8;
   General_Offset : constant Byte_Offset:=1;
   Specific_Offset : constant Byte_Offset:=2;
   General_Reserved : constant Firmware_Tables.Byte:=16#F0#;
   Memory_Reserved : constant Firmware_Tables.Byte:=16#C0#;
   IO_Reserved : constant Firmware_Tables.Byte:=16#CC#;
   Write_Bit : constant Firmware_Tables.Byte:=1;
   Decode_Bit : constant Firmware_Tables.Byte:=2;
   Minimum_Bit : constant Firmware_Tables.Byte:=4;
   Maximum_Bit : constant Firmware_Tables.Byte:=8;
   IO_Translation_Bit : constant Firmware_Tables.Byte:=16;
   Memory_Translation_Bit : constant Firmware_Tables.Byte:=32;
   Sparse_Bit : constant Firmware_Tables.Byte:=32;
   Two_Bit_Mask : constant Firmware_Tables.Byte:=3;
   Cache_Divisor : constant Firmware_Tables.Byte:=2;
   Memory_Type_Divisor : constant Firmware_Tables.Byte:=8;
   Memory_Type_Code : constant Firmware_Tables.Byte:=0;
   IO_Type_Code : constant Firmware_Tables.Byte:=1;
   Bus_Type_Code : constant Firmware_Tables.Byte:=2;
   PCC_Type_Code : constant Firmware_Tables.Byte:=10;
   First_Vendor_Code : constant Firmware_Tables.Byte:=192;
   type Numeric_Field is (Granularity_Field, Minimum_Field, Maximum_Field,
      Translation_Field, Length_Field);
   subtype Datum_Bytes is Positive range 2 .. 8;
   function Read_Value (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Width : Datum_Bytes) return Address_Value with
      Pre=>Offset<=Byte_Count(Data'Length)
        and then Byte_Count(Data'Length)-Offset>=Byte_Count(Width);
   function Read_Value (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      Width : Datum_Bytes) return Address_Value is
      Value : Interfaces.Unsigned_64:=0;
   begin
      for I in Natural range 0 .. Width-1 loop
         Value:=Value or Interfaces.Shift_Left(Interfaces.Unsigned_64
           (Data(Data'First+Natural(Offset)+I)),Bits_Per_Byte*I);
      end loop;
      return Address_Value(Value);
   end Read_Value;
   function Decode (Data : Firmware_Tables.Bytes; Offset : Byte_Offset;
      QWord_Indices : QWord_Source_Index_Policy:=Preserve_Unnegotiated)
      return Decode_Result is
      Item : constant Frame:=Read_Frame(Data,Offset);
      Width : Address_Width;
      Bytes : Datum_Bytes;
      Fixed_Bytes : Byte_Count;
      Resource_Type, General, Specific : Firmware_Tables.Byte;
      Info : Type_Information;
      Source : Source_Field := (Present=>False);
   begin
      if Item.Status/=ACPI_Resource_Data.Decoded then return(Status=>Malformed_Frame); end if;
      if Item.Encoding/=Large_Item then return(Status=>Unsupported_Descriptor); end if;
      case Item.Name is
         when Word_Name => Width:=Word_Width; Bytes:=2;
         when DWord_Name => Width:=DWord_Width; Bytes:=4;
         when QWord_Name => Width:=QWord_Width; Bytes:=8;
         when others => return(Status=>Unsupported_Descriptor);
      end case;
      Fixed_Bytes:=Prefix_Bytes+Field_Count*Byte_Count(Bytes);
      if Byte_Count(Item.Length)<Fixed_Bytes then return(Status=>Bad_Length); end if;
      Resource_Type:=Data(Data'First+Natural(Item.Payload));
      General:=Data(Data'First+Natural(Item.Payload+General_Offset));
      Specific:=Data(Data'First+Natural(Item.Payload+Specific_Offset));
      if (General and General_Reserved)/=0 then return(Status=>Reserved_Bits_Set); end if;
      case Resource_Type is
         when Memory_Type_Code =>
            if (Specific and Memory_Reserved)/=0 then return(Status=>Reserved_Bits_Set); end if;
            Info:=(Kind=>Memory_Range,Raw=>Specific,
              Declared_Writeable=>(Specific and Write_Bit)/=0,Memory_To_IO=>(Specific and Memory_Translation_Bit)/=0,
              Cache=>Memory_Cache_Claim'Val(Natural((Specific/Cache_Divisor) and Two_Bit_Mask)),
              Memory_Type=>Memory_Range_Claim'Val(Natural((Specific/Memory_Type_Divisor) and Two_Bit_Mask)));
         when IO_Type_Code =>
            if (Specific and IO_Reserved)/=0 then return(Status=>Reserved_Bits_Set); end if;
            if (Specific and Two_Bit_Mask)=0 then return(Status=>Reserved_IO_Coverage); end if;
            Info:=(Kind=>IO_Range,Raw=>Specific,
              IO_To_Memory=>(Specific and IO_Translation_Bit)/=0,Sparse_Bit=>(Specific and Sparse_Bit)/=0,
              Coverage=>IO_Range_Claim'Val(Natural(Specific and Two_Bit_Mask)-1));
         when Bus_Type_Code =>
            if Specific/=0 then return(Status=>Reserved_Bits_Set); end if;
            Info:=(Kind=>Bus_Range,Raw=>Specific);
         when PCC_Type_Code => Info:=(Kind=>PCC_Range,Raw=>Specific);
         when First_Vendor_Code .. Firmware_Tables.Byte'Last => Info:=(Kind=>Vendor_Range,Raw=>Specific,Vendor_Type=>Resource_Type);
         when others => return(Status=>Reserved_Resource_Type);
      end case;
      if Fixed_Bytes<Byte_Count(Item.Length) then
         declare
            Tail : constant Byte_Offset:=Item.Payload+Fixed_Bytes;
            Name_First : constant Byte_Offset:=Tail+1;
         begin
            if Item.Next_Offset-Name_First<1 then return(Status=>Bad_Source); end if;
            if Width=QWord_Width and then QWord_Indices=Require_Zero
              and then Data(Data'First+Natural(Tail))/=0 then
               return(Status=>Bad_Source_Index);
            end if;
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
      declare
         First : constant Byte_Offset:=Item.Payload+Prefix_Bytes;
         function At_Field (Field : Numeric_Field) return Address_Value with
           Pre=>First<=Byte_Count(Data'Length)
             and then Field_Count*Byte_Count(Bytes)<=Byte_Count(Data'Length)-First;
         function At_Field (Field : Numeric_Field) return Address_Value is
           (Read_Value(Data,First+Byte_Count(Numeric_Field'Pos(Field))*Byte_Count(Bytes),Bytes));
      begin
         return (Status=>Decoded,Width=>Width,Information=>Info,General_Flags=>General,
           Mode=>(if (General and Decode_Bit)/=0 then Subtractive_Decode else Positive_Decode),
           Minimum_Fixed=>(General and Minimum_Bit)/=0,Maximum_Fixed=>(General and Maximum_Bit)/=0,
           Granularity=>At_Field(Granularity_Field),Minimum=>At_Field(Minimum_Field),
           Maximum=>At_Field(Maximum_Field),Translation=>At_Field(Translation_Field),Length=>At_Field(Length_Field),
           Source=>Source,Next_Offset=>Item.Next_Offset);
      end;
   end Decode;
end ACPI_Resource_Data.Address_Spaces;
