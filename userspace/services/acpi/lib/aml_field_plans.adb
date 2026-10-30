pragma Ada_2022;
with ACPI_FADT.Transactions;
package body AML_Field_Plans with SPARK_Mode is
   use type AML_Decode.Byte;
   use type AML_Field_Layout.Field_Form;
   use type AML_Field_Layout.Region_Geometry;
   use type ACPI_FADT.Transactions.Access_Width;
   Access_Mask : constant AML_Decode.Byte := 16#0F#;
   Lock_Mask : constant AML_Decode.Byte := 16#10#;
   Update_Mask : constant AML_Decode.Byte := 16#60#;
   Reserved_Mask : constant AML_Decode.Byte := 16#80#;
   Last_Access : constant AML_Decode.Byte := 5;
   Preserve_Code : constant AML_Decode.Byte := 0;
   Ones_Code : constant AML_Decode.Byte := 16#20#;
   Zeros_Code : constant AML_Decode.Byte := 16#40#;
   Any_Code : constant AML_Decode.Byte := 0;
   Byte_Code : constant AML_Decode.Byte := 1;
   Word_Code : constant AML_Decode.Byte := 2;
   Dword_Code : constant AML_Decode.Byte := 3;
   Qword_Code : constant AML_Decode.Byte := 4;
   Narrow_Bits : constant AML_Decode.Integer_Value := 32;

   function Prepare (Field : AML_Field_Layout.Field_Geometry) return Plan is
      Width : ACPI_FADT.Transactions.Access_Width;
      Update : AML_Field_Datums.Update_Rule;
      Datums : AML_Field_Datums.Plan;
   begin
      if Field.Form /= AML_Field_Layout.Ordinary_Field or else Field.Has_Connection
        or else Field.Region.Space not in AML_Field_Layout.Memory_Space | AML_Field_Layout.IO_Space
        or else Field.Bit_Length = 0 or else Field.Bit_Length > Maximum_Bits
        or else Field.Region.Length = 0
        or else Field.Region.Base > AML_Decode.Integer_Value'Last - (Field.Region.Length - 1)
        or else (Field.Flags and Reserved_Mask) /= 0
        or else (Field.Flags and Lock_Mask) /= 0
        or else (Field.Flags and Access_Mask) > Last_Access
        or else Field.Attribute /= 0 or else Field.Access_Length /= 0
      then return (others => <>); end if;
      if Field.Region.Space = AML_Field_Layout.IO_Space
        and then (Field.Region.Base > AML_Field_Layout.IO_Last_Address
          or else Field.Region.Length - 1 > AML_Field_Layout.IO_Last_Address - Field.Region.Base)
      then return (others => <>); end if;
      case Field.Effective_Access is
         when Any_Code | Byte_Code => Width := ACPI_FADT.Transactions.Byte_Access;
         when Word_Code => Width := ACPI_FADT.Transactions.Word_Access;
         when Dword_Code => Width := ACPI_FADT.Transactions.Dword_Access;
         when Qword_Code =>
            if Field.Region.Space = AML_Field_Layout.IO_Space then
               return (others => <>);
            end if;
            Width := ACPI_FADT.Transactions.Qword_Access;
         when others => return (others => <>);
      end case;
      case Field.Flags and Update_Mask is
         when Preserve_Code => Update := AML_Field_Datums.Preserve;
         when Ones_Code => Update := AML_Field_Datums.Write_As_Ones;
         when Zeros_Code => Update := AML_Field_Datums.Write_As_Zeros;
         when others => return (others => <>);
      end case;
      Datums := AML_Field_Datums.Prepare
        (Field.Region.Base, Field.Region.Length,
         AML_Field_Datums.Bit_Position (Field.Bit_Offset),
         AML_Field_Datums.Bit_Length (Field.Bit_Length), Width);
      if not AML_Field_Datums.Valid (Datums)
        or else AML_Field_Datums.Count (Datums) = 0
        or else AML_Field_Datums.Count (Datums) > AML_Field_Datums.Datum_Index (Maximum_Datums)
      then return (others => <>); end if;
      return (Ready => True, Geometry => Field, Datums => Datums,
         Number => Datum_Count (AML_Field_Datums.Count (Datums)), Update => Update);
   end Prepare;

   function Datum_At (Item : Plan; Position : Datum_Position)
      return AML_Field_Datums.Datum is
     (AML_Field_Datums.At_Index (Item.Datums, AML_Field_Datums.Datum_Index (Position - 1)));

   function Matches (Item : Plan; Position : Datum_Position;
      Register : AML_Field_Layout.Register_Geometry) return Boolean is
   begin
      if not Valid (Item) or else Position > Count (Item) then return False; end if;
      declare
         Datum : constant AML_Field_Datums.Datum := Datum_At (Item, Position);
      begin
         return Register.Region = Item.Geometry.Region
           and then Register.Byte_Offset = AML_Decode.Integer_Value (Datum.Offset)
           and then Register.Width = Datum.Access_Width;
      end;
   end Matches;

   function Result_Form (Item : Plan; Width : AML_Decode.Integer_Width)
      return Read_Form is
     (if Item.Geometry.Bit_Length >
         (case Width is when AML_Decode.Bits_32 => Narrow_Bits,
                        when AML_Decode.Bits_64 => Maximum_Bits)
      then Buffer_Result else Integer_Result);

   function Buffer_Length (Item : Plan) return Positive is
     (Positive ((Item.Geometry.Bit_Length - 1) / AML_Field_Layout.Bits_Per_Byte + 1));
end AML_Field_Plans;
