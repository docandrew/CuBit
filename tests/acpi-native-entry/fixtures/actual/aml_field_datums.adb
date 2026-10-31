pragma Ada_2022;
with Interfaces;
package body AML_Field_Datums with SPARK_Mode is
   function Width_Mask (Access_Width : Width) return Scalar is
      N : constant Positive := Bits_Per_Byte * ACPI_FADT.Transactions.Octets (Access_Width);
   begin
      if N = 64 then return Scalar'Last; end if;
      return Interfaces.Shift_Left (Scalar'(1), N) - 1;
   end Width_Mask;
   function Prepare (Base, Region_Length : Scalar; Start : Bit_Position;
      Length : Bit_Length; Access_Width : Width) return Plan is
      Size : constant Scalar := Scalar (ACPI_FADT.Transactions.Octets (Access_Width));
      First_Block, Last_Block : Scalar;
      Last_Bit, First_Byte, Last_Byte : Scalar;
   begin
      if Region_Length = 0 or else Length = 0 then return (others => <>); end if;
      if Base > Scalar'Last - (Region_Length - 1)
        or else Scalar (Start) > Scalar'Last - (Scalar (Length) - 1)
      then return (others => <>); end if;
      Last_Bit := Scalar (Start) + (Scalar (Length) - 1);
      First_Block := Block_At (Scalar (Start), Access_Width);
      Last_Block := Block_At (Last_Bit, Access_Width);
      pragma Assert (Last_Block >= First_Block);
      First_Byte := First_Block * Size;
      Last_Byte := Last_Block * Size;
      if Last_Byte >= Region_Length or else Size > Region_Length - Last_Byte
      then return (others => <>); end if;
      if not ACPI_FADT.Transactions.Aligned (Base + First_Byte, Access_Width)
      then return (others => <>); end if;
      return (Ready => True, First_Byte => First_Byte,
         Start_Bit => Scalar (Start), Last_Bit => Last_Bit,
         Number => Datum_Index (Last_Block - First_Block + 1),
         Access_Width => Access_Width);
   end Prepare;
   function At_Index (Item : Plan; Index : Datum_Index) return Datum is
      Size : constant Scalar := Scalar (ACPI_FADT.Transactions.Octets (Item.Access_Width));
      Unit_Bits : constant Datum_Bit_Width := Datum_Bits (Item.Access_Width);
      Block_Number : constant Scalar :=
        Block_At (Item.Start_Bit, Item.Access_Width) + Scalar (Index);
      Offset : constant Scalar := Block_Number * Size;
      First : constant Scalar := Block_First (Block_Number, Item.Access_Width);
      Shift : constant Local_Bit := (if Index = 0 then
        Local_Position (Item.Start_Bit, Item.Access_Width) else 0);
      Low : constant Scalar := (if Index = 0 then Item.Start_Bit else First);
      Available_Bits : constant Local_Length := Unit_Bits - Shift;
      Remaining : constant Scalar := Item.Last_Bit - Low;
      N : constant Local_Length := (if Remaining >= Scalar (Available_Bits) then
        Available_Bits else Local_Length (Remaining + 1));
      Mask : Scalar;
   begin
      case Item.Access_Width is
         when ACPI_FADT.Transactions.Byte_Access =>
            pragma Assert (Block_Number <= Item.Last_Bit / Byte_Bits);
            pragma Assert (Block_Number >= Item.Start_Bit / Byte_Bits);
            pragma Assert (First <= Item.Last_Bit);
            pragma Assert (if Index > 0 then First >= Item.Start_Bit);
         when ACPI_FADT.Transactions.Word_Access =>
            pragma Assert (Block_Number <= Item.Last_Bit / Word_Bits);
            pragma Assert (Block_Number >= Item.Start_Bit / Word_Bits);
            pragma Assert (First <= Item.Last_Bit);
            pragma Assert (if Index > 0 then First >= Item.Start_Bit);
         when ACPI_FADT.Transactions.Dword_Access =>
            pragma Assert (Block_Number <= Item.Last_Bit / Dword_Bits);
            pragma Assert (Block_Number >= Item.Start_Bit / Dword_Bits);
            pragma Assert (First <= Item.Last_Bit);
            pragma Assert (if Index > 0 then First >= Item.Start_Bit);
         when ACPI_FADT.Transactions.Qword_Access =>
            pragma Assert (Block_Number <= Item.Last_Bit / Qword_Bits);
            pragma Assert (Block_Number >= Item.Start_Bit / Qword_Bits);
            pragma Assert (First <= Item.Last_Bit);
            pragma Assert (if Index > 0 then First >= Item.Start_Bit);
      end case;
      pragma Assert (Low >= Item.Start_Bit);
      pragma Assert (Low <= Item.Last_Bit);
      if N = 64 then Mask := Scalar'Last;
      else Mask := Interfaces.Shift_Left (Scalar'(1), N) - 1; end if;
      return (Offset => Byte_Position (Offset), Source_Bit => Bit_Position (Low - Item.Start_Bit),
         First_Bit => Shift, Bits => N, Mask => Interfaces.Shift_Left (Mask, Shift),
         Access_Width => Item.Access_Width);
   end At_Index;
   function Merge (Access_Width : Width; Mask, Old_Value, Value : Scalar;
      Rule : Update_Rule) return Scalar is
      W : constant Scalar := Width_Mask (Access_Width);
      Selected : constant Scalar := Mask and W;
      Outside : constant Scalar := W and not Selected;
   begin
      return (Value and Selected) or
         (case Rule is when Preserve => Old_Value and Outside,
          when Write_As_Ones => Outside, when Write_As_Zeros => 0);
   end Merge;
   function Needs_Read (Access_Width : Width; Mask : Scalar;
      Rule : Update_Rule) return Boolean is
     (Rule = Preserve and then (Mask and Width_Mask (Access_Width)) /= Width_Mask (Access_Width));
end AML_Field_Datums;
