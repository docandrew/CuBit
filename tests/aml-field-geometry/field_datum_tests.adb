pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with AML_Field_Datums;
with ACPI_FADT.Transactions;
procedure Field_Datum_Tests is
   use AML_Field_Datums;
   use type Scalar;
   use type ACPI_FADT.Transactions.Access_Width;
   package T renames ACPI_FADT.Transactions;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "datum check" & Checks'Image; end if;
   end Check;
   P : Plan;
   procedure Model (W : Width; Start, Length : Natural) is
      Unit_Bits : constant Positive := 8 * T.Octets (W);
      Last : constant Natural := Start + Length - 1;
      Expected_Datums : constant Natural := Last / Unit_Bits - Start / Unit_Bits + 1;
      Item : Datum;
      Mask, Expected, Old, Value, Bit : Scalar;
      First_Selected, Selected_Count : Natural;
   begin
      P := Prepare (16#1000#, 128, Bit_Position (Start), Bit_Length (Length), W);
      Check (Valid (P) and then Count (P) = Datum_Index (Expected_Datums));
      for I in 0 .. Expected_Datums - 1 loop
         Item := At_Index (P, Datum_Index (I));
         Mask := 0; Selected_Count := 0; First_Selected := 0;
         for B in 0 .. Unit_Bits - 1 loop
            declare
               Global_Bit : constant Natural := (Start / Unit_Bits + I) * Unit_Bits + B;
            begin
               if Global_Bit in Start .. Last then
                  if Selected_Count = 0 then First_Selected := Global_Bit; end if;
                  Selected_Count := Selected_Count + 1;
                  Mask := Mask or Interfaces.Shift_Left (Scalar'(1), B);
               end if;
            end;
         end loop;
         Check (Item.Mask = Mask and then Item.Bits = Selected_Count
           and then Item.Source_Bit = Bit_Position (First_Selected - Start)
           and then Item.First_Bit = First_Selected mod Unit_Bits
           and then Item.Offset = Byte_Position ((Start / Unit_Bits + I) * T.Octets (W))
           and then Item.Access_Width = W);
         Old := 16#A55A_F00F_1234_5678#; Value := 16#3CC3_9696_FEDC_BA98#;
         for Rule in Update_Rule loop
            Expected := 0;
            for B in 0 .. Unit_Bits - 1 loop
               Bit := Interfaces.Shift_Left (Scalar'(1), B);
               if (Mask and Bit) /= 0 then
                  Expected := Expected or (Value and Bit);
               elsif Rule = Preserve then Expected := Expected or (Old and Bit);
               elsif Rule = Write_As_Ones then Expected := Expected or Bit;
               end if;
            end loop;
            Check (Merge (W, Item.Mask, Old, Value, Rule) = Expected);
            Check (Needs_Read (W, Item.Mask, Rule) =
              (Rule = Preserve and then Selected_Count /= Unit_Bits));
         end loop;
      end loop;
   end Model;
begin
   -- Exhaust every start bit and one-/two-datum field length, including crossings.
   for W in Width loop
      for Start in 0 .. 8 * T.Octets (W) - 1 loop
         for Length in 1 .. 16 * T.Octets (W) loop Model (W, Start, Length); end loop;
      end loop;
   end loop;
   P := Prepare (0, 0, 0, 1, T.Byte_Access); Check (not Valid (P));
   P := Prepare (0, 1, 0, 0, T.Byte_Access); Check (not Valid (P));
   P := Prepare (1, 8, 0, 1, T.Word_Access); Check (not Valid (P));
   P := Prepare (0, 7, 0, 1, T.Qword_Access); Check (not Valid (P));
   P := Prepare (0, 8, 63, 2, T.Qword_Access); Check (not Valid (P));
   P := Prepare (Scalar'Last, 2, 0, 1, T.Byte_Access); Check (not Valid (P));
   P := Prepare (Scalar'Last, 1, 0, 8, T.Byte_Access); Check (Valid (P));
   P := Prepare (0, Scalar'Last, Bit_Position'Last, 2, T.Byte_Access); Check (not Valid (P));
   P := Prepare (0, Scalar'Last, Bit_Position'Last, 1, T.Qword_Access);
   Check (Valid (P));
   declare Item : constant Datum := At_Index (P, 0); begin
      Check (Item.Mask = 16#8000_0000_0000_0000# and then Item.Bits = 1
         and then Item.Source_Bit = 0 and then Item.First_Bit = 63);
   end;
   P := Prepare (1, Scalar'Last, 0, Bit_Length'Last, T.Byte_Access);
   Check (Valid (P) and then Count (P) = Datum_Index (Scalar'Last / 8 + 1));
   declare Item : constant Datum := At_Index (P, Count (P) - 1); begin
      Check (Item.Bits = 7 and then Item.Mask = 16#7F#);
   end;
   P := Prepare (2, Scalar'Last, 0, 1, T.Byte_Access); Check (not Valid (P));
   Ada.Text_IO.Put_Line ("Field datum checks" & Checks'Image);
end Field_Datum_Tests;
