with Ada.Text_IO;
with AML_Decode;
with AML_Table_Backing;
with Firmware_Tables;
procedure Table_Field_Span_Tests is
   package B renames AML_Table_Backing;
   use type AML_Decode.Status;
   use type AML_Decode.Byte;
   Capacity : constant := 8_256;
   Input : aliased B.State (1, Capacity);
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function Expected (Offset, Count : Natural) return AML_Decode.Byte is
      Value : Natural := 0;
   begin
      for I in 1 .. Count loop
         if (Natural (Input.Data ((Offset + I - 1) / 8 + 1)) /
             (2 ** ((Offset + I - 1) mod 8))) mod 2 /= 0
         then Value := Value + 2 ** (I - 1); end if;
      end loop;
      return AML_Decode.Byte (Value);
   end Expected;
   R : B.Field_Result;
   W : B.Window_Result;
   Lengths : constant array (Positive range <>) of Natural := [0, 1, 4, 8, 1_024, 1_025, 8_192];
begin
   Input.Count := 1;
   Input.Tables (1) := (Offset => 0, Extent => Capacity);
   for I in Input.Data'Range loop Input.Data (I) := Firmware_Tables.Byte ((I * 137) mod 256); end loop;
   for Offset in 0 .. 7 loop
      for Length of Lengths loop
         for Tail in 0 .. 7 loop
            declare
               Bits : constant Natural := Length * 8 + Tail;
            begin
               R := B.Validate_Field (Input, 1, Capacity, Offset, Bits);
               Check (R.Status = AML_Decode.Accepted);
               Check (B.Field_Length (R.Field) = Length + (if Tail = 0 then 0 else 1));
               for I in 1 .. B.Field_Length (R.Field) loop
                  Check (B.Read_Byte (Input, R.Field, I - 1) =
                    Expected (Offset + (I - 1) * 8, Natural'Min (8, Bits - (I - 1) * 8)));
               end loop;
            end;
         end loop;
      end loop;
   end loop;
   R := B.Validate_Field (Input, 1, Capacity, Capacity * 8, 0);
   Check (R.Status = AML_Decode.Accepted and then B.Field_Length (R.Field) = 0);
   Check (B.Validate_Field (Input, 1, Capacity, Capacity * 8, 1).Status = AML_Decode.Truncated);
   Check (B.Validate_Field (Input, 1, Capacity, Natural'Last, 1).Status = AML_Decode.Truncated);
   Check (B.Validate_Field (Input, 1, Capacity, 1, Natural'Last).Status = AML_Decode.Truncated);
   W := B.Read_Window (Input, 1, Capacity, 3, 8_192 * 8 + 3, 8_192, 1);
   Check (W.Status = AML_Decode.Accepted and then W.Total_Length = 8_193
     and then W.First_Byte = 8_192 and then W.Length = 1
     and then W.Content (1) = Expected (3 + 8_192 * 8, 3));
   Check (B.Read_Window (Input, 1, Capacity, 0, 8_192 * 8, 8_192, 0).Status = AML_Decode.Accepted);
   Check (B.Read_Window (Input, 1, Capacity, 0, 8_192 * 8, 8_192, 1).Status = AML_Decode.Truncated);
   Check (B.Read_Window (Input, 1, Capacity, 0, 8_192 * 8, 0, 1_025).Status = AML_Decode.Limit_Exceeded);
   Check (B.Validate_Field (Input, 1, Capacity - 1, 0, 8).Status = AML_Decode.Malformed);
   Input.Count := 2;
   Check (B.Validate_Field (Input, 1, Capacity, 0, 8).Status = AML_Decode.Malformed);
   Input.Count := 0;
   Check (B.Validate_Field (Input, 1, Capacity, 0, 8).Status = AML_Decode.Malformed);
   Input.Count := 1;
   Input.Tables (1).Offset := Natural'Last;
   Check (B.Validate_Field (Input, 1, Capacity, 0, 8).Status = AML_Decode.Malformed);
   Input.Tables (1) := (Offset => 0, Extent => Firmware_Tables.Table_Header_Size - 1);
   Check (B.Validate_Field (Input, 1, Firmware_Tables.Table_Header_Size - 1, 0, 8).Status = AML_Decode.Malformed);
   Ada.Text_IO.Put_Line ("Table field span checks" & Checks'Image);
end Table_Field_Span_Tests;
