with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_FADT; use ACPI_FADT;
with ACPI_FADT.Transactions; use ACPI_FADT.Transactions;
procedure Transaction_Tests is
   Checks : Natural := 0;
   P : Plan;
   Item : Generic_Address := (Space => 0, Address => 4096, others => 0);
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   First_Byte, Last_Byte : Natural;
begin
   -- Independent bit-by-bit oracle finds transactions intersecting the field.
   for Width in Access_Width loop
      for Offset in Unsigned_8 loop
         for Bits in Unsigned_8 range 1 .. 255 loop
            Item.Bit_Offset := Offset; Item.Width := Bits;
            P := Describe (Item, Width, 4096, 8191);
            Check (P.Code = Ready);
            First_Byte := 64; Last_Byte := 0;
            for Bit in Natural (Offset) .. Natural (Offset) + Natural (Bits) - 1 loop
               declare
                  Byte_Index : constant Natural := Bit / 8;
                  Transaction_Start : constant Natural := Byte_Index - Byte_Index mod Octets (Width);
               begin
                  First_Byte := Natural'Min (First_Byte, Transaction_Start);
                  Last_Byte := Natural'Max (Last_Byte, Transaction_Start + Octets (Width) - 1);
               end;
            end loop;
            Check (P.First = 4096 + Unsigned_64 (First_Byte) and P.Last = 4096 + Unsigned_64 (Last_Byte));
            for I in 1 .. P.Count loop
               Check (Address_At (P, I) = P.First + Unsigned_64 ((I - 1) * Octets (Width)));
            end loop;
            Check (Describe (Item, Width, P.First + 1, P.Last).Code = Outside_Range);
            Check (Describe (Item, Width, P.First, P.Last - 1).Code = Outside_Range);
         end loop;
      end loop;
   end loop;
   Item := (Space => 0, Address => 4096, Width => 1, others => 0);
   Check (Describe (Item, Qword_Access, 4096, 4096).Code = Outside_Range);
   Check (Describe (Item, Qword_Access, 4096, 4103).Code = Ready);
   Item.Address := 4097;
   Check (Describe (Item, Qword_Access, 0, Unsigned_64'Last).Code = Misaligned);
   Item.Address := Unsigned_64'Last;
   Check (Describe (Item, Byte_Access, 0, Unsigned_64'Last).Code = Ready);
   Item.Bit_Offset := 8;
   Check (Describe (Item, Byte_Access, 0, Unsigned_64'Last).Code = Outside_Range);
   Item := (Space => 1, Address => 65535, Width => 8, others => 0);
   Check (Describe (Item, Byte_Access, 0, Unsigned_64'Last).Code = Ready);
   Item.Width := 9;
   Check (Describe (Item, Byte_Access, 0, Unsigned_64'Last).Code = Outside_Range);
   Item.Address := 4096;
   Check (Describe (Item, Qword_Access, 0, Unsigned_64'Last).Code = Unsupported_IO_Width);
   Item.Space := 2;
   Check (Describe (Item, Byte_Access, 0, Unsigned_64'Last).Code = Unsupported_Space);
   Item.Space := 0; Item.Width := 0;
   Check (Describe (Item, Byte_Access, 0, Unsigned_64'Last).Code = Empty_Field);
   Item.Address := 0;
   Check (Describe (Item, Byte_Access, 0, Unsigned_64'Last).Code = Absent);
   Ada.Text_IO.Put_Line ("ACPI-TRANSACTION: PASS" & Checks'Image);
end Transaction_Tests;
