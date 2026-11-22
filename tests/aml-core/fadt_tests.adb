with Ada.Text_IO;
with FADT_Register_Tests;
with Interfaces; use Interfaces;
with Firmware_Tables;
with ACPI_FADT; use ACPI_FADT;
procedure FADT_Tests is
   Signature : constant String := "FACP";
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "FADT" & Checks'Image; end if;
   end Check;
   function Expected (Offset, Count : Natural) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Count - 1 loop
         Value := Value * 256 + Unsigned_64 ((Offset + I) mod 256);
      end loop;
      return Value;
   end Expected;
   procedure Check_GAS (Item : Optional_Address; Offset, Length : Natural) is
   begin
      Check (Item.Present = (Length >= Offset + 12));
      if Item.Present then
         Check (Item.Value.Space = Unsigned_8 (Offset mod 256));
         Check (Item.Value.Width = Unsigned_8 ((Offset + 1) mod 256));
         Check (Item.Value.Bit_Offset = Unsigned_8 ((Offset + 2) mod 256));
         Check (Item.Value.Access_Size = Unsigned_8 ((Offset + 3) mod 256));
         Check (Item.Value.Address = Expected (Offset + 4, 8));
      else Check (Item.Value = Generic_Address'(others => <>)); end if;
   end Check_GAS;
   procedure Check_Pointer (Item : Optional_Pointer; Offset, Length : Natural) is
   begin
      Check (Item.Present = (Length >= Offset + 8));
      Check (Item.Value = (if Item.Present then Expected (Offset, 8) else 0));
   end Check_Pointer;
   procedure Seal (Data : in out Firmware_Tables.Bytes) is
      Sum : Unsigned_8 := 0;
   begin
      Data (Data'First + 9) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (Data'First + 9) := 0 - Sum;
   end Seal;
   Length_Offsets : constant array (Block_Kind) of Natural := [88, 88, 89, 89, 90, 91, 92, 93];
begin
   for Shift in 0 .. 2 loop
      for Length in 0 .. 300 loop
         declare
            First : constant Positive := (case Shift is when 0 => 1, when 1 => 101,
                                          when others => Positive'Last - 300);
            Data : Firmware_Tables.Bytes (First .. First + Length - 1);
            R : Result;
         begin
            for I in Data'Range loop Data (I) := Unsigned_8 ((I - First) mod 256); end loop;
            if Length >= 36 then
               for I in 0 .. 3 loop
                  Data (First + I) := Character'Pos (Signature (I + 1));
                  Data (First + 4 + I) := Unsigned_8 ((Length / 256 ** I) mod 256);
               end loop;
               Data (First + 8) := 6;
               Seal (Data);
            end if;
            R := Decode (Data);
            Check (R.Valid = (Length >= 116));
            if R.Valid then
               Check (R.Value.Revision = 6);
               Check (R.Value.FACS = Unsigned_32 (Expected (36, 4)));
               Check (R.Value.DSDT = Unsigned_32 (Expected (40, 4)));
               Check (R.Value.SCI = Unsigned_16 (Expected (46, 2)));
               Check (R.Value.SMI_Command = Unsigned_32 (Expected (48, 4)));
               Check (R.Value.Flags = Unsigned_32 (Expected (112, 4)));
               for Kind in Block_Kind loop
                  Check (R.Value.Legacy (Kind) = Unsigned_32 (Expected (56 + Block_Kind'Pos (Kind) * 4, 4)));
                  Check (R.Value.Lengths (Kind) = Unsigned_8 (Length_Offsets (Kind)));
                  Check_GAS (R.Value.Extended (Kind), 148 + Block_Kind'Pos (Kind) * 12, Length);
               end loop;
               Check_GAS (R.Value.Reset, 116, Length);
               Check (R.Value.Reset_Value_Present = (Length >= 129));
               Check (R.Value.Reset_Value = (if Length >= 129 then 128 else 0));
               Check (R.Value.ARM_Boot_Present = (Length >= 131));
               Check (R.Value.ARM_Boot = (if Length >= 131 then Unsigned_16 (Expected (129, 2)) else 0));
               Check (R.Value.Minor_Present = (Length >= 132));
               Check (R.Value.Minor = (if Length >= 132 then 131 else 0));
               Check_Pointer (R.Value.X_FACS, 132, Length); Check_Pointer (R.Value.X_DSDT, 140, Length);
               Check_GAS (R.Value.Sleep_Control, 244, Length); Check_GAS (R.Value.Sleep_Status, 256, Length);
               Check_Pointer (R.Value.Hypervisor, 268, Length);
               Data (First) := Character'Pos ('Z'); Seal (Data);
               Check (not Decode (Data).Valid);
               Data (First) := Character'Pos ('F'); Seal (Data);
               Data (First + 9) := Data (First + 9) + 1;
               Check (not Decode (Data).Valid);
               Seal (Data);
               Check (not Decode (Data (First .. Data'Last - 1)).Valid);
            end if;
         end;
      end loop;
   end loop;
   declare
      Limits : constant array (Positive range <>) of Unsigned_64 := [0, 1, 255, 256, 16#FFFF_FFFF#, Unsigned_64'Last];
      Item : Description;
   begin
      for L of Limits loop
         for X of Limits loop
            for Present in Boolean loop
               Check (Select_Pointer (255, (Present, X), L) =
                 (if Present and X /= 0 and X <= L then X elsif L >= 255 then 255 else 0));
            end loop;
         end loop;
      end loop;
      for Rev in Unsigned_8 loop
         Item.Revision := Rev; Item.Flags := 16#10_0000#;
         Check (Hardware_Reduced (Item) = (Rev >= 5));
         Item.Flags := 0; Check (not Hardware_Reduced (Item));
      end loop;
   end;
   FADT_Register_Tests;
   Ada.Text_IO.Put_Line ("ACPI-FADT-CHECK: PASS" & Checks'Image);
end FADT_Tests;
