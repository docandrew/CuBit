with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables; use Firmware_Tables;

procedure Table_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "check" & Checks'Image;
      end if;
   end Check;

   procedure Text (Data : in out Bytes; Value : String) is
   begin
      for I in Value'Range loop
         Data (Data'First + I - Value'First) := Character'Pos (Value (I));
      end loop;
   end Text;

   procedure Word (Data : in out Bytes; Offset : Natural; Value : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         Data (Data'First + Offset + I) := Byte
           (Shift_Right (Value, 8 * I) and 255);
      end loop;
   end Word;

   procedure Seal (Data : in out Bytes; Offset : Natural) is
      Total : Byte := 0;
   begin
      Data (Data'First + Offset) := 0;
      for B of Data loop
         Total := Total + B;
      end loop;
      Data (Data'First + Offset) := -Total;
   end Seal;

   Legacy : Bytes (1 .. 20) := [others => 0];
   Modern : Bytes (1 .. 40) := [others => 0];
   Table : Bytes (1 .. 64) := [others => 0];
begin
   Text (Legacy, "RSD PTR ");
   Word (Legacy, 16, 16#1234_5678#);
   Seal (Legacy, 8);
   declare
      R : constant Root_Result := Read_Root (Legacy);
   begin
      Check (R.Status = Accepted and then R.Kind = RSDT
             and then R.Address = 16#1234_5678# and then R.Extent = 20);
   end;
   for N in 0 .. 19 loop
      Check (Read_Root (Legacy (1 .. N)).Status = Truncated);
   end loop;
   Modern (1 .. 20) := Legacy;
   Modern (16) := 2;
   Word (Modern, 20, 40);
   Word (Modern, 24, 16#8765_4321#);
   Word (Modern, 28, 16#0000_0001#);
   Seal (Modern (1 .. 20), 8);
   Seal (Modern, 32);
   declare
      R : constant Root_Result := Read_Root (Modern);
      Shifted : constant Bytes (101 .. 140) := Modern;
   begin
      Check (R.Status = Accepted and then R.Kind = XSDT
             and then R.Address = 16#1_8765_4321# and then R.Extent = 40);
      Check (Read_Root (Shifted) = R);
   end;
   for N in 20 .. 39 loop
      Check (Read_Root (Modern (1 .. N)).Status = Truncated);
   end loop;
   -- Every single-byte corruption must be rejected, including extension bytes.
   for I in Modern'Range loop
      for Delta_Byte in Byte range 1 .. 255 loop
         declare
            Mutant : Bytes := Modern;
         begin
            Mutant (I) := Mutant (I) + Delta_Byte;
            Check (Read_Root (Mutant).Status /= Accepted);
         end;
      end loop;
   end loop;
   declare
      M : Bytes := Modern;
   begin
      Word (M, 24, 0);
      Word (M, 28, 0);
      Seal (M, 32);
      Check (Read_Root (M).Kind = RSDT);
      Word (M, 16, 0);
      Seal (M (1 .. 20), 8);
      Seal (M, 32);
      Check (Read_Root (M).Status = Missing_Root);
      M := Modern;
      M (16) := 1;
      Seal (M (1 .. 20), 8);
      Check (Read_Root (M).Status = Unsupported_Revision);
      M (16) := 3;
      Seal (M (1 .. 20), 8);
      Seal (M, 32);
      Check (Read_Root (M).Status = Accepted);
      Word (M, 20, 35);
      Check (Read_Root (M).Status = Invalid_Length);
      Word (M, 20, Unsigned_32'Last);
      Check (Read_Root (M).Status = Truncated);
   end;
   Text (Table, "DMAR");
   Word (Table, 4, 64);
   Table (9) := 1;
   Seal (Table, 9);
   declare
      R : constant Table_Result := Read_Table (Table, "DMAR");
      Shifted : constant Bytes (Positive'Last - 63 .. Positive'Last) := Table;
      Padded : Bytes (1 .. 68) := [others => 255];
   begin
      Check (R.Status = Accepted and then R.Extent = 64
             and then R.Revision = 1);
      Check (Read_Table (Shifted, "DMAR") = R);
      Padded (1 .. 64) := Table;
      Check (Read_Table (Padded, "DMAR") = R);
      Check (Read_Table (Table, "XSDT").Status = Wrong_Signature);
   end;
   for N in 0 .. 63 loop
      Check (Read_Table (Table (1 .. N), "DMAR").Status = Truncated);
   end loop;
   for I in Table'Range loop
      for Delta_Byte in Byte range 1 .. 255 loop
         declare
            Mutant : Bytes := Table;
         begin
            Mutant (I) := Mutant (I) + Delta_Byte;
            Check (Read_Table (Mutant, "DMAR").Status /= Accepted);
         end;
      end loop;
   end loop;
   for Length in Unsigned_32 range 0 .. 35 loop
      Word (Table, 4, Length);
      Check (Read_Table (Table, "DMAR").Status = Invalid_Length);
   end loop;
   Word (Table, 4, Unsigned_32'Last);
   Check (Read_Table (Table, "DMAR").Status = Truncated);
   Put_Line ("ACPI table admission:" & Checks'Image & " checks PASS");
end Table_Tests;
