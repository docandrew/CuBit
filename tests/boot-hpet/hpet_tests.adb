with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables; use Firmware_Tables;
with Firmware_Tables.HPET; use Firmware_Tables.HPET;

procedure HPET_Tests is
   Data : Bytes (1 .. 56) := [others => 0];
   Limit : constant Address_Value := 2 ** 48 - 1;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Seal (B : in out Bytes) is
      Sum : Byte := 0;
   begin
      B (B'First + 9) := 0;
      for V of B loop Sum := Sum + V; end loop;
      B (B'First + 9) := -Sum;
   end Seal;
begin
   for I in 1 .. 4 loop
      Data (I) := Character'Pos (String'("HPET") (I));
   end loop;
   Data (5) := 56;
   Data (9) := 1;
   Data (47) := 16#D0#;
   Data (48) := 16#FE#;
   Seal (Data);
   Check (Decode (Data, Limit) = (True, 16#FED0_0000#));
   Check (Decode (Data, 16#FED0_03FF#).Valid);
   Check (not Decode (Data, 16#FED0_03FE#).Valid);
   Check (not Decode (Data, 0).Valid);
   declare
      Shifted : constant Bytes (Positive'Last - 55 .. Positive'Last) := Data;
   begin Check (Decode (Shifted, Limit) = Decode (Data, Limit)); end;
   for N in 0 .. 55 loop Check (not Decode (Data (1 .. N), Limit).Valid); end loop;
   for I in Data'Range loop
      for D in Byte range 1 .. 255 loop
         declare
            Bad : Bytes := Data;
         begin
            Bad (I) := Bad (I) + D;
            Check (not Decode (Bad, Limit).Valid);
         end;
      end loop;
   end loop;
   for Offset in 40 .. 43 loop
      for V in Byte loop
         declare
            B : Bytes := Data;
            Allowed : constant Boolean :=
              (case Offset is
                 when 40 | 42 => V = 0,
                 when 41 => V in 0 | 64,
                 when others => V in 0 | 4);
         begin
            B (1 + Offset) := V; Seal (B);
            Check (Decode (B, Limit).Valid = Allowed);
         end;
      end loop;
   end loop;
   -- Recompute checksums so these exercise semantic rejection, not merely
   -- checksum admission. Include address overflow and unaligned windows.
   for Offset in 44 .. 51 loop
      declare
         B : Bytes := Data;
      begin
         B (1 + Offset) := 255; Seal (B);
         if Offset in 44 | 45 | 50 | 51 then
            Check (not Decode (B, Limit).Valid);
         end if;
      end;
   end loop;
   declare
      B : Bytes := Data;
   begin
      B (45 .. 52) := [others => 0]; Seal (B);
      Check (not Decode (B, Limit).Valid);
      B := Data; B (9) := 2; Seal (B);
      Check (not Decode (B, Limit).Valid);
      B := Data; B (5) := 55; Seal (B (1 .. 55));
      Check (not Decode (B, Limit).Valid);
   end;
   for V in Unsigned_32 range 0 .. 65_535 loop
      Check (Quiescent_Config (V) = V - (V mod 4));
      Check (Quiescent_Config (V or 16#FFFF_0000#) =
             ((V - (V mod 4)) or 16#FFFF_0000#));
   end loop;
   Put_Line ("HPET checks:" & Checks'Image);
end HPET_Tests;
