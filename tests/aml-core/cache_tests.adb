pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Page_Cache; use Firmware_Page_Cache;
procedure Cache_Tests is
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   Raw, Base, V, Flags : Unsigned_64;
   D : Description;
   Max : constant Unsigned_64 := 2 ** 52 - 1;
begin
   for Kind in Leaf_Kind loop
      for Cache in Cache_Index loop
         Base := 16#8000_0000#;
         Raw := Base + 1 + (if Kind = Page_4K then 0 else 128)
           + (if Cache mod 2 = 1 then 8 else 0)
           + (if Cache / 2 mod 2 = 1 then 16 else 0)
           + (if Cache / 4 = 1 then (if Kind = Page_4K then 128 else 4096) else 0);
         for Page in Unsigned_64 range 0 .. Leaf_Bytes (Kind) / 4096 - 1 loop
            V := 16#FFFF_8000_0000_0000# + Page * 4096 + 4095;
            D := Decode (Raw, V, Max, Kind);
            Check (D.Valid and then D.Frame = Base + Page * 4096 and then D.Cache = Cache);
         end loop;
         Check (not Decode (Raw and not Unsigned_64'(1), 0, Max, Kind).Valid);
         Check (not Decode (Raw, 0, Base + 4094, Kind).Valid);
         Check (Decode (Raw, 0, Base + 4095, Kind).Valid);
         if Kind /= Page_4K then
            Check (not Decode (Raw and not Unsigned_64'(128), 0, Max, Kind).Valid);
            for Bit in 13 .. (if Kind = Page_2M then 20 else 29) loop
               Check (not Decode (Raw or Shift_Left (Unsigned_64'(1), Bit), 0, Max, Kind).Valid);
            end loop;
         end if;
         -- Source permission/AD/global/NX bits must not change the cache index.
         D := Decode (Raw or 16#8000_0000_0000_0166#, 0, Max, Kind);
         Check (D.Valid and then D.Frame = Base and then D.Cache = Cache);
      end loop;
   end loop;
   for Cache in Cache_Index loop
      Flags := Read_Only_Flags (Cache);
      Check ((Flags and 2) = 0 and (Flags and 256) = 0);
      Check ((Flags and 16#8000_0000_0000_0005#) = 16#8000_0000_0000_0005#);
      -- Independent makePTE-style translation of abstract PAT bit12 to bit7.
      Raw := (Flags and 16#8000_0000_0000_001D#)
        or (if (Flags and 4096) /= 0 then 128 else 0);
      D := Decode (Raw, 0, 4095, Page_4K);
      Check (D.Valid and then D.Frame = 0 and then D.Cache = Cache);
   end loop;
   Check (not Decode (1, 0, 4094, Page_4K).Valid);
   D := Decode (16#000F_FFFF_FFFF_F001#, 0, Max, Page_4K);
   Check (D.Valid and then D.Frame = Max - 4095);
   Ada.Text_IO.Put_Line ("ACPI-PAGE-CACHE: PASS" & Checks'Image);
end Cache_Tests;
