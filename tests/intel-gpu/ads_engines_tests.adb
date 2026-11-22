with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADS_Engines; use Intel_GPU_ADS_Engines;
procedure ADS_Engines_Tests is
   Description : Inventory;
   Result : Encoding;
   Expected : Engine_Bytes;
   Fuse : Unsigned_32;
   Logical_Video : Natural;
begin
   for Disabled in Unsigned_32 range 0 .. 7 loop
      Fuse := (Disabled and 1) or Shift_Left (Disabled and 2, 1) or
        Shift_Left (Disabled and 4, 14);
      Description := Decode (16#8086#, 16#46D2#, Fuse);
      Result := Encode (Description);
      pragma Assert (Result.Valid);
      Expected := [0 .. 511 => 32, others => 0];
      Expected (0) := 0;
      Expected (3 * 32) := 0;
      Expected (512) := 1;
      Expected (512 + 3 * 4) := 1;
      Logical_Video := 0;
      for Physical in 0 .. 2 loop
         if (Physical = 0 and (Disabled and 1) = 0) or
           (Physical = 2 and (Disabled and 2) = 0)
         then
            Expected (32 + Logical_Video) := Unsigned_8 (Physical);
            Expected (516) := Expected (516) + 2 ** Physical;
            Logical_Video := Logical_Video + 1;
         end if;
      end loop;
      if (Disabled and 4) = 0 then
         Expected (2 * 32) := 0;
         Expected (512 + 2 * 4) := 1;
      end if;
      pragma Assert (Result.Bytes = Expected);
   end loop;
   Description := Decode (16#8086#, 16#46D2#, 16#000E00FE#);
   Result := Encode (Description);
   pragma Assert (Result.Valid and Result.Bytes (32) = 0 and
                  Result.Bytes (33) = 32 and Result.Bytes (516) = 1);
   Description.Valid := False;
   Result := Encode (Description);
   pragma Assert (not Result.Valid and
                  Result.Bytes = Engine_Bytes'(0 .. 511 => 32, others => 0));
   for Core in Engine range Render .. Copy loop
      Description := Decode (16#8086#, 16#46D2#, 0);
      Description.Engines (Core) := False;
      pragma Assert (not Encode (Description).Valid);
   end loop;
   Ada.Text_IO.Put_Line ("ADS engines: 8 fuse combinations, NUC snapshot and invalid inputs PASS");
end ADS_Engines_Tests;
