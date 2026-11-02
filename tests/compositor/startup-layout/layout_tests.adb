with Desktop_Startup_Layout;
with Ada.Text_IO;
procedure Layout_Tests is
   package L renames Desktop_Startup_Layout;
   use type L.Wide;
   Cases : Natural := 0;
   procedure Check (W, H : L.Wide; Expected : Natural) is
   begin
      pragma Assert (L.Required_Bytes (W, H) = Expected);
      pragma Assert (L.Supported (W, H) = (Expected /= 0));
      pragma Assert ((L.GPU_Budget_Bytes (W, H) /= 0) = (Expected /= 0));
      if Expected /= 0 then
         --  The budget holds Budget_Frames frames and never shrinks below
         --  the budget every mode up to 1080p always had.
         pragma Assert (L.Wide (L.GPU_Budget_Bytes (W, H)) >= L.Budget_Frames * L.Wide (Expected));
         pragma Assert (L.Wide (L.GPU_Budget_Bytes (W, H)) >= L.Minimum_GPU_Budget);
      end if;
      Cases := Cases + 1;
   end Check;
begin
   Check (0, 768, 0); Check (1024, 0, 0);
   Check (1, 1, 4); Check (1024, 768, 3_145_728);
   Check (1920, 1080, 8_294_400);
   Check (2048, 2048, 16_777_216);
   Check (4096, 1024, 16_777_216);
   --  LIM-001: sized from the mode, not a fixed 16 MiB. Each of these was
   --  refused before (a 4K frame is about 33 MB).
   Check (4096, 1025, 16_793_600);
   Check (3840, 2160, 33_177_600);
   Check (5120, 2880, 58_982_400);
   Check (4096, 4096, 67_108_864);
   Check (7680, 4320, 132_710_400);
   Check (4097, 1, 16_388); Check (1, 4097, 16_388);
   pragma Assert (L.GPU_Budget_Bytes (1920, 1080) = 128 * 1024 * 1024);
   pragma Assert (L.GPU_Budget_Bytes (3840, 2160) = 16 * 33_177_600);
   --  Past the representational ceilings: a named refusal with its demand.
   Check (65535, 65535, 0);
   pragma Assert (L.GPU_Budget_Demand (65535, 65535) > L.Capacity_Limit);
   Check (65536, 1, 0); Check (1, 65536, 0);
   Check (L.Wide'Last, L.Wide'Last, 0);
   -- Old unchecked arithmetic wraps these to plausible BGRA sizes.
   pragma Assert ((2 ** 62 + 1024) * L.Wide (768) * 4 = 3_145_728);
   Check (2 ** 62 + 1024, 768, 0);
   Check (1024, 2 ** 62 + 768, 0);
   Ada.Text_IO.Put_Line ("PASS:" & Cases'Image &
     " startup layout cases, 4K/5K/8K sized from the mode, modular wrap rejection");
end Layout_Tests;
