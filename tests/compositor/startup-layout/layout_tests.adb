with Desktop_Startup_Layout;
with Ada.Text_IO;
procedure Layout_Tests is
   package L renames Desktop_Startup_Layout;
   use type L.Wide;
   procedure Check (W, H : L.Wide; Expected : Natural) is
   begin
      pragma Assert (L.Required_Bytes (W, H) = Expected);
      pragma Assert (L.Supported (W, H) = (Expected /= 0));
   end Check;
begin
   Check (0, 768, 0); Check (1024, 0, 0);
   Check (1, 1, 4); Check (1024, 768, 3_145_728);
   Check (1920, 1080, 8_294_400);
   Check (2048, 2048, 16_777_216);
   Check (4096, 1024, 16_777_216);
   Check (4096, 1025, 0); Check (4096, 4096, 0);
   Check (4097, 1, 0); Check (1, 4097, 0);
   Check (L.Wide'Last, L.Wide'Last, 0);
   -- Old unchecked arithmetic wraps these to plausible BGRA sizes.
   pragma Assert ((2 ** 62 + 1024) * L.Wide (768) * 4 = 3_145_728);
   Check (2 ** 62 + 1024, 768, 0);
   Check (1024, 2 ** 62 + 768, 0);
   Ada.Text_IO.Put_Line ("PASS: 14 startup layout cases, including modular wrap rejection");
end Layout_Tests;
