with Ada.Text_IO; with Interfaces;
with Intel_GPU_PHY_Pages; with Intel_GPU_Display_Pages; with Intel_GPU_Reset_Pages;
procedure PHY_Pages_Tests is
   use Interfaces; use Intel_GPU_PHY_Pages;
   type Pair is record Offset : Unsigned_32; Address : Unsigned_64; end record;
   Expected : constant array (1 .. 17) of Pair :=
     ((16#64C00#, 16#61500C00#), (16#1626A0#, 16#615016A0#),
      (16#162604#, 16#61501604#), (16#162104#, 16#61501104#),
      (16#162124#, 16#61501124#), (16#162128#, 16#61501128#),
      (16#162120#, 16#61501120#), (16#162100#, 16#61501100#),
      (16#162014#, 16#61501014#), (16#64C04#, 16#61500C04#),
      (16#6C6A0#, 16#615026A0#), (16#6C604#, 16#61502604#),
      (16#6C104#, 16#61502104#), (16#6C124#, 16#61502124#),
      (16#6C128#, 16#61502128#), (16#6C100#, 16#61502100#),
      (16#6C014#, 16#61502014#));
   Found : Unsigned_64;
begin
   pragma Assert (Offset (0) = 16#64000# and Offset (1) = 16#162000# and Offset (2) = 16#6C000#);
   for I in Page_Index loop
      pragma Assert (Slot (I) in 29 .. 31);
      for J in Intel_GPU_Display_Pages.Page_Index loop
         pragma Assert (Slot (I) /= Intel_GPU_Display_Pages.Slot (J));
      end loop;
      for J in Intel_GPU_Reset_Pages.Page_Index loop
         pragma Assert (Slot (I) /= Intel_GPU_Reset_Pages.Slot (J));
      end loop;
   end loop;
   -- Every byte offset, not only aligned offsets: no unintended registers.
   for R in Unsigned_32 range 0 .. 16#1FFFFF# loop
      Found := 0;
      for E of Expected loop
         if E.Offset = R then Found := E.Address; end if;
      end loop;
      pragma Assert (Write_Address (R) = Found);
   end loop;
   pragma Assert (Write_Address (Unsigned_32'Last) = 0);
   Ada.Text_IO.Put_Line ("PHY pages PASS: exhaustive 2MiB byte offsets, 17 exact writes, distinct slots");
end PHY_Pages_Tests;
