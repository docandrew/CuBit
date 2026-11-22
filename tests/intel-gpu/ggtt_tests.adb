with Interfaces; use Interfaces;
with Intel_GPU_GGTT; use Intel_GPU_GGTT;
procedure GGTT_Tests is
   Item : Window;
begin
   pragma Assert (Encode_System_Page (0) = 0);
   pragma Assert (Encode_System_Page (4096) = 4097);
   pragma Assert (Encode_System_Page (16#FFFF_F000#) = 16#FFFF_F001#);
   pragma Assert (Encode_System_Page (2 ** 32) = 0);
   pragma Assert (Encode_System_Page (Unsigned_64'Last) = 0);
   for Offset in Unsigned_64 range 1 .. 4095 loop
      pragma Assert (Encode_System_Page (16#1234_5000# + Offset) = 0);
   end loop;
   for Page in Unsigned_64 range 1 .. 2 ** 20 - 1 loop
      pragma Assert (Encode_System_Page (Page * 4096) = Page * 4096 + 1);
      pragma Assert ((Encode_System_Page (Page * 4096) and 4095) = 1);
   end loop;
   for GGC in Unsigned_16 loop
      pragma Assert (Table_Size (GGC) =
        (if GGC = Unsigned_16'Last or else ((GGC / 64) mod 4) = 0 then 0
         else 2 ** Natural ((GGC / 64) mod 4) * 1024 * 1024));
   end loop;
   Item := Plan_Window (2 * 1024 * 1024, 0, 335360);
   pragma Assert (Item.Valid and Item.First_Entry = 0 and
     Item.Entry_Count = 82 and Item.BAR_Offset = Table_BAR_Offset and
     Item.Mapping_Bytes = 4096);
   -- Last entry in a table page, spanning into the next page.
   Item := Plan_Window (2 * 1024 * 1024, 511 * 4096, 8192);
   pragma Assert (Item.Valid and Item.Mapping_Bytes = 8192);
   Item := Plan_Window (4096, 511 * 4096, 4096);
   pragma Assert (Item.Valid and Item.Entry_Count = 1);
   pragma Assert (not Plan_Window (4096, 511 * 4096, 4097).Valid);
   pragma Assert (not Plan_Window (4096, 512 * 4096, 1).Valid);
   pragma Assert (not Plan_Window (0, 0, 4096).Valid);
   pragma Assert (not Plan_Window (4097, 0, 4096).Valid);
   pragma Assert (not Plan_Window (4096, 1, 4096).Valid);
   pragma Assert (not Plan_Window (4096, 0, 0).Valid);
   pragma Assert (not Plan_Window (Unsigned_64'Last, 0, 1).Valid);
   pragma Assert (not Plan_Window (4096, Unsigned_64'Last - 4095, 1).Valid);
   pragma Assert (not Plan_Window (4096, 0, Unsigned_64'Last).Valid);
   for Pages in Unsigned_64 range 1 .. 2048 loop
      Item := Plan_Window (Pages * 4096, 0, Pages * 4096 * 512);
      pragma Assert (Item.Valid and Item.Mapping_Bytes = Pages * 4096);
      pragma Assert (not Plan_Window (Pages * 4096, 0, Pages * 4096 * 512 + 1).Valid);
   end loop;
end GGTT_Tests;
