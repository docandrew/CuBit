with ELF_Admission; use ELF_Admission;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Check_ELF_Admission is
   H : Program_Header := (p_type => 1, p_flags => 5,
     p_offset => 4096, p_vaddr => 16#400000#, p_paddr => 0,
     p_filesz => 4096, p_memsz => 8192, p_align => 4096);
   Image_Size : constant Unsigned_64 := 16#10000#;
   Limit : constant Unsigned_64 := 16#10000000#;
begin
   pragma Assert (Table_Fits (Image_Size, 64, 5, 56));
   pragma Assert (not Table_Fits (Image_Size, 64, 0, 56));
   pragma Assert (not Table_Fits (Image_Size, 64, Max_Headers + 1, 56));
   pragma Assert (not Table_Fits (Image_Size, 64, 5, 1));
   pragma Assert (not Table_Fits (Image_Size, Unsigned_64'Last, 5, 56));
   pragma Assert (not Table_Fits (343, 64, 5, 56));
   pragma Assert (Table_Fits (344, 64, 5, 56));
   pragma Assert (Segment_Fits (H, Image_Size, Limit));
   H.p_filesz := Unsigned_64'Last;
   pragma Assert (not Segment_Fits (H, Image_Size, Limit));
   H.p_filesz := 4096;
   H.p_memsz := 4095;
   pragma Assert (not Segment_Fits (H, Image_Size, Limit));
   H.p_memsz := Unsigned_64'Last;
   pragma Assert (not Segment_Fits (H, Image_Size, Limit));
   H.p_memsz := 8192;
   H.p_vaddr := Unsigned_64'Last - 4095;
   pragma Assert (not Segment_Fits (H, Image_Size, Limit));
   H.p_vaddr := Limit - 4 * Page_Size;
   pragma Assert (Segment_Fits (H, Image_Size, Limit));
   H.p_vaddr := H.p_vaddr + Page_Size;
   pragma Assert (not Segment_Fits (H, Image_Size, Limit));
   H.p_vaddr := 16#400000#;
   H.p_offset := Image_Size;
   pragma Assert (not Segment_Fits (H, Image_Size, Limit));
   H.p_filesz := 0;
   pragma Assert (Segment_Fits (H, Image_Size, Limit));
   H.p_align := 3;
   pragma Assert (not Segment_Fits (H, Image_Size, Limit));
   H.p_align := 4096;
   for Offset in Unsigned_64 range 1 .. 4095 loop
      H.p_vaddr := 16#400000# + Offset;
      pragma Assert (not Segment_Fits (H, Image_Size, Limit));
   end loop;
   Ada.Text_IO.Put_Line ("PASS ELF admission: wire table, file/memory ranges, alignment, guard reservation, overflow");
end Check_ELF_Admission;
