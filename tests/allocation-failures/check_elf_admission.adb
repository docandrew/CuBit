with ELF_Admission; use ELF_Admission;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Check_ELF_Admission is
   H : Program_Header := (p_type => 1, p_flags => 5,
     p_offset => 4096, p_vaddr => 16#400000#, p_paddr => 0,
     p_filesz => 4096, p_memsz => 8192, p_align => 4096);
   Image_Size : constant Unsigned_64 := 16#10000#;
   Limit : constant Unsigned_64 := 16#10000000#;
   Pages, Before : Natural;
   Accepted : Boolean;
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;
begin
   pragma Assert (Pages_For (0) = 0);
   pragma Assert (Pages_For (4096) = 1);
   pragma Assert (Pages_For (4097) = 2);
   pragma Assert (Pages_For (Unsigned_64'Last) = 2 ** 52);
   -- A tiny file may legitimately describe a huge zero-filled segment.
   H.p_filesz := 0;
   for Bytes of Unsigned_64_Array'(32 * 1024 * 1024, 1024 * 1024 * 1024,
                                  64 * 1024 * 1024 * 1024) loop
      H.p_memsz := Bytes;
      pragma Assert (Segment_Fits (H, Image_Size, 16#0000_8000_0000_0000#));
      Pages := 0;
      Add_Image_Pages (Pages, Bytes, Accepted);
      pragma Assert (Accepted and then Unsigned_64 (Pages) = Bytes / Page_Size);
      pragma Assert (Frame_Capacity (Pages, 256, 4096) = Pages + 256 + 4096);
   end loop;
   Pages := 0;
   Add_Image_Pages (Pages, 4097, Accepted);
   pragma Assert (Accepted and then Pages = 2);
   Add_Image_Pages (Pages, 4097, Accepted);
   pragma Assert (Accepted and then Pages = 4);
   Before := Pages;
   Add_Image_Pages (Pages, Unsigned_64'Last, Accepted);
   pragma Assert (not Accepted and then Pages = Before);
   Pages := Natural'Last - 1;
   Add_Image_Pages (Pages, 4096, Accepted);
   pragma Assert (Accepted and then Pages = Natural'Last);
   Add_Image_Pages (Pages, 1, Accepted);
   pragma Assert (not Accepted and then Pages = Natural'Last);
   Add_Image_Pages (Pages, 0, Accepted);
   pragma Assert (Accepted and then Pages = Natural'Last);
   pragma Assert (Frame_Capacity (Natural'Last - 4352, 256, 4096) = Natural'Last);
   pragma Assert (Frame_Capacity (Natural'Last - 4351, 256, 4096) = 0);
   pragma Assert (Frame_Capacity (Natural'Last, Positive'Last, Natural'Last) = 0);
   H.p_filesz := 4096;
   H.p_memsz := 8192;
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
   Ada.Text_IO.Put_Line ("PASS ELF frame accounting: large BSS, independent image/stack/heap, overflow without mutation");
end Check_ELF_Admission;
