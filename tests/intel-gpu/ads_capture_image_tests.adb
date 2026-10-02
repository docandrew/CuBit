with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADS_Capture_Image; use Intel_GPU_ADS_Capture_Image;
with Intel_GPU_ADLN_Capture;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_Capture_List;
procedure ADS_Capture_Image_Tests is
   package Inv renames Intel_GPU_ADLN_Inventory;
   package Top renames Intel_GPU_ADLN_Steering;
   function Word (Data : Pointer_Bytes; Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (Data (Offset)) + Unsigned_32 (Data (Offset + 1)) * 256 +
      Unsigned_32 (Data (Offset + 2)) * 65536 + Unsigned_32 (Data (Offset + 3)) * 16777216);
   Data : Capture_Image;
   Source : Intel_GPU_ADLN_Capture.Lists;
   Description : Inv.Inventory;
   Topology : constant Top.Topology := Top.Decode (1, 63, 0);
   Base : constant Unsigned_64 := 16#100000#;
   Cursor : Natural;
   Fuse : Unsigned_32;
   procedure Check (Page : Intel_GPU_Capture_List.Page; Pointer : Natural) is
   begin
      if Page (0) = 0 then
         pragma Assert (Word (Data.Pointers, Pointer) = Unsigned_32 (Base));
      else
         pragma Assert (Word (Data.Pointers, Pointer) = Unsigned_32 (Base) + Unsigned_32 (Cursor));
         for J in Page'Range loop
            pragma Assert (Data.Data (Cursor + J) = Page (J));
         end loop;
         Cursor := Cursor + 4096;
      end if;
   end Check;
begin
   for Media in Natural range 0 .. 7 loop
      Fuse := 0;
      if (Unsigned_32 (Media) and 1) /= 0 then Fuse := Fuse or 1; end if;
      if (Unsigned_32 (Media) and 2) /= 0 then Fuse := Fuse or 4; end if;
      if (Unsigned_32 (Media) and 4) /= 0 then Fuse := Fuse or 65536; end if;
      Description := Inv.Decode (16#8086#, 16#46D2#, Fuse);
      Source := Intel_GPU_ADLN_Capture.Build (Description, Topology);
      Data := Build (Description, Topology, Base, Capacity);
      pragma Assert (Data.Valid);
      pragma Assert (for all J in 0 .. 4095 => Data.Data (J) = 0);
      Cursor := 4096;
      for C in Intel_GPU_ADLN_Capture.Class_ID loop
         Check (Source.Classes (C), 128 + C * 4);
         Check (Source.Instances (C), C * 4);
      end loop;
      Check (Source.Global, 256);
      pragma Assert (Data.Used = Cursor);
      pragma Assert (for all J in Cursor .. Capacity - 1 => Data.Data (J) = 0);
      for I in Natural range 4 .. 31 loop
         pragma Assert (Word (Data.Pointers, I * 4) = Unsigned_32 (Base));
         pragma Assert (Word (Data.Pointers, 128 + I * 4) = Unsigned_32 (Base));
      end loop;
      pragma Assert (Word (Data.Pointers, 260) = Unsigned_32 (Base));
   end loop;
   pragma Assert (Build (Description, Topology, 16#FEE00000# - Capacity, Capacity).Valid);
   pragma Assert (not Build (Description, Topology, 16#FEE00000# - Capacity + 4096, Capacity).Valid);
   pragma Assert (not Build (Description, Topology, Base, Capacity - 1).Valid);
   pragma Assert (not Build (Description, Topology, 0, Capacity).Valid);
   pragma Assert (not Build (Description, Topology, Base + 1, Capacity).Valid);
   pragma Assert (not Build (Description, Topology, Unsigned_64'Last, Capacity).Valid);
   Description.Valid := False;
   Data := Build (Description, Topology, Base, Capacity);
   pragma Assert (not Data.Valid and then Data.Used = 0 and then
     Data.Data = Capture_Bytes'(others => 0) and then Data.Pointers = Pointer_Bytes'(others => 0));
   Ada.Text_IO.Put_Line ("ADS capture image: PASS (tables, backing, bounds)");
end ADS_Capture_Image_Tests;
