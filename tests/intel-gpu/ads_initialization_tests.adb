with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADS_Initialization; use Intel_GPU_ADS_Initialization;
with Intel_GPU_ADS_Layout; use Intel_GPU_ADS_Layout;
with Intel_GPU_ADS_Header;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
procedure ADS_Initialization_Tests is
   package Inv renames Intel_GPU_ADLN_Inventory;
   package Top renames Intel_GPU_ADLN_Steering;
   function Word (Data : Intel_GPU_ADS_Header.Header_Bytes; Offset : Natural) return Unsigned_64 is
     (Unsigned_64 (Data (Offset)) + Unsigned_64 (Data (Offset + 1)) * 256 +
      Unsigned_64 (Data (Offset + 2)) * 65536 + Unsigned_64 (Data (Offset + 3)) * 16777216);
   Data, Rejected : Prepared_Image;
   Inventory : Inv.Inventory;
   Topology : constant Top.Topology := Top.Decode (1, 63, 0);
   Base : constant Unsigned_64 := 16#100000#;
   Backing : constant Unsigned_64 := 16 * 1024 * 1024;
   Fuse : Unsigned_32;
   function Run (Address : Unsigned_64; Size : Unsigned_64 := Backing;
                 Private_Size : Unsigned_64 := 16#801000#;
                 DB2 : Unsigned_32 := 0) return Prepared_Image is
     (Prepare (Inventory, Topology, 0, DB2, 16#1000000#, Address, Size, Private_Size));
   procedure Check_All_Pointers (Image : Prepared_Image; Address : Unsigned_64) is
      Pointer, Bytes : Unsigned_64;
   begin
      pragma Assert (Image.Valid);
      for Slot in Natural range 0 .. 511 loop
         Pointer := Word (Image.Header, Slot * 8);
         Bytes := (Word (Image.Header, Slot * 8 + 4) and 16#FFFF#) * 16;
         pragma Assert (Shift_Right (Word (Image.Header, Slot * 8 + 4), 16) = 0);
         if Pointer = 0 then
            pragma Assert (Bytes = 0);
         else
            pragma Assert (Bytes > 0 and then Pointer mod 4 = 0);
            pragma Assert (Pointer >= Address + Image.Layout.Offset (Registers));
            pragma Assert (Pointer + Bytes <= Address + Image.Layout.Offset (Registers) +
              Unsigned_64 (Image.Registers.Used));
         end if;
      end loop;
      for Slot in Natural range 0 .. 65 loop
         Pointer := Word (Image.Header, 4252 + Slot * 4);
         pragma Assert (Pointer mod 4096 = 0);
         pragma Assert (Pointer >= Address + Image.Layout.Offset (Capture));
         pragma Assert (Pointer + 4096 <= Address + Image.Layout.Offset (Capture) +
           Unsigned_64 (Image.Capture.Used));
      end loop;
      for C in Natural range 0 .. 15 loop
         Pointer := Word (Image.Header, 4116 + C * 4);
         Bytes := Word (Image.Header, 4180 + C * 4);
         if Pointer = 0 then
            pragma Assert (Bytes = 0);
         else
            pragma Assert (Pointer mod 4096 = 0 and then Bytes > 0);
            pragma Assert (Pointer >= Address + Image.Layout.Offset (Golden_Contexts));
            pragma Assert (Pointer + Bytes + 4416 <= Address +
              Image.Layout.Offset (Golden_Contexts) + Image.Layout.Bytes (Golden_Contexts));
         end if;
      end loop;
   end Check_All_Pointers;
begin
   for Media in Unsigned_32 range 0 .. 7 loop
      Fuse := 0;
      if (Media and 1) /= 0 then Fuse := Fuse or 1; end if;
      if (Media and 2) /= 0 then Fuse := Fuse or 4; end if;
      if (Media and 4) /= 0 then Fuse := Fuse or 65536; end if;
      Inventory := Inv.Decode (16#8086#, 16#46D2#, Fuse);
      Data := Run (Base);
      Check_All_Pointers (Data, Base);
      pragma Assert (Data.Valid and then Sound (Data.Layout));
      pragma Assert (Word (Data.Header, 4100) = Base + Data.Layout.Offset (Policies));
      pragma Assert (Word (Data.Header, 4104) = Base + Data.Layout.Offset (System_Info));
      pragma Assert (Word (Data.Header, 4244) = Base + Data.Layout.Offset (Private_Data));
      pragma Assert (Word (Data.Header, 4116) = Base + Data.Layout.Offset (Golden_Contexts));
      pragma Assert (Word (Data.Header, 4180) = 52928);
      pragma Assert (Data.Policies (76) = 1);
      pragma Assert (Word (Data.Header, 4112) = 0 and then Word (Data.Header, 4524) = 0);
      for I in Data.Registers.Descriptors'Range loop
         pragma Assert (Data.Header (I) = Data.Registers.Descriptors (I));
      end loop;
      for I in Data.Capture.Pointers'Range loop
         pragma Assert (Data.Header (4252 + I) = Data.Capture.Pointers (I));
      end loop;
      pragma Assert (Run (Limit - Data.Layout.Total).Valid);
      pragma Assert (not Run (Limit - Data.Layout.Total + 4096).Valid);
      pragma Assert (Run (Base, Data.Layout.Total).Valid);
      pragma Assert (not Run (Base, Data.Layout.Total - 1).Valid);
      for DSS in Unsigned_32 range 1 .. 63 loop
         Data := Prepare (Inventory, Top.Decode (1, DSS, 0),
           16#00FF0000#, 16#00FF0000#, 16#1000000#, Base, Backing, 16#801000#);
         Check_All_Pointers (Data, Base);
         pragma Assert (Data.System_Info.Bytes (584) = 0 and then
           Data.System_Info.Bytes (585) = 1); -- 256 doorbells
      end loop;
   end loop;
   pragma Assert (not Run (0).Valid);
   pragma Assert (not Run (Base + 1).Valid);
   pragma Assert (not Run (Unsigned_64'Last).Valid);
   pragma Assert (not Run (Base, Private_Size => 0).Valid);
   Rejected := Run (Base, DB2 => 1);
   pragma Assert (not Rejected.Valid and then Rejected.Layout.Total = 0);
   -- Reusing a result after success must never preserve publishable header
   -- bytes when the next request is rejected.
   for Offset in Unsigned_64 range 1 .. 4095 loop
      Rejected := Run (Base + Offset);
      pragma Assert (not Rejected.Valid and then Rejected.Layout.Total = 0);
      pragma Assert (for all B of Rejected.Header => B = 0);
   end loop;
   for Bad in Natural range 0 .. 5 loop
      declare
         Bad_Topology : constant Top.Topology :=
           (case Bad is
              when 0 => Top.Decode (0, 63, 0),
              when 1 => Top.Decode (3, 63, 0),
              when 2 => Top.Decode (1, 0, 0),
              when 3 => Top.Decode (1, 63, 15),
              when 4 => Top.Decode (1, Unsigned_32'Last, 0),
              when others => Top.Decode (1, 63, Unsigned_32'Last));
      begin
         Rejected := Prepare (Inventory, Bad_Topology, 0, 0,
           16#1000000#, Base, Backing, 16#801000#);
         pragma Assert (not Rejected.Valid and then Rejected.Layout.Total = 0);
         pragma Assert (for all B of Rejected.Header => B = 0);
      end;
   end loop;
   for Size in Unsigned_32 range 0 .. 4096 loop
      Rejected := Prepare (Inventory, Topology, 0, 0, Size,
        Base, Backing, 16#801000#);
      pragma Assert (not Rejected.Valid and then Rejected.Layout.Total = 0);
      pragma Assert (for all B of Rejected.Header => B = 0);
   end loop;
   Inventory := Inv.Decode (16#8086#, 16#FFFF#, 0);
   Rejected := Run (Base);
   pragma Assert (not Rejected.Valid);
   pragma Assert (for all B of Rejected.Header => B = 0);
   Ada.Text_IO.Put_Line ("ADS initialization: PASS (504 topologies, all pointers, recovery off, boundaries)");
   Ada.Text_IO.Put_Line ("ADS rejection: PASS (4095 alignments, 4097 MMIO bounds, malformed topology/device)");
end ADS_Initialization_Tests;
