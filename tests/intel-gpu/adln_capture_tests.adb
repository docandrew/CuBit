with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Capture_List; use Intel_GPU_Capture_List;
with Intel_GPU_ADLN_Capture; use Intel_GPU_ADLN_Capture;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
procedure ADLN_Capture_Tests is
   package Inv renames Intel_GPU_ADLN_Inventory;
   package Top renames Intel_GPU_ADLN_Steering;
   function Word (Data : Page; Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (Data (Offset)) + Unsigned_32 (Data (Offset + 1)) * 256 +
      Unsigned_32 (Data (Offset + 2)) * 65536 + Unsigned_32 (Data (Offset + 3)) * 16777216);
   Data : Lists;
   Description : Inv.Inventory;
   Topology : Top.Topology;
   Cursor : Natural;
   Fuse : Unsigned_32;
begin
   for Media in Natural range 0 .. 7 loop
      Fuse := Unsigned_32 (Media mod 2) + Unsigned_32 ((Media / 2) mod 2) * 4 +
        Unsigned_32 (Media / 4) * 65536;
      Description := Inv.Decode (16#8086#, 16#46D2#, Fuse);
      for Mask in Unsigned_32 range 1 .. 63 loop
         Topology := Top.Decode (1, Mask, 0);
         Data := Build (Description, Topology);
         pragma Assert (Data.Valid and then Word (Data.Global, 0) = 9);
         pragma Assert (Word (Data.Global, 4) = 16#A188#);
         pragma Assert (Word (Data.Global, 132) = 16#CEC4#);
         Cursor := 3;
         for DSS in Natural range 0 .. 5 loop
            if (Mask and 2 ** DSS) /= 0 then
               pragma Assert (Word (Data.Classes (0), 4 + Cursor * 16) = 16#E160#);
               pragma Assert (Word (Data.Classes (0), 20 + Cursor * 16) = 16#E164#);
               pragma Assert (Word (Data.Classes (0), 12 + Cursor * 16) = Unsigned_32 (DSS) * 1048576);
               Cursor := Cursor + 2;
            end if;
         end loop;
         pragma Assert (Word (Data.Classes (0), 0) = Unsigned_32 (Cursor));
         pragma Assert (Data.Classes (1) = Page'(others => 0));
         pragma Assert (Data.Classes (3) = Page'(others => 0));
         for C in Class_ID loop
            declare
               Active : constant Boolean := C = 0 or C = 3 or
                 (C = 1 and then (Description.Engines (Inv.Video_0) or Description.Engines (Inv.Video_2))) or
                 (C = 2 and then Description.Engines (Inv.Enhance_0));
            begin
               if Active then
                  pragma Assert (Word (Data.Instances (C), 0) = 33);
                  pragma Assert (Word (Data.Instances (C), 4) = 16#50#);
                  pragma Assert (Word (Data.Instances (C), 516) = 16#28C#);
               else
                  pragma Assert (Data.Instances (C) = Page'(others => 0));
               end if;
            end;
         end loop;
         pragma Assert (Word (Data.Classes (2), 0) =
           (if Description.Engines (Inv.Enhance_0) then 4 else 0));
      end loop;
   end loop;
   Topology.Valid := False;
   pragma Assert (not Build (Description, Topology).Valid);
   Topology := Top.Decode (1, 1, 0);
   Description.Valid := False;
   pragma Assert (not Build (Description, Topology).Valid);
   Ada.Text_IO.Put_Line ("ADL-N capture: PASS (504 inventories/topologies)");
end ADLN_Capture_Tests;
