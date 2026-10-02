with Interfaces; use Interfaces;
with Memory_Grants; use Memory_Grants;
with Ada.Text_IO;
procedure DMA_Range_Test is
   Size : constant Unsigned_64 := 32 * 1024 * 1024;
begin
   pragma Assert (not Overlaps_Received_Bytes (16#7000_0000_0000#, Size));
   pragma Assert (not Overlaps_Received_Bytes (Received_Region_First - Size, Size));
   pragma Assert (Overlaps_Received_Bytes (Received_Region_First - Size + 1, Size));
   pragma Assert (Overlaps_Received_Bytes (Received_Region_First, Size));
   pragma Assert (Overlaps_Received_Bytes (Received_Region_Limit - 1, Size));
   pragma Assert (not Overlaps_Received_Bytes (Received_Region_Limit, Size));
   pragma Assert (not Overlaps_Received_Bytes (Received_Region_First, 0));
   pragma Assert (Overlaps_Received_Bytes (Unsigned_64'Last, 2));
   pragma Assert (not Overlaps_Received_Bytes (Unsigned_64'Last, 1));
   for Pages in Page_Count loop
      pragma Assert (Overlaps_Received_Region (Received_Region_First - 4096, Pages) =
        Overlaps_Received_Bytes (Received_Region_First - 4096, Unsigned_64 (Pages) * Page_Size));
   end loop;
   Ada.Text_IO.Put_Line ("DMA range PASS: order13, exact exclusion boundaries, overflow and grant wrapper");
end DMA_Range_Test;
