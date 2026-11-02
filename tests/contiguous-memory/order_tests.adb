with Ada.Text_IO;
with Contiguous_Memory; use Contiguous_Memory;

--  LIM-001: devmgr asked the kernel for 2 ** 14 pages (its DMA_MAX_ORDER)
--  while the kernel's buddy allocator stops at 2 ** 13; the manifest
--  compiler admitted 64 MiB of DMA that ALLOC_DMA always refused. Now one
--  definition: the kernel's order, which devmgr and the compiler derive from.
procedure Order_Tests is
begin
   pragma Assert (Maximum_Order = 13);
   pragma Assert (Maximum_Bytes = 32 * 1024 * 1024);
   pragma Assert (Order_For (1) = 0);
   pragma Assert (Order_For (2) = 1);
   pragma Assert (Order_For (3) = 2);
   pragma Assert (Order_For (Maximum_Pages) = Maximum_Order);
   pragma Assert (Order_For (Maximum_Pages / 2 + 1) = Maximum_Order);
   for Pages in Page_Count loop
      declare
         O : constant Order := Order_For (Pages);
      begin
         pragma Assert (2 ** O >= Pages);
         pragma Assert (O = 0 or else 2 ** (O - 1) < Pages);
      end;
   end loop;
   --  The old devmgr order is past what the kernel can allocate.
   pragma Assert (14 not in Order);
   Ada.Text_IO.Put_Line ("contiguous-memory: PASS orders for 1 .." & Maximum_Pages'Image &
     " pages, ceiling" & Maximum_Bytes'Image & " bytes");
end Order_Tests;
